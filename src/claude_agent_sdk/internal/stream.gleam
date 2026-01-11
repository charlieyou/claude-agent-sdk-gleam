/// QueryStream: Single-process iterator over CLI output.
///
/// This module provides the streaming abstraction over the CLI subprocess.
/// QueryStream is an opaque type that maintains state for:
/// - BEAM port handle (shared across copies)
/// - Line reassembly buffer (per-copy)
/// - State machine for stream lifecycle
/// - Error counters for decode failure thresholds
///
/// **Single-process constraint**: QueryStream is NOT thread-safe.
/// It must only be used from the process that created it.
import claude_agent_sdk/error.{
  type ErrorDiagnostic, type Warning, CleanExitNoResult, NonZeroExitAfterResult,
  Warning, diagnose_exit_code,
}
import claude_agent_sdk/internal/constants
import claude_agent_sdk/internal/decoder
import claude_agent_sdk/internal/port_ffi.{type Port, Data, ExitStatus, Timeout}
import claude_agent_sdk/message.{type MessageEnvelope, Result}
import gleam/bit_array
import gleam/int
import gleam/option.{None}
import gleam/string

// ============================================================================
// ReadLineResult: Result of reading a line from the buffer
// ============================================================================

/// Result of attempting to read a complete line from the buffer.
pub type ReadLineResult {
  /// A complete line was found and decoded as UTF-8
  CompleteLine(String)
  /// Port was closed (exit_status received with exit code 0 and empty buffer)
  PortClosed
  /// Exit status received with non-zero code
  ExitReceived(Int)
  /// Buffer exceeded max_line_size_bytes
  BufferOverflow
  /// An error occurred (e.g., invalid UTF-8)
  ReadError(String)
  /// No complete line yet; need more data
  NeedMoreData
}

// ============================================================================
// StreamState: State machine for stream lifecycle
// ============================================================================

/// State machine governing stream iteration behavior.
///
/// Transitions:
/// - Streaming -> ResultReceived (on Result message)
/// - Streaming -> PendingEndOfStream (on clean exit without Result)
/// - Streaming -> Closed (on non-zero exit without Result)
/// - ResultReceived -> PendingEndOfStream (on exit status after Result)
/// - PendingEndOfStream -> Closed (after yielding EndOfStream)
pub type StreamState {
  /// Normal iteration: yielding messages as they arrive
  Streaming
  /// Result message received; draining remaining messages
  ResultReceived
  /// Exit status received after Result; will yield EndOfStream next
  PendingEndOfStream
  /// Stream is closed; no more items will be yielded
  Closed
}

// ============================================================================
// QueryStreamInternal: Internal state record
// ============================================================================

/// Internal state for QueryStream.
/// Not exported - only accessible via QueryStream opaque type.
type QueryStreamInternal {
  QueryStreamInternal(
    /// Direct BEAM port handle
    port: Port,
    /// Line reassembly buffer for partial reads
    buffer: BitArray,
    /// Current state in the stream lifecycle
    state: StreamState,
    /// Count of consecutive JSON decode errors
    consecutive_decode_errors: Int,
    /// Messages drained after ResultReceived
    drain_count: Int,
    /// Whether close() has been called
    closed: Bool,
    /// Whether a Result message has been received.
    /// CRITICAL: If True, no exit_status can produce ProcessError.
    result_seen: Bool,
  )
}

// ============================================================================
// QueryStream: Opaque public type
// ============================================================================

/// Opaque iterator over CLI output.
///
/// **Ownership**: The port handle is shared across copies of QueryStream,
/// but buffer and counters are per-copy. Always use the most recent copy
/// returned from next() to maintain correct state.
///
/// **Single-process**: Must only be used from the creating process.
pub opaque type QueryStream {
  QueryStream(internal: QueryStreamInternal)
}

// ============================================================================
// Constructor (internal use only)
// ============================================================================

/// Create a new QueryStream from a port.
/// Internal only - called by query() after successful spawn.
pub fn new(port: Port) -> QueryStream {
  QueryStream(QueryStreamInternal(
    port: port,
    buffer: <<>>,
    state: Streaming,
    consecutive_decode_errors: 0,
    drain_count: 0,
    closed: False,
    result_seen: False,
  ))
}

// ============================================================================
// State accessors (for testing and internal use)
// ============================================================================

/// Get the current stream state.
pub fn get_state(stream: QueryStream) -> StreamState {
  let QueryStream(internal) = stream
  internal.state
}

/// Check if the stream is closed.
pub fn is_closed(stream: QueryStream) -> Bool {
  let QueryStream(internal) = stream
  internal.closed
}

// ============================================================================
// close(): Idempotent cleanup with mailbox drain
// ============================================================================

/// Close the stream and release resources.
///
/// **Semantics**:
/// - Idempotent: Safe to call multiple times; subsequent calls are no-ops
/// - Bounded-blocking: Closes port then drains mailbox (bounded)
/// - Auto-called: Called automatically on terminal errors and EndOfStream
/// - Same-process: MUST be called from same process that called query()
///
/// **Note**: close() performs a user-initiated abort and does NOT parse/emit
/// any remaining messages in the mailbox.
///
/// Returns an updated QueryStream with closed=True. Always use the returned
/// stream for subsequent operations to maintain correct state.
pub fn close(stream: QueryStream) -> QueryStream {
  let QueryStream(internal) = stream
  case internal.closed {
    True -> stream
    False -> {
      port_ffi.ffi_close_port(internal.port)
      QueryStream(QueryStreamInternal(..internal, closed: True, state: Closed))
    }
  }
}

// ============================================================================
// Line Buffer Operations
// ============================================================================

/// Normalize CRLF to LF for Windows compatibility.
/// Replaces all \r\n sequences with \n at the byte level.
pub fn normalize_crlf(buffer: BitArray) -> BitArray {
  normalize_crlf_loop(buffer, <<>>)
}

fn normalize_crlf_loop(input: BitArray, acc: BitArray) -> BitArray {
  case input {
    // CRLF sequence: skip \r, keep \n
    <<0x0D, 0x0A, rest:bits>> -> normalize_crlf_loop(rest, <<acc:bits, 0x0A>>)
    // Single byte: copy through
    <<byte, rest:bits>> -> normalize_crlf_loop(rest, <<acc:bits, byte>>)
    // Empty: done
    <<>> -> acc
    // Fallback for any other pattern (shouldn't happen with valid BitArray)
    _ -> acc
  }
}

/// Append data to the buffer, checking for overflow.
/// Returns updated buffer or BufferOverflow error.
/// Note: CRLF normalization is deferred to read_line() to handle sequences
/// that span chunk boundaries correctly.
pub fn append_to_buffer(
  buffer: BitArray,
  data: BitArray,
) -> Result(BitArray, ReadLineResult) {
  // Check sizes BEFORE allocation to fail fast
  let buffer_size = bit_array.byte_size(buffer)
  let data_size = bit_array.byte_size(data)
  case buffer_size + data_size > constants.max_line_size_bytes {
    True -> Error(BufferOverflow)
    False -> Ok(bit_array.append(buffer, data))
  }
}

/// Try to extract a complete line from the buffer.
/// Returns the line (without newline) and remaining buffer, or NeedMoreData.
/// Normalizes CRLF to LF before scanning to handle Windows line endings.
/// Uses O(n) pattern matching instead of O(n²) slicing.
pub fn read_line(buffer: BitArray) -> #(ReadLineResult, BitArray) {
  // Normalize CRLF first to handle Windows line endings
  let normalized = normalize_crlf(buffer)
  // Use O(n) scanning via pattern matching
  find_newline_pattern(normalized, <<>>)
}

fn find_newline_pattern(
  remaining: BitArray,
  acc: BitArray,
) -> #(ReadLineResult, BitArray) {
  case remaining {
    <<>> -> #(NeedMoreData, acc)
    <<0x0A, rest:bits>> -> {
      // Found newline - acc contains the line bytes
      case bit_array.to_string(acc) {
        Ok(line) -> #(CompleteLine(line), rest)
        Error(_) -> #(
          ReadError("Invalid UTF-8 in line: " <> string.inspect(acc)),
          rest,
        )
      }
    }
    <<byte, rest:bits>> -> find_newline_pattern(rest, <<acc:bits, byte>>)
    // Fallback for non-byte-aligned data (shouldn't happen)
    _ -> #(NeedMoreData, bit_array.append(acc, remaining))
  }
}

/// Get the current buffer from a QueryStream.
pub fn get_buffer(stream: QueryStream) -> BitArray {
  let QueryStream(internal) = stream
  internal.buffer
}

/// Update the buffer in a QueryStream.
pub fn set_buffer(stream: QueryStream, buffer: BitArray) -> QueryStream {
  let QueryStream(internal) = stream
  QueryStream(QueryStreamInternal(..internal, buffer: buffer))
}

// ============================================================================
// State Accessors (continued)
// ============================================================================

/// Check if a Result message has been seen.
pub fn get_result_seen(stream: QueryStream) -> Bool {
  let QueryStream(internal) = stream
  internal.result_seen
}

/// Get the consecutive decode error count.
pub fn get_consecutive_decode_errors(stream: QueryStream) -> Int {
  let QueryStream(internal) = stream
  internal.consecutive_decode_errors
}

// ============================================================================
// State Machine Transitions
// ============================================================================

/// Transition result for exit status handling.
/// Describes the next action after receiving an exit status.
pub type ExitTransitionResult {
  /// Yield a ProcessError (terminal - stream will close)
  YieldProcessError(exit_code: Int)
  /// Yield a warning, then EndOfStream (non-fatal anomaly)
  YieldWarningThenEnd(warning: Warning)
  /// Yield EndOfStream (normal completion)
  YieldEndOfStream
}

/// Handle exit status according to the state machine.
///
/// **State Machine (result_seen precedence)**:
/// | result_seen | exit_code | Outcome                          |
/// |-------------|-----------|----------------------------------|
/// | FALSE       | 0         | Warning(CleanExitNoResult) + EoS |
/// | FALSE       | !=0       | ProcessError (terminal)          |
/// | TRUE        | 0         | EndOfStream (normal)             |
/// | TRUE        | !=0       | Warning(NonZeroAfter) + EoS      |
///
/// CRITICAL: If result_seen == TRUE, no exit_status produces ProcessError.
pub fn handle_exit_status(
  stream: QueryStream,
  exit_code: Int,
) -> #(ExitTransitionResult, QueryStream) {
  let QueryStream(internal) = stream
  let result_seen = internal.result_seen

  case result_seen, exit_code {
    // Result seen + exit 0 -> normal completion
    True, 0 -> {
      let updated =
        QueryStream(QueryStreamInternal(..internal, state: PendingEndOfStream))
      #(YieldEndOfStream, updated)
    }

    // Result seen + non-zero exit -> warning only, Result is authoritative
    True, code -> {
      let warning =
        Warning(
          code: NonZeroExitAfterResult(code),
          message: "CLI exited with code "
            <> int.to_string(code)
            <> " after Result message; Result is authoritative",
          context: None,
        )
      let updated =
        QueryStream(QueryStreamInternal(..internal, state: PendingEndOfStream))
      #(YieldWarningThenEnd(warning), updated)
    }

    // No result + exit 0 -> anomaly warning
    False, 0 -> {
      let warning =
        Warning(
          code: CleanExitNoResult,
          message: "CLI exited cleanly (code 0) but no Result message was received",
          context: None,
        )
      let updated =
        QueryStream(QueryStreamInternal(..internal, state: PendingEndOfStream))
      #(YieldWarningThenEnd(warning), updated)
    }

    // No result + non-zero exit -> terminal error
    False, code -> {
      let updated = QueryStream(QueryStreamInternal(..internal, state: Closed))
      #(YieldProcessError(code), updated)
    }
  }
}

/// Transition to ResultReceived state after seeing a Result message.
/// Sets result_seen = True and state = ResultReceived.
pub fn mark_result_received(stream: QueryStream) -> QueryStream {
  let QueryStream(internal) = stream
  QueryStream(
    QueryStreamInternal(..internal, state: ResultReceived, result_seen: True),
  )
}

/// Increment the consecutive decode error counter.
/// Returns the updated stream and whether the threshold was exceeded.
pub fn increment_decode_errors(
  stream: QueryStream,
  _last_error: String,
) -> #(QueryStream, Bool) {
  let QueryStream(internal) = stream
  let new_count = internal.consecutive_decode_errors + 1
  let threshold_exceeded = new_count >= constants.max_consecutive_decode_errors
  let updated =
    QueryStream(
      QueryStreamInternal(..internal, consecutive_decode_errors: new_count),
    )
  #(updated, threshold_exceeded)
}

/// Reset the consecutive decode error counter (on successful decode).
pub fn reset_decode_errors(stream: QueryStream) -> QueryStream {
  let QueryStream(internal) = stream
  case internal.consecutive_decode_errors {
    0 -> stream
    _ ->
      QueryStream(QueryStreamInternal(..internal, consecutive_decode_errors: 0))
  }
}

/// Transition to PendingEndOfStream state (for draining after ResultReceived).
pub fn mark_pending_end_of_stream(stream: QueryStream) -> QueryStream {
  let QueryStream(internal) = stream
  QueryStream(QueryStreamInternal(..internal, state: PendingEndOfStream))
}

/// Transition to Closed state (terminal).
/// Idempotent: returns unchanged stream if already closed.
pub fn mark_closed(stream: QueryStream) -> QueryStream {
  let QueryStream(internal) = stream
  case internal.closed {
    True -> stream
    False ->
      QueryStream(QueryStreamInternal(..internal, state: Closed, closed: True))
  }
}

// ============================================================================
// StreamItem: What next() yields on success
// ============================================================================

/// What the stream can yield on success.
pub type StreamItem {
  /// A parsed message from the CLI
  Message(MessageEnvelope)
  /// A non-fatal warning (stream continues)
  WarningEvent(Warning)
  /// Normal end of stream (terminal - no more items)
  EndOfStream
}

// ============================================================================
// NextError: Errors from next()
// ============================================================================

/// Errors that next() can return.
/// These mirror error.StreamError but are defined here to avoid circular deps.
pub type NextError {
  /// CLI process exited with non-zero code (terminal)
  NextProcessError(exit_code: Int, diagnostic: ErrorDiagnostic)
  /// Single line exceeded buffer limit (terminal)
  NextBufferOverflow
  /// Too many consecutive JSON decode failures (terminal)
  NextTooManyDecodeErrors(count: Int, last_error: String)
  /// Line was not valid JSON (non-terminal)
  NextJsonDecodeError(line: String, error: String)
  /// Valid JSON but unknown message type (non-terminal)
  NextUnexpectedMessageError(raw_json: String)
}

// ============================================================================
// next(): Core iteration function
// ============================================================================

/// Advance the stream by one item.
///
/// **Timeout Behavior by State**:
/// | Stream State           | Timeout Behavior       | Notes                        |
/// |------------------------|------------------------|------------------------------|
/// | Streaming (before Res) | Blocks indefinitely    | User controls via max_turns  |
/// | ResultReceived         | 100ms timeout per call | Drain behavior               |
/// | PendingEndOfStream     | Returns immediately    | Yield EndOfStream            |
/// | Closed                 | Returns immediately    | No I/O needed                |
///
/// **Return Value Classification**:
/// - Ok(Message(envelope)) -> call next() again
/// - Ok(WarningEvent(warning)) -> call next() again
/// - Ok(EndOfStream) -> stop iteration
/// - Error(NextJsonDecodeError(_)) -> Non-terminal, call next() again
/// - Error(NextUnexpectedMessageError(_)) -> Non-terminal, call next() again
/// - Error(NextProcessError(_)) -> Terminal, stop iteration
/// - Error(NextBufferOverflow) -> Terminal, stop iteration
/// - Error(NextTooManyDecodeErrors(_)) -> Terminal, stop iteration
///
/// **Critical Contract**: Always use returned QueryStream - old copies have stale state.
pub fn next(
  stream: QueryStream,
) -> #(Result(StreamItem, NextError), QueryStream) {
  let QueryStream(internal) = stream

  // Fast path: already closed
  case internal.closed {
    True -> #(Ok(EndOfStream), stream)
    False -> next_impl(stream)
  }
}

/// Internal implementation of next() after closed check.
fn next_impl(
  stream: QueryStream,
) -> #(Result(StreamItem, NextError), QueryStream) {
  let QueryStream(internal) = stream

  // Check for PendingEndOfStream state - yield EndOfStream and close
  case internal.state {
    PendingEndOfStream -> {
      let closed = mark_closed(stream)
      #(Ok(EndOfStream), closed)
    }
    _ -> next_receive(stream)
  }
}

/// Receive data from port and process it.
fn next_receive(
  stream: QueryStream,
) -> #(Result(StreamItem, NextError), QueryStream) {
  let QueryStream(internal) = stream

  // First, check if we have a complete line in the buffer
  let #(line_result, new_buffer) = read_line(internal.buffer)
  case line_result {
    CompleteLine(line) -> {
      // Update stream with new buffer and process the line
      let updated = set_buffer(stream, new_buffer)
      process_line(updated, line)
    }
    BufferOverflow -> {
      // Terminal error
      let closed = mark_closed(stream)
      #(Error(NextBufferOverflow), closed)
    }
    ReadError(msg) -> {
      // Treat as decode error
      let #(updated, exceeded) = increment_decode_errors(stream, msg)
      case exceeded {
        True -> {
          let closed = mark_closed(updated)
          #(
            Error(NextTooManyDecodeErrors(
              get_consecutive_decode_errors(closed),
              msg,
            )),
            closed,
          )
        }
        False -> #(Error(NextJsonDecodeError("", msg)), updated)
      }
    }
    NeedMoreData -> {
      // Need to receive more data from port
      let updated = set_buffer(stream, new_buffer)
      receive_from_port(updated)
    }
    // These shouldn't occur from read_line, but handle gracefully
    PortClosed | ExitReceived(_) -> {
      let closed = mark_closed(stream)
      #(Ok(EndOfStream), closed)
    }
  }
}

/// Receive data from the port based on current state's timeout behavior.
fn receive_from_port(
  stream: QueryStream,
) -> #(Result(StreamItem, NextError), QueryStream) {
  let QueryStream(internal) = stream

  // Determine timeout based on state
  let port_result = case internal.state {
    // Streaming: block indefinitely
    Streaming -> port_ffi.receive_blocking(internal.port)
    // ResultReceived: use drain timeout (100ms)
    ResultReceived ->
      port_ffi.receive_timeout(
        internal.port,
        constants.post_result_drain_timeout_ms,
      )
    // PendingEndOfStream/Closed: shouldn't get here, but use minimal timeout
    PendingEndOfStream | Closed -> port_ffi.receive_timeout(internal.port, 0)
  }

  case port_result {
    Ok(Data(bytes)) -> {
      // Append to buffer and try to read a line
      case append_to_buffer(internal.buffer, bytes) {
        Ok(new_buffer) -> {
          let updated = set_buffer(stream, new_buffer)
          next_receive(updated)
        }
        Error(BufferOverflow) -> {
          let closed = mark_closed(stream)
          #(Error(NextBufferOverflow), closed)
        }
        Error(_) -> {
          // Other ReadLineResult errors shouldn't come from append_to_buffer
          let closed = mark_closed(stream)
          #(Error(NextBufferOverflow), closed)
        }
      }
    }
    Ok(ExitStatus(code)) -> {
      // Handle exit status via state machine
      handle_exit_and_yield(stream, code)
    }
    Ok(Timeout) -> {
      // Timeout during drain - treat as end of stream
      case internal.state {
        ResultReceived -> {
          // After result, timeout means nothing more to drain
          // Transition to PendingEndOfStream and let next_impl handle the yield
          let updated = mark_pending_end_of_stream(stream)
          next_impl(updated)
        }
        _ -> {
          // In other states, shouldn't timeout (blocking)
          // But if it happens, treat as end
          let closed = mark_closed(stream)
          #(Ok(EndOfStream), closed)
        }
      }
    }
    Ok(port_ffi.Eof) -> {
      // EOF without exit status - unusual but handle gracefully
      let closed = mark_closed(stream)
      #(Ok(EndOfStream), closed)
    }
    Error(msg) -> {
      // Port error - treat as process error with unknown code
      let diagnostic = diagnose_exit_code(-1, True)
      let closed = mark_closed(stream)
      #(
        Error(NextProcessError(
          -1,
          error.ErrorDiagnostic(
            ..diagnostic,
            exit_code_hint: "Port error: " <> msg,
          ),
        )),
        closed,
      )
    }
  }
}

/// Handle exit status and yield appropriate result.
fn handle_exit_and_yield(
  stream: QueryStream,
  exit_code: Int,
) -> #(Result(StreamItem, NextError), QueryStream) {
  let QueryStream(internal) = stream
  let #(transition, updated_stream) = handle_exit_status(stream, exit_code)

  case transition {
    YieldEndOfStream -> {
      let closed = mark_closed(updated_stream)
      #(Ok(EndOfStream), closed)
    }
    YieldWarningThenEnd(warning) -> {
      // Yield warning, stream stays in PendingEndOfStream
      // Next call will yield EndOfStream
      #(Ok(WarningEvent(warning)), updated_stream)
    }
    YieldProcessError(code) -> {
      // Determine if stdout was empty (buffer is empty and no messages seen)
      let stdout_was_empty = bit_array.byte_size(internal.buffer) == 0
      let diagnostic = diagnose_exit_code(code, stdout_was_empty)
      let closed = mark_closed(updated_stream)
      #(Error(NextProcessError(code, diagnostic)), closed)
    }
  }
}

/// Process a complete line (parse JSON and update state).
fn process_line(
  stream: QueryStream,
  line: String,
) -> #(Result(StreamItem, NextError), QueryStream) {
  // Try to decode the line as a message
  let raw_bytes = bit_array.from_string(line)
  case decoder.decode_message_envelope(line, raw_bytes) {
    Ok(envelope) -> {
      // Reset decode error counter on success
      let updated = reset_decode_errors(stream)

      // Check if this is a Result message -> transition state
      let final_stream = case envelope.message {
        Result(_) -> mark_result_received(updated)
        _ -> updated
      }

      #(Ok(Message(envelope)), final_stream)
    }
    Error(decoder.JsonSyntaxError(msg)) -> {
      // Invalid JSON - increment error counter
      let #(updated, exceeded) = increment_decode_errors(stream, msg)
      case exceeded {
        True -> {
          let closed = mark_closed(updated)
          #(
            Error(NextTooManyDecodeErrors(
              get_consecutive_decode_errors(closed),
              msg,
            )),
            closed,
          )
        }
        False -> #(Error(NextJsonDecodeError(line, msg)), updated)
      }
    }
    Error(decoder.JsonDecodeError(msg)) -> {
      // Valid JSON but missing/invalid fields
      let #(updated, exceeded) = increment_decode_errors(stream, msg)
      case exceeded {
        True -> {
          let closed = mark_closed(updated)
          #(
            Error(NextTooManyDecodeErrors(
              get_consecutive_decode_errors(closed),
              msg,
            )),
            closed,
          )
        }
        False -> #(Error(NextJsonDecodeError(line, msg)), updated)
      }
    }
    Error(decoder.UnexpectedMessageType(_type_str)) -> {
      // Valid JSON but unknown message type - non-terminal
      // Don't increment decode error counter for unknown types (forward compat)
      let updated = reset_decode_errors(stream)
      #(Error(NextUnexpectedMessageError(line)), updated)
    }
  }
}
