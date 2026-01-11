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
  type ErrorDiagnostic, type StreamError, type Warning, CleanExitNoResult,
  IncompleteLastLine, NonZeroExitAfterResult, UnexpectedMessageAfterResult,
  Warning, diagnose_exit_code,
}
import claude_agent_sdk/internal/constants
import claude_agent_sdk/internal/decoder
import claude_agent_sdk/internal/port_ffi.{type Port, Data, ExitStatus, Timeout}
import claude_agent_sdk/message.{type MessageEnvelope, Result}
import claude_agent_sdk/runner.{type Handle, type Runner}
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam/yielder.{type Yielder, Done, Next}

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
// StreamSource: Abstraction over port or runner-based stream
// ============================================================================

/// Internal abstraction for the underlying stream source.
/// Either a direct BEAM port (production) or a Runner+Handle (testing).
type StreamSource {
  /// Direct BEAM port handle (production use)
  PortSource(Port)
  /// Runner with handle for mock-based testing
  RunnerSource(runner: Runner, handle: Handle)
}

// ============================================================================
// QueryStreamInternal: Internal state record
// ============================================================================

/// Internal state for QueryStream.
/// Not exported - only accessible via QueryStream opaque type.
type QueryStreamInternal {
  QueryStreamInternal(
    /// Stream source: either Port or Runner+Handle
    source: StreamSource,
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
    /// Pending warnings to emit before normal stream processing.
    /// Used for initial warnings (e.g., UnparseableCliVersion in permissive mode).
    pending_warnings: List(Warning),
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
// Constructors
// ============================================================================

/// Create a new QueryStream from a port.
/// Internal only - called by query() after successful spawn.
pub fn new(port: Port) -> QueryStream {
  new_with_warnings(port, [])
}

/// Create a new QueryStream from a port with initial warnings.
/// Used when permissive mode allows unknown CLI versions.
pub fn new_with_warnings(port: Port, warnings: List(Warning)) -> QueryStream {
  QueryStream(QueryStreamInternal(
    source: PortSource(port),
    buffer: <<>>,
    state: Streaming,
    consecutive_decode_errors: 0,
    drain_count: 0,
    closed: False,
    result_seen: False,
    pending_warnings: warnings,
  ))
}

/// Create a new QueryStream from a Runner and Handle.
/// Used for testing with test_runner() to avoid spawning real subprocesses.
pub fn new_from_runner(runner: Runner, handle: Handle) -> QueryStream {
  new_from_runner_with_warnings(runner, handle, [])
}

/// Create a new QueryStream from a Runner and Handle with initial warnings.
/// Used for testing when permissive mode allows unknown CLI versions.
pub fn new_from_runner_with_warnings(
  runner: Runner,
  handle: Handle,
  warnings: List(Warning),
) -> QueryStream {
  QueryStream(QueryStreamInternal(
    source: RunnerSource(runner:, handle:),
    buffer: <<>>,
    state: Streaming,
    consecutive_decode_errors: 0,
    drain_count: 0,
    closed: False,
    result_seen: False,
    pending_warnings: warnings,
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
  // Delegate to mark_closed which handles both state update and actual cleanup
  mark_closed(stream)
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

/// Increment the drain counter (when data arrives after Result).
/// Returns the updated stream and whether the limit was exceeded.
pub fn increment_drain_count(stream: QueryStream) -> #(QueryStream, Bool) {
  let QueryStream(internal) = stream
  let new_count = internal.drain_count + 1
  let limit_exceeded = new_count > constants.post_result_drain_max_iterations
  let updated =
    QueryStream(QueryStreamInternal(..internal, drain_count: new_count))
  #(updated, limit_exceeded)
}

/// Transition to PendingEndOfStream state (for draining after ResultReceived).
pub fn mark_pending_end_of_stream(stream: QueryStream) -> QueryStream {
  let QueryStream(internal) = stream
  QueryStream(QueryStreamInternal(..internal, state: PendingEndOfStream))
}

/// Transition to Closed state (terminal) and perform cleanup.
/// Idempotent: returns unchanged stream if already closed.
/// This function both updates state AND performs actual resource cleanup.
pub fn mark_closed(stream: QueryStream) -> QueryStream {
  let QueryStream(internal) = stream
  case internal.closed {
    True -> stream
    False -> {
      // Perform actual cleanup based on source type
      case internal.source {
        PortSource(port) -> port_ffi.ffi_close_port(port)
        RunnerSource(runner:, handle:) -> runner.close(runner, handle)
      }
      QueryStream(QueryStreamInternal(..internal, state: Closed, closed: True))
    }
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

  // First, check for pending warnings - yield them before normal processing
  case internal.pending_warnings {
    [warning, ..rest] -> {
      let updated =
        QueryStream(QueryStreamInternal(..internal, pending_warnings: rest))
      #(Ok(WarningEvent(warning)), updated)
    }
    [] ->
      // No pending warnings, check for PendingEndOfStream state
      case internal.state {
        PendingEndOfStream -> {
          let closed = mark_closed(stream)
          #(Ok(EndOfStream), closed)
        }
        _ -> next_receive(stream)
      }
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
      // Need to receive more data from source
      let updated = set_buffer(stream, new_buffer)
      receive_from_source(updated)
    }
    // These shouldn't occur from read_line, but handle gracefully
    PortClosed | ExitReceived(_) -> {
      let closed = mark_closed(stream)
      #(Ok(EndOfStream), closed)
    }
  }
}

/// Receive data from the source based on current state's timeout behavior.
fn receive_from_source(
  stream: QueryStream,
) -> #(Result(StreamItem, NextError), QueryStream) {
  let QueryStream(internal) = stream

  // Read from source (port or runner)
  let read_result = case internal.source {
    PortSource(port) -> {
      // Determine timeout based on state
      let port_result = case internal.state {
        // Streaming: block indefinitely
        Streaming -> port_ffi.receive_blocking(port)
        // ResultReceived: use drain timeout (100ms)
        ResultReceived ->
          port_ffi.receive_timeout(port, constants.post_result_drain_timeout_ms)
        // PendingEndOfStream/Closed: shouldn't get here, but use minimal timeout
        PendingEndOfStream | Closed -> port_ffi.receive_timeout(port, 0)
      }
      // Convert port result to unified format
      case port_result {
        Ok(Data(bytes)) -> SourceData(bytes)
        Ok(ExitStatus(code)) -> SourceExitStatus(code)
        Ok(Timeout) -> SourceTimeout
        Ok(port_ffi.Eof) -> SourceEof
        Error(msg) -> SourceError(msg)
      }
    }
    RunnerSource(runner:, handle:) -> {
      // Runner doesn't support timeout, but mock can simulate it
      case runner.read_next(runner, handle) {
        runner.Data(bytes) -> SourceData(bytes)
        runner.ExitStatus(code) -> SourceExitStatus(code)
        runner.Eof -> SourceEof
        runner.ReadError(msg) -> SourceError(msg)
      }
    }
  }

  // Process the read result
  case read_result {
    SourceData(bytes) -> {
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
    SourceExitStatus(code) -> {
      // Handle exit status via state machine
      handle_exit_and_yield(stream, code)
    }
    SourceTimeout -> {
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
    SourceEof -> {
      // EOF without exit status - unusual but handle gracefully
      let closed = mark_closed(stream)
      #(Ok(EndOfStream), closed)
    }
    SourceError(msg) -> {
      // Source error - treat as process error with unknown code
      let diagnostic = diagnose_exit_code(-1, True)
      let closed = mark_closed(stream)
      #(
        Error(NextProcessError(
          -1,
          error.ErrorDiagnostic(
            ..diagnostic,
            exit_code_hint: "Source error: " <> msg,
          ),
        )),
        closed,
      )
    }
  }
}

/// Internal type for unified read results from any source.
type SourceReadResult {
  SourceData(BitArray)
  SourceExitStatus(Int)
  SourceTimeout
  SourceEof
  SourceError(String)
}

/// Handle exit status and yield appropriate result.
///
/// Incomplete buffer handling (from plan):
/// - exit=0, incomplete buffer: WarningEvent(IncompleteLastLine), then EndOfStream
/// - exit!=0, incomplete buffer: ProcessError with last_non_json_line populated
fn handle_exit_and_yield(
  stream: QueryStream,
  exit_code: Int,
) -> #(Result(StreamItem, NextError), QueryStream) {
  let QueryStream(internal) = stream
  let buffer_size = bit_array.byte_size(internal.buffer)
  let has_incomplete_line = buffer_size > 0
  let #(transition, updated_stream) = handle_exit_status(stream, exit_code)

  case transition {
    YieldEndOfStream -> {
      let closed = mark_closed(updated_stream)
      #(Ok(EndOfStream), closed)
    }
    YieldWarningThenEnd(warning) -> {
      // Check if we have an incomplete line in buffer that overrides the warning
      case has_incomplete_line, warning.code {
        // exit=0, no result, incomplete buffer -> IncompleteLastLine instead of CleanExitNoResult
        True, CleanExitNoResult -> {
          let incomplete_line = case bit_array.to_string(internal.buffer) {
            Ok(s) -> s
            Error(_) -> "<invalid utf-8>"
          }
          let incomplete_warning =
            Warning(
              code: IncompleteLastLine(incomplete_line),
              message: "CLI exited cleanly but buffer contained incomplete line (no trailing newline)",
              context: None,
            )
          #(Ok(WarningEvent(incomplete_warning)), updated_stream)
        }
        // Other warnings pass through unchanged
        _, _ -> #(Ok(WarningEvent(warning)), updated_stream)
      }
    }
    YieldProcessError(code) -> {
      // Determine if stdout was empty (buffer is empty)
      let stdout_was_empty = buffer_size == 0
      let base_diagnostic = diagnose_exit_code(code, stdout_was_empty)
      // If buffer has incomplete line, include it in diagnostic
      let diagnostic = case has_incomplete_line {
        True -> {
          let last_line = case bit_array.to_string(internal.buffer) {
            Ok(s) -> Some(s)
            Error(_) -> Some("<invalid utf-8>")
          }
          error.ErrorDiagnostic(
            ..base_diagnostic,
            last_non_json_line: last_line,
          )
        }
        False -> base_diagnostic
      }
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
  // Check if we're in ResultReceived state - any data after Result is unexpected
  case get_state(stream) {
    ResultReceived -> {
      // Increment drain count and check if limit exceeded
      let #(updated, limit_exceeded) = increment_drain_count(stream)
      case limit_exceeded {
        True -> {
          // Exceeded drain limit, transition to end of stream
          let final_stream = mark_pending_end_of_stream(updated)
          #(Ok(EndOfStream), final_stream)
        }
        False -> {
          // Still within limit, emit warning and continue
          // Reset decode error counter since we're handling this as a warning
          let final_stream = reset_decode_errors(updated)
          let warning =
            Warning(
              code: UnexpectedMessageAfterResult,
              message: "Received data after Result message",
              context: Some(line),
            )
          #(Ok(WarningEvent(warning)), final_stream)
        }
      }
    }
    _ -> {
      // Normal processing: try to decode the line as a message
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
  }
}

// ============================================================================
// CollectResult: Result of collecting stream items
// ============================================================================

/// Result of collecting items from a stream.
/// Contains all collected items plus any warnings and errors encountered.
pub type CollectResult(a) {
  CollectResult(
    /// Items collected from the stream
    items: List(a),
    /// Non-fatal warnings emitted during iteration
    warnings: List(Warning),
    /// Non-terminal errors (JsonDecodeError, UnexpectedMessageError)
    non_terminal_errors: List(StreamError),
    /// Terminal error if stream ended with an error (ProcessError, etc.)
    terminal_error: Option(StreamError),
  )
}

// ============================================================================
// FoldAction: Control flow for fold_stream
// ============================================================================

/// Control flow for fold_stream callback.
pub type FoldAction(a) {
  /// Continue folding with new accumulator value
  Continue(a)
  /// Stop folding early with final accumulator value
  Stop(a)
}

// ============================================================================
// Resource Helpers (STABLE API)
// ============================================================================

/// Convert NextError to StreamError for collect/fold APIs.
fn next_error_to_stream_error(err: NextError) -> StreamError {
  case err {
    NextProcessError(code, diagnostic) -> error.ProcessError(code, diagnostic)
    NextBufferOverflow -> error.BufferOverflow
    NextTooManyDecodeErrors(count, last) ->
      error.TooManyDecodeErrors(count, last)
    NextJsonDecodeError(line, msg) -> error.JsonDecodeError(line, msg)
    NextUnexpectedMessageError(raw) -> error.UnexpectedMessageError(raw)
  }
}

/// Execute a function with a stream, ensuring cleanup on any exit path.
///
/// **Guarantees**: close() is always called on the stream, whether the
/// function returns normally, returns early, or panics (within Gleam's
/// non-exception semantics).
///
/// **Usage**:
/// ```gleam
/// let result = with_stream(query(prompt, options), fn(stream) {
///   // Use stream here...
///   my_result
/// })
/// ```
///
/// **STABLE API**: Breaking changes require major version bump.
pub fn with_stream(
  result: Result(QueryStream, error.QueryError),
  f: fn(QueryStream) -> a,
) -> Result(a, error.QueryError) {
  case result {
    Error(e) -> Error(e)
    Ok(stream) -> {
      // Use rescue to catch panics and ensure cleanup
      let callback_result = port_ffi.rescue(fn() { f(stream) })
      // Always close, regardless of how f returns (including panic)
      let _ = close(stream)
      // Re-raise if callback panicked
      case callback_result {
        Ok(value) -> Ok(value)
        Error(panic_msg) -> panic as panic_msg
      }
    }
  }
}

/// Collect all items from a stream.
///
/// Consumes the entire stream, collecting all messages and warnings.
/// Non-terminal errors are recorded but iteration continues.
/// Terminal errors stop iteration and are recorded in terminal_error.
///
/// **Guarantees**: Stream is always closed after collection.
///
/// **STABLE API**: Breaking changes require major version bump.
pub fn collect_items(stream: QueryStream) -> CollectResult(StreamItem) {
  collect_items_loop(stream, [], [], [])
}

fn collect_items_loop(
  stream: QueryStream,
  items: List(StreamItem),
  warnings: List(Warning),
  non_terminal_errors: List(StreamError),
) -> CollectResult(StreamItem) {
  let #(result, updated) = next(stream)
  case result {
    Ok(EndOfStream) ->
      CollectResult(
        items: list.reverse(items),
        warnings: list.reverse(warnings),
        non_terminal_errors: list.reverse(non_terminal_errors),
        terminal_error: None,
      )
    Ok(Message(envelope)) ->
      collect_items_loop(
        updated,
        [Message(envelope), ..items],
        warnings,
        non_terminal_errors,
      )
    Ok(WarningEvent(warning)) ->
      collect_items_loop(
        updated,
        [WarningEvent(warning), ..items],
        [warning, ..warnings],
        non_terminal_errors,
      )
    Error(err) -> {
      let stream_error = next_error_to_stream_error(err)
      case error.is_terminal(stream_error) {
        True ->
          CollectResult(
            items: list.reverse(items),
            warnings: list.reverse(warnings),
            non_terminal_errors: list.reverse(non_terminal_errors),
            terminal_error: Some(stream_error),
          )
        False ->
          collect_items_loop(updated, items, warnings, [
            stream_error,
            ..non_terminal_errors
          ])
      }
    }
  }
}

/// Collect only messages from a stream (filtering out warnings).
///
/// Consumes the entire stream, collecting only MessageEnvelope items.
/// Warnings are tracked separately. Non-terminal errors are recorded
/// but iteration continues.
///
/// **Guarantees**: Stream is always closed after collection.
///
/// **STABLE API**: Breaking changes require major version bump.
pub fn collect_messages(stream: QueryStream) -> CollectResult(MessageEnvelope) {
  collect_messages_loop(stream, [], [], [])
}

fn collect_messages_loop(
  stream: QueryStream,
  messages: List(MessageEnvelope),
  warnings: List(Warning),
  non_terminal_errors: List(StreamError),
) -> CollectResult(MessageEnvelope) {
  let #(result, updated) = next(stream)
  case result {
    Ok(EndOfStream) ->
      CollectResult(
        items: list.reverse(messages),
        warnings: list.reverse(warnings),
        non_terminal_errors: list.reverse(non_terminal_errors),
        terminal_error: None,
      )
    Ok(Message(envelope)) ->
      collect_messages_loop(
        updated,
        [envelope, ..messages],
        warnings,
        non_terminal_errors,
      )
    Ok(WarningEvent(warning)) ->
      collect_messages_loop(
        updated,
        messages,
        [warning, ..warnings],
        non_terminal_errors,
      )
    Error(err) -> {
      let stream_error = next_error_to_stream_error(err)
      case error.is_terminal(stream_error) {
        True ->
          CollectResult(
            items: list.reverse(messages),
            warnings: list.reverse(warnings),
            non_terminal_errors: list.reverse(non_terminal_errors),
            terminal_error: Some(stream_error),
          )
        False ->
          collect_messages_loop(updated, messages, warnings, [
            stream_error,
            ..non_terminal_errors
          ])
      }
    }
  }
}

/// Fold over stream items with custom accumulation logic.
///
/// Allows early termination via FoldAction.Stop. The callback receives
/// each stream result (Ok or Error) and returns either Continue(acc)
/// or Stop(acc).
///
/// **Guarantees**: Stream is always closed after folding.
///
/// **Returns**: Tuple of (final_accumulator, optional_terminal_error)
///
/// **STABLE API**: Breaking changes require major version bump.
pub fn fold_stream(
  stream: QueryStream,
  initial: a,
  f: fn(a, Result(StreamItem, StreamError)) -> FoldAction(a),
) -> #(a, Option(StreamError)) {
  fold_stream_loop(stream, initial, f)
}

fn fold_stream_loop(
  stream: QueryStream,
  acc: a,
  f: fn(a, Result(StreamItem, StreamError)) -> FoldAction(a),
) -> #(a, Option(StreamError)) {
  let #(result, updated) = next(stream)
  case result {
    Ok(EndOfStream) -> {
      let _ = close(updated)
      #(acc, None)
    }
    Ok(item) -> {
      case f(acc, Ok(item)) {
        Continue(new_acc) -> fold_stream_loop(updated, new_acc, f)
        Stop(final_acc) -> {
          let _ = close(updated)
          #(final_acc, None)
        }
      }
    }
    Error(err) -> {
      let stream_error = next_error_to_stream_error(err)
      case error.is_terminal(stream_error) {
        True -> {
          // Stream already closed by next() on terminal error
          #(acc, Some(stream_error))
        }
        False -> {
          case f(acc, Error(stream_error)) {
            Continue(new_acc) -> fold_stream_loop(updated, new_acc, f)
            Stop(final_acc) -> {
              let _ = close(updated)
              #(final_acc, None)
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// to_yielder: Convert to gleam/yielder for combinator interop
// ============================================================================

/// Convert a QueryStream to a Yielder for gleam/yielder combinator interop.
///
/// **WARNING: Leak Risk**
/// Unlike `with_stream()`, `collect_*()`, and `fold_stream()`, this function
/// does NOT guarantee cleanup. The port resource may leak if:
/// - The yielder is not fully consumed
/// - An exception occurs during iteration
/// - Early break from iteration (e.g., via `take()`)
///
/// **Preferred Alternatives**:
/// - Use `with_stream()` for guaranteed cleanup with early termination
/// - Use `collect_items()` or `collect_messages()` for full consumption
/// - Use `fold_stream()` for accumulation with early stop
///
/// **Use to_yielder() only when**:
/// - You need gleam/yielder combinators (map, filter, zip, etc.)
/// - Cleanup is handled externally (e.g., supervisor process)
/// - You guarantee full consumption of the yielder
///
pub fn to_yielder(
  stream: QueryStream,
) -> Yielder(Result(StreamItem, StreamError)) {
  yielder.unfold(from: stream, with: fn(s) {
    case next(s) {
      #(Ok(EndOfStream), _updated) -> Done
      #(Ok(item), updated) -> Next(element: Ok(item), accumulator: updated)
      #(Error(err), updated) -> {
        let stream_error = next_error_to_stream_error(err)
        // Stream already closed by next() on terminal error (see fold_stream_loop pattern)
        Next(element: Error(stream_error), accumulator: updated)
      }
    }
  })
}
