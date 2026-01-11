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
import claude_agent_sdk/internal/constants
import claude_agent_sdk/internal/port_ffi.{type Port}
import gleam/bit_array
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
      QueryStream(QueryStreamInternal(..internal, closed: True))
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
pub fn append_to_buffer(
  buffer: BitArray,
  data: BitArray,
) -> Result(BitArray, ReadLineResult) {
  let new_buffer = bit_array.append(buffer, data)
  let size = bit_array.byte_size(new_buffer)
  case size > constants.max_line_size_bytes {
    True -> Error(BufferOverflow)
    False -> Ok(new_buffer)
  }
}

/// Try to extract a complete line from the buffer.
/// Returns the line (without newline) and remaining buffer, or NeedMoreData.
pub fn read_line(buffer: BitArray) -> #(ReadLineResult, BitArray) {
  // First normalize CRLF
  let normalized = normalize_crlf(buffer)
  read_line_from_normalized(normalized, 0)
}

fn read_line_from_normalized(
  buffer: BitArray,
  pos: Int,
) -> #(ReadLineResult, BitArray) {
  let size = bit_array.byte_size(buffer)
  case pos >= size {
    True -> #(NeedMoreData, buffer)
    False -> {
      // Check byte at position
      case bit_array.slice(buffer, pos, 1) {
        Ok(<<0x0A>>) -> {
          // Found newline at pos
          case bit_array.slice(buffer, 0, pos) {
            Ok(line_bytes) -> {
              // Get remaining bytes after newline
              let remaining_start = pos + 1
              let remaining_len = size - remaining_start
              let remaining = case
                bit_array.slice(buffer, remaining_start, remaining_len)
              {
                Ok(r) -> r
                Error(_) -> <<>>
              }
              // Decode line as UTF-8
              case bit_array.to_string(line_bytes) {
                Ok(line) -> #(CompleteLine(line), remaining)
                Error(_) -> #(
                  ReadError(
                    "Invalid UTF-8 in line: " <> string.inspect(line_bytes),
                  ),
                  remaining,
                )
              }
            }
            Error(_) -> #(ReadError("Failed to slice line from buffer"), buffer)
          }
        }
        Ok(_) -> read_line_from_normalized(buffer, pos + 1)
        Error(_) -> #(ReadError("Failed to read byte at position"), buffer)
      }
    }
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
// Placeholder API (to be implemented in later issues)
// ============================================================================

// TODO(casg-j37.7): Implement state machine transitions
// TODO(casg-j37.8): Implement next() iterator
