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
import claude_agent_sdk/internal/port_ffi.{type Port}

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
pub fn close(stream: QueryStream) -> Nil {
  let QueryStream(internal) = stream
  case internal.closed {
    True -> Nil
    False -> port_ffi.ffi_close_port(internal.port)
  }
}
// ============================================================================
// Placeholder API (to be implemented in later issues)
// ============================================================================

// TODO(casg-j37.7): Implement state machine transitions
// TODO(casg-j37.8): Implement next() iterator
