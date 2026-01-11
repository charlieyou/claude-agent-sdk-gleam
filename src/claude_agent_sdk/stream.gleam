/// Public stream module for Claude Agent SDK.
///
/// This module exposes the stream iteration API without leaking internal implementation.
/// Users can `import claude_agent_sdk/stream.{next, close, QueryStream, StreamItem}`.
///
/// **Note**: `StreamItem`, `Warning`, `WarningCode`, and `StreamError` are also available
/// from the main `claude_agent_sdk` module. They are re-exported here for convenience
/// when working exclusively with stream operations.
import claude_agent_sdk/error
import claude_agent_sdk/internal/stream as internal_stream

// ============================================================================
// Type Re-exports
// ============================================================================

/// Opaque iterator over CLI output.
///
/// **Ownership**: Always use the most recent copy returned from next()
/// to maintain correct state.
///
/// **Single-process**: Must only be used from the creating process.
pub type QueryStream =
  internal_stream.QueryStream

/// What the stream can yield on success.
pub type StreamItem =
  error.StreamItem

/// Non-fatal warning that can be yielded from the stream.
pub type Warning =
  error.Warning

/// Warning codes for categorizing warnings.
pub type WarningCode =
  error.WarningCode

/// Errors during stream iteration.
pub type StreamError =
  error.StreamError

/// Result of collecting stream items or messages.
pub type CollectResult(a) =
  internal_stream.CollectResult(a)

/// Control flow for fold_stream callback.
pub type FoldAction(a) =
  internal_stream.FoldAction(a)

// ============================================================================
// Stream Operations
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
/// - Error(JsonDecodeError(_)) -> Non-terminal, call next() again
/// - Error(UnexpectedMessageError(_)) -> Non-terminal, call next() again
/// - Error(ProcessError(_)) -> Terminal, stop iteration
/// - Error(BufferOverflow) -> Terminal, stop iteration
/// - Error(TooManyDecodeErrors(_)) -> Terminal, stop iteration
///
/// **Critical Contract**: Always use returned QueryStream - old copies have stale state.
pub fn next(
  stream: QueryStream,
) -> #(Result(StreamItem, StreamError), QueryStream) {
  let #(result, new_stream) = internal_stream.next(stream)

  // Map internal NextError to public StreamError, and internal StreamItem to public StreamItem
  let mapped_result = case result {
    Ok(internal_stream.Message(envelope)) -> Ok(error.Message(envelope))
    Ok(internal_stream.WarningEvent(warning)) -> Ok(error.WarningEvent(warning))
    Ok(internal_stream.EndOfStream) -> Ok(error.EndOfStream)
    Error(internal_stream.NextProcessError(exit_code, diagnostic)) ->
      Error(error.ProcessError(exit_code, diagnostic))
    Error(internal_stream.NextBufferOverflow) -> Error(error.BufferOverflow)
    Error(internal_stream.NextTooManyDecodeErrors(count, last_error)) ->
      Error(error.TooManyDecodeErrors(count, last_error))
    Error(internal_stream.NextJsonDecodeError(line, err)) ->
      Error(error.JsonDecodeError(line, err))
    Error(internal_stream.NextUnexpectedMessageError(raw_json)) ->
      Error(error.UnexpectedMessageError(raw_json))
  }

  #(mapped_result, new_stream)
}

/// Close the stream and release resources.
///
/// This function:
/// - Closes the port immediately (sends exit signal to CLI process)
/// - Marks the stream as closed
/// - Is idempotent: safe to call multiple times
///
/// **Note**: close() performs a user-initiated abort and does NOT parse/emit
/// any remaining messages in the mailbox.
///
/// Returns an updated QueryStream with closed=True. Always use the returned
/// stream for subsequent operations to maintain correct state.
pub fn close(stream: QueryStream) -> QueryStream {
  internal_stream.close(stream)
}

/// Check if the stream is closed.
pub fn is_closed(stream: QueryStream) -> Bool {
  internal_stream.is_closed(stream)
}

// ============================================================================
// Resource Helpers
// ============================================================================

/// Execute a function with a stream, ensuring proper cleanup.
///
/// The callback receives the stream and can iterate, abort early, or do any
/// stream operations. When the callback returns, close() is called automatically.
///
/// Takes a Result containing either a QueryStream or QueryError (as returned
/// by query()). If the result is an error, returns that error immediately.
///
/// ## Example
///
/// ```gleam
/// let result = with_stream(query(prompt, options), fn(stream) {
///   // Do something with the stream
///   // Stream is automatically closed when this returns
///   Ok(42)
/// })
/// ```
pub const with_stream = internal_stream.with_stream

/// Collect all items from the stream into a list.
///
/// Iterates through the entire stream, collecting all Message and WarningEvent
/// items until EndOfStream or a terminal error. Returns all collected items
/// along with any warnings and decode errors encountered.
pub const collect_items = internal_stream.collect_items

/// Collect all messages from the stream.
///
/// Similar to collect_items but extracts only MessageEnvelope values,
/// filtering out WarningEvents and EndOfStream markers.
pub const collect_messages = internal_stream.collect_messages

/// Fold over stream items with early termination support.
///
/// Allows early termination via FoldAction.Stop. The callback receives
/// the accumulator and each stream result, returning either Continue(new_acc)
/// or Stop(final_value).
///
/// The stream is automatically closed when iteration ends (either by reaching
/// EndOfStream, encountering a terminal error, or early stop).
pub const fold_stream = internal_stream.fold_stream
