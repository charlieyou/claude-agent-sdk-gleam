/// Stream iteration API for Claude Agent SDK.
///
/// This module provides the core streaming interface for processing Claude CLI responses.
/// Use `next()` to iterate through messages, `close()` to release resources, and the
/// helper functions (`with_stream`, `collect_*`, `fold_stream`) for common patterns.
///
/// ## Quick Start
///
/// ```gleam
/// import claude_agent_sdk
/// import claude_agent_sdk/stream
///
/// // Collect all messages (recommended for simple use cases)
/// let result = claude_agent_sdk.with_stream(
///   claude_agent_sdk.query("Hello!", options),
///   claude_agent_sdk.collect_messages,
/// )
/// ```
///
/// ## Process Ownership Contract
///
/// **Critical**: QueryStream is not process-safe; use from the process that created it.
/// The SDK does not detect cross-process use; violating this contract results in
/// undefined behavior (typically deadlock).
///
/// Behavior is UNDEFINED if:
/// - `next()` is called from a different process than `query()`
/// - `next()` is called from multiple processes concurrently
/// - `close()` is called from a different process than `query()`
///
/// Because `next()` blocks, cross-process cancellation requires an OTP wrapper
/// (see Cancellation Recipe in README).
///
/// ### Mailbox Isolation Invariant
///
/// The Erlang FFI uses selective receive to match only the specific Port reference:
/// ```erlang
/// receive {Port, Msg} -> ... end  % CORRECT - isolates to this port
/// ```
/// This ensures each stream only receives its own port's messages, even when
/// multiple streams exist in the same process.
///
/// ## Type Re-exports
///
/// `StreamItem`, `Warning`, `WarningCode`, and `StreamError` are also available
/// from the main `claude_agent_sdk` module. They are re-exported here for convenience
/// when working exclusively with stream operations.
///
/// For pattern matching on variant types, import the source module:
/// - `claude_agent_sdk/error` for `StreamItem`, `StreamError`, `Warning`, `WarningCode`
import claude_agent_sdk/error
import claude_agent_sdk/internal/stream as internal_stream

// ============================================================================
// Type Re-exports
// ============================================================================

/// Opaque iterator over CLI output.
///
/// QueryStream wraps a BEAM port that communicates with the Claude CLI process.
/// Each call to `next()` advances the iterator and returns the next message or
/// error from the stream.
///
/// ## Process Ownership (Critical)
///
/// **Single-process only. Use from the process that created it.**
///
/// QueryStream is bound to the BEAM process that called `query()`. The underlying
/// port sends messages only to its owning process. Calling `next()`, `close()`,
/// or any stream operation from a different process will:
/// - Never receive port messages (they go to the owner)
/// - Block indefinitely on `next()`
/// - Fail silently on `close()`
///
/// If you need cross-process streaming, have the owner process forward messages
/// or use the `with_stream()` helper to ensure cleanup within the owning process.
///
/// ## Value Semantics
///
/// **Always use the most recent QueryStream returned from `next()`.**
///
/// QueryStream is immutable. Each `next()` call returns a new QueryStream with
/// updated internal state (buffer contents, closed flag, etc.). Using a stale
/// copy may cause missed messages or incorrect state.
///
/// ## Example
///
/// ```gleam
/// case claude_agent_sdk.query("Hello", options) {
///   Ok(stream) -> {
///     // Iterate within the same process
///     let #(result, stream) = stream.next(stream)
///     // Always use the returned stream for the next call
///     stream.close(stream)
///   }
///   Error(err) -> // handle error
/// }
/// ```
pub type QueryStream =
  internal_stream.QueryStream

/// What the stream can yield on success.
///
/// StreamItem represents the three possible outcomes of a successful `next()` call:
/// - `Message(envelope)`: A parsed message from the CLI (assistant response, tool result, etc.)
/// - `WarningEvent(warning)`: A non-fatal warning (continue iterating)
/// - `EndOfStream`: Normal completion marker (stop iterating)
///
/// See `claude_agent_sdk/error` for variant constructors when pattern matching.
pub type StreamItem =
  error.StreamItem

/// Non-fatal warning that can be yielded from the stream.
///
/// Warnings indicate issues that don't prevent stream iteration from continuing.
/// Each warning has a `WarningCode` for programmatic handling and a human-readable message.
///
/// See `claude_agent_sdk/error` for the Warning constructor.
pub type Warning =
  error.Warning

/// Warning codes for categorizing warnings.
///
/// Use these codes for programmatic warning handling rather than parsing message strings.
/// Codes include: `UnparseableCliVersion`, `CleanExitNoResult`, `NonZeroExitAfterResult`,
/// `UnexpectedMessageAfterResult`, `IncompleteLastLine`, `DeprecatedOption`.
///
/// See `claude_agent_sdk/error` for variant constructors when pattern matching.
pub type WarningCode =
  error.WarningCode

/// Errors during stream iteration.
///
/// StreamError indicates problems during `next()` calls. Errors are either:
/// - **Terminal**: Stream is done, stop iterating (`ProcessError`, `BufferOverflow`, `TooManyDecodeErrors`)
/// - **Non-terminal**: Stream continues, call `next()` again (`JsonDecodeError`, `UnexpectedMessageError`)
///
/// Use `error.is_terminal()` to check if iteration should stop.
///
/// See `claude_agent_sdk/error` for variant constructors when pattern matching.
pub type StreamError =
  error.StreamError

/// Result of collecting stream items or messages.
///
/// Contains the collected items/messages along with any warnings and decode errors
/// that occurred during collection. Terminal errors (if any) are returned in the
/// `terminal_error` field.
pub type CollectResult(a) =
  internal_stream.CollectResult(a)

/// Control flow for fold_stream callback.
///
/// Return from the fold callback to control iteration:
/// - `Continue(new_accumulator)`: Continue to the next item with updated state
/// - `Stop(final_value)`: Stop iteration early and return this value
pub type FoldAction(a) =
  internal_stream.FoldAction(a)

// ============================================================================
// Stream Operations
// ============================================================================

/// Advance the stream by one item.
///
/// Reads the next message from the Claude CLI process and returns it along
/// with an updated stream. This is the primary iteration function for
/// processing CLI responses.
///
/// ## Process Ownership (Critical)
///
/// **Process Safety**: Must be called from the same process that called `query()`.
/// Blocks until the next message arrives. Cannot be cancelled from another process.
/// Because `next()` blocks, cross-process cancellation requires an OTP wrapper.
///
/// This function receives messages from the underlying BEAM port. If called
/// from a process other than the one that created the stream, it will block
/// indefinitely because port messages are delivered only to the owning process.
///
/// ## Parameters
///
/// - `stream`: The current QueryStream state. Must be the most recent copy
///   returned from a previous `next()` call.
///
/// ## Returns
///
/// A tuple of `#(Result(StreamItem, StreamError), QueryStream)`:
/// - The Result contains either a StreamItem on success or a StreamError on failure
/// - The QueryStream is the updated stream state (always use this for the next call)
///
/// ## Timeout Behavior by State
///
/// | Stream State           | Timeout Behavior       | Notes                        |
/// |------------------------|------------------------|------------------------------|
/// | Streaming (before Res) | Blocks indefinitely    | User controls via max_turns  |
/// | ResultReceived         | 100ms timeout per call | Drain behavior               |
/// | PendingEndOfStream     | Returns immediately    | Yield EndOfStream            |
/// | Closed                 | Returns immediately    | No I/O needed                |
///
/// ## Return Value Classification
///
/// - `Ok(Message(envelope))` -> call `next()` again
/// - `Ok(WarningEvent(warning))` -> call `next()` again
/// - `Ok(EndOfStream)` -> stop iteration (normal completion)
/// - `Error(JsonDecodeError(_))` -> Non-terminal, call `next()` again
/// - `Error(UnexpectedMessageError(_))` -> Non-terminal, call `next()` again
/// - `Error(ProcessError(_))` -> Terminal, stop iteration
/// - `Error(BufferOverflow)` -> Terminal, stop iteration
/// - `Error(TooManyDecodeErrors(_))` -> Terminal, stop iteration
///
/// Use `error.is_terminal()` to check if an error ends the stream.
///
/// ## Example
///
/// ```gleam
/// fn iterate(stream: QueryStream) -> Nil {
///   let #(result, stream) = stream.next(stream)
///   case result {
///     Ok(stream.Message(envelope)) -> {
///       // Process message
///       iterate(stream)  // Use updated stream
///     }
///     Ok(stream.EndOfStream) -> stream.close(stream)
///     Error(err) if error.is_terminal(err) -> stream.close(stream)
///     Error(_) -> iterate(stream)  // Non-terminal, continue
///   }
/// }
/// ```
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
/// Closes the underlying BEAM port immediately, sending an exit signal to
/// the Claude CLI process. This is an abort operation—any messages remaining
/// in the stream are discarded.
///
/// ## Process Ownership (Critical)
///
/// **Process Safety**: `close()` is only effective when called by the process
/// that spawned the query.
///
/// The BEAM port is owned by the process that called `query()`. If `close()`
/// is called from a different process:
/// - The port will NOT be closed (the close message goes to a different process)
/// - The CLI process will continue running
/// - Resources will not be released
/// - A waiting `next()` will NOT be unblocked
///
/// For reliable cleanup across process boundaries, use `with_stream()` or
/// ensure the owning process handles cleanup.
///
/// ## Behavior
///
/// - Closes the port immediately (sends exit signal to CLI process)
/// - Marks the stream as closed
/// - Is idempotent: safe to call multiple times
/// - Discards any unread messages in the stream buffer
///
/// ## Parameters
///
/// - `stream`: The QueryStream to close
///
/// ## Returns
///
/// An updated QueryStream with `closed=True`. Always use the returned stream
/// for subsequent operations to maintain correct state.
///
/// ## Example
///
/// ```gleam
/// // Manual cleanup
/// let stream = stream.close(stream)
///
/// // Preferred: use with_stream for automatic cleanup
/// with_stream(query(prompt, options), fn(stream) {
///   // Stream is automatically closed when this returns
///   Ok(result)
/// })
/// ```
pub fn close(stream: QueryStream) -> QueryStream {
  internal_stream.close(stream)
}

/// Check if the stream is closed.
///
/// Returns `True` if the stream has been closed (either explicitly via
/// `close()` or implicitly when iteration completes).
///
/// ## Parameters
///
/// - `stream`: The QueryStream to check
///
/// ## Returns
///
/// `True` if the stream is closed, `False` if it's still active.
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
pub const to_yielder = internal_stream.to_yielder
