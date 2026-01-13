/// Session lifecycle events for bidirectional mode.
///
/// This module defines the SessionEvent type for receiving session lifecycle
/// notifications separate from the message stream. Events indicate session
/// completion, stop, or failure.
import claude_agent_sdk/message.{type ResultMessage}

/// Session lifecycle event variants.
///
/// These events are delivered via a separate Subject from messages,
/// allowing callers to handle lifecycle events independently.
pub type SessionEvent {
  /// Session completed normally with result.
  SessionCompleted(result: ResultMessage)
  /// Session was stopped via stop() call.
  SessionStopped
  /// Session failed with error.
  SessionFailed(error: String)
}
