/// Session type for bidirectional mode.
///
/// This module defines the Session opaque type that represents a handle to
/// a bidirectional CLI session. The Session wraps an OTP Subject for
/// communicating with the underlying GenServer actor.
///
/// ## Current Status: Skeleton
///
/// This is the initial type definition. The actual actor implementation
/// will be added in Epic 8 (Bidirectional Protocol).
import gleam/erlang/process.{type Subject}

/// Message type for session actor communication.
/// Will be expanded with actual messages in Epic 8.
pub type SessionMessage {
  /// Placeholder message for type completeness.
  Shutdown
}

/// Opaque session handle for bidirectional mode.
///
/// Wraps a Subject for communicating with the session GenServer.
/// The session actor manages the CLI process, message routing,
/// and hook callbacks.
pub opaque type Session {
  Session(subject: Subject(SessionMessage))
}

/// Get the underlying subject from a session (internal use only).
pub fn get_subject(session: Session) -> Subject(SessionMessage) {
  session.subject
}
