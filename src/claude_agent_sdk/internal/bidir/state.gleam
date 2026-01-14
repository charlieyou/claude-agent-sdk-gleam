/// Pure state types and transition logic for bidirectional sessions.
///
/// This module contains the pure, OTP-independent state machine for session
/// lifecycle management. All types and functions here are safe to use from
/// other pure modules.
///
/// ## Module Boundaries
///
/// - **state.gleam** (this file): Pure types (SessionLifecycle) and the
///   transition function. Re-exports SessionError from bidir.gleam.
/// - **bidir.gleam**: OTP actor that uses these types with Process/Subject deps
///
/// ## Note on SessionError
///
/// SessionError is defined in bidir.gleam (the authoritative module) and
/// re-exported here. This avoids duplication while keeping the pure state
/// module usable without directly importing the OTP-dependent bidir module.
import claude_agent_sdk/internal/bidir.{
  type SessionError, CliExitedDuringInit, CliExitedDuringStartup,
  InitializationError, InitializationTimeout, RuntimeError,
}

/// Session lifecycle states.
///
/// - Starting: Actor started, CLI port not yet spawned
/// - InitSent: Initialize request sent, awaiting response
/// - Running: Fully operational, processing messages
/// - Stopped: Clean shutdown completed
/// - Failed: Unrecoverable error occurred
pub type SessionLifecycle {
  /// Actor started, port not yet spawned.
  Starting
  /// Initialize request sent to CLI, awaiting response.
  InitSent
  /// Fully operational, processing messages.
  Running
  /// Clean shutdown completed.
  Stopped
  /// Unrecoverable error occurred.
  Failed(SessionError)
}

/// Stub for SessionState type.
///
/// The full SessionState type is defined in bidir.gleam and contains
/// OTP-dependent fields (Subject, Pid). This stub exists to satisfy the
/// module structure requirement; the actual implementation remains in
/// bidir.gleam until T003 (actor extraction).
pub type SessionState {
  /// Placeholder - actual state is in bidir.gleam.
  SessionStateStub
}

/// Events that trigger lifecycle state transitions.
pub type LifecycleEvent {
  /// CLI process spawned successfully.
  CliSpawned
  /// Initialize request succeeded.
  InitSuccess
  /// Initialize request timed out.
  InitTimeout
  /// Stop was requested by the SDK user.
  StopRequested
  /// CLI port closed unexpectedly.
  PortClosed
  /// An error occurred with the given reason.
  ErrorOccurred(String)
}

/// Error returned when an invalid state transition is attempted.
pub type InvalidTransition {
  InvalidTransition(from: SessionLifecycle, event: LifecycleEvent)
}

/// Attempt a lifecycle state transition.
///
/// Returns Ok(new_state) for valid transitions, Error(InvalidTransition) otherwise.
///
/// Valid transitions:
/// - Starting + CliSpawned → InitSent
/// - Starting + ErrorOccurred/PortClosed → Failed
/// - InitSent + InitSuccess → Running
/// - InitSent + ErrorOccurred/InitTimeout/PortClosed → Failed
/// - Running + StopRequested/PortClosed → Stopped
/// - Running + ErrorOccurred → Failed
/// - Stopped/Failed + any → Error (terminal states)
pub fn transition(
  from: SessionLifecycle,
  event: LifecycleEvent,
) -> Result(SessionLifecycle, InvalidTransition) {
  case from, event {
    // Terminal states reject all transitions
    Stopped, _ -> Error(InvalidTransition(from, event))
    Failed(_), _ -> Error(InvalidTransition(from, event))

    // Starting state transitions
    Starting, CliSpawned -> Ok(InitSent)
    Starting, ErrorOccurred(reason) -> Ok(Failed(RuntimeError(reason)))
    Starting, PortClosed -> Ok(Failed(CliExitedDuringStartup))
    Starting, _ -> Error(InvalidTransition(from, event))

    // InitSent state transitions
    InitSent, InitSuccess -> Ok(Running)
    InitSent, ErrorOccurred(reason) -> Ok(Failed(InitializationError(reason)))
    InitSent, InitTimeout -> Ok(Failed(InitializationTimeout))
    InitSent, PortClosed -> Ok(Failed(CliExitedDuringInit))
    InitSent, _ -> Error(InvalidTransition(from, event))

    // Running state transitions
    Running, StopRequested -> Ok(Stopped)
    Running, PortClosed -> Ok(Stopped)
    Running, ErrorOccurred(reason) -> Ok(Failed(RuntimeError(reason)))
    Running, _ -> Error(InvalidTransition(from, event))
  }
}
