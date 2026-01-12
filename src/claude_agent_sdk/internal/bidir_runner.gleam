/// Internal bidirectional runner for start_session() path.
/// This module is NOT part of the public API - it's used internally by GenServer.
import claude_agent_sdk/internal/port_ffi.{type Port, type WriteError}

/// Push-based runner for start_session() (bidir mode only).
/// This is INTERNAL - not exported from the public API.
pub type BidirRunner {
  BidirRunner(
    port: Port,
    write: fn(String) -> Result(Nil, WriteError),
    close: fn() -> Nil,
  )
}

/// Error type for BidirRunner operations.
pub type StartError {
  /// Placeholder error - start() not yet implemented.
  NotImplemented
  /// Failed to spawn the CLI process.
  SpawnFailed(String)
}

/// Start a BidirRunner with the given CLI arguments.
///
/// NOTE: This is a stub that returns Error(NotImplemented).
/// Actual implementation will come in a subsequent task.
pub fn start(_args: List(String)) -> Result(BidirRunner, StartError) {
  Error(NotImplemented)
}
