/// Test helpers for mocking BidirRunner.
/// Provides utilities for injecting messages and capturing writes in tests.
import claude_agent_sdk/internal/bidir_runner.{type BidirRunner}
import claude_agent_sdk/internal/port_ffi.{type WriteError}
import gleam/erlang/process.{type Subject}

/// A mock runner with message injection capability.
/// The injection subject allows sending messages that simulate port output.
pub type MockRunner {
  MockRunner(
    runner: BidirRunner,
    /// Subject for injecting BitArray data (simulating port output)
    inject: Subject(BitArray),
    /// Subject for capturing write() calls
    writes: Subject(String),
    /// Subject for notifying when close() is called
    closed: Subject(Bool),
  )
}

/// Create a mock BidirRunner with injection and capture capabilities.
///
/// Returns a MockRunner containing:
/// - `runner`: The BidirRunner to use in tests
/// - `inject`: Subject to send BitArray data (simulates port output)
/// - `writes`: Subject that receives all write() call data
/// - `closed`: Subject that receives True when close() is called
///
/// Example:
/// ```gleam
/// let mock = mock_bidir_runner.new()
/// // Use mock.runner with GenServer
/// // Inject a message:
/// process.send(mock.inject, <<"hello":utf8>>)
/// // Check writes:
/// let assert Ok(data) = process.receive(mock.writes, 100)
/// ```
pub fn new() -> MockRunner {
  let inject = process.new_subject()
  let writes = process.new_subject()
  let closed = process.new_subject()

  let runner =
    bidir_runner.mock(
      on_write: fn(data) -> Result(Nil, WriteError) {
        process.send(writes, data)
        Ok(Nil)
      },
      on_close: fn() { process.send(closed, True) },
    )

  MockRunner(runner: runner, inject: inject, writes: writes, closed: closed)
}

/// Create a mock runner with custom write behavior.
/// Allows tests to simulate write errors or other custom behavior.
pub fn new_with_write(
  on_write: fn(String) -> Result(Nil, WriteError),
) -> MockRunner {
  let inject = process.new_subject()
  let writes = process.new_subject()
  let closed = process.new_subject()

  let runner =
    bidir_runner.mock(on_write: on_write, on_close: fn() {
      process.send(closed, True)
    })

  MockRunner(runner: runner, inject: inject, writes: writes, closed: closed)
}
