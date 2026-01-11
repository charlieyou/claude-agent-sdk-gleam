/// Runner: Abstraction layer for CLI process execution.
///
/// This module provides the `Runner` type that abstracts over process execution,
/// enabling unit testing of stream semantics without spawning actual OS processes.
///
/// ## Purpose
///
/// In production, the SDK spawns the Claude CLI directly via BEAM ports. For testing,
/// you can provide a mock `Runner` that simulates CLI behavior, allowing you to:
/// - Test stream iteration logic without external dependencies
/// - Simulate various CLI outputs, errors, and edge cases
/// - Run tests deterministically without network or filesystem access
///
/// ## Design
///
/// The Runner is an opaque function record with three operations:
/// - `spawn`: Start a process and return a handle
/// - `read_next`: Read the next chunk of output from a handle
/// - `close`: Close a handle and clean up resources
///
/// ## Usage
///
/// Create a test runner with `test_runner()` and pass it to `with_test_mode()`:
///
/// ```gleam
/// let runner = runner.test_runner(
///   on_spawn: fn(_, _, _) { Ok(dynamic.from(my_state)) },
///   on_read: fn(handle) { runner.Data(<<"{...}\n":utf8>>) },
///   on_close: fn(_) { Nil },
/// )
/// let opts = options.default_options() |> options.with_test_mode(runner)
/// ```
import gleam/dynamic.{type Dynamic}

/// Result of reading from a process handle.
///
/// Returned by the `on_read` callback in test runners and internally by the
/// production port reader. Each variant represents a different outcome from
/// reading the next chunk of process output.
pub type ReadResult {
  /// Data chunk received from process stdout.
  /// The BitArray contains raw bytes that may or may not be newline-terminated.
  Data(BitArray)
  /// Process exited with the given status code.
  /// Exit code 0 typically indicates success; non-zero indicates failure.
  ExitStatus(Int)
  /// An error occurred during read.
  /// Contains an error message describing what went wrong.
  ReadError(String)
  /// End of file reached (port closed without exit status).
  Eof
}

/// Opaque handle type for process references.
///
/// This type is opaque to prevent test code from constructing handles directly.
/// Handles are created by spawn() and used with read_next() and close().
/// The internal variants distinguish real Erlang ports from test mocks.
pub opaque type Handle {
  /// Handle wrapping a real BEAM port (future implementation)
  ErlangHandle(Dynamic)
  /// Handle wrapping test-provided state (Dynamic for type erasure)
  TestHandle(Dynamic)
}

/// Opaque type representing a process runner.
///
/// A Runner encapsulates the three operations needed to manage a subprocess:
/// spawning, reading output, and closing. The opaque type prevents direct
/// construction, ensuring callers use either the real CLI runner (future)
/// or test_runner() for testing.
pub opaque type Runner {
  Runner(
    spawn: fn(String, List(String), String) -> Result(Handle, String),
    read_next: fn(Handle) -> ReadResult,
    close: fn(Handle) -> Nil,
  )
}

/// Create a test runner with user-provided callbacks.
///
/// The callbacks receive/return Dynamic values to allow tests to use their
/// own state types (cast to/from Dynamic). Internally, the returned values
/// are wrapped in TestHandle to distinguish them from real Erlang handles.
///
/// ## ETS State Pattern
///
/// Tests typically use ETS with unique ref keys for concurrency-safe state:
/// - on_spawn: Create ref, insert initial state into ETS, return ref
/// - on_read: Lookup state by ref, update ETS, return next ReadResult
/// - on_close: Delete ref from ETS
///
/// ## Example
///
/// ```gleam
/// // Test that simulates CLI producing two JSON lines then exiting
/// let runner = test_runner(
///   on_spawn: fn(_cmd, _args, _cwd) {
///     let ref = ets_helpers.make_ref()
///     ets_helpers.insert(table, ref, initial_state)
///     Ok(ref)
///   },
///   on_read: fn(handle) {
///     case ets_helpers.lookup(table, handle) {
///       option.Some(state) -> // return next ReadResult based on state
///       option.None -> ReadError("Handle not found")
///     }
///   },
///   on_close: fn(handle) {
///     ets_helpers.delete(table, handle)
///     Nil
///   },
/// )
/// ```
pub fn test_runner(
  on_spawn on_spawn: fn(String, List(String), String) -> Result(Dynamic, String),
  on_read on_read: fn(Dynamic) -> ReadResult,
  on_close on_close: fn(Dynamic) -> Nil,
) -> Runner {
  Runner(
    spawn: fn(cmd, args, cwd) {
      case on_spawn(cmd, args, cwd) {
        Ok(handle_data) -> Ok(TestHandle(handle_data))
        Error(msg) -> Error(msg)
      }
    },
    read_next: fn(handle) {
      case handle {
        TestHandle(data) -> on_read(data)
        ErlangHandle(_) ->
          ReadError("ErlangHandle not supported in test runner")
      }
    },
    close: fn(handle) {
      case handle {
        TestHandle(data) -> on_close(data)
        ErlangHandle(_) -> Nil
      }
    },
  )
}

// ============================================================================
// Internal accessors (used by stream implementation)
// ============================================================================

/// Spawn a process using the runner.
///
/// Returns a Result containing either the internal handle or an error message.
/// The handle is opaque and can only be used with read_next and close.
///
/// Internal only - used by stream implementation.
@internal
pub fn spawn(
  runner: Runner,
  command: String,
  args: List(String),
  cwd: String,
) -> Result(Handle, String) {
  runner.spawn(command, args, cwd)
}

/// Read the next chunk of output from a handle.
///
/// Internal only - used by stream implementation.
@internal
pub fn read_next(runner: Runner, handle: Handle) -> ReadResult {
  runner.read_next(handle)
}

/// Close a handle and release associated resources.
///
/// Internal only - used by stream implementation.
@internal
pub fn close(runner: Runner, handle: Handle) -> Nil {
  runner.close(handle)
}
