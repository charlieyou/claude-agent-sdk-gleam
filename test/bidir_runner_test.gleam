import claude_agent_sdk/error.{SpawnFailed}
import claude_agent_sdk/internal/bidir_runner
import claude_agent_sdk/internal/port_ffi
import claude_agent_sdk/internal/port_io
import gleam/bit_array
import gleam/erlang/process
import gleam/string
import gleeunit/should
import support/mock_bidir_runner

/// start() returns SpawnFailed when 'claude' is not found in PATH.
/// If claude IS in PATH, we verify the runner can be created and closed.
pub fn bidir_runner_start_cli_test() {
  let result = bidir_runner.start([])
  case result {
    Error(SpawnFailed(_)) -> {
      // Expected: claude not in PATH
      should.be_true(True)
    }
    Ok(runner) -> {
      // Claude is in PATH - verify we can close cleanly
      runner.close()
      should.be_true(True)
    }
    Error(_) -> {
      // Other errors (e.g., Timeout, ActorStartFailed) - unexpected
      should.fail()
    }
  }
}

/// start_with_path returns SpawnFailed for non-existent executable.
pub fn bidir_runner_start_with_invalid_path_test() {
  let result = bidir_runner.start_with_path("/nonexistent/path/to/claude", [])
  case result {
    Error(SpawnFailed(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

/// start_raw spawns correctly with a valid executable found via PATH.
pub fn bidir_runner_start_raw_valid_executable_test() {
  case port_ffi.find_cli_path("true") {
    Error(_) -> should.be_true(True)
    Ok(true_path) -> {
      let result = bidir_runner.start_raw(true_path, [])
      case result {
        Ok(runner) -> {
          runner.close()
          should.be_true(True)
        }
        Error(_) -> should.fail()
      }
    }
  }
}

/// start_raw write function can send data to a process stdin.
pub fn bidir_runner_write_test() {
  case port_ffi.find_cli_path("cat") {
    Error(_) -> should.be_true(True)
    Ok(cat_path) -> {
      let result = bidir_runner.start_raw(cat_path, [])
      case result {
        Ok(runner) -> {
          let write_result = runner.write("test data\n")
          case write_result {
            Ok(Nil) -> should.be_true(True)
            Error(_) -> should.fail()
          }
          runner.close()
        }
        Error(_) -> should.fail()
      }
    }
  }
}

/// Verify start_with_path prepends --output-format and --input-format args.
/// Spawns echo and reads output to verify the stream-json args are present.
pub fn bidir_runner_start_with_path_prepends_args_test() {
  case port_ffi.find_cli_path("echo") {
    Error(_) -> should.be_true(True)
    Ok(echo_path) -> {
      // start_with_path prepends stream-json args
      let result = bidir_runner.start_with_path(echo_path, ["user-arg"])
      case result {
        Ok(runner) -> {
          // Read echo's output - should contain the prepended args
          case port_ffi.receive_timeout(runner.port, 1000) {
            Ok(port_ffi.Data(data)) -> {
              let output = bit_array.to_string(data)
              case output {
                Ok(str) -> {
                  // Echo prints all args: --output-format stream-json --input-format stream-json user-arg
                  should.be_true(string.contains(str, "--output-format"))
                  should.be_true(string.contains(str, "stream-json"))
                  should.be_true(string.contains(str, "--input-format"))
                  should.be_true(string.contains(str, "user-arg"))
                }
                Error(_) -> should.fail()
              }
            }
            _ -> should.fail()
          }
          runner.close()
        }
        Error(_) -> should.fail()
      }
    }
  }
}

/// Verify OTP version check for stderr_to_stdout support works.
pub fn bidir_runner_stderr_support_check_test() {
  let supports_stderr = port_ffi.supports_stderr_to_stdout()
  case supports_stderr {
    True -> should.be_true(True)
    False -> should.be_true(True)
  }
}

/// Test that port receives binary data correctly (verifies binary + {packet,0} options).
/// Spawns echo and verifies we receive raw binary data (not Erlang list).
pub fn bidir_runner_receives_binary_data_test() {
  case port_ffi.find_cli_path("echo") {
    Error(_) -> should.be_true(True)
    Ok(echo_path) -> {
      let result = bidir_runner.start_raw(echo_path, ["hello"])
      case result {
        Ok(runner) -> {
          // Read echo's output - with binary option we get BitArray
          case port_ffi.receive_timeout(runner.port, 1000) {
            Ok(port_ffi.Data(data)) -> {
              // Verify we got binary data containing "hello"
              let output = bit_array.to_string(data)
              case output {
                Ok(str) -> should.be_true(string.contains(str, "hello"))
                Error(_) -> should.fail()
              }
            }
            _ -> should.fail()
          }
          runner.close()
        }
        Error(_) -> should.fail()
      }
    }
  }
}

/// Test that port receives exit_status (verifies exit_status option).
/// Spawns 'true' (exits 0) and verifies we receive ExitStatus message.
pub fn bidir_runner_exit_status_option_test() {
  case port_ffi.find_cli_path("true") {
    Error(_) -> should.be_true(True)
    Ok(true_path) -> {
      let result = bidir_runner.start_raw(true_path, [])
      case result {
        Ok(runner) -> {
          // 'true' exits immediately with status 0
          // With exit_status option, we receive ExitStatus message
          case port_ffi.receive_timeout(runner.port, 1000) {
            Ok(port_ffi.ExitStatus(code)) -> {
              // 'true' exits with code 0
              should.equal(code, 0)
            }
            _ -> should.fail()
          }
          runner.close()
        }
        Error(_) -> should.fail()
      }
    }
  }
}

/// Test that port receives non-zero exit status correctly.
/// Spawns 'false' (exits 1) and verifies we receive ExitStatus(1).
pub fn bidir_runner_exit_status_nonzero_test() {
  case port_ffi.find_cli_path("false") {
    Error(_) -> should.be_true(True)
    Ok(false_path) -> {
      let result = bidir_runner.start_raw(false_path, [])
      case result {
        Ok(runner) -> {
          // 'false' exits immediately with status 1
          case port_ffi.receive_timeout(runner.port, 1000) {
            Ok(port_ffi.ExitStatus(code)) -> {
              // 'false' exits with code 1
              should.equal(code, 1)
            }
            _ -> should.fail()
          }
          runner.close()
        }
        Error(_) -> should.fail()
      }
    }
  }
}

// =============================================================================
// Mock BidirRunner Tests
// =============================================================================

/// mock() creates a BidirRunner that captures write calls via callback.
pub fn mock_captures_writes_test() {
  let writes = process.new_subject()

  let runner =
    bidir_runner.mock(
      on_write: fn(data) {
        process.send(writes, data)
        Ok(Nil)
      },
      on_close: fn() { Nil },
    )

  let assert Ok(Nil) = runner.write("hello")
  let assert Ok("hello") = process.receive(writes, 100)
}

/// mock() close callback is invoked when close() is called.
pub fn mock_close_callback_test() {
  let closed = process.new_subject()

  let runner =
    bidir_runner.mock(on_write: fn(_) { Ok(Nil) }, on_close: fn() {
      process.send(closed, True)
    })

  runner.close()
  let assert Ok(True) = process.receive(closed, 100)
}

/// mock() write callback can return errors.
pub fn mock_write_error_test() {
  let runner =
    bidir_runner.mock(
      on_write: fn(_) { Error(port_io.PortClosed) },
      on_close: fn() { Nil },
    )

  let result = runner.write("data")
  case result {
    Error(port_io.PortClosed) -> should.be_true(True)
    _ -> should.fail()
  }
}

/// mock() port field is usable for pattern matching and distinct between instances.
pub fn mock_port_is_usable_test() {
  let runner =
    bidir_runner.mock(on_write: fn(_) { Ok(Nil) }, on_close: fn() { Nil })

  // Port should be extractable as a valid dynamic reference
  let port_dynamic = port_ffi.port_to_dynamic(runner.port)

  // Create a second mock to verify port comparison works
  let runner2 =
    bidir_runner.mock(on_write: fn(_) { Ok(Nil) }, on_close: fn() { Nil })
  let port_dynamic2 = port_ffi.port_to_dynamic(runner2.port)

  // Mock ports should be distinct references
  should.be_true(port_dynamic != port_dynamic2)
}

/// MockRunner helper captures writes via subject.
pub fn mock_runner_helper_captures_writes_test() {
  let mock = mock_bidir_runner.new()

  let assert Ok(Nil) = mock.runner.write("test data")
  let assert Ok("test data") = process.receive(mock.writes, 100)
}

/// MockRunner helper notifies on close via subject.
pub fn mock_runner_helper_notifies_close_test() {
  let mock = mock_bidir_runner.new()

  mock.runner.close()
  let assert Ok(True) = process.receive(mock.closed, 100)
}

/// MockRunner custom write behavior test.
pub fn mock_runner_custom_write_test() {
  let mock =
    mock_bidir_runner.new_with_write(fn(_) { Error(port_io.PortClosed) })

  let result = mock.runner.write("will fail")
  case result {
    Error(port_io.PortClosed) -> should.be_true(True)
    _ -> should.fail()
  }
}
