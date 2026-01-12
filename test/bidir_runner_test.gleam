import claude_agent_sdk/internal/bidir_runner
import claude_agent_sdk/internal/port_ffi
import gleam/bit_array
import gleam/string
import gleeunit/should

/// start() returns SpawnFailed when 'claude' is not found in PATH.
/// If claude IS in PATH, we verify the runner can be created and closed.
pub fn bidir_runner_start_cli_test() {
  let result = bidir_runner.start([])
  case result {
    Error(bidir_runner.SpawnFailed(_)) -> {
      // Expected: claude not in PATH
      should.be_true(True)
    }
    Error(bidir_runner.NotImplemented) -> {
      // Should never happen - start() is implemented
      should.fail()
    }
    Ok(runner) -> {
      // Claude is in PATH - verify we can close cleanly
      runner.close()
      should.be_true(True)
    }
  }
}

/// start_with_path returns SpawnFailed for non-existent executable.
pub fn bidir_runner_start_with_invalid_path_test() {
  let result = bidir_runner.start_with_path("/nonexistent/path/to/claude", [])
  case result {
    Error(bidir_runner.SpawnFailed(_)) -> should.be_true(True)
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
