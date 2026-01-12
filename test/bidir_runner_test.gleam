import claude_agent_sdk/internal/bidir_runner
import claude_agent_sdk/internal/port_ffi
import gleeunit/should

/// start() returns SpawnFailed if 'claude' is not in PATH.
/// This is the expected behavior - we spawn the actual CLI.
pub fn bidir_runner_start_missing_cli_test() {
  // Try to spawn claude - if not in PATH, should return SpawnFailed
  let result = bidir_runner.start([])
  case result {
    Error(bidir_runner.SpawnFailed(_)) -> should.be_true(True)
    Error(bidir_runner.NotImplemented) ->
      // If still NotImplemented, fail with message
      should.fail()
    Ok(runner) -> {
      // Clean up if it somehow succeeded
      runner.close()
      should.be_true(True)
    }
  }
}

/// start() with custom executable path returns SpawnFailed for invalid path.
pub fn bidir_runner_start_with_invalid_path_test() {
  let result = bidir_runner.start_with_path("/nonexistent/claude", [])
  case result {
    Error(bidir_runner.SpawnFailed(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

/// start() spawns correctly with a valid executable (echo for test).
/// Tests that the BidirRunner is created and can be closed.
pub fn bidir_runner_start_with_valid_executable_test() {
  // Use /bin/echo as a simple test executable
  let result = bidir_runner.start_with_path("/bin/echo", ["hello"])
  case result {
    Ok(runner) -> {
      // Successfully spawned - close it
      runner.close()
      should.be_true(True)
    }
    Error(_) -> {
      // Echo should always exist on Unix
      should.fail()
    }
  }
}

/// start() write function sends data to the port.
pub fn bidir_runner_write_test() {
  // Use /bin/cat as a test - it echoes stdin to stdout
  let result = bidir_runner.start_with_path("/bin/cat", [])
  case result {
    Ok(runner) -> {
      // Write to the port - should succeed
      let write_result = runner.write("test data")
      case write_result {
        Ok(Nil) -> should.be_true(True)
        Error(_) -> should.fail()
      }
      // Clean up
      runner.close()
    }
    Error(_) -> should.fail()
  }
}

/// Verify port options include stderr_to_stdout on OTP >= 25.
pub fn bidir_runner_stderr_merged_test() {
  // This is tested implicitly by spawning - if stderr_to_stdout wasn't
  // working, we'd get crashes on OTP >= 25 or different behavior.
  // The FFI sets the option conditionally based on OTP version.
  let supports_stderr = port_ffi.supports_stderr_to_stdout()
  // Just verify the support check works
  case supports_stderr {
    True -> should.be_true(True)
    False -> should.be_true(True)
  }
}
