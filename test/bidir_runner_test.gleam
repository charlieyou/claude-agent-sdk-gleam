import claude_agent_sdk/internal/bidir_runner
import claude_agent_sdk/internal/port_ffi
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

/// start_raw spawns correctly with a valid executable.
/// Uses 'true' which exists on all Unix systems and exits immediately with 0.
pub fn bidir_runner_start_raw_valid_executable_test() {
  // 'true' is a POSIX standard utility that does nothing and exits 0
  let result = bidir_runner.start_raw("/usr/bin/true", [])
  case result {
    Ok(runner) -> {
      runner.close()
      should.be_true(True)
    }
    Error(_) -> {
      // /usr/bin/true should exist on all Unix
      should.fail()
    }
  }
}

/// start_raw write function can send data to a process stdin.
/// Uses 'cat' which reads stdin and echoes to stdout.
pub fn bidir_runner_write_test() {
  let result = bidir_runner.start_raw("/bin/cat", [])
  case result {
    Ok(runner) -> {
      // Write to the port - should succeed while cat is running
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

/// Verify OTP version check for stderr_to_stdout support works.
pub fn bidir_runner_stderr_support_check_test() {
  // Just verify the support check function works without error
  let supports_stderr = port_ffi.supports_stderr_to_stdout()
  // Result should be a valid boolean (True or False)
  case supports_stderr {
    True -> should.be_true(True)
    False -> should.be_true(True)
  }
}
