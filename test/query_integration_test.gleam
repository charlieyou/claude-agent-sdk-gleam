import claude_agent_sdk/internal/cli.{CliVersion, UnknownVersion}
import gleam/io
import gleeunit/should
import support/integration_helpers.{
  check_cli_help_flags, detect_cli_version_with_timeout, find_executable,
  integration_enabled,
}

/// Timeout for preflight checks (5 seconds)
const preflight_timeout_ms = 5000

/// Preflight test that validates CLI is available and properly configured.
/// This test runs before other integration tests to provide actionable error messages.
pub fn integration__preflight_check_test() {
  case integration_enabled("integration__preflight_check_test") {
    True -> {
      // 1. CLI exists and is in PATH
      case find_executable("claude") {
        Error(_) -> {
          io.println("[SKIP:ENV] claude not in PATH - install CLI first")
          should.be_true(True)
        }
        Ok(cli_path) -> {
          // 2. Version check (with 5s timeout)
          case detect_cli_version_with_timeout(cli_path, preflight_timeout_ms) {
            Error(_) -> {
              io.println("[SKIP:ENV] claude --version timed out or failed")
              should.be_true(True)
            }
            Ok(UnknownVersion(raw)) -> {
              io.println(
                "[WARN] Unparseable version: " <> raw <> " - tests may fail",
              )
              // Continue to help check since we have a CLI
              check_help_and_finish(cli_path)
            }
            Ok(CliVersion(maj, _, _, raw)) -> {
              case maj < 1 {
                True -> {
                  io.println(
                    "[SKIP:ENV] CLI version "
                    <> raw
                    <> " < 1.0.0 - upgrade required",
                  )
                  should.be_true(True)
                }
                False -> {
                  // 3. Required flags exist (non-network check)
                  check_help_and_finish(cli_path)
                }
              }
            }
          }
        }
      }
    }
    False -> should.be_true(True)
  }
}

/// Helper to check --help output for required flags and finish the test.
fn check_help_and_finish(cli_path: String) {
  case check_cli_help_flags(cli_path) {
    Error(reason) -> {
      io.println("[SKIP:ENV] " <> reason)
      should.be_true(True)
    }
    Ok(_) -> {
      io.println("[PREFLIGHT] All checks passed - integration tests will run")
      should.be_true(True)
    }
  }
}
