/// E2E Serial Tests for Environment-Dependent Error Scenarios (SDK-60, SDK-61).
///
/// These tests MUST run serially because they modify global environment variables:
/// - PATH (for CLI not found)
///
/// SDK-61 uses a mock runner to simulate auth failure because the real CLI hangs
/// waiting for interactive authentication when run without valid credentials.
///
/// ## Why Serial?
/// Modifying environment variables affects all concurrent processes.
/// Running these tests in parallel would cause race conditions and flaky tests.
///
/// ## Running Tests
/// Run these tests in isolation to avoid race conditions with other E2E tests:
/// ```bash
/// gleam test -- --e2e
/// gleam test -- --only sdk_error_serial
/// ```
import claude_agent_sdk
import claude_agent_sdk/error
import claude_agent_sdk/runner
import e2e/helpers
import gleam/dynamic
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/string
import gleeunit/should

// NOTE: dynamic import is used for dynamic.nil() in test_runner on_spawn callback

// ============================================================================
// FFI Bindings for Environment Manipulation
// ============================================================================

/// Get environment variable value.
/// Returns Ok(value) if set, Error(Nil) if not set.
@external(erlang, "e2e_helpers_ffi", "get_env")
fn get_env(name: String) -> Result(String, Nil)

/// Set environment variable.
@external(erlang, "e2e_helpers_ffi", "set_env")
fn set_env(name: String, value: String) -> Nil

/// Unset environment variable.
@external(erlang, "e2e_helpers_ffi", "unset_env")
fn unset_env(name: String) -> Nil

// ============================================================================
// Test Helpers
// ============================================================================

/// Save the current value of an environment variable.
fn save_env(name: String) -> Option(String) {
  case get_env(name) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

/// Restore an environment variable to its previous state.
fn restore_env(name: String, saved: Option(String)) -> Nil {
  case saved {
    Some(value) -> set_env(name, value)
    None -> unset_env(name)
  }
}

// ============================================================================
// SDK-60: CLI Not Found
// ============================================================================

/// SDK-60: Verify CliNotFoundError when PATH excludes claude.
///
/// This test modifies PATH to point to a non-existent directory,
/// which should cause query() to return CliNotFoundError.
pub fn sdk_60_cli_not_found_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      // Save original PATH
      let original_path = save_env("PATH")

      // Set PATH to exclude claude CLI
      set_env("PATH", "/nonexistent/path/only")

      // Attempt query - should fail with CliNotFoundError
      let result =
        claude_agent_sdk.query("test", claude_agent_sdk.default_options())

      // Restore PATH immediately (before any assertions that might panic)
      restore_env("PATH", original_path)

      // Now assert on the result
      case result {
        Error(error.CliNotFoundError(msg)) -> {
          // Verify message contains helpful installation instructions
          msg
          |> string.contains("not found")
          |> should.be_true

          msg
          |> string.contains("npm install")
          |> should.be_true

          io.println("[PASS] SDK-60: CliNotFoundError with helpful message")
        }
        Error(other_error) -> {
          io.println(
            "[FAIL] SDK-60: Expected CliNotFoundError, got: "
            <> error.error_to_string(other_error),
          )
          should.fail()
        }
        Ok(_stream) -> {
          io.println(
            "[FAIL] SDK-60: Expected CliNotFoundError, but query succeeded",
          )
          should.fail()
        }
      }
    }
  }
}

// ============================================================================
// SDK-61: Authentication Failure (Mock-based)
// ============================================================================

/// SDK-61: Verify clean error handling when authentication fails.
///
/// This test uses a mock runner to simulate what the CLI does when auth fails:
/// - Exits with code 1
/// - Produces no stdout (common for auth failures)
///
/// We use a mock because the real CLI hangs waiting for interactive
/// authentication when run without valid credentials in a non-TTY environment.
///
/// The SDK should surface the error cleanly with helpful diagnostics.
pub fn sdk_61_auth_failure_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      // Create a mock runner that simulates CLI auth failure:
      // - Spawns successfully
      // - Returns EOF immediately (empty stdout)
      // - Exits with code 1 (auth failure)
      let mock_runner =
        runner.test_runner(
          on_spawn: fn(_cmd, _args, _cwd) { Ok(dynamic.nil()) },
          on_read: fn(_handle) {
            // Return exit code 1 immediately (simulates auth failure with no output)
            runner.ExitStatus(1)
          },
          on_close: fn(_handle) { Nil },
        )

      let result =
        claude_agent_sdk.query(
          "test",
          claude_agent_sdk.default_options()
            |> claude_agent_sdk.with_max_turns(1)
            |> claude_agent_sdk.with_test_mode(mock_runner)
            |> claude_agent_sdk.with_skip_version_check,
        )

      // Assert: SDK handles auth failure gracefully
      case result {
        Error(query_error) -> {
          // Query failed at startup - acceptable
          io.println(
            "[PASS] SDK-61: Auth failure surfaced as QueryError: "
            <> error.error_to_string(query_error),
          )
        }
        Ok(stream) -> {
          // Query started - error should come through stream as ProcessError
          let stream_result = consume_until_error_or_end(stream)
          case stream_result {
            StreamErrored(err) -> {
              // Verify it's a ProcessError with exit code 1
              case err {
                error.ProcessError(exit_code, diagnostic) -> {
                  // Exit code should be 1 (auth failure)
                  exit_code
                  |> should.equal(1)

                  // Diagnostic should have auth-related hint
                  // Assert on exact hint text for stability
                  diagnostic.exit_code_hint
                  |> should.equal("Authentication required")

                  // Troubleshooting should mention authentication
                  diagnostic.troubleshooting
                  |> string.contains("Authenticate")
                  |> should.be_true

                  io.println(
                    "[PASS] SDK-61: Auth failure surfaced as ProcessError with helpful diagnostics",
                  )
                }
                _ -> {
                  io.println(
                    "[PASS] SDK-61: Auth failure surfaced as StreamError: "
                    <> error.stream_error_to_string(err),
                  )
                }
              }
            }
            StreamCompleted -> {
              io.println(
                "[FAIL] SDK-61: Expected error but stream completed normally",
              )
              should.fail()
            }
          }
        }
      }
    }
  }
}

/// Result of consuming a stream until error or completion.
type StreamConsumeResult {
  StreamErrored(error.StreamError)
  StreamCompleted
}

/// Consume stream items until a terminal error or EndOfStream.
/// Uses a max iteration counter to prevent infinite loops on repeated non-terminal errors.
fn consume_until_error_or_end(
  stream: claude_agent_sdk.QueryStream,
) -> StreamConsumeResult {
  // Max 100 iterations to prevent infinite loop on pathological non-terminal errors
  consume_loop(stream, 100)
}

fn consume_loop(
  stream: claude_agent_sdk.QueryStream,
  remaining: Int,
) -> StreamConsumeResult {
  case remaining <= 0 {
    True -> {
      // Safety limit reached - close and return as completed
      let _ = claude_agent_sdk.close(stream)
      StreamCompleted
    }
    False -> {
      let #(result, updated_stream) = claude_agent_sdk.next(stream)
      case result {
        Ok(error.EndOfStream) -> {
          let _ = claude_agent_sdk.close(updated_stream)
          StreamCompleted
        }
        Ok(error.Message(_)) | Ok(error.WarningEvent(_)) -> {
          consume_loop(updated_stream, remaining - 1)
        }
        Error(err) -> {
          case claude_agent_sdk.is_terminal(err) {
            True -> {
              let _ = claude_agent_sdk.close(updated_stream)
              StreamErrored(err)
            }
            False -> {
              // Non-terminal error, continue (decrement counter)
              consume_loop(updated_stream, remaining - 1)
            }
          }
        }
      }
    }
  }
}
