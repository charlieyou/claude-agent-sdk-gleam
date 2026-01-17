/// Integration tests for environment-dependent error scenarios (SDK-61).
///
/// These tests use mocks/fakes and are intentionally NOT part of the real CLI E2E suite.
import claude_agent_sdk
import claude_agent_sdk/error
import claude_agent_sdk/runner
import e2e/helpers
import gleam/dynamic
import gleam/json
import gleam/string
import gleeunit/should

// NOTE: dynamic import is used for dynamic.nil() in test_runner on_spawn callback

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
pub fn sdk_61_auth_failure_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_61_auth_failure")

  let ctx = helpers.test_step(ctx, "create_mock_runner")
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

  let ctx = helpers.test_step(ctx, "execute_query")
  let result =
    claude_agent_sdk.query(
      "test",
      claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)
        |> claude_agent_sdk.with_test_mode(mock_runner)
        |> claude_agent_sdk.with_skip_version_check,
    )

  let ctx = helpers.test_step(ctx, "validate_error")
  // Assert: SDK handles auth failure gracefully
  case result {
    Error(query_error) -> {
      // Query failed at startup - acceptable
      helpers.log_info_with(ctx, "query_error", [
        #("error", json.string(error.error_to_string(query_error))),
      ])
      helpers.log_test_complete(
        ctx,
        True,
        "Auth failure surfaced as QueryError",
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

              helpers.log_info_with(ctx, "process_error", [
                #("exit_code", json.int(exit_code)),
                #("hint", json.string(diagnostic.exit_code_hint)),
              ])
              helpers.log_test_complete(
                ctx,
                True,
                "Auth failure surfaced as ProcessError with helpful diagnostics",
              )
            }
            _ -> {
              helpers.log_info_with(ctx, "stream_error", [
                #("error", json.string(error.stream_error_to_string(err))),
              ])
              helpers.log_test_complete(
                ctx,
                True,
                "Auth failure surfaced as StreamError",
              )
            }
          }
        }
        StreamCompleted -> {
          helpers.log_error(
            ctx,
            "unexpected_completion",
            "Expected error but stream completed normally",
          )
          helpers.log_test_complete(
            ctx,
            False,
            "Expected error but stream completed normally",
          )
          should.fail()
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
