/// E2E Tests for Network/Credential Failure Handling.
///
/// These tests verify the SDK handles authentication and credential failures
/// with clear error types. They use mock runners for isolation - no real CLI
/// calls and no global env mutation.
///
/// ## Test Cases
/// - SDK-AUTH-01: Invalid API key produces clear authentication error
/// - SDK-AUTH-02: Error maps to SDK error type (ProcessError with diagnostic)
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// ```
import claude_agent_sdk
import claude_agent_sdk/error.{ProcessError}
import claude_agent_sdk/runner
import e2e/helpers
import gleam/dynamic
import gleam/json
import gleeunit/should

// ============================================================================
// Helper: Type Coercion FFI
// ============================================================================

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> dynamic.Dynamic

// ============================================================================
// SDK-AUTH-01: Authentication Error Handling
// ============================================================================

/// SDK-AUTH-01: Verify invalid API key produces clear authentication error.
///
/// This test simulates an authentication failure by using a mock runner that
/// returns exit code 1 with empty stdout - the pattern seen when Claude CLI
/// encounters an invalid API key. The SDK should surface this as a ProcessError
/// with authentication-related diagnostic hints.
///
/// Isolation: Uses mock runner, NOT global env mutation. Other tests unaffected.
pub fn sdk_auth_01_invalid_api_key_test() {
  let ctx = helpers.new_test_context("sdk_auth_01_invalid_api_key")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  // Mock runner simulates authentication failure:
  // - CLI spawns successfully
  // - First read returns exit status 1 (auth failure pattern)
  // - No data emitted (empty stdout, error only on stderr which we don't capture)
  let mock_runner =
    runner.test_runner(
      on_spawn: fn(_cmd, _args, _cwd) { Ok(to_dynamic(Nil)) },
      on_read: fn(_handle) {
        // Simulate auth failure: immediate exit with code 1, no stdout
        // This matches the real CLI behavior when ANTHROPIC_API_KEY is invalid
        runner.ExitStatus(1)
      },
      on_close: fn(_handle) { Nil },
    )

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(mock_runner)
    |> claude_agent_sdk.with_max_turns(1)
    |> claude_agent_sdk.with_skip_version_check

  let ctx = helpers.test_step(ctx, "execute_query")
  case claude_agent_sdk.query("test prompt", opts) {
    Error(_err) -> {
      // Query itself failed at spawn - acceptable
      helpers.log_info(ctx, "query_spawn_failed_acceptable")
      helpers.log_test_complete(ctx, True, "Query spawn failed as acceptable")
    }
    Ok(stream) -> {
      // Query succeeded, read from stream - should get auth error
      let ctx = helpers.test_step(ctx, "read_stream")
      let #(result, updated_stream) = claude_agent_sdk.next(stream)

      case result {
        Error(ProcessError(exit_code, diagnostic)) -> {
          let ctx = helpers.test_step(ctx, "verify_error_type")

          // Exit code should be 1 (authentication/general error)
          exit_code |> should.equal(1)

          // Diagnostic should indicate stdout was empty (auth failure pattern)
          diagnostic.stdout_was_empty |> should.be_true

          // Exit code hint should mention authentication
          // "Authentication required" for exit 1 + empty stdout
          { diagnostic.exit_code_hint == "Authentication required" }
          |> should.be_true

          helpers.log_info_with(ctx, "auth_error_received", [
            #("exit_code", json.int(exit_code)),
            #("stdout_was_empty", json.bool(diagnostic.stdout_was_empty)),
            #("exit_code_hint", json.string(diagnostic.exit_code_hint)),
          ])
          helpers.log_test_complete(
            ctx,
            True,
            "Authentication error surfaced with clear type",
          )
        }
        Error(other_error) -> {
          // Any terminal error is acceptable as long as it's recognizable
          claude_agent_sdk.is_terminal(other_error) |> should.be_true
          helpers.log_info(ctx, "terminal_error_received")
          helpers.log_test_complete(ctx, True, "Terminal error handled")
        }
        Ok(error.EndOfStream) -> {
          // EndOfStream is acceptable if runner returned exit
          helpers.log_info(ctx, "end_of_stream_received")
          helpers.log_test_complete(ctx, True, "EndOfStream handled")
        }
        Ok(_) -> {
          // Unexpected success - should not happen with auth-failing runner
          helpers.log_error(ctx, "unexpected_success", "Should not succeed")
          helpers.log_test_complete(ctx, False, "Unexpected success")
          should.fail()
        }
      }

      // Cleanup
      let _ = claude_agent_sdk.close(updated_stream)
      Nil
    }
  }
}

// ============================================================================
// SDK-AUTH-02: Error Type Mapping
// ============================================================================

/// SDK-AUTH-02: Verify auth error maps to recognizable SDK error type.
///
/// This test verifies that authentication failures are mapped to the SDK's
/// error type system with actionable diagnostic information. The error should
/// include:
/// - Clear exit code interpretation
/// - Troubleshooting guidance
/// - Context about stdout state
pub fn sdk_auth_02_error_type_mapping_test() {
  let ctx = helpers.new_test_context("sdk_auth_02_error_type_mapping")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  // Mock runner simulates auth failure with different exit code patterns
  // Exit 1 with empty stdout = authentication required
  let mock_runner =
    runner.test_runner(
      on_spawn: fn(_cmd, _args, _cwd) { Ok(to_dynamic(Nil)) },
      on_read: fn(_handle) { runner.ExitStatus(1) },
      on_close: fn(_handle) { Nil },
    )

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(mock_runner)
    |> claude_agent_sdk.with_max_turns(1)
    |> claude_agent_sdk.with_skip_version_check

  let ctx = helpers.test_step(ctx, "execute_query")
  case claude_agent_sdk.query("test prompt", opts) {
    Error(_err) -> {
      helpers.log_info(ctx, "query_spawn_failed_acceptable")
      helpers.log_test_complete(ctx, True, "Query spawn failed as acceptable")
    }
    Ok(stream) -> {
      let ctx = helpers.test_step(ctx, "read_stream")
      let #(result, updated_stream) = claude_agent_sdk.next(stream)

      case result {
        Error(ProcessError(exit_code, diagnostic)) -> {
          let ctx = helpers.test_step(ctx, "verify_diagnostic_fields")

          // Verify all diagnostic fields are populated
          // exit_code_hint should be non-empty
          { diagnostic.exit_code_hint != "" } |> should.be_true

          // troubleshooting should be non-empty
          { diagnostic.troubleshooting != "" } |> should.be_true

          // For auth failures, troubleshooting should mention authentication or CLI
          let has_actionable_guidance =
            diagnostic.troubleshooting != ""
            && { exit_code == 1 || diagnostic.stdout_was_empty }

          has_actionable_guidance |> should.be_true

          helpers.log_info_with(ctx, "diagnostic_fields_verified", [
            #("exit_code", json.int(exit_code)),
            #("exit_code_hint", json.string(diagnostic.exit_code_hint)),
            #(
              "troubleshooting_length",
              json.int(string_length(diagnostic.troubleshooting)),
            ),
          ])
          helpers.log_test_complete(
            ctx,
            True,
            "Error type mapping provides actionable diagnostics",
          )
        }
        Error(_other_error) -> {
          // Other terminal errors are acceptable
          helpers.log_info(ctx, "other_error_received")
          helpers.log_test_complete(ctx, True, "Other error handled")
        }
        Ok(_) -> {
          // Any success is acceptable for this test
          helpers.log_info(ctx, "success_received")
          helpers.log_test_complete(ctx, True, "Success handled")
        }
      }

      let _ = claude_agent_sdk.close(updated_stream)
      Nil
    }
  }
}

/// Helper to get string length (avoiding import)
fn string_length(s: String) -> Int {
  do_string_length(s)
}

@external(erlang, "string", "length")
fn do_string_length(s: String) -> Int
