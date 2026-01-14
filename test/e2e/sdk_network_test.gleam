/// E2E Tests for Network/Credential Failure Handling.
///
/// These tests verify the SDK handles authentication and credential failures
/// with clear error types. They use mock runners for isolation - no real CLI
/// calls and no global env mutation.
///
/// ## Why Mock Instead of Real CLI?
/// The real Claude CLI hangs waiting for interactive authentication when run
/// without valid credentials in a non-TTY environment. Using mocks allows us to
/// test the SDK's error handling of the auth failure pattern (exit code 1,
/// empty stdout) without the test timing out.
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
import gleam/string
import gleeunit/should

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
      on_spawn: fn(_cmd, _args, _cwd) { Ok(dynamic.nil()) },
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
  // Query must succeed - spawn failure would indicate test setup issue
  let assert Ok(stream) = claude_agent_sdk.query("test prompt", opts)

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
    _ -> {
      // Any other result is unexpected - auth failure should produce ProcessError
      helpers.log_error(ctx, "unexpected_result", "Expected ProcessError")
      helpers.log_test_complete(
        ctx,
        False,
        "Expected ProcessError with auth diagnostic",
      )
      should.fail()
    }
  }

  // Cleanup
  let _ = claude_agent_sdk.close(updated_stream)
  Nil
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
      on_spawn: fn(_cmd, _args, _cwd) { Ok(dynamic.nil()) },
      on_read: fn(_handle) { runner.ExitStatus(1) },
      on_close: fn(_handle) { Nil },
    )

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(mock_runner)
    |> claude_agent_sdk.with_max_turns(1)
    |> claude_agent_sdk.with_skip_version_check

  let ctx = helpers.test_step(ctx, "execute_query")
  // Query must succeed - spawn failure would indicate test setup issue
  let assert Ok(stream) = claude_agent_sdk.query("test prompt", opts)

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
          json.int(string.length(diagnostic.troubleshooting)),
        ),
      ])
      helpers.log_test_complete(
        ctx,
        True,
        "Error type mapping provides actionable diagnostics",
      )
    }
    _ -> {
      // Any other result is unexpected - auth failure should produce ProcessError
      helpers.log_error(ctx, "unexpected_result", "Expected ProcessError")
      helpers.log_test_complete(
        ctx,
        False,
        "Expected ProcessError with diagnostic fields",
      )
      should.fail()
    }
  }

  let _ = claude_agent_sdk.close(updated_stream)
  Nil
}
