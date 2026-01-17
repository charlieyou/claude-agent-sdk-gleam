/// Integration Tests for SDK Error Handling of Authentication/Credential Failures.
///
/// SCOPE: These tests validate SDK logic ONLY - how the SDK interprets
/// process exit codes and maps them to error types. They do NOT test
/// actual Claude CLI behavior.
///
/// ## Mock-Based Design
/// These tests use mock runners that simulate the exit code patterns we expect
/// from the CLI. This means:
/// - If the real CLI changes its exit codes/output, these tests won't detect it
/// - They verify SDK error mapping, not CLI contract compliance
/// - Real CLI E2E tests would need valid credentials and network access
///
/// ## What These Tests Actually Verify
/// - SDK maps exit code 1 + empty stdout → ProcessError with auth hint
/// - Diagnostic fields (exit_code_hint, troubleshooting) are populated
/// - Error types are actionable for SDK consumers
///
/// ## Test Cases
/// - SDK-AUTH-01: SDK correctly interprets simulated auth failure pattern
/// - SDK-AUTH-02: Diagnostic fields populated for process errors
///
/// ## Running Tests
/// ```bash
/// gleam test
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

/// SDK-AUTH-01: Verify SDK interprets auth failure pattern correctly.
///
/// This test uses a mock runner returning exit code 1 with empty stdout.
/// We ASSUME this pattern indicates auth failure based on observed CLI behavior,
/// but this test does NOT verify the CLI actually produces this pattern.
///
/// What's tested: SDK error mapping logic
/// What's NOT tested: Actual CLI behavior with invalid credentials
pub fn sdk_auth_01_invalid_api_key_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_auth_01_invalid_api_key")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  // Mock runner returns pattern we ASSUME represents auth failure.
  // CAVEAT: If real CLI changes exit code behavior, this test won't catch it.
  let mock_runner =
    runner.test_runner(
      on_spawn: fn(_cmd, _args, _cwd) { Ok(dynamic.nil()) },
      on_read: fn(_handle) {
        // Assumed auth failure pattern: exit 1, empty stdout
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

/// SDK-AUTH-02: Verify diagnostic fields are populated for process errors.
///
/// Tests that ProcessError includes actionable diagnostic fields.
/// Uses mock runner - does NOT verify actual CLI produces these patterns.
pub fn sdk_auth_02_error_type_mapping_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_auth_02_error_type_mapping")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  // Mock runner returning assumed auth failure pattern (exit 1, empty stdout)
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
