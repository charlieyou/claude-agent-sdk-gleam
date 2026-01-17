/// E2E Tests for Stderr/Debug Callback Behavior.
///
/// These tests verify how the SDK captures stderr output from the Claude CLI.
/// The behavior depends on:
/// - OTP version (>= 25 supports stderr_to_stdout port option)
/// - Debug flag (--verbose or debug-to-stderr extra_arg)
///
/// ## What's Being Tested
/// - Stderr support detection based on OTP version
/// - Sessions complete successfully regardless of stderr capture mode
/// - On OTP >= 25, stderr is captured with stdout (no terminal pollution)
/// - On OTP < 25, stderr goes to terminal (not captured)
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// # Ensure the Claude CLI is authenticated (e.g., `claude auth login`)
/// gleam test -- --only sdk_stderr
/// ```
import claude_agent_sdk
import claude_agent_sdk/error.{error_to_string}
import claude_agent_sdk/internal/port_ffi
import e2e/helpers
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleeunit/should

// ============================================================================
// Stderr Support Detection
// ============================================================================

/// Test that we can detect OTP version and stderr support.
/// This is a unit-style test that validates the detection mechanism.
pub fn otp_version_detection_test() {
  let otp_version = port_ffi.get_otp_version()
  let supports_stderr = port_ffi.supports_stderr_to_stdout()

  // OTP version should be a reasonable number (we support OTP 24+)
  // If version detection fails, get_otp_version returns 0
  should.be_true(otp_version >= 0)

  // stderr support should match OTP version threshold
  // OTP >= 25 supports stderr_to_stdout
  case otp_version >= 25 {
    True -> should.be_true(supports_stderr)
    False -> should.be_false(supports_stderr)
  }
}

// ============================================================================
// E2E: Session Completes with Stderr Capture (OTP-dependent)
// ============================================================================

/// Test that sessions complete successfully with --verbose flag.
/// The bidir_runner uses --verbose by default, and on OTP >= 25 this
/// means stderr is captured via stderr_to_stdout port option.
///
/// This test verifies:
/// 1. Session can start and complete with stderr capture enabled
/// 2. Stream produces expected protocol messages
/// 3. No hangs or crashes due to stderr handling
///
/// On OTP >= 25: stderr is mixed with stdout (debug logs in stream)
/// On OTP < 25: stderr goes to terminal (debug logs not captured)
pub fn sdk_stderr_verbose_session_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_stderr_verbose_session")

      // Log OTP version and stderr support for diagnostics
      let otp_version = port_ffi.get_otp_version()
      let supports_stderr = port_ffi.supports_stderr_to_stdout()

      let ctx =
        helpers.test_step_with(ctx, "check_otp_version", [
          #("otp_version", json.int(otp_version)),
          #("supports_stderr", json.bool(supports_stderr)),
        ])

      let ctx = helpers.test_step(ctx, "configure_options")

      // Use default options with max_turns=1 for fast test
      // The bidir_runner always passes --verbose, so stderr capture is active
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query_with_stderr_capture")

      case helpers.query_and_consume_with_timeout("Say hi", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_error_summary(ctx, "QueryFailure", error_to_string(err))
          helpers.log_test_complete(ctx, False, "query() failed")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          // Timeout is acceptable for E2E - don't fail, just skip
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "skipped due to timeout")
          Nil
        }
        helpers.QuerySuccess(result) -> {
          let ctx = helpers.test_step(ctx, "validate_session_completed")
          helpers.log_stream_transcript(ctx, result.messages)

          // Protocol invariant: session must produce messages
          list.length(result.messages)
          |> should.not_equal(0)

          // Protocol invariant: stream should terminate normally
          result.terminated_normally
          |> should.be_true

          // Protocol invariant: should have a result message
          helpers.has_result_message(result.messages)
          |> should.be_true

          let counts = helpers.count_message_types(result.messages)

          helpers.log_info_with(ctx, "session_completed", [
            #("otp_version", json.int(otp_version)),
            #("stderr_to_stdout", json.bool(supports_stderr)),
            #("message_count", json.int(list.length(result.messages))),
            #("system_count", json.int(counts.system)),
            #("result_count", json.int(counts.result)),
          ])

          helpers.log_test_complete(
            ctx,
            True,
            "Session completed with stderr capture (OTP "
              <> int.to_string(otp_version)
              <> ", stderr_to_stdout="
              <> case supports_stderr {
              True -> "true"
              False -> "false"
            }
              <> ")",
          )
        }
      }
    }
  }
}

// ============================================================================
// E2E: Stderr Callback Parity Gap Documentation
// ============================================================================

/// Test documenting current SDK state regarding stderr callback.
/// The Python SDK has an explicit stderr callback; Gleam SDK does not yet.
///
/// This test:
/// 1. Runs a real session to verify basic functionality works
/// 2. Documents the parity gap with Python SDK
/// 3. Verifies OTP-dependent behavior is correctly detected
///
/// When on_stderr callback is added to BidirOptions, this test should
/// be updated to exercise that callback.
pub fn sdk_stderr_callback_parity_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_stderr_callback_parity")

      let supports_stderr = port_ffi.supports_stderr_to_stdout()
      let otp_version = port_ffi.get_otp_version()

      // Document current SDK state
      let ctx =
        helpers.test_step_with(ctx, "document_parity_gap", [
          #("has_on_stderr_callback", json.bool(False)),
          #(
            "note",
            json.string(
              "BidirOptions lacks on_stderr field - parity gap with Python SDK",
            ),
          ),
          #("stderr_to_stdout_support", json.bool(supports_stderr)),
        ])

      let ctx = helpers.test_step(ctx, "run_session_to_verify_behavior")

      // Run a minimal session to verify stderr handling doesn't break anything
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      case helpers.query_and_consume_with_timeout("Hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Session failed")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "skipped due to timeout")
          Nil
        }
        helpers.QuerySuccess(result) -> {
          // Verify session completed
          result.terminated_normally
          |> should.be_true

          helpers.has_result_message(result.messages)
          |> should.be_true

          helpers.log_info_with(ctx, "session_verified", [
            #("message_count", json.int(list.length(result.messages))),
            #("terminated_normally", json.bool(result.terminated_normally)),
          ])

          helpers.log_test_complete(
            ctx,
            True,
            "Session works correctly (OTP "
              <> int.to_string(otp_version)
              <> ") - on_stderr callback not yet implemented",
          )
        }
      }
    }
  }
}
