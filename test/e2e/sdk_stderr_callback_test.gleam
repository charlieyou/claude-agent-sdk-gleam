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
import claude_agent_sdk/error.{
  EndOfStream, JsonDecodeError, Message, WarningEvent, error_to_string,
}
import claude_agent_sdk/internal/port_ffi
import claude_agent_sdk/message.{type MessageEnvelope, Result as ResultMsg}
import e2e/helpers
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleeunit/should

// ============================================================================
// Stderr Capture Tracking
// ============================================================================

/// Result type that tracks both messages and non-JSON lines (stderr on OTP >= 25).
/// Non-JSON lines from the stream appear as JsonDecodeError events.
pub type StderrAwareResult {
  StderrAwareResult(
    messages: List(MessageEnvelope),
    /// Non-JSON lines captured from stream (stderr on OTP >= 25)
    non_json_lines: List(String),
    terminated_normally: Bool,
  )
}

/// Consume stream while tracking non-JSON lines (which indicate stderr on OTP >= 25).
/// Unlike helpers.consume_stream, this tracks JsonDecodeError events.
fn consume_stream_with_stderr_tracking(
  stream: claude_agent_sdk.QueryStream,
) -> StderrAwareResult {
  consume_loop(stream, [], [], False)
}

fn consume_loop(
  stream: claude_agent_sdk.QueryStream,
  messages: List(MessageEnvelope),
  non_json_lines: List(String),
  saw_end: Bool,
) -> StderrAwareResult {
  case saw_end {
    True -> {
      let _ = claude_agent_sdk.close(stream)
      StderrAwareResult(
        messages: list.reverse(messages),
        non_json_lines: list.reverse(non_json_lines),
        terminated_normally: True,
      )
    }
    False -> {
      let #(result, updated_stream) = claude_agent_sdk.next(stream)
      case result {
        Ok(Message(envelope)) -> {
          case envelope.message {
            ResultMsg(_) -> {
              let _ = claude_agent_sdk.close(updated_stream)
              StderrAwareResult(
                messages: list.reverse([envelope, ..messages]),
                non_json_lines: list.reverse(non_json_lines),
                terminated_normally: True,
              )
            }
            _ ->
              consume_loop(
                updated_stream,
                [envelope, ..messages],
                non_json_lines,
                False,
              )
          }
        }
        Ok(EndOfStream) ->
          consume_loop(updated_stream, messages, non_json_lines, True)
        Ok(WarningEvent(_)) ->
          consume_loop(updated_stream, messages, non_json_lines, False)
        Error(JsonDecodeError(line, _)) -> {
          // Non-JSON line - this is stderr output on OTP >= 25
          consume_loop(
            updated_stream,
            messages,
            [line, ..non_json_lines],
            False,
          )
        }
        Error(err) -> {
          case claude_agent_sdk.is_terminal(err) {
            True -> {
              let _ = claude_agent_sdk.close(updated_stream)
              StderrAwareResult(
                messages: list.reverse(messages),
                non_json_lines: list.reverse(non_json_lines),
                terminated_normally: False,
              )
            }
            False ->
              consume_loop(updated_stream, messages, non_json_lines, False)
          }
        }
      }
    }
  }
}

/// Result of query with stderr tracking and timeout.
pub type StderrQueryOutcome {
  StderrQuerySuccess(StderrAwareResult)
  StderrQueryFailure(claude_agent_sdk.QueryError)
  StderrQueryTimedOut
}

/// Run query with stderr tracking and timeout.
/// Acquires global query lock to prevent concurrent E2E test contention.
fn query_with_stderr_tracking(
  prompt: String,
  options: claude_agent_sdk.QueryOptions,
  timeout_ms: Int,
) -> StderrQueryOutcome {
  let _ = helpers.acquire_query_lock()
  let subject: process.Subject(StderrQueryOutcome) = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let outcome = case claude_agent_sdk.query(prompt, options) {
        Ok(stream) ->
          StderrQuerySuccess(consume_stream_with_stderr_tracking(stream))
        Error(err) -> StderrQueryFailure(err)
      }
      process.send(subject, outcome)
    })

  let outcome = case process.receive(subject, timeout_ms) {
    Ok(outcome) -> outcome
    Error(Nil) -> {
      helpers.kill_pid(pid)
      StderrQueryTimedOut
    }
  }
  let _ = helpers.release_query_lock()
  outcome
}

// ============================================================================
// Stderr Support Detection
// ============================================================================

/// Test that we can detect OTP version and stderr support.
/// This is a unit-style test that validates the detection mechanism.
pub fn otp_version_detection_test() {
  let otp_version = port_ffi.get_otp_version()
  let supports_stderr = port_ffi.supports_stderr_to_stdout()

  // OTP version should be a positive number (we support OTP 24+)
  // If version detection fails, get_otp_version returns 0, which should fail this test
  should.be_true(otp_version > 0)

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
/// 4. OTP-dependent stderr capture behavior (non-JSON lines captured on OTP >= 25)
///
/// On OTP >= 25: stderr is mixed with stdout (debug logs appear as non-JSON lines)
/// On OTP < 25: stderr goes to terminal (no non-JSON lines captured)
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

      let ctx = helpers.test_step(ctx, "execute_query_with_stderr_tracking")

      // Use stderr-aware query that tracks non-JSON lines (stderr output on OTP >= 25)
      case query_with_stderr_tracking("Say hi", opts, 30_000) {
        StderrQueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_error_summary(ctx, "QueryFailure", error_to_string(err))
          helpers.log_test_complete(ctx, False, "query() failed")
          should.fail()
        }
        StderrQueryTimedOut -> {
          // For stderr tests, timeout indicates a potential regression in stderr handling
          // (e.g., if stderr handling causes hangs). Fail rather than skip.
          helpers.log_error(
            ctx,
            "query_timeout_failure",
            "Session timed out - potential stderr handling regression",
          )
          helpers.log_test_complete(
            ctx,
            False,
            "timed out - stderr handling may be causing hang",
          )
          should.fail()
        }
        StderrQuerySuccess(result) -> {
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
          let non_json_count = list.length(result.non_json_lines)

          // OTP-dependent stderr capture assertion:
          // On OTP >= 25 with --verbose, the CLI emits debug output to stderr which is
          // merged into stdout. This appears as non-JSON lines (JsonDecodeError events).
          // We track these to verify stderr capture is working correctly.
          //
          // Note: The Claude CLI may not emit stderr on every run, so we can't assert
          // non_json_count > 0 on OTP >= 25. However, we CAN assert that if we're on
          // OTP < 25 (no stderr capture), we should NOT see non-JSON lines from stderr.
          // Any non-JSON lines on OTP < 25 would indicate a protocol violation.
          let ctx = helpers.test_step(ctx, "validate_stderr_capture_behavior")
          case supports_stderr {
            True -> {
              // OTP >= 25: stderr is captured. Log what we got for diagnostics.
              // Non-JSON lines may or may not be present depending on CLI behavior.
              helpers.log_info_with(ctx, "stderr_capture_active", [
                #("non_json_lines_captured", json.int(non_json_count)),
              ])
            }
            False -> {
              // OTP < 25: stderr goes to terminal, not captured.
              // Assert zero non-JSON lines - any would indicate protocol violation
              // or unexpected stdout pollution from the CLI.
              non_json_count
              |> should.equal(0)
              helpers.log_info_with(ctx, "stderr_not_captured", [
                #("non_json_lines_captured", json.int(non_json_count)),
              ])
            }
          }

          helpers.log_info_with(ctx, "session_completed", [
            #("otp_version", json.int(otp_version)),
            #("stderr_to_stdout", json.bool(supports_stderr)),
            #("message_count", json.int(list.length(result.messages))),
            #("non_json_lines", json.int(non_json_count)),
            #("system_count", json.int(counts.system)),
            #("result_count", json.int(counts.result)),
          ])

          helpers.log_test_complete(
            ctx,
            True,
            "Session completed with stderr tracking (OTP "
              <> int.to_string(otp_version)
              <> ", stderr_to_stdout="
              <> case supports_stderr {
              True -> "true"
              False -> "false"
            }
              <> ", non_json_lines="
              <> int.to_string(non_json_count)
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
/// NOTE: This test uses query_and_consume_with_timeout which does NOT track
/// non-JSON lines or stderr pollution. For detailed stderr/non-JSON validation
/// including OTP-dependent behavior, see verbose_stderr_capture_test_.
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
          // For stderr/parity tests, timeout indicates a potential regression
          helpers.log_error(
            ctx,
            "query_timeout_failure",
            "Session timed out - potential stderr handling regression",
          )
          helpers.log_test_complete(
            ctx,
            False,
            "timed out - stderr handling may be causing hang",
          )
          should.fail()
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
