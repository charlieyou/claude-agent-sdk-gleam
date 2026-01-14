/// E2E Tests for Hook Timeout Behavior (SDK-37).
///
/// These tests verify SDK behavior when hooks are slow/blocking.
///
/// ## Discovery Summary (from SDK source analysis)
///
/// The SDK has a well-defined hook timeout mechanism:
/// - Default timeout: 30,000ms (30 seconds) - see bidir.gleam:640
/// - Timeout is configurable via with_hook_timeout() in options.gleam
/// - On timeout: fail-open behavior (continue: true, reason: "timeout")
/// - Implementation in handle_hook_timeout() at bidir.gleam:1265
///
/// ## Test Strategy
///
/// Since the default timeout is 30 seconds, a full timeout test would be slow.
/// This test uses a shorter hook timeout (configured via default_hook_timeout_ms)
/// to verify the fail-open behavior without excessive wait times.
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// ```
import claude_agent_sdk/internal/bidir.{
  type HookConfig, type SubscriberMessage, CliMessage, HookConfig, Running,
  SessionEnded,
}
import claude_agent_sdk/internal/bidir_runner
import e2e/helpers.{get_monotonic_ms, skip_if_no_e2e}
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/json
import gleeunit/should

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// ============================================================================
// Constants
// ============================================================================

/// Hook timeout for this test (5 seconds).
/// Shorter than default 30s to make test faster while still testing timeout behavior.
const test_hook_timeout_ms: Int = 5000

/// How long the slow hook sleeps (2x the timeout to ensure timeout fires).
const slow_hook_sleep_ms: Int = 10_000

// ============================================================================
// Helper Functions
// ============================================================================

/// Decode a string field from Dynamic
fn decode_string_field(
  input: Dynamic,
  field_name: String,
) -> Result(String, Nil) {
  let decoder = {
    use value <- decode.field(field_name, decode.string)
    decode.success(value)
  }
  decode.run(input, decoder)
  |> result_map_error_to_nil
}

fn result_map_error_to_nil(result: Result(a, b)) -> Result(a, Nil) {
  case result {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

/// Build a "continue" hook response
fn continue_response() -> Dynamic {
  to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
}

/// Start a bidir session with hooks and custom timeout configuration.
fn start_session_with_timeout_config(
  hooks: HookConfig,
  prompt: String,
  hook_timeout_ms: Int,
) -> Result(
  #(process.Subject(bidir.ActorMessage), process.Subject(SubscriberMessage)),
  String,
) {
  let args = ["--max-turns", "1"]

  case bidir_runner.start(args) {
    Error(err) -> {
      Error("Failed to start runner: " <> bidir_runner_error_to_string(err))
    }
    Ok(runner) -> {
      let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
      // Use custom config with shorter hook timeout
      let config =
        bidir.StartConfig(
          ..bidir.default_config(subscriber),
          default_hook_timeout_ms: hook_timeout_ms,
        )

      case bidir.start_with_hooks(runner, config, hooks) {
        Error(err) -> {
          Error("Failed to start session: " <> bidir_start_error_to_string(err))
        }
        Ok(session) -> {
          bidir.send_user_message(session, prompt)
          Ok(#(session, subscriber))
        }
      }
    }
  }
}

fn bidir_runner_error_to_string(err: bidir_runner.StartError) -> String {
  case err {
    bidir_runner.NotImplemented -> "NotImplemented"
    bidir_runner.SpawnFailed(reason) -> "SpawnFailed: " <> reason
  }
}

fn bidir_start_error_to_string(err: bidir.StartError) -> String {
  case err {
    bidir.ActorStartFailed(_) -> "ActorStartFailed"
    bidir.RunnerStartFailed(reason) -> "RunnerStartFailed: " <> reason
  }
}

/// Wait for session to reach Running state.
fn wait_for_running(
  session: process.Subject(bidir.ActorMessage),
  max_attempts: Int,
) -> Result(Nil, bidir.SessionLifecycle) {
  wait_for_running_loop(session, max_attempts, bidir.Starting)
}

fn wait_for_running_loop(
  session: process.Subject(bidir.ActorMessage),
  max_attempts: Int,
  last_state: bidir.SessionLifecycle,
) -> Result(Nil, bidir.SessionLifecycle) {
  case max_attempts <= 0 {
    True -> Error(last_state)
    False -> {
      let state = bidir.get_lifecycle(session, 1000)
      case state {
        Running -> Ok(Nil)
        bidir.Failed(_) -> Error(state)
        bidir.Stopped -> Error(state)
        _ -> {
          process.sleep(100)
          wait_for_running_loop(session, max_attempts - 1, state)
        }
      }
    }
  }
}

fn handle_wait_failure_with_ctx(
  ctx: helpers.TestContext,
  session: process.Subject(bidir.ActorMessage),
  _state: bidir.SessionLifecycle,
) -> Nil {
  helpers.log_error(
    ctx,
    "session_failed",
    "Session failed to reach Running state",
  )
  helpers.log_test_complete(ctx, False, "Session failed to reach Running state")
  bidir.shutdown(session)
  should.fail()
}

/// Collect messages until session ends or timeout.
fn collect_messages(
  subscriber: process.Subject(SubscriberMessage),
  timeout_ms: Int,
  acc: List(Dynamic),
) -> #(List(Dynamic), Bool) {
  let deadline_ms = get_monotonic_ms() + timeout_ms
  collect_messages_loop(subscriber, deadline_ms, acc)
}

fn collect_messages_loop(
  subscriber: process.Subject(SubscriberMessage),
  deadline_ms: Int,
  acc: List(Dynamic),
) -> #(List(Dynamic), Bool) {
  let remaining_ms = deadline_ms - get_monotonic_ms()
  case remaining_ms > 0 {
    False -> #(acc, False)
    True ->
      case process.receive(subscriber, remaining_ms) {
        Ok(CliMessage(msg)) ->
          collect_messages_loop(subscriber, deadline_ms, [msg, ..acc])
        Ok(SessionEnded(_)) -> #(acc, True)
        Error(Nil) -> #(acc, False)
      }
  }
}

// ============================================================================
// SDK-37: Hook Timeout Behavior Test
// ============================================================================

/// SDK-37: Hook timeout fires and session continues (fail-open behavior).
///
/// ## What This Tests
/// - Slow hooks trigger the timeout mechanism
/// - SDK uses fail-open: continues despite hook timeout
/// - Timeout is configurable (using 5s instead of default 30s)
///
/// ## SDK Timeout Mechanism (Documented)
/// - Default: 30,000ms per bidir.gleam:640
/// - Configurable via StartConfig.default_hook_timeout_ms
/// - On timeout: sends continue response with reason:"timeout"
/// - Implementation: handle_hook_timeout() at bidir.gleam:1265
pub fn sdk_37_hook_timeout_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_37_hook_timeout")
      let ctx = helpers.test_step(ctx, "setup_slow_hook")

      // Track when hook is called and when it completes
      let hook_started: process.Subject(Int) = process.new_subject()
      let hook_completed: process.Subject(Int) = process.new_subject()
      let post_tool_received: process.Subject(String) = process.new_subject()

      // Define slow PreToolUse hook that sleeps longer than timeout
      let hooks =
        HookConfig(
          handlers: dict.from_list([
            #("PreToolUse", fn(_input: Dynamic) -> Dynamic {
              // Signal hook started with timestamp
              let start_time = get_monotonic_ms()
              process.send(hook_started, start_time)

              // Use captured ctx from outer scope for consistent logging
              helpers.log_info_with(ctx, "hook_sleeping", [
                #("sleep_ms", json.int(slow_hook_sleep_ms)),
                #("timeout_ms", json.int(test_hook_timeout_ms)),
              ])

              // Sleep longer than the configured timeout
              process.sleep(slow_hook_sleep_ms)

              // Signal hook completed (may not reach if killed by timeout)
              process.send(hook_completed, get_monotonic_ms())

              continue_response()
            }),
            #("PostToolUse", fn(input: Dynamic) -> Dynamic {
              // If this fires, the SDK continued after timeout (fail-open worked)
              let tool_name = case decode_string_field(input, "tool_name") {
                Ok(name) -> name
                Error(Nil) -> "unknown"
              }
              process.send(post_tool_received, tool_name)
              continue_response()
            }),
          ]),
          permission_handlers: dict.new(),
        )

      let ctx = helpers.test_step(ctx, "start_session_with_timeout")
      helpers.log_info_with(ctx, "config", [
        #("hook_timeout_ms", json.int(test_hook_timeout_ms)),
        #("slow_hook_sleep_ms", json.int(slow_hook_sleep_ms)),
      ])

      // Start session with custom short timeout
      case
        start_session_with_timeout_config(
          hooks,
          "Run echo timeout_test",
          test_hook_timeout_ms,
        )
      {
        Error(err) -> {
          helpers.log_info_with(ctx, "session_skip", [
            #("reason", json.string(err)),
          ])
          helpers.log_test_complete(ctx, True, "Skipped: " <> err)
        }
        Ok(#(session, subscriber)) -> {
          let ctx = helpers.test_step(ctx, "wait_for_running")
          case wait_for_running(session, 50) {
            Error(state) -> handle_wait_failure_with_ctx(ctx, session, state)
            Ok(Nil) -> {
              let ctx = helpers.test_step(ctx, "wait_for_hook_start")
              // Wait for slow hook to be invoked
              case process.receive(hook_started, 30_000) {
                Ok(start_time) -> {
                  helpers.log_info_with(ctx, "hook_started", [
                    #("start_time_ms", json.int(start_time)),
                  ])

                  // Now wait for either:
                  // 1. PostToolUse (means timeout fired and fail-open worked)
                  // 2. Hook completed (means no timeout - unexpected)
                  let ctx = helpers.test_step(ctx, "wait_for_timeout_behavior")

                  // Wait slightly longer than hook timeout but less than hook sleep
                  let wait_time = test_hook_timeout_ms + 2000

                  // Check if PostToolUse fires (indicating fail-open behavior)
                  case process.receive(post_tool_received, wait_time) {
                    Ok(tool_name) -> {
                      // PostToolUse fired - this confirms fail-open behavior
                      let elapsed = get_monotonic_ms() - start_time
                      helpers.log_info_with(ctx, "fail_open_confirmed", [
                        #("tool_name", json.string(tool_name)),
                        #("elapsed_ms", json.int(elapsed)),
                        #("timeout_ms", json.int(test_hook_timeout_ms)),
                      ])

                      // Verify timeout fired before hook would have completed
                      let timeout_worked = elapsed < slow_hook_sleep_ms

                      case timeout_worked {
                        True -> {
                          helpers.log_info(ctx, "timeout_behavior_verified")
                          helpers.log_test_complete(
                            ctx,
                            True,
                            "Hook timeout fired after "
                              <> int.to_string(elapsed)
                              <> "ms (timeout="
                              <> int.to_string(test_hook_timeout_ms)
                              <> "ms), session continued (fail-open)",
                          )
                        }
                        False -> {
                          helpers.log_error(
                            ctx,
                            "timing_unexpected",
                            "PostToolUse received but elapsed "
                              <> int.to_string(elapsed)
                              <> "ms >= hook sleep "
                              <> int.to_string(slow_hook_sleep_ms)
                              <> "ms - timeout should have fired earlier",
                          )
                          helpers.log_test_complete(
                            ctx,
                            False,
                            "Timeout did not fire before hook would have completed",
                          )
                          let _ = collect_messages(subscriber, 5000, [])
                          bidir.shutdown(session)
                          should.fail()
                        }
                      }

                      let _ = collect_messages(subscriber, 5000, [])
                      bidir.shutdown(session)
                    }
                    Error(Nil) -> {
                      // Check if hook completed (no timeout behavior)
                      case process.receive(hook_completed, 100) {
                        Ok(complete_time) -> {
                          let elapsed = complete_time - start_time
                          helpers.log_error(
                            ctx,
                            "hook_completed_no_timeout",
                            "Hook completed in "
                              <> int.to_string(elapsed)
                              <> "ms without timeout firing (timeout="
                              <> int.to_string(test_hook_timeout_ms)
                              <> "ms)",
                          )
                          helpers.log_test_complete(
                            ctx,
                            False,
                            "Hook completed without timeout - timeout mechanism not applied",
                          )
                          let _ = collect_messages(subscriber, 5000, [])
                          bidir.shutdown(session)
                          should.fail()
                        }
                        Error(Nil) -> {
                          helpers.log_error(
                            ctx,
                            "no_post_tool_or_completion",
                            "Neither PostToolUse nor hook completion received within wait period",
                          )
                          helpers.log_test_complete(
                            ctx,
                            False,
                            "Neither PostToolUse nor hook completion received - test setup or CLI issue",
                          )
                          let _ = collect_messages(subscriber, 5000, [])
                          bidir.shutdown(session)
                          should.fail()
                        }
                      }
                    }
                  }
                }
                Error(Nil) -> {
                  helpers.log_error(
                    ctx,
                    "hook_not_invoked",
                    "PreToolUse hook not invoked within 30s",
                  )
                  helpers.log_test_complete(
                    ctx,
                    False,
                    "Hook not invoked - CLI did not execute tool",
                  )
                  let _ = collect_messages(subscriber, 5000, [])
                  bidir.shutdown(session)
                  should.fail()
                }
              }
            }
          }
        }
      }
    }
  }
}
