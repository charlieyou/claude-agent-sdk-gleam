/// E2E Tests for Permission Denied Flow.
///
/// These tests verify that the on_can_use_tool permission handler
/// can deny tool execution via the bidirectional protocol API.
///
/// ## What's Being Tested
/// - Permission handler returning Deny prevents Bash tool execution
/// - Permission handler is actually invoked (not just skipped)
/// - Tool does NOT execute after denial (verified via PostToolUse hook)
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// # Ensure the Claude CLI is authenticated (e.g., `claude auth login`)
/// gleam test -- --only sdk_permission
/// ```
import claude_agent_sdk/internal/bidir.{
  type HookConfig, type SubscriberMessage, HookConfig, Running, SessionEnded,
}
import claude_agent_sdk/internal/bidir_runner
import e2e/helpers.{get_monotonic_ms, skip_if_no_e2e}
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/io
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// ============================================================================
// Permission Denied Test
// ============================================================================

/// Test that permission denial handler works - tool does not execute when denied.
///
/// This test:
/// 1. Registers permission_handlers for Bash tool that return Deny
/// 2. Uses PostToolUse hook to detect if tool actually executes (should NOT)
/// 3. Sends prompt "Run `echo testmarker`" to trigger Bash tool
/// 4. Waits for permission handler to be invoked (fails if not invoked)
/// 5. Verifies tool did NOT execute (PostToolUse hook should not fire)
pub fn permission_denied_flow_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_permission_denied")
      let ctx = helpers.test_step(ctx, "setup_hooks")

      // Subject to track permission handler invocations
      let permission_subject: process.Subject(String) = process.new_subject()
      // Subject to track tool executions (should NOT receive messages if deny works)
      let execution_subject: process.Subject(String) = process.new_subject()

      // Create a permission handler that denies all Bash tools
      let deny_handler = fn(input: Dynamic) -> Dynamic {
        let tool_name = case decode_string_field(input, "tool_name") {
          Ok(name) -> name
          Error(Nil) -> "unknown"
        }
        process.send(permission_subject, "permission_denied:" <> tool_name)
        permission_deny_response("Denied by test")
      }

      let hooks =
        HookConfig(
          handlers: dict.from_list([
            // PostToolUse hook fires ONLY if tool actually executes
            #("PostToolUse", fn(_input: Dynamic) -> Dynamic {
              process.send(execution_subject, "tool_executed")
              continue_response()
            }),
          ]),
          permission_handlers: dict.from_list([
            // Register for both case variants of Bash
            #("Bash", deny_handler),
            #("bash", deny_handler),
          ]),
        )

      let ctx = helpers.test_step(ctx, "start_session")
      case start_session_with_hooks(hooks, "Run `echo testmarker`") {
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
              let ctx = helpers.test_step(ctx, "wait_for_permission")
              // Wait for permission check - this MUST happen for test to be valid
              case process.receive(permission_subject, 30_000) {
                Ok(msg) -> {
                  helpers.log_info_with(ctx, "permission_handler_invoked", [
                    #("message", json.string(msg)),
                  ])

                  // Collect remaining messages to let session complete
                  let ctx = helpers.test_step(ctx, "collect_stream_messages")
                  let #(messages, _ended) =
                    collect_messages(subscriber, 10_000, [])

                  // Check that no tool output ("testmarker") appears in stream
                  // This MUST be checked first - if tool output appears, test fails
                  let ctx = helpers.test_step(ctx, "check_no_tool_output")
                  let has_tool_output = check_for_testmarker(messages)
                  case has_tool_output {
                    True -> {
                      helpers.log_error(
                        ctx,
                        "testmarker_found",
                        "Tool output 'testmarker' found in stream - tool executed",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "Tool executed - testmarker found in output",
                      )
                      bidir.shutdown(session)
                      should.fail()
                    }
                    False -> helpers.log_info(ctx, "no_testmarker_in_stream")
                  }

                  // Check if denial is surfaced in message stream
                  let ctx = helpers.test_step(ctx, "check_denial_in_stream")
                  let denial_in_stream = check_for_denial_message(messages)
                  case denial_in_stream {
                    True -> helpers.log_info(ctx, "denial_surfaced_in_stream")
                    False -> {
                      helpers.log_error(
                        ctx,
                        "denial_not_in_stream",
                        "Denial not surfaced in message stream",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "Denial not surfaced in stream (AC requirement)",
                      )
                      bidir.shutdown(session)
                      should.fail()
                    }
                  }

                  let ctx = helpers.test_step(ctx, "verify_no_execution")
                  // Verify tool did NOT execute (PostToolUse should not have fired)
                  case process.receive(execution_subject, 500) {
                    Ok(_) -> {
                      // FAILURE: Tool executed despite denial
                      helpers.log_error(
                        ctx,
                        "tool_executed_despite_deny",
                        "PostToolUse hook fired - tool executed despite permission denial",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "Tool executed despite permission deny",
                      )
                      bidir.shutdown(session)
                      should.fail()
                    }
                    Error(Nil) -> {
                      // SUCCESS: All checks passed
                      helpers.log_info(ctx, "deny_prevented_execution")
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "Permission deny prevented tool execution",
                      )
                      bidir.shutdown(session)
                    }
                  }
                }
                Error(Nil) -> {
                  // FAILURE: Permission handler was not invoked
                  // This means the test cannot validate permission handling
                  helpers.log_error(
                    ctx,
                    "permission_timeout",
                    "Permission handler not invoked within timeout - test invalid",
                  )
                  helpers.log_test_complete(
                    ctx,
                    False,
                    "Permission handler not invoked",
                  )
                  // Collect messages to flush session before shutdown
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

/// Build a permission "deny" response
fn permission_deny_response(message: String) -> Dynamic {
  to_dynamic(
    dict.from_list([
      #("behavior", to_dynamic("deny")),
      #("message", to_dynamic(message)),
    ]),
  )
}

/// Build a "continue" response for hooks
fn continue_response() -> Dynamic {
  to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
}

/// Start a bidir session with the given hooks and prompt.
fn start_session_with_hooks(
  hooks: HookConfig,
  prompt: String,
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
      let config = bidir.default_config(subscriber)

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

/// Convert bidir_runner.StartError to string
fn bidir_runner_error_to_string(err: bidir_runner.StartError) -> String {
  case err {
    bidir_runner.NotImplemented -> "NotImplemented"
    bidir_runner.SpawnFailed(reason) -> "SpawnFailed: " <> reason
  }
}

/// Convert bidir.StartError to string
fn bidir_start_error_to_string(err: bidir.StartError) -> String {
  case err {
    bidir.ActorStartFailed(_) -> "ActorStartFailed"
    bidir.RunnerStartFailed(reason) -> "RunnerStartFailed: " <> reason
  }
}

/// Wait for session to reach Running state
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

/// Check if any message contains denial-related content.
/// Looks for patterns like "denied", "permission", "blocked" in message strings.
fn check_for_denial_message(messages: List(Dynamic)) -> Bool {
  list.any(messages, fn(msg) {
    let msg_str = string.lowercase(string.inspect(msg))
    string.contains(msg_str, "denied")
    || string.contains(msg_str, "permission")
    || string.contains(msg_str, "blocked")
    || string.contains(msg_str, "reject")
  })
}

/// Check if any message contains "testmarker" (would indicate tool execution).
fn check_for_testmarker(messages: List(Dynamic)) -> Bool {
  list.any(messages, fn(msg) {
    let msg_str = string.inspect(msg)
    string.contains(msg_str, "testmarker")
  })
}

/// Collect messages from subscriber until session ends or timeout.
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
        Ok(bidir.CliMessage(msg)) ->
          collect_messages_loop(subscriber, deadline_ms, [msg, ..acc])
        Ok(SessionEnded(_)) -> #(acc, True)
        Error(Nil) -> #(acc, False)
      }
  }
}

/// Handle wait_for_running failure with context logging
fn handle_wait_failure_with_ctx(
  ctx: helpers.TestContext,
  session: process.Subject(bidir.ActorMessage),
  state: bidir.SessionLifecycle,
) -> Nil {
  helpers.log_error(ctx, "session_failed", string.inspect(state))
  helpers.log_test_complete(ctx, False, "Session failed to reach Running state")
  bidir.shutdown(session)
  should.fail()
}
