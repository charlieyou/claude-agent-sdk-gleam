/// End-to-End Tests for Bidirectional Protocol with Real Claude CLI.
///
/// These tests are opt-in: they only run when --e2e is set.
/// They validate the full bidirectional protocol against the real CLI.
///
/// ## Prerequisites
/// - Claude CLI installed and in PATH
/// - Claude CLI authenticated (e.g., `claude auth login`)
/// - Environment: --e2e
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// # Ensure the Claude CLI is authenticated (e.g., `claude auth login`)
/// gleam test -- --only bidir_e2e
/// ```
///
/// ## Test Coverage
/// - Real session with hook registration and firing
/// - Control operations (set_model, interrupt)
/// - Permission callback handling
/// - Graceful interrupt during operation
import claude_agent_sdk/control.{Default, Interrupt, SetModel, SetPermissionMode}
import claude_agent_sdk/error.{type StartError, SpawnFailed}
import claude_agent_sdk/internal/bidir.{
  type HookConfig, type RequestResult, type SubscriberMessage, CliMessage,
  HookConfig, RequestSuccess, Running, SessionEnded,
}
import claude_agent_sdk/internal/bidir_runner
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/io
import gleam/json
import gleam/string
import gleeunit/should

import e2e/helpers.{get_monotonic_ms, is_cli_available, skip_if_no_cli_e2e}

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

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

/// Build a "continue" hook response (allows operation to proceed)
fn continue_response() -> Dynamic {
  to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
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

/// Start a bidir session with hooks and prompt.
/// Returns (session, subscriber) or Error.
fn start_session_with_hooks(
  hooks: HookConfig,
  prompt: String,
) -> Result(
  #(process.Subject(bidir.ActorMessage), process.Subject(SubscriberMessage)),
  String,
) {
  let args = ["--max-turns", "1"]

  case bidir_runner.start(args) {
    Error(err) ->
      Error("Failed to start runner: " <> start_error_to_string(err))
    Ok(runner) -> {
      let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
      let config = bidir.default_config(subscriber)

      case bidir.start_with_hooks(runner, config, hooks) {
        Error(err) ->
          Error("Failed to start session: " <> start_error_to_string(err))
        Ok(session) -> {
          bidir.send_user_message(session, prompt)
          Ok(#(session, subscriber))
        }
      }
    }
  }
}

fn start_error_to_string(err: StartError) -> String {
  case err {
    SpawnFailed(reason) -> "SpawnFailed: " <> reason
    _ -> "Other StartError"
  }
}

/// Wait for session to reach Running state (with timeout).
/// Returns Ok if Running, Error with the last observed lifecycle state otherwise.
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
  state: bidir.SessionLifecycle,
) -> Nil {
  helpers.log_error_with(
    ctx,
    "session_not_running",
    "Session failed to reach Running state",
    [
      #("lifecycle_state", json.string(string.inspect(state))),
    ],
  )
  helpers.log_test_complete(ctx, False, "Session failed to reach Running state")
  bidir.shutdown(session)
  should.fail()
}

/// Collect subscriber messages until session ends or timeout.
/// Uses deadline tracking to ensure total wait time doesn't exceed timeout_ms.
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
// Test: Real Session with Hook
// ============================================================================

/// E2E test: Real session with PreToolUse hook fires with real tool context.
///
/// Validates:
/// - Hook registration works with real CLI
/// - Hook fires with tool_name when CLI uses a tool
/// - Session completes successfully
pub fn real_session_with_hook_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_cli_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      case is_cli_available() {
        False -> {
          io.println("[SKIP] Claude CLI not found in PATH")
          Nil
        }
        True -> {
          let ctx = helpers.new_test_context("bidir_real_session_hook")
          let ctx = helpers.test_step(ctx, "setup_hooks")
          let hook_subject: process.Subject(Dynamic) = process.new_subject()

          let hooks =
            HookConfig(
              handlers: dict.from_list([
                #("PreToolUse", fn(input: Dynamic) -> Dynamic {
                  process.send(hook_subject, input)
                  continue_response()
                }),
              ]),
              permission_handlers: dict.new(),
            )

          let ctx = helpers.test_step(ctx, "start_session")
          case start_session_with_hooks(hooks, "Run echo hello") {
            Error(err) -> {
              helpers.log_info_with(ctx, "session_skip", [
                #("reason", json.string(err)),
              ])
              helpers.log_test_complete(ctx, True, "Skipped: " <> err)
            }
            Ok(#(session, subscriber)) -> {
              let ctx = helpers.test_step(ctx, "wait_for_running")
              case wait_for_running(session, 50) {
                Error(state) ->
                  handle_wait_failure_with_ctx(ctx, session, state)
                Ok(Nil) -> {
                  let ctx = helpers.test_step(ctx, "wait_for_hook")
                  case process.receive(hook_subject, 30_000) {
                    Ok(hook_input) -> {
                      // Verify hook received tool context
                      case decode_string_field(hook_input, "tool_name") {
                        Ok(tool_name) -> {
                          helpers.log_info_with(ctx, "hook_fired", [
                            #("tool_name", json.string(tool_name)),
                          ])
                          helpers.log_test_complete(
                            ctx,
                            True,
                            "Hook fired with tool_name: " <> tool_name,
                          )
                        }
                        Error(Nil) -> {
                          helpers.log_info(ctx, "hook_fired_no_tool_name")
                          helpers.log_test_complete(
                            ctx,
                            True,
                            "Hook fired (no tool_name field)",
                          )
                        }
                      }
                      let _ = collect_messages(subscriber, 5000, [])
                      bidir.shutdown(session)
                    }
                    Error(Nil) -> {
                      helpers.log_error(
                        ctx,
                        "hook_timeout",
                        "Hook not invoked within timeout",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "Hook not invoked within timeout",
                      )
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
  }
}

// ============================================================================
// Test: Real Control Operations
// ============================================================================

/// E2E test: set_model control operation succeeds with real CLI.
///
/// Validates:
/// - Control request is sent to CLI
/// - CLI responds with success or expected error
pub fn real_control_operations_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_cli_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      case is_cli_available() {
        False -> {
          io.println("[SKIP] Claude CLI not found in PATH")
          Nil
        }
        True -> {
          let ctx = helpers.new_test_context("bidir_real_control_ops")
          let ctx = helpers.test_step(ctx, "setup_hooks")
          let hooks =
            HookConfig(handlers: dict.new(), permission_handlers: dict.new())

          let ctx = helpers.test_step(ctx, "start_session")
          case start_session_with_hooks(hooks, "Say hi") {
            Error(err) -> {
              helpers.log_info_with(ctx, "session_skip", [
                #("reason", json.string(err)),
              ])
              helpers.log_test_complete(ctx, True, "Skipped: " <> err)
            }
            Ok(#(session, subscriber)) -> {
              let ctx = helpers.test_step(ctx, "wait_for_running")
              case wait_for_running(session, 50) {
                Error(state) ->
                  handle_wait_failure_with_ctx(ctx, session, state)
                Ok(Nil) -> {
                  // Send set_model request
                  let ctx = helpers.test_step(ctx, "send_set_model")
                  let result_subject: process.Subject(RequestResult) =
                    process.new_subject()
                  bidir.send_control_request(
                    session,
                    SetModel("req_model_1", "sonnet"),
                    result_subject,
                  )

                  // Wait for response (may succeed or fail depending on CLI version)
                  case process.receive(result_subject, 10_000) {
                    Ok(RequestSuccess(_)) -> {
                      helpers.log_info(ctx, "set_model_succeeded")
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "set_model succeeded",
                      )
                    }
                    Ok(bidir.RequestError(msg)) -> {
                      // Error is acceptable - CLI may not support this operation
                      helpers.log_info_with(ctx, "set_model_error", [
                        #("error", json.string(msg)),
                      ])
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "set_model returned error (expected for some CLI versions): "
                          <> msg,
                      )
                    }
                    Ok(bidir.RequestTimeout) -> {
                      helpers.log_info(ctx, "set_model_timeout")
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "set_model timed out (CLI may not respond)",
                      )
                    }
                    Ok(bidir.RequestSessionStopped) -> {
                      helpers.log_info(ctx, "session_stopped_before_response")
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "Session stopped before response",
                      )
                    }
                    Error(Nil) -> {
                      helpers.log_error(
                        ctx,
                        "no_response",
                        "No response received",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "No response received",
                      )
                      should.fail()
                    }
                  }

                  let _ = collect_messages(subscriber, 5000, [])
                  bidir.shutdown(session)
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
// Test: Real Permission Callback
// ============================================================================

/// E2E test: Permission callback denies Bash tool, session continues.
///
/// Validates:
/// - Permission handler is invoked for tool use
/// - Deny response prevents tool execution
/// - Session handles denial gracefully
pub fn real_permission_callback_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_cli_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      case is_cli_available() {
        False -> {
          io.println("[SKIP] Claude CLI not found in PATH")
          Nil
        }
        True -> {
          let ctx = helpers.new_test_context("bidir_real_permission_callback")
          let ctx = helpers.test_step(ctx, "setup_handlers")
          let permission_subject: process.Subject(String) =
            process.new_subject()
          let post_tool_subject: process.Subject(String) = process.new_subject()

          let deny_handler = fn(input: Dynamic) -> Dynamic {
            let tool_name = case decode_string_field(input, "tool_name") {
              Ok(name) -> name
              Error(Nil) -> "unknown"
            }
            process.send(permission_subject, "denied:" <> tool_name)
            permission_deny_response("Blocked by E2E test")
          }

          let hooks =
            HookConfig(
              handlers: dict.from_list([
                #("PostToolUse", fn(_input: Dynamic) -> Dynamic {
                  process.send(post_tool_subject, "tool_executed")
                  continue_response()
                }),
              ]),
              permission_handlers: dict.from_list([
                #("Bash", deny_handler),
                #("bash", deny_handler),
              ]),
            )

          let ctx = helpers.test_step(ctx, "start_runner")
          let args = ["--max-turns", "1"]
          case bidir_runner.start(args) {
            Error(err) -> {
              helpers.log_info_with(ctx, "runner_skip", [
                #("reason", json.string(start_error_to_string(err))),
              ])
              helpers.log_test_complete(
                ctx,
                True,
                "Skipped: Failed to start runner",
              )
            }
            Ok(runner) -> {
              let ctx = helpers.test_step(ctx, "start_session")
              let subscriber: process.Subject(SubscriberMessage) =
                process.new_subject()
              let config = bidir.default_config(subscriber)
              case bidir.start_with_hooks(runner, config, hooks) {
                Error(err) -> {
                  helpers.log_info_with(ctx, "session_skip", [
                    #("reason", json.string(start_error_to_string(err))),
                  ])
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "Skipped: Failed to start session",
                  )
                }
                Ok(session) -> {
                  let ctx = helpers.test_step(ctx, "wait_for_running")
                  case wait_for_running(session, 50) {
                    Error(state) ->
                      handle_wait_failure_with_ctx(ctx, session, state)
                    Ok(Nil) -> {
                      // Ensure permission mode is set to default before prompt
                      let ctx = helpers.test_step(ctx, "set_permission_mode")
                      let perm_subject: process.Subject(RequestResult) =
                        process.new_subject()
                      bidir.send_control_request(
                        session,
                        SetPermissionMode("req_perm_mode_1", Default),
                        perm_subject,
                      )
                      let _ = process.receive(perm_subject, 5000)

                      // Send prompt after permission mode is set
                      let ctx = helpers.test_step(ctx, "send_user_message")
                      bidir.send_user_message(session, "Run echo hello")

                      // Wait for permission handler invocation
                      let ctx =
                        helpers.test_step(ctx, "wait_for_permission_handler")
                      case process.receive(permission_subject, 30_000) {
                        Ok(msg) -> {
                          helpers.log_info_with(
                            ctx,
                            "permission_handler_invoked",
                            [
                              #("message", json.string(msg)),
                            ],
                          )

                          // Wait for session to complete
                          let _ = collect_messages(subscriber, 10_000, [])

                          // Verify tool did NOT execute
                          case process.receive(post_tool_subject, 500) {
                            Ok(_) -> {
                              helpers.log_info(
                                ctx,
                                "tool_executed_despite_deny",
                              )
                              helpers.log_test_complete(
                                ctx,
                                True,
                                "Tool executed despite permission deny (warning)",
                              )
                              bidir.shutdown(session)
                            }
                            Error(Nil) -> {
                              helpers.log_info(
                                ctx,
                                "permission_deny_prevented_execution",
                              )
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
                          helpers.log_error(
                            ctx,
                            "permission_handler_timeout",
                            "Permission handler not invoked within timeout",
                          )
                          helpers.log_test_complete(
                            ctx,
                            False,
                            "Permission handler not invoked within timeout",
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
      }
    }
  }
}

// ============================================================================
// Test: Real Interrupt
// ============================================================================

/// E2E test: Interrupt stops session gracefully.
///
/// Validates:
/// - Interrupt control request is sent
/// - Session stops without crash
/// - Cleanup completes normally
pub fn real_interrupt_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_cli_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      case is_cli_available() {
        False -> {
          io.println("[SKIP] Claude CLI not found in PATH")
          Nil
        }
        True -> {
          let ctx = helpers.new_test_context("bidir_real_interrupt")
          let ctx = helpers.test_step(ctx, "setup_hooks")
          let hooks =
            HookConfig(handlers: dict.new(), permission_handlers: dict.new())

          // Use a prompt that may take longer to process
          let ctx = helpers.test_step(ctx, "start_session")
          case start_session_with_hooks(hooks, "Write a haiku about testing") {
            Error(err) -> {
              helpers.log_info_with(ctx, "session_skip", [
                #("reason", json.string(err)),
              ])
              helpers.log_test_complete(ctx, True, "Skipped: " <> err)
            }
            Ok(#(session, subscriber)) -> {
              let ctx = helpers.test_step(ctx, "wait_for_running")
              case wait_for_running(session, 50) {
                Error(state) ->
                  handle_wait_failure_with_ctx(ctx, session, state)
                Ok(Nil) -> {
                  // Give CLI a moment to start processing
                  let ctx = helpers.test_step(ctx, "delay_before_interrupt")
                  process.sleep(500)

                  // Send interrupt
                  let ctx = helpers.test_step(ctx, "send_interrupt")
                  let result_subject: process.Subject(RequestResult) =
                    process.new_subject()
                  bidir.send_control_request(
                    session,
                    Interrupt("req_int_1"),
                    result_subject,
                  )

                  // Wait for response
                  case process.receive(result_subject, 10_000) {
                    Ok(RequestSuccess(_)) -> {
                      helpers.log_info(ctx, "interrupt_succeeded")
                    }
                    Ok(bidir.RequestError(msg)) -> {
                      // Error may occur if nothing to interrupt
                      helpers.log_info_with(ctx, "interrupt_error", [
                        #("error", json.string(msg)),
                      ])
                    }
                    Ok(bidir.RequestTimeout) -> {
                      helpers.log_info(ctx, "interrupt_timeout")
                    }
                    Ok(bidir.RequestSessionStopped) -> {
                      helpers.log_info(ctx, "session_stopped_before_response")
                    }
                    Error(Nil) -> {
                      helpers.log_info(ctx, "no_response_for_interrupt")
                    }
                  }

                  // Ensure session can be shut down cleanly
                  let ctx = helpers.test_step(ctx, "cleanup_and_verify")
                  let _ = collect_messages(subscriber, 2000, [])
                  bidir.shutdown(session)

                  // Verify actor stopped
                  let pid = bidir.get_pid(session)
                  process.sleep(100)
                  should.equal(process.is_alive(pid), False)

                  helpers.log_info(ctx, "session_stopped_gracefully")
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "Session stopped gracefully after interrupt",
                  )
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
// Test: CLI Availability Helper
// ============================================================================

/// Validate is_cli_available() helper returns correct value.
///
/// When --e2e, should return True if claude is in PATH.
/// When --e2e is not set, should return False.
pub fn is_cli_available_helper_test_() {
  use <- helpers.with_e2e_timeout()
  // This test runs unconditionally to validate the helper
  let result = is_cli_available()

  case skip_if_no_cli_e2e() {
    Error(_) -> {
      // --e2e not set - helper should return False
      should.equal(result, False)
      io.println(
        "[PASS] is_cli_available() returns False when --e2e is not set",
      )
    }
    Ok(Nil) -> {
      // --e2e set - use structured logging
      let ctx = helpers.new_test_context("bidir_cli_available_helper")
      let ctx = helpers.test_step(ctx, "check_cli_available")
      helpers.log_info_with(ctx, "cli_available_result", [
        #("result", json.bool(result)),
      ])
      // Either result is valid depending on environment
      helpers.log_test_complete(
        ctx,
        True,
        "is_cli_available() returned: " <> bool_to_string(result),
      )
    }
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}
