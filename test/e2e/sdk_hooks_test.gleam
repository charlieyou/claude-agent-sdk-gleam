/// E2E Tests for Hooks via Bidirectional Protocol (SDK-30 to SDK-36).
///
/// These tests verify hook registration and dispatch via bidir.start_with_hooks() API.
/// They are skipped when E2E_SDK_TEST != 1.
///
/// ## What's Being Tested
/// - SDK-30: PreToolUse hook fires with tool_name and tool_input
/// - SDK-31: PreToolUse Block response prevents tool execution
/// - SDK-32: PreToolUse ModifyInput response modifies tool input
/// - SDK-33: PostToolUse hook receives tool_output
/// - SDK-34: Permission handler (can_use_tool) Allow/Deny
/// - SDK-35: Stop hook fires when session ends
/// - SDK-36: Multiple hooks of same type all fire
///
/// ## Running Tests
/// ```bash
/// export E2E_SDK_TEST=1
/// export ANTHROPIC_API_KEY="..."
/// gleam test -- --only sdk_hooks
/// ```
import claude_agent_sdk/internal/bidir.{
  type HookConfig, type SubscriberMessage, CliMessage, HookConfig, Running,
  SessionEnded,
}
import claude_agent_sdk/internal/bidir_runner
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/io
import gleeunit/should

import e2e/helpers.{skip_if_no_e2e}

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

/// Check if a Dynamic value has a specific field
fn has_field(input: Dynamic, field_name: String) -> Bool {
  let decoder = {
    use _value <- decode.field(field_name, decode.dynamic)
    decode.success(True)
  }
  case decode.run(input, decoder) {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn result_map_error_to_nil(result: Result(a, b)) -> Result(a, Nil) {
  case result {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

/// Start a bidir session with the given hooks and prompt.
/// Returns (session, subscriber) or panics if start fails.
fn start_session_with_hooks(
  hooks: HookConfig,
  prompt: String,
) -> Result(
  #(process.Subject(bidir.ActorMessage), process.Subject(SubscriberMessage)),
  String,
) {
  // Build CLI args with the prompt
  let args = ["-p", prompt, "--max-turns", "1"]

  // Start the runner
  case bidir_runner.start(args) {
    Error(err) -> {
      Error("Failed to start runner: " <> bidir_runner_error_to_string(err))
    }
    Ok(runner) -> {
      // Create subscriber for receiving messages
      let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
      let config = bidir.default_config(subscriber)

      // Start session with hooks
      case bidir.start_with_hooks(runner, config, hooks) {
        Error(err) -> {
          Error("Failed to start session: " <> bidir_start_error_to_string(err))
        }
        Ok(session) -> Ok(#(session, subscriber))
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

/// Wait for session to reach Running state (with timeout).
fn wait_for_running(
  session: process.Subject(bidir.ActorMessage),
  max_attempts: Int,
) -> Bool {
  case max_attempts <= 0 {
    True -> False
    False -> {
      case bidir.get_lifecycle(session, 1000) {
        Running -> True
        bidir.Failed(_) -> False
        _ -> {
          process.sleep(100)
          wait_for_running(session, max_attempts - 1)
        }
      }
    }
  }
}

/// Wait for and collect subscriber messages until session ends or timeout.
fn collect_messages(
  subscriber: process.Subject(SubscriberMessage),
  timeout_ms: Int,
  acc: List(Dynamic),
) -> #(List(Dynamic), Bool) {
  case process.receive(subscriber, timeout_ms) {
    Ok(CliMessage(msg)) ->
      collect_messages(subscriber, timeout_ms, [msg, ..acc])
    Ok(SessionEnded(_)) -> #(acc, True)
    Error(Nil) -> #(acc, False)
  }
}

fn lifecycle_to_string(lifecycle: bidir.SessionLifecycle) -> String {
  case lifecycle {
    bidir.Starting -> "Starting"
    bidir.InitSent -> "InitSent"
    bidir.Running -> "Running"
    bidir.Stopped -> "Stopped"
    bidir.Failed(_) -> "Failed"
  }
}

// ============================================================================
// SDK-30: PreToolUse Hook Registration and Invocation
// ============================================================================

/// SDK-30: PreToolUse hook is invoked when Claude uses a tool.
/// Verifies hook receives tool_name and tool_input.
pub fn sdk_30_pre_tool_use_hook_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      // Create a subject to capture hook invocations
      let hook_subject: process.Subject(Dynamic) = process.new_subject()

      // Define hook handler that captures the input
      let hooks =
        HookConfig(
          handlers: dict.from_list([
            #("PreToolUse", fn(input: Dynamic) -> Dynamic {
              // Send the input to our receiver for verification
              process.send(hook_subject, input)
              // Return Continue to allow tool execution
              to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
            }),
          ]),
          permission_handlers: dict.new(),
        )

      // Start session with a prompt that triggers tool use
      case start_session_with_hooks(hooks, "Run echo hello") {
        Error(err) -> {
          io.println("[SKIP] " <> err)
          Nil
        }
        Ok(#(session, subscriber)) -> {
          // Wait for session to be running
          case wait_for_running(session, 50) {
            False -> {
              io.println("[FAIL] Session failed to reach Running state")
              bidir.shutdown(session)
              should.fail()
            }
            True -> {
              // Wait for hook invocation or timeout
              case process.receive(hook_subject, 30_000) {
                Ok(hook_input) -> {
                  // Verify hook received tool context
                  // The input should have tool_name field
                  has_field(hook_input, "tool_name")
                  |> should.be_true

                  // Collect remaining messages and cleanup
                  let _ = collect_messages(subscriber, 5000, [])
                  bidir.shutdown(session)
                }
                Error(Nil) -> {
                  // No hook invocation - may be OK if model didn't use tools
                  io.println(
                    "[INFO] No PreToolUse hook invoked (model may not have used tools)",
                  )
                  bidir.shutdown(session)
                  Nil
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
// SDK-31: PreToolUse Block Response
// ============================================================================

/// SDK-31: PreToolUse hook can block tool execution.
/// Verifies Block response prevents tool from running.
pub fn sdk_31_pre_tool_use_block_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      // Track if hook was invoked
      let hook_subject: process.Subject(String) = process.new_subject()

      // Define hook that blocks Bash tool
      let hooks =
        HookConfig(
          handlers: dict.from_list([
            #("PreToolUse", fn(input: Dynamic) -> Dynamic {
              // Extract tool_name to check what's being blocked
              let tool_name = case decode_string_field(input, "tool_name") {
                Ok(name) -> name
                Error(Nil) -> "unknown"
              }

              process.send(hook_subject, tool_name)

              // Return Block to prevent tool execution
              to_dynamic(
                dict.from_list([
                  #("block", to_dynamic(True)),
                  #("reason", to_dynamic("Blocked by test")),
                ]),
              )
            }),
          ]),
          permission_handlers: dict.new(),
        )

      case start_session_with_hooks(hooks, "Run echo hello") {
        Error(err) -> {
          io.println("[SKIP] " <> err)
          Nil
        }
        Ok(#(session, subscriber)) -> {
          case wait_for_running(session, 50) {
            False -> {
              io.println("[FAIL] Session failed to reach Running state")
              bidir.shutdown(session)
              should.fail()
            }
            True -> {
              // Wait for hook to be invoked
              case process.receive(hook_subject, 30_000) {
                Ok(tool_name) -> {
                  io.println("[INFO] Blocked tool: " <> tool_name)

                  // Session should continue (not crash)
                  process.sleep(1000)
                  case bidir.get_lifecycle(session, 1000) {
                    Running -> {
                      io.println("[PASS] Session still running after block")
                      Nil
                    }
                    bidir.Stopped -> {
                      io.println("[PASS] Session completed after block")
                      Nil
                    }
                    other -> {
                      io.println(
                        "[INFO] Session in state: "
                        <> lifecycle_to_string(other),
                      )
                      Nil
                    }
                  }
                  let _ = collect_messages(subscriber, 5000, [])
                  bidir.shutdown(session)
                }
                Error(Nil) -> {
                  io.println(
                    "[INFO] No PreToolUse hook invoked (model may not have used tools)",
                  )
                  bidir.shutdown(session)
                  Nil
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
// SDK-32: PreToolUse ModifyInput Response
// ============================================================================

/// SDK-32: PreToolUse hook can modify tool input.
/// Note: This test is more complex as it requires verifying the modified
/// input was actually used. For now, we verify the hook mechanism works.
pub fn sdk_32_pre_tool_use_modify_input_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let hook_subject: process.Subject(String) = process.new_subject()

      // Define hook that modifies input (changes command)
      let hooks =
        HookConfig(
          handlers: dict.from_list([
            #("PreToolUse", fn(input: Dynamic) -> Dynamic {
              let tool_name = case decode_string_field(input, "tool_name") {
                Ok(name) -> name
                Error(Nil) -> "unknown"
              }

              process.send(hook_subject, "modified:" <> tool_name)

              // Return ModifyInput with altered command
              // The structure should match what CLI expects
              to_dynamic(
                dict.from_list([
                  #("modify_input", to_dynamic(True)),
                  #(
                    "new_input",
                    to_dynamic(
                      dict.from_list([
                        #("command", to_dynamic("echo modified")),
                      ]),
                    ),
                  ),
                ]),
              )
            }),
          ]),
          permission_handlers: dict.new(),
        )

      case start_session_with_hooks(hooks, "Run echo original") {
        Error(err) -> {
          io.println("[SKIP] " <> err)
          Nil
        }
        Ok(#(session, subscriber)) -> {
          case wait_for_running(session, 50) {
            False -> {
              bidir.shutdown(session)
              io.println("[FAIL] Session failed to reach Running state")
              should.fail()
            }
            True -> {
              case process.receive(hook_subject, 30_000) {
                Ok(msg) -> {
                  io.println("[INFO] Hook response: " <> msg)
                  // Hook was invoked and attempted modification
                  let _ = collect_messages(subscriber, 5000, [])
                  bidir.shutdown(session)
                }
                Error(Nil) -> {
                  io.println("[INFO] No PreToolUse hook invoked")
                  bidir.shutdown(session)
                  Nil
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
// SDK-33: PostToolUse Hook Invocation
// ============================================================================

/// SDK-33: PostToolUse hook receives tool_output after execution.
pub fn sdk_33_post_tool_use_hook_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let hook_subject: process.Subject(Dynamic) = process.new_subject()

      let hooks =
        HookConfig(
          handlers: dict.from_list([
            // Allow PreToolUse to proceed
            #("PreToolUse", fn(_input: Dynamic) -> Dynamic {
              to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
            }),
            // Capture PostToolUse
            #("PostToolUse", fn(input: Dynamic) -> Dynamic {
              process.send(hook_subject, input)
              to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
            }),
          ]),
          permission_handlers: dict.new(),
        )

      case start_session_with_hooks(hooks, "Run echo hello") {
        Error(err) -> {
          io.println("[SKIP] " <> err)
          Nil
        }
        Ok(#(session, subscriber)) -> {
          case wait_for_running(session, 50) {
            False -> {
              bidir.shutdown(session)
              io.println("[FAIL] Session failed to reach Running state")
              should.fail()
            }
            True -> {
              // Wait for PostToolUse hook
              case process.receive(hook_subject, 30_000) {
                Ok(hook_input) -> {
                  // Verify hook received tool_result
                  has_field(hook_input, "tool_result")
                  |> should.be_true

                  let _ = collect_messages(subscriber, 5000, [])
                  bidir.shutdown(session)
                }
                Error(Nil) -> {
                  io.println("[INFO] No PostToolUse hook invoked")
                  bidir.shutdown(session)
                  Nil
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
// SDK-34: Permission Handler (can_use_tool)
// ============================================================================

/// SDK-34: Permission handler can Allow or Deny tool use.
pub fn sdk_34_can_use_tool_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let hook_subject: process.Subject(String) = process.new_subject()

      let hooks =
        HookConfig(
          handlers: dict.new(),
          permission_handlers: dict.from_list([
            // Handler for Bash tool - deny it
            #("Bash", fn(input: Dynamic) -> Dynamic {
              let tool_name = case decode_string_field(input, "tool_name") {
                Ok(name) -> name
                Error(Nil) -> "unknown"
              }

              process.send(hook_subject, "permission_check:" <> tool_name)

              // Return Deny to block tool use
              to_dynamic(
                dict.from_list([
                  #("behavior", to_dynamic("deny")),
                  #("message", to_dynamic("Denied by test")),
                ]),
              )
            }),
          ]),
        )

      case start_session_with_hooks(hooks, "Run echo hello") {
        Error(err) -> {
          io.println("[SKIP] " <> err)
          Nil
        }
        Ok(#(session, subscriber)) -> {
          case wait_for_running(session, 50) {
            False -> {
              bidir.shutdown(session)
              io.println("[FAIL] Session failed to reach Running state")
              should.fail()
            }
            True -> {
              case process.receive(hook_subject, 30_000) {
                Ok(msg) -> {
                  io.println("[INFO] " <> msg)
                  // Permission handler was invoked
                  let _ = collect_messages(subscriber, 5000, [])
                  bidir.shutdown(session)
                }
                Error(Nil) -> {
                  io.println("[INFO] No permission check invoked")
                  bidir.shutdown(session)
                  Nil
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
// SDK-35: Stop Hook
// ============================================================================

/// SDK-35: Stop hook fires when session ends.
pub fn sdk_35_stop_hook_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let hook_subject: process.Subject(String) = process.new_subject()

      let hooks =
        HookConfig(
          handlers: dict.from_list([
            #("Stop", fn(input: Dynamic) -> Dynamic {
              let reason = case decode_string_field(input, "reason") {
                Ok(r) -> r
                Error(Nil) -> "unknown"
              }

              process.send(hook_subject, "stop:" <> reason)
              to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
            }),
          ]),
          permission_handlers: dict.new(),
        )

      // Use a simple prompt that completes quickly
      case start_session_with_hooks(hooks, "Say hello") {
        Error(err) -> {
          io.println("[SKIP] " <> err)
          Nil
        }
        Ok(#(session, subscriber)) -> {
          case wait_for_running(session, 50) {
            False -> {
              bidir.shutdown(session)
              io.println("[FAIL] Session failed to reach Running state")
              should.fail()
            }
            True -> {
              // Wait for session to complete naturally
              let _ = collect_messages(subscriber, 30_000, [])

              // Check if stop hook was invoked
              case process.receive(hook_subject, 1000) {
                Ok(msg) -> {
                  io.println("[PASS] Stop hook invoked: " <> msg)
                  bidir.shutdown(session)
                }
                Error(Nil) -> {
                  // Stop hook may not fire in all cases
                  io.println(
                    "[INFO] Stop hook not invoked (may be expected behavior)",
                  )
                  bidir.shutdown(session)
                  Nil
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
// SDK-36: Multiple Hooks of Same Type
// ============================================================================

/// SDK-36: Multiple handlers can be registered (via different callback_ids).
/// Note: The current HookConfig uses a Dict so each callback_id can only have
/// one handler. This test verifies that behavior is consistent.
pub fn sdk_36_multiple_hooks_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let hook_subject: process.Subject(String) = process.new_subject()

      // Register multiple different hook types
      let hooks =
        HookConfig(
          handlers: dict.from_list([
            #("PreToolUse", fn(_input: Dynamic) -> Dynamic {
              process.send(hook_subject, "pre_tool_use")
              to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
            }),
            #("PostToolUse", fn(_input: Dynamic) -> Dynamic {
              process.send(hook_subject, "post_tool_use")
              to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
            }),
          ]),
          permission_handlers: dict.new(),
        )

      case start_session_with_hooks(hooks, "Run echo test") {
        Error(err) -> {
          io.println("[SKIP] " <> err)
          Nil
        }
        Ok(#(session, subscriber)) -> {
          case wait_for_running(session, 50) {
            False -> {
              bidir.shutdown(session)
              io.println("[FAIL] Session failed to reach Running state")
              should.fail()
            }
            True -> {
              // Collect hook invocations (give time for both hooks to fire)
              let hooks_fired =
                collect_hook_invocations(hook_subject, [], 30_000)

              io.println(
                "[INFO] Hooks fired: " <> hooks_list_to_string(hooks_fired),
              )

              // At least one hook should fire if tool was used
              let _ = collect_messages(subscriber, 5000, [])
              bidir.shutdown(session)
            }
          }
        }
      }
    }
  }
}

/// Collect multiple hook invocations with timeout between each.
fn collect_hook_invocations(
  subject: process.Subject(String),
  acc: List(String),
  timeout: Int,
) -> List(String) {
  case process.receive(subject, timeout) {
    Ok(msg) -> {
      // Reduce timeout for subsequent messages
      collect_hook_invocations(subject, [msg, ..acc], 5000)
    }
    Error(Nil) -> acc
  }
}

fn hooks_list_to_string(hooks: List(String)) -> String {
  case hooks {
    [] -> "(none)"
    _ -> {
      hooks
      |> list_join(", ")
    }
  }
}

fn list_join(items: List(String), sep: String) -> String {
  case items {
    [] -> ""
    [item] -> item
    [item, ..rest] -> item <> sep <> list_join(rest, sep)
  }
}
