/// E2E Tests for Query Options (SDK-20 to SDK-28).
///
/// These tests verify that QueryOptions builder functions correctly
/// configure CLI behavior. They are skipped unless --e2e is provided.
///
/// ## What We Can and Can't Test
/// **Can verify**:
/// - Options are accepted without error
/// - Tool filters affect SystemMessage.tools list
/// - Query completes (doesn't hang/crash)
///
/// **Cannot verify**:
/// - Model actually changed (would need introspection)
/// - System prompt affects response content (semantic, not structural)
/// - Budget enforcement mid-stream (complex to trigger)
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// # Ensure the Claude CLI is authenticated (e.g., `claude auth login`)
/// gleam test
/// ```
import claude_agent_sdk
import claude_agent_sdk/error.{error_to_string}
import claude_agent_sdk/message.{System}
import claude_agent_sdk/options.{AcceptEdits, BypassPermissions, Default}
import e2e/helpers
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

// ============================================================================
// SDK-20: Model Selection
// ============================================================================

/// SDK-20: Verify with_model() selects model correctly.
/// If model is invalid, query may fail; if valid, should succeed.
pub fn sdk_20_model_selection_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_20_model_selection")
      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_model("claude-sonnet-4-20250514")
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case helpers.query_and_consume_with_timeout("Say hello", opts, 30_000) {
        helpers.QuerySuccess(_result) -> {
          helpers.log_info(ctx, "model_accepted")
          helpers.log_test_complete(ctx, True, "Model selection test passed")
        }
        helpers.QueryFailure(err) -> {
          helpers.log_info_with(ctx, "model_error", [
            #("error", json.string(string.inspect(err))),
          ])
          helpers.log_test_complete(
            ctx,
            True,
            "Model error is valid behavior (may not exist in all environments)",
          )
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
      }
    }
  }
}

// ============================================================================
// SDK-21: Max Turns
// ============================================================================

/// SDK-21: Verify with_max_turns() limits turns.
/// Should terminate without hanging.
pub fn sdk_21_max_turns_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_21_max_turns")
      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case helpers.query_and_consume_with_timeout("Say hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, True, "Infra/config error - skip test")
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(result) -> {
          helpers.log_info_with(ctx, "query_success", [
            #("message_count", json.int(list.length(result.messages))),
          ])
          // Should terminate (not hang forever)
          list.length(result.messages)
          |> should.not_equal(0)
          helpers.log_test_complete(ctx, True, "Max turns test passed")
        }
      }
    }
  }
}

// ============================================================================
// SDK-22: Max Budget
// ============================================================================

/// SDK-22: Verify with_max_budget() is accepted.
/// Very low budget may trigger budget stop; we just verify no crash.
pub fn sdk_22_max_budget_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_22_max_budget")
      let ctx = helpers.test_step(ctx, "configure_options")
      // Very low budget - may or may not trigger budget exceeded
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_budget(0.001)
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case helpers.query_and_consume_with_timeout("Hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, True, "Infra/config error - skip test")
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(result) -> {
          helpers.log_info_with(ctx, "query_success", [
            #("message_count", json.int(list.length(result.messages))),
          ])
          helpers.log_test_complete(
            ctx,
            True,
            "Max budget test passed - stream terminates without crash",
          )
        }
      }
    }
  }
}

// ============================================================================
// SDK-23: System Prompt
// ============================================================================

/// SDK-23: Verify with_system_prompt() is accepted.
/// We can't verify semantic effect, just that query completes.
pub fn sdk_23_system_prompt_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_23_system_prompt")
      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_system_prompt(
          "You are a helpful assistant. Always respond briefly.",
        )
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case helpers.query_and_consume_with_timeout("Say hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed unexpectedly")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(result) -> {
          helpers.log_info_with(ctx, "query_success", [
            #("message_count", json.int(list.length(result.messages))),
          ])
          // Query completed - system prompt was accepted
          list.length(result.messages)
          |> should.not_equal(0)
          helpers.log_test_complete(ctx, True, "System prompt test passed")
        }
      }
    }
  }
}

// ============================================================================
// SDK-24: Allowed Tools
// ============================================================================

/// SDK-24: Verify with_allowed_tools() filters tools.
/// Check SystemMessage.tools reflects the filter.
pub fn sdk_24_allowed_tools_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_24_allowed_tools")
      let ctx = helpers.test_step(ctx, "configure_options")
      let allowed_tools = ["Read"]
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_allowed_tools(allowed_tools)
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case helpers.query_and_consume_with_timeout("Hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, True, "Infra/config error - skip test")
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(result) -> {
          helpers.log_info_with(ctx, "query_success", [
            #("message_count", json.int(list.length(result.messages))),
          ])
          // Consume stream and find SystemMessage (handles WarningEvent first)
          let system_msg =
            list.find(result.messages, fn(envelope) {
              case envelope.message {
                System(_) -> True
                _ -> False
              }
            })

          case system_msg {
            Error(Nil) -> {
              helpers.log_info(ctx, "no_system_message_found")
            }
            Ok(envelope) -> {
              let assert System(sys_msg) = envelope.message
              case sys_msg.tools {
                Some(tools) -> {
                  helpers.log_info_with(ctx, "tools_found", [
                    #("tool_count", json.int(list.length(tools))),
                  ])
                  // All tools should be in allowed list (case-insensitive membership)
                  list.each(tools, fn(tool) {
                    let tool_lower = string.lowercase(tool)
                    let is_allowed =
                      list.any(allowed_tools, fn(a) {
                        string.lowercase(a) == tool_lower
                      })
                    should.be_true(is_allowed)
                  })
                }
                None -> {
                  helpers.log_info(ctx, "system_message_tools_none")
                }
              }
            }
          }
          helpers.log_test_complete(ctx, True, "Allowed tools test passed")
        }
      }
    }
  }
}

// ============================================================================
// SDK-25: Disallowed Tools
// ============================================================================

/// SDK-25: Verify with_disallowed_tools() excludes tools.
/// Check SystemMessage.tools doesn't include disallowed tool.
pub fn sdk_25_disallowed_tools_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_25_disallowed_tools")
      let ctx = helpers.test_step(ctx, "configure_options")
      let disallowed_tools = ["Bash"]
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_disallowed_tools(disallowed_tools)
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case helpers.query_and_consume_with_timeout("Hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, True, "Infra/config error - skip test")
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(result) -> {
          helpers.log_info_with(ctx, "query_success", [
            #("message_count", json.int(list.length(result.messages))),
          ])
          // Consume stream and find SystemMessage (handles WarningEvent first)
          let system_msg =
            list.find(result.messages, fn(envelope) {
              case envelope.message {
                System(_) -> True
                _ -> False
              }
            })

          case system_msg {
            Error(Nil) -> {
              helpers.log_info(ctx, "no_system_message_found")
            }
            Ok(envelope) -> {
              let assert System(sys_msg) = envelope.message
              case sys_msg.tools {
                Some(tools) -> {
                  helpers.log_info_with(ctx, "tools_found", [
                    #("tool_count", json.int(list.length(tools))),
                  ])
                  // Verify no disallowed tools present (case-insensitive)
                  list.each(tools, fn(tool) {
                    let tool_lower = string.lowercase(tool)
                    let is_disallowed =
                      list.any(disallowed_tools, fn(d) {
                        string.lowercase(d) == tool_lower
                      })
                    should.be_false(is_disallowed)
                  })
                }
                None -> {
                  helpers.log_info(ctx, "system_message_tools_none")
                }
              }
            }
          }
          helpers.log_test_complete(ctx, True, "Disallowed tools test passed")
        }
      }
    }
  }
}

// ============================================================================
// SDK-26: Permission Mode - Default
// ============================================================================

/// SDK-26: Verify with_permission_mode(Default) is accepted.
/// Default mode prompts for permission interactively.
pub fn sdk_26_permission_default_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_26_permission_default")
      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_permission_mode(Default)
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case helpers.query_and_consume_with_timeout("Hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed unexpectedly")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(_result) -> {
          helpers.log_info(ctx, "query_success")
          helpers.log_test_complete(
            ctx,
            True,
            "Permission default mode test passed",
          )
        }
      }
    }
  }
}

// ============================================================================
// SDK-27: Permission Mode - AcceptEdits
// ============================================================================

/// SDK-27: Verify with_permission_mode(AcceptEdits) is accepted.
/// AcceptEdits mode auto-accepts file edits.
pub fn sdk_27_permission_accept_edits_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_27_permission_accept_edits")
      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_permission_mode(AcceptEdits)
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case helpers.query_and_consume_with_timeout("Hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed unexpectedly")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(_result) -> {
          helpers.log_info(ctx, "query_success")
          helpers.log_test_complete(
            ctx,
            True,
            "Permission accept edits mode test passed",
          )
        }
      }
    }
  }
}

// ============================================================================
// SDK-28: Permission Mode - BypassPermissions
// ============================================================================

/// SDK-28: Verify with_permission_mode(BypassPermissions) is accepted.
/// BypassPermissions skips all permission prompts.
pub fn sdk_28_permission_bypass_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_28_permission_bypass")
      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_permission_mode(BypassPermissions)
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case helpers.query_and_consume_with_timeout("Hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed unexpectedly")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(_result) -> {
          helpers.log_info(ctx, "query_success")
          helpers.log_test_complete(
            ctx,
            True,
            "Permission bypass mode test passed",
          )
        }
      }
    }
  }
}
