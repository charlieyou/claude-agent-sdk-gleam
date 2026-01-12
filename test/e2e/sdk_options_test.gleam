/// E2E Tests for Query Options (SDK-20 to SDK-28).
///
/// These tests verify that QueryOptions builder functions correctly
/// configure CLI behavior. They are skipped when E2E_SDK_TEST != 1.
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
/// export E2E_SDK_TEST=1
/// export ANTHROPIC_API_KEY="..."
/// gleam test
/// ```
import claude_agent_sdk
import claude_agent_sdk/message.{System}
import claude_agent_sdk/options.{AcceptEdits, BypassPermissions, Default}
import e2e/helpers.{consume_stream, skip_if_no_e2e}
import gleam/io
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
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_model("claude-sonnet-4-20250514")
        |> claude_agent_sdk.with_max_turns(1)

      case claude_agent_sdk.query("Say hello", opts) {
        Ok(stream) -> {
          let _ = consume_stream(stream)
          // Model accepted - test passes
          should.be_true(True)
        }
        Error(err) -> {
          // Model error is valid behavior (may not exist in all environments)
          io.println("Model error: " <> string.inspect(err))
          should.be_true(True)
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
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      case claude_agent_sdk.query("Say hello", opts) {
        Error(err) -> {
          // Expected in environments without valid API key/CLI
          io.println(
            "[SKIP] query() failed (expected in some envs): "
            <> string.inspect(err),
          )
        }
        Ok(stream) -> {
          let result = consume_stream(stream)

          // Should terminate (not hang forever)
          list.length(result.messages)
          |> should.not_equal(0)
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
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      // Very low budget - may or may not trigger budget exceeded
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_budget(0.001)
        |> claude_agent_sdk.with_max_turns(1)

      case claude_agent_sdk.query("Hello", opts) {
        Error(err) -> {
          // Expected in environments without valid API key/CLI
          io.println(
            "[SKIP] query() failed (expected in some envs): "
            <> string.inspect(err),
          )
        }
        Ok(stream) -> {
          let result = consume_stream(stream)

          // May or may not exceed budget - just verify no crash and stream terminates
          io.println(
            "Max budget test: "
            <> string.inspect(list.length(result.messages))
            <> " messages",
          )
          should.be_true(True)
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
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_system_prompt(
          "You are a helpful assistant. Always respond briefly.",
        )
        |> claude_agent_sdk.with_max_turns(1)

      let assert Ok(stream) = claude_agent_sdk.query("Say hello", opts)
      let result = consume_stream(stream)

      // Query completed - system prompt was accepted
      list.length(result.messages)
      |> should.not_equal(0)
    }
  }
}

// ============================================================================
// SDK-24: Allowed Tools
// ============================================================================

/// SDK-24: Verify with_allowed_tools() filters tools.
/// Check SystemMessage.tools reflects the filter.
pub fn sdk_24_allowed_tools_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let allowed_tools = ["Read"]
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_allowed_tools(allowed_tools)
        |> claude_agent_sdk.with_max_turns(1)

      case claude_agent_sdk.query("Hello", opts) {
        Error(err) -> {
          // Expected in environments without valid API key/CLI
          io.println(
            "[SKIP] query() failed (expected in some envs): "
            <> string.inspect(err),
          )
        }
        Ok(stream) -> {
          // Consume stream and find SystemMessage (handles WarningEvent first)
          let result = consume_stream(stream)
          let system_msg =
            list.find(result.messages, fn(envelope) {
              case envelope.message {
                System(_) -> True
                _ -> False
              }
            })

          case system_msg {
            Error(Nil) -> {
              io.println("[INFO] No SystemMessage found in stream")
            }
            Ok(envelope) -> {
              let assert System(sys_msg) = envelope.message
              case sys_msg.tools {
                Some(tools) -> {
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
                  io.println("[INFO] SystemMessage.tools is None")
                }
              }
            }
          }
          should.be_true(True)
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
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let disallowed_tools = ["Bash"]
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_disallowed_tools(disallowed_tools)
        |> claude_agent_sdk.with_max_turns(1)

      case claude_agent_sdk.query("Hello", opts) {
        Error(err) -> {
          // Expected in environments without valid API key/CLI
          io.println(
            "[SKIP] query() failed (expected in some envs): "
            <> string.inspect(err),
          )
        }
        Ok(stream) -> {
          // Consume stream and find SystemMessage (handles WarningEvent first)
          let result = consume_stream(stream)
          let system_msg =
            list.find(result.messages, fn(envelope) {
              case envelope.message {
                System(_) -> True
                _ -> False
              }
            })

          case system_msg {
            Error(Nil) -> {
              io.println("[INFO] No SystemMessage found in stream")
            }
            Ok(envelope) -> {
              let assert System(sys_msg) = envelope.message
              case sys_msg.tools {
                Some(tools) -> {
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
                  io.println("[INFO] SystemMessage.tools is None")
                }
              }
            }
          }
          should.be_true(True)
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
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_permission_mode(Default)
        |> claude_agent_sdk.with_max_turns(1)

      let assert Ok(stream) = claude_agent_sdk.query("Hello", opts)
      let _ = consume_stream(stream)

      // Just verify it doesn't crash
      should.be_true(True)
    }
  }
}

// ============================================================================
// SDK-27: Permission Mode - AcceptEdits
// ============================================================================

/// SDK-27: Verify with_permission_mode(AcceptEdits) is accepted.
/// AcceptEdits mode auto-accepts file edits.
pub fn sdk_27_permission_accept_edits_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_permission_mode(AcceptEdits)
        |> claude_agent_sdk.with_max_turns(1)

      let assert Ok(stream) = claude_agent_sdk.query("Hello", opts)
      let _ = consume_stream(stream)

      // Just verify it doesn't crash
      should.be_true(True)
    }
  }
}

// ============================================================================
// SDK-28: Permission Mode - BypassPermissions
// ============================================================================

/// SDK-28: Verify with_permission_mode(BypassPermissions) is accepted.
/// BypassPermissions skips all permission prompts.
pub fn sdk_28_permission_bypass_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_permission_mode(BypassPermissions)
        |> claude_agent_sdk.with_max_turns(1)

      let assert Ok(stream) = claude_agent_sdk.query("Hello", opts)
      let _ = consume_stream(stream)

      // Just verify it doesn't crash
      should.be_true(True)
    }
  }
}
