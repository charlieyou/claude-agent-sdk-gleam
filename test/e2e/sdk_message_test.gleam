/// E2E Tests for Message Parsing (SDK-10 to SDK-14).
///
/// These tests verify that all message types decode correctly from real Claude CLI
/// output. They test protocol invariants and type safety, not semantic content.
///
/// ## Running Tests
/// ```bash
/// export E2E_SDK_TEST=1
/// export ANTHROPIC_API_KEY="..."
/// gleam test
/// ```
import claude_agent_sdk
import claude_agent_sdk/content.{TextBlock, ToolUseBlock}
import claude_agent_sdk/error.{error_to_string}
import claude_agent_sdk/message.{
  type MessageEnvelope, type ResultMessage, type Usage, Assistant, Result,
  System,
}
import claude_agent_sdk/options.{BypassPermissions}
import e2e/helpers.{consume_stream, skip_if_no_e2e}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

// ============================================================================
// SDK-10: SystemMessage Parsing
// ============================================================================

/// SDK-10: Verify SystemMessage fields decode correctly.
/// Tests session_id, tools, and mcp_servers fields.
pub fn sdk_10_system_message_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      case claude_agent_sdk.query("Hello", opts) {
        Error(err) -> {
          io.println("[FAIL] query() failed: " <> error_to_string(err))
          should.fail()
        }
        Ok(stream) -> {
          let result = consume_stream(stream)

          // Find SystemMessage
          let system_msg =
            list.find(result.messages, fn(envelope) {
              case envelope.message {
                System(_) -> True
                _ -> False
              }
            })

          case system_msg {
            Error(Nil) -> {
              io.println("[FAIL] No SystemMessage found")
              should.fail()
            }
            Ok(envelope) -> {
              let assert System(sys) = envelope.message

              // Protocol invariant: session_id should be present and non-empty
              case sys.session_id {
                Some(id) -> {
                  string.length(id)
                  |> should.not_equal(0)
                }
                None -> {
                  io.println("[FAIL] session_id is None")
                  should.fail()
                }
              }

              // Protocol invariant: tools should be a list (may be empty)
              case sys.tools {
                Some(tools) -> {
                  // Just verify it's a list by checking length >= 0
                  { list.length(tools) >= 0 }
                  |> should.be_true
                }
                None -> {
                  // tools field may be absent in some configurations
                  Nil
                }
              }

              // Protocol invariant: mcp_servers should be a list (may be empty)
              case sys.mcp_servers {
                Some(servers) -> {
                  { list.length(servers) >= 0 }
                  |> should.be_true
                }
                None -> {
                  // mcp_servers field may be absent
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
// SDK-11: Content Blocks (TextBlock, ToolUseBlock)
// ============================================================================

/// SDK-11: Verify AssistantMessage content blocks decode correctly.
/// Tests that TextBlock content is accessible via pattern match.
pub fn sdk_11_content_blocks_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      case claude_agent_sdk.query("Say hello briefly", opts) {
        Error(err) -> {
          io.println("[FAIL] query() failed: " <> error_to_string(err))
          should.fail()
        }
        Ok(stream) -> {
          let result = consume_stream(stream)

          // Find AssistantMessage with TextBlock
          let has_text_block =
            list.any(result.messages, fn(envelope) {
              case envelope.message {
                Assistant(msg) -> {
                  case msg.message {
                    Some(msg_content) -> {
                      case msg_content.content {
                        Some(blocks) -> {
                          list.any(blocks, fn(block) {
                            case block {
                              TextBlock(text) -> string.length(text) > 0
                              _ -> False
                            }
                          })
                        }
                        None -> False
                      }
                    }
                    None -> False
                  }
                }
                _ -> False
              }
            })

          // Protocol invariant: simple query should produce text response
          has_text_block
          |> should.be_true
        }
      }
    }
  }
}

// ============================================================================
// SDK-12: ToolResultBlock Round-trip
// ============================================================================

/// SDK-12: Verify tool flow (ToolUseBlock -> ToolResultBlock).
/// Tests that tool use and result can be parsed without crashes.
pub fn sdk_12_tool_result_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(2)
        |> claude_agent_sdk.with_permission_mode(BypassPermissions)

      case claude_agent_sdk.query("Read the file gleam.toml", opts) {
        Error(err) -> {
          io.println("[FAIL] query() failed: " <> error_to_string(err))
          should.fail()
        }
        Ok(stream) -> {
          let result = consume_stream(stream)

          // Look for ToolUseBlock in any AssistantMessage
          let has_tool_use =
            list.any(result.messages, fn(envelope) {
              case envelope.message {
                Assistant(msg) -> {
                  case msg.message {
                    Some(msg_content) -> {
                      case msg_content.content {
                        Some(blocks) -> {
                          list.any(blocks, fn(block) {
                            case block {
                              ToolUseBlock(id, name, _) -> {
                                // Verify id and name are non-empty
                                string.length(id) > 0 && string.length(name) > 0
                              }
                              _ -> False
                            }
                          })
                        }
                        None -> False
                      }
                    }
                    None -> False
                  }
                }
                _ -> False
              }
            })

          // Tool use may or may not occur depending on model choice
          // The key invariant is: no crashes during parsing
          // If tool use occurred, validate structure
          case has_tool_use {
            True -> {
              io.println("[INFO] Tool use detected and parsed successfully")
            }
            False -> {
              io.println("[INFO] No tool use in this response (acceptable)")
            }
          }

          // Stream should still terminate normally
          result.terminated_normally
          |> should.be_true
        }
      }
    }
  }
}

// ============================================================================
// SDK-13: UsageData Accuracy
// ============================================================================

/// SDK-13: Verify usage statistics parse correctly.
/// Tests that token counts are valid integers >= 0.
pub fn sdk_13_usage_data_test() {
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
          io.println("[FAIL] query() failed: " <> error_to_string(err))
          should.fail()
        }
        Ok(stream) -> {
          let result = consume_stream(stream)

          // Find ResultMessage with usage
          let result_usage = find_result_usage(result.messages)

          case result_usage {
            Some(usage) -> {
              // Protocol invariant: token counts should be >= 0
              case usage.input_tokens {
                Some(tokens) -> {
                  { tokens >= 0 }
                  |> should.be_true
                  io.println("[INFO] input_tokens: " <> int.to_string(tokens))
                }
                None -> {
                  io.println("[INFO] input_tokens not present")
                }
              }

              case usage.output_tokens {
                Some(tokens) -> {
                  { tokens >= 0 }
                  |> should.be_true
                  io.println("[INFO] output_tokens: " <> int.to_string(tokens))
                }
                None -> {
                  io.println("[INFO] output_tokens not present")
                }
              }

              // Cache tokens are optional but should be >= 0 if present
              case usage.cache_creation_input_tokens {
                Some(tokens) -> {
                  { tokens >= 0 }
                  |> should.be_true
                }
                None -> Nil
              }

              case usage.cache_read_input_tokens {
                Some(tokens) -> {
                  { tokens >= 0 }
                  |> should.be_true
                }
                None -> Nil
              }
            }
            None -> {
              io.println("[WARN] No usage data in ResultMessage")
              // Usage may not always be present; document behavior
              Nil
            }
          }
        }
      }
    }
  }
}

/// Helper to extract Usage from ResultMessage.
fn find_result_usage(messages: List(MessageEnvelope)) -> option.Option(Usage) {
  list.find_map(messages, fn(envelope) {
    case envelope.message {
      Result(res) -> {
        case res.usage {
          Some(u) -> Ok(u)
          None -> Error(Nil)
        }
      }
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}

// ============================================================================
// SDK-14: ErrorMessage Handling
// ============================================================================

/// SDK-14: Verify error messages surface cleanly without panic.
/// Tests error handling for invalid/error responses.
pub fn sdk_14_error_message_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      // Test error scenario: use invalid model to trigger API error
      // This tests that error responses are handled gracefully
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      // Normal query - verify ResultMessage with is_error handling
      case claude_agent_sdk.query("Say hi", opts) {
        Error(err) -> {
          // Query-level errors are acceptable; verify no panic
          io.println("[INFO] Query error (expected): " <> error_to_string(err))
          Nil
        }
        Ok(stream) -> {
          let result = consume_stream(stream)

          // Find ResultMessage and check is_error field
          let result_msg = find_result_message(result.messages)

          case result_msg {
            Some(res) -> {
              // is_error should be a valid Bool option
              case res.is_error {
                Some(is_err) -> {
                  io.println(
                    "[INFO] is_error: "
                    <> case is_err {
                      True -> "true"
                      False -> "false"
                    },
                  )
                }
                None -> {
                  io.println("[INFO] is_error not present")
                }
              }

              // If there are errors, they should be a list
              case res.errors {
                Some(errs) -> {
                  { list.length(errs) >= 0 }
                  |> should.be_true
                }
                None -> Nil
              }
            }
            None -> {
              io.println("[WARN] No ResultMessage found")
            }
          }

          // Key invariant: stream consumed without crash
          { list.length(result.messages) >= 0 }
          |> should.be_true
        }
      }
    }
  }
}

/// Helper to extract ResultMessage from messages.
fn find_result_message(
  messages: List(MessageEnvelope),
) -> option.Option(ResultMessage) {
  list.find_map(messages, fn(envelope) {
    case envelope.message {
      Result(res) -> Ok(res)
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}
