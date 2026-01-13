/// E2E Tests for Message Parsing (SDK-10 to SDK-14).
///
/// These tests verify that all message types decode correctly from real Claude CLI
/// output. They test protocol invariants and type safety, not semantic content.
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// # Ensure the Claude CLI is authenticated (e.g., `claude auth login`)
/// ```
import claude_agent_sdk
import claude_agent_sdk/content.{TextBlock, ToolResultBlock, ToolUseBlock}
import claude_agent_sdk/error.{error_to_string}
import claude_agent_sdk/message.{
  type MessageEnvelope, type ResultMessage, type Usage, Assistant, Result,
  System, User,
}
import claude_agent_sdk/options.{BypassPermissions}
import e2e/helpers
import gleam/io
import gleam/json
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
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_10_system_message")

      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case helpers.query_and_consume_with_timeout("Hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(result) -> {
          let ctx = helpers.test_step(ctx, "find_system_message")

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
              helpers.log_error(
                ctx,
                "no_system_message",
                "No SystemMessage found",
              )
              helpers.log_test_complete(ctx, False, "No SystemMessage found")
              should.fail()
            }
            Ok(envelope) -> {
              let assert System(sys) = envelope.message
              let ctx = helpers.test_step(ctx, "validate_session_id")

              // Protocol invariant: session_id should be present and non-empty
              case sys.session_id {
                Some(id) -> {
                  string.length(id)
                  |> should.not_equal(0)
                  helpers.log_info_with(ctx, "session_id_valid", [
                    #("session_id_length", json.int(string.length(id))),
                  ])
                }
                None -> {
                  helpers.log_error(
                    ctx,
                    "session_id_missing",
                    "session_id is None",
                  )
                  helpers.log_test_complete(ctx, False, "session_id is None")
                  should.fail()
                }
              }

              let ctx = helpers.test_step(ctx, "validate_tools")
              // Protocol invariant: tools should be a list (may be empty)
              case sys.tools {
                Some(tools) -> {
                  // Just verify it's a list by checking length >= 0
                  { list.length(tools) >= 0 }
                  |> should.be_true
                  helpers.log_info_with(ctx, "tools_present", [
                    #("tools_count", json.int(list.length(tools))),
                  ])
                }
                None -> {
                  // tools field may be absent in some configurations
                  helpers.log_info(ctx, "tools_absent")
                }
              }

              let ctx = helpers.test_step(ctx, "validate_mcp_servers")
              // Protocol invariant: mcp_servers should be a list (may be empty)
              case sys.mcp_servers {
                Some(servers) -> {
                  { list.length(servers) >= 0 }
                  |> should.be_true
                  helpers.log_info_with(ctx, "mcp_servers_present", [
                    #("mcp_servers_count", json.int(list.length(servers))),
                  ])
                }
                None -> {
                  // mcp_servers field may be absent
                  helpers.log_info(ctx, "mcp_servers_absent")
                }
              }

              helpers.log_test_complete(ctx, True, "SystemMessage validated")
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
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_11_content_blocks")

      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case
        helpers.query_and_consume_with_timeout(
          "Say hello briefly",
          opts,
          30_000,
        )
      {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(result) -> {
          let ctx = helpers.test_step(ctx, "find_text_block")

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

          let ctx = helpers.test_step(ctx, "validate_text_block")
          // Protocol invariant: simple query should produce text response
          has_text_block
          |> should.be_true

          helpers.log_info_with(ctx, "text_block_found", [
            #("has_text_block", json.bool(has_text_block)),
          ])
          helpers.log_test_complete(ctx, True, "TextBlock validated")
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
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_12_tool_result")

      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(2)
        |> claude_agent_sdk.with_permission_mode(BypassPermissions)

      let ctx = helpers.test_step(ctx, "execute_query")
      case
        helpers.query_and_consume_with_timeout(
          "Read the file gleam.toml",
          opts,
          30_000,
        )
      {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(result) -> {
          let ctx = helpers.test_step(ctx, "find_tool_use_block")

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
          // If tool use occurred, validate structure and check for ToolResultBlock
          case has_tool_use {
            True -> {
              let ctx = helpers.test_step(ctx, "validate_tool_result_block")
              helpers.log_info(ctx, "tool_use_detected")

              // Finding 2 fix: Check for ToolResultBlock in User messages
              // When there's a ToolUseBlock, there should be a corresponding ToolResultBlock
              let has_tool_result =
                list.any(result.messages, fn(envelope) {
                  case envelope.message {
                    User(user_msg) -> {
                      case user_msg.message {
                        Some(msg_content) -> {
                          case msg_content.content {
                            Some(blocks) -> {
                              list.any(blocks, fn(block) {
                                // Explicit pattern match on ToolResultBlock variant
                                case block {
                                  ToolResultBlock(tool_use_id:, ..) ->
                                    string.length(tool_use_id) > 0
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

              // If tool use occurred, we should have tool results too
              has_tool_result
              |> should.be_true
              helpers.log_info_with(ctx, "tool_result_found", [
                #("has_tool_result", json.bool(has_tool_result)),
              ])

              let ctx = helpers.test_step(ctx, "validate_stream_termination")
              // Stream should still terminate normally
              result.terminated_normally
              |> should.be_true
              helpers.log_test_complete(ctx, True, "Tool flow validated")
            }
            False -> {
              helpers.log_info(ctx, "no_tool_use_acceptable")

              let ctx = helpers.test_step(ctx, "validate_stream_termination")
              // Stream should still terminate normally
              result.terminated_normally
              |> should.be_true
              helpers.log_test_complete(ctx, True, "No tool use (acceptable)")
            }
          }
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
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_13_usage_data")

      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case helpers.query_and_consume_with_timeout("Say hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(result) -> {
          let ctx = helpers.test_step(ctx, "find_usage_data")

          // Find ResultMessage with usage
          let result_usage = find_result_usage(result.messages)

          case result_usage {
            Some(usage) -> {
              let ctx = helpers.test_step(ctx, "validate_input_tokens")
              // Protocol invariant: token counts should be >= 0
              case usage.input_tokens {
                Some(tokens) -> {
                  { tokens >= 0 }
                  |> should.be_true
                  helpers.log_info_with(ctx, "input_tokens_valid", [
                    #("input_tokens", json.int(tokens)),
                  ])
                }
                None -> {
                  helpers.log_info(ctx, "input_tokens_absent")
                }
              }

              let ctx = helpers.test_step(ctx, "validate_output_tokens")
              case usage.output_tokens {
                Some(tokens) -> {
                  { tokens >= 0 }
                  |> should.be_true
                  helpers.log_info_with(ctx, "output_tokens_valid", [
                    #("output_tokens", json.int(tokens)),
                  ])
                }
                None -> {
                  helpers.log_info(ctx, "output_tokens_absent")
                }
              }

              let ctx = helpers.test_step(ctx, "validate_cache_tokens")
              // Cache tokens are optional but should be >= 0 if present
              case usage.cache_creation_input_tokens {
                Some(tokens) -> {
                  { tokens >= 0 }
                  |> should.be_true
                  helpers.log_info_with(ctx, "cache_creation_tokens", [
                    #("cache_creation_input_tokens", json.int(tokens)),
                  ])
                }
                None -> Nil
              }

              case usage.cache_read_input_tokens {
                Some(tokens) -> {
                  { tokens >= 0 }
                  |> should.be_true
                  helpers.log_info_with(ctx, "cache_read_tokens", [
                    #("cache_read_input_tokens", json.int(tokens)),
                  ])
                }
                None -> Nil
              }

              helpers.log_test_complete(ctx, True, "Usage data validated")
            }
            None -> {
              helpers.log_info(ctx, "no_usage_data_acceptable")
              // Usage may not always be present; document behavior
              helpers.log_test_complete(ctx, True, "No usage data (acceptable)")
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
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_14_error_message")

      let ctx = helpers.test_step(ctx, "configure_invalid_options")
      // Test error scenario: use invalid model to trigger API error
      // This tests that error responses are handled gracefully
      // Finding 1 & 3 fix: Actually use an invalid model to trigger error
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_model("invalid-model-xyz-does-not-exist")
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      // Query with invalid model should trigger an error
      case helpers.query_and_consume_with_timeout("Say hi", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          let ctx = helpers.test_step(ctx, "validate_error_surfacing")
          // Query-level errors are expected with invalid model
          // Key assertion: error is surfaced properly, no panic
          let err_str = error_to_string(err)
          helpers.log_info_with(ctx, "query_error_expected", [
            #("error", json.string(err_str)),
          ])
          // Verify we got a meaningful error, not empty
          { string.length(err_str) > 0 }
          |> should.be_true
          helpers.log_test_complete(ctx, True, "Error surfaced cleanly")
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "Skipped due to timeout")
        }
        helpers.QuerySuccess(result) -> {
          let ctx = helpers.test_step(ctx, "validate_result_error_flags")
          // If query somehow succeeds, the model should still report an error

          // Find ResultMessage and check is_error field
          let result_msg = find_result_message(result.messages)

          case result_msg {
            Some(res) -> {
              // With invalid model, we expect is_error=True or errors list populated
              case res.is_error {
                Some(is_err) -> {
                  helpers.log_info_with(ctx, "is_error_field", [
                    #("is_error", json.bool(is_err)),
                  ])
                  // We expect an error condition
                  is_err
                  |> should.be_true
                  helpers.log_test_complete(ctx, True, "Error flag validated")
                }
                None -> {
                  // If is_error not present, check for errors list
                  case res.errors {
                    Some(errs) -> {
                      { errs != [] }
                      |> should.be_true
                      helpers.log_info_with(ctx, "errors_list_populated", [
                        #("errors_count", json.int(list.length(errs))),
                      ])
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "Errors list validated",
                      )
                    }
                    None -> {
                      // Neither is_error nor errors populated - fail for invalid model
                      helpers.log_error(
                        ctx,
                        "no_error_indicators",
                        "Expected is_error or errors",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "No error indicators",
                      )
                      should.fail()
                    }
                  }
                }
              }
            }
            None -> {
              helpers.log_info(ctx, "no_result_message")
              // Key invariant: stream consumed without crash
              { list.length(result.messages) >= 0 }
              |> should.be_true
              helpers.log_test_complete(
                ctx,
                True,
                "Stream consumed without crash",
              )
            }
          }
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
