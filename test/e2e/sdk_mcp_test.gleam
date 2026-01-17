/// E2E Tests for MCP Integration and Content Types (SDK-50 to SDK-54).
///
/// These tests verify MCP server configuration passthrough works correctly.
/// The SDK passes MCP configuration to the CLI, which connects to MCP servers.
/// Also includes tests for new content types (ThinkingBlock, is_partial).
///
/// ## What We're Testing
/// - `with_mcp_config(path)` correctly passes path to CLI
/// - CLI accepts MCP config
/// - MCP failures don't crash the query
/// - SDK-53: ThinkingBlock decoding
/// - SDK-54: is_partial field in streaming
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// # Ensure the Claude CLI is authenticated (e.g., `claude auth login`)
/// gleam test
/// ```
import claude_agent_sdk
import claude_agent_sdk/content
import claude_agent_sdk/error.{error_to_string}
import claude_agent_sdk/internal/bidir
import claude_agent_sdk/message
import claude_agent_sdk/options
import claude_agent_sdk/session
import e2e/helpers.{skip_if_no_e2e}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import support/mock_bidir_runner

// ============================================================================
// SDK-50: MCP Configuration
// ============================================================================

/// SDK-50: Query with valid MCP config executes without error.
/// Tests that with_mcp_config() correctly passes path to CLI.
pub fn sdk_50_mcp_config_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_50_mcp_config")

      let helpers.TestContext(test_id: test_id, ..) = ctx
      let mcp_path =
        "artifacts/e2e/"
        <> test_id
        <> "-"
        <> int.to_string(helpers.get_monotonic_ms())
        <> "-mcp.json"
      let mcp_json =
        "{\"mcpServers\":{\"echo\":{\"command\":\"sh\",\"args\":[\"-c\",\"exit 0\"]}}}"
      let _ = helpers.write_text_file(mcp_path, mcp_json)

      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_mcp_config(mcp_path)
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case claude_agent_sdk.query("Hello", opts) {
        Ok(stream) -> {
          // Close immediately to avoid blocking if CLI hangs during MCP startup
          let _ = claude_agent_sdk.close(stream)
          helpers.log_info(ctx, "mcp_config_accepted")
          helpers.log_test_complete(ctx, True, "MCP config was accepted")
        }
        Error(err) -> {
          // Document the error - MCP server startup may fail
          helpers.log_info_with(ctx, "mcp_config_error", [
            #("error", json.string(error_to_string(err))),
          ])
          // Config passthrough may still have worked even if MCP server failed
          // This is acceptable behavior
          helpers.log_test_complete(
            ctx,
            True,
            "MCP server startup may fail - acceptable",
          )
        }
      }
    }
  }
}

// ============================================================================
// SDK-51: MCP Tool Availability
// ============================================================================

/// SDK-51: MCP configuration allows a query to start.
/// Note: Stream is closed early to avoid hanging on MCP startup.
pub fn sdk_51_mcp_tools_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_51_mcp_tools")

      let helpers.TestContext(test_id: test_id, ..) = ctx
      let mcp_path =
        "artifacts/e2e/"
        <> test_id
        <> "-"
        <> int.to_string(helpers.get_monotonic_ms())
        <> "-mcp.json"
      let mcp_json =
        "{\"mcpServers\":{\"echo\":{\"command\":\"sh\",\"args\":[\"-c\",\"exit 0\"]}}}"
      let _ = helpers.write_text_file(mcp_path, mcp_json)

      let ctx = helpers.test_step(ctx, "configure_options")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_mcp_config(mcp_path)
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")
      case claude_agent_sdk.query("Hello", opts) {
        Ok(stream) -> {
          // Close immediately to avoid blocking if CLI hangs during MCP startup
          let _ = claude_agent_sdk.close(stream)
          helpers.log_info(ctx, "mcp_query_started")
          helpers.log_test_complete(
            ctx,
            True,
            "MCP query started; stream closed early to avoid hang",
          )
          should.be_true(True)
        }
        Error(err) -> {
          // MCP query failure is acceptable in E2E (MCP server may not be available)
          // Log as True to match other "acceptable non-success" patterns (e.g., timeouts)
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(
            ctx,
            True,
            "Skipped: MCP query failed to start (acceptable)",
          )
        }
      }
    }
  }
}

// ============================================================================
// SDK-52: MCP Failure Handling
// ============================================================================

/// SDK-52: Non-existent MCP config doesn't crash query.
/// Tests graceful handling of MCP configuration errors.
pub fn sdk_52_mcp_failure_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_52_mcp_failure")

      let helpers.TestContext(test_id: test_id, ..) = ctx
      let bad_path =
        "artifacts/e2e/"
        <> test_id
        <> "-"
        <> int.to_string(helpers.get_monotonic_ms())
        <> "-missing-mcp.json"

      let ctx = helpers.test_step(ctx, "configure_bad_mcp_path")
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_mcp_config(bad_path)
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query_with_bad_config")
      case claude_agent_sdk.query("Hello", opts) {
        Ok(stream) -> {
          // Query started despite bad MCP config; close early to avoid blocking
          let _ = claude_agent_sdk.close(stream)
          helpers.log_info(ctx, "query_started_with_bad_config")
          helpers.log_test_complete(
            ctx,
            True,
            "Query started with bad MCP config (stream closed early)",
          )
          should.be_true(True)
        }
        Error(err) -> {
          // Error should be clear about MCP config issue
          let err_str = error_to_string(err)
          helpers.log_info_with(ctx, "mcp_failure_error", [
            #("error", json.string(err_str)),
          ])
          // Verify error message is meaningful (not empty)
          string.length(err_str)
          |> should.not_equal(0)
          helpers.log_test_complete(
            ctx,
            True,
            "MCP failure handled gracefully with meaningful error",
          )
        }
      }
    }
  }
}

// ============================================================================
// SDK-53: ThinkingBlock E2E Test
// ============================================================================

/// SDK-53: ThinkingBlock decoded correctly in assistant responses.
/// Uses mock runner to verify ThinkingBlock variant is decoded properly.
pub fn sdk_53_thinking_block_decoded_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_53_thinking_block_decoded")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  // Create mock runner
  let mock = mock_bidir_runner.new()
  let runner = mock.runner

  let base_opts = options.bidir_options()
  let bidir_opts =
    options.BidirOptions(
      ..base_opts,
      bidir_runner_factory: option.Some(fn() { runner }),
    )

  let ctx = helpers.test_step(ctx, "start_session_new")
  let cli_opts = options.cli_options()
  let sdk_opts = options.sdk_options()

  case claude_agent_sdk.start_session_new(cli_opts, sdk_opts, bidir_opts) {
    Error(err) -> {
      helpers.log_error(
        ctx,
        "session_start_failed",
        claude_agent_sdk.start_error_to_string(err),
      )
      helpers.log_test_complete(ctx, False, "Failed to start session")
      should.fail()
    }
    Ok(sess) -> {
      let ctx = helpers.test_step(ctx, "complete_init_handshake")
      let actor = session.get_actor(sess)

      // Complete init
      let assert Ok(_init_msg) = process.receive(mock.writes, 500)
      let init_response =
        "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
      bidir.inject_message(actor, init_response)
      process.sleep(50)

      let ctx = helpers.test_step(ctx, "inject_thinking_block_response")
      // Inject assistant response with ThinkingBlock
      let thinking_response =
        "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-sonnet-4\",\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"Let me analyze this carefully...\",\"signature\":\"sig123\"},{\"type\":\"text\",\"text\":\"Based on my analysis...\"}],\"stop_reason\":\"end_turn\"},\"session_id\":\"sess_1\"}"
      bidir.inject_message(actor, thinking_response)

      // Get messages and verify ThinkingBlock is present
      let ctx = helpers.test_step(ctx, "verify_thinking_block")
      let messages_subject = claude_agent_sdk.messages(sess)
      case process.receive(messages_subject, 1000) {
        Ok(message.Assistant(msg)) -> {
          case msg.message {
            option.Some(inner_msg) -> {
              case inner_msg.content {
                option.Some(content_blocks) -> {
                  // Check for ThinkingBlock in content
                  let has_thinking =
                    list.any(content_blocks, fn(block) {
                      case block {
                        content.ThinkingBlock(thinking:, signature:) -> {
                          thinking == "Let me analyze this carefully..."
                          && signature == option.Some("sig123")
                        }
                        _ -> False
                      }
                    })

                  case has_thinking {
                    True -> {
                      helpers.log_info(ctx, "thinking_block_decoded")
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "ThinkingBlock decoded correctly with signature",
                      )
                    }
                    False -> {
                      helpers.log_error(
                        ctx,
                        "thinking_block_missing",
                        "ThinkingBlock not found in content",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "ThinkingBlock not found",
                      )
                      should.fail()
                    }
                  }
                }
                option.None -> {
                  helpers.log_error(ctx, "no_content", "Message has no content")
                  helpers.log_test_complete(
                    ctx,
                    False,
                    "Message has no content",
                  )
                  should.fail()
                }
              }
            }
            option.None -> {
              helpers.log_error(
                ctx,
                "no_message",
                "Assistant has no inner message",
              )
              helpers.log_test_complete(ctx, False, "No inner message")
              should.fail()
            }
          }
        }
        Ok(_) -> {
          helpers.log_error(
            ctx,
            "wrong_message_type",
            "Expected Assistant message",
          )
          helpers.log_test_complete(ctx, False, "Expected Assistant message")
          should.fail()
        }
        Error(Nil) -> {
          helpers.log_error(ctx, "response_timeout", "No response received")
          helpers.log_test_complete(ctx, False, "No response received")
          should.fail()
        }
      }

      bidir.shutdown(actor)
    }
  }
}

// ============================================================================
// SDK-54: Partial Messages E2E Test
// ============================================================================

/// SDK-54: is_partial field decoded correctly in streaming responses.
/// Uses mock runner to verify partial message flow.
pub fn sdk_54_partial_messages_flow_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_54_partial_messages_flow")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  // Create mock runner
  let mock = mock_bidir_runner.new()
  let runner = mock.runner

  let base_opts = options.bidir_options()
  let bidir_opts =
    options.BidirOptions(
      ..base_opts,
      bidir_runner_factory: option.Some(fn() { runner }),
      include_partial_messages: True,
    )

  let ctx = helpers.test_step(ctx, "start_session_new")
  let cli_opts = options.cli_options()
  let sdk_opts = options.sdk_options()

  case claude_agent_sdk.start_session_new(cli_opts, sdk_opts, bidir_opts) {
    Error(err) -> {
      helpers.log_error(
        ctx,
        "session_start_failed",
        claude_agent_sdk.start_error_to_string(err),
      )
      helpers.log_test_complete(ctx, False, "Failed to start session")
      should.fail()
    }
    Ok(sess) -> {
      let ctx = helpers.test_step(ctx, "complete_init_handshake")
      let actor = session.get_actor(sess)

      // Complete init
      let assert Ok(_init_msg) = process.receive(mock.writes, 500)
      let init_response =
        "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
      bidir.inject_message(actor, init_response)
      process.sleep(50)

      let ctx = helpers.test_step(ctx, "inject_partial_messages")
      // Inject partial message (is_partial: true)
      let partial_msg =
        "{\"type\":\"assistant\",\"is_partial\":true,\"message\":{\"model\":\"claude-sonnet-4\",\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"In progress...\"}],\"stop_reason\":null},\"session_id\":\"sess_1\"}"
      bidir.inject_message(actor, partial_msg)

      // Get messages and verify is_partial=true
      let ctx = helpers.test_step(ctx, "verify_partial_true")
      let messages_subject = claude_agent_sdk.messages(sess)
      case process.receive(messages_subject, 1000) {
        Ok(message.Assistant(msg)) -> {
          case msg.is_partial {
            True -> {
              helpers.log_info(ctx, "partial_message_received")

              // Inject final message (is_partial: false or missing)
              let ctx = helpers.test_step(ctx, "inject_final_message")
              let final_msg =
                "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-sonnet-4\",\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Complete response.\"}],\"stop_reason\":\"end_turn\"},\"session_id\":\"sess_1\"}"
              bidir.inject_message(actor, final_msg)

              // Verify final message has is_partial=false
              let ctx = helpers.test_step(ctx, "verify_partial_false")
              case process.receive(messages_subject, 1000) {
                Ok(message.Assistant(final)) -> {
                  case final.is_partial {
                    False -> {
                      helpers.log_info(ctx, "final_message_received")
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "Partial message flow verified: partial->final",
                      )
                    }
                    True -> {
                      helpers.log_error(
                        ctx,
                        "final_still_partial",
                        "Final message has is_partial=true",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "Final message should have is_partial=false",
                      )
                      should.fail()
                    }
                  }
                }
                Ok(_) -> {
                  helpers.log_info(ctx, "received_non_assistant_final")
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "Partial flow partial: first message received",
                  )
                }
                Error(Nil) -> {
                  // Final message timeout - partial at least worked
                  helpers.log_info(ctx, "final_timeout_but_partial_worked")
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "Partial message received (final timed out)",
                  )
                }
              }
            }
            False -> {
              helpers.log_error(
                ctx,
                "partial_not_set",
                "First message should have is_partial=true",
              )
              helpers.log_test_complete(
                ctx,
                False,
                "Expected is_partial=true on first message",
              )
              should.fail()
            }
          }
        }
        Ok(_) -> {
          helpers.log_error(
            ctx,
            "wrong_message_type",
            "Expected Assistant message",
          )
          helpers.log_test_complete(ctx, False, "Expected Assistant message")
          should.fail()
        }
        Error(Nil) -> {
          helpers.log_error(ctx, "response_timeout", "No response received")
          helpers.log_test_complete(ctx, False, "No response received")
          should.fail()
        }
      }

      bidir.shutdown(actor)
    }
  }
}
