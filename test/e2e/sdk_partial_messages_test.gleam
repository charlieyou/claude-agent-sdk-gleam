/// E2E Tests for Partial Messages / Stream Deltas (SDK-55, SDK-56).
///
/// These tests verify include_partial_messages opt-in behavior.
/// When disabled (default), partial messages should not appear in the stream.
/// When enabled, partial messages with is_partial=true should be delivered.
///
/// ## Test Strategy
/// - SDK-55: Default disabled - verify partials NOT delivered when flag is off
/// - SDK-56: Opt-in enabled - verify partials ARE delivered when flag is on
///
/// Uses mock runner to inject partial messages and verify SDK behavior.
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// ```
import claude_agent_sdk
import claude_agent_sdk/internal/bidir
import claude_agent_sdk/message
import claude_agent_sdk/options
import claude_agent_sdk/session
import e2e/helpers.{skip_if_no_e2e}
import gleam/erlang/process
import gleam/io
import gleam/option
import gleeunit/should
import support/mock_bidir_runner

// ============================================================================
// SDK-55: Default Disabled - Partials Not Delivered
// ============================================================================

/// SDK-55: Verify default options have include_partial_messages=false
/// and that non-partial messages decode correctly with is_partial=false.
///
/// Note: The SDK passes through what CLI sends - filtering is CLI-side.
/// This test verifies:
/// 1. Default BidirOptions has include_partial_messages=false
/// 2. A non-partial message (without is_partial field) decodes with is_partial=false
pub fn sdk_55_default_partial_messages_disabled_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_55_default_partial_disabled")
      let ctx = helpers.test_step(ctx, "verify_default_options")

      // Verify default BidirOptions has include_partial_messages=false
      let default_opts = options.bidir_options()
      default_opts.include_partial_messages
      |> should.be_false

      helpers.log_info(ctx, "default_include_partial_false")

      let ctx = helpers.test_step(ctx, "setup_mock_runner")
      let mock = mock_bidir_runner.new()
      let runner = mock.runner

      // Use default options (no partial messages flag)
      let bidir_opts =
        options.BidirOptions(
          ..default_opts,
          bidir_runner_factory: option.Some(fn() { runner }),
        )

      // Verify the flag is still false
      bidir_opts.include_partial_messages
      |> should.be_false

      let ctx = helpers.test_step(ctx, "start_session")
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
          let ctx = helpers.test_step(ctx, "complete_init")
          let actor = session.get_actor(sess)

          // Complete init handshake
          let assert Ok(_init_msg) = process.receive(mock.writes, 500)
          let init_response =
            "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
          bidir.inject_message(actor, init_response)
          process.sleep(50)

          // Inject a NON-partial message (what CLI would send without the flag)
          let ctx = helpers.test_step(ctx, "inject_non_partial_message")
          let complete_msg =
            "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-sonnet-4\",\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Complete response\"}],\"stop_reason\":\"end_turn\"},\"session_id\":\"sess_1\"}"
          bidir.inject_message(actor, complete_msg)

          // Verify we receive the message with is_partial=false
          let ctx = helpers.test_step(ctx, "verify_is_partial_false")
          let messages_subject = claude_agent_sdk.messages(sess)
          case process.receive(messages_subject, 1000) {
            Ok(message.Assistant(msg)) -> {
              case msg.is_partial {
                False -> {
                  helpers.log_info(ctx, "non_partial_message_received")
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "Default options correctly set, non-partial message received",
                  )
                }
                True -> {
                  helpers.log_error(
                    ctx,
                    "unexpected_partial",
                    "Message has is_partial=true but we injected non-partial",
                  )
                  helpers.log_test_complete(
                    ctx,
                    False,
                    "Expected is_partial=false",
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
              helpers.log_test_complete(
                ctx,
                False,
                "Expected Assistant message",
              )
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
  }
}

// ============================================================================
// SDK-56: Opt-in Enabled - Partials Delivered
// ============================================================================

/// SDK-56: With include_partial_messages=true, partial messages with
/// is_partial=true should be delivered through the messages subject.
///
/// This complements SDK-54 by testing the explicit opt-in path using
/// with_partial_messages() builder function.
pub fn sdk_56_partial_messages_optin_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_56_partial_messages_optin")
      let ctx = helpers.test_step(ctx, "setup_mock_runner")

      let mock = mock_bidir_runner.new()
      let runner = mock.runner

      // Enable partial messages via builder
      let bidir_opts =
        options.bidir_options()
        |> options.with_partial_messages
        |> fn(opts) {
          options.BidirOptions(
            ..opts,
            bidir_runner_factory: option.Some(fn() { runner }),
          )
        }

      // Verify the flag is now true
      let ctx = helpers.test_step(ctx, "verify_optin_flag")
      bidir_opts.include_partial_messages
      |> should.be_true

      helpers.log_info(ctx, "include_partial_messages_enabled")

      let ctx = helpers.test_step(ctx, "start_session")
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
          let ctx = helpers.test_step(ctx, "complete_init")
          let actor = session.get_actor(sess)

          // Complete init handshake
          let assert Ok(_init_msg) = process.receive(mock.writes, 500)
          let init_response =
            "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
          bidir.inject_message(actor, init_response)
          process.sleep(50)

          // Inject partial message (is_partial: true)
          let ctx = helpers.test_step(ctx, "inject_partial_message")
          let partial_msg =
            "{\"type\":\"assistant\",\"is_partial\":true,\"message\":{\"model\":\"claude-sonnet-4\",\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Streaming...\"}],\"stop_reason\":null},\"session_id\":\"sess_1\"}"
          bidir.inject_message(actor, partial_msg)

          // Verify we receive the partial message
          let ctx = helpers.test_step(ctx, "verify_partial_received")
          let messages_subject = claude_agent_sdk.messages(sess)
          case process.receive(messages_subject, 1000) {
            Ok(message.Assistant(msg)) -> {
              case msg.is_partial {
                True -> {
                  helpers.log_info(ctx, "partial_message_received")

                  // Now inject final message
                  let ctx = helpers.test_step(ctx, "inject_final_message")
                  let final_msg =
                    "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-sonnet-4\",\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Complete response\"}],\"stop_reason\":\"end_turn\"},\"session_id\":\"sess_1\"}"
                  bidir.inject_message(actor, final_msg)

                  // Verify final message
                  let ctx = helpers.test_step(ctx, "verify_final_message")
                  case process.receive(messages_subject, 1000) {
                    Ok(message.Assistant(final)) -> {
                      case final.is_partial {
                        False -> {
                          helpers.log_info(ctx, "final_message_received")
                          helpers.log_test_complete(
                            ctx,
                            True,
                            "Opt-in partial messages: partial->final flow verified",
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
                            "Final should have is_partial=false",
                          )
                          should.fail()
                        }
                      }
                    }
                    Ok(_) -> {
                      helpers.log_error(
                        ctx,
                        "wrong_message_type",
                        "Expected Assistant message for final",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "Final should be Assistant message",
                      )
                      should.fail()
                    }
                    Error(Nil) -> {
                      helpers.log_error(
                        ctx,
                        "final_timeout",
                        "Final message not received",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "Final message timed out",
                      )
                      should.fail()
                    }
                  }
                }
                False -> {
                  helpers.log_error(
                    ctx,
                    "not_partial",
                    "Expected is_partial=true",
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
              helpers.log_test_complete(
                ctx,
                False,
                "Expected Assistant message",
              )
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
  }
}

// ============================================================================
// SDK-57: Multiple Partials Sequence
// ============================================================================

/// SDK-57: With partial messages enabled, multiple partial updates followed
/// by a final message should all be delivered in order.
///
/// This tests the streaming delta behavior where content is incrementally
/// built up through multiple partial messages before completion.
pub fn sdk_57_multiple_partials_sequence_test_() {
  use <- helpers.with_e2e_timeout()
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_57_multiple_partials")
      let ctx = helpers.test_step(ctx, "setup_mock_runner")

      let mock = mock_bidir_runner.new()
      let runner = mock.runner

      let bidir_opts =
        options.bidir_options()
        |> options.with_partial_messages
        |> fn(opts) {
          options.BidirOptions(
            ..opts,
            bidir_runner_factory: option.Some(fn() { runner }),
          )
        }

      let ctx = helpers.test_step(ctx, "start_session")
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
          let ctx = helpers.test_step(ctx, "complete_init")
          let actor = session.get_actor(sess)

          // Complete init handshake
          let assert Ok(_init_msg) = process.receive(mock.writes, 500)
          let init_response =
            "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
          bidir.inject_message(actor, init_response)
          process.sleep(50)

          // Inject sequence of partials
          let ctx = helpers.test_step(ctx, "inject_partial_sequence")

          // Partial 1: "Hello"
          let partial1 =
            "{\"type\":\"assistant\",\"is_partial\":true,\"message\":{\"model\":\"claude-sonnet-4\",\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}],\"stop_reason\":null},\"session_id\":\"sess_1\"}"
          bidir.inject_message(actor, partial1)

          // Partial 2: "Hello, World"
          let partial2 =
            "{\"type\":\"assistant\",\"is_partial\":true,\"message\":{\"model\":\"claude-sonnet-4\",\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Hello, World\"}],\"stop_reason\":null},\"session_id\":\"sess_1\"}"
          bidir.inject_message(actor, partial2)

          // Final: "Hello, World!"
          let final_msg =
            "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-sonnet-4\",\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Hello, World!\"}],\"stop_reason\":\"end_turn\"},\"session_id\":\"sess_1\"}"
          bidir.inject_message(actor, final_msg)

          // Collect all messages
          let ctx = helpers.test_step(ctx, "collect_messages")
          let messages_subject = claude_agent_sdk.messages(sess)

          // Receive first partial
          case process.receive(messages_subject, 1000) {
            Ok(message.Assistant(msg1)) -> {
              msg1.is_partial |> should.be_true
              helpers.log_info(ctx, "received_partial_1")

              // Receive second partial
              case process.receive(messages_subject, 1000) {
                Ok(message.Assistant(msg2)) -> {
                  msg2.is_partial |> should.be_true
                  helpers.log_info(ctx, "received_partial_2")

                  // Receive final
                  case process.receive(messages_subject, 1000) {
                    Ok(message.Assistant(msg3)) -> {
                      msg3.is_partial |> should.be_false
                      helpers.log_info(ctx, "received_final")
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "Multiple partials sequence: 2 partials + 1 final received",
                      )
                    }
                    Ok(_) -> {
                      helpers.log_error(
                        ctx,
                        "wrong_message_type",
                        "Expected Assistant message for final",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "Final should be Assistant message",
                      )
                      should.fail()
                    }
                    Error(Nil) -> {
                      helpers.log_error(
                        ctx,
                        "final_timeout",
                        "Final message not received",
                      )
                      helpers.log_test_complete(
                        ctx,
                        False,
                        "Final message timed out",
                      )
                      should.fail()
                    }
                  }
                }
                Ok(_) -> {
                  helpers.log_error(
                    ctx,
                    "wrong_message_type",
                    "Expected Assistant message for second partial",
                  )
                  helpers.log_test_complete(
                    ctx,
                    False,
                    "Second partial should be Assistant message",
                  )
                  should.fail()
                }
                Error(Nil) -> {
                  helpers.log_error(
                    ctx,
                    "second_partial_timeout",
                    "Second partial not received",
                  )
                  helpers.log_test_complete(
                    ctx,
                    False,
                    "Second partial timed out",
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
              helpers.log_test_complete(
                ctx,
                False,
                "Expected Assistant message",
              )
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
  }
}
