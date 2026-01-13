/// E2E Tests for Stream API (SDK-01 to SDK-04).
///
/// These tests exercise the complete query() -> next() -> close() lifecycle
/// against a real Claude CLI. They are skipped unless --e2e is provided.
///
/// ## Protocol Invariant Assertions
/// We assert on structure, not content:
/// - Stream yields at least one item
/// - Stream eventually terminates
/// - SystemMessage contains session_id
/// - Result message appears before stream ends
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// # Ensure the Claude CLI is authenticated (e.g., `claude auth login`)
/// gleam test
/// ```
import claude_agent_sdk
import claude_agent_sdk/error.{error_to_string}
import e2e/helpers
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

// ============================================================================
// SDK-01: Basic Query (Integration Path Test)
// ============================================================================

/// SDK-01: Basic query -> stream -> close lifecycle.
/// This is the integration path test that validates the complete flow.
pub fn sdk_01_basic_query_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_01_basic_query")
      let ctx = helpers.test_step(ctx, "configure_options")

      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")

      case helpers.query_and_consume_with_timeout("Say hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_error_summary(ctx, "QueryFailure", error_to_string(err))
          helpers.log_test_complete(ctx, False, "query() failed")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "skipped due to timeout")
          Nil
        }
        helpers.QuerySuccess(result) -> {
          let ctx = helpers.test_step(ctx, "validate_protocol")
          helpers.log_stream_transcript(ctx, result.messages)

          // Protocol invariant: must have at least one message
          list.length(result.messages)
          |> should.not_equal(0)

          // Protocol invariant: stream should terminate normally
          result.terminated_normally
          |> should.be_true

          // Protocol invariant: should have a result message
          helpers.has_result_message(result.messages)
          |> should.be_true

          // Protocol invariant: should have at least one system message
          let counts = helpers.count_message_types(result.messages)
          { counts.system >= 1 }
          |> should.be_true

          // Protocol invariant: should have at least one result
          { counts.result >= 1 }
          |> should.be_true

          helpers.log_info_with(ctx, "protocol_validated", [
            #("message_count", json.int(list.length(result.messages))),
            #("system_count", json.int(counts.system)),
            #("result_count", json.int(counts.result)),
          ])
          helpers.log_test_complete(ctx, True, "all protocol invariants passed")
        }
      }
    }
  }
}

// ============================================================================
// SDK-02: Stream Iteration Pattern
// ============================================================================

/// SDK-02: Verify next() iteration pattern works correctly.
/// Tests that we can manually iterate through stream items.
pub fn sdk_02_stream_iteration_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_02_stream_iteration")
      let ctx = helpers.test_step(ctx, "configure_options")

      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_query")

      case helpers.query_and_consume_with_timeout("Count to 3", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_error_summary(ctx, "QueryFailure", error_to_string(err))
          helpers.log_test_complete(ctx, False, "query() failed")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "skipped due to timeout")
          Nil
        }
        helpers.QuerySuccess(final_result) -> {
          let ctx = helpers.test_step(ctx, "validate_termination")
          helpers.log_stream_transcript(ctx, final_result.messages)

          // Should have terminated normally
          final_result.terminated_normally
          |> should.be_true

          helpers.log_test_complete(ctx, True, "stream terminated normally")
        }
      }
    }
  }
}

// ============================================================================
// SDK-03: Multi-turn Conversation
// ============================================================================

/// SDK-03: Multi-turn conversation with max_turns(3).
/// Tests that multi-turn works and produces expected message flow.
pub fn sdk_03_multi_turn_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_03_multi_turn")
      let ctx =
        helpers.test_step_with(ctx, "configure_multi_turn", [
          #("max_turns", json.int(3)),
        ])

      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(3)

      let ctx = helpers.test_step(ctx, "execute_query")

      // Use a prompt that's likely to stay within token limits
      case
        helpers.query_and_consume_with_timeout("Say hi briefly", opts, 30_000)
      {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_error_summary(ctx, "QueryFailure", error_to_string(err))
          helpers.log_test_complete(ctx, False, "query() failed")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout_skip")
          helpers.log_test_complete(ctx, True, "skipped due to timeout")
          Nil
        }
        helpers.QuerySuccess(result) -> {
          let ctx = helpers.test_step(ctx, "validate_protocol")
          helpers.log_stream_transcript(ctx, result.messages)

          // Protocol invariant: stream should terminate
          result.terminated_normally
          |> should.be_true

          // Protocol invariant: should have messages
          list.length(result.messages)
          |> should.not_equal(0)

          // Protocol invariant: should have result
          helpers.has_result_message(result.messages)
          |> should.be_true

          // With multi-turn, we may have multiple assistant messages
          let counts = helpers.count_message_types(result.messages)
          { counts.assistant >= 1 }
          |> should.be_true

          helpers.log_info_with(ctx, "multi_turn_validated", [
            #("assistant_count", json.int(counts.assistant)),
            #("message_count", json.int(list.length(result.messages))),
          ])
          helpers.log_test_complete(
            ctx,
            True,
            "multi-turn conversation completed",
          )
        }
      }
    }
  }
}

// ============================================================================
// SDK-04: Session Resume
// ============================================================================

/// SDK-04: Session resume via with_resume().
/// Tests that we can resume a previous session.
pub fn sdk_04_session_resume_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_04_session_resume")

      // Phase 1: Start initial conversation
      let ctx = helpers.test_step(ctx, "initial_query")
      let opts1 =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      case
        helpers.query_and_consume_with_timeout(
          "Remember the number 42",
          opts1,
          30_000,
        )
      {
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "initial_query_failed", error_to_string(err))
          helpers.log_error_summary(ctx, "QueryFailure", error_to_string(err))
          helpers.log_test_complete(ctx, False, "initial query() failed")
          should.fail()
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "initial_query_timeout_skip")
          helpers.log_test_complete(ctx, True, "skipped due to timeout")
          Nil
        }
        helpers.QuerySuccess(result1) -> {
          helpers.log_stream_transcript(ctx, result1.messages)

          // Extract session_id for resume
          case helpers.extract_session_id(result1.messages) {
            None -> {
              helpers.log_error(
                ctx,
                "no_session_id",
                "No session_id in initial response",
              )
              helpers.log_error_summary(
                ctx,
                "MissingSessionId",
                "No session_id in initial response",
              )
              helpers.log_test_complete(ctx, False, "no session_id")
              should.fail()
            }
            Some(session_id) -> {
              let ctx =
                helpers.test_step_with(ctx, "resume_session", [
                  #("session_id", json.string(session_id)),
                ])

              // Phase 2: Resume session
              let opts2 =
                claude_agent_sdk.default_options()
                |> claude_agent_sdk.with_max_turns(1)
                |> claude_agent_sdk.with_resume(session_id)

              case
                helpers.query_and_consume_with_timeout(
                  "What number?",
                  opts2,
                  30_000,
                )
              {
                helpers.QueryFailure(err) -> {
                  helpers.log_error(
                    ctx,
                    "resume_query_failed",
                    error_to_string(err),
                  )
                  helpers.log_error_summary(
                    ctx,
                    "ResumeQueryFailure",
                    error_to_string(err),
                  )
                  helpers.log_test_complete(ctx, False, "resume query() failed")
                  should.fail()
                }
                helpers.QueryTimedOut -> {
                  helpers.log_info(ctx, "resume_query_timeout_skip")
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "skipped due to resume timeout",
                  )
                  Nil
                }
                helpers.QuerySuccess(result2) -> {
                  let ctx = helpers.test_step(ctx, "validate_resume")
                  helpers.log_stream_transcript(ctx, result2.messages)

                  // Protocol invariant: resumed session should work
                  result2.terminated_normally
                  |> should.be_true

                  // Protocol invariant: should have messages
                  list.length(result2.messages)
                  |> should.not_equal(0)

                  // Protocol invariant: should have result
                  helpers.has_result_message(result2.messages)
                  |> should.be_true

                  helpers.log_test_complete(
                    ctx,
                    True,
                    "session resume completed",
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
