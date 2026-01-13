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
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      case helpers.query_and_consume_with_timeout("Say hello", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          io.println("[FAIL] query() failed: " <> error_to_string(err))
          should.fail()
        }
        helpers.QueryTimedOut -> {
          io.println("[FAIL] query() timed out")
          should.fail()
        }
        helpers.QuerySuccess(result) -> {
          // Consume stream and validate protocol invariants

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
          // (streams can include multiple System envelopes, e.g., init plus later events)
          let counts = helpers.count_message_types(result.messages)
          { counts.system >= 1 }
          |> should.be_true

          // Protocol invariant: should have at least one result
          { counts.result >= 1 }
          |> should.be_true
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
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      case helpers.query_and_consume_with_timeout("Count to 3", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          io.println("[FAIL] query() failed: " <> error_to_string(err))
          should.fail()
        }
        helpers.QueryTimedOut -> {
          io.println("[FAIL] query() timed out")
          should.fail()
        }
        helpers.QuerySuccess(final_result) -> {
          // Should have terminated normally
          final_result.terminated_normally
          |> should.be_true
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
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(3)

      // Use a prompt that's likely to stay within token limits
      case helpers.query_and_consume_with_timeout("Say hi briefly", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          io.println("[FAIL] query() failed: " <> error_to_string(err))
          should.fail()
        }
        helpers.QueryTimedOut -> {
          io.println("[FAIL] query() timed out")
          should.fail()
        }
        helpers.QuerySuccess(result) -> {

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
          // (but not guaranteed - depends on model behavior)
          let counts = helpers.count_message_types(result.messages)
          { counts.assistant >= 1 }
          |> should.be_true
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
      // Phase 1: Start initial conversation
      let opts1 =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      case helpers.query_and_consume_with_timeout(
        "Remember the number 42",
        opts1,
        30_000,
      ) {
        helpers.QueryFailure(err) -> {
          io.println("[FAIL] initial query() failed: " <> error_to_string(err))
          should.fail()
        }
        helpers.QueryTimedOut -> {
          io.println("[FAIL] initial query() timed out")
          should.fail()
        }
        helpers.QuerySuccess(result1) -> {

          // Extract session_id for resume
          case helpers.extract_session_id(result1.messages) {
            None -> {
              io.println("[FAIL] No session_id in initial response")
              should.fail()
            }
            Some(session_id) -> {
              // Phase 2: Resume session
              let opts2 =
                claude_agent_sdk.default_options()
                |> claude_agent_sdk.with_max_turns(1)
                |> claude_agent_sdk.with_resume(session_id)

              case helpers.query_and_consume_with_timeout(
                "What number?",
                opts2,
                30_000,
              ) {
                helpers.QueryFailure(err) -> {
                  io.println(
                    "[FAIL] resume query() failed: " <> error_to_string(err),
                  )
                  should.fail()
                }
                helpers.QueryTimedOut -> {
                  io.println("[FAIL] resume query() timed out")
                  should.fail()
                }
                helpers.QuerySuccess(result2) -> {

                  // Protocol invariant: resumed session should work
                  result2.terminated_normally
                  |> should.be_true

                  // Protocol invariant: should have messages
                  list.length(result2.messages)
                  |> should.not_equal(0)

                  // Protocol invariant: should have result
                  helpers.has_result_message(result2.messages)
                  |> should.be_true
                  // Note: We don't assert on content (whether it remembers 42)
                  // because that's semantic, not protocol invariant
                }
              }
            }
          }
        }
      }
    }
  }
}
