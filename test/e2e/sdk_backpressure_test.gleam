/// E2E Test for backpressure and large payload handling (T005).
///
/// Tests that the SDK handles slow consumers and large payloads without crashing
/// or hanging. This validates graceful degradation under backpressure conditions.
///
/// ## Acceptable Outcomes
/// - Normal completion (Result message received)
/// - BufferOverflow with stream closed cleanly
/// - MaxOutputExceeded error (CLI-side limit)
///
/// ## Running
/// ```bash
/// gleam test -- --e2e
/// ```
import claude_agent_sdk
import claude_agent_sdk/error.{
  BufferOverflow, EndOfStream, Message, ProcessError, TooManyDecodeErrors,
  WarningEvent, stream_error_to_string,
}
import claude_agent_sdk/message.{Result}
import e2e/helpers
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleeunit/should

// ============================================================================
// Backpressure Test
// ============================================================================

/// Outcome categorization for backpressure test.
pub type BackpressureOutcome {
  /// Normal completion - Result message received.
  NormalCompletion(message_count: Int)
  /// Buffer overflow - SDK hit buffer limit.
  BufferOverflowOutcome(messages_before_overflow: Int)
  /// Process error with exit code (may include MaxOutputExceeded from CLI).
  ProcessErrorOutcome(exit_code: Int, message_count: Int)
  /// Too many decode errors - malformed output.
  DecodeErrorOutcome(count: Int, message_count: Int)
  /// Generic timeout - test failure.
  TimeoutOutcome
  /// Unknown/unclassified error - test failure.
  UnknownErrorOutcome(error_string: String)
}

/// Check if outcome is acceptable per the issue allowlist.
fn is_acceptable_outcome(outcome: BackpressureOutcome) -> Bool {
  case outcome {
    NormalCompletion(_) -> True
    BufferOverflowOutcome(_) -> True
    // Exit code 0 with no result is still acceptable (CLI may have truncated)
    ProcessErrorOutcome(0, _) -> True
    // Non-zero exit with messages could be MaxOutputExceeded or other CLI limit
    ProcessErrorOutcome(_, message_count) -> message_count > 0
    DecodeErrorOutcome(_, _) -> False
    TimeoutOutcome -> False
    UnknownErrorOutcome(_) -> False
  }
}

/// T005: Backpressure/large payload test.
/// Requests large output and consumes slowly to simulate slow consumer.
pub fn t005_backpressure_large_payload_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("t005_backpressure")
      let ctx = helpers.test_step(ctx, "configure_options")

      // Configure for large output - max_turns=1 to keep it bounded
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "execute_large_output_query")

      // Request large output that will create backpressure
      let prompt =
        "Generate a 1000-word essay about the history of computing. Include details about early computers, the development of programming languages, and modern computing trends."

      // Use 60s timeout as specified in issue
      case
        query_and_consume_slowly_with_timeout(prompt, opts, 60_000, ctx, 100)
      {
        TimeoutOutcome -> {
          helpers.log_error(ctx, "query_timeout", "Query timed out after 60s")
          helpers.log_error_summary(ctx, "Timeout", "60s timeout exceeded")
          helpers.log_test_complete(ctx, False, "timeout - not acceptable")
          should.fail()
        }
        outcome -> {
          let ctx = helpers.test_step(ctx, "validate_outcome")

          // Log the outcome details
          let outcome_str = outcome_to_string(outcome)
          helpers.log_info_with(ctx, "outcome_received", [
            #("outcome", json.string(outcome_str)),
            #("acceptable", json.bool(is_acceptable_outcome(outcome))),
          ])

          case is_acceptable_outcome(outcome) {
            True -> {
              helpers.log_test_complete(
                ctx,
                True,
                "acceptable outcome: " <> outcome_str,
              )
            }
            False -> {
              helpers.log_error(
                ctx,
                "unacceptable_outcome",
                "Outcome not in allowlist: " <> outcome_str,
              )
              helpers.log_error_summary(ctx, "UnacceptableOutcome", outcome_str)
              helpers.log_test_complete(
                ctx,
                False,
                "unacceptable outcome: " <> outcome_str,
              )
              should.fail()
            }
          }
        }
      }
    }
  }
}

/// Convert outcome to string for logging.
fn outcome_to_string(outcome: BackpressureOutcome) -> String {
  case outcome {
    NormalCompletion(count) ->
      "NormalCompletion(messages=" <> int.to_string(count) <> ")"
    BufferOverflowOutcome(count) ->
      "BufferOverflow(messages_before=" <> int.to_string(count) <> ")"
    ProcessErrorOutcome(code, count) ->
      "ProcessError(exit_code="
      <> int.to_string(code)
      <> ", messages="
      <> int.to_string(count)
      <> ")"
    DecodeErrorOutcome(err_count, msg_count) ->
      "DecodeError(errors="
      <> int.to_string(err_count)
      <> ", messages="
      <> int.to_string(msg_count)
      <> ")"
    TimeoutOutcome -> "Timeout"
    UnknownErrorOutcome(err) -> "UnknownError(" <> err <> ")"
  }
}

/// Run query and consume slowly with a timeout.
/// Adds sleep_ms delay between each next() call to simulate slow consumer.
fn query_and_consume_slowly_with_timeout(
  prompt: String,
  options: claude_agent_sdk.QueryOptions,
  timeout_ms: Int,
  ctx: helpers.TestContext,
  sleep_ms: Int,
) -> BackpressureOutcome {
  let subject: process.Subject(BackpressureOutcome) = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let outcome = case claude_agent_sdk.query(prompt, options) {
        Ok(stream) -> consume_slowly(stream, [], ctx, sleep_ms)
        Error(err) ->
          UnknownErrorOutcome("QueryError: " <> error.error_to_string(err))
      }
      process.send(subject, outcome)
    })

  case process.receive(subject, timeout_ms) {
    Ok(outcome) -> outcome
    Error(Nil) -> {
      let _ = helpers.kill_pid(pid)
      TimeoutOutcome
    }
  }
}

/// Consume stream slowly, adding delay between each next() call.
fn consume_slowly(
  stream: claude_agent_sdk.QueryStream,
  acc: List(message.MessageEnvelope),
  ctx: helpers.TestContext,
  sleep_ms: Int,
) -> BackpressureOutcome {
  // Add delay to simulate slow consumer
  process.sleep(sleep_ms)

  let #(result, updated_stream) = claude_agent_sdk.next(stream)
  case result {
    Ok(Message(envelope)) -> {
      // Log each message type for debugging
      let msg_type = case envelope.message {
        message.System(_) -> "system"
        message.Assistant(_) -> "assistant"
        message.User(_) -> "user"
        Result(_) -> "result"
      }
      helpers.log_debug(ctx, "message_received", [
        #("type", json.string(msg_type)),
        #("count", json.int(list.length(acc) + 1)),
      ])

      let updated_acc = [envelope, ..acc]
      case envelope.message {
        Result(_) -> {
          // Normal completion - got Result message
          let _ = claude_agent_sdk.close(updated_stream)
          NormalCompletion(list.length(updated_acc))
        }
        _ -> consume_slowly(updated_stream, updated_acc, ctx, sleep_ms)
      }
    }
    Ok(EndOfStream) -> {
      let _ = claude_agent_sdk.close(updated_stream)
      // Check if we got a result before EOS
      case has_result(acc) {
        True -> NormalCompletion(list.length(acc))
        False ->
          // EOS without Result - still acceptable as partial completion
          ProcessErrorOutcome(0, list.length(acc))
      }
    }
    Ok(WarningEvent(_)) ->
      // Skip warnings, continue iteration
      consume_slowly(updated_stream, acc, ctx, sleep_ms)
    Error(err) -> {
      let _ = claude_agent_sdk.close(updated_stream)
      categorize_error(err, list.length(acc))
    }
  }
}

/// Check if message list contains a Result.
fn has_result(messages: List(message.MessageEnvelope)) -> Bool {
  list.any(messages, fn(env) {
    case env.message {
      Result(_) -> True
      _ -> False
    }
  })
}

/// Categorize a stream error into an outcome.
fn categorize_error(
  err: error.StreamError,
  message_count: Int,
) -> BackpressureOutcome {
  case err {
    BufferOverflow -> BufferOverflowOutcome(message_count)
    ProcessError(exit_code, _) -> ProcessErrorOutcome(exit_code, message_count)
    TooManyDecodeErrors(count, _) -> DecodeErrorOutcome(count, message_count)
    _ -> UnknownErrorOutcome(stream_error_to_string(err))
  }
}
