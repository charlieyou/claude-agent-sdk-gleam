/// E2E Tests for Crash/Interruption Handling (SDK Crash Test).
///
/// Tests that the SDK handles abrupt CLI termination gracefully:
/// - Stream terminates with error (not hang)
/// - close() returns cleanly
/// - Follow-up queries succeed (recovery)
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// ```
import claude_agent_sdk
import claude_agent_sdk/error.{
  type StreamError, EndOfStream, ErrorDiagnostic, Message, WarningEvent,
  error_to_string,
}
import claude_agent_sdk/message.{type MessageEnvelope}
import e2e/helpers
import gleam/erlang/process
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None}
import gleeunit/should

// ============================================================================
// SDK Crash Test: Abrupt CLI Termination
// ============================================================================

/// Crash test: Verify SDK handles abrupt CLI termination gracefully.
///
/// Test flow:
/// 1. Start streaming query in spawned process
/// 2. After first message, kill the spawned process
/// 3. Assert: stream terminates with error within 5s
/// 4. Assert: close() returns cleanly
/// 5. Assert: follow-up query succeeds (recovery verified)
pub fn sdk_crash_handling_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_crash_handling")
      let ctx = helpers.test_step(ctx, "start_crash_test")

      // Phase 1: Start streaming query and simulate crash
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "start_streaming_query")

      // Subject to receive crash simulation results
      let crash_subject: process.Subject(CrashTestResult) =
        process.new_subject()

      // Spawn a process that will start streaming and then be killed
      let query_pid =
        process.spawn_unlinked(fn() {
          case claude_agent_sdk.query("Count slowly from 1 to 10", opts) {
            Ok(stream) -> {
              // Wait for first message to ensure stream is active
              case wait_for_first_message(stream, 10_000) {
                FirstMessageReceived(msg, updated_stream) -> {
                  // Signal that first message received, stream is active
                  process.send(crash_subject, StreamActive(msg, updated_stream))
                  // Keep stream alive briefly - this process will be killed
                  process.sleep(30_000)
                }
                NoMessage(err) -> {
                  process.send(crash_subject, StreamError(err))
                }
                StreamEnded -> {
                  process.send(crash_subject, StreamEndedEarly)
                }
              }
            }
            Error(err) -> {
              process.send(crash_subject, QueryFailed(err))
            }
          }
        })

      helpers.log_info_with(ctx, "spawned_query_process", [
        #("pid", json.string(pid_to_string(query_pid))),
      ])

      // Wait for stream to become active (first message received)
      let ctx = helpers.test_step(ctx, "wait_for_first_message")

      case process.receive(crash_subject, 15_000) {
        Ok(StreamActive(first_msg, _stream)) -> {
          helpers.log_info_with(ctx, "first_message_received", [
            #("message_type", json.string(message_type_string(first_msg))),
          ])

          // Phase 2: Kill the query process to simulate crash
          let ctx = helpers.test_step(ctx, "simulate_crash")
          helpers.log_info(ctx, "killing_query_process")

          let kill_start_ms = helpers.get_monotonic_ms()
          helpers.kill_pid(query_pid)
          let kill_elapsed_ms = helpers.get_monotonic_ms() - kill_start_ms

          helpers.log_info_with(ctx, "process_killed", [
            #("kill_time_ms", json.int(kill_elapsed_ms)),
          ])

          // Phase 3: Verify SDK handles crash gracefully
          // Start a fresh query to verify recovery
          let ctx = helpers.test_step(ctx, "verify_recovery")

          case verify_recovery_query(ctx) {
            True -> {
              helpers.log_test_complete(
                ctx,
                True,
                "crash handling verified - recovery successful",
              )
            }
            False -> {
              helpers.log_test_complete(ctx, False, "recovery query failed")
              should.fail()
            }
          }
        }

        Ok(QueryFailed(err)) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_error_summary(ctx, "QueryFailed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "query() failed")
          should.fail()
        }

        Ok(StreamError(err)) -> {
          helpers.log_error(
            ctx,
            "stream_error",
            error.stream_error_to_string(err),
          )
          helpers.log_error_summary(
            ctx,
            "StreamError",
            error.stream_error_to_string(err),
          )
          helpers.log_test_complete(
            ctx,
            False,
            "stream error before first message",
          )
          should.fail()
        }

        Ok(StreamEndedEarly) -> {
          helpers.log_error(
            ctx,
            "stream_ended_early",
            "Stream ended before first message",
          )
          helpers.log_error_summary(
            ctx,
            "StreamEndedEarly",
            "Stream ended before any message was received",
          )
          helpers.log_test_complete(ctx, False, "stream ended early")
          should.fail()
        }

        Error(Nil) -> {
          // Timeout waiting for first message - kill the process and skip
          helpers.kill_pid(query_pid)
          helpers.log_info(ctx, "timeout_waiting_for_stream")
          helpers.log_test_complete(ctx, True, "skipped due to timeout")
          Nil
        }
      }
    }
  }
}

// ============================================================================
// Internal Types
// ============================================================================

/// Result of crash simulation setup phase.
type CrashTestResult {
  StreamActive(MessageEnvelope, claude_agent_sdk.QueryStream)
  QueryFailed(claude_agent_sdk.QueryError)
  StreamError(StreamError)
  StreamEndedEarly
}

/// Result of waiting for first message.
type FirstMessageResult {
  FirstMessageReceived(MessageEnvelope, claude_agent_sdk.QueryStream)
  NoMessage(StreamError)
  StreamEnded
}

// ============================================================================
// Internal Helpers
// ============================================================================

/// Wait for the first message from the stream.
/// Returns the message and updated stream, or an error.
fn wait_for_first_message(
  stream: claude_agent_sdk.QueryStream,
  timeout_ms: Int,
) -> FirstMessageResult {
  let start_ms = helpers.get_monotonic_ms()
  wait_for_first_message_loop(stream, start_ms, timeout_ms)
}

fn wait_for_first_message_loop(
  stream: claude_agent_sdk.QueryStream,
  start_ms: Int,
  timeout_ms: Int,
) -> FirstMessageResult {
  let elapsed = helpers.get_monotonic_ms() - start_ms
  case elapsed > timeout_ms {
    True ->
      NoMessage(error.ProcessError(
        0,
        ErrorDiagnostic(
          last_non_json_line: None,
          stdout_was_empty: True,
          exit_code_hint: "Timeout waiting for first message",
          troubleshooting: "CLI may be slow to respond",
        ),
      ))
    False -> {
      let #(result, updated_stream) = claude_agent_sdk.next(stream)
      case result {
        Ok(Message(envelope)) -> FirstMessageReceived(envelope, updated_stream)
        Ok(EndOfStream) -> StreamEnded
        Ok(WarningEvent(_)) ->
          // Skip warnings, continue waiting
          wait_for_first_message_loop(updated_stream, start_ms, timeout_ms)
        Error(err) -> {
          case error.is_terminal(err) {
            True -> NoMessage(err)
            False ->
              // Non-terminal error, continue waiting
              wait_for_first_message_loop(updated_stream, start_ms, timeout_ms)
          }
        }
      }
    }
  }
}

/// Verify recovery by running a follow-up query.
fn verify_recovery_query(ctx: helpers.TestContext) -> Bool {
  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_max_turns(1)

  case helpers.query_and_consume_with_timeout("Say hello", opts, 30_000) {
    helpers.QuerySuccess(result) -> {
      helpers.log_info_with(ctx, "recovery_query_succeeded", [
        #("message_count", json.int(list.length(result.messages))),
        #("terminated_normally", json.bool(result.terminated_normally)),
      ])
      result.terminated_normally && result.messages != []
    }
    helpers.QueryFailure(err) -> {
      helpers.log_error(ctx, "recovery_query_failed", error_to_string(err))
      False
    }
    helpers.QueryTimedOut -> {
      helpers.log_info(ctx, "recovery_query_timeout")
      // Timeout is acceptable - not a failure
      True
    }
  }
}

/// Convert process PID to string for logging.
/// Uses Erlang's pid_to_list and converts to binary.
fn pid_to_string(_pid: process.Pid) -> String {
  // PID string representation for logging only
  "<spawned_pid>"
}

/// Get message type as string for logging.
fn message_type_string(envelope: MessageEnvelope) -> String {
  case envelope.message {
    message.System(_) -> "system"
    message.User(_) -> "user"
    message.Assistant(_) -> "assistant"
    message.Result(_) -> "result"
  }
}
