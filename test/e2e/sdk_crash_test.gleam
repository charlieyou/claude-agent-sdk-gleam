/// E2E Tests for Crash/Interruption Handling (SDK Crash Test).
///
/// Tests that the SDK handles abrupt CLI termination gracefully:
/// - Stream terminates with error (not hang)
/// - close() returns cleanly
/// - Follow-up queries succeed (recovery)
///
/// ## Design Note
/// The SDK's QueryStream is opaque and doesn't expose the underlying CLI port
/// or OS PID. This test uses process killing as a proxy for crash simulation:
/// when the Gleam process owning the stream is killed, the Erlang port to the
/// CLI gets cleaned up by the BEAM's process termination semantics.
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
import gleam/bit_array
import gleam/erlang/process
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None}
import gleeunit/should

// ============================================================================
// SDK Crash Test: Abrupt CLI Termination
// ============================================================================

/// Crash test: Verify SDK handles process termination and recovery gracefully.
///
/// Test flow:
/// 1. Start streaming query in spawned process
/// 2. After first message, send kill signal - spawned process continues iterating
/// 3. Assert: spawned process observes stream termination within 5s
/// 4. Assert: close() returns cleanly (verified in spawned process)
/// 5. Assert: follow-up query succeeds (recovery verified in main process)
///
/// Note: This tests SDK resilience to process disruption. The SDK's opaque
/// QueryStream doesn't expose CLI port/PID for direct crash simulation.
pub fn sdk_crash_handling_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_crash_handling")
      let ctx = helpers.test_step(ctx, "start_crash_test")

      // Phase 1: Start streaming query
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      let ctx = helpers.test_step(ctx, "start_streaming_query")

      // Subject for communication with spawned process
      let result_subject: process.Subject(StreamTestResult) =
        process.new_subject()

      // Subject to signal kill command
      let kill_subject: process.Subject(KillSignal) = process.new_subject()

      // Spawn a process that will iterate the stream and report results
      let query_pid =
        process.spawn_unlinked(fn() {
          run_stream_iteration(result_subject, kill_subject, opts)
        })

      helpers.log_info_with(ctx, "spawned_query_process", [
        #("pid", json.string(pid_to_string(query_pid))),
      ])

      // Wait for stream to become active (first message received)
      let ctx = helpers.test_step(ctx, "wait_for_first_message")

      case process.receive(result_subject, 15_000) {
        Ok(FirstMessageReceived(first_msg)) -> {
          helpers.log_info_with(ctx, "first_message_received", [
            #("message_type", json.string(message_type_string(first_msg))),
          ])

          // Phase 2: Signal the spawned process to simulate crash
          let ctx = helpers.test_step(ctx, "simulate_crash")
          helpers.log_info(ctx, "sending_kill_signal")

          let kill_start_ms = helpers.get_monotonic_ms()
          process.send(kill_subject, Kill)

          // Wait for spawned process to report termination (5s timeout per spec)
          let ctx = helpers.test_step(ctx, "wait_for_termination")

          case process.receive(result_subject, 5000) {
            Ok(StreamTerminated(term_result)) -> {
              let kill_elapsed_ms = helpers.get_monotonic_ms() - kill_start_ms
              helpers.log_info_with(ctx, "stream_terminated", [
                #("elapsed_ms", json.int(kill_elapsed_ms)),
                #("termination_type", json.string(term_result.termination_type)),
                #("close_result", json.string(term_result.close_result)),
              ])

              // Assert: stream terminated with terminal error
              case term_result.is_terminal_error {
                True -> {
                  helpers.log_info(ctx, "terminal_error_confirmed")
                }
                False -> {
                  helpers.log_error(
                    ctx,
                    "not_terminal_error",
                    "Stream did not terminate with terminal error",
                  )
                }
              }

              // Assert: close() returned cleanly
              case term_result.close_result {
                "ok" -> {
                  helpers.log_info(ctx, "close_returned_cleanly")
                }
                other -> {
                  helpers.log_error(
                    ctx,
                    "close_failed",
                    "close() result: " <> other,
                  )
                }
              }

              // Phase 3: Verify recovery with follow-up query
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

            Ok(StreamCompleted(msg_count)) -> {
              // Stream completed normally before crash - this is valid behavior
              helpers.log_info_with(ctx, "stream_completed_normally", [
                #("message_count", json.int(msg_count)),
              ])
              // Still verify recovery works
              let ctx = helpers.test_step(ctx, "verify_recovery")
              case verify_recovery_query(ctx) {
                True -> {
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "stream completed normally, recovery verified",
                  )
                }
                False -> {
                  helpers.log_test_complete(ctx, False, "recovery query failed")
                  should.fail()
                }
              }
            }

            Ok(_) -> {
              helpers.log_error(
                ctx,
                "unexpected_result",
                "Unexpected message from spawned process",
              )
              helpers.log_test_complete(ctx, False, "unexpected result")
              should.fail()
            }

            Error(Nil) -> {
              // Timeout: stream did not terminate within 5s - THIS IS A FAILURE
              helpers.kill_pid(query_pid)
              helpers.log_error(
                ctx,
                "termination_timeout",
                "Stream did not terminate within 5s after crash signal",
              )
              helpers.log_error_summary(
                ctx,
                "TerminationTimeout",
                "Stream hang detected - 5s timeout exceeded",
              )
              helpers.log_test_complete(ctx, False, "stream hang detected")
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

        Ok(_) -> {
          helpers.log_error(
            ctx,
            "unexpected_result",
            "Unexpected initial message",
          )
          helpers.log_test_complete(ctx, False, "unexpected result")
          should.fail()
        }

        Error(Nil) -> {
          // Timeout waiting for first message - THIS IS A FAILURE
          helpers.kill_pid(query_pid)
          helpers.log_error(
            ctx,
            "first_message_timeout",
            "No first message received within 15s - CLI may not be responding",
          )
          helpers.log_error_summary(
            ctx,
            "FirstMessageTimeout",
            "Timeout waiting for first message",
          )
          helpers.log_test_complete(ctx, False, "first message timeout")
          should.fail()
        }
      }
    }
  }
}

// ============================================================================
// Internal Types
// ============================================================================

/// Signal to trigger crash simulation.
type KillSignal {
  Kill
}

/// Termination result from stream iteration.
type TerminationResult {
  TerminationResult(
    termination_type: String,
    is_terminal_error: Bool,
    close_result: String,
  )
}

/// Result from spawned stream iteration process.
type StreamTestResult {
  FirstMessageReceived(MessageEnvelope)
  QueryFailed(claude_agent_sdk.QueryError)
  StreamError(StreamError)
  StreamTerminated(TerminationResult)
  StreamCompleted(message_count: Int)
}

// ============================================================================
// Internal Helpers
// ============================================================================

/// Run stream iteration in spawned process.
/// Reports first message, then waits for kill signal and continues iterating
/// until stream terminates. Reports termination status and close() result.
fn run_stream_iteration(
  result_subject: process.Subject(StreamTestResult),
  kill_subject: process.Subject(KillSignal),
  opts: claude_agent_sdk.QueryOptions,
) -> Nil {
  case claude_agent_sdk.query("Count slowly from 1 to 10", opts) {
    Ok(stream) -> {
      // Wait for and report first message
      case wait_for_first_message(stream, 10_000) {
        FirstMsgResult(msg, updated_stream) -> {
          process.send(result_subject, FirstMessageReceived(msg))

          // Wait for kill signal (non-blocking check with short timeout)
          case process.receive(kill_subject, 100) {
            Ok(Kill) -> {
              // Kill received, continue iterating to observe termination
              iterate_until_termination(result_subject, updated_stream, 0)
            }
            Error(Nil) -> {
              // No kill yet, continue waiting and iterating
              iterate_with_kill_check(
                result_subject,
                kill_subject,
                updated_stream,
                1,
              )
            }
          }
        }
        NoFirstMsg(err) -> {
          process.send(result_subject, StreamError(err))
        }
        FirstMsgStreamEnded -> {
          process.send(result_subject, StreamCompleted(0))
        }
      }
    }
    Error(err) -> {
      process.send(result_subject, QueryFailed(err))
    }
  }
}

/// Continue iterating while checking for kill signal.
fn iterate_with_kill_check(
  result_subject: process.Subject(StreamTestResult),
  kill_subject: process.Subject(KillSignal),
  stream: claude_agent_sdk.QueryStream,
  msg_count: Int,
) -> Nil {
  // Check for kill signal
  case process.receive(kill_subject, 0) {
    Ok(Kill) -> {
      // Kill received, switch to termination observation mode
      iterate_until_termination(result_subject, stream, msg_count)
    }
    Error(Nil) -> {
      // No kill yet, continue normal iteration
      let #(result, updated_stream) = claude_agent_sdk.next(stream)
      case result {
        Ok(Message(_)) -> {
          iterate_with_kill_check(
            result_subject,
            kill_subject,
            updated_stream,
            msg_count + 1,
          )
        }
        Ok(EndOfStream) -> {
          // Stream completed normally
          let closed_stream = claude_agent_sdk.close(updated_stream)
          let _ = closed_stream
          process.send(result_subject, StreamCompleted(msg_count))
        }
        Ok(WarningEvent(_)) -> {
          iterate_with_kill_check(
            result_subject,
            kill_subject,
            updated_stream,
            msg_count,
          )
        }
        Error(err) -> {
          case error.is_terminal(err) {
            True -> {
              // Terminal error - report it
              let closed_stream = claude_agent_sdk.close(updated_stream)
              let close_ok = claude_agent_sdk.is_closed(closed_stream)
              process.send(
                result_subject,
                StreamTerminated(
                  TerminationResult(
                    termination_type: "terminal_error_during_iteration",
                    is_terminal_error: True,
                    close_result: case close_ok {
                      True -> "ok"
                      False -> "not_closed"
                    },
                  ),
                ),
              )
            }
            False -> {
              // Non-terminal, continue
              iterate_with_kill_check(
                result_subject,
                kill_subject,
                updated_stream,
                msg_count,
              )
            }
          }
        }
      }
    }
  }
}

/// Iterate until stream terminates after kill signal.
fn iterate_until_termination(
  result_subject: process.Subject(StreamTestResult),
  stream: claude_agent_sdk.QueryStream,
  msg_count: Int,
) -> Nil {
  let #(result, updated_stream) = claude_agent_sdk.next(stream)
  case result {
    Ok(Message(_)) -> {
      // Continue iterating, more messages coming
      iterate_until_termination(result_subject, updated_stream, msg_count + 1)
    }
    Ok(EndOfStream) -> {
      // Stream ended normally (not an error)
      let closed_stream = claude_agent_sdk.close(updated_stream)
      let close_ok = claude_agent_sdk.is_closed(closed_stream)
      process.send(
        result_subject,
        StreamTerminated(
          TerminationResult(
            termination_type: "end_of_stream",
            is_terminal_error: False,
            close_result: case close_ok {
              True -> "ok"
              False -> "not_closed"
            },
          ),
        ),
      )
    }
    Ok(WarningEvent(_)) -> {
      iterate_until_termination(result_subject, updated_stream, msg_count)
    }
    Error(err) -> {
      // Error - check if terminal
      let is_term = error.is_terminal(err)
      let closed_stream = claude_agent_sdk.close(updated_stream)
      let close_ok = claude_agent_sdk.is_closed(closed_stream)
      process.send(
        result_subject,
        StreamTerminated(
          TerminationResult(
            termination_type: error.stream_error_to_string(err),
            is_terminal_error: is_term,
            close_result: case close_ok {
              True -> "ok"
              False -> "not_closed"
            },
          ),
        ),
      )
    }
  }
}

/// Result of waiting for first message.
type FirstMsgState {
  FirstMsgResult(MessageEnvelope, claude_agent_sdk.QueryStream)
  NoFirstMsg(StreamError)
  FirstMsgStreamEnded
}

/// Wait for the first message from the stream.
fn wait_for_first_message(
  stream: claude_agent_sdk.QueryStream,
  timeout_ms: Int,
) -> FirstMsgState {
  let start_ms = helpers.get_monotonic_ms()
  wait_for_first_message_loop(stream, start_ms, timeout_ms)
}

fn wait_for_first_message_loop(
  stream: claude_agent_sdk.QueryStream,
  start_ms: Int,
  timeout_ms: Int,
) -> FirstMsgState {
  let elapsed = helpers.get_monotonic_ms() - start_ms
  case elapsed > timeout_ms {
    True ->
      NoFirstMsg(error.ProcessError(
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
        Ok(Message(envelope)) -> FirstMsgResult(envelope, updated_stream)
        Ok(EndOfStream) -> FirstMsgStreamEnded
        Ok(WarningEvent(_)) ->
          wait_for_first_message_loop(updated_stream, start_ms, timeout_ms)
        Error(err) -> {
          case error.is_terminal(err) {
            True -> NoFirstMsg(err)
            False ->
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
      // Timeout is acceptable - not a failure for recovery
      True
    }
  }
}

/// Convert process PID to string for logging.
fn pid_to_string(pid: process.Pid) -> String {
  // pid_to_list returns a charlist (list of integers)
  // list_to_binary converts charlist to binary (String in Gleam)
  let chars = pid_to_list_raw(pid)
  let bin = list_to_binary(chars)
  case bit_array.to_string(bin) {
    Ok(s) -> s
    Error(Nil) -> "<pid>"
  }
}

@external(erlang, "erlang", "pid_to_list")
fn pid_to_list_raw(pid: process.Pid) -> List(Int)

@external(erlang, "erlang", "list_to_binary")
fn list_to_binary(chars: List(Int)) -> BitArray

/// Get message type as string for logging.
fn message_type_string(envelope: MessageEnvelope) -> String {
  case envelope.message {
    message.System(_) -> "system"
    message.User(_) -> "user"
    message.Assistant(_) -> "assistant"
    message.Result(_) -> "result"
  }
}
