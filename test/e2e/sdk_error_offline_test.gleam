/// E2E Tests for Offline Error Handling (SDK-62 to SDK-64).
///
/// These tests exercise error handling using mock runners - no real CLI calls.
/// They verify graceful handling of network-level failures without API costs.
///
/// ## Test Cases
/// - SDK-62: Delayed exit handling (mock runner delays then exits non-zero)
/// - SDK-62: Timeout handling (mock runner simulates hung CLI via Timeout)
/// - SDK-63: Stream interruption (partial data then abrupt close)
/// - SDK-64: Invalid JSON in stream (malformed JSON handling)
///
/// ## Running Tests
/// ```bash
/// gleam test -- --only sdk_error_offline
/// ```
import claude_agent_sdk
import claude_agent_sdk/error.{
  EndOfStream, JsonDecodeError, Message, ProcessError, TooManyDecodeErrors,
  WarningEvent,
}
import claude_agent_sdk/runner
import e2e/helpers
import gleam/dynamic
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import support/ets_helpers

// ============================================================================
// Helper: Type Coercion FFI
// ============================================================================

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> dynamic.Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn from_dynamic(d: dynamic.Dynamic) -> a

// ============================================================================
// SDK-62: Delayed Exit Handling
// ============================================================================

/// SDK-62: Verify delayed non-zero exit is handled gracefully.
///
/// The mock runner delays briefly before returning a non-zero exit status,
/// simulating a slow process that eventually fails. The SDK should surface
/// this as a ProcessError with the correct exit code.
///
/// Note: This tests delayed exit handling, not timeout enforcement.
/// True timeout testing would require a mock that hangs indefinitely
/// and SDK-level timeout configuration.
pub fn sdk_62_delayed_exit_test() {
  let ctx = helpers.new_test_context("sdk_62_delayed_exit")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  // Create mock runner that delays before returning exit status
  let mock_runner =
    runner.test_runner(
      on_spawn: fn(_cmd, _args, _cwd) { Ok(to_dynamic(Nil)) },
      on_read: fn(_handle) {
        // Simulate a slow process that eventually fails
        process.sleep(100)
        // Return non-zero exit to indicate failure
        runner.ExitStatus(1)
      },
      on_close: fn(_handle) { Nil },
    )

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(mock_runner)
    |> claude_agent_sdk.with_max_turns(1)
    |> claude_agent_sdk.with_skip_version_check

  let ctx = helpers.test_step(ctx, "execute_query")
  case claude_agent_sdk.query("test prompt", opts) {
    Error(_err) -> {
      // Query itself failed - acceptable for delayed exit scenario
      helpers.log_info(ctx, "query_failed_acceptable")
      helpers.log_test_complete(ctx, True, "Query failed as acceptable outcome")
    }
    Ok(stream) -> {
      // Query succeeded, read from stream
      let ctx = helpers.test_step(ctx, "read_stream")
      let #(result, updated_stream) = claude_agent_sdk.next(stream)

      // Should get an error (ProcessError for non-zero exit)
      case result {
        Error(ProcessError(exit_code, _diagnostic)) -> {
          // Non-zero exit after delay is expected
          exit_code |> should.equal(1)
          helpers.log_info_with(ctx, "process_error_received", [
            #("exit_code", json.int(exit_code)),
          ])
          helpers.log_test_complete(
            ctx,
            True,
            "Delayed exit handled with ProcessError",
          )
        }
        Error(other_error) -> {
          // Any terminal error is acceptable
          claude_agent_sdk.is_terminal(other_error) |> should.be_true
          helpers.log_info(ctx, "terminal_error_received")
          helpers.log_test_complete(ctx, True, "Terminal error handled")
        }
        Ok(EndOfStream) -> {
          // EndOfStream is acceptable if runner returned exit
          helpers.log_info(ctx, "end_of_stream_received")
          helpers.log_test_complete(ctx, True, "EndOfStream handled")
        }
        Ok(_) -> {
          // Unexpected success - should not happen with failing runner
          helpers.log_error(ctx, "unexpected_success", "Should not succeed")
          helpers.log_test_complete(ctx, False, "Unexpected success")
          should.fail()
        }
      }

      // Cleanup
      let _ = claude_agent_sdk.close(updated_stream)
      Nil
    }
  }
}

/// SDK-62: Verify timeout during streaming is handled gracefully.
///
/// The mock runner returns Timeout on the first read, simulating a hung CLI
/// that stops producing output. The SDK should handle this as end-of-stream
/// or return an appropriate error, not hang indefinitely.
///
/// This complements sdk_62_delayed_exit_test which tests delayed exit handling.
/// This test exercises the Timeout variant added to runner.ReadResult.
pub fn sdk_62_timeout_test() {
  let ctx = helpers.new_test_context("sdk_62_timeout")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  // Create mock runner that returns Timeout immediately (simulating hung CLI)
  let mock_runner =
    runner.test_runner(
      on_spawn: fn(_cmd, _args, _cwd) { Ok(to_dynamic(Nil)) },
      on_read: fn(_handle) {
        // Simulate CLI hang by returning Timeout
        runner.Timeout
      },
      on_close: fn(_handle) { Nil },
    )

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(mock_runner)
    |> claude_agent_sdk.with_max_turns(1)
    |> claude_agent_sdk.with_skip_version_check

  let ctx = helpers.test_step(ctx, "execute_query")
  case claude_agent_sdk.query("test prompt", opts) {
    Error(_err) -> {
      // Query itself failed - acceptable for timeout scenario
      helpers.log_info(ctx, "query_failed_acceptable")
      helpers.log_test_complete(ctx, True, "Query failed as acceptable outcome")
    }
    Ok(stream) -> {
      // Query succeeded, read from stream - should see timeout handling
      let ctx = helpers.test_step(ctx, "read_stream")
      let #(result, updated_stream) = claude_agent_sdk.next(stream)

      // Timeout should result in EndOfStream or an error, not hang
      case result {
        Ok(EndOfStream) -> {
          // Timeout treated as end of stream - valid behavior
          helpers.log_info(ctx, "timeout_as_end_of_stream")
          helpers.log_test_complete(ctx, True, "Timeout handled as EndOfStream")
        }
        Error(_) -> {
          // Any error is acceptable for timeout scenario
          helpers.log_info(ctx, "timeout_as_error")
          helpers.log_test_complete(ctx, True, "Timeout handled as error")
        }
        Ok(_) -> {
          // Unexpected data after timeout - fail the test
          helpers.log_error(
            ctx,
            "unexpected_data",
            "Received data after timeout",
          )
          helpers.log_test_complete(ctx, False, "Unexpected data after timeout")
          should.fail()
        }
      }

      // Cleanup
      let _ = claude_agent_sdk.close(updated_stream)
      Nil
    }
  }
}

// ============================================================================
// SDK-63: Stream Interruption
// ============================================================================

/// Helper: State key for ETS
const call_count_key = "call_count"

/// SDK-63: Verify stream interruption produces clear error, not crash.
///
/// The mock runner emits one valid message then abruptly closes with
/// a non-zero exit status, simulating a network interruption.
pub fn sdk_63_stream_interruption_test() {
  let ctx = helpers.new_test_context("sdk_63_stream_interruption")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  let table = ets_helpers.new("sdk_63_test_state")

  // Initialize call counter
  ets_helpers.insert(table, to_dynamic(call_count_key), to_dynamic(0))

  // Valid system message to emit first
  let system_json =
    "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"sess_123\"}\n"

  let mock_runner =
    runner.test_runner(
      on_spawn: fn(_cmd, _args, _cwd) { Ok(to_dynamic(table)) },
      on_read: fn(handle) {
        let tbl: ets_helpers.Table = from_dynamic(handle)
        case ets_helpers.lookup(tbl, to_dynamic(call_count_key)) {
          Some(count_dyn) -> {
            let count: Int = from_dynamic(count_dyn)
            ets_helpers.insert(
              tbl,
              to_dynamic(call_count_key),
              to_dynamic(count + 1),
            )
            case count {
              0 -> {
                // First call: return valid system message
                runner.Data(<<system_json:utf8>>)
              }
              _ -> {
                // Second call: abrupt close (simulating interruption)
                runner.ExitStatus(1)
              }
            }
          }
          None -> runner.ReadError("State not found")
        }
      },
      on_close: fn(handle) {
        let tbl: ets_helpers.Table = from_dynamic(handle)
        ets_helpers.delete(tbl, to_dynamic(call_count_key))
        Nil
      },
    )

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(mock_runner)
    |> claude_agent_sdk.with_max_turns(1)
    |> claude_agent_sdk.with_skip_version_check

  let ctx = helpers.test_step(ctx, "execute_query")
  case claude_agent_sdk.query("test prompt", opts) {
    Error(_err) -> {
      helpers.log_error(ctx, "query_failed", "Query should not fail")
      helpers.log_test_complete(ctx, False, "Query failed unexpectedly")
      should.fail()
    }
    Ok(stream) -> {
      // First read should yield the system message
      let ctx = helpers.test_step(ctx, "read_first_message")
      let #(result1, stream1) = claude_agent_sdk.next(stream)

      case result1 {
        Ok(Message(_envelope)) -> {
          // Good - got the message before interruption
          helpers.log_info(ctx, "first_message_received")
        }
        _ -> {
          helpers.log_error(
            ctx,
            "missing_message",
            "Expected system message first",
          )
          helpers.log_test_complete(ctx, False, "Missing first message")
          let _ = claude_agent_sdk.close(stream1)
          should.fail()
        }
      }

      // Second read should get the interruption error
      let ctx = helpers.test_step(ctx, "verify_interruption_error")
      let #(result2, stream2) = claude_agent_sdk.next(stream1)

      case result2 {
        Error(ProcessError(exit_code, _diagnostic)) -> {
          // Stream interrupted - exit code should be 1
          exit_code |> should.equal(1)
          helpers.log_info_with(ctx, "interruption_error_received", [
            #("exit_code", json.int(exit_code)),
          ])
          helpers.log_test_complete(
            ctx,
            True,
            "Stream interruption handled gracefully",
          )
          let _ = claude_agent_sdk.close(stream2)
          Nil
        }
        Ok(WarningEvent(_)) -> {
          // Warning about no result is also acceptable before interruption error
          let #(result3, stream3) = claude_agent_sdk.next(stream2)
          case result3 {
            Error(ProcessError(_, _)) -> {
              helpers.log_info(ctx, "interruption_after_warning")
              helpers.log_test_complete(
                ctx,
                True,
                "Interruption after warning handled",
              )
            }
            Ok(EndOfStream) -> {
              helpers.log_info(ctx, "end_of_stream_after_warning")
              helpers.log_test_complete(
                ctx,
                True,
                "EndOfStream after warning handled",
              )
            }
            _ -> {
              helpers.log_error(
                ctx,
                "unexpected_result",
                "Unexpected result after warning",
              )
              helpers.log_test_complete(ctx, False, "Unexpected result")
              should.fail()
            }
          }
          let _ = claude_agent_sdk.close(stream3)
          Nil
        }
        _ -> {
          helpers.log_error(ctx, "unexpected_result", "Expected interruption")
          helpers.log_test_complete(ctx, False, "Unexpected result")
          let _ = claude_agent_sdk.close(stream2)
          should.fail()
        }
      }
    }
  }
}

// ============================================================================
// SDK-64: Invalid JSON in Stream
// ============================================================================

/// SDK-64: Verify malformed JSON is handled gracefully.
///
/// The mock runner emits a mix of valid JSON, malformed JSON, and more valid JSON.
/// The SDK should:
/// - Process valid lines normally
/// - Log/skip malformed lines (non-terminal error)
/// - Continue processing subsequent valid lines (graceful degradation)
pub fn sdk_64_invalid_json_handling_test() {
  let ctx = helpers.new_test_context("sdk_64_invalid_json")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  let table = ets_helpers.new("sdk_64_test_state")

  // Initialize line index
  ets_helpers.insert(table, to_dynamic("line_idx"), to_dynamic(0))

  // Lines to emit: valid, malformed, malformed, valid, result, exit
  let lines = [
    "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"sess_456\"}\n",
    "{malformed json line\n", "{\"broken\": \n",
    "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}],\"model\":\"claude-3-opus-20240229\",\"stop_reason\":null,\"stop_sequence\":null}}\n",
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}\n",
  ]

  let mock_runner =
    runner.test_runner(
      on_spawn: fn(_cmd, _args, _cwd) { Ok(to_dynamic(table)) },
      on_read: fn(handle) {
        let tbl: ets_helpers.Table = from_dynamic(handle)
        case ets_helpers.lookup(tbl, to_dynamic("line_idx")) {
          Some(idx_dyn) -> {
            let idx: Int = from_dynamic(idx_dyn)
            ets_helpers.insert(tbl, to_dynamic("line_idx"), to_dynamic(idx + 1))
            case list.drop(lines, idx) {
              [line, ..] -> runner.Data(<<line:utf8>>)
              [] -> runner.ExitStatus(0)
            }
          }
          None -> runner.ReadError("State not found")
        }
      },
      on_close: fn(handle) {
        let tbl: ets_helpers.Table = from_dynamic(handle)
        ets_helpers.delete(tbl, to_dynamic("line_idx"))
        Nil
      },
    )

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(mock_runner)
    |> claude_agent_sdk.with_max_turns(1)
    |> claude_agent_sdk.with_skip_version_check

  let ctx = helpers.test_step(ctx, "execute_query")
  case claude_agent_sdk.query("test prompt", opts) {
    Error(_err) -> {
      helpers.log_error(ctx, "query_failed", "Query should not fail")
      helpers.log_test_complete(ctx, False, "Query failed unexpectedly")
      should.fail()
    }
    Ok(stream) -> {
      // Collect all results to verify graceful degradation
      let ctx = helpers.test_step(ctx, "collect_stream_results")
      let #(messages, errors, stream_final) = collect_stream_results(stream)

      // Should have processed valid messages (system, assistant, result = 3)
      { list.length(messages) >= 2 } |> should.be_true

      // Should have encountered JSON decode errors (2 malformed lines)
      { list.length(errors) >= 1 } |> should.be_true

      // Verify the errors are non-terminal JsonDecodeError
      list.each(errors, fn(err) {
        case err {
          JsonDecodeError(_, _) -> Nil
          _ ->
            // TooManyDecodeErrors would also be acceptable if threshold hit
            case err {
              TooManyDecodeErrors(_, _) -> Nil
              _ -> should.fail()
            }
        }
      })

      helpers.log_info_with(ctx, "graceful_degradation_verified", [
        #("messages_received", json.int(list.length(messages))),
        #("errors_received", json.int(list.length(errors))),
      ])
      helpers.log_test_complete(
        ctx,
        True,
        "Malformed JSON handled with graceful degradation",
      )

      // Cleanup
      let _ = claude_agent_sdk.close(stream_final)
      Nil
    }
  }
}

/// Helper: Collect all messages and errors from stream until termination.
fn collect_stream_results(
  stream: claude_agent_sdk.QueryStream,
) -> #(
  List(claude_agent_sdk.MessageEnvelope),
  List(claude_agent_sdk.StreamError),
  claude_agent_sdk.QueryStream,
) {
  collect_stream_loop(stream, [], [])
}

fn collect_stream_loop(
  stream: claude_agent_sdk.QueryStream,
  messages_acc: List(claude_agent_sdk.MessageEnvelope),
  errors_acc: List(claude_agent_sdk.StreamError),
) -> #(
  List(claude_agent_sdk.MessageEnvelope),
  List(claude_agent_sdk.StreamError),
  claude_agent_sdk.QueryStream,
) {
  let #(result, updated_stream) = claude_agent_sdk.next(stream)

  case result {
    Ok(Message(envelope)) ->
      collect_stream_loop(
        updated_stream,
        [envelope, ..messages_acc],
        errors_acc,
      )
    Ok(WarningEvent(_)) ->
      // Skip warnings, continue
      collect_stream_loop(updated_stream, messages_acc, errors_acc)
    Ok(EndOfStream) -> #(
      list.reverse(messages_acc),
      list.reverse(errors_acc),
      updated_stream,
    )
    Error(err) -> {
      case claude_agent_sdk.is_terminal(err) {
        True -> #(
          list.reverse(messages_acc),
          list.reverse([err, ..errors_acc]),
          updated_stream,
        )
        False ->
          // Non-terminal error - record and continue
          collect_stream_loop(updated_stream, messages_acc, [err, ..errors_acc])
      }
    }
  }
}

// ============================================================================
// SDK-64b: Verify valid messages before/after errors are processed
// ============================================================================

/// SDK-64b: Explicitly verify that valid messages are processed even
/// when surrounded by malformed JSON.
pub fn sdk_64b_valid_messages_preserved_test() {
  let ctx = helpers.new_test_context("sdk_64b_valid_preserved")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  let table = ets_helpers.new("sdk_64b_test_state")

  // Initialize line index
  ets_helpers.insert(table, to_dynamic("line_idx"), to_dynamic(0))

  // Sequence: valid -> bad -> valid -> result
  let lines = [
    "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"sess_789\"}\n",
    "not json at all\n",
    "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_02\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"World\"}],\"model\":\"claude-3-opus-20240229\",\"stop_reason\":null,\"stop_sequence\":null}}\n",
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"complete\"}\n",
  ]

  let mock_runner =
    runner.test_runner(
      on_spawn: fn(_cmd, _args, _cwd) { Ok(to_dynamic(table)) },
      on_read: fn(handle) {
        let tbl: ets_helpers.Table = from_dynamic(handle)
        case ets_helpers.lookup(tbl, to_dynamic("line_idx")) {
          Some(idx_dyn) -> {
            let idx: Int = from_dynamic(idx_dyn)
            ets_helpers.insert(tbl, to_dynamic("line_idx"), to_dynamic(idx + 1))
            case list.drop(lines, idx) {
              [line, ..] -> runner.Data(<<line:utf8>>)
              [] -> runner.ExitStatus(0)
            }
          }
          None -> runner.ReadError("State not found")
        }
      },
      on_close: fn(handle) {
        let tbl: ets_helpers.Table = from_dynamic(handle)
        ets_helpers.delete(tbl, to_dynamic("line_idx"))
        Nil
      },
    )

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(mock_runner)
    |> claude_agent_sdk.with_max_turns(1)
    |> claude_agent_sdk.with_skip_version_check

  let ctx = helpers.test_step(ctx, "execute_query")
  case claude_agent_sdk.query("test prompt", opts) {
    Error(_err) -> {
      helpers.log_error(ctx, "query_failed", "Query should not fail")
      helpers.log_test_complete(ctx, False, "Query failed unexpectedly")
      should.fail()
    }
    Ok(stream) -> {
      let ctx = helpers.test_step(ctx, "collect_stream_results")
      let #(messages, errors, stream_final) = collect_stream_results(stream)

      // Should have exactly 3 valid messages (system, assistant, result)
      list.length(messages) |> should.equal(3)

      // Should have exactly 1 JSON decode error
      list.length(errors) |> should.equal(1)

      // First error should be JsonDecodeError
      case list.first(errors) {
        Ok(JsonDecodeError(line, _)) -> {
          // The malformed line should be preserved in the error
          line |> should.equal("not json at all")
          helpers.log_info_with(ctx, "valid_messages_preserved", [
            #("messages_count", json.int(list.length(messages))),
            #("errors_count", json.int(list.length(errors))),
            #("malformed_line", json.string(line)),
          ])
          helpers.log_test_complete(
            ctx,
            True,
            "Valid messages preserved around malformed JSON",
          )
        }
        _ -> {
          helpers.log_error(
            ctx,
            "unexpected_error_type",
            "Expected JsonDecodeError",
          )
          helpers.log_test_complete(ctx, False, "Wrong error type")
          should.fail()
        }
      }

      // Cleanup
      let _ = claude_agent_sdk.close(stream_final)
      Nil
    }
  }
}
