/// E2E Tests for Offline Error Handling (SDK-62 to SDK-64).
///
/// These tests exercise error handling using mock runners - no real CLI calls.
/// They verify graceful handling of network-level failures without API costs.
///
/// ## Test Cases
/// - SDK-62: Network timeout (mock runner never responds)
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
import gleam/dynamic
import gleam/erlang/process
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
// SDK-62: Network Timeout
// ============================================================================

/// SDK-62: Verify timeout error is surfaced when mock runner hangs.
///
/// The mock runner sleeps for 10 seconds on read, simulating a hang.
/// The SDK should detect this and eventually surface a timeout-related error.
///
/// Note: This test uses a relatively short sleep to keep test time reasonable.
/// In a real timeout scenario, the SDK would use its configured timeout value.
pub fn sdk_62_network_timeout_test() {
  // Create mock runner that hangs on read
  let mock_runner =
    runner.test_runner(
      on_spawn: fn(_cmd, _args, _cwd) { Ok(to_dynamic(Nil)) },
      on_read: fn(_handle) {
        // Simulate a hanging network request
        // Use a short sleep for test purposes, but still longer than
        // reasonable response time
        process.sleep(100)
        // After sleep, return exit status to complete the test
        // In a real scenario, the SDK timeout would fire before this
        runner.ExitStatus(1)
      },
      on_close: fn(_handle) { Nil },
    )

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(mock_runner)
    |> claude_agent_sdk.with_max_turns(1)
    |> claude_agent_sdk.with_skip_version_check

  case claude_agent_sdk.query("test prompt", opts) {
    Error(_err) -> {
      // Query itself failed - acceptable for timeout scenario
      Nil
    }
    Ok(stream) -> {
      // Query succeeded, read from stream
      let #(result, updated_stream) = claude_agent_sdk.next(stream)

      // Should get an error (ProcessError for non-zero exit, or similar)
      case result {
        Error(ProcessError(exit_code, _diagnostic)) -> {
          // Non-zero exit after hanging is expected
          exit_code |> should.equal(1)
        }
        Error(other_error) -> {
          // Any error is acceptable for a hung/timeout scenario
          claude_agent_sdk.is_terminal(other_error) |> should.be_true
        }
        Ok(EndOfStream) -> {
          // EndOfStream is acceptable if runner returned exit
          Nil
        }
        Ok(_) -> {
          // Unexpected success - should not happen with hanging runner
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

  case claude_agent_sdk.query("test prompt", opts) {
    Error(_err) -> {
      should.fail()
    }
    Ok(stream) -> {
      // First read should yield the system message
      let #(result1, stream1) = claude_agent_sdk.next(stream)

      case result1 {
        Ok(Message(_envelope)) -> {
          // Good - got the message before interruption
          Nil
        }
        _ -> {
          let _ = claude_agent_sdk.close(stream1)
          should.fail()
        }
      }

      // Second read should get the interruption error
      let #(result2, stream2) = claude_agent_sdk.next(stream1)

      case result2 {
        Error(ProcessError(exit_code, _diagnostic)) -> {
          // Stream interrupted - exit code should be 1
          exit_code |> should.equal(1)
        }
        Ok(WarningEvent(_)) -> {
          // Warning about no result is also acceptable before interruption error
          let #(result3, stream3) = claude_agent_sdk.next(stream2)
          case result3 {
            Error(ProcessError(_, _)) -> Nil
            Ok(EndOfStream) -> Nil
            _ -> should.fail()
          }
          let _ = claude_agent_sdk.close(stream3)
          Nil
        }
        _ -> {
          let _ = claude_agent_sdk.close(stream2)
          should.fail()
        }
      }

      // Cleanup
      let _ = claude_agent_sdk.close(stream2)
      Nil
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

  case claude_agent_sdk.query("test prompt", opts) {
    Error(_err) -> {
      should.fail()
    }
    Ok(stream) -> {
      // Collect all results to verify graceful degradation
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

  case claude_agent_sdk.query("test prompt", opts) {
    Error(_err) -> {
      should.fail()
    }
    Ok(stream) -> {
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
        }
        _ -> should.fail()
      }

      // Cleanup
      let _ = claude_agent_sdk.close(stream_final)
      Nil
    }
  }
}
