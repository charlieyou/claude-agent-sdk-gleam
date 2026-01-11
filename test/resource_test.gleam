/// Tests for resource safety helpers (with_stream, collect_items, collect_messages, fold_stream).
///
/// These tests verify that each helper guarantees cleanup (close is called)
/// regardless of how the helper returns:
/// - Normal completion
/// - Early return
/// - Full consumption
/// - Early stop via fold
/// - Panic in callback
///
/// Tests use test_runner() with ETS state management to avoid spawning real subprocesses
/// and to verify close() is invoked exactly once.
import claude_agent_sdk/error.{type QueryError, CliNotFoundError}
import claude_agent_sdk/internal/stream.{
  Continue, Message, Stop, close, collect_items, collect_messages, fold_stream,
  is_closed, new_from_runner, with_stream,
}
import claude_agent_sdk/runner.{Data, Eof, ExitStatus, test_runner}
import gleam/dynamic
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import support/ets_helpers

// ============================================================================
// FFI helpers for type casting (TEST ONLY)
// ============================================================================

/// FFI to coerce any value to Dynamic
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> dynamic.Dynamic

/// FFI to unsafely coerce Dynamic to any type (TEST ONLY)
@external(erlang, "gleam_stdlib", "identity")
fn from_dynamic(d: dynamic.Dynamic) -> a

// ============================================================================
// Test state management
// ============================================================================

/// Keys for ETS test state
const lines_key = "lines"

const close_count_key = "close_count"

/// Create a test runner that tracks close() invocations.
/// Uses ETS to store state: lines to emit and close invocation count.
fn create_test_runner_with_close_tracking(
  table: ets_helpers.Table,
  lines: List(String),
) {
  // Initialize state
  ets_helpers.insert(table, to_dynamic(lines_key), to_dynamic(lines))
  ets_helpers.insert(table, to_dynamic(close_count_key), to_dynamic(0))

  test_runner(
    on_spawn: fn(_cmd, _args, _cwd) {
      let ref = ets_helpers.make_ref()
      Ok(ref)
    },
    on_read: fn(_state) {
      case ets_helpers.lookup(table, to_dynamic(lines_key)) {
        Some(lines_dyn) -> {
          let remaining: List(String) = from_dynamic(lines_dyn)
          case remaining {
            [line, ..rest] -> {
              ets_helpers.insert(table, to_dynamic(lines_key), to_dynamic(rest))
              Data(<<line:utf8>>)
            }
            [] -> ExitStatus(0)
          }
        }
        None -> Eof
      }
    },
    on_close: fn(_state) {
      case ets_helpers.lookup(table, to_dynamic(close_count_key)) {
        Some(count_dyn) -> {
          let count: Int = from_dynamic(count_dyn)
          ets_helpers.insert(
            table,
            to_dynamic(close_count_key),
            to_dynamic(count + 1),
          )
        }
        None -> Nil
      }
      Nil
    },
  )
}

/// Get the close count from ETS state.
fn get_close_count(table: ets_helpers.Table) -> Int {
  case ets_helpers.lookup(table, to_dynamic(close_count_key)) {
    Some(count_dyn) -> from_dynamic(count_dyn)
    None -> 0
  }
}

/// Create a test runner that returns non-zero exit status.
fn create_failing_runner(table: ets_helpers.Table, exit_code: Int) {
  ets_helpers.insert(table, to_dynamic(close_count_key), to_dynamic(0))

  test_runner(
    on_spawn: fn(_cmd, _args, _cwd) {
      let ref = ets_helpers.make_ref()
      Ok(ref)
    },
    on_read: fn(_state) { ExitStatus(exit_code) },
    on_close: fn(_state) {
      case ets_helpers.lookup(table, to_dynamic(close_count_key)) {
        Some(count_dyn) -> {
          let count: Int = from_dynamic(count_dyn)
          ets_helpers.insert(
            table,
            to_dynamic(close_count_key),
            to_dynamic(count + 1),
          )
        }
        None -> Nil
      }
      Nil
    },
  )
}

// ============================================================================
// Valid JSON fixtures
// ============================================================================

const valid_system_json = "{\"type\":\"system\"}\n"

const valid_assistant_json = "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"claude-3-opus-20240229\",\"stop_reason\":null,\"stop_sequence\":null}}\n"

const valid_result_json = "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}\n"

// ============================================================================
// with_stream Tests
// ============================================================================

pub fn with_stream_closes_port_on_normal_return_test() {
  let table = ets_helpers.new("with_stream_normal_return")
  let runner =
    create_test_runner_with_close_tracking(table, [
      valid_system_json,
      valid_result_json,
    ])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)
      let query_result: Result(stream.QueryStream, QueryError) = Ok(stream)

      // Track that stream is initially open
      stream |> is_closed |> should.be_false

      // Use with_stream - should close stream after callback returns
      let result =
        with_stream(query_result, fn(s) {
          // Stream should be open inside callback
          s |> is_closed |> should.be_false
          "returned_value"
        })

      // Should get our value back
      case result {
        Ok("returned_value") -> Nil
        _ -> should.fail()
      }

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

pub fn with_stream_closes_port_on_early_return_test() {
  let table = ets_helpers.new("with_stream_early_return")
  let runner =
    create_test_runner_with_close_tracking(table, [
      valid_assistant_json,
      valid_assistant_json,
      valid_result_json,
    ])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)
      let query_result: Result(stream.QueryStream, QueryError) = Ok(stream)

      // Return early after first message (don't consume full stream)
      let result =
        with_stream(query_result, fn(s) {
          // Read just one message
          let #(item, _updated) = stream.next(s)
          case item {
            Ok(Message(_)) -> "got_first_message"
            _ -> "unexpected"
          }
          // Return early without consuming rest
        })

      case result {
        Ok("got_first_message") -> Nil
        _ -> should.fail()
      }

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

pub fn with_stream_passes_query_error_through_test() {
  // Simulate a QueryError (CLI not found)
  let query_result: Result(stream.QueryStream, QueryError) =
    Error(CliNotFoundError("claude not found"))

  // The callback should never be invoked - we verify by checking the error
  // is passed through unmodified
  let result =
    with_stream(query_result, fn(_s) {
      // This should never be called - if it is, we return a different value
      "callback_was_called"
    })

  // Should get the original error back (not Ok("callback_was_called"))
  case result {
    Error(CliNotFoundError("claude not found")) -> Nil
    Ok("callback_was_called") -> should.fail()
    _ -> should.fail()
  }
}

pub fn with_stream_closes_port_on_panic_test() {
  let table = ets_helpers.new("with_stream_panic")
  let runner =
    create_test_runner_with_close_tracking(table, [
      valid_system_json,
      valid_result_json,
    ])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)
      let query_result: Result(stream.QueryStream, QueryError) = Ok(stream)

      // Use rescue to catch the panic, but verify cleanup still happens
      let panic_result =
        ets_helpers.rescue(fn() {
          with_stream(query_result, fn(_s) { panic as "Simulated user panic" })
        })

      // Should have caught the panic
      case panic_result {
        Error(msg) -> {
          // Verify the panic message is propagated
          { msg == "Simulated user panic" } |> should.be_true
        }
        Ok(_) -> should.fail()
      }

      // CRITICAL: Verify close was called exactly once despite the panic
      get_close_count(table) |> should.equal(1)
    }
  }
}

// ============================================================================
// collect_items Tests
// ============================================================================

pub fn collect_items_closes_port_after_full_consumption_test() {
  let table = ets_helpers.new("collect_items_full")
  // Exit with code 0, generates CleanExitNoResult warning
  let runner = create_test_runner_with_close_tracking(table, [])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      // collect_items should fully consume and close the stream
      let result = collect_items(stream)

      // Should have collected at least the warning
      { list.length(result.warnings) >= 1 } |> should.be_true
      result.terminal_error |> should.equal(None)

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

pub fn collect_items_closes_port_on_terminal_error_test() {
  let table = ets_helpers.new("collect_items_error")
  // Exit with non-zero code (terminal error)
  let runner = create_failing_runner(table, 1)

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      let result = collect_items(stream)

      // Should have terminal error recorded
      case result.terminal_error {
        Some(error.ProcessError(1, _)) -> Nil
        _ -> should.fail()
      }

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

pub fn collect_items_handles_multiple_messages_test() {
  let table = ets_helpers.new("collect_items_multi")
  let runner =
    create_test_runner_with_close_tracking(table, [
      valid_assistant_json,
      valid_result_json,
    ])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      let result = collect_items(stream)

      // Should have at least one message item
      { list.length(result.items) >= 1 } |> should.be_true
      result.terminal_error |> should.equal(None)

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

// ============================================================================
// collect_messages Tests
// ============================================================================

pub fn collect_messages_closes_port_and_filters_to_messages_test() {
  let table = ets_helpers.new("collect_messages_filter")
  // Exit with code 0, no messages, generates warning
  let runner = create_test_runner_with_close_tracking(table, [])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      let result = collect_messages(stream)

      // Items should be empty (no actual messages, only warning which is filtered)
      result.items |> list.length |> should.equal(0)
      // But warnings should still be recorded
      { list.length(result.warnings) >= 1 } |> should.be_true

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

pub fn collect_messages_extracts_only_message_envelopes_test() {
  let table = ets_helpers.new("collect_messages_envelopes")
  let runner =
    create_test_runner_with_close_tracking(table, [valid_assistant_json])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      let result = collect_messages(stream)

      // Should have exactly one message envelope
      result.items |> list.length |> should.equal(1)
      // Warnings (like CleanExitNoResult) are separate
      { list.length(result.warnings) >= 1 } |> should.be_true

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

pub fn collect_messages_closes_port_on_terminal_error_test() {
  let table = ets_helpers.new("collect_messages_error")
  // Exit with non-zero code
  let runner = create_failing_runner(table, 1)

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      let result = collect_messages(stream)

      // Should have terminal error recorded
      case result.terminal_error {
        Some(error.ProcessError(1, _)) -> Nil
        _ -> should.fail()
      }

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

// ============================================================================
// fold_stream Tests
// ============================================================================

pub fn fold_stream_closes_port_on_early_stop_test() {
  let table = ets_helpers.new("fold_stream_early_stop")
  // Exit with code 0, generates warning then EndOfStream
  let runner = create_test_runner_with_close_tracking(table, [])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      // Fold that stops immediately on first item
      let #(final_acc, err) =
        fold_stream(stream, 0, fn(acc, _result) {
          // Stop after first item
          Stop(acc + 1)
        })

      final_acc |> should.equal(1)
      err |> should.equal(None)

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

pub fn fold_stream_closes_port_after_full_consumption_test() {
  let table = ets_helpers.new("fold_stream_full")
  // Exit with code 0
  let runner = create_test_runner_with_close_tracking(table, [])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      // Fold that counts all items until EndOfStream
      let #(count, err) =
        fold_stream(stream, 0, fn(acc, _result) { Continue(acc + 1) })

      // Should have counted at least the warning event
      { count >= 1 } |> should.be_true
      err |> should.equal(None)

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

pub fn fold_stream_closes_port_on_terminal_error_test() {
  let table = ets_helpers.new("fold_stream_error")
  // Exit with non-zero code
  let runner = create_failing_runner(table, 1)

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      let #(_acc, err) =
        fold_stream(stream, 0, fn(acc, _result) { Continue(acc + 1) })

      // Should have terminal error
      case err {
        Some(error.ProcessError(1, _)) -> Nil
        _ -> should.fail()
      }

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

pub fn fold_stream_accumulates_correctly_test() {
  let table = ets_helpers.new("fold_stream_accumulate")
  let runner =
    create_test_runner_with_close_tracking(table, [
      valid_assistant_json,
      valid_assistant_json,
    ])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      // Count only Message items
      let #(message_count, _err) =
        fold_stream(stream, 0, fn(acc, result) {
          case result {
            Ok(Message(_)) -> Continue(acc + 1)
            _ -> Continue(acc)
          }
        })

      // Should have counted both messages
      message_count |> should.equal(2)

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

pub fn fold_stream_stops_mid_stream_test() {
  let table = ets_helpers.new("fold_stream_mid")
  let runner =
    create_test_runner_with_close_tracking(table, [
      valid_assistant_json,
      valid_assistant_json,
      valid_assistant_json,
    ])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      // Stop after seeing 2 messages
      let #(count, err) =
        fold_stream(stream, 0, fn(acc, result) {
          case result {
            Ok(Message(_)) -> {
              let new_count = acc + 1
              case new_count >= 2 {
                True -> Stop(new_count)
                False -> Continue(new_count)
              }
            }
            _ -> Continue(acc)
          }
        })

      // Should have stopped at 2 messages
      count |> should.equal(2)
      err |> should.equal(None)

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}

// ============================================================================
// Close Idempotency Tests
// ============================================================================

pub fn close_is_idempotent_test() {
  let table = ets_helpers.new("close_idempotent")
  let runner = create_test_runner_with_close_tracking(table, [])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)

      // First close
      let stream1 = close(stream)
      stream1 |> is_closed |> should.be_true

      // Second close should be no-op
      let stream2 = close(stream1)
      stream2 |> is_closed |> should.be_true

      // Verify close was called only once (second call is a no-op)
      get_close_count(table) |> should.equal(1)
    }
  }
}

// ============================================================================
// Resource Safety Integration Tests
// ============================================================================

pub fn resource_safety_with_stream_after_iterate_then_early_return_test() {
  let table = ets_helpers.new("resource_safety_iterate")
  let runner =
    create_test_runner_with_close_tracking(table, [
      valid_system_json,
      valid_result_json,
    ])

  case runner.spawn(runner, "claude", [], "/tmp") {
    Error(_) -> should.fail()
    Ok(handle) -> {
      let stream = new_from_runner(runner, handle)
      let query_result: Result(stream.QueryStream, QueryError) = Ok(stream)

      let result =
        with_stream(query_result, fn(s) {
          // Iterate once
          let #(item1, _s1) = stream.next(s)
          case item1 {
            Ok(Message(_)) -> {
              // Return early with the updated stream still usable
              // (but with_stream will close it)
              "iterated_and_returned"
            }
            _ -> "unexpected"
          }
        })

      case result {
        Ok("iterated_and_returned") -> Nil
        _ -> should.fail()
      }

      // Verify close was called exactly once
      get_close_count(table) |> should.equal(1)
    }
  }
}
