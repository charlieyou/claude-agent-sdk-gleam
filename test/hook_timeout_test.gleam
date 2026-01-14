/// Tests for hook timeout management (casg-red.3).
///
/// Verifies the first-event-wins protocol where either:
/// - Hook completes (HookDone) before timeout → timer cancelled
/// - Timeout fires first (HookTimeout) → task killed, fail-open response
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/string
import gleeunit/should

import claude_agent_sdk/internal/bidir.{
  type SubscriberMessage, HookConfig, Running, StartConfig,
}
import support/mock_bidir_runner

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

/// Receive messages until finding one containing target_id and required text.
fn receive_until_match_contains(
  subject: process.Subject(String),
  target_id: String,
  required: String,
  max_attempts: Int,
) -> Result(String, Nil) {
  case max_attempts <= 0 {
    True -> Error(Nil)
    False -> {
      case process.receive(subject, 500) {
        Ok(msg) -> {
          case
            string.contains(msg, target_id) && string.contains(msg, required)
          {
            True -> Ok(msg)
            False ->
              receive_until_match_contains(
                subject,
                target_id,
                required,
                max_attempts - 1,
              )
          }
        }
        Error(Nil) -> Error(Nil)
      }
    }
  }
}

// =============================================================================
// Hook Timeout Tests
// =============================================================================

/// Test that fast callbacks complete before timeout fires.
/// Verifies timer is cancelled when HookDone wins the race.
pub fn fast_callback_completes_before_timeout_test() {
  let mock = mock_bidir_runner.new()

  // Create hook config with fast handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("fast_hook", fn(_input: Dynamic) -> Dynamic {
          // Fast callback - completes well before any timeout
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use short timeout for test (100ms)
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 100,
      enable_file_checkpointing: False,
      mcp_servers: [],
    )

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send hook_callback
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_fast_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"fast_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Wait for response
  process.sleep(50)
  let assert Ok(response_json) =
    receive_until_match_contains(mock.writes, "cli_fast_1", "continue", 5)

  // Should be a success response (not timeout/fail-open)
  should.be_true(string.contains(response_json, "cli_fast_1"))
  should.be_true(string.contains(response_json, "success"))
  should.be_true(string.contains(response_json, "continue"))

  // Cleanup
  bidir.shutdown(session)
}

/// Test that slow callbacks are killed when timeout fires first.
/// Verifies task is killed and fail-open response is sent.
pub fn slow_callback_times_out_test() {
  let mock = mock_bidir_runner.new()

  // Subject to track callback execution
  let callback_tracker: process.Subject(String) = process.new_subject()

  // Create hook config with slow handler (sleeps longer than timeout)
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("slow_hook", fn(_input: Dynamic) -> Dynamic {
          // Signal that callback started
          process.send(callback_tracker, "started")
          // Sleep for 300ms - longer than 100ms timeout
          process.sleep(300)
          // This should NOT be reached if timeout works correctly
          process.send(callback_tracker, "completed")
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use short timeout for test (100ms)
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 100,
      enable_file_checkpointing: False,
      mcp_servers: [],
    )

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send hook_callback
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_slow_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"slow_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Verify callback started
  let assert Ok("started") = process.receive(callback_tracker, 500)

  // Wait for timeout to fire (100ms) plus some margin
  process.sleep(150)

  // Should have received fail-open response
  let assert Ok(response_json) =
    receive_until_match_contains(mock.writes, "cli_slow_1", "continue", 5)
  should.be_true(string.contains(response_json, "cli_slow_1"))
  should.be_true(string.contains(response_json, "success"))
  should.be_true(string.contains(response_json, "continue"))

  // Verify callback did NOT complete (was killed)
  let completed_check = process.receive(callback_tracker, 300)
  should.be_error(completed_check)

  // Cleanup
  bidir.shutdown(session)
}

/// Test that late HookDone messages are ignored after timeout.
/// When timeout wins, subsequent hook completion should be no-op.
pub fn late_hook_done_ignored_after_timeout_test() {
  let mock = mock_bidir_runner.new()

  // Subject to control callback timing
  let callback_gate: process.Subject(Nil) = process.new_subject()

  // Create hook config with handler that waits for signal
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("gated_hook", fn(_input: Dynamic) -> Dynamic {
          // Wait for explicit signal before completing
          // This simulates a callback that might complete late
          let _ = process.receive(callback_gate, 1000)
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use very short timeout (50ms)
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 50,
      enable_file_checkpointing: False,
      mcp_servers: [],
    )

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)
  process.sleep(50)

  // Send hook_callback
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_gated_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"gated_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Wait for timeout (50ms + margin)
  process.sleep(100)

  // Should have received timeout response
  let assert Ok(response_json) =
    receive_until_match_contains(mock.writes, "cli_gated_1", "continue", 5)
  should.be_true(string.contains(response_json, "cli_gated_1"))

  // Now signal the callback to complete (late)
  process.send(callback_gate, Nil)
  process.sleep(50)

  // Should NOT receive another response (late HookDone ignored)
  let duplicate_check = process.receive(mock.writes, 100)
  should.be_error(duplicate_check)

  // Cleanup
  bidir.shutdown(session)
}
