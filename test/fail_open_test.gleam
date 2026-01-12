/// Tests for fail-open behavior (casg-red.4).
///
/// Verifies that hook timeouts and crashes send continue: true responses
/// to the CLI, ensuring fail-open semantics.
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/string
import gleeunit/should

import claude_agent_sdk/internal/bidir.{
  type SubscriberMessage, HookConfig, Running, StartConfig,
}
import support/mock_bidir_runner

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

/// Receive messages from a subject until finding one containing target_id.
/// Returns the matching message or Error if max_attempts exceeded.
fn receive_until_match(
  subject: Subject(String),
  target_id: String,
  max_attempts: Int,
) -> Result(String, Nil) {
  case max_attempts <= 0 {
    True -> Error(Nil)
    False -> {
      case process.receive(subject, 500) {
        Ok(msg) -> {
          case string.contains(msg, target_id) {
            True -> Ok(msg)
            False -> receive_until_match(subject, target_id, max_attempts - 1)
          }
        }
        Error(Nil) -> Error(Nil)
      }
    }
  }
}

// =============================================================================
// Fail-Open Tests - Crash Scenarios
// =============================================================================

/// Test that crashing callback triggers fail-open response.
/// When a hook callback raises an exception, the session should:
/// 1. Catch the crash
/// 2. Send continue: true response to CLI
/// 3. Continue operating normally
pub fn crashing_callback_returns_continue_true_test() {
  let mock = mock_bidir_runner.new()

  // Subject to verify callback was invoked
  let crash_tracker: process.Subject(String) = process.new_subject()

  // Create hook config with handler that crashes
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("crashing_hook", fn(_input: Dynamic) -> Dynamic {
          // Signal that we're about to crash
          process.send(crash_tracker, "crashing")
          // Raise an exception - this simulates a buggy hook
          panic as "Simulated hook crash"
        }),
      ]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 5000,
      enable_file_checkpointing: False,
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

  // Send hook_callback for crashing hook
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_crash_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"crashing_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Verify callback started (received the crash signal)
  let assert Ok("crashing") = process.receive(crash_tracker, 500)

  // Wait for crash to be handled
  process.sleep(100)

  // Should have received fail-open response (loop until matching request_id)
  let assert Ok(response_json) =
    receive_until_match(mock.writes, "cli_crash_1", 5)
  should.be_true(string.contains(response_json, "success"))
  should.be_true(string.contains(response_json, "continue"))

  // Session should still be Running (fail-open - not crashed)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}

/// Test that session continues normally after crash fail-open.
/// After a hook crashes and fail-open response is sent,
/// subsequent hooks should work normally.
pub fn session_continues_after_crash_fail_open_test() {
  let mock = mock_bidir_runner.new()

  // Subject to track callback execution
  let callback_tracker: process.Subject(String) = process.new_subject()

  // Create hook config with one crashing and one working handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("crashing_hook", fn(_input: Dynamic) -> Dynamic {
          process.send(callback_tracker, "crash_invoked")
          panic as "Simulated crash"
        }),
        #("working_hook", fn(_input: Dynamic) -> Dynamic {
          process.send(callback_tracker, "working_invoked")
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 5000,
      enable_file_checkpointing: False,
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

  // First: send crashing hook
  let crash_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_crash\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"crashing_hook\",\"input\":{}}}"
  bidir.inject_message(session, crash_callback_json)

  // Wait for crash handling
  let assert Ok("crash_invoked") = process.receive(callback_tracker, 500)
  process.sleep(100)

  // Get fail-open response for crash (loop until matching request_id)
  let assert Ok(crash_response) =
    receive_until_match(mock.writes, "cli_crash", 5)
  should.be_true(string.contains(crash_response, "continue"))

  // Second: send working hook - should work normally
  let working_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_working\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"working_hook\",\"input\":{}}}"
  bidir.inject_message(session, working_callback_json)

  // Wait for normal completion
  let assert Ok("working_invoked") = process.receive(callback_tracker, 500)
  process.sleep(50)

  // Get success response for working hook (loop until matching request_id)
  let assert Ok(working_response) =
    receive_until_match(mock.writes, "cli_working", 5)
  should.be_true(string.contains(working_response, "success"))
  should.be_true(string.contains(working_response, "continue"))

  // Session should still be Running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}

/// Test that timeout sends fail-open response with continue: true.
/// (Duplicate coverage with hook_timeout_test, but verifies the fail-open
/// semantics specifically for this issue's acceptance criteria)
pub fn timeout_returns_continue_true_test() {
  let mock = mock_bidir_runner.new()

  // Create hook config with slow handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("slow_hook", fn(_input: Dynamic) -> Dynamic {
          // Sleep longer than timeout
          process.sleep(500)
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use short timeout (100ms)
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 100,
      enable_file_checkpointing: False,
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

  // Send hook_callback for slow hook
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_timeout_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"slow_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Wait for timeout (100ms + margin)
  process.sleep(200)

  // Should have received fail-open response (loop until matching request_id)
  let assert Ok(response_json) =
    receive_until_match(mock.writes, "cli_timeout_1", 5)
  should.be_true(string.contains(response_json, "success"))
  should.be_true(string.contains(response_json, "continue"))

  // Cleanup
  bidir.shutdown(session)
}
