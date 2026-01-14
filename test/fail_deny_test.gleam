/// Tests for fail-deny behavior (casg-red.5).
///
/// Verifies that permission callback timeouts and crashes send behavior: "deny"
/// responses to the CLI, ensuring fail-deny semantics for security.
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/string
import gleeunit/should

import claude_agent_sdk/internal/bidir
import claude_agent_sdk/internal/bidir/actor.{
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

/// Receive messages until finding one containing target_id and required text.
fn receive_until_match_contains(
  subject: Subject(String),
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
// Fail-Deny Tests - Permission Timeout
// =============================================================================

/// Test that permission callback timeout returns behavior: "deny".
/// When a permission callback times out, the session should:
/// 1. Kill the task
/// 2. Send behavior: "deny" response to CLI
/// 3. Log security warning with SECURITY: prefix
pub fn permission_timeout_returns_deny_test() {
  let mock = mock_bidir_runner.new()

  // Create hook config with slow permission handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([]),
      permission_handlers: dict.from_list([
        #("slow_tool", fn(_input: Dynamic) -> Dynamic {
          // Sleep longer than timeout
          process.sleep(500)
          to_dynamic(dict.from_list([#("behavior", "allow")]))
        }),
      ]),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  // Use short timeout (100ms)
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

  // Send can_use_tool request
  let permission_json =
    "{\"type\":\"control_request\",\"request_id\":\"perm_timeout_1\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"slow_tool\",\"input\":{},\"permission_suggestions\":[]}}"
  bidir.inject_message(session, permission_json)

  // Wait for timeout (100ms + margin)
  process.sleep(200)

  // Should have received deny response
  let assert Ok(response_json) =
    receive_until_match(mock.writes, "perm_timeout_1", 5)
  should.be_true(string.contains(response_json, "success"))
  should.be_true(string.contains(response_json, "deny"))

  // Session should still be Running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Fail-Deny Tests - Permission Crash
// =============================================================================

/// Test that crashing permission callback returns behavior: "deny".
/// When a permission callback crashes, the session should:
/// 1. Catch the crash
/// 2. Send behavior: "deny" response to CLI
/// 3. Log security warning with SECURITY: prefix
pub fn permission_crash_returns_deny_test() {
  let mock = mock_bidir_runner.new()

  // Subject to verify callback was invoked
  let crash_tracker: Subject(String) = process.new_subject()

  // Create hook config with crashing permission handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([]),
      permission_handlers: dict.from_list([
        #("crashing_tool", fn(_input: Dynamic) -> Dynamic {
          // Signal that we're about to crash
          process.send(crash_tracker, "crashing")
          // Raise an exception
          panic as "Simulated permission crash"
        }),
      ]),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 5000,
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

  // Send can_use_tool request for crashing handler
  let permission_json =
    "{\"type\":\"control_request\",\"request_id\":\"perm_crash_1\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"crashing_tool\",\"input\":{},\"permission_suggestions\":[]}}"
  bidir.inject_message(session, permission_json)

  // Verify callback started (received the crash signal)
  let assert Ok("crashing") = process.receive(crash_tracker, 500)

  // Wait for crash to be handled
  process.sleep(100)

  // Should have received deny response
  let assert Ok(response_json) =
    receive_until_match(mock.writes, "perm_crash_1", 5)
  should.be_true(string.contains(response_json, "success"))
  should.be_true(string.contains(response_json, "deny"))

  // Session should still be Running (fail-deny doesn't crash the session)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Regression Tests - Hook Still Fail-Open
// =============================================================================

/// Test that hook timeout still returns continue: true (regression test).
/// Ensure the fail-deny implementation doesn't break hook fail-open behavior.
pub fn hook_timeout_still_returns_continue_test() {
  let mock = mock_bidir_runner.new()

  // Create hook config with slow hook handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("slow_hook", fn(_input: Dynamic) -> Dynamic {
          // Sleep longer than timeout
          process.sleep(500)
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  // Use short timeout (100ms)
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

  // Send hook_callback for slow hook
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"hook_timeout_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"slow_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Wait for timeout (100ms + margin)
  process.sleep(200)

  // Should have received fail-open response (continue: true, NOT deny)
  let assert Ok(response_json) =
    receive_until_match_contains(mock.writes, "hook_timeout_1", "continue", 5)
  should.be_true(string.contains(response_json, "success"))
  should.be_true(string.contains(response_json, "continue"))
  // Should NOT contain "deny"
  should.be_false(string.contains(response_json, "deny"))

  // Cleanup
  bidir.shutdown(session)
}

/// Test that hook crash still returns continue: true (regression test).
/// Ensure the fail-deny implementation doesn't break hook fail-open behavior.
pub fn hook_crash_still_returns_continue_test() {
  let mock = mock_bidir_runner.new()

  // Subject to verify callback was invoked
  let crash_tracker: Subject(String) = process.new_subject()

  // Create hook config with crashing hook handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("crashing_hook", fn(_input: Dynamic) -> Dynamic {
          process.send(crash_tracker, "crashing")
          panic as "Simulated hook crash"
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 5000,
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

  // Send hook_callback for crashing hook
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"hook_crash_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"crashing_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Verify callback started
  let assert Ok("crashing") = process.receive(crash_tracker, 500)

  // Wait for crash to be handled
  process.sleep(100)

  // Should have received fail-open response (continue: true, NOT deny)
  let assert Ok(response_json) =
    receive_until_match_contains(mock.writes, "hook_crash_1", "continue", 5)
  should.be_true(string.contains(response_json, "success"))
  should.be_true(string.contains(response_json, "continue"))
  // Should NOT contain "deny"
  should.be_false(string.contains(response_json, "deny"))

  // Cleanup
  bidir.shutdown(session)
}
