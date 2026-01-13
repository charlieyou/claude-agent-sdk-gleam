/// Integration tests for hook callbacks end-to-end through mock runner.
///
/// Verifies hook callbacks work correctly including:
/// - Context passing (tool_name, tool_input, tool_output)
/// - Response handling
/// - Timeout behavior (fail-open vs fail-deny)
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/string
import gleeunit/should

import claude_agent_sdk/internal/bidir.{
  type SubscriberMessage, HookConfig, Running, StartConfig,
}
import support/full_mock_runner

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

/// Receive messages from a subject until finding one containing target_id.
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
// Test: PreToolUse Hook Receives Context
// =============================================================================

/// Test that PreToolUse hook callback is invoked with context.
/// Verifies: hook receives input as Dynamic (context is passed).
pub fn pre_tool_use_receives_context_test() {
  // Subject to capture hook invocation - sends True if all expected fields present
  let hook_called: Subject(Bool) = process.new_subject()

  // Mock runner with PreToolUse hook simulation
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()
    |> full_mock_runner.with_hook_simulation(
      "PreToolUse",
      json.object([
        #("tool_name", json.string("bash")),
        #("tool_input", json.object([#("command", json.string("ls -la"))])),
        #("session_id", json.string("test-session")),
      ]),
    )

  let adapter = full_mock_runner.start(mock)

  // Hook handler verifies expected context fields are present
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("PreToolUse", fn(input: Dynamic) -> Dynamic {
          // Decode and verify expected fields: tool_name, tool_input, session_id
          let decoder = {
            use tool_name <- decode.field("tool_name", decode.string)
            use _tool_input <- decode.field("tool_input", decode.dynamic)
            use session_id <- decode.field("session_id", decode.string)
            decode.success(#(tool_name, session_id))
          }
          let result = decode.run(input, decoder)
          let valid = case result {
            Ok(#("bash", "test-session")) -> True
            _ -> False
          }
          process.send(hook_called, valid)
          to_dynamic(dict.from_list([#("continue", True)]))
        }),
      ]),
      permission_handlers: dict.new(),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) =
    bidir.start_with_hooks(adapter.bidir_runner, config, hooks)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Wait for and process init
  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Trigger the PreToolUse hook simulation
  let _adapter = full_mock_runner.trigger_hook_simulation(adapter)
  process.sleep(100)

  // Verify hook was called with non-nil context
  let assert Ok(received_context) = process.receive(hook_called, 500)
  should.be_true(received_context)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Test: PostToolUse Hook Receives Output
// =============================================================================

/// Test that PostToolUse hook callback is invoked with context.
/// Verifies: hook receives input as Dynamic (context with output is passed).
pub fn post_tool_use_receives_output_test() {
  // Subject to capture hook invocation - sends True if all expected fields present
  let hook_called: Subject(Bool) = process.new_subject()

  // Mock runner with PostToolUse hook simulation including tool_result
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()
    |> full_mock_runner.with_hook_simulation(
      "PostToolUse",
      json.object([
        #("tool_name", json.string("bash")),
        #("tool_input", json.object([#("command", json.string("echo hello"))])),
        #("tool_result", json.string("hello\n")),
        #("session_id", json.string("test-session")),
      ]),
    )

  let adapter = full_mock_runner.start(mock)

  // Hook handler verifies expected context fields are present
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("PostToolUse", fn(input: Dynamic) -> Dynamic {
          // Decode and verify expected fields: tool_name, tool_input, tool_result, session_id
          let decoder = {
            use tool_name <- decode.field("tool_name", decode.string)
            use _tool_input <- decode.field("tool_input", decode.dynamic)
            use tool_result <- decode.field("tool_result", decode.string)
            use session_id <- decode.field("session_id", decode.string)
            decode.success(#(tool_name, tool_result, session_id))
          }
          let result = decode.run(input, decoder)
          let valid = case result {
            Ok(#("bash", "hello\n", "test-session")) -> True
            _ -> False
          }
          process.send(hook_called, valid)
          to_dynamic(dict.from_list([#("continue", True)]))
        }),
      ]),
      permission_handlers: dict.new(),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) =
    bidir.start_with_hooks(adapter.bidir_runner, config, hooks)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Wait for and process init
  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Trigger the PostToolUse hook simulation
  let _adapter = full_mock_runner.trigger_hook_simulation(adapter)
  process.sleep(100)

  // Verify hook was called with non-nil context
  let assert Ok(received_context) = process.receive(hook_called, 500)
  should.be_true(received_context)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Test: can_use_tool Permission
// =============================================================================

/// Test that can_use_tool permission callback is invoked with correct context.
/// Verifies: permission handler is called and can return allow/deny.
/// Note: The current implementation passes tool_name and permission_suggestions
/// to permission handlers (bidir.gleam:1421-1426), not the tool input.
pub fn can_use_tool_permission_test() {
  // Subject to capture permission invocation - sends True if expected fields present
  let permission_called: Subject(Bool) = process.new_subject()

  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)

  // Permission handler verifies expected context fields
  let hooks =
    HookConfig(
      handlers: dict.new(),
      permission_handlers: dict.from_list([
        #("test_tool", fn(input: Dynamic) -> Dynamic {
          // Decode and verify expected fields: tool_name, permission_suggestions
          let decoder = {
            use tool_name <- decode.field("tool_name", decode.string)
            use _suggestions <- decode.field(
              "permission_suggestions",
              decode.list(decode.dynamic),
            )
            decode.success(tool_name)
          }
          let result = decode.run(input, decoder)
          let valid = case result {
            Ok("test_tool") -> True
            _ -> False
          }
          process.send(permission_called, valid)
          to_dynamic(dict.from_list([#("behavior", "allow")]))
        }),
      ]),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) =
    bidir.start_with_hooks(adapter.bidir_runner, config, hooks)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Wait for and process init
  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Inject can_use_tool request
  let permission_json =
    "{\"type\":\"control_request\",\"request_id\":\"perm_1\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"test_tool\",\"input\":{\"arg\":\"value\"},\"permission_suggestions\":[]}}"
  bidir.inject_message(session, permission_json)
  process.sleep(100)

  // Verify permission handler was invoked with context
  let assert Ok(received_context) = process.receive(permission_called, 500)
  should.be_true(received_context)

  // Verify response was sent back with "allow"
  let assert Ok(response) =
    receive_until_match(adapter.captured_writes, "perm_1", 5)
  should.be_true(string.contains(response, "allow"))
  should.be_true(string.contains(response, "success"))

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Test: Hook Timeout Fails Open
// =============================================================================

/// Test that hook callback timeout fails open (continue: true).
/// Verifies: slow hooks return continue: true when they time out.
pub fn hook_timeout_fails_open_test() {
  // Track if callback started
  let callback_started: Subject(Bool) = process.new_subject()

  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()
    |> full_mock_runner.with_hook_simulation(
      "SlowHook",
      json.object([#("session_id", json.string("test"))]),
    )

  let adapter = full_mock_runner.start(mock)

  // Hook handler that sleeps longer than timeout
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("SlowHook", fn(_input: Dynamic) -> Dynamic {
          process.send(callback_started, True)
          // Sleep 300ms - longer than 100ms timeout
          process.sleep(300)
          to_dynamic(dict.from_list([#("continue", True)]))
        }),
      ]),
      permission_handlers: dict.new(),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  // Configure short timeout (100ms)
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 100,
      enable_file_checkpointing: False,
      mcp_servers: [],
    )

  let assert Ok(session) =
    bidir.start_with_hooks(adapter.bidir_runner, config, hooks)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Wait for and process init
  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Trigger the slow hook
  let _adapter = full_mock_runner.trigger_hook_simulation(adapter)

  // Verify hook started
  let assert Ok(True) = process.receive(callback_started, 500)

  // Wait for timeout (100ms + margin)
  process.sleep(200)

  // Should receive fail-open response with continue: true
  let assert Ok(response) =
    receive_until_match(adapter.captured_writes, "cli_hook_", 5)
  should.be_true(string.contains(response, "continue"))
  should.be_true(string.contains(response, "success"))
  // Should NOT contain "deny"
  should.be_false(string.contains(response, "deny"))

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Test: Permission Timeout Fails Deny
// =============================================================================

/// Test that permission callback timeout fails deny (behavior: "deny").
/// Verifies: slow permission handlers return behavior: "deny" when they time out.
pub fn permission_timeout_fails_deny_test() {
  // Track if callback started
  let callback_started: Subject(Bool) = process.new_subject()

  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)

  // Permission handler that sleeps longer than timeout
  let hooks =
    HookConfig(
      handlers: dict.new(),
      permission_handlers: dict.from_list([
        #("slow_permission_tool", fn(_input: Dynamic) -> Dynamic {
          process.send(callback_started, True)
          // Sleep 300ms - longer than 100ms timeout
          process.sleep(300)
          to_dynamic(dict.from_list([#("behavior", "allow")]))
        }),
      ]),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  // Configure short timeout (100ms)
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 100,
      enable_file_checkpointing: False,
      mcp_servers: [],
    )

  let assert Ok(session) =
    bidir.start_with_hooks(adapter.bidir_runner, config, hooks)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Wait for and process init
  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Inject can_use_tool request for slow permission handler
  let permission_json =
    "{\"type\":\"control_request\",\"request_id\":\"perm_slow_1\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"slow_permission_tool\",\"input\":{},\"permission_suggestions\":[]}}"
  bidir.inject_message(session, permission_json)

  // Verify callback started
  let assert Ok(True) = process.receive(callback_started, 500)

  // Wait for timeout (100ms + margin)
  process.sleep(200)

  // Should receive fail-deny response with behavior: "deny"
  let assert Ok(response) =
    receive_until_match(adapter.captured_writes, "perm_slow_1", 5)
  should.be_true(string.contains(response, "deny"))
  should.be_true(string.contains(response, "success"))
  // Should NOT contain "allow" or "continue"
  should.be_false(string.contains(response, "allow"))

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Test: Hook Error Fails Open
// =============================================================================

/// Test that hook callback errors fail open (continue: true).
/// Verifies: crashing hooks return continue: true.
pub fn hook_error_fails_open_test() {
  let crash_tracker: Subject(Bool) = process.new_subject()

  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()
    |> full_mock_runner.with_hook_simulation(
      "CrashingHook",
      json.object([#("session_id", json.string("test"))]),
    )

  let adapter = full_mock_runner.start(mock)

  // Hook handler that crashes
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("CrashingHook", fn(_input: Dynamic) -> Dynamic {
          process.send(crash_tracker, True)
          panic as "Simulated hook crash"
        }),
      ]),
      permission_handlers: dict.new(),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) =
    bidir.start_with_hooks(adapter.bidir_runner, config, hooks)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Wait for and process init
  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Trigger the crashing hook
  let _adapter = full_mock_runner.trigger_hook_simulation(adapter)

  // Verify crash happened
  let assert Ok(True) = process.receive(crash_tracker, 500)
  process.sleep(100)

  // Should receive fail-open response
  let assert Ok(response) =
    receive_until_match(adapter.captured_writes, "cli_hook_", 5)
  should.be_true(string.contains(response, "continue"))
  should.be_true(string.contains(response, "success"))
  should.be_false(string.contains(response, "deny"))

  // Session should still be running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Test: Permission Error Fails Deny
// =============================================================================

/// Test that permission callback errors fail deny (behavior: "deny").
/// Verifies: crashing permission handlers return behavior: "deny".
pub fn permission_error_fails_deny_test() {
  let crash_tracker: Subject(Bool) = process.new_subject()

  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)

  // Permission handler that crashes
  let hooks =
    HookConfig(
      handlers: dict.new(),
      permission_handlers: dict.from_list([
        #("crashing_tool", fn(_input: Dynamic) -> Dynamic {
          process.send(crash_tracker, True)
          panic as "Simulated permission crash"
        }),
      ]),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) =
    bidir.start_with_hooks(adapter.bidir_runner, config, hooks)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Wait for and process init
  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Inject can_use_tool request for crashing handler
  let permission_json =
    "{\"type\":\"control_request\",\"request_id\":\"perm_crash_1\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"crashing_tool\",\"input\":{},\"permission_suggestions\":[]}}"
  bidir.inject_message(session, permission_json)

  // Verify crash happened
  let assert Ok(True) = process.receive(crash_tracker, 500)
  process.sleep(100)

  // Should receive fail-deny response
  let assert Ok(response) =
    receive_until_match(adapter.captured_writes, "perm_crash_1", 5)
  should.be_true(string.contains(response, "deny"))
  should.be_true(string.contains(response, "success"))
  should.be_false(string.contains(response, "allow"))

  // Session should still be running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}
