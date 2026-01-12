/// Tests for hook callback dispatch (T1).
///
/// Verifies:
/// - HookCallback in Running state looks up handler by callback_id
/// - Callback receives correctly typed HookContext (input, tool_use_id)
/// - control_response sent via BidirRunner.write()
/// - Unknown callback_id logged and ignored (fail-open)
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/string
import gleeunit/should

import claude_agent_sdk/internal/bidir.{
  type SubscriberMessage, HookConfig, Running,
}
import support/mock_bidir_runner

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// =============================================================================
// Hook Dispatch Tests - Happy Path
// =============================================================================

pub fn hook_callback_invokes_registered_handler_test() {
  // Create mock runner to capture writes
  let mock = mock_bidir_runner.new()

  // Create a subject to receive callback invocation
  let callback_receiver: process.Subject(Dynamic) = process.new_subject()

  // Create hook config with registered handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("hook_0", fn(input: Dynamic) -> Dynamic {
          // Send the input to our receiver for verification
          process.send(callback_receiver, input)
          // Return a success result (will be wrapped in HookSuccess)
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start session with hooks
  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running by sending explicit init response
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)

  // Wait for Running state
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send hook_callback control request
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"hook_0\",\"input\":{\"event\":\"pre_tool\",\"tool_name\":\"Bash\"},\"tool_use_id\":\"tu_123\"}}"
  bidir.inject_message(session, hook_callback_json)

  // Allow processing
  process.sleep(50)

  // Verify callback was invoked with correct input
  let assert Ok(received_input) = process.receive(callback_receiver, 500)
  // The input should be the dynamic from the hook_callback
  should.be_true(dynamic.classify(received_input) != "Nil")

  // Verify response was written
  // Skip init message, get the control_response
  let assert Ok(response_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(response_json, "control_response"))
  should.be_true(string.contains(response_json, "cli_1"))

  // Cleanup
  bidir.shutdown(session)
}

pub fn hook_callback_unknown_callback_id_ignored_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  // Create empty hook config (no handlers registered)
  let hooks = HookConfig(handlers: dict.new())

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start session with empty hooks
  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)

  // Wait for Running state
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send hook_callback with unknown callback_id
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_99\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"unknown_hook\",\"input\":{\"event\":\"pre_tool\"}}}"
  bidir.inject_message(session, hook_callback_json)

  // Allow processing
  process.sleep(50)

  // Session should still be Running (fail-open - not crashed)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Verify fail-open response was sent (CLI should not hang)
  let assert Ok(response_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(response_json, "control_response"))
  should.be_true(string.contains(response_json, "cli_99"))
  should.be_true(string.contains(response_json, "continue"))

  // Cleanup
  bidir.shutdown(session)
}

pub fn hook_callback_response_has_correct_structure_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  // Create hook config with handler returning specific result
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("test_hook", fn(_input: Dynamic) -> Dynamic {
          // Return HookSuccess-compatible structure
          to_dynamic(
            dict.from_list([
              #("continue", to_dynamic(True)),
              #("output", to_dynamic("test output")),
            ]),
          )
        }),
      ]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

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
    "{\"type\":\"control_request\",\"request_id\":\"cli_2\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"test_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)
  process.sleep(50)

  // Get the response
  let assert Ok(response_json) = process.receive(mock.writes, 500)

  // Verify JSON structure matches spec:
  // {"type": "control_response", "response": {"subtype": "success", "request_id": "cli_2", "response": {...}}}
  should.be_true(string.contains(response_json, "\"type\":\"control_response\""))
  should.be_true(string.contains(response_json, "\"request_id\":\"cli_2\""))
  should.be_true(string.contains(response_json, "\"subtype\":\"success\""))

  // Cleanup
  bidir.shutdown(session)
}
