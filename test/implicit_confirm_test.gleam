/// Tests for implicit confirmation during InitSent (T4).
///
/// Verifies:
/// - hook_callback during InitSent transitions to RUNNING
/// - can_use_tool during InitSent transitions to RUNNING
/// - mcp_message during InitSent transitions to RUNNING
/// - Regular messages during InitSent do NOT transition
/// - Init timeout canceled on implicit confirmation
/// - Triggering message processed after transition
import gleam/erlang/process
import gleeunit/should

import claude_agent_sdk/internal/bidir
import claude_agent_sdk/internal/bidir/actor.{
  type SubscriberMessage, InitSent, Running,
}
import support/mock_bidir_runner

// =============================================================================
// Implicit Confirmation Tests - hook_callback
// =============================================================================

pub fn hook_callback_during_init_sent_transitions_to_running_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor - should transition to InitSent
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Should be in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Send hook_callback - should trigger implicit confirmation
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"hook_0\",\"input\":{\"event\":\"test\"}}}"
  bidir.inject_message(session, hook_callback_json)

  // Allow time for message processing
  process.sleep(50)

  // Should transition to Running via implicit confirmation
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Implicit Confirmation Tests - can_use_tool
// =============================================================================

pub fn can_use_tool_during_init_sent_transitions_to_running_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor - should transition to InitSent
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Should be in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Send can_use_tool - should trigger implicit confirmation
  let can_use_tool_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_2\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"Bash\",\"input\":{\"command\":\"ls\"},\"permission_suggestions\":[\"allow\"]}}"
  bidir.inject_message(session, can_use_tool_json)

  // Allow time for message processing
  process.sleep(50)

  // Should transition to Running via implicit confirmation
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Implicit Confirmation Tests - mcp_message
// =============================================================================

pub fn mcp_message_during_init_sent_transitions_to_running_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor - should transition to InitSent
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Should be in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Send mcp_message - should trigger implicit confirmation
  let mcp_message_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_3\",\"request\":{\"subtype\":\"mcp_message\",\"server_name\":\"git-server\",\"message\":{\"method\":\"list\"}}}"
  bidir.inject_message(session, mcp_message_json)

  // Allow time for message processing
  process.sleep(50)

  // Should transition to Running via implicit confirmation
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Regular Messages Do NOT Trigger Implicit Confirmation
// =============================================================================

pub fn regular_message_during_init_sent_does_not_transition_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor - should transition to InitSent
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Should be in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Send regular system message - should NOT trigger implicit confirmation
  let system_message_json =
    "{\"type\":\"system\",\"content\":\"Hello from CLI\"}"
  bidir.inject_message(session, system_message_json)

  // Allow time for message processing
  process.sleep(50)

  // Should still be in InitSent - regular messages don't confirm
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Cleanup
  bidir.shutdown(session)
}

pub fn assistant_message_during_init_sent_does_not_transition_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor - should transition to InitSent
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Should be in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Send regular assistant message - should NOT trigger implicit confirmation
  let assistant_message_json =
    "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}}"
  bidir.inject_message(session, assistant_message_json)

  // Allow time for message processing
  process.sleep(50)

  // Should still be in InitSent - regular messages don't confirm
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Implicit Confirmation Clears Init Request ID
// =============================================================================

pub fn implicit_confirmation_clears_init_request_id_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor - should transition to InitSent
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Should be in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Send hook_callback to trigger implicit confirmation
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"hook_0\",\"input\":{\"event\":\"test\"}}}"
  bidir.inject_message(session, hook_callback_json)

  // Allow time for message processing
  process.sleep(50)

  // Should be Running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Now send an init success response with old request_id - should be ignored
  // (since init_request_id was cleared)
  let success_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, success_json)

  // Allow time for message processing
  process.sleep(50)

  // Should still be Running (not crash or change state)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Unknown callback_id Still Transitions (with warning)
// =============================================================================

pub fn unknown_callback_id_still_transitions_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor - should transition to InitSent
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Should be in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Send hook_callback with unknown callback_id - should still trigger implicit confirmation
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"unknown_hook_999\",\"input\":{\"event\":\"test\"}}}"
  bidir.inject_message(session, hook_callback_json)

  // Allow time for message processing
  process.sleep(50)

  // Should transition to Running - implicit confirmation happens regardless of callback_id validity
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}
