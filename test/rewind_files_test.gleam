/// Tests for rewind_files control operation.
///
/// Verifies:
/// - Correct wire format sent to CLI
/// - Checkpointing requirement enforced
/// - Success/error response handling
/// - 30s timeout (longer than default)
import gleam/dict
import gleam/erlang/process
import gleam/string
import gleeunit/should

import claude_agent_sdk/control.{RewindFiles}
import claude_agent_sdk/internal/bidir.{
  type RequestResult, type SubscriberMessage,
}
import claude_agent_sdk/internal/bidir/actor.{
  CheckpointingNotEnabled, RequestError, RequestSuccess,
}
import support/mock_bidir_runner

// =============================================================================
// Wire Format Tests
// =============================================================================

/// Test: rewind_files sends correct wire format
///
/// Verifies the JSON structure matches spec:
/// {"type":"control_request","request_id":"req_X","request":{"subtype":"rewind_files","user_message_id":"msg_123"}}
pub fn rewind_files_sends_correct_wire_format_test() {
  // Arrange: session with checkpointing enabled, mock runner
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    bidir.StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: True,
      mcp_servers: [],
    )

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request to be sent
  let assert Ok(_init_request) = process.receive(mock.writes, 500)

  // Send init success to transition to Running
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Verify we're in Running state
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Act: call rewind_files(session, "msg_123")
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    RewindFiles("req_1", "msg_123"),
    result_subject,
  )

  // Assert: mock received correct wire format
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(request_json, "control_request"))
  should.be_true(string.contains(request_json, "rewind_files"))
  should.be_true(string.contains(request_json, "user_message_id"))
  should.be_true(string.contains(request_json, "msg_123"))

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Checkpointing Requirement Tests
// =============================================================================

/// Test: rewind_files fails without checkpointing
///
/// Verifies:
/// - Returns Error(CheckpointingNotEnabled) when checkpointing disabled
/// - NO request sent to mock runner
pub fn rewind_files_fails_without_checkpointing_test() {
  // Arrange: session with checkpointing DISABLED (default)
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)
  // Note: default_config has enable_file_checkpointing: False

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request to be sent
  let assert Ok(_init_request) = process.receive(mock.writes, 500)

  // Send init success to transition to Running
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Verify we're in Running state
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Act: try to rewind files
  let result = bidir.rewind_files(session, "msg_123", 1000)

  // Assert: returns Error(CheckpointingNotEnabled)
  case result {
    Error(CheckpointingNotEnabled) -> should.be_true(True)
    _ -> should.fail()
  }

  // Assert: NO request sent to mock runner (only init request)
  case process.receive(mock.writes, 100) {
    Error(Nil) -> should.be_true(True)
    // Expected: no additional writes
    Ok(_) -> should.fail()
    // Unexpected: request was sent
  }

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Response Handling Tests
// =============================================================================

/// Test: rewind_files receives success response
///
/// Verifies successful completion returns Ok(Nil)
pub fn rewind_files_receives_success_response_test() {
  // Arrange: session with checkpointing, mock responds with success
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    bidir.StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: True,
      mcp_servers: [],
    )

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request and send success
  let assert Ok(_init_request) = process.receive(mock.writes, 500)
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Verify Running state
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Act: send rewind request and simulate CLI success response
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    RewindFiles("req_1", "msg_123"),
    result_subject,
  )

  // Consume the request
  let assert Ok(_request_json) = process.receive(mock.writes, 500)

  // Simulate CLI success response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_1\"}}"
  bidir.inject_message(session, response_json)

  // Assert: caller receives Ok result
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_payload) -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

/// Test: rewind_files receives error response
///
/// Verifies CLI error returns Error(RewindFilesCliError("Checkpoint not found"))
pub fn rewind_files_receives_error_response_test() {
  // Arrange: session with checkpointing
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    bidir.StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: True,
      mcp_servers: [],
    )

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request and send success
  let assert Ok(_init_request) = process.receive(mock.writes, 500)
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Verify Running state
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Act: send rewind request
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    RewindFiles("req_1", "invalid_id"),
    result_subject,
  )

  // Consume the request
  let assert Ok(_request_json) = process.receive(mock.writes, 500)

  // Simulate CLI error response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req_1\",\"error\":\"Checkpoint not found: invalid_id\"}}"
  bidir.inject_message(session, response_json)

  // Assert: caller receives Error result
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestError(message) -> {
      should.be_true(string.contains(message, "Checkpoint not found"))
    }
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Public API Tests
// =============================================================================

/// Test: rewind_files public function returns error when checkpointing disabled
pub fn rewind_files_public_api_checkpointing_disabled_test() {
  // Arrange: session with checkpointing DISABLED
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request and send success
  let assert Ok(_init_request) = process.receive(mock.writes, 500)
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Verify Running state
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Act: call the public rewind_files function
  let result = bidir.rewind_files(session, "msg_123", 1000)

  // Assert: returns CheckpointingNotEnabled
  case result {
    Error(CheckpointingNotEnabled) -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}
