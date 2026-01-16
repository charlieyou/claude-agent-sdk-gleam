/// Tests for set_model control operation.
///
/// Verifies:
/// - Wire format is correct
/// - Success responses are handled
/// - Error responses are handled
/// - Various model string formats work
/// - Synchronous API works correctly
import gleam/dict
import gleam/erlang/process
import gleam/option.{None}
import gleam/string
import gleeunit/should

import claude_agent_sdk/control.{SetModel}
import claude_agent_sdk/internal/bidir.{
  type RequestResult, type SubscriberMessage,
}
import claude_agent_sdk/internal/bidir/actor.{
  RequestError, RequestSuccess, RequestTimeout, SetModelSessionStopped,
  StartConfig,
}
import support/mock_bidir_runner

// =============================================================================
// Wire Format Tests
// =============================================================================

/// Test: set_model sends correct wire format with short model name
pub fn set_model_sends_correct_wire_format_test() {
  // Arrange: start session with mock runner
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

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

  // Act: send set_model request
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    SetModel("req_1", "sonnet"),
    result_subject,
  )

  // Assert: mock runner received control_request JSON with correct format
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(request_json, "\"type\":\"control_request\""))
  should.be_true(string.contains(request_json, "\"subtype\":\"set_model\""))
  should.be_true(string.contains(request_json, "\"model\":\"sonnet\""))
  should.be_true(string.contains(request_json, "\"request_id\":\"req_1\""))

  // Send response to complete the request
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_1\"}}"
  bidir.inject_message(session, response_json)
  let assert Ok(_) = process.receive(result_subject, 500)

  // Cleanup
  bidir.shutdown(session)
}

/// Test: set_model with full model ID
pub fn set_model_with_full_model_id_test() {
  // Arrange: start session with mock runner
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init and complete handshake
  let assert Ok(_init_request) = process.receive(mock.writes, 500)
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Act: send set_model with full model ID
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  let full_model_id = "claude-3-5-sonnet-20241022"
  bidir.send_control_request(
    session,
    SetModel("req_1", full_model_id),
    result_subject,
  )

  // Assert: model field contains full ID
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(
    request_json,
    "\"model\":\"claude-3-5-sonnet-20241022\"",
  ))

  // Send response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_1\"}}"
  bidir.inject_message(session, response_json)
  let assert Ok(_) = process.receive(result_subject, 500)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Response Handling Tests
// =============================================================================

/// Test: set_model receives success response
pub fn set_model_receives_success_response_test() {
  // Arrange: start session with mock runner
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Complete init handshake
  let assert Ok(_init_request) = process.receive(mock.writes, 500)
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Act: call set_model
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, SetModel("req_1", "opus"), result_subject)

  // Consume request
  let assert Ok(_) = process.receive(mock.writes, 500)

  // Arrange: mock responds with success
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_1\"}}"
  bidir.inject_message(session, response_json)

  // Assert: returns Ok (RequestSuccess)
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_payload) -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

/// Test: set_model receives error response for invalid model
pub fn set_model_receives_error_for_invalid_model_test() {
  // Arrange: start session with mock runner
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Complete init handshake
  let assert Ok(_init_request) = process.receive(mock.writes, 500)
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Act: call set_model with invalid model
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    SetModel("req_1", "invalid-model"),
    result_subject,
  )

  // Consume request
  let assert Ok(_) = process.receive(mock.writes, 500)

  // Arrange: mock responds with error
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req_1\",\"error\":\"Invalid model: invalid-model\"}}"
  bidir.inject_message(session, response_json)

  // Assert: returns Error (RequestError with message)
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestError(message) -> {
      should.be_true(string.contains(message, "Invalid model"))
    }
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Timeout Tests
// =============================================================================

/// Test: set_model times out after configured timeout
pub fn set_model_timeout_test() {
  // Arrange: start session with short timeout
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 100,
      // 100ms timeout for testing
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
      mcp_servers: [],
      on_warning: None,
    )

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Complete init handshake
  let assert Ok(_init_request) = process.receive(mock.writes, 500)
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Act: send set_model but don't respond
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    SetModel("req_1", "sonnet"),
    result_subject,
  )

  // Consume request but don't respond
  let assert Ok(_) = process.receive(mock.writes, 500)

  // Assert: caller receives timeout
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestTimeout -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Synchronous API Tests
// =============================================================================

/// Test: synchronous set_model returns session stopped error when session is stopped
///
/// This verifies the synchronous API wrapper correctly handles stopped sessions.
/// When the session is stopped, the actor responds with RequestSessionStopped,
/// which set_model maps to SetModelSessionStopped.
pub fn set_model_sync_returns_session_stopped_test() {
  // Arrange: start session
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Complete init handshake
  let assert Ok(_init_request) = process.receive(mock.writes, 500)
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Verify we're in Running state
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Shutdown the session
  bidir.shutdown(session)
  process.sleep(100)

  // Act: call synchronous set_model on stopped session
  let result = bidir.set_model(session, "sonnet")

  // Assert: returns SetModelSessionStopped (actor responds with session stopped)
  should.equal(result, Error(SetModelSessionStopped))
}
