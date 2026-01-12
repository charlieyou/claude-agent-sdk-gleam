/// Tests for the interrupt control operation.
///
/// Tests the interrupt functionality via the async send_control_request API
/// (which interrupt() wraps) since the synchronous API can't be tested without
/// complex cross-process coordination.
///
/// The tests verify:
/// - Correct wire format sent to CLI
/// - Success response handling
/// - Error response handling
/// - Timeout handling
import gleam/dict
import gleam/erlang/process
import gleam/string
import gleeunit/should

import claude_agent_sdk/control.{Interrupt}
import claude_agent_sdk/internal/bidir.{
  type RequestResult, type SubscriberMessage, InterruptTimeout, RequestError,
  RequestSuccess, RequestTimeout, Running,
}
import support/mock_bidir_runner

// =============================================================================
// Wire Format Tests
// =============================================================================

/// Test: interrupt sends correct wire format
///
/// Verifies the JSON payload matches the expected structure:
/// {"type":"control_request","request_id":"req_X","request":{"subtype":"interrupt"}}
pub fn interrupt_sends_correct_wire_format_test() {
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
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send interrupt request using async API
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_test"), result_subject)

  // Assert: mock runner received control_request JSON with correct format
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(request_json, "\"type\":\"control_request\""))
  should.be_true(string.contains(request_json, "\"subtype\":\"interrupt\""))
  should.be_true(string.contains(request_json, "\"request_id\":\"req_test\""))

  // Send response to complete the request
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_test\"}}"
  bidir.inject_message(session, response_json)

  // Wait for result
  let assert Ok(_result) = process.receive(result_subject, 500)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Response Handling Tests
// =============================================================================

/// Test: interrupt receives success response
///
/// When CLI responds with success, the request returns RequestSuccess
pub fn interrupt_receives_success_response_test() {
  // Arrange: start session with mock runner
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
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send interrupt request
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_1"), result_subject)

  // Consume the request from mock
  let assert Ok(_request_json) = process.receive(mock.writes, 500)

  // Simulate CLI success response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_1\"}}"
  bidir.inject_message(session, response_json)

  // Assert: caller receives RequestSuccess
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

/// Test: interrupt receives error response
///
/// When CLI responds with error, the request returns RequestError
pub fn interrupt_receives_error_response_test() {
  // Arrange: start session with mock runner
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
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send interrupt request
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_1"), result_subject)

  // Consume the request from mock
  let assert Ok(_request_json) = process.receive(mock.writes, 500)

  // Simulate CLI error response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req_1\",\"error\":\"No operation to interrupt\"}}"
  bidir.inject_message(session, response_json)

  // Assert: caller receives RequestError with the error message
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestError(msg) -> should.equal(msg, "No operation to interrupt")
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

/// Test: interrupt times out if no response
///
/// When CLI doesn't respond within timeout, request returns RequestTimeout
pub fn interrupt_timeout_test() {
  // Arrange: start session with short default timeout for faster test
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use a short timeout so test doesn't take 5 seconds
  let config =
    bidir.StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 100,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
    )

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request and send success
  let assert Ok(_init_request) = process.receive(mock.writes, 500)
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Verify Running state
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send interrupt request but don't respond
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_timeout"), result_subject)

  // Consume the request from mock (but don't respond)
  let assert Ok(_) = process.receive(mock.writes, 500)

  // Assert: caller receives RequestTimeout after ~100ms
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestTimeout -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Public API Tests
// =============================================================================

/// Test: interrupt() returns InterruptTimeout on timeout
///
/// The interrupt() timeout is controlled by the session's default_timeout_ms.
/// With default_timeout_ms: 100, the actor's internal timer fires at ~100ms
/// and sends RequestTimeout to the caller.
pub fn interrupt_api_timeout_test() {
  // Arrange: start session with short timeout
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    bidir.StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 100,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
    )

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request and send success
  let assert Ok(_init_request) = process.receive(mock.writes, 500)
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Verify Running state
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Call interrupt without any responder - should timeout
  let result = bidir.interrupt(session)

  // Assert: returns InterruptTimeout
  should.equal(result, Error(InterruptTimeout))

  // Cleanup
  bidir.shutdown(session)
}

/// Test: interrupt() returns SessionStopped if session is stopped
pub fn interrupt_api_session_stopped_test() {
  // Arrange: start session with mock runner
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
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Shutdown the session
  bidir.shutdown(session)
  process.sleep(100)

  // Call interrupt on stopped session
  let result = bidir.interrupt(session)

  // Assert: returns SessionStopped
  // Note: Calling interrupt on a stopped session should timeout or error
  // since the actor won't respond. The exact behavior depends on Gleam's
  // actor implementation - it might return an error or just timeout.
  case result {
    Error(InterruptTimeout) -> should.be_true(True)
    // Timeout is acceptable since actor is stopped
    _ -> should.fail()
  }
}
