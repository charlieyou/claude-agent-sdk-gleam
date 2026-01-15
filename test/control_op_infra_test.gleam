/// Integration tests for SDK-initiated control operation infrastructure.
///
/// Tests the request/response pattern with timeout management that all
/// control operations use. This test is expected to FAIL until the
/// infrastructure is fully implemented.
///
/// Wire format reference:
/// SDK sends: {"type":"control_request","request_id":"req_1","request":{"subtype":"interrupt"}}
/// CLI responds: {"type":"control_response","response":{"subtype":"success","request_id":"req_1"}}
import gleam/dict
import gleam/erlang/process
import gleam/string
import gleeunit/should

import claude_agent_sdk/control.{Interrupt}
import claude_agent_sdk/internal/bidir
import claude_agent_sdk/internal/bidir/actor.{
  type RequestResult, type SubscriberMessage, InitSent, RequestSuccess,
  RequestTimeout, Running,
}
import support/mock_bidir_runner

// =============================================================================
// Control Operation Request/Response Tests
// =============================================================================

/// Test: control operation sends request and receives response
///
/// This is the core integration test for the control operation infrastructure.
/// It verifies:
/// 1. SDK can send a control request (Interrupt)
/// 2. Request is encoded and sent to CLI via runner
/// 3. Response is correlated and returned to caller
pub fn control_op_sends_request_receives_response_test() {
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

  // Act: send a control request (Interrupt)
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_1"), result_subject)

  // Assert: mock runner received control_request JSON
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(request_json, "control_request"))
  should.be_true(string.contains(request_json, "interrupt"))
  should.be_true(string.contains(request_json, "req_1"))

  // Simulate CLI response
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

/// Test: control request times out if no response
///
/// Verifies the timeout mechanism:
/// 1. Send control request with short timeout
/// 2. Don't send response
/// 3. Caller receives RequestTimeout
pub fn control_request_timeout_test() {
  // Arrange: start session with short timeout
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    actor.StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 100,
      // 100ms timeout
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
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
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Act: send control request but don't respond
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_timeout"), result_subject)

  // Consume the request from mock (but don't respond)
  let assert Ok(_) = process.receive(mock.writes, 500)

  // Assert: caller receives timeout after ~100ms
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestTimeout -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

/// Test: late response after timeout is discarded
///
/// Verifies that responses arriving after timeout don't cause issues:
/// 1. Send control request with short timeout
/// 2. Wait for timeout
/// 3. Send late response
/// 4. Verify no crash and response is ignored
pub fn late_response_after_timeout_discarded_test() {
  // Arrange: start session with short timeout
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    actor.StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 50,
      // 50ms timeout
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
      mcp_servers: [],
    )

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request and send success
  let assert Ok(_init_request) = process.receive(mock.writes, 500)
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Send control request and wait for timeout
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_late"), result_subject)
  let assert Ok(_) = process.receive(mock.writes, 500)

  // Wait for timeout
  let assert Ok(RequestTimeout) = process.receive(result_subject, 200)

  // Send late response - should be discarded without crash
  let late_response =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_late\"}}"
  bidir.inject_message(session, late_response)
  process.sleep(50)

  // Actor should still be alive and responsive
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}

/// Test: control request queued during InitSent
///
/// Verifies that control requests sent before Running state are queued:
/// 1. Send control request while in InitSent
/// 2. Complete initialization
/// 3. Verify request is flushed and response received
pub fn control_request_queued_during_init_test() {
  // Arrange: start session
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request but DON'T respond yet
  let assert Ok(_init_request) = process.receive(mock.writes, 500)

  // Verify we're in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Send control request while still in InitSent - should be queued
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_queued"), result_subject)

  // No new write should appear yet (request is queued)
  case process.receive(mock.writes, 100) {
    Error(Nil) -> should.be_true(True)
    // Expected: no write yet
    Ok(_) -> should.fail()
    // Unexpected: request sent before Running
  }

  // Complete initialization
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)

  // Now the queued request should be flushed
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(request_json, "interrupt"))
  should.be_true(string.contains(request_json, "req_queued"))

  // Send response for the flushed request
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_queued\"}}"
  bidir.inject_message(session, response_json)

  // Verify caller receives the result
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}
