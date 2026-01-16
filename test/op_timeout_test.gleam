/// Tests for operation timeout and error handling edge cases.
///
/// Covers the "first-event-wins" pattern where exactly one of these resolves a request:
/// 1. Response arrives -> success/error result
/// 2. Timer fires -> Timeout error
/// 3. Session stops -> SessionStopped error
///
/// Most infrastructure tests are in control_op_infra_test.gleam. This file covers
/// edge cases specific to T6 acceptance criteria.
import gleam/dict
import gleam/erlang/process
import gleam/option.{None}
import gleeunit/should

import claude_agent_sdk/control.{Interrupt}
import claude_agent_sdk/internal/bidir.{
  type RequestResult, type SubscriberMessage,
}
import claude_agent_sdk/internal/bidir/actor.{
  RequestSessionStopped, RequestSuccess, StartConfig,
}
import support/mock_bidir_runner

// =============================================================================
// Session Stop Cancels Pending Requests Test
// =============================================================================

/// Test: session stop cancels pending requests and returns SessionStopped
///
/// Verifies cleanup_session behavior:
/// 1. Start session with pending control request
/// 2. Call shutdown() before response arrives
/// 3. Pending request receives RequestSessionStopped
/// 4. No crash, clean shutdown
pub fn session_stop_cancels_pending_requests_test() {
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

  // Verify we're in Running state
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Send a control request that we won't respond to
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_pending"), result_subject)

  // Consume the request from mock (but don't respond)
  let assert Ok(_request_json) = process.receive(mock.writes, 500)

  // Act: shutdown the session while request is pending
  bidir.shutdown(session)

  // Assert: pending request receives SessionStopped
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSessionStopped -> should.be_true(True)
    _ -> should.fail()
  }
}

// =============================================================================
// Response Before Timeout Cancels Timer Test
// =============================================================================

/// Test: response before timeout cancels timer (timer message doesn't crash)
///
/// Verifies first-event-wins pattern:
/// 1. Send control request with timeout
/// 2. Response arrives quickly
/// 3. Timer fires later (stale timeout)
/// 4. No crash, actor continues running
///
/// This is a race condition test - the timer may or may not fire, but either way
/// the actor should remain stable. We verify by checking the actor is still alive
/// after enough time for the timeout to have potentially fired.
pub fn response_before_timeout_cancels_timer_test() {
  // Arrange: start session with short timeout
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 100,
      // 100ms timeout
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

  // Verify Running state
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Send control request
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_fast"), result_subject)

  // Consume the request from mock
  let assert Ok(_request_json) = process.receive(mock.writes, 500)

  // Respond immediately (before timeout)
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_fast\"}}"
  bidir.inject_message(session, response_json)

  // Assert: caller receives success before timeout
  let assert Ok(result) = process.receive(result_subject, 50)
  case result {
    RequestSuccess(_) -> should.be_true(True)
    _ -> should.fail()
  }

  // Wait for timeout to potentially fire (100ms + buffer)
  process.sleep(150)

  // Actor should still be alive and responsive (stale timeout was handled gracefully)
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Cleanup
  bidir.shutdown(session)
}
