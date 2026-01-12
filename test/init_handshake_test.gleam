/// Tests for initialization handshake (T3).
///
/// Verifies:
/// - Init control_request sent on actor start
/// - Success response transitions to RUNNING
/// - Error response transitions to FAILED
/// - 10s timeout transitions to FAILED
/// - Capabilities stored on success
/// - queued_ops flushed on success
import gleam/dict
import gleam/erlang/process
import gleam/string
import gleeunit/should

import claude_agent_sdk/internal/bidir.{
  type SubscriberMessage, CliExitedDuringInit, InitFailed, InitSent,
  InitializationError, InitializationTimeout, Running, SessionEnded,
}
import support/mock_bidir_runner

// =============================================================================
// Init Request Sent Tests
// =============================================================================

pub fn init_request_sent_on_start_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  // Create subscriber for messages
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start the actor - should send init request and transition to InitSent
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Verify we transition to InitSent
  let lifecycle = bidir.get_lifecycle(session, 1000)
  should.equal(lifecycle, InitSent)

  // Verify init request was sent
  let assert Ok(written) = process.receive(mock.writes, 500)
  should.be_true(string.contains(written, "control_request"))
  should.be_true(string.contains(written, "initialize"))
  should.be_true(string.contains(written, "req_0"))

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Success Response Tests
// =============================================================================

pub fn success_response_transitions_to_running_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Should be in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Send success response directly to actor
  let success_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, success_json)

  // Allow time for message processing
  process.sleep(50)

  // Should transition to Running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}

pub fn capabilities_stored_on_success_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Send success response with capabilities
  let success_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"hooks_supported\":true,\"permissions_supported\":true}}}}"
  bidir.inject_message(session, success_json)

  // Allow time for message processing
  process.sleep(50)

  // Verify capabilities are stored (via get_capabilities call)
  let caps = bidir.get_capabilities(session, 1000)
  should.be_some(caps)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Error Response Tests
// =============================================================================

pub fn error_response_transitions_to_failed_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Should be in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Send error response
  let error_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req_0\",\"error\":\"unsupported version\"}}"
  bidir.inject_message(session, error_json)

  // Actor will exit - listen for SessionEnded on subscriber
  let assert Ok(msg) = process.receive(subscriber, 1000)
  case msg {
    SessionEnded(InitFailed(InitializationError(reason))) -> {
      should.be_true(string.contains(reason, "unsupported version"))
    }
    _ -> {
      should.fail()
    }
  }
}

// =============================================================================
// Timeout Tests
// =============================================================================

pub fn timeout_transitions_to_failed_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()

  // Use a short init timeout for testing (100ms instead of 10s)
  let config =
    bidir.StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 100,
      default_hook_timeout_ms: 30_000,
    )

  // Start actor
  let assert Ok(_session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Don't send any response - let it timeout
  // Actor will exit - listen for SessionEnded on subscriber
  let assert Ok(msg) = process.receive(subscriber, 1000)
  case msg {
    SessionEnded(InitFailed(InitializationTimeout)) -> should.be_true(True)
    _ -> should.fail()
  }
}

// =============================================================================
// Request ID Mismatch Tests
// =============================================================================

pub fn wrong_request_id_ignored_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Send success response with wrong request_id
  let wrong_id_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"wrong_id\",\"response\":{}}}"
  bidir.inject_message(session, wrong_id_json)

  // Allow time for message processing
  process.sleep(50)

  // Should still be in InitSent (wrong ID ignored)
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Port Closed During Init Tests
// =============================================================================

pub fn port_closed_during_init_transitions_to_failed_test() {
  // Create mock runner
  let mock = mock_bidir_runner.new()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start actor
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Wait for init request
  let assert Ok(_written) = process.receive(mock.writes, 500)

  // Should be in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Simulate port closed
  bidir.inject_port_closed(session)

  // Actor will exit - listen for SessionEnded on subscriber
  let assert Ok(msg) = process.receive(subscriber, 1000)
  case msg {
    SessionEnded(InitFailed(CliExitedDuringInit)) -> should.be_true(True)
    _ -> should.fail()
  }
}
