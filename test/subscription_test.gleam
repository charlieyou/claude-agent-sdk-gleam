/// Tests for subscription functions (messages/events).
///
/// This test file exercises the messages() and events() functions
/// that return Subjects for receiving push-based updates from a session.
import gleam/erlang/process

import gleeunit/should

import claude_agent_sdk
import claude_agent_sdk/event
import claude_agent_sdk/session

// =============================================================================
// Message Subscription Tests
// =============================================================================

/// Test that messages() returns a Subject(Message).
pub fn messages_returns_subject_test() {
  // Create subjects for testing
  let actor_subject = process.new_subject()
  let messages_subject = process.new_subject()
  let events_subject = process.new_subject()

  // Create a session with the subjects
  let test_session =
    session.new(actor_subject, messages_subject, events_subject)

  // Get the messages subject via public API
  let result = claude_agent_sdk.messages(test_session)

  // Verify we get the same subject back
  should.equal(result, messages_subject)
}

// =============================================================================
// Events Subscription Tests
// =============================================================================

/// Test that events() returns a Subject(SessionEvent).
pub fn events_returns_subject_test() {
  // Create subjects for testing
  let actor_subject = process.new_subject()
  let messages_subject = process.new_subject()
  let events_subject = process.new_subject()

  // Create a session with the subjects
  let test_session =
    session.new(actor_subject, messages_subject, events_subject)

  // Get the events subject via public API
  let result = claude_agent_sdk.events(test_session)

  // Verify we get the same subject back
  should.equal(result, events_subject)
}

// =============================================================================
// SessionEvent Type Tests
// =============================================================================

/// Test that SessionEvent variants can be constructed.
pub fn session_event_variants_test() {
  // Test SessionCompleted variant
  let _completed: event.SessionEvent =
    event.SessionCompleted(result: create_mock_result())

  // Test SessionStopped variant
  let stopped: event.SessionEvent = event.SessionStopped

  // Test SessionFailed variant
  let failed: event.SessionEvent = event.SessionFailed(error: "test error")

  // Verify they match expected values
  should.equal(stopped, event.SessionStopped)
  should.equal(failed, event.SessionFailed(error: "test error"))
}

/// Test SessionEvent type is re-exported from main module.
pub fn session_event_reexport_test() {
  // Verify we can use the type alias from main module
  let _event: claude_agent_sdk.SessionEvent = event.SessionStopped

  should.be_true(True)
}

// =============================================================================
// Helpers
// =============================================================================

import gleam/option.{None}

import claude_agent_sdk/message

/// Create a mock ResultMessage for testing.
fn create_mock_result() -> message.ResultMessage {
  message.ResultMessage(
    subtype: None,
    uuid: None,
    session_id: None,
    is_error: None,
    duration_ms: None,
    duration_api_ms: None,
    num_turns: None,
    result: None,
    total_cost_usd: None,
    usage: None,
    model_usage: None,
    permission_denials: None,
    structured_output: None,
    errors: None,
  )
}
