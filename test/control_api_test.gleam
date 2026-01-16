/// Tests for public control operation APIs.
///
/// This test file exercises the control operations (interrupt, set_permission_mode,
/// set_model, rewind_files, stop) and related error types.
/// Tests are designed to fail until the actual implementation is complete
/// (TDD Phase 1).
import gleam/erlang/process
import gleeunit/should

import claude_agent_sdk.{
  type ControlError, type Session, type StopError, control_error_to_string,
  interrupt, rewind_files, set_model, set_permission_mode, stop,
  stop_error_to_string,
}
import claude_agent_sdk/error
import claude_agent_sdk/event.{type SessionEvent}
import claude_agent_sdk/internal/bidir/actor.{
  type ActorMessage, type SubscriberMessage,
}
import claude_agent_sdk/message.{type Message}
import claude_agent_sdk/options
import claude_agent_sdk/session

// =============================================================================
// Error Type Tests (Pass in Phase 1)
// =============================================================================

/// Test that ControlError type is accessible via main module.
pub fn control_error_type_accessible_test() {
  // Verify ControlError variants are accessible and can be constructed
  let timeout_err: ControlError = error.ControlTimeout
  let closed_err: ControlError = error.ControlSessionClosed
  let rejected_err: ControlError = error.ControlRejected("test", "message")
  let checkpoint_err: ControlError = error.ControlCheckpointingNotEnabled

  // Use should.equal to verify values directly
  should.equal(timeout_err, error.ControlTimeout)
  should.equal(closed_err, error.ControlSessionClosed)
  should.equal(rejected_err, error.ControlRejected("test", "message"))
  should.equal(checkpoint_err, error.ControlCheckpointingNotEnabled)
}

/// Test that StopError type is accessible via main module.
pub fn stop_error_type_accessible_test() {
  // Verify StopError variants are accessible and can be constructed
  let closed_err: StopError = error.StopSessionClosed
  let not_impl_err: StopError = error.StopNotImplemented

  // Use should.equal to verify values directly
  should.equal(closed_err, error.StopSessionClosed)
  should.equal(not_impl_err, error.StopNotImplemented)
}

/// Test that control_error_to_string works for all variants.
pub fn control_error_to_string_test() {
  control_error_to_string(error.ControlTimeout)
  |> should.equal("Control operation timed out")

  control_error_to_string(error.ControlSessionClosed)
  |> should.equal("Session is closed")

  control_error_to_string(error.ControlRejected("interrupt", "nothing to stop"))
  |> should.equal("interrupt rejected by CLI: nothing to stop")

  control_error_to_string(error.ControlCheckpointingNotEnabled)
  |> should.equal(
    "File checkpointing is not enabled (required for rewind_files)",
  )
}

/// Test that stop_error_to_string works for all variants.
pub fn stop_error_to_string_test() {
  stop_error_to_string(error.StopSessionClosed)
  |> should.equal("Session is already closed")

  stop_error_to_string(error.StopNotImplemented)
  |> should.equal("stop is not yet implemented")
}

// =============================================================================
// API Wiring Tests - Control ops are now wired to actor
// =============================================================================
// Note: Full integration tests using mock runners are in control_integration_test.gleam.
// These tests verify basic wiring with a dummy session (no real actor).

/// Helper to create a dummy Session for testing.
/// Uses session.new directly. Since there's no real actor, control ops will timeout.
fn get_test_session() -> Session {
  let actor: process.Subject(ActorMessage) = process.new_subject()
  let messages: process.Subject(Message) = process.new_subject()
  let events: process.Subject(SessionEvent) = process.new_subject()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  session.new(actor, messages, events, subscriber)
}

/// Test that interrupt times out when actor doesn't respond.
/// With dummy session (no actor), the function should timeout.
pub fn interrupt_timeout_with_dummy_session_test() {
  let sess = get_test_session()
  case interrupt(sess) {
    // With no actor to respond, we expect timeout
    Error(error.ControlTimeout) -> should.be_true(True)
    Error(error.ControlSessionClosed) -> should.be_true(True)
    Error(error.ControlRejected(_, _)) -> should.fail()
    Error(error.ControlCheckpointingNotEnabled) -> should.fail()
    Ok(_) -> should.fail()
  }
}

/// Test that set_permission_mode times out when actor doesn't respond.
pub fn set_permission_mode_timeout_with_dummy_session_test() {
  let sess = get_test_session()
  case set_permission_mode(sess, options.Default) {
    Error(error.ControlTimeout) -> should.be_true(True)
    Error(error.ControlSessionClosed) -> should.be_true(True)
    Error(error.ControlRejected(_, _)) -> should.fail()
    Error(error.ControlCheckpointingNotEnabled) -> should.fail()
    Ok(_) -> should.fail()
  }
}

/// Test that set_model returns session closed for dead actor.
/// set_model checks if actor is alive before sending, so returns immediately.
pub fn set_model_session_closed_with_dummy_session_test() {
  let sess = get_test_session()
  case set_model(sess, "claude-sonnet-4-20250514") {
    // set_model has early actor-alive check, returns SessionStopped
    Error(error.ControlSessionClosed) -> should.be_true(True)
    Error(error.ControlTimeout) -> should.be_true(True)
    Error(error.ControlRejected(_, _)) -> should.fail()
    Error(error.ControlCheckpointingNotEnabled) -> should.fail()
    Ok(_) -> should.fail()
  }
}

/// Test that rewind_files times out when actor doesn't respond.
pub fn rewind_files_timeout_with_dummy_session_test() {
  let sess = get_test_session()
  case rewind_files(sess, "user-msg-123") {
    // Actor doesn't respond to checkpointing query, times out internally
    Error(error.ControlTimeout) -> should.be_true(True)
    Error(error.ControlSessionClosed) -> should.be_true(True)
    Error(error.ControlCheckpointingNotEnabled) -> should.fail()
    Error(error.ControlRejected(_, _)) -> should.fail()
    Ok(_) -> should.fail()
  }
}

/// Test that stop compiles and returns StopNotImplemented.
pub fn stop_returns_not_implemented_test() {
  let session = get_test_session()
  case stop(session) {
    Error(error.StopNotImplemented) -> should.be_true(True)
    Error(_other) -> should.fail()
    Ok(_) -> should.fail()
  }
}

// =============================================================================
// Type Signature Verification Tests (Pass in Phase 1)
// =============================================================================

/// Verify interrupt has correct type signature.
pub fn interrupt_type_signature_test() {
  // This test verifies the function compiles with expected signature.
  // Type: fn(Session) -> Result(Nil, ControlError)
  let _fn_ref: fn(Session) -> Result(Nil, ControlError) = interrupt
  should.be_true(True)
}

/// Verify set_permission_mode has correct type signature.
pub fn set_permission_mode_type_signature_test() {
  // Type: fn(Session, PermissionMode) -> Result(Nil, ControlError)
  let _fn_ref: fn(Session, options.PermissionMode) -> Result(Nil, ControlError) =
    set_permission_mode
  should.be_true(True)
}

/// Verify set_model has correct type signature.
pub fn set_model_type_signature_test() {
  // Type: fn(Session, String) -> Result(Nil, ControlError)
  let _fn_ref: fn(Session, String) -> Result(Nil, ControlError) = set_model
  should.be_true(True)
}

/// Verify rewind_files has correct type signature.
pub fn rewind_files_type_signature_test() {
  // Type: fn(Session, String) -> Result(Nil, ControlError)
  let _fn_ref: fn(Session, String) -> Result(Nil, ControlError) = rewind_files
  should.be_true(True)
}

/// Verify stop has correct type signature.
pub fn stop_type_signature_test() {
  // Type: fn(Session) -> Result(Nil, StopError)
  let _fn_ref: fn(Session) -> Result(Nil, StopError) = stop
  should.be_true(True)
}
