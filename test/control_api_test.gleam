/// Tests for public control operation APIs.
///
/// This test file exercises the control operations (interrupt, set_permission_mode,
/// set_model, rewind_files, stop) and related error types.
/// Tests are designed to fail until the actual implementation is complete
/// (TDD Phase 1).
import gleeunit/should

import claude_agent_sdk.{
  type ControlError, type Session, type StartError, type StopError,
  control_error_to_string, default_options, interrupt, rewind_files, set_model,
  set_permission_mode, start_session, stop, stop_error_to_string,
}
import claude_agent_sdk/error
import claude_agent_sdk/options

// =============================================================================
// Error Type Tests (Pass in Phase 1)
// =============================================================================

/// Test that ControlError type is accessible via main module.
pub fn control_error_type_accessible_test() {
  // Verify ControlError variants are accessible and can be constructed
  let timeout_err: ControlError = error.ControlTimeout
  let closed_err: ControlError = error.ControlSessionClosed
  let not_impl_err: ControlError = error.ControlNotImplemented

  // Use should.equal to verify values directly
  should.equal(timeout_err, error.ControlTimeout)
  should.equal(closed_err, error.ControlSessionClosed)
  should.equal(not_impl_err, error.ControlNotImplemented)
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

  control_error_to_string(error.ControlNotImplemented)
  |> should.equal("Control operation is not yet implemented")
}

/// Test that stop_error_to_string works for all variants.
pub fn stop_error_to_string_test() {
  stop_error_to_string(error.StopSessionClosed)
  |> should.equal("Session is already closed")

  stop_error_to_string(error.StopNotImplemented)
  |> should.equal("stop is not yet implemented")
}

// =============================================================================
// API Surface Tests - Stub Returns NotImplemented (Pass in Phase 1)
// =============================================================================

/// Helper to get a Session for testing (uses start_session stub).
/// Since start_session returns NotImplemented, we use this to verify
/// the function signatures compile correctly.
fn get_test_session() -> Result(Session, StartError) {
  start_session("test", default_options())
}

/// Test that interrupt compiles and returns ControlNotImplemented.
pub fn interrupt_returns_not_implemented_test() {
  case get_test_session() {
    Ok(session) -> {
      case interrupt(session) {
        Error(error.ControlNotImplemented) -> should.be_true(True)
        Error(_other) -> should.fail()
        Ok(_) -> should.fail()
      }
    }
    Error(error.NotImplemented) -> {
      // Expected - start_session stub returns NotImplemented
      // Test passes because function signatures compile
      should.be_true(True)
    }
    Error(_) -> should.fail()
  }
}

/// Test that set_permission_mode compiles and returns ControlNotImplemented.
pub fn set_permission_mode_returns_not_implemented_test() {
  case get_test_session() {
    Ok(session) -> {
      case set_permission_mode(session, options.Default) {
        Error(error.ControlNotImplemented) -> should.be_true(True)
        Error(_other) -> should.fail()
        Ok(_) -> should.fail()
      }
    }
    Error(error.NotImplemented) -> {
      // Expected - start_session stub returns NotImplemented
      should.be_true(True)
    }
    Error(_) -> should.fail()
  }
}

/// Test that set_model compiles and returns ControlNotImplemented.
pub fn set_model_returns_not_implemented_test() {
  case get_test_session() {
    Ok(session) -> {
      case set_model(session, "claude-sonnet-4-20250514") {
        Error(error.ControlNotImplemented) -> should.be_true(True)
        Error(_other) -> should.fail()
        Ok(_) -> should.fail()
      }
    }
    Error(error.NotImplemented) -> {
      // Expected - start_session stub returns NotImplemented
      should.be_true(True)
    }
    Error(_) -> should.fail()
  }
}

/// Test that rewind_files compiles and returns ControlNotImplemented.
pub fn rewind_files_returns_not_implemented_test() {
  case get_test_session() {
    Ok(session) -> {
      case rewind_files(session, "user-msg-123") {
        Error(error.ControlNotImplemented) -> should.be_true(True)
        Error(_other) -> should.fail()
        Ok(_) -> should.fail()
      }
    }
    Error(error.NotImplemented) -> {
      // Expected - start_session stub returns NotImplemented
      should.be_true(True)
    }
    Error(_) -> should.fail()
  }
}

/// Test that stop compiles and returns StopNotImplemented.
pub fn stop_returns_not_implemented_test() {
  case get_test_session() {
    Ok(session) -> {
      case stop(session) {
        Error(error.StopNotImplemented) -> should.be_true(True)
        Error(_other) -> should.fail()
        Ok(_) -> should.fail()
      }
    }
    Error(error.NotImplemented) -> {
      // Expected - start_session stub returns NotImplemented
      should.be_true(True)
    }
    Error(_) -> should.fail()
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
