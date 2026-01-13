/// Tests for start_session() bidirectional session API.
///
/// This test file exercises the start_session() entry point and related types.
/// Tests are designed to fail until the actual implementation is complete
/// (TDD Phase 1).
import gleeunit/should

import claude_agent_sdk.{
  type Session, type StartError, default_options, start_error_to_string,
  start_session,
}
import claude_agent_sdk/error

// =============================================================================
// TDD Phase 1: Failing Test
// =============================================================================

/// Test that start_session compiles and returns the expected error.
///
/// This test verifies the API surface is correct. It expects NotImplemented
/// error since the actual implementation will be added in Epic 8.
pub fn start_session_returns_not_implemented_test() {
  let options = default_options()

  case start_session("Hello, Claude!", options) {
    Error(error.NotImplemented) -> {
      // Expected: stub returns NotImplemented
      should.be_true(True)
    }
    Error(_other) -> {
      // Unexpected error variant
      should.fail()
    }
    Ok(_session) -> {
      // Unexpected success - implementation not ready
      should.fail()
    }
  }
}

/// Test that StartError type is accessible via main module.
pub fn start_error_type_accessible_test() {
  // Verify StartError variants are accessible and can be constructed
  let timeout_err: StartError = error.Timeout
  let spawn_err: StartError = error.SpawnFailed("test reason")
  let not_impl_err: StartError = error.NotImplemented

  // Use should.equal to verify values directly (avoids unreachable pattern warnings)
  should.equal(timeout_err, error.Timeout)
  should.equal(spawn_err, error.SpawnFailed("test reason"))
  should.equal(not_impl_err, error.NotImplemented)
}

/// Test that start_error_to_string works for all variants.
pub fn start_error_to_string_test() {
  start_error_to_string(error.Timeout)
  |> should.equal("Session initialization timed out")

  start_error_to_string(error.SpawnFailed("connection refused"))
  |> should.equal("Failed to spawn CLI process: connection refused")

  start_error_to_string(error.NotImplemented)
  |> should.equal("start_session is not yet implemented")
}

/// Test that Session type is accessible via main module.
/// Note: Session is opaque, so we can only verify the type exists.
pub fn session_type_accessible_test() {
  // This test verifies Session type compiles correctly.
  // Since Session is opaque and start_session returns NotImplemented,
  // we can only verify the type alias exists.
  let _result: Result(Session, StartError) =
    start_session("test", default_options())

  // If this compiles, the types are correctly exported
  should.be_true(True)
}
/// Test that start_session returns Ok(Session) when fully implemented.
///
/// THIS TEST IS EXPECTED TO FAIL in TDD Phase 1.
/// It will pass once Epic 8 implementation is complete.
///
/// Uncomment this test when ready to implement:
// =============================================================================
// TDD Phase 2: Success Test (Expected to Fail Until Implementation)
// =============================================================================

// pub fn start_session_succeeds_test() {
//   let options = default_options()
//
//   case start_session("Hello, Claude!", options) {
//     Ok(_session) -> {
//       // Success - session was created
//       should.be_true(True)
//     }
//     Error(err) -> {
//       // Should not fail once implemented
//       io.debug(#("Unexpected error", err))
//       should.fail()
//     }
//   }
// }
