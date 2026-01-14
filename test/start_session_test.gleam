/// Tests for start_session() bidirectional session API.
///
/// This test file exercises the start_session() entry point and related types.
import gleam/io
import gleeunit/should

import claude_agent_sdk.{
  type Session, type StartError, default_options, start_error_to_string,
  start_session,
}
import claude_agent_sdk/error

// =============================================================================
// API Surface Tests
// =============================================================================

/// Test that start_session compiles and returns an error (stub not yet implemented).
///
/// This test verifies the API surface is correct. The current stub returns
/// SpawnFailed until actual implementation is complete.
pub fn start_session_returns_error_test() {
  let options = default_options()

  case start_session("Hello, Claude!", options) {
    Error(error.SpawnFailed(_)) -> {
      // Expected: stub returns SpawnFailed with "not yet implemented" message
      should.be_true(True)
    }
    Error(_other) -> {
      // Other error variants are also acceptable
      should.be_true(True)
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
  let actor_err: StartError = error.ActorStartFailed("actor error")
  let runner_err: StartError = error.RunnerStartFailed("runner error")

  // Use should.equal to verify values directly (avoids unreachable pattern warnings)
  should.equal(timeout_err, error.Timeout)
  should.equal(spawn_err, error.SpawnFailed("test reason"))
  should.equal(actor_err, error.ActorStartFailed("actor error"))
  should.equal(runner_err, error.RunnerStartFailed("runner error"))
}

/// Test that start_error_to_string works for all variants.
pub fn start_error_to_string_test() {
  start_error_to_string(error.Timeout)
  |> should.equal("Session initialization timed out")

  start_error_to_string(error.SpawnFailed("connection refused"))
  |> should.equal("Failed to spawn CLI process: connection refused")

  start_error_to_string(error.ActorStartFailed("init failed"))
  |> should.equal("Actor failed to start: init failed")

  start_error_to_string(error.RunnerStartFailed("port error"))
  |> should.equal("Runner failed to start: port error")
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

// =============================================================================
// TDD Phase 1: Skipped Test (Will enable when start_session is implemented)
// =============================================================================

/// Test that start_session returns Ok(Session) when fully implemented.
///
/// THIS TEST IS SKIPPED in TDD Phase 1 because start_session returns NotImplemented.
/// It will be enabled once the actual implementation is complete (Epic 8).
pub fn start_session_succeeds_test() {
  // Skip: start_session not yet implemented
  io.println("[SKIP] start_session not implemented - skipping success test")
}
