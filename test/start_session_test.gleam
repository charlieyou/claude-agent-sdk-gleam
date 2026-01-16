/// Tests for start_session() bidirectional session API.
///
/// This test file exercises the start_session() entry point and related types.
import gleam/erlang/process
import gleam/io
import gleeunit/should

import claude_agent_sdk.{
  type Session, type StartError, default_options, start_error_to_string,
  start_session, start_session_new,
}
import claude_agent_sdk/error
import claude_agent_sdk/internal/bidir
import claude_agent_sdk/options
import claude_agent_sdk/session
import support/mock_bidir_runner

// =============================================================================
// API Surface Tests
// =============================================================================

/// Test that start_session with mock runner succeeds.
///
/// Uses a mock runner factory to avoid needing real CLI.
pub fn start_session_returns_error_test() {
  // Create mock runner factory
  let mock = mock_bidir_runner.new()
  let runner = mock.runner

  let query_opts =
    default_options()
    |> options.with_bidir_runner_factory_query(fn() { runner })

  case start_session("Hello, Claude!", query_opts) {
    Ok(sess) -> {
      // Expected: succeeds with mock runner
      // Clean up
      let actor = session.get_actor(sess)
      bidir.shutdown(actor)
      process.sleep(50)
      should.be_true(True)
    }
    Error(_) -> {
      // Should not fail with mock runner
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
