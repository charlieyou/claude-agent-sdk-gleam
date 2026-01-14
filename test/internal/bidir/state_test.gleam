/// Unit tests for bidir/state.gleam - pure lifecycle state machine.
///
/// Tests the pure state transition logic without spawning any processes.
/// Verifies:
/// - All valid transitions (Starting→InitSent→Running→Stopped)
/// - Invalid transitions return Error(InvalidTransition)
/// - Terminal states (Stopped, Failed) reject all events
import gleeunit/should

import claude_agent_sdk/error.{
  CliExitedDuringInit, CliExitedDuringStartup, InitializationError,
  InitializationTimeout, RuntimeError,
}
import claude_agent_sdk/internal/bidir/state.{
  CliSpawned, ErrorOccurred, Failed, InitSent, InitSuccess, InitTimeout,
  InvalidTransition, PortClosed, Running, Starting, StopRequested, Stopped,
  initial_state, is_terminal, transition,
}

// =============================================================================
// initial_state Tests
// =============================================================================

pub fn initial_state_returns_starting_test() {
  should.equal(initial_state(), Starting)
}

// =============================================================================
// is_terminal Tests
// =============================================================================

pub fn starting_is_not_terminal_test() {
  should.be_false(is_terminal(Starting))
}

pub fn init_sent_is_not_terminal_test() {
  should.be_false(is_terminal(InitSent))
}

pub fn running_is_not_terminal_test() {
  should.be_false(is_terminal(Running))
}

pub fn stopped_is_terminal_test() {
  should.be_true(is_terminal(Stopped))
}

pub fn failed_is_terminal_test() {
  should.be_true(is_terminal(Failed(RuntimeError("test"))))
}

// =============================================================================
// Valid Transitions - Happy Path
// =============================================================================

pub fn starting_to_init_sent_on_cli_spawned_test() {
  let result = transition(Starting, CliSpawned)
  should.equal(result, Ok(InitSent))
}

pub fn init_sent_to_running_on_success_test() {
  let result = transition(InitSent, InitSuccess)
  should.equal(result, Ok(Running))
}

pub fn running_to_stopped_on_stop_request_test() {
  let result = transition(Running, StopRequested)
  should.equal(result, Ok(Stopped))
}

pub fn running_to_stopped_on_port_closed_test() {
  let result = transition(Running, PortClosed)
  should.equal(result, Ok(Stopped))
}

// =============================================================================
// Valid Transitions - Error Paths
// =============================================================================

pub fn starting_to_failed_on_error_test() {
  let result = transition(Starting, ErrorOccurred("spawn error"))
  should.equal(result, Ok(Failed(RuntimeError("spawn error"))))
}

pub fn starting_to_failed_on_port_closed_test() {
  let result = transition(Starting, PortClosed)
  should.equal(result, Ok(Failed(CliExitedDuringStartup)))
}

pub fn init_sent_to_failed_on_error_test() {
  let result = transition(InitSent, ErrorOccurred("init error"))
  should.equal(result, Ok(Failed(InitializationError("init error"))))
}

pub fn init_sent_to_failed_on_timeout_test() {
  let result = transition(InitSent, InitTimeout)
  should.equal(result, Ok(Failed(InitializationTimeout)))
}

pub fn init_sent_to_failed_on_port_closed_test() {
  let result = transition(InitSent, PortClosed)
  should.equal(result, Ok(Failed(CliExitedDuringInit)))
}

pub fn running_to_failed_on_error_test() {
  let result = transition(Running, ErrorOccurred("runtime error"))
  should.equal(result, Ok(Failed(RuntimeError("runtime error"))))
}

// =============================================================================
// Invalid Transitions - Terminal States
// =============================================================================

pub fn stopped_rejects_cli_spawned_test() {
  should.be_error(transition(Stopped, CliSpawned))
}

pub fn stopped_rejects_init_success_test() {
  should.be_error(transition(Stopped, InitSuccess))
}

pub fn stopped_rejects_stop_requested_test() {
  should.be_error(transition(Stopped, StopRequested))
}

pub fn stopped_rejects_port_closed_test() {
  should.be_error(transition(Stopped, PortClosed))
}

pub fn stopped_rejects_error_occurred_test() {
  should.be_error(transition(Stopped, ErrorOccurred("error")))
}

pub fn stopped_rejects_init_timeout_test() {
  should.be_error(transition(Stopped, InitTimeout))
}

pub fn failed_rejects_cli_spawned_test() {
  let failed = Failed(RuntimeError("previous error"))
  should.be_error(transition(failed, CliSpawned))
}

pub fn failed_rejects_init_success_test() {
  let failed = Failed(RuntimeError("previous error"))
  should.be_error(transition(failed, InitSuccess))
}

pub fn failed_rejects_stop_requested_test() {
  let failed = Failed(RuntimeError("previous error"))
  should.be_error(transition(failed, StopRequested))
}

pub fn failed_rejects_port_closed_test() {
  let failed = Failed(RuntimeError("previous error"))
  should.be_error(transition(failed, PortClosed))
}

pub fn failed_rejects_error_occurred_test() {
  let failed = Failed(RuntimeError("previous error"))
  should.be_error(transition(failed, ErrorOccurred("new error")))
}

pub fn failed_rejects_init_timeout_test() {
  let failed = Failed(RuntimeError("previous error"))
  should.be_error(transition(failed, InitTimeout))
}

// =============================================================================
// Invalid Transitions - Wrong Event for State
// =============================================================================

pub fn starting_rejects_init_success_test() {
  should.be_error(transition(Starting, InitSuccess))
}

pub fn starting_rejects_stop_requested_test() {
  should.be_error(transition(Starting, StopRequested))
}

pub fn starting_rejects_init_timeout_test() {
  should.be_error(transition(Starting, InitTimeout))
}

pub fn init_sent_rejects_cli_spawned_test() {
  should.be_error(transition(InitSent, CliSpawned))
}

pub fn init_sent_rejects_stop_requested_test() {
  should.be_error(transition(InitSent, StopRequested))
}

pub fn running_rejects_cli_spawned_test() {
  should.be_error(transition(Running, CliSpawned))
}

pub fn running_rejects_init_success_test() {
  should.be_error(transition(Running, InitSuccess))
}

pub fn running_rejects_init_timeout_test() {
  should.be_error(transition(Running, InitTimeout))
}

// =============================================================================
// InvalidTransition Error Details
// =============================================================================

pub fn invalid_transition_contains_from_state_test() {
  let result = transition(Stopped, CliSpawned)
  case result {
    Error(InvalidTransition(from, _event)) -> {
      should.equal(from, Stopped)
    }
    _ -> should.fail()
  }
}

pub fn invalid_transition_contains_event_test() {
  let result = transition(Stopped, InitSuccess)
  case result {
    Error(InvalidTransition(_from, event)) -> {
      should.equal(event, InitSuccess)
    }
    _ -> should.fail()
  }
}

// =============================================================================
// Full Path Test
// =============================================================================

pub fn full_happy_path_starting_to_stopped_test() {
  // Verify the complete happy path without spawning processes
  let s0 = initial_state()
  should.equal(s0, Starting)
  should.be_false(is_terminal(s0))

  let assert Ok(s1) = transition(s0, CliSpawned)
  should.equal(s1, InitSent)
  should.be_false(is_terminal(s1))

  let assert Ok(s2) = transition(s1, InitSuccess)
  should.equal(s2, Running)
  should.be_false(is_terminal(s2))

  let assert Ok(s3) = transition(s2, StopRequested)
  should.equal(s3, Stopped)
  should.be_true(is_terminal(s3))
}

pub fn error_path_init_sent_to_failed_test() {
  let s0 = initial_state()
  let assert Ok(s1) = transition(s0, CliSpawned)
  let assert Ok(s2) = transition(s1, InitTimeout)
  should.equal(s2, Failed(InitializationTimeout))
  should.be_true(is_terminal(s2))
}
