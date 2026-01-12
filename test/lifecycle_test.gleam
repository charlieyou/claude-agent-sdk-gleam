/// Tests for bidirectional session lifecycle state machine.
///
/// Verifies all valid and invalid state transitions per spec:
/// - STARTING → INIT_SENT → RUNNING → STOPPED
/// - INIT_SENT/RUNNING → FAILED (error paths)
/// - Terminal states (STOPPED/FAILED) reject all transitions
import gleeunit/should

import claude_agent_sdk/internal/bidir.{
  type InvalidTransition, type LifecycleEvent, type SessionLifecycle,
  CliExitedDuringInit, CliExitedDuringStartup, CliSpawned, ErrorOccurred, Failed,
  InitSent, InitSuccess, InitTimeout, InitializationError, InitializationTimeout,
  InvalidTransition, PortClosed, Running, RuntimeError, Starting, StopRequested,
  Stopped, transition,
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

pub fn stopped_rejects_all_events_test() {
  // Stopped is terminal - no transitions allowed
  should.be_error(transition(Stopped, CliSpawned))
  should.be_error(transition(Stopped, InitSuccess))
  should.be_error(transition(Stopped, StopRequested))
  should.be_error(transition(Stopped, PortClosed))
  should.be_error(transition(Stopped, ErrorOccurred("error")))
  should.be_error(transition(Stopped, InitTimeout))
}

pub fn failed_rejects_all_events_test() {
  // Failed is terminal - no transitions allowed
  let failed = Failed(RuntimeError("previous error"))
  should.be_error(transition(failed, CliSpawned))
  should.be_error(transition(failed, InitSuccess))
  should.be_error(transition(failed, StopRequested))
  should.be_error(transition(failed, PortClosed))
  should.be_error(transition(failed, ErrorOccurred("new error")))
  should.be_error(transition(failed, InitTimeout))
}

// =============================================================================
// Invalid Transitions - Wrong Event for State
// =============================================================================

pub fn starting_only_accepts_cli_spawned_test() {
  // Starting should reject events other than CliSpawned
  should.be_error(transition(Starting, InitSuccess))
  should.be_error(transition(Starting, StopRequested))
  should.be_error(transition(Starting, InitTimeout))
}

pub fn starting_accepts_error_to_fail_test() {
  // Starting can fail on error
  let result = transition(Starting, ErrorOccurred("spawn error"))
  should.equal(result, Ok(Failed(RuntimeError("spawn error"))))
}

pub fn starting_accepts_port_closed_to_fail_test() {
  // Port closing during start is a failure
  let result = transition(Starting, PortClosed)
  should.equal(result, Ok(Failed(CliExitedDuringStartup)))
}

pub fn init_sent_rejects_cli_spawned_test() {
  // Can't spawn CLI twice
  should.be_error(transition(InitSent, CliSpawned))
}

pub fn init_sent_rejects_stop_requested_test() {
  // Can't stop during initialization
  should.be_error(transition(InitSent, StopRequested))
}

pub fn running_rejects_cli_spawned_test() {
  // Can't spawn CLI when already running
  should.be_error(transition(Running, CliSpawned))
}

pub fn running_rejects_init_success_test() {
  // Can't re-initialize
  should.be_error(transition(Running, InitSuccess))
}

pub fn running_rejects_init_timeout_test() {
  // Init timeout not valid when already running
  should.be_error(transition(Running, InitTimeout))
}

// =============================================================================
// Invalid Transition Error Details
// =============================================================================

pub fn invalid_transition_contains_from_state_test() {
  let result = transition(Stopped, CliSpawned)
  case result {
    Error(InvalidTransition(from, _to_event)) -> {
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
