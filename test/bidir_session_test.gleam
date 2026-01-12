/// Tests for bidirectional session GenServer.
///
/// These tests verify the OTP actor skeleton:
/// - Actor can be started
/// - Actor responds to basic messages
/// - Session state types are correctly defined
import gleam/erlang/process
import gleeunit/should

import claude_agent_sdk/internal/bidir.{
  type SessionLifecycle, type SubscriberMessage, Failed, InitSent, Pong, Running,
  Starting, Stopped,
}
import support/mock_bidir_runner

// =============================================================================
// Type Definition Tests
// =============================================================================

pub fn session_lifecycle_variants_exist_test() {
  // Verify all lifecycle variants can be constructed
  let _starting: SessionLifecycle = Starting
  let _init_sent: SessionLifecycle = InitSent
  let _running: SessionLifecycle = Running
  let _stopped: SessionLifecycle = Stopped
  let _failed: SessionLifecycle = Failed("test error")

  should.be_true(True)
}

// =============================================================================
// Actor Start Tests
// =============================================================================

pub fn actor_starts_successfully_test() {
  // Create a mock runner
  let mock = mock_bidir_runner.new()

  // Create subscriber for messages
  let subscriber = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start the actor
  let result = bidir.start(mock.runner, config)

  // Actor should start successfully
  should.be_ok(result)
}

pub fn actor_responds_to_ping_test() {
  // Create a mock runner
  let mock = mock_bidir_runner.new()

  // Create subscriber for messages
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start the actor
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Send a ping and verify we get Pong back
  let response = bidir.ping(session, 1000)
  should.equal(response, Pong)
}

pub fn actor_returns_lifecycle_state_test() {
  // Create a mock runner
  let mock = mock_bidir_runner.new()

  // Create subscriber for messages
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start the actor
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Get lifecycle state - should be Starting initially
  let lifecycle = bidir.get_lifecycle(session, 1000)
  should.equal(lifecycle, Starting)
}

// =============================================================================
// State Type Tests
// =============================================================================

pub fn session_state_can_be_constructed_test() {
  // Create a mock runner
  let mock = mock_bidir_runner.new()

  // Create subscriber for messages
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Verify we can start (which creates SessionState internally)
  let result = bidir.start(mock.runner, config)
  should.be_ok(result)
}

pub fn default_config_sets_timeout_test() {
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Default timeout should be 60 seconds
  should.equal(config.default_timeout_ms, 60_000)
}

// =============================================================================
// Multiple Session Tests
// =============================================================================

pub fn can_start_multiple_sessions_test() {
  // Create two independent sessions
  let mock1 = mock_bidir_runner.new()
  let mock2 = mock_bidir_runner.new()

  let subscriber1: process.Subject(SubscriberMessage) = process.new_subject()
  let subscriber2: process.Subject(SubscriberMessage) = process.new_subject()

  let config1 = bidir.default_config(subscriber1)
  let config2 = bidir.default_config(subscriber2)

  let result1 = bidir.start(mock1.runner, config1)
  let result2 = bidir.start(mock2.runner, config2)

  should.be_ok(result1)
  should.be_ok(result2)
}

// =============================================================================
// Shutdown Tests
// =============================================================================

pub fn actor_can_shutdown_test() {
  // Create a mock runner
  let mock = mock_bidir_runner.new()

  // Create subscriber for messages
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start the actor
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Verify actor is alive
  let response = bidir.ping(session, 1000)
  should.equal(response, Pong)

  // Shutdown the actor
  bidir.shutdown(session)

  // Give it time to shutdown
  process.sleep(50)

  // Verify close was called on the runner
  case process.receive(mock.closed, 100) {
    Ok(True) -> should.be_true(True)
    Ok(False) -> should.fail()
    Error(_) -> should.fail()
  }
}
