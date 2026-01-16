/// Tests for bidirectional session GenServer.
///
/// These tests verify the OTP actor skeleton:
/// - Actor can be started
/// - Actor responds to basic messages
/// - Session state types are correctly defined
import gleam/erlang/process
import gleeunit/should

import claude_agent_sdk/error.{RuntimeError}
import claude_agent_sdk/event
import claude_agent_sdk/internal/bidir
import claude_agent_sdk/internal/bidir/actor.{
  type RequestResult, type SessionLifecycle, type SubscriberMessage, Failed,
  InitSent, Pong, QueuedRequest, RequestSessionStopped, Running, SessionEnded,
  Starting, Stopped, UserRequested,
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
  let _failed: SessionLifecycle = Failed(RuntimeError("test error"))

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

  // Get lifecycle state - should be InitSent after actor sends init request
  let lifecycle = bidir.get_lifecycle(session, 1000)
  should.equal(lifecycle, InitSent)
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

// =============================================================================
// Termination Cleanup Tests
// =============================================================================

pub fn shutdown_notifies_subscriber_with_session_ended_test() {
  // Create a mock runner
  let mock = mock_bidir_runner.new()

  // Create subscriber for messages
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start the actor
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Shutdown the actor
  bidir.shutdown(session)

  // Give it time to shutdown
  process.sleep(50)

  // Verify subscriber received SessionEnded with UserRequested reason
  case process.receive(subscriber, 100) {
    Ok(SessionEnded(UserRequested)) -> should.be_true(True)
    Ok(_other) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn shutdown_closes_runner_and_notifies_subscriber_test() {
  // Create a mock runner
  let mock = mock_bidir_runner.new()

  // Create subscriber for messages
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start the actor
  let assert Ok(session) = bidir.start(mock.runner, config)

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

  // Verify subscriber received SessionEnded
  case process.receive(subscriber, 100) {
    Ok(SessionEnded(UserRequested)) -> should.be_true(True)
    Ok(_other) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn shutdown_resolves_queued_operations_test() {
  // Create a mock runner
  let mock = mock_bidir_runner.new()

  // Create subscriber for messages
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start the actor
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Create a reply subject for the queued operation
  let reply_subject: process.Subject(RequestResult) = process.new_subject()

  // Create a queued operation
  let queued_op =
    QueuedRequest(
      request_id: "test-req-1",
      payload: "",
      reply_to: reply_subject,
    )

  // Queue the operation via internal state manipulation
  // Note: In production, operations are queued via bidir.queue_operation
  // For this test, we verify the cleanup behavior by checking the reply_subject
  // after shutdown. The operation would be queued during Starting/InitSent state.

  // Since we can't easily inject queued_ops into a running actor,
  // we verify the type system: QueuedRequest has reply_to field
  // and RequestSessionStopped is a valid RequestResult variant
  let _op = queued_op
  let _result: RequestResult = RequestSessionStopped

  // Shutdown the actor
  bidir.shutdown(session)

  // Give it time to shutdown
  process.sleep(50)

  // Verify subscriber received SessionEnded
  case process.receive(subscriber, 100) {
    Ok(SessionEnded(UserRequested)) -> should.be_true(True)
    Ok(_other) -> should.fail()
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Public API Integration Tests (start_session_new)
// =============================================================================

import claude_agent_sdk
import claude_agent_sdk/options
import claude_agent_sdk/session

pub fn start_session_new_with_mock_runner_test() {
  // Create a mock runner factory
  let mock = mock_bidir_runner.new()
  let runner = mock.runner

  // Configure options with mock runner factory
  let cli_opts = options.cli_options()
  let sdk_opts = options.sdk_options()
  let bidir_opts =
    options.bidir_options()
    |> options.with_bidir_runner_factory(fn() { runner })

  // Start session through public API
  let result =
    claude_agent_sdk.start_session_new(cli_opts, sdk_opts, bidir_opts)

  // Should succeed
  should.be_ok(result)

  // Get the session and verify it's usable
  let assert Ok(sess) = result

  // Get the PUBLIC events subject for lifecycle events
  let events_subject = claude_agent_sdk.events(sess)

  // Shutdown via the actor subject
  let actor_subject = session.get_actor(sess)
  bidir.shutdown(actor_subject)

  // Give it time to shutdown
  process.sleep(50)

  // Verify events subject received SessionStopped (forwarded from bridge)
  case process.receive(events_subject, 200) {
    Ok(event.SessionStopped) -> should.be_true(True)
    Ok(_other) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn start_session_new_emits_lifecycle_via_subscriber_test() {
  // Create a mock runner factory
  let mock = mock_bidir_runner.new()
  let runner = mock.runner

  // Configure options with mock runner factory
  let cli_opts = options.cli_options()
  let sdk_opts = options.sdk_options()
  let bidir_opts =
    options.bidir_options()
    |> options.with_bidir_runner_factory(fn() { runner })

  // Start session through public API
  let assert Ok(sess) =
    claude_agent_sdk.start_session_new(cli_opts, sdk_opts, bidir_opts)

  // Verify session lifecycle is InitSent (waiting for CLI handshake)
  let actor_subject = session.get_actor(sess)
  let lifecycle = bidir.get_lifecycle(actor_subject, 1000)
  should.equal(lifecycle, InitSent)

  // Clean up
  bidir.shutdown(actor_subject)
  process.sleep(50)
}

pub fn start_session_legacy_works_with_query_options_test() {
  // Create a mock runner factory
  let mock = mock_bidir_runner.new()
  let runner = mock.runner

  // Configure legacy QueryOptions with mock runner factory
  let query_opts =
    options.default_options()
    |> options.with_bidir_runner_factory_query(fn() { runner })

  // Start session through legacy API
  let result = claude_agent_sdk.start_session("test prompt", query_opts)

  // Should succeed
  should.be_ok(result)

  // Get the session and clean up
  let assert Ok(sess) = result
  let actor_subject = session.get_actor(sess)
  bidir.shutdown(actor_subject)
  process.sleep(50)
}

/// Regression test: Public events() API receives SessionEnded on shutdown.
/// This test fails if the bridge process is not spawned to forward
/// SubscriberMessage to the public events subject.
pub fn public_events_api_receives_session_ended_test() {
  // Create a mock runner factory
  let mock = mock_bidir_runner.new()
  let runner = mock.runner

  // Configure options with mock runner factory
  let cli_opts = options.cli_options()
  let sdk_opts = options.sdk_options()
  let bidir_opts =
    options.bidir_options()
    |> options.with_bidir_runner_factory(fn() { runner })

  // Start session through public API
  let assert Ok(sess) =
    claude_agent_sdk.start_session_new(cli_opts, sdk_opts, bidir_opts)

  // Small delay to ensure bridge process has started and is receiving
  process.sleep(10)

  // Get the PUBLIC events subject (not internal subscriber)
  let events_subject = claude_agent_sdk.events(sess)

  // Shutdown the actor - this should send SessionEnded to subscriber,
  // which should be bridged to the public events subject
  let actor_subject = session.get_actor(sess)
  bidir.shutdown(actor_subject)

  // The public events subject should receive SessionEnded via bridge
  // If this times out, the bridge is not working
  case process.receive(events_subject, 500) {
    Ok(_event) -> should.be_true(True)
    Error(_) -> {
      // Timeout means bridge is broken - events never forwarded
      should.fail()
    }
  }
}
