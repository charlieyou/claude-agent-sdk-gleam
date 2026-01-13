/// Integration tests for bidirectional protocol using FullMockRunner.
///
/// Tests the complete session lifecycle without requiring the real CLI.
/// Uses FullMockRunner to simulate the bidirectional protocol.
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/json
import gleam/string
import gleeunit/should

import claude_agent_sdk/internal/bidir.{
  type SubscriberMessage, CliMessage, HookConfig, Running, SessionEnded,
  StartConfig, UserRequested,
}
import support/full_mock_runner

/// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// =============================================================================
// Test: Full Session Lifecycle
// =============================================================================

/// Test: Start session -> receive init ack -> send message -> stop
///
/// Verifies the complete happy-path lifecycle:
/// 1. Session starts and sends initialize request
/// 2. Mock runner auto-responds with init success
/// 3. Session transitions to Running
/// 4. Mock runner emits a regular message
/// 5. Subscriber receives the message
/// 6. Session can be stopped cleanly
pub fn full_session_lifecycle_test() {
  // Create mock runner with auto-init-ack
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()
    |> full_mock_runner.with_message_sequence([
      full_mock_runner.regular_message("Hello from Claude"),
    ])

  // Start the runner and get adapter
  let adapter = full_mock_runner.start(mock)

  // Create subscriber and config
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start session with mock runner
  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Wait for init request to be captured
  let assert Ok(_init_msg) = process.receive(adapter.captured_writes, 500)

  // Process init (sends auto-ack)
  let adapter = full_mock_runner.process_init(adapter)

  // Allow processing time
  process.sleep(50)

  // Should transition to Running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Emit message
  let _adapter = full_mock_runner.emit_next_message(adapter)
  process.sleep(50)

  // Subscriber should receive the message
  let assert Ok(msg) = process.receive(subscriber, 500)
  case msg {
    CliMessage(_payload) -> {
      // Message received - test passes
      Nil
    }
    _ -> {
      should.fail()
    }
  }

  // Clean shutdown
  bidir.shutdown(session)

  // Wait for session end notification
  let assert Ok(ended) = process.receive(subscriber, 500)
  case ended {
    SessionEnded(UserRequested) -> Nil
    _ -> should.fail()
  }
}

// =============================================================================
// Test: Message Routing (Control vs Regular)
// =============================================================================

/// Test: Mock runner sends control message and regular message
/// Verifies control messages go to control channel, regular to subscriber.
pub fn message_routing_test() {
  // Create mock runner that sends both control and regular messages
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()
    |> full_mock_runner.with_message_sequence([
      // Regular message first
      full_mock_runner.regular_message("Regular output"),
      // Then a hook callback (control request)
      full_mock_runner.hook_callback("test_hook", "cli_req_1"),
    ])

  let adapter = full_mock_runner.start(mock)

  // Setup hooks to capture callback
  let callback_received = process.new_subject()
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("test_hook", fn(_input: Dynamic) -> Dynamic {
          process.send(callback_received, True)
          to_dynamic(dict.from_list([#("continue", True)]))
        }),
      ]),
      permission_handlers: dict.new(),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) =
    bidir.start_with_hooks(adapter.bidir_runner, config, hooks)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Wait for and process init
  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Emit regular message
  let adapter = full_mock_runner.emit_next_message(adapter)
  process.sleep(50)

  // Should receive regular message on subscriber
  let assert Ok(regular_msg) = process.receive(subscriber, 500)
  case regular_msg {
    CliMessage(_) -> Nil
    _ -> should.fail()
  }

  // Emit hook callback
  let _adapter = full_mock_runner.emit_next_message(adapter)
  process.sleep(100)

  // Hook handler should have been called
  let assert Ok(True) = process.receive(callback_received, 500)

  // Hook response should have been written
  let assert Ok(hook_response) = process.receive(adapter.captured_writes, 500)
  should.be_true(string.contains(hook_response, "cli_req_1"))
  should.be_true(string.contains(hook_response, "control_response"))

  bidir.shutdown(session)
}

// =============================================================================
// Test: Init Handshake with Hook Registration
// =============================================================================

/// Test: Session starts with hooks registered, mock receives initialize with hook list.
pub fn init_handshake_with_hooks_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()

  // Config with MCP servers (which are sent in initialize)
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
      mcp_servers: [
        #("test_server", fn(_: Dynamic) -> Dynamic { to_dynamic(Nil) }),
      ],
    )

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Capture init request
  let assert Ok(init_json) = process.receive(adapter.captured_writes, 500)

  // Verify initialize structure
  should.be_true(string.contains(init_json, "\"type\":\"control_request\""))
  should.be_true(string.contains(init_json, "\"subtype\":\"initialize\""))
  should.be_true(string.contains(init_json, "\"request_id\":\"req_0\""))
  // MCP servers should be in the request
  should.be_true(string.contains(init_json, "test_server"))

  // Complete init
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  bidir.shutdown(session)
}

// =============================================================================
// Test: Hook Simulation
// =============================================================================

/// Test: Mock runner sends hook callback at appropriate time.
pub fn hook_simulation_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()
    |> full_mock_runner.with_hook_simulation(
      "pre_tool_hook",
      json.object([#("tool_name", json.string("bash"))]),
    )

  let adapter = full_mock_runner.start(mock)

  // Hook handler that tracks invocations
  let hook_calls = process.new_subject()
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("pre_tool_hook", fn(input: Dynamic) -> Dynamic {
          process.send(hook_calls, input)
          to_dynamic(dict.from_list([#("continue", True)]))
        }),
      ]),
      permission_handlers: dict.new(),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) =
    bidir.start_with_hooks(adapter.bidir_runner, config, hooks)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init
  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Trigger simulated hook
  let _adapter = full_mock_runner.trigger_hook_simulation(adapter)
  process.sleep(100)

  // Hook should have been called with correct input
  let assert Ok(_hook_input) = process.receive(hook_calls, 500)

  // Response should have been sent
  let assert Ok(response) = process.receive(adapter.captured_writes, 500)
  should.be_true(string.contains(response, "control_response"))

  bidir.shutdown(session)
}

// =============================================================================
// Test: Arbitrary Protocol Sequence Injection
// =============================================================================

/// Test: Inject arbitrary protocol sequences for edge case testing.
pub fn arbitrary_sequence_injection_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()
    |> full_mock_runner.with_raw_sequence([
      // Multiple regular messages - format: {"type": "assistant", "message": {...}}
      "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"First\"}]}}",
      "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Second\"}]}}",
      "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Third\"}]}}",
    ])

  let adapter = full_mock_runner.start(mock)

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init
  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Emit all messages
  let adapter = full_mock_runner.emit_next_message(adapter)
  let adapter = full_mock_runner.emit_next_message(adapter)
  let _adapter = full_mock_runner.emit_next_message(adapter)
  process.sleep(100)

  // Should receive all three messages
  let assert Ok(CliMessage(_)) = process.receive(subscriber, 200)
  let assert Ok(CliMessage(_)) = process.receive(subscriber, 200)
  let assert Ok(CliMessage(_)) = process.receive(subscriber, 200)

  bidir.shutdown(session)
}

// =============================================================================
// Test: Session State Tracking
// =============================================================================

/// Test: Mock runner correctly tracks session state.
pub fn session_state_tracking_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Mock should know it's in init state (before processing init)
  should.equal(
    full_mock_runner.get_mock_state(adapter),
    full_mock_runner.AwaitingInit,
  )

  // Complete init
  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  // Mock should be in running state
  should.equal(
    full_mock_runner.get_mock_state(adapter),
    full_mock_runner.MockRunning,
  )

  bidir.shutdown(session)

  // Give time for close to be called
  process.sleep(50)

  // Mock should know session was closed (via closed_subject)
  should.equal(
    full_mock_runner.get_mock_state(adapter),
    full_mock_runner.Closed,
  )
}
