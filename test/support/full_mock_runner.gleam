/// Full mock runner for bidirectional protocol simulation.
///
/// This mock runner simulates the complete Claude CLI bidirectional protocol,
/// enabling end-to-end testing of session lifecycle without the real CLI.
///
/// ## Features
///
/// - Auto-responds to initialize handshake
/// - Queues and emits hook callbacks at controlled times
/// - Routes messages correctly (control vs regular)
/// - Tracks session state for assertions
///
/// ## Usage
///
/// ```gleam
/// let mock = full_mock_runner.new()
///   |> full_mock_runner.with_auto_init_ack()
///   |> full_mock_runner.with_message_sequence([...])
///
/// let adapter = full_mock_runner.start(mock)
/// let session = bidir.start(adapter.bidir_runner, config)
///
/// // Process init and emit messages...
/// full_mock_runner.process_init(adapter)
/// full_mock_runner.emit_next_message(adapter)
/// ```
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

import claude_agent_sdk/internal/bidir
import claude_agent_sdk/internal/bidir_runner.{type BidirRunner}
import claude_agent_sdk/internal/port_ffi.{type WriteError}

/// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// =============================================================================
// Types
// =============================================================================

/// Mock runner state tracking.
pub type MockState {
  /// Waiting for initialize request.
  AwaitingInit
  /// Session is running (init completed).
  MockRunning
  /// Session was closed.
  Closed
}

/// Queued message to emit.
pub type QueuedMessage {
  /// A regular message (assistant/system/user/result).
  RegularMsg(json: String)
  /// A hook callback control request.
  HookCallbackMsg(callback_id: String, request_id: String, input: Dynamic)
  /// Raw JSON string (for arbitrary injection).
  RawMsg(json: String)
}

/// Configuration for the mock runner.
pub opaque type FullMockRunner {
  FullMockRunner(
    /// Whether to auto-respond to initialize.
    auto_init_ack: Bool,
    /// Queue of messages to emit.
    message_queue: List(QueuedMessage),
    /// Simulated hooks to trigger.
    hook_simulations: List(#(String, Dynamic)),
  )
}

/// Adapter returned by start() for interacting with the mock.
///
/// This adapter includes the BidirRunner for use with bidir.start(),
/// plus channels for test interaction and state tracking.
pub type RunnerAdapter {
  RunnerAdapter(
    /// The BidirRunner to pass to bidir.start().
    bidir_runner: BidirRunner,
    /// Subject to receive captured writes from the session.
    captured_writes: Subject(String),
    /// Subject to notify when close is called.
    closed_subject: Subject(Bool),
    /// The session subject (set after bidir.start returns).
    session_subject: Option(Subject(bidir.ActorMessage)),
    /// Configuration for this mock.
    config: FullMockRunner,
    /// Current mock state.
    state: MockState,
    /// Pending messages to emit.
    pending_messages: List(QueuedMessage),
    /// Pending hook simulations.
    pending_hooks: List(#(String, Dynamic)),
    /// Counter for generating request IDs.
    next_request_id: Int,
  )
}

// =============================================================================
// Builder API
// =============================================================================

/// Create a new FullMockRunner with default configuration.
pub fn new() -> FullMockRunner {
  FullMockRunner(auto_init_ack: False, message_queue: [], hook_simulations: [])
}

/// Configure the mock to auto-respond to initialize requests.
pub fn with_auto_init_ack(mock: FullMockRunner) -> FullMockRunner {
  FullMockRunner(..mock, auto_init_ack: True)
}

/// Queue a sequence of messages to emit.
pub fn with_message_sequence(
  mock: FullMockRunner,
  messages: List(QueuedMessage),
) -> FullMockRunner {
  FullMockRunner(
    ..mock,
    message_queue: list.append(mock.message_queue, messages),
  )
}

/// Queue raw JSON strings to emit (for edge case testing).
pub fn with_raw_sequence(
  mock: FullMockRunner,
  json_strings: List(String),
) -> FullMockRunner {
  let raw_msgs = list.map(json_strings, fn(s) { RawMsg(s) })
  FullMockRunner(
    ..mock,
    message_queue: list.append(mock.message_queue, raw_msgs),
  )
}

/// Add a hook simulation to trigger later.
pub fn with_hook_simulation(
  mock: FullMockRunner,
  callback_id: String,
  input: Dynamic,
) -> FullMockRunner {
  FullMockRunner(
    ..mock,
    hook_simulations: list.append(mock.hook_simulations, [#(callback_id, input)]),
  )
}

// =============================================================================
// Message Builders
// =============================================================================

/// Create a regular assistant message.
///
/// Format: {"type": "assistant", "message": {"content": [{"type": "text", "text": "..."}]}}
pub fn regular_message(text: String) -> QueuedMessage {
  let json_str =
    json.object([
      #("type", json.string("assistant")),
      #(
        "message",
        json.object([
          #(
            "content",
            json.array(
              [
                json.object([
                  #("type", json.string("text")),
                  #("text", json.string(text)),
                ]),
              ],
              fn(x) { x },
            ),
          ),
        ]),
      ),
    ])
    |> json.to_string

  RegularMsg(json_str)
}

/// Create a hook callback control request.
pub fn hook_callback(callback_id: String, request_id: String) -> QueuedMessage {
  HookCallbackMsg(callback_id, request_id, to_dynamic(dict.new()))
}

// =============================================================================
// Runner Lifecycle
// =============================================================================

/// Start the mock runner and return an adapter for interaction.
///
/// The adapter includes a BidirRunner suitable for passing to bidir.start().
/// After starting the session, call set_session() to enable message injection.
pub fn start(config: FullMockRunner) -> RunnerAdapter {
  let captured_writes: Subject(String) = process.new_subject()
  let closed_subject: Subject(Bool) = process.new_subject()

  // Create the BidirRunner that integrates with bidir.start()
  let bidir_runner =
    bidir_runner.mock(
      on_write: fn(data) -> Result(Nil, WriteError) {
        process.send(captured_writes, data)
        Ok(Nil)
      },
      on_close: fn() { process.send(closed_subject, True) },
    )

  RunnerAdapter(
    bidir_runner: bidir_runner,
    captured_writes: captured_writes,
    closed_subject: closed_subject,
    session_subject: None,
    config: config,
    state: AwaitingInit,
    pending_messages: config.message_queue,
    pending_hooks: config.hook_simulations,
    next_request_id: 1,
  )
}

/// Set the session subject for message injection.
///
/// Call this after bidir.start() returns successfully.
pub fn set_session(
  adapter: RunnerAdapter,
  session: Subject(bidir.ActorMessage),
) -> RunnerAdapter {
  RunnerAdapter(..adapter, session_subject: Some(session))
}

/// Stop the mock runner (cleanup).
pub fn stop(_adapter: RunnerAdapter) -> Nil {
  Nil
}

// =============================================================================
// Mock Control Functions
// =============================================================================

/// Process the captured init request and send auto-ack if configured.
///
/// Call this after receiving the init request on captured_writes.
/// If auto_init_ack is enabled, this sends a success response.
/// Returns the updated adapter with MockRunning state.
pub fn process_init(adapter: RunnerAdapter) -> RunnerAdapter {
  case adapter.config.auto_init_ack, adapter.session_subject {
    True, Some(session) -> {
      let success_json =
        "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
      bidir.inject_message(session, success_json)
      RunnerAdapter(..adapter, state: MockRunning)
    }
    _, _ -> adapter
  }
}

/// Emit the next message from the queue.
///
/// The message will be injected into the session via inject_message.
/// Returns the updated adapter with the message removed from queue.
pub fn emit_next_message(adapter: RunnerAdapter) -> RunnerAdapter {
  case adapter.pending_messages, adapter.session_subject {
    [msg, ..rest], Some(session) -> {
      let json_str = queued_message_to_json(msg, adapter.next_request_id)
      bidir.inject_message(session, json_str)
      RunnerAdapter(
        ..adapter,
        pending_messages: rest,
        next_request_id: adapter.next_request_id + 1,
      )
    }
    _, _ -> adapter
  }
}

/// Trigger a pending hook simulation.
///
/// Returns the updated adapter with the hook removed from pending list.
pub fn trigger_hook_simulation(adapter: RunnerAdapter) -> RunnerAdapter {
  case adapter.pending_hooks, adapter.session_subject {
    [#(callback_id, input), ..rest], Some(session) -> {
      let req_id = "cli_hook_" <> int.to_string(adapter.next_request_id)
      let json_str = build_hook_callback_json(callback_id, req_id, input)
      bidir.inject_message(session, json_str)
      RunnerAdapter(
        ..adapter,
        pending_hooks: rest,
        next_request_id: adapter.next_request_id + 1,
      )
    }
    _, _ -> adapter
  }
}

/// Get the current mock state.
pub fn get_mock_state(adapter: RunnerAdapter) -> MockState {
  // Check if close was called
  case process.receive(adapter.closed_subject, 0) {
    Ok(True) -> Closed
    _ -> adapter.state
  }
}

/// Mark the adapter as closed (for state tracking after shutdown).
pub fn mark_closed(adapter: RunnerAdapter) -> RunnerAdapter {
  RunnerAdapter(..adapter, state: Closed)
}

// =============================================================================
// Internal Helpers
// =============================================================================

/// Convert a queued message to JSON string.
fn queued_message_to_json(msg: QueuedMessage, _next_id: Int) -> String {
  case msg {
    RegularMsg(json) -> json
    RawMsg(json) -> json
    HookCallbackMsg(callback_id, request_id, _input) -> {
      build_hook_callback_json(callback_id, request_id, to_dynamic(dict.new()))
    }
  }
}

/// Build hook callback JSON.
fn build_hook_callback_json(
  callback_id: String,
  request_id: String,
  _input: Dynamic,
) -> String {
  json.object([
    #("type", json.string("control_request")),
    #("request_id", json.string(request_id)),
    #(
      "request",
      json.object([
        #("subtype", json.string("hook_callback")),
        #("callback_id", json.string(callback_id)),
        #("input", json.object([])),
      ]),
    ),
  ])
  |> json.to_string
}
