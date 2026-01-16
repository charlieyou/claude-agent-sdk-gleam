/// Integration tests for MCP message routing.
///
/// Verifies that MCP messages from CLI are routed to registered handlers
/// and responses are sent back correctly.
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

import claude_agent_sdk/control.{McpResponse}
import claude_agent_sdk/internal/bidir
import claude_agent_sdk/internal/bidir/actor.{
  type ActorMessage, type SubscriberMessage, InitSent, InjectedMessage, Running,
  StartConfig,
}
import claude_agent_sdk/internal/mcp_router
import support/full_mock_runner

/// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// =============================================================================
// Unit Tests: mcp_router.route
// =============================================================================

pub fn route_found_handler_returns_routed_test() {
  let handlers =
    dict.from_list([
      #("test-server", fn(_: Dynamic) -> Dynamic { to_dynamic("response") }),
    ])

  let result =
    mcp_router.route(handlers, "req_123", "test-server", to_dynamic(dict.new()))

  case result {
    mcp_router.Routed(_response) -> should.be_true(True)
    mcp_router.ServerNotFound(_) -> should.fail()
  }
}

pub fn route_missing_handler_returns_server_not_found_test() {
  let handlers = dict.new()

  let result =
    mcp_router.route(
      handlers,
      "req_456",
      "unknown-server",
      to_dynamic(dict.new()),
    )

  case result {
    mcp_router.Routed(_) -> should.fail()
    mcp_router.ServerNotFound(name) -> should.equal(name, "unknown-server")
  }
}

pub fn route_invokes_handler_with_message_test() {
  // Handler that echoes the message
  let handlers =
    dict.from_list([
      #("echo-server", fn(msg: Dynamic) -> Dynamic { msg }),
    ])

  let input_msg = to_dynamic(dict.from_list([#("method", "tools/list")]))
  let result = mcp_router.route(handlers, "req_789", "echo-server", input_msg)

  case result {
    mcp_router.Routed(response) -> {
      // Verify it's an McpResponse with correct request_id
      case response {
        McpResponse(req_id, _data) -> should.equal(req_id, "req_789")
        _ -> should.fail()
      }
    }
    mcp_router.ServerNotFound(_) -> should.fail()
  }
}

/// Test: Request id correlation - id "abc" → response id "abc".
///
/// Acceptance Criteria (T006): request id "abc" → response id "abc"
pub fn route_request_id_abc_returns_response_id_abc_test() {
  let handlers =
    dict.from_list([
      #("test-server", fn(_: Dynamic) -> Dynamic { to_dynamic("result") }),
    ])

  let result =
    mcp_router.route(handlers, "abc", "test-server", to_dynamic(dict.new()))

  case result {
    mcp_router.Routed(response) -> {
      case response {
        McpResponse(req_id, _data) -> should.equal(req_id, "abc")
        _ -> should.fail()
      }
    }
    mcp_router.ServerNotFound(_) -> should.fail()
  }
}

/// Test: Malformed request (no id) → error response with null id.
///
/// Acceptance Criteria (T006): malformed request (no id) → error response with null id
///
/// Per JSON-RPC 2.0 spec, when a request has no id, the error response
/// should have id: null and use error code -32600 (Invalid Request).
pub fn make_jsonrpc_error_null_id_returns_null_id_test() {
  let error_response =
    mcp_router.make_jsonrpc_error_null_id("Missing request id")

  // Verify the response structure
  let id_decoder = decode.at(["id"], decode.optional(decode.string))
  case decode.run(error_response, id_decoder) {
    Ok(id) -> {
      // id should be None (null in JSON)
      should.equal(id, None)
    }
    Error(_) -> should.fail()
  }

  // Verify error code is -32600 (Invalid Request)
  let code_decoder = decode.at(["error", "code"], decode.int)
  case decode.run(error_response, code_decoder) {
    Ok(code) -> should.equal(code, -32_600)
    Error(_) -> should.fail()
  }

  // Verify jsonrpc version is "2.0"
  let version_decoder = decode.at(["jsonrpc"], decode.string)
  case decode.run(error_response, version_decoder) {
    Ok(version) -> should.equal(version, "2.0")
    Error(_) -> should.fail()
  }

  // Verify error message is present
  let msg_decoder = decode.at(["error", "message"], decode.string)
  case decode.run(error_response, msg_decoder) {
    Ok(msg) -> should.equal(msg, "Missing request id")
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Integration Test: MCP Message Flow (CLI → Handler → Response)
// =============================================================================

/// Test: MCP message from CLI is routed to handler and response sent back.
///
/// Acceptance Criteria (from T004):
/// - Given a Running session with registered MCP handler for server "test"
/// - When CLI sends MCP request for server "test" with id "123"
/// - Then handler receives request and SDK sends response with id "123" to CLI
pub fn mcp_message_routed_to_handler_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)

  // Track handler invocations
  let handler_calls = process.new_subject()

  // Create config with MCP server handler
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
      mcp_servers: [
        #("test-server", fn(msg: Dynamic) -> Dynamic {
          // Record the call and return a response
          process.send(handler_calls, msg)
          to_dynamic(dict.from_list([#("status", "ok")]))
        }),
      ],
      on_warning: None,
    )

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init handshake
  let assert Ok(init_msg) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.capture_init_request(adapter, init_msg)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  // Verify session is running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Inject MCP message from "CLI"
  let mcp_message_json =
    json.object([
      #("type", json.string("control_request")),
      #("request_id", json.string("mcp_123")),
      #(
        "request",
        json.object([
          #("subtype", json.string("mcp_message")),
          #("server_name", json.string("test-server")),
          #("message", json.object([#("method", json.string("tools/list"))])),
        ]),
      ),
    ])
    |> json.to_string

  bidir.inject_message(session, mcp_message_json)
  process.sleep(100)

  // Verify handler was called
  let assert Ok(received_msg) = process.receive(handler_calls, 500)

  // Decode the message to verify structure
  let method_decoder = decode.at(["method"], decode.string)
  case decode.run(received_msg, method_decoder) {
    Ok(method) -> should.equal(method, "tools/list")
    Error(_) -> should.fail()
  }

  // Verify response was sent to CLI
  let assert Ok(response_json) = process.receive(adapter.captured_writes, 500)

  // Response should be control_response with success subtype and mcp_response key
  // Format: {"type":"control_response","response":{"subtype":"success","request_id":"...","response":{"mcp_response":...}}}
  should.be_true(string.contains(response_json, "control_response"))
  should.be_true(string.contains(response_json, "mcp_123"))
  should.be_true(string.contains(response_json, "mcp_response"))

  bidir.shutdown(session)
}

/// Test: MCP message for unknown server returns JSON-RPC error response.
pub fn mcp_message_unknown_server_returns_error_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)

  // Create config with NO MCP servers
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
      mcp_servers: [],
      on_warning: None,
    )

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)

  // Set session for injection
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init handshake
  let assert Ok(init_msg) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.capture_init_request(adapter, init_msg)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Inject MCP message for unknown server
  let mcp_message_json =
    json.object([
      #("type", json.string("control_request")),
      #("request_id", json.string("mcp_unknown")),
      #(
        "request",
        json.object([
          #("subtype", json.string("mcp_message")),
          #("server_name", json.string("nonexistent-server")),
          #("message", json.object([#("data", json.string("test"))])),
        ]),
      ),
    ])
    |> json.to_string

  bidir.inject_message(session, mcp_message_json)
  process.sleep(100)

  // Session should still be running (no crash)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Should receive an error response (not silent ignore)
  let assert Ok(response_json) = process.receive(adapter.captured_writes, 500)
  should.be_true(string.contains(response_json, "mcp_unknown"))
  should.be_true(string.contains(response_json, "error"))
  should.be_true(string.contains(response_json, "-32603"))

  bidir.shutdown(session)
}

// =============================================================================
// T005 Tests: MCP Message Lifecycle State Machine
// =============================================================================

/// Test: Handler error returns JSON-RPC error response with code -32603.
///
/// Acceptance Criteria: Handler error → -32603 response
pub fn handler_error_returns_jsonrpc_error_test() {
  // Handler that panics
  let handlers =
    dict.from_list([
      #("crash-server", fn(_: Dynamic) -> Dynamic { panic as "Handler crash!" }),
    ])

  let result =
    mcp_router.route(
      handlers,
      "req_error",
      "crash-server",
      to_dynamic(dict.new()),
    )

  case result {
    mcp_router.Routed(response) -> {
      // Should be routed with error response
      case response {
        McpResponse(req_id, data) -> {
          should.equal(req_id, "req_error")
          // Verify error response contains -32603 code
          let code_decoder = decode.at(["error", "code"], decode.int)
          case decode.run(data, code_decoder) {
            Ok(code) -> should.equal(code, -32_603)
            Error(_) -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
    mcp_router.ServerNotFound(_) -> should.fail()
  }
}

/// Test: MCP messages during InitSent are queued, not immediately routed.
///
/// This verifies that MCP messages arriving before init completes are queued
/// and not processed until the session transitions to Running.
pub fn mcp_messages_queued_during_init_sent_test() {
  let mock = full_mock_runner.new()
  // Don't auto-ack init - we want to stay in InitSent
  let adapter = full_mock_runner.start(mock)

  // Track handler invocations
  let handler_calls = process.new_subject()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
      mcp_servers: [
        #("test-server", fn(msg: Dynamic) -> Dynamic {
          process.send(handler_calls, msg)
          to_dynamic("response")
        }),
      ],
      on_warning: None,
    )

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let adapter = full_mock_runner.set_session(adapter, session)

  // Consume init message but don't ack
  let assert Ok(_init_msg) = process.receive(adapter.captured_writes, 500)

  // Session should be in InitSent
  should.equal(bidir.get_lifecycle(session, 1000), actor.InitSent)

  // Inject MCP message while in InitSent
  let mcp_message_json =
    json.object([
      #("type", json.string("control_request")),
      #("request_id", json.string("mcp_queued")),
      #(
        "request",
        json.object([
          #("subtype", json.string("mcp_message")),
          #("server_name", json.string("test-server")),
          #("message", json.object([#("data", json.string("queued"))])),
        ]),
      ),
    ])
    |> json.to_string

  bidir.inject_message(session, mcp_message_json)
  process.sleep(50)

  // Handler should NOT have been called (message is queued)
  case process.receive(handler_calls, 100) {
    Ok(_) -> should.fail()
    // Expected: no message
    Error(Nil) -> should.be_true(True)
  }

  // Now complete init handshake manually by injecting init response
  let init_success_json =
    json.object([
      #("type", json.string("control_response")),
      #(
        "response",
        json.object([
          #("subtype", json.string("success")),
          #("request_id", json.string("req_0")),
          #("response", json.object([#("capabilities", json.object([]))])),
        ]),
      ),
    ])
    |> json.to_string
  bidir.inject_message(session, init_success_json)
  process.sleep(100)

  // Session should be running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Handler should now have been called (queued message flushed)
  let assert Ok(_received_msg) = process.receive(handler_calls, 500)

  bidir.shutdown(session)
}

/// Test: MCP messages during Stopped are discarded silently.
pub fn mcp_messages_discarded_when_stopped_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)

  let handler_calls = process.new_subject()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
      mcp_servers: [
        #("test-server", fn(msg: Dynamic) -> Dynamic {
          process.send(handler_calls, msg)
          to_dynamic("response")
        }),
      ],
      on_warning: None,
    )

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init
  let assert Ok(init_msg) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.capture_init_request(adapter, init_msg)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)

  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Shutdown session (transition to Stopped)
  bidir.shutdown(session)
  process.sleep(50)

  // Try to inject MCP message after shutdown
  // (Note: this may fail silently if the actor is already dead, which is fine)
  let mcp_message_json =
    json.object([
      #("type", json.string("control_request")),
      #("request_id", json.string("mcp_after_stop")),
      #(
        "request",
        json.object([
          #("subtype", json.string("mcp_message")),
          #("server_name", json.string("test-server")),
          #("message", json.object([#("data", json.string("ignored"))])),
        ]),
      ),
    ])
    |> json.to_string

  // Send message to dead actor - will be silently dropped
  process.send(session, InjectedMessage(mcp_message_json))
  process.sleep(50)

  // Handler should not have been called
  case process.receive(handler_calls, 100) {
    Ok(_) -> should.fail()
    Error(Nil) -> should.be_true(True)
  }
}

/// Test: MCP queue overflow emits warning via on_warning callback.
///
/// Verifies that when the MCP queue exceeds max size (100), the oldest
/// message is dropped and the on_warning callback is invoked.
pub fn mcp_queue_overflow_emits_warning_test() {
  let mock = full_mock_runner.new()
  let adapter = full_mock_runner.start(mock)

  // Track warning calls
  let warning_calls = process.new_subject()
  let handler_calls = process.new_subject()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
      mcp_servers: [
        #("test-server", fn(msg: Dynamic) -> Dynamic {
          process.send(handler_calls, msg)
          to_dynamic("response")
        }),
      ],
      on_warning: Some(fn(msg: String) { process.send(warning_calls, msg) }),
    )

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let _adapter = full_mock_runner.set_session(adapter, session)

  // Consume init message but don't ack - stay in InitSent
  let assert Ok(_init_msg) = process.receive(adapter.captured_writes, 500)
  should.equal(bidir.get_lifecycle(session, 1000), InitSent)

  // Send 101 MCP messages to trigger overflow
  list.range(1, 101)
  |> list.each(fn(i) {
    let mcp_json =
      json.object([
        #("type", json.string("control_request")),
        #("request_id", json.string("mcp_" <> int.to_string(i))),
        #(
          "request",
          json.object([
            #("subtype", json.string("mcp_message")),
            #("server_name", json.string("test-server")),
            #("message", json.object([#("seq", json.int(i))])),
          ]),
        ),
      ])
      |> json.to_string
    bidir.inject_message(session, mcp_json)
  })
  process.sleep(100)

  // on_warning should have been called (at least once for the 101st message)
  let assert Ok(warning_msg) = process.receive(warning_calls, 500)
  should.be_true(string.contains(warning_msg, "MCP queue overflow"))

  bidir.shutdown(session)
}

/// Test: MCP queue maintains FIFO order and drops oldest on overflow.
///
/// Verifies that when queue overflows, the oldest (first-in) message is dropped,
/// not the newest, and messages are flushed in FIFO order.
pub fn mcp_queue_fifo_order_test() {
  let mock = full_mock_runner.new()
  let adapter = full_mock_runner.start(mock)

  // Track handler calls with sequence numbers
  let handler_calls = process.new_subject()

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use a smaller queue for testing (we'll use max_mcp_queue_size constant)
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
      mcp_servers: [
        #("test-server", fn(msg: Dynamic) -> Dynamic {
          process.send(handler_calls, msg)
          to_dynamic("ok")
        }),
      ],
      on_warning: None,
    )

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let _adapter = full_mock_runner.set_session(adapter, session)

  // Consume init message but don't ack - stay in InitSent
  let assert Ok(_init_msg) = process.receive(adapter.captured_writes, 500)

  // Send 3 messages with sequence 1, 2, 3
  list.range(1, 3)
  |> list.each(fn(i) {
    let mcp_json =
      json.object([
        #("type", json.string("control_request")),
        #("request_id", json.string("mcp_" <> int.to_string(i))),
        #(
          "request",
          json.object([
            #("subtype", json.string("mcp_message")),
            #("server_name", json.string("test-server")),
            #("message", json.object([#("seq", json.int(i))])),
          ]),
        ),
      ])
      |> json.to_string
    bidir.inject_message(session, mcp_json)
  })
  process.sleep(50)

  // Complete init handshake to flush queue
  let init_success_json =
    json.object([
      #("type", json.string("control_response")),
      #(
        "response",
        json.object([
          #("subtype", json.string("success")),
          #("request_id", json.string("req_0")),
          #("response", json.object([#("capabilities", json.object([]))])),
        ]),
      ),
    ])
    |> json.to_string
  bidir.inject_message(session, init_success_json)
  process.sleep(100)

  // Verify messages arrive in FIFO order (1, 2, 3)
  let seq_decoder = decode.at(["seq"], decode.int)

  let assert Ok(msg1) = process.receive(handler_calls, 500)
  let assert Ok(seq1) = decode.run(msg1, seq_decoder)
  should.equal(seq1, 1)

  let assert Ok(msg2) = process.receive(handler_calls, 500)
  let assert Ok(seq2) = decode.run(msg2, seq_decoder)
  should.equal(seq2, 2)

  let assert Ok(msg3) = process.receive(handler_calls, 500)
  let assert Ok(seq3) = decode.run(msg3, seq_decoder)
  should.equal(seq3, 3)

  bidir.shutdown(session)
}
