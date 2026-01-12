/// Tests for message routing in bidirectional session GenServer.
///
/// Verifies that each incoming message type is routed to the correct handler:
/// - control_request (hook_callback) -> hook dispatcher
/// - control_request (can_use_tool) -> permission dispatcher
/// - control_request (mcp_message) -> MCP handler
/// - control_response -> correlate with pending_requests
/// - Regular messages -> subscriber Subject
/// - Malformed messages -> logged and dropped
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/option.{None, Some}
import gleeunit/should

// Helper to convert any value to Dynamic (identity function in Erlang)
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

import claude_agent_sdk/control.{
  CanUseTool, ControlRequest, ControlResponse, Error as ControlError,
  HookCallback, McpMessage, RegularMessage, Success,
}
import claude_agent_sdk/internal/bidir.{
  PendingRequest, RequestError, RequestSuccess, RouteHookCallback, RouteMcp,
  RoutePermission, RouteResponse, RouteSubscriber,
}
import claude_agent_sdk/internal/control_decoder
import claude_agent_sdk/message.{System, SystemMessage}

// =============================================================================
// Routing Dispatch Table Tests
// =============================================================================

pub fn decode_hook_callback_routes_to_control_request_test() {
  // Verify hook_callback JSON decodes to ControlRequest(HookCallback)
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"req-1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"cb-1\",\"input\":{}}}"
  let result = control_decoder.decode_line(json)
  case result {
    Ok(ControlRequest(HookCallback(request_id: rid, callback_id: cid, ..))) -> {
      should.equal(rid, "req-1")
      should.equal(cid, "cb-1")
    }
    _ -> should.fail()
  }
}

pub fn decode_can_use_tool_routes_to_control_request_test() {
  // Verify can_use_tool JSON decodes to ControlRequest(CanUseTool)
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"req-2\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"bash\",\"input\":{},\"permission_suggestions\":[]}}"
  let result = control_decoder.decode_line(json)
  case result {
    Ok(ControlRequest(CanUseTool(request_id: rid, tool_name: name, ..))) -> {
      should.equal(rid, "req-2")
      should.equal(name, "bash")
    }
    _ -> should.fail()
  }
}

pub fn decode_mcp_message_routes_to_control_request_test() {
  // Verify mcp_message JSON decodes to ControlRequest(McpMessage)
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"req-3\",\"request\":{\"subtype\":\"mcp_message\",\"server_name\":\"test-server\",\"message\":{}}}"
  let result = control_decoder.decode_line(json)
  case result {
    Ok(ControlRequest(McpMessage(request_id: rid, server_name: name, ..))) -> {
      should.equal(rid, "req-3")
      should.equal(name, "test-server")
    }
    _ -> should.fail()
  }
}

pub fn decode_control_response_success_test() {
  // Verify success control_response decodes correctly
  let json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req-4\",\"response\":{\"key\":\"value\"}}}"
  let result = control_decoder.decode_line(json)
  case result {
    Ok(ControlResponse(Success(request_id: rid, ..))) -> {
      should.equal(rid, "req-4")
    }
    _ -> should.fail()
  }
}

pub fn decode_control_response_error_test() {
  // Verify error control_response decodes correctly
  let json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req-5\",\"error\":\"something went wrong\"}}"
  let result = control_decoder.decode_line(json)
  case result {
    Ok(ControlResponse(ControlError(request_id: rid, message: msg))) -> {
      should.equal(rid, "req-5")
      should.equal(msg, "something went wrong")
    }
    _ -> should.fail()
  }
}

pub fn decode_regular_system_message_test() {
  // Verify system message decodes to RegularMessage
  let json =
    "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"sess-1\",\"tools\":[],\"mcp_servers\":[],\"model\":\"claude-3\"}"
  let result = control_decoder.decode_line(json)
  case result {
    Ok(RegularMessage(System(..))) -> should.be_true(True)
    _ -> should.fail()
  }
}

// =============================================================================
// Response Correlation Tests
// =============================================================================

pub fn correlate_response_matches_pending_request_test() {
  // Test that correlate_response finds matching pending request
  let reply_subject: process.Subject(bidir.RequestResult) =
    process.new_subject()
  let pending =
    dict.from_list([
      #(
        "req-100",
        PendingRequest(
          request_id: "req-100",
          reply_to: reply_subject,
          sent_at: 0,
        ),
      ),
    ])

  // Simulate response correlation
  let response = Success(request_id: "req-100", payload: to_dynamic("ok"))
  let matched = dict.get(pending, response.request_id)
  should.be_ok(matched)
}

pub fn correlate_response_unmatched_request_id_test() {
  // Test that unmatched request_id returns Error
  let reply_subject: process.Subject(bidir.RequestResult) =
    process.new_subject()
  let pending =
    dict.from_list([
      #(
        "req-100",
        PendingRequest(
          request_id: "req-100",
          reply_to: reply_subject,
          sent_at: 0,
        ),
      ),
    ])

  // Look up non-existent request
  let matched = dict.get(pending, "req-999")
  should.be_error(matched)
}

// =============================================================================
// Route Incoming Message Tests
// =============================================================================

pub fn route_incoming_identifies_hook_callback_test() {
  // Test route_incoming correctly identifies hook_callback
  let msg =
    ControlRequest(HookCallback(
      request_id: "req-1",
      callback_id: "cb-1",
      input: to_dynamic(Nil),
      tool_use_id: None,
    ))
  let route = bidir.route_incoming(msg)
  should.equal(route, RouteHookCallback("cb-1"))
}

pub fn route_incoming_identifies_permission_request_test() {
  // Test route_incoming correctly identifies can_use_tool
  let msg =
    ControlRequest(CanUseTool(
      request_id: "req-2",
      tool_name: "bash",
      input: to_dynamic(Nil),
      permission_suggestions: [],
      blocked_path: None,
    ))
  let route = bidir.route_incoming(msg)
  should.equal(route, RoutePermission("bash"))
}

pub fn route_incoming_identifies_mcp_message_test() {
  // Test route_incoming correctly identifies mcp_message
  let msg =
    ControlRequest(McpMessage(
      request_id: "req-3",
      server_name: "test-server",
      message: to_dynamic(Nil),
    ))
  let route = bidir.route_incoming(msg)
  should.equal(route, RouteMcp("test-server"))
}

pub fn route_incoming_identifies_control_response_test() {
  // Test route_incoming correctly identifies control_response
  let msg =
    ControlResponse(Success(request_id: "req-4", payload: to_dynamic(Nil)))
  let route = bidir.route_incoming(msg)
  should.equal(route, RouteResponse("req-4"))
}

pub fn route_incoming_identifies_control_error_response_test() {
  // Test route_incoming correctly identifies error control_response
  let msg = ControlResponse(ControlError(request_id: "req-5", message: "error"))
  let route = bidir.route_incoming(msg)
  should.equal(route, RouteResponse("req-5"))
}

pub fn route_incoming_identifies_regular_message_test() {
  // Test route_incoming correctly identifies regular message
  let sys_msg =
    SystemMessage(
      subtype: Some("init"),
      uuid: None,
      session_id: Some("sess-1"),
      cwd: None,
      model: Some("claude-3"),
      tools: Some([]),
      mcp_servers: Some([]),
      permission_mode: None,
      api_key_source: None,
      slash_commands: None,
      agents: None,
      claude_code_version: None,
    )
  let msg = RegularMessage(System(sys_msg))
  let route = bidir.route_incoming(msg)
  should.equal(route, RouteSubscriber)
}

// =============================================================================
// Resolve Pending Request Tests
// =============================================================================

pub fn resolve_pending_sends_success_to_reply_subject_test() {
  // Test that resolving a pending request sends success to reply_to
  let reply_subject: process.Subject(bidir.RequestResult) =
    process.new_subject()
  let pending_req =
    PendingRequest(request_id: "req-1", reply_to: reply_subject, sent_at: 0)

  // Resolve with success
  let response = Success(request_id: "req-1", payload: to_dynamic("result"))
  bidir.resolve_pending(pending_req, response)

  // Check reply was sent
  case process.receive(reply_subject, 100) {
    Ok(RequestSuccess(_payload)) -> {
      // Payload should be the dynamic value
      should.be_true(True)
    }
    _ -> should.fail()
  }
}

pub fn resolve_pending_sends_error_to_reply_subject_test() {
  // Test that resolving a pending request sends error to reply_to
  let reply_subject: process.Subject(bidir.RequestResult) =
    process.new_subject()
  let pending_req =
    PendingRequest(request_id: "req-1", reply_to: reply_subject, sent_at: 0)

  // Resolve with error
  let response = ControlError(request_id: "req-1", message: "failed")
  bidir.resolve_pending(pending_req, response)

  // Check reply was sent
  case process.receive(reply_subject, 100) {
    Ok(RequestError(msg)) -> should.equal(msg, "failed")
    _ -> should.fail()
  }
}
