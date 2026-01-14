/// Unit tests for bidir/routing.gleam - pure message routing logic.
///
/// Tests the pure routing function without spawning any processes.
/// Verifies that each message type routes to the correct destination.
import gleeunit/should

import claude_agent_sdk/internal/bidir/routing.{
  CanUseToolVariant, ControlRequestType, ControlResponseType,
  HookCallbackVariant, McpMessageVariant, RegularMessageType, RouteHookCallback,
  RouteMcp, RoutePermission, RouteResponse, RouteSubscriber, route_incoming,
}

// =============================================================================
// Control Request Routing
// =============================================================================

pub fn route_hook_callback_test() {
  let msg = ControlRequestType(HookCallbackVariant("cb-123"))
  let route = route_incoming(msg)
  should.equal(route, RouteHookCallback("cb-123"))
}

pub fn route_hook_callback_with_different_id_test() {
  let msg = ControlRequestType(HookCallbackVariant("hook_42"))
  let route = route_incoming(msg)
  should.equal(route, RouteHookCallback("hook_42"))
}

pub fn route_permission_request_test() {
  let msg = ControlRequestType(CanUseToolVariant("bash"))
  let route = route_incoming(msg)
  should.equal(route, RoutePermission("bash"))
}

pub fn route_permission_request_different_tool_test() {
  let msg = ControlRequestType(CanUseToolVariant("file_editor"))
  let route = route_incoming(msg)
  should.equal(route, RoutePermission("file_editor"))
}

pub fn route_mcp_message_test() {
  let msg = ControlRequestType(McpMessageVariant("test-server"))
  let route = route_incoming(msg)
  should.equal(route, RouteMcp("test-server"))
}

pub fn route_mcp_message_different_server_test() {
  let msg = ControlRequestType(McpMessageVariant("my-mcp-server"))
  let route = route_incoming(msg)
  should.equal(route, RouteMcp("my-mcp-server"))
}

// =============================================================================
// Control Response Routing
// =============================================================================

pub fn route_control_response_test() {
  let msg = ControlResponseType("req-100")
  let route = route_incoming(msg)
  should.equal(route, RouteResponse("req-100"))
}

pub fn route_control_response_different_id_test() {
  let msg = ControlResponseType("req_0")
  let route = route_incoming(msg)
  should.equal(route, RouteResponse("req_0"))
}

// =============================================================================
// Regular Message Routing
// =============================================================================

pub fn route_regular_message_test() {
  let msg = RegularMessageType
  let route = route_incoming(msg)
  should.equal(route, RouteSubscriber)
}

// =============================================================================
// Route Equality Tests
// =============================================================================

pub fn route_hook_callback_equality_test() {
  let route1 = RouteHookCallback("cb-1")
  let route2 = RouteHookCallback("cb-1")
  should.equal(route1, route2)
}

pub fn route_hook_callback_inequality_test() {
  let route1 = RouteHookCallback("cb-1")
  let route2 = RouteHookCallback("cb-2")
  should.not_equal(route1, route2)
}

pub fn different_route_types_not_equal_test() {
  let hook_route = RouteHookCallback("test")
  let perm_route = RoutePermission("test")
  should.not_equal(hook_route, perm_route)
}
