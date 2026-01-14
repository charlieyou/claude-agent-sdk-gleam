/// Pure message routing logic for bidirectional sessions.
///
/// This module contains the pure routing logic that determines where incoming
/// messages should be dispatched. It has no OTP dependencies and can be unit
/// tested in isolation.
/// Routing destination for an incoming message.
///
/// Used by the dispatcher to determine which handler should process a message.
pub type MessageRoute {
  /// Route to hook callback handler with the callback_id.
  RouteHookCallback(callback_id: String)
  /// Route to permission handler with the tool_name.
  RoutePermission(tool_name: String)
  /// Route to MCP handler with the server_name.
  RouteMcp(server_name: String)
  /// Route to response correlator with the request_id.
  RouteResponse(request_id: String)
  /// Route to subscriber for regular messages.
  RouteSubscriber
}

/// Incoming message variants for routing decisions.
///
/// This is a simplified view of messages for routing purposes.
/// The actual message types are defined in control.gleam.
pub type IncomingMessageType {
  /// A control request from CLI (hooks, permissions, MCP).
  ControlRequestType(ControlRequestVariant)
  /// A control response from CLI (success/error for SDK requests).
  ControlResponseType(request_id: String)
  /// A regular message (stream events, etc.).
  RegularMessageType
}

/// Control request variants for routing.
pub type ControlRequestVariant {
  /// Hook callback request.
  HookCallbackVariant(callback_id: String)
  /// Permission check request.
  CanUseToolVariant(tool_name: String)
  /// MCP server message.
  McpMessageVariant(server_name: String)
}

/// Determine the routing destination for an incoming message.
///
/// This pure function inspects the message structure and returns the
/// appropriate route. The caller uses this to dispatch to the correct handler.
pub fn route_incoming(message: IncomingMessageType) -> MessageRoute {
  case message {
    ControlRequestType(request) -> route_control_request(request)
    ControlResponseType(request_id) -> RouteResponse(request_id)
    RegularMessageType -> RouteSubscriber
  }
}

/// Route a control request to its handler.
fn route_control_request(request: ControlRequestVariant) -> MessageRoute {
  case request {
    HookCallbackVariant(callback_id) -> RouteHookCallback(callback_id)
    CanUseToolVariant(tool_name) -> RoutePermission(tool_name)
    McpMessageVariant(server_name) -> RouteMcp(server_name)
  }
}
