/// MCP (Model Context Protocol) router for bidirectional sessions.
///
/// Routes MCP messages from the CLI to registered server handlers and
/// sends responses back via the provided response function.
///
/// ## Message Flow
///
/// ```
/// CLI sends mcp_message → actor receives McpMessage
///     → mcp_router.route() → lookup handler by server_name
///     → handler(message) → Dynamic response
///     → send McpResponse to CLI
/// ```
///
/// ## Future Work (T005/T006)
///
/// - State machine for message lifecycle (T005)
/// - Request/response correlation (T006)
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}

import claude_agent_sdk/control.{type OutgoingControlResponse, McpResponse}

/// Result of routing an MCP message.
pub type RouteResult {
  /// Handler found and invoked successfully.
  Routed(response: OutgoingControlResponse)
  /// No handler registered for the given server name.
  ServerNotFound(server_name: String)
}

/// Route an MCP message to the appropriate handler.
///
/// Looks up the server_name in handlers dict and invokes the handler
/// with the message. Returns RouteResult indicating success or failure.
///
/// ## Arguments
///
/// - `handlers`: Dict mapping server names to handler functions
/// - `request_id`: The request ID for correlation (forwarded to McpResponse)
/// - `server_name`: The MCP server to route to
/// - `message`: The MCP message payload (Dynamic)
///
/// ## Returns
///
/// - `Routed(McpResponse(...))` if handler found and invoked
/// - `ServerNotFound(server_name)` if no handler registered
pub fn route(
  handlers: Dict(String, fn(Dynamic) -> Dynamic),
  request_id: String,
  server_name: String,
  message: Dynamic,
) -> RouteResult {
  case dict.get(handlers, server_name) {
    Ok(handler) -> {
      let response_data = handler(message)
      Routed(McpResponse(request_id, response_data))
    }
    Error(Nil) -> ServerNotFound(server_name)
  }
}
