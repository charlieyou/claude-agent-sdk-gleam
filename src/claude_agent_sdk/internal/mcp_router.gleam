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
/// ## Error Handling
///
/// Handler crashes/errors return JSON-RPC error response with code -32603
/// (internal error) per the MCP specification.
///
/// ## Request/Response Correlation (T006)
///
/// Request IDs are extracted from the control request envelope and passed
/// through to the response. The router wraps handler responses in McpResponse
/// with the same request_id. For malformed requests (no id), use
/// make_jsonrpc_error_null_id to return an error with null id per JSON-RPC 2.0.
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}

import claude_agent_sdk/control.{type OutgoingControlResponse, McpResponse}
import claude_agent_sdk/internal/port_io

/// JSON-RPC internal error code per specification.
pub const jsonrpc_internal_error: Int = -32_603

/// JSON-RPC invalid request error code per specification.
/// Used when request is malformed (e.g., missing id).
pub const jsonrpc_invalid_request: Int = -32_600

// FFI: Convert any value to Dynamic (identity function at runtime)
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

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
/// Handler crashes are caught and return JSON-RPC error response with
/// code -32603 (internal error).
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
/// - `Routed(McpResponse(...))` if handler found and invoked (success or error)
/// - `ServerNotFound(server_name)` if no handler registered
pub fn route(
  handlers: Dict(String, fn(Dynamic) -> Dynamic),
  request_id: String,
  server_name: String,
  message: Dynamic,
) -> RouteResult {
  case dict.get(handlers, server_name) {
    Ok(handler) -> {
      // Use rescue to catch handler panics and convert to JSON-RPC error
      case port_io.rescue(fn() { handler(message) }) {
        Ok(response_data) -> {
          Routed(McpResponse(request_id, response_data))
        }
        Error(error_msg) -> {
          // Handler crashed - return JSON-RPC error response
          let error_response = make_jsonrpc_error(request_id, error_msg)
          Routed(McpResponse(request_id, error_response))
        }
      }
    }
    Error(Nil) -> ServerNotFound(server_name)
  }
}

/// Create a JSON-RPC error response as Dynamic.
///
/// Format: {"jsonrpc": "2.0", "id": "...", "error": {"code": -32603, "message": "..."}}
/// Uses dict.from_list to create an Erlang map that can be decoded.
pub fn make_jsonrpc_error(request_id: String, error_message: String) -> Dynamic {
  to_dynamic(
    dict.from_list([
      #("jsonrpc", to_dynamic("2.0")),
      #("id", to_dynamic(request_id)),
      #(
        "error",
        to_dynamic(
          dict.from_list([
            #("code", to_dynamic(jsonrpc_internal_error)),
            #("message", to_dynamic(error_message)),
          ]),
        ),
      ),
    ]),
  )
}

/// Create a JSON-RPC error response with null id as Dynamic.
///
/// Per JSON-RPC 2.0 spec, when a request has no id or is malformed,
/// the response should have id: null.
///
/// Format: {"jsonrpc": "2.0", "id": null, "error": {"code": -32600, "message": "..."}}
pub fn make_jsonrpc_error_null_id(error_message: String) -> Dynamic {
  to_dynamic(
    dict.from_list([
      #("jsonrpc", to_dynamic("2.0")),
      #("id", to_dynamic(Nil)),
      #(
        "error",
        to_dynamic(
          dict.from_list([
            #("code", to_dynamic(jsonrpc_invalid_request)),
            #("message", to_dynamic(error_message)),
          ]),
        ),
      ),
    ]),
  )
}
