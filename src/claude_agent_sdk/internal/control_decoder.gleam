/// JSON decoder for bidirectional protocol control messages.
///
/// This module decodes NDJSON lines from CLI into IncomingMessage variants.
/// It handles:
/// - Regular messages (system, assistant, user, result)
/// - Control requests (hook_callback, can_use_tool, mcp_message)
/// - Control responses (success, error)
///
/// ## Tolerant Decoding
/// - Unknown fields are ignored (forward compatibility)
/// - request_id may be at `response.request_id` or top-level
/// - Missing optional fields default gracefully
import claude_agent_sdk/control.{
  type IncomingMessage, CanUseTool, ControlRequest, ControlResponse,
  Error as ControlError, HookCallback, McpMessage, RegularMessage, Success,
}
import claude_agent_sdk/internal/decoder
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}

/// Decode error types for control message parsing.
pub type DecodeError {
  /// JSON syntax error during parsing
  JsonError(String)
  /// Required field is missing from JSON
  MissingField(String)
  /// Message type field has invalid value
  InvalidType(String)
  /// Control request/response has unknown subtype
  UnknownSubtype(subtype: String, message: String)
}

/// Main entry point: decode NDJSON line into IncomingMessage.
///
/// Parses JSON and routes to appropriate decoder based on "type" field:
/// - "control_request" -> IncomingControlRequest
/// - "control_response" -> IncomingControlResponse
/// - "system"/"assistant"/"user"/"result" -> RegularMessage
pub fn decode_line(json_string: String) -> Result(IncomingMessage, DecodeError) {
  case json.parse(json_string, decode.dynamic) {
    Error(_) -> Error(JsonError("Invalid JSON syntax"))
    Ok(raw) -> decode_from_dynamic(raw)
  }
}

/// Decode IncomingMessage from already-parsed Dynamic.
pub fn decode_from_dynamic(raw: Dynamic) -> Result(IncomingMessage, DecodeError) {
  // Extract the type field
  let type_decoder = {
    use msg_type <- decode.field("type", decode.string)
    decode.success(msg_type)
  }
  case decode.run(raw, type_decoder) {
    Error(_) -> Error(MissingField("type"))
    Ok(msg_type) -> {
      case msg_type {
        "control_request" -> decode_control_request(raw)
        "control_response" -> decode_control_response(raw)
        "system" -> decode_regular_message(raw)
        "assistant" -> decode_regular_message(raw)
        "user" -> decode_regular_message(raw)
        "result" -> decode_regular_message(raw)
        unknown -> Error(InvalidType(unknown))
      }
    }
  }
}

/// Decode a control request from Dynamic.
fn decode_control_request(raw: Dynamic) -> Result(IncomingMessage, DecodeError) {
  // Extract request_id from top level
  let request_id_decoder = {
    use req_id <- decode.field("request_id", decode.string)
    decode.success(req_id)
  }
  case decode.run(raw, request_id_decoder) {
    Error(_) -> Error(MissingField("request_id"))
    Ok(request_id) -> {
      // Extract the request object
      let request_decoder = {
        use request <- decode.field("request", decode.dynamic)
        decode.success(request)
      }
      case decode.run(raw, request_decoder) {
        Error(_) -> Error(MissingField("request"))
        Ok(request) -> decode_control_request_inner(request_id, request)
      }
    }
  }
}

/// Decode the inner request object based on subtype.
fn decode_control_request_inner(
  request_id: String,
  request: Dynamic,
) -> Result(IncomingMessage, DecodeError) {
  // Get subtype from request
  let subtype_decoder = {
    use subtype <- decode.field("subtype", decode.string)
    decode.success(subtype)
  }
  case decode.run(request, subtype_decoder) {
    Error(_) -> Error(MissingField("request.subtype"))
    Ok(subtype) -> {
      case subtype {
        "hook_callback" -> decode_hook_callback(request_id, request)
        "can_use_tool" -> decode_can_use_tool(request_id, request)
        "mcp_message" -> decode_mcp_message(request_id, request)
        unknown ->
          Error(UnknownSubtype(
            subtype: unknown,
            message: "Unknown control_request subtype: " <> unknown,
          ))
      }
    }
  }
}

/// Decode hook_callback request.
fn decode_hook_callback(
  request_id: String,
  request: Dynamic,
) -> Result(IncomingMessage, DecodeError) {
  let decoder = {
    use callback_id <- decode.field("callback_id", decode.string)
    use input <- decode.field("input", decode.dynamic)
    use tool_use_id <- decode.optional_field(
      "tool_use_id",
      None,
      decode.optional(decode.string),
    )
    decode.success(HookCallback(request_id:, callback_id:, input:, tool_use_id:))
  }
  case decode.run(request, decoder) {
    Error(_) -> Error(MissingField("hook_callback fields"))
    Ok(req) -> Ok(ControlRequest(req))
  }
}

/// Decode can_use_tool request.
fn decode_can_use_tool(
  request_id: String,
  request: Dynamic,
) -> Result(IncomingMessage, DecodeError) {
  let decoder = {
    use tool_name <- decode.field("tool_name", decode.string)
    use input <- decode.field("input", decode.dynamic)
    use permission_suggestions <- decode.field(
      "permission_suggestions",
      decode.list(decode.string),
    )
    use blocked_path <- decode.optional_field(
      "blocked_path",
      None,
      decode.optional(decode.string),
    )
    decode.success(CanUseTool(
      request_id:,
      tool_name:,
      input:,
      permission_suggestions:,
      blocked_path:,
    ))
  }
  case decode.run(request, decoder) {
    Error(_) -> Error(MissingField("can_use_tool fields"))
    Ok(req) -> Ok(ControlRequest(req))
  }
}

/// Decode mcp_message request.
fn decode_mcp_message(
  request_id: String,
  request: Dynamic,
) -> Result(IncomingMessage, DecodeError) {
  let decoder = {
    use server_name <- decode.field("server_name", decode.string)
    use message <- decode.field("message", decode.dynamic)
    decode.success(McpMessage(request_id:, server_name:, message:))
  }
  case decode.run(request, decoder) {
    Error(_) -> Error(MissingField("mcp_message fields"))
    Ok(req) -> Ok(ControlRequest(req))
  }
}

/// Decode a control response from Dynamic.
fn decode_control_response(raw: Dynamic) -> Result(IncomingMessage, DecodeError) {
  // Extract the response object
  let response_decoder = {
    use response <- decode.field("response", decode.dynamic)
    decode.success(response)
  }
  case decode.run(raw, response_decoder) {
    Error(_) -> Error(MissingField("response"))
    Ok(response) -> decode_control_response_inner(raw, response)
  }
}

/// Decode the inner response object based on subtype.
fn decode_control_response_inner(
  raw: Dynamic,
  response: Dynamic,
) -> Result(IncomingMessage, DecodeError) {
  // Get subtype from response
  let subtype_decoder = {
    use subtype <- decode.field("subtype", decode.string)
    decode.success(subtype)
  }
  case decode.run(response, subtype_decoder) {
    Error(_) -> Error(MissingField("response.subtype"))
    Ok(subtype) -> {
      case subtype {
        "success" -> decode_success_response(raw, response)
        "error" -> decode_error_response(raw, response)
        unknown ->
          Error(UnknownSubtype(
            subtype: unknown,
            message: "Unknown control_response subtype: " <> unknown,
          ))
      }
    }
  }
}

/// Decode success response.
/// Tolerant: request_id may be in response or at top level.
fn decode_success_response(
  raw: Dynamic,
  response: Dynamic,
) -> Result(IncomingMessage, DecodeError) {
  // Try to get request_id from response first, then top level
  let request_id = get_request_id(raw, response)
  case request_id {
    None -> Error(MissingField("request_id"))
    Some(req_id) -> {
      // Get the payload (response.response)
      let payload_decoder = {
        use payload <- decode.optional_field(
          "response",
          dynamic.nil(),
          decode.dynamic,
        )
        decode.success(payload)
      }
      case decode.run(response, payload_decoder) {
        Error(_) ->
          Ok(
            ControlResponse(Success(request_id: req_id, payload: dynamic.nil())),
          )
        Ok(payload) ->
          Ok(ControlResponse(Success(request_id: req_id, payload:)))
      }
    }
  }
}

/// Decode error response.
/// Tolerant: request_id may be in response or at top level.
/// Tolerant: error text may be in "error" or "message" field.
fn decode_error_response(
  raw: Dynamic,
  response: Dynamic,
) -> Result(IncomingMessage, DecodeError) {
  // Try to get request_id from response first, then top level
  let request_id = get_request_id(raw, response)
  case request_id {
    None -> Error(MissingField("request_id"))
    Some(req_id) -> {
      // Get the error message - try "error" field first, then "message" for tolerance
      let error_msg = get_error_message(response)
      case error_msg {
        None -> Error(MissingField("response.error or response.message"))
        Some(msg) ->
          Ok(ControlResponse(ControlError(request_id: req_id, message: msg)))
      }
    }
  }
}

/// Helper to get error message from "error" or "message" field (tolerant).
fn get_error_message(response: Dynamic) -> Option(String) {
  let error_decoder = {
    use error <- decode.field("error", decode.string)
    decode.success(error)
  }
  case decode.run(response, error_decoder) {
    Ok(msg) -> Some(msg)
    Error(_) -> {
      // Try "message" field as fallback
      let message_decoder = {
        use msg <- decode.field("message", decode.string)
        decode.success(msg)
      }
      case decode.run(response, message_decoder) {
        Ok(msg) -> Some(msg)
        Error(_) -> None
      }
    }
  }
}

/// Helper to get request_id from response object first, then top level.
fn get_request_id(raw: Dynamic, response: Dynamic) -> Option(String) {
  let req_id_decoder = {
    use req_id <- decode.field("request_id", decode.string)
    decode.success(req_id)
  }
  // Try response first
  case decode.run(response, req_id_decoder) {
    Ok(id) -> Some(id)
    Error(_) -> {
      // Fall back to top level
      case decode.run(raw, req_id_decoder) {
        Ok(id) -> Some(id)
        Error(_) -> None
      }
    }
  }
}

/// Decode regular messages by delegating to existing decoder.
fn decode_regular_message(raw: Dynamic) -> Result(IncomingMessage, DecodeError) {
  // Determine the type and call the appropriate decoder function
  let type_decoder = {
    use msg_type <- decode.field("type", decode.string)
    decode.success(msg_type)
  }
  case decode.run(raw, type_decoder) {
    Error(_) -> Error(MissingField("type"))
    Ok(msg_type) -> {
      let result = case msg_type {
        "system" -> decoder.decode_system_message(raw)
        "assistant" -> decoder.decode_assistant_message(raw)
        "user" -> decoder.decode_user_message(raw)
        "result" -> decoder.decode_result_message(raw)
        _ -> Error(decoder.UnexpectedMessageType(msg_type))
      }
      case result {
        Ok(msg) -> Ok(RegularMessage(msg))
        Error(e) -> {
          case e {
            decoder.JsonSyntaxError(s) -> Error(JsonError(s))
            decoder.UnexpectedMessageType(t) -> Error(InvalidType(t))
            decoder.JsonDecodeError(s) -> Error(MissingField(s))
          }
        }
      }
    }
  }
}
