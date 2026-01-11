/// JSON decoders for Claude Agent SDK message types.
///
/// This module provides decoders for parsing NDJSON messages from the Claude CLI.
/// All decoders follow the forward-compatibility policy:
/// - Unknown message types yield UnexpectedMessageError
/// - Unknown content block types yield UnknownBlock(raw) for forward compatibility
/// - Known block types with missing required fields yield decode error
/// - Unknown JSON fields are silently ignored
///
/// ## Content Block Decoding
/// Use `decode_content_block` for general content block decoding. It returns:
/// - Ok(ContentBlock) for valid known types (TextBlock, ToolUseBlock, etc.)
/// - Ok(UnknownBlock(raw)) for unknown type values (forward compatibility)
/// - Error for known types missing required fields
///
/// For type-specific decoding with explicit errors, use:
/// - `decode_text_block` - validates type is "text" and text field exists
/// - `decode_tool_use_block` - validates type is "tool_use" and id/name/input exist
import claude_agent_sdk/content.{
  type ContentBlock, type ToolResultBlock, TextBlock, ToolResultBlock,
  ToolUseBlock, UnknownBlock,
}
import claude_agent_sdk/message.{
  type AssistantMessageContent, type McpServerStatus, type Message,
  type PermissionDenial, type ResultSubtype, type Usage, type UserMessageContent,
  Assistant, AssistantMessage, AssistantMessageContent, ErrorDuringExecution,
  ErrorMaxBudget, ErrorMaxTurns, McpServerStatus, PermissionDenial, Result,
  ResultMessage, Success, System, SystemMessage, UnknownSubtype, Usage, User,
  UserMessage, UserMessageContent,
}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{None, Some}

/// Decode error types
pub type DecodeError {
  /// JSON syntax error
  JsonSyntaxError(String)
  /// Valid JSON but unknown message type
  UnexpectedMessageType(String)
  /// Valid JSON but missing/invalid required fields
  JsonDecodeError(String)
}

/// Decode a JSON string into a Message type.
/// Returns DecodeError for invalid JSON, unknown types, or missing required fields.
pub fn decode_message(json_string: String) -> Result(Message, DecodeError) {
  // First parse JSON string to Dynamic
  case json.parse(json_string, decode.dynamic) {
    Error(_) -> Error(JsonSyntaxError("Invalid JSON"))
    Ok(raw) -> {
      // Get the type field to dispatch to correct decoder
      let type_decoder = {
        use msg_type <- decode.field("type", decode.string)
        decode.success(msg_type)
      }
      case decode.run(raw, type_decoder) {
        Error(_) -> Error(JsonDecodeError("Missing required field: type"))
        Ok(msg_type) -> {
          case msg_type {
            "system" -> decode_system_message(raw)
            "assistant" -> decode_assistant_message(raw)
            "user" -> decode_user_message(raw)
            "result" -> decode_result_message(raw)
            unknown -> Error(UnexpectedMessageType(unknown))
          }
        }
      }
    }
  }
}

/// Decode a system message from Dynamic.
pub fn decode_system_message(raw: Dynamic) -> Result(Message, DecodeError) {
  let decoder = {
    use subtype <- decode.optional_field(
      "subtype",
      None,
      decode.string |> decode.map(Some),
    )
    use uuid <- decode.optional_field(
      "uuid",
      None,
      decode.string |> decode.map(Some),
    )
    use session_id <- decode.optional_field(
      "session_id",
      None,
      decode.string |> decode.map(Some),
    )
    use cwd <- decode.optional_field(
      "cwd",
      None,
      decode.string |> decode.map(Some),
    )
    use model <- decode.optional_field(
      "model",
      None,
      decode.string |> decode.map(Some),
    )
    use tools <- decode.optional_field(
      "tools",
      None,
      decode.list(decode.string) |> decode.map(Some),
    )
    use mcp_servers <- decode.optional_field(
      "mcp_servers",
      None,
      decode.list(mcp_server_status_decoder()) |> decode.map(Some),
    )
    use permission_mode <- decode.optional_field(
      "permissionMode",
      None,
      decode.string |> decode.map(Some),
    )
    use api_key_source <- decode.optional_field(
      "apiKeySource",
      None,
      decode.string |> decode.map(Some),
    )
    use slash_commands <- decode.optional_field(
      "slash_commands",
      None,
      decode.list(decode.string) |> decode.map(Some),
    )
    use agents <- decode.optional_field(
      "agents",
      None,
      decode.list(decode.string) |> decode.map(Some),
    )
    use claude_code_version <- decode.optional_field(
      "claude_code_version",
      None,
      decode.string |> decode.map(Some),
    )
    decode.success(SystemMessage(
      subtype:,
      uuid:,
      session_id:,
      cwd:,
      model:,
      tools:,
      mcp_servers:,
      permission_mode:,
      api_key_source:,
      slash_commands:,
      agents:,
      claude_code_version:,
    ))
  }
  case decode.run(raw, decoder) {
    Ok(msg) -> Ok(System(msg))
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

/// Decode an assistant message from Dynamic.
pub fn decode_assistant_message(raw: Dynamic) -> Result(Message, DecodeError) {
  let decoder = {
    use uuid <- decode.optional_field(
      "uuid",
      None,
      decode.string |> decode.map(Some),
    )
    use session_id <- decode.optional_field(
      "session_id",
      None,
      decode.string |> decode.map(Some),
    )
    use parent_tool_use_id <- decode.optional_field(
      "parent_tool_use_id",
      None,
      decode.string |> decode.map(Some),
    )
    use message <- decode.optional_field(
      "message",
      None,
      assistant_message_content_decoder() |> decode.map(Some),
    )
    decode.success(AssistantMessage(
      uuid:,
      session_id:,
      parent_tool_use_id:,
      message:,
    ))
  }
  case decode.run(raw, decoder) {
    Ok(msg) -> Ok(Assistant(msg))
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

/// Decode a user message from Dynamic.
pub fn decode_user_message(raw: Dynamic) -> Result(Message, DecodeError) {
  let decoder = {
    use uuid <- decode.optional_field(
      "uuid",
      None,
      decode.string |> decode.map(Some),
    )
    use session_id <- decode.optional_field(
      "session_id",
      None,
      decode.string |> decode.map(Some),
    )
    use parent_tool_use_id <- decode.optional_field(
      "parent_tool_use_id",
      None,
      decode.string |> decode.map(Some),
    )
    use message <- decode.optional_field(
      "message",
      None,
      user_message_content_decoder() |> decode.map(Some),
    )
    use tool_use_result <- decode.optional_field(
      "tool_use_result",
      None,
      decode.dynamic |> decode.map(Some),
    )
    decode.success(UserMessage(
      uuid:,
      session_id:,
      parent_tool_use_id:,
      message:,
      tool_use_result:,
    ))
  }
  case decode.run(raw, decoder) {
    Ok(msg) -> Ok(User(msg))
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

/// Decode a result message from Dynamic.
pub fn decode_result_message(raw: Dynamic) -> Result(Message, DecodeError) {
  let decoder = {
    use subtype_str <- decode.optional_field(
      "subtype",
      None,
      decode.string |> decode.map(Some),
    )
    use uuid <- decode.optional_field(
      "uuid",
      None,
      decode.string |> decode.map(Some),
    )
    use session_id <- decode.optional_field(
      "session_id",
      None,
      decode.string |> decode.map(Some),
    )
    use is_error <- decode.optional_field(
      "is_error",
      None,
      decode.bool |> decode.map(Some),
    )
    use duration_ms <- decode.optional_field(
      "duration_ms",
      None,
      decode.int |> decode.map(Some),
    )
    use duration_api_ms <- decode.optional_field(
      "duration_api_ms",
      None,
      decode.int |> decode.map(Some),
    )
    use num_turns <- decode.optional_field(
      "num_turns",
      None,
      decode.int |> decode.map(Some),
    )
    use result <- decode.optional_field(
      "result",
      None,
      decode.string |> decode.map(Some),
    )
    use total_cost_usd <- decode.optional_field(
      "total_cost_usd",
      None,
      decode.float |> decode.map(Some),
    )
    use usage <- decode.optional_field(
      "usage",
      None,
      usage_decoder() |> decode.map(Some),
    )
    use model_usage <- decode.optional_field(
      "modelUsage",
      None,
      decode.dynamic |> decode.map(Some),
    )
    use permission_denials <- decode.optional_field(
      "permission_denials",
      None,
      decode.list(permission_denial_decoder()) |> decode.map(Some),
    )
    use structured_output <- decode.optional_field(
      "structured_output",
      None,
      decode.dynamic |> decode.map(Some),
    )
    use errors <- decode.optional_field(
      "errors",
      None,
      decode.list(decode.string) |> decode.map(Some),
    )
    // Convert subtype string to ResultSubtype
    let subtype = case subtype_str {
      Some(s) -> Some(decode_result_subtype(s))
      None -> None
    }
    decode.success(ResultMessage(
      subtype:,
      uuid:,
      session_id:,
      is_error:,
      duration_ms:,
      duration_api_ms:,
      num_turns:,
      result:,
      total_cost_usd:,
      usage:,
      model_usage:,
      permission_denials:,
      structured_output:,
      errors:,
    ))
  }
  case decode.run(raw, decoder) {
    Ok(msg) -> Ok(Result(msg))
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

// =============================================================================
// Internal Decoders
// =============================================================================

/// Decoder for McpServerStatus
fn mcp_server_status_decoder() -> decode.Decoder(McpServerStatus) {
  use name <- decode.field("name", decode.string)
  use status <- decode.field("status", decode.string)
  decode.success(McpServerStatus(name:, status:))
}

/// Decoder for AssistantMessageContent
fn assistant_message_content_decoder() -> decode.Decoder(
  AssistantMessageContent,
) {
  use model <- decode.optional_field(
    "model",
    None,
    decode.string |> decode.map(Some),
  )
  use id <- decode.optional_field("id", None, decode.string |> decode.map(Some))
  use message_type <- decode.optional_field(
    "type",
    None,
    decode.string |> decode.map(Some),
  )
  use role <- decode.optional_field(
    "role",
    None,
    decode.string |> decode.map(Some),
  )
  use content <- decode.optional_field(
    "content",
    None,
    content_blocks_decoder() |> decode.map(Some),
  )
  use stop_reason <- decode.optional_field(
    "stop_reason",
    None,
    decode.string |> decode.map(Some),
  )
  use usage <- decode.optional_field(
    "usage",
    None,
    usage_decoder() |> decode.map(Some),
  )
  decode.success(AssistantMessageContent(
    model:,
    id:,
    message_type:,
    role:,
    content:,
    stop_reason:,
    usage:,
  ))
}

/// Decoder for UserMessageContent
fn user_message_content_decoder() -> decode.Decoder(UserMessageContent) {
  use role <- decode.optional_field(
    "role",
    None,
    decode.string |> decode.map(Some),
  )
  use content <- decode.optional_field(
    "content",
    None,
    tool_result_blocks_decoder() |> decode.map(Some),
  )
  decode.success(UserMessageContent(role:, content:))
}

/// Decoder for Usage
fn usage_decoder() -> decode.Decoder(Usage) {
  use input_tokens <- decode.optional_field(
    "input_tokens",
    None,
    decode.int |> decode.map(Some),
  )
  use output_tokens <- decode.optional_field(
    "output_tokens",
    None,
    decode.int |> decode.map(Some),
  )
  use cache_creation_input_tokens <- decode.optional_field(
    "cache_creation_input_tokens",
    None,
    decode.int |> decode.map(Some),
  )
  use cache_read_input_tokens <- decode.optional_field(
    "cache_read_input_tokens",
    None,
    decode.int |> decode.map(Some),
  )
  decode.success(Usage(
    input_tokens:,
    output_tokens:,
    cache_creation_input_tokens:,
    cache_read_input_tokens:,
  ))
}

/// Decoder for PermissionDenial
fn permission_denial_decoder() -> decode.Decoder(PermissionDenial) {
  use tool_name <- decode.field("tool_name", decode.string)
  use tool_use_id <- decode.field("tool_use_id", decode.string)
  use tool_input <- decode.field("tool_input", decode.dynamic)
  decode.success(PermissionDenial(tool_name:, tool_use_id:, tool_input:))
}

/// Decoder for content blocks list
fn content_blocks_decoder() -> decode.Decoder(List(ContentBlock)) {
  decode.list(content_block_decoder())
}

/// Decoder for a single content block
fn content_block_decoder() -> decode.Decoder(ContentBlock) {
  // First try to get the type field
  let type_decoder = {
    use block_type <- decode.field("type", decode.string)
    decode.success(block_type)
  }
  use type_result <- decode.then(decode.dynamic)
  case decode.run(type_result, type_decoder) {
    Ok(block_type) -> {
      case block_type {
        "text" -> text_block_inner_decoder(type_result)
        "tool_use" -> tool_use_block_inner_decoder(type_result)
        _ -> decode.success(UnknownBlock(type_result))
      }
    }
    Error(_) -> decode.success(UnknownBlock(type_result))
  }
}

/// Decoder for TextBlock (inner)
fn text_block_inner_decoder(raw: Dynamic) -> decode.Decoder(ContentBlock) {
  let decoder = {
    use text <- decode.field("text", decode.string)
    decode.success(TextBlock(text))
  }
  case decode.run(raw, decoder) {
    Ok(block) -> decode.success(block)
    Error(_) -> decode.success(UnknownBlock(raw))
  }
}

/// Decoder for ToolUseBlock (inner)
fn tool_use_block_inner_decoder(raw: Dynamic) -> decode.Decoder(ContentBlock) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use name <- decode.field("name", decode.string)
    use input <- decode.field("input", decode.dynamic)
    decode.success(ToolUseBlock(id:, name:, input:))
  }
  case decode.run(raw, decoder) {
    Ok(block) -> decode.success(block)
    Error(_) -> decode.success(UnknownBlock(raw))
  }
}

/// Decoder for tool result blocks list
fn tool_result_blocks_decoder() -> decode.Decoder(List(ToolResultBlock)) {
  decode.list(tool_result_block_decoder())
}

/// Decoder for a single tool result block
fn tool_result_block_decoder() -> decode.Decoder(ToolResultBlock) {
  use tool_use_id <- decode.field("tool_use_id", decode.string)
  use content <- decode.field("content", decode.string)
  use is_error <- decode.optional_field(
    "is_error",
    None,
    decode.bool |> decode.map(Some),
  )
  decode.success(ToolResultBlock(tool_use_id:, content:, is_error:))
}

/// Decode content blocks from an assistant message.
/// Returns a list of ContentBlocks, with unknown types preserved as UnknownBlock.
/// Known types with missing required fields yield decode error.
pub fn decode_content_blocks(
  raw: Dynamic,
) -> Result(List(ContentBlock), DecodeError) {
  let list_decoder = decode.list(decode.dynamic)
  case decode.run(raw, list_decoder) {
    Ok(dynamics) -> decode_content_block_list(dynamics, [])
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

/// Helper to decode a list of content blocks, collecting results.
fn decode_content_block_list(
  dynamics: List(Dynamic),
  acc: List(ContentBlock),
) -> Result(List(ContentBlock), DecodeError) {
  case dynamics {
    [] -> Ok(list.reverse(acc))
    [first, ..rest] -> {
      case decode_content_block(first) {
        Ok(block) -> decode_content_block_list(rest, [block, ..acc])
        Error(e) -> Error(e)
      }
    }
  }
}

/// Decode a single content block from Dynamic.
/// Routes to appropriate decoder based on "type" field.
/// Unknown types yield Ok(UnknownBlock(raw)) for forward compatibility.
/// Known types with missing required fields yield Error.
pub fn decode_content_block(raw: Dynamic) -> Result(ContentBlock, DecodeError) {
  // First, try to get the type field
  let type_decoder = {
    use block_type <- decode.field("type", decode.string)
    decode.success(block_type)
  }
  case decode.run(raw, type_decoder) {
    Ok(block_type) -> {
      case block_type {
        "text" -> decode_text_block_inner(raw)
        "tool_use" -> decode_tool_use_block_inner(raw)
        // Unknown types yield UnknownBlock for forward compatibility
        _ -> Ok(UnknownBlock(raw))
      }
    }
    // Missing type field - treat as unknown block
    Error(_) -> Ok(UnknownBlock(raw))
  }
}

/// Decode a TextBlock from Dynamic (internal helper).
/// Returns Error if required "text" field is missing.
fn decode_text_block_inner(raw: Dynamic) -> Result(ContentBlock, DecodeError) {
  let decoder = {
    use text <- decode.field("text", decode.string)
    decode.success(text)
  }
  case decode.run(raw, decoder) {
    Ok(text) -> Ok(TextBlock(text))
    Error(errors) ->
      Error(JsonDecodeError(
        "TextBlock missing required field: " <> format_decode_errors(errors),
      ))
  }
}

/// Decode a ToolUseBlock from Dynamic (internal helper).
/// Returns Error if required id/name/input fields are missing.
fn decode_tool_use_block_inner(
  raw: Dynamic,
) -> Result(ContentBlock, DecodeError) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use name <- decode.field("name", decode.string)
    use input <- decode.field("input", decode.dynamic)
    decode.success(#(id, name, input))
  }
  case decode.run(raw, decoder) {
    Ok(#(id, name, input)) -> Ok(ToolUseBlock(id:, name:, input:))
    Error(errors) ->
      Error(JsonDecodeError(
        "ToolUseBlock missing required field: " <> format_decode_errors(errors),
      ))
  }
}

/// Decode a TextBlock, returning a Result for explicit error handling.
pub fn decode_text_block(raw: Dynamic) -> Result(ContentBlock, DecodeError) {
  let type_decoder = {
    use block_type <- decode.field("type", decode.string)
    decode.success(block_type)
  }
  case decode.run(raw, type_decoder) {
    Ok("text") -> {
      let text_decoder = {
        use text <- decode.field("text", decode.string)
        decode.success(text)
      }
      case decode.run(raw, text_decoder) {
        Ok(text) -> Ok(TextBlock(text))
        Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
      }
    }
    Ok(other) ->
      Error(JsonDecodeError("Expected type 'text', got '" <> other <> "'"))
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

/// Decode a ToolUseBlock, returning a Result for explicit error handling.
pub fn decode_tool_use_block(raw: Dynamic) -> Result(ContentBlock, DecodeError) {
  let type_decoder = {
    use block_type <- decode.field("type", decode.string)
    decode.success(block_type)
  }
  case decode.run(raw, type_decoder) {
    Ok("tool_use") -> {
      let decoder = {
        use id <- decode.field("id", decode.string)
        use name <- decode.field("name", decode.string)
        use input <- decode.field("input", decode.dynamic)
        decode.success(#(id, name, input))
      }
      case decode.run(raw, decoder) {
        Ok(#(id, name, input)) -> Ok(ToolUseBlock(id:, name:, input:))
        Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
      }
    }
    Ok(other) ->
      Error(JsonDecodeError("Expected type 'tool_use', got '" <> other <> "'"))
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

/// Decode a ToolResultBlock from Dynamic.
pub fn decode_tool_result_block(
  raw: Dynamic,
) -> Result(ToolResultBlock, DecodeError) {
  let decoder = {
    use tool_use_id <- decode.field("tool_use_id", decode.string)
    use content <- decode.field("content", decode.string)
    use is_error <- decode.optional_field(
      "is_error",
      None,
      decode.bool |> decode.map(Some),
    )
    decode.success(ToolResultBlock(tool_use_id:, content:, is_error:))
  }
  case decode.run(raw, decoder) {
    Ok(result) -> Ok(result)
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

// =============================================================================
// Supporting Type Decoders
// =============================================================================

/// Decode a ResultSubtype from a string value.
/// Returns UnknownSubtype for unrecognized values (forward compatibility).
pub fn decode_result_subtype(raw: String) -> ResultSubtype {
  case raw {
    "success" -> Success
    "error_max_turns" -> ErrorMaxTurns
    "error_during_execution" -> ErrorDuringExecution
    "error_max_budget" -> ErrorMaxBudget
    _ -> UnknownSubtype(raw)
  }
}

/// Decode Usage from Dynamic.
/// All fields are optional (may be absent in older CLI versions).
pub fn decode_usage(raw: Dynamic) -> Result(Usage, DecodeError) {
  let decoder = {
    use input_tokens <- decode.optional_field(
      "input_tokens",
      None,
      decode.int |> decode.map(Some),
    )
    use output_tokens <- decode.optional_field(
      "output_tokens",
      None,
      decode.int |> decode.map(Some),
    )
    use cache_creation_input_tokens <- decode.optional_field(
      "cache_creation_input_tokens",
      None,
      decode.int |> decode.map(Some),
    )
    use cache_read_input_tokens <- decode.optional_field(
      "cache_read_input_tokens",
      None,
      decode.int |> decode.map(Some),
    )
    decode.success(Usage(
      input_tokens:,
      output_tokens:,
      cache_creation_input_tokens:,
      cache_read_input_tokens:,
    ))
  }
  case decode.run(raw, decoder) {
    Ok(usage) -> Ok(usage)
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

/// Decode McpServerStatus from Dynamic.
pub fn decode_mcp_server_status(
  raw: Dynamic,
) -> Result(McpServerStatus, DecodeError) {
  let decoder = {
    use name <- decode.field("name", decode.string)
    use status <- decode.field("status", decode.string)
    decode.success(McpServerStatus(name:, status:))
  }
  case decode.run(raw, decoder) {
    Ok(mcp_status) -> Ok(mcp_status)
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

/// Decode PermissionDenial from Dynamic.
pub fn decode_permission_denial(
  raw: Dynamic,
) -> Result(PermissionDenial, DecodeError) {
  let decoder = {
    use tool_name <- decode.field("tool_name", decode.string)
    use tool_use_id <- decode.field("tool_use_id", decode.string)
    use tool_input <- decode.field("tool_input", decode.dynamic)
    decode.success(PermissionDenial(tool_name:, tool_use_id:, tool_input:))
  }
  case decode.run(raw, decoder) {
    Ok(denial) -> Ok(denial)
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

/// Format decode errors as a string.
fn format_decode_errors(errors: List(decode.DecodeError)) -> String {
  case errors {
    [] -> "Unknown decode error"
    [first, ..] -> decode_error_to_string(first)
  }
}

/// Convert a single DecodeError to a string.
fn decode_error_to_string(error: decode.DecodeError) -> String {
  case error {
    decode.DecodeError(expected, found, path) ->
      "Expected "
      <> expected
      <> " at "
      <> format_path(path)
      <> ", got "
      <> found
  }
}

/// Format a path list as a dot-separated string.
fn format_path(path: List(String)) -> String {
  case path {
    [] -> "root"
    segments -> join_path(segments, "")
  }
}

/// Join path segments with dots.
fn join_path(segments: List(String), acc: String) -> String {
  case segments {
    [] -> acc
    [first] ->
      case acc {
        "" -> first
        _ -> acc <> "." <> first
      }
    [first, ..rest] ->
      case acc {
        "" -> join_path(rest, first)
        _ -> join_path(rest, acc <> "." <> first)
      }
  }
}
