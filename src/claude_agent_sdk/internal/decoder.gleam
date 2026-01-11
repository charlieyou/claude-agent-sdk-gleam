/// JSON decoders for Claude Agent SDK message types.
///
/// This module provides decoders for parsing NDJSON messages from the Claude CLI.
/// All decoders follow the forward-compatibility policy:
/// - Unknown message types yield UnexpectedMessageError
/// - Unknown content block types yield UnknownBlock(raw)
/// - Unknown JSON fields are silently ignored
import claude_agent_sdk/content.{
  type ContentBlock, type ToolResultBlock, TextBlock, ToolResultBlock,
  ToolUseBlock, UnknownBlock,
}
import claude_agent_sdk/message.{
  type McpServerStatus, type PermissionDenial, type ResultSubtype, type Usage,
  ErrorDuringExecution, ErrorMaxBudget, ErrorMaxTurns, McpServerStatus,
  PermissionDenial, Success, UnknownSubtype, Usage,
}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
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

/// Decode a JSON string into a message type.
/// Returns DecodeError for invalid JSON, unknown types, or missing required fields.
pub fn decode_message(_json_string: String) -> Result(Dynamic, DecodeError) {
  // Skeleton: always fails until implemented
  Error(JsonDecodeError("decoder not implemented"))
}

/// Decode a system message from Dynamic.
pub fn decode_system_message(_raw: Dynamic) -> Result(Dynamic, DecodeError) {
  Error(JsonDecodeError("decode_system_message not implemented"))
}

/// Decode an assistant message from Dynamic.
pub fn decode_assistant_message(_raw: Dynamic) -> Result(Dynamic, DecodeError) {
  Error(JsonDecodeError("decode_assistant_message not implemented"))
}

/// Decode a user message from Dynamic.
pub fn decode_user_message(_raw: Dynamic) -> Result(Dynamic, DecodeError) {
  Error(JsonDecodeError("decode_user_message not implemented"))
}

/// Decode a result message from Dynamic.
pub fn decode_result_message(_raw: Dynamic) -> Result(Dynamic, DecodeError) {
  Error(JsonDecodeError("decode_result_message not implemented"))
}

/// Decode content blocks from an assistant message.
/// Returns a list of ContentBlocks, with unknown types preserved as UnknownBlock.
pub fn decode_content_blocks(
  raw: Dynamic,
) -> Result(List(ContentBlock), DecodeError) {
  let list_decoder = decode.list(decode.dynamic)
  case decode.run(raw, list_decoder) {
    Ok(dynamics) -> {
      let blocks = list.map(dynamics, decode_content_block)
      Ok(blocks)
    }
    Error(errors) -> Error(JsonDecodeError(format_decode_errors(errors)))
  }
}

/// Decode a single content block from Dynamic.
/// Routes to appropriate decoder based on "type" field.
/// Unknown types yield UnknownBlock(raw) for forward compatibility.
pub fn decode_content_block(raw: Dynamic) -> ContentBlock {
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
        _ -> UnknownBlock(raw)
      }
    }
    Error(_) -> UnknownBlock(raw)
  }
}

/// Decode a TextBlock from Dynamic.
/// Returns UnknownBlock if required fields are missing.
fn decode_text_block_inner(raw: Dynamic) -> ContentBlock {
  let decoder = {
    use text <- decode.field("text", decode.string)
    decode.success(text)
  }
  case decode.run(raw, decoder) {
    Ok(text) -> TextBlock(text)
    Error(_) -> UnknownBlock(raw)
  }
}

/// Decode a ToolUseBlock from Dynamic.
/// Returns UnknownBlock if required fields are missing.
fn decode_tool_use_block_inner(raw: Dynamic) -> ContentBlock {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use name <- decode.field("name", decode.string)
    use input <- decode.field("input", decode.dynamic)
    decode.success(#(id, name, input))
  }
  case decode.run(raw, decoder) {
    Ok(#(id, name, input)) -> ToolUseBlock(id:, name:, input:)
    Error(_) -> UnknownBlock(raw)
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
    use tool <- decode.field("tool", decode.string)
    use reason <- decode.optional_field(
      "reason",
      None,
      decode.string |> decode.map(Some),
    )
    decode.success(PermissionDenial(tool:, reason:))
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
