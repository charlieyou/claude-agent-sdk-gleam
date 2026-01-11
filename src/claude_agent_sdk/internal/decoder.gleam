/// JSON decoders for Claude Agent SDK message types.
///
/// This module provides decoders for parsing NDJSON messages from the Claude CLI.
/// All decoders follow the forward-compatibility policy:
/// - Unknown message types yield UnexpectedMessageError
/// - Unknown content block types yield UnknownBlock(raw)
/// - Unknown JSON fields are silently ignored
import gleam/dynamic.{type Dynamic}

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
pub fn decode_content_blocks(_raw: Dynamic) -> Result(Dynamic, DecodeError) {
  Error(JsonDecodeError("decode_content_blocks not implemented"))
}
