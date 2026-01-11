/// Content block types for Claude Agent SDK messages.
///
/// This module defines the discriminated union types for content blocks
/// that appear in assistant and user messages. Following forward-compatibility
/// policy, unknown block types are preserved as UnknownBlock(raw).
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

/// Content block types for assistant messages.
/// Uses discriminated union pattern for type-safe content handling.
pub type ContentBlock {
  /// Text content block
  TextBlock(text: String)
  /// Tool use request block
  ToolUseBlock(id: String, name: String, input: Dynamic)
  /// Unknown block type for forward compatibility
  /// Preserves raw Dynamic data for blocks added in future API versions
  UnknownBlock(raw: Dynamic)
}

/// Tool result block for user messages.
/// Separate type since tool results appear only in user message content.
pub type ToolResultBlock {
  ToolResultBlock(
    /// Tool use ID this result is for
    tool_use_id: String,
    /// Result content
    content: String,
    /// Whether the tool execution failed
    is_error: Option(Bool),
  )
}
