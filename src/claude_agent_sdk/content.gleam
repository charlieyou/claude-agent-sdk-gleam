/// Content block types for Claude Agent SDK messages.
///
/// This module defines the content block types that appear within messages.
/// Assistant messages contain `ContentBlock` items (text and tool use),
/// while user messages contain `ToolResultBlock` items (tool execution output).
///
/// ## Forward Compatibility
///
/// Unknown block types are preserved as `UnknownBlock(raw)` to ensure the SDK
/// can handle new content types added in future CLI versions without breaking.
///
/// ## Pattern Matching
///
/// Import this module for pattern matching on content blocks:
///
/// ```gleam
/// import claude_agent_sdk/content
///
/// case block {
///   content.TextBlock(text) -> // handle text
///   content.ToolUseBlock(id, name, input) -> // handle tool use
///   content.ThinkingBlock(thinking, signature) -> // handle thinking
///   content.UnknownBlock(raw) -> // forward compatibility
/// }
/// ```
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

/// Content block types for assistant messages.
///
/// Represents the different types of content Claude can produce in a response.
/// Uses discriminated union pattern for type-safe content handling.
pub type ContentBlock {
  /// Text content block containing Claude's response text.
  TextBlock(text: String)
  /// Tool use request block.
  /// - `id`: Unique identifier for this tool use (reference in tool results)
  /// - `name`: Tool name (e.g., "Bash", "Read", "Write")
  /// - `input`: Tool-specific input parameters as Dynamic JSON
  ToolUseBlock(id: String, name: String, input: Dynamic)
  /// Thinking block containing Claude's extended thinking.
  /// - `thinking`: The thinking content
  /// - `signature`: Optional signature for thinking block verification
  ThinkingBlock(thinking: String, signature: Option(String))
  /// Unknown block type for forward compatibility.
  /// Preserves raw Dynamic data for block types added in future CLI versions.
  UnknownBlock(raw: Dynamic)
}

/// Tool result block for user messages.
///
/// Contains the output from a tool execution. These blocks appear in user
/// messages as responses to `ToolUseBlock` requests from Claude.
pub type ToolResultBlock {
  ToolResultBlock(
    /// Tool use ID this result responds to (matches `ToolUseBlock.id`).
    tool_use_id: String,
    /// Result content (tool output as text).
    content: String,
    /// Whether the tool execution failed.
    /// `Some(True)` indicates an error; `Some(False)` or `None` indicates success.
    is_error: Option(Bool),
  )
}
