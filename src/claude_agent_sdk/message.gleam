/// Message type definitions for Claude Agent SDK.
///
/// This module defines all message types that are populated by JSON decoders.
/// All optional fields use Option wrapping per the decoder policy.
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

import claude_agent_sdk/content.{type ToolResultBlock}

// =============================================================================
// Message Discriminated Union
// =============================================================================

/// Top-level message type (discriminated union).
/// Messages are distinguished by their "type" field in the JSON.
pub type Message {
  /// System message - emitted once at session start
  System(SystemMessage)
  /// Assistant message - Claude's response with text and/or tool use
  Assistant(AssistantMessage)
  /// User message - tool results returned to Claude
  User(UserMessage)
  /// Result message - final message indicating query completion
  Result(ResultMessage)
}

// =============================================================================
// Message Envelope
// =============================================================================

/// Wrapper that preserves raw JSON alongside parsed message.
/// Useful for debugging, logging, and forward compatibility.
pub type MessageEnvelope {
  MessageEnvelope(
    /// The parsed message
    message: Message,
    /// Original JSON string
    raw_json: String,
    /// Original bytes (for binary-safe handling)
    raw_bytes: BitArray,
  )
}

// =============================================================================
// System Message
// =============================================================================

/// System message emitted once at session start.
/// All fields except type are optional per decoder policy.
pub type SystemMessage {
  SystemMessage(
    /// Always "init" for session start
    subtype: Option(String),
    /// Unique message identifier
    uuid: Option(String),
    /// Session identifier (UUID)
    session_id: Option(String),
    /// Working directory
    cwd: Option(String),
    /// Active model name
    model: Option(String),
    /// Available built-in tools
    tools: Option(List(String)),
    /// Connected MCP servers with status
    mcp_servers: Option(List(McpServerStatus)),
    /// Current permission mode
    permission_mode: Option(String),
    /// Authentication source
    api_key_source: Option(String),
    /// Available slash commands
    slash_commands: Option(List(String)),
    /// Available subagent types
    agents: Option(List(String)),
    /// CLI version
    claude_code_version: Option(String),
  )
}

// =============================================================================
// Assistant Message
// =============================================================================

/// Assistant message - Claude's response.
pub type AssistantMessage {
  AssistantMessage(
    /// Unique message identifier
    uuid: Option(String),
    /// Session identifier
    session_id: Option(String),
    /// Parent tool use ID (null if top-level)
    parent_tool_use_id: Option(String),
    /// The message content
    message: Option(AssistantMessageContent),
  )
}

/// Content of an assistant message.
pub type AssistantMessageContent {
  AssistantMessageContent(
    /// Model used
    model: Option(String),
    /// Message ID from API
    id: Option(String),
    /// Always "message"
    message_type: Option(String),
    /// Always "assistant"
    role: Option(String),
    /// Content blocks (text, tool_use, etc.)
    content: Option(List(ContentBlock)),
    /// Why the response stopped
    stop_reason: Option(String),
    /// Token usage
    usage: Option(Usage),
  )
}

// =============================================================================
// Content Blocks
// =============================================================================

/// Content block types in assistant messages.
pub type ContentBlock {
  /// Text block
  TextBlock(text: String)
  /// Tool use block
  ToolUseBlock(id: String, name: String, input: Dynamic)
  /// Unknown block type (forward compatibility)
  UnknownBlock(raw: Dynamic)
}

// =============================================================================
// User Message
// =============================================================================

/// User message - tool results returned to Claude.
pub type UserMessage {
  UserMessage(
    /// Unique message identifier
    uuid: Option(String),
    /// Session identifier
    session_id: Option(String),
    /// Parent tool use ID
    parent_tool_use_id: Option(String),
    /// The message content
    message: Option(UserMessageContent),
    /// Tool use result metadata
    tool_use_result: Option(Dynamic),
  )
}

/// Content of a user message.
pub type UserMessageContent {
  UserMessageContent(
    /// Always "user"
    role: Option(String),
    /// Content blocks (tool_result, etc.)
    content: Option(List(ToolResultBlock)),
  )
}

// =============================================================================
// Result Message
// =============================================================================

/// Result message - final message indicating query completion.
pub type ResultMessage {
  ResultMessage(
    /// Result subtype (success, error_max_turns, etc.)
    subtype: Option(ResultSubtype),
    /// Unique message identifier
    uuid: Option(String),
    /// Session identifier
    session_id: Option(String),
    /// Whether the query ended in error
    is_error: Option(Bool),
    /// Total wall-clock time in ms
    duration_ms: Option(Int),
    /// Time spent in API calls in ms
    duration_api_ms: Option(Int),
    /// Number of conversation turns
    num_turns: Option(Int),
    /// Final text result
    result: Option(String),
    /// Total cost in USD
    total_cost_usd: Option(Float),
    /// Aggregate token usage
    usage: Option(Usage),
    /// Per-model usage breakdown
    model_usage: Option(Dynamic),
    /// Tools that were denied permission
    permission_denials: Option(List(PermissionDenial)),
    /// Parsed JSON if using structured output
    structured_output: Option(Dynamic),
    /// Error messages (if is_error: true)
    errors: Option(List(String)),
  )
}

/// Result subtypes indicating how the query completed.
pub type ResultSubtype {
  /// Query completed successfully
  Success
  /// Exceeded maximum turns
  ErrorMaxTurns
  /// Runtime error occurred
  ErrorDuringExecution
  /// Exceeded budget limit
  ErrorMaxBudget
  /// Unknown subtype (forward compatibility)
  UnknownSubtype(String)
}

// =============================================================================
// Supporting Types
// =============================================================================

/// Token usage statistics.
pub type Usage {
  Usage(
    /// Input tokens used
    input_tokens: Option(Int),
    /// Output tokens generated
    output_tokens: Option(Int),
    /// Tokens used for cache creation
    cache_creation_input_tokens: Option(Int),
    /// Tokens read from cache
    cache_read_input_tokens: Option(Int),
  )
}

/// MCP server connection status.
pub type McpServerStatus {
  McpServerStatus(
    /// Server name
    name: String,
    /// Connection status
    status: String,
  )
}

/// Permission denial record.
pub type PermissionDenial {
  PermissionDenial(
    /// Tool that was denied
    tool: String,
    /// Reason for denial
    reason: Option(String),
  )
}
