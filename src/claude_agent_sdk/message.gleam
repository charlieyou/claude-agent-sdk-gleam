/// Message type definitions for Claude Agent SDK.
///
/// This module defines all message types emitted by the Claude CLI during a
/// conversation. Messages are parsed from JSON-lines output and wrapped in
/// `MessageEnvelope` for delivery via `stream.next()`.
///
/// ## Message Flow
///
/// A typical conversation produces messages in this order:
/// 1. `System` (once at session start, subtype: "init")
/// 2. `Assistant` (Claude's response with text and/or tool use)
/// 3. `User` (tool results returned to Claude, if tools were used)
/// 4. Repeat steps 2-3 for each turn
/// 5. `Result` (final message indicating query completion)
///
/// ## Optional Fields
///
/// All optional fields use `Option` wrapping per SDK policy. The CLI may
/// omit fields in some contexts, so always handle `None` cases.
///
/// ## Pattern Matching
///
/// Import this module for pattern matching on message variants:
///
/// ```gleam
/// import claude_agent_sdk/message
///
/// case envelope.message {
///   message.System(sys) -> // handle system message
///   message.Assistant(asst) -> // handle assistant response
///   message.User(user) -> // handle tool results
///   message.Result(res) -> // handle completion
/// }
/// ```
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

import claude_agent_sdk/content.{type ContentBlock, type ToolResultBlock}

// =============================================================================
// Message Discriminated Union
// =============================================================================

/// Top-level message type (discriminated union).
///
/// Messages are distinguished by their "type" field in the JSON. Each variant
/// wraps a specific message type with its own fields.
///
/// See individual message types for field documentation.
pub type Message {
  /// System message - emitted once at session start.
  /// Contains session metadata: ID, model, tools, MCP servers, etc.
  System(SystemMessage)
  /// Assistant message - Claude's response.
  /// Contains content blocks (text, tool use requests).
  Assistant(AssistantMessage)
  /// User message - tool results returned to Claude.
  /// Contains tool result blocks with execution output.
  User(UserMessage)
  /// Result message - final message indicating query completion.
  /// Contains final result text, usage stats, and completion reason.
  Result(ResultMessage)
}

// =============================================================================
// Message Envelope
// =============================================================================

/// Wrapper that preserves raw JSON alongside parsed message.
///
/// The envelope provides access to both the parsed `Message` and the original
/// JSON data. This is useful for:
/// - Debugging parse issues
/// - Logging raw CLI output
/// - Forward compatibility with new fields not yet parsed
pub type MessageEnvelope {
  MessageEnvelope(
    /// The parsed message.
    message: Message,
    /// Original JSON string (UTF-8 decoded).
    raw_json: String,
    /// Original bytes for binary-safe handling.
    /// Use when the JSON might contain non-UTF-8 data.
    raw_bytes: BitArray,
  )
}

// =============================================================================
// System Message
// =============================================================================

/// System message emitted once at session start.
///
/// Contains session metadata including the session ID, active model, available
/// tools, and connected MCP servers. This is always the first message in a stream.
///
/// All fields are optional per SDK policy; the CLI may omit fields.
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
///
/// Contains Claude's response content including text blocks and tool use requests.
/// Multiple assistant messages may be emitted per turn if Claude uses tools.
pub type AssistantMessage {
  AssistantMessage(
    /// Unique message identifier.
    uuid: Option(String),
    /// Session identifier (matches `SystemMessage.session_id`).
    session_id: Option(String),
    /// Parent tool use ID if this is a subagent response; `None` if top-level.
    parent_tool_use_id: Option(String),
    /// The message content with text and/or tool use blocks.
    message: Option(AssistantMessageContent),
  )
}

/// Content of an assistant message.
///
/// Contains the actual response data including content blocks, stop reason,
/// and token usage for this specific response.
pub type AssistantMessageContent {
  AssistantMessageContent(
    /// Model used for this response (e.g., "claude-sonnet-4-20250514").
    model: Option(String),
    /// Message ID from the Anthropic API.
    id: Option(String),
    /// Always "message".
    message_type: Option(String),
    /// Always "assistant".
    role: Option(String),
    /// Content blocks: text, tool_use, or unknown.
    /// See `claude_agent_sdk/content` for block types.
    content: Option(List(ContentBlock)),
    /// Why the response stopped: "end_turn", "tool_use", "max_tokens", etc.
    stop_reason: Option(String),
    /// Token usage for this response.
    usage: Option(Usage),
  )
}

// =============================================================================
// User Message
// =============================================================================

/// User message - tool results returned to Claude.
///
/// Represents tool execution results that are sent back to Claude after
/// it requested a tool use. The CLI generates these automatically.
pub type UserMessage {
  UserMessage(
    /// Unique message identifier.
    uuid: Option(String),
    /// Session identifier.
    session_id: Option(String),
    /// Parent tool use ID that this result responds to.
    parent_tool_use_id: Option(String),
    /// The message content with tool result blocks.
    message: Option(UserMessageContent),
    /// Additional tool use result metadata (varies by tool).
    tool_use_result: Option(Dynamic),
  )
}

/// Content of a user message.
///
/// Contains tool result blocks with the output of executed tools.
pub type UserMessageContent {
  UserMessageContent(
    /// Always "user".
    role: Option(String),
    /// Tool result blocks with execution output.
    /// See `claude_agent_sdk/content.ToolResultBlock`.
    content: Option(List(ToolResultBlock)),
  )
}

// =============================================================================
// Result Message
// =============================================================================

/// Result message - final message indicating query completion.
///
/// Always the last message in a stream. Contains the final result text,
/// usage statistics, timing information, and the completion reason (subtype).
pub type ResultMessage {
  ResultMessage(
    /// How the query completed: success, error_max_turns, etc.
    subtype: Option(ResultSubtype),
    /// Unique message identifier.
    uuid: Option(String),
    /// Session identifier (for resuming this session later).
    session_id: Option(String),
    /// Whether the query ended in error.
    is_error: Option(Bool),
    /// Total wall-clock time in milliseconds.
    duration_ms: Option(Int),
    /// Time spent waiting for API responses in milliseconds.
    duration_api_ms: Option(Int),
    /// Number of conversation turns completed.
    num_turns: Option(Int),
    /// Final text result from Claude.
    result: Option(String),
    /// Total cost in USD for this query.
    total_cost_usd: Option(Float),
    /// Aggregate token usage across all turns.
    usage: Option(Usage),
    /// Per-model usage breakdown (for multi-model queries).
    model_usage: Option(Dynamic),
    /// Tools that were denied permission during execution.
    permission_denials: Option(List(PermissionDenial)),
    /// Parsed JSON if using structured output schema.
    structured_output: Option(Dynamic),
    /// Error messages if `is_error` is `True`.
    errors: Option(List(String)),
  )
}

/// Result subtypes indicating how the query completed.
///
/// Describes the final outcome of the query. Check this to determine if
/// the query succeeded or why it stopped.
pub type ResultSubtype {
  /// Query completed successfully.
  Success
  /// Exceeded maximum turns limit (`max_turns` option).
  ErrorMaxTurns
  /// Runtime error occurred during execution.
  ErrorDuringExecution
  /// Exceeded budget limit (`max_budget_usd` option).
  ErrorMaxBudget
  /// Unknown subtype for forward compatibility.
  /// Contains the raw subtype string from the CLI.
  UnknownSubtype(String)
}

// =============================================================================
// Supporting Types
// =============================================================================

/// Token usage statistics.
///
/// Tracks input and output tokens for billing and monitoring.
/// Available in both per-response (`AssistantMessageContent.usage`) and
/// aggregate (`ResultMessage.usage`) forms.
pub type Usage {
  Usage(
    /// Input tokens used (prompt tokens).
    input_tokens: Option(Int),
    /// Output tokens generated (completion tokens).
    output_tokens: Option(Int),
    /// Tokens used for cache creation (prompt caching).
    cache_creation_input_tokens: Option(Int),
    /// Tokens read from cache (prompt caching).
    cache_read_input_tokens: Option(Int),
  )
}

/// MCP server connection status.
///
/// Reported in the system message to indicate which MCP servers
/// are available for tool use.
pub type McpServerStatus {
  McpServerStatus(
    /// Server name as configured in MCP config.
    name: String,
    /// Connection status: "connected", "error", etc.
    status: String,
  )
}

/// Permission denial record.
///
/// Reported in the result message when tools are denied permission
/// during execution. Useful for understanding why certain operations failed.
pub type PermissionDenial {
  PermissionDenial(
    /// Name of the tool that was denied (e.g., "Bash", "Write").
    tool_name: String,
    /// Tool use ID of the denied request.
    tool_use_id: String,
    /// Input that was passed to the tool.
    tool_input: Dynamic,
  )
}
