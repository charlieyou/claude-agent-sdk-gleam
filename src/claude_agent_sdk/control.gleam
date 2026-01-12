/// Control message types for bidirectional communication with Claude CLI.
///
/// This module defines the control protocol types used for advanced features
/// like hooks, permissions, MCP servers, and session management. These types
/// enable the SDK to send control requests to the CLI and receive control
/// responses/requests back.
///
/// ## Message Flow
///
/// ```
/// SDK ──OutgoingControlRequest──> CLI
/// SDK <──IncomingMessage──────── CLI
/// SDK ──OutgoingControlResponse─> CLI (when IncomingControlRequest received)
/// ```
///
/// ## Usage
///
/// Control messages are typically used internally by the SDK. Users interact
/// with control features through high-level APIs rather than these types directly.
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

import claude_agent_sdk/message.{type Message}

// =============================================================================
// Outgoing Control Requests (SDK → CLI)
// =============================================================================

/// Control requests sent from SDK to CLI.
///
/// These initiate control operations like session initialization, interruption,
/// permission mode changes, model switching, and file rewinding.
pub type OutgoingControlRequest {
  /// Initialize bidirectional control session.
  /// Sent once at startup to register hooks and configure the session.
  Initialize(
    request_id: String,
    hooks: List(HookRegistration),
    mcp_servers: List(String),
    enable_file_checkpointing: Bool,
  )
  /// Interrupt the current operation.
  /// Signals the CLI to stop processing and return control.
  Interrupt(request_id: String)
  /// Change permission mode mid-session.
  /// Allows dynamic permission escalation/restriction.
  SetPermissionMode(request_id: String, mode: PermissionMode)
  /// Switch to a different model mid-session.
  SetModel(request_id: String, model: String)
  /// Rewind file state to a previous message checkpoint.
  RewindFiles(request_id: String, user_message_id: String)
}

// =============================================================================
// Incoming Messages (CLI → SDK, decoder output)
// =============================================================================

/// Top-level incoming message type (output of decoder).
///
/// The decoder parses JSON lines from CLI and returns one of:
/// - `RegularMessage`: Standard message (system/assistant/user/result)
/// - `ControlRequest`: CLI-initiated request requiring SDK response
/// - `ControlResponse`: Response to an SDK-initiated control request
pub type IncomingMessage {
  /// Standard message from CLI (system, assistant, user, result).
  RegularMessage(Message)
  /// CLI-initiated control request requiring SDK response.
  ControlRequest(IncomingControlRequest)
  /// Response to SDK-initiated control request.
  ControlResponse(IncomingControlResponse)
}

// =============================================================================
// Incoming Control Requests (CLI → SDK)
// =============================================================================

/// Control requests initiated by CLI, requiring SDK response.
///
/// These represent callbacks from the CLI that the SDK must handle:
/// - Hook execution callbacks
/// - Permission prompts
/// - MCP server messages
pub type IncomingControlRequest {
  /// Hook callback from CLI.
  /// The SDK should execute the registered hook and respond with HookResponse.
  HookCallback(
    request_id: String,
    callback_id: String,
    input: Dynamic,
    tool_use_id: Option(String),
  )
  /// Tool permission request.
  /// The SDK should prompt user or auto-respond based on policy.
  CanUseTool(
    request_id: String,
    tool_name: String,
    input: Dynamic,
    permission_suggestions: List(String),
    blocked_path: Option(String),
  )
  /// MCP server message forwarding.
  /// The SDK should route to appropriate MCP handler.
  McpMessage(request_id: String, server_name: String, message: Dynamic)
}

// =============================================================================
// Incoming Control Responses (CLI → SDK)
// =============================================================================

/// Response from CLI to SDK-initiated control request.
///
/// Each response includes the original request_id for correlation.
pub type IncomingControlResponse {
  /// Control request completed successfully.
  /// Payload varies by original request type.
  Success(request_id: String, payload: Dynamic)
  /// Control request failed.
  /// Message describes what went wrong.
  Error(request_id: String, message: String)
}

// =============================================================================
// Outgoing Control Responses (SDK → CLI)
// =============================================================================

/// Responses from SDK to CLI-initiated control requests.
///
/// Sent after processing an IncomingControlRequest.
pub type OutgoingControlResponse {
  /// Response to HookCallback request.
  HookResponse(request_id: String, result: HookResult)
  /// Response to CanUseTool request.
  PermissionResponse(request_id: String, result: PermissionResult)
  /// Response to McpMessage request.
  McpResponse(request_id: String, response: Dynamic)
}

// =============================================================================
// Supporting Types
// =============================================================================

/// Permission mode for controlling tool execution behavior.
///
/// Mirrors `options.PermissionMode` but defined here for control protocol use.
/// This allows the control module to be self-contained.
pub type PermissionMode {
  /// Default behavior - prompts for permission
  Default
  /// Automatically accept file edits
  AcceptEdits
  /// Skip all permission prompts
  BypassPermissions
  /// Plan mode - read-only exploration
  Plan
}

/// Hook registration for bidirectional communication.
///
/// Defines a hook to be registered with the CLI during initialization.
pub type HookRegistration {
  HookRegistration(
    /// Unique identifier for this hook.
    hook_id: String,
    /// When the hook triggers (e.g., "pre_tool", "post_tool", "on_message").
    event: String,
    /// Optional filter pattern (e.g., tool name pattern).
    filter: Option(String),
  )
}

/// Result of hook execution.
pub type HookResult {
  /// Hook executed successfully.
  /// Output is hook-specific data to return to CLI.
  HookSuccess(output: Dynamic)
  /// Hook execution failed.
  HookError(message: String)
  /// Hook wants to skip/cancel the operation.
  HookSkip(reason: String)
}

/// Result of permission check.
pub type PermissionResult {
  /// Permission granted.
  Allow
  /// Permission denied by user.
  Deny
  /// Permission granted for this session only.
  AllowOnce
  /// Permission granted for all matching operations this session.
  AllowAll
  /// User wants to edit the input before execution.
  Edit(modified_input: Dynamic)
}
