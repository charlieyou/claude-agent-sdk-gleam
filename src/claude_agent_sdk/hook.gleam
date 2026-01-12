/// Hook context types for Claude Agent SDK bidirectional protocol.
///
/// This module defines hook-related types for all supported hook events.
/// These types enable type-safe hook callbacks with context-specific data.
///
/// ## Hook Events
///
/// The SDK supports the following hook events:
/// - `PreToolUse` - Before a tool is executed
/// - `PostToolUse` - After a tool completes
/// - `UserPromptSubmit` - When user submits a prompt
/// - `Stop` - When the session stops
/// - `SubagentStop` - When a subagent stops
/// - `PreCompact` - Before context compaction
///
/// ## Pattern Matching
///
/// Import this module for pattern matching on hook inputs:
///
/// ```gleam
/// import claude_agent_sdk/hook
///
/// case input {
///   hook.PreToolUseInput(ctx) -> // handle pre-tool-use
///   hook.PostToolUseInput(ctx) -> // handle post-tool-use
///   hook.UserPromptSubmitInput(ctx) -> // handle user prompt submit
///   hook.StopInput(ctx) -> // handle stop
///   hook.SubagentStopInput(ctx) -> // handle subagent stop
///   hook.PreCompactInput(ctx) -> // handle pre-compact
/// }
/// ```
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

// =============================================================================
// Hook Events
// =============================================================================

/// Hook event types indicating when a hook fires.
///
/// Used to identify which event triggered a hook callback.
pub type HookEvent {
  /// Fires before a tool is executed.
  PreToolUse
  /// Fires after a tool completes execution.
  PostToolUse
  /// Fires when user submits a prompt.
  UserPromptSubmit
  /// Fires when the session stops.
  Stop
  /// Fires when a subagent stops.
  SubagentStop
  /// Fires before context compaction.
  PreCompact
}

// =============================================================================
// Context Types
// =============================================================================

/// Context for pre-tool-use hooks.
///
/// Provides information about the tool about to be executed.
pub type PreToolUseContext {
  PreToolUseContext(
    /// Name of the tool being invoked (e.g., "Bash", "Read", "Write").
    tool_name: String,
    /// Tool-specific input parameters as Dynamic JSON.
    tool_input: Dynamic,
    /// Session identifier.
    session_id: String,
  )
}

/// Context for post-tool-use hooks.
///
/// Provides information about the tool that just completed.
pub type PostToolUseContext {
  PostToolUseContext(
    /// Name of the tool that was invoked.
    tool_name: String,
    /// Tool-specific input parameters as Dynamic JSON.
    tool_input: Dynamic,
    /// Tool output as Dynamic JSON.
    tool_output: Dynamic,
    /// Session identifier.
    session_id: String,
  )
}

/// Context for can-use-tool permission checks.
///
/// Provides information for permission decisions about tool usage.
pub type CanUseToolContext {
  CanUseToolContext(
    /// Name of the tool being requested.
    tool_name: String,
    /// Tool-specific input parameters as Dynamic JSON.
    tool_input: Dynamic,
    /// Session identifier.
    session_id: String,
    /// Permission suggestions from the tool definition.
    permission_suggestions: List(String),
    /// Path that is blocked, if applicable.
    blocked_path: Option(String),
  )
}

/// Context for user-prompt-submit hooks.
///
/// Provides the prompt content before processing.
pub type UserPromptSubmitContext {
  UserPromptSubmitContext(
    /// The user's prompt text.
    prompt: String,
    /// Session identifier.
    session_id: String,
  )
}

/// Context for stop hooks.
///
/// Provides information about why the session is stopping.
pub type StopContext {
  StopContext(
    /// Reason for stopping.
    reason: String,
    /// Session identifier.
    session_id: String,
  )
}

/// Context for subagent-stop hooks.
///
/// Provides information about a subagent that stopped.
pub type SubagentStopContext {
  SubagentStopContext(
    /// Identifier of the subagent that stopped.
    subagent_id: String,
    /// Reason for stopping.
    reason: String,
    /// Session identifier.
    session_id: String,
  )
}

/// Context for pre-compact hooks.
///
/// Provides session information before context compaction.
pub type PreCompactContext {
  PreCompactContext(
    /// Session identifier.
    session_id: String,
  )
}

// =============================================================================
// Result Types
// =============================================================================

/// Result of a hook execution.
///
/// Hooks can control the flow by returning one of these results.
/// Note: This is distinct from `control.HookResult` which is used for
/// protocol-level hook responses.
pub type HookExecutionResult {
  /// Continue with normal execution.
  Continue
  /// Block the operation with a reason.
  Block(reason: String)
  /// Modify the input before proceeding.
  ModifyInput(new_input: Dynamic)
}

/// Result of a permission check.
///
/// Permission hooks return either Allow or Deny.
/// Note: This is distinct from `control.PermissionResult` which supports
/// additional variants like AllowOnce, AllowAll, and Edit for the protocol.
pub type PermissionCheckResult {
  /// Allow the operation to proceed.
  Allow
  /// Deny the operation with a reason.
  Deny(reason: String)
}

// =============================================================================
// Hook Input (typed union for decoder)
// =============================================================================

/// Typed hook input for decoding incoming hook callbacks.
///
/// Each variant wraps the context type for its corresponding event.
pub type HookInput {
  /// Pre-tool-use hook input.
  PreToolUseInput(PreToolUseContext)
  /// Post-tool-use hook input.
  PostToolUseInput(PostToolUseContext)
  /// Can-use-tool permission check input.
  CanUseToolInput(CanUseToolContext)
  /// User-prompt-submit hook input.
  UserPromptSubmitInput(UserPromptSubmitContext)
  /// Stop hook input.
  StopInput(StopContext)
  /// Subagent-stop hook input.
  SubagentStopInput(SubagentStopContext)
  /// Pre-compact hook input.
  PreCompactInput(PreCompactContext)
}
