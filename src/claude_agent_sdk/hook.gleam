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
import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}

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
  /// Permission check for tool usage.
  CanUseTool
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

// =============================================================================
// Hook Decode Errors
// =============================================================================

/// Error types for hook input decoding.
pub type HookDecodeError {
  /// A required field is missing from the input.
  MissingField(field: String)
  /// A field has the wrong type.
  WrongType(field: String, expected: String)
  /// Unknown event name in the input.
  UnknownEventName(name: String)
}

// =============================================================================
// Hook Input Decoders
// =============================================================================

/// Decode hook input to typed context based on event type.
///
/// Dispatches to the appropriate decoder based on the HookEvent variant.
/// Unknown fields are ignored for forward compatibility.
pub fn decode_hook_input(
  event: HookEvent,
  input: Dynamic,
) -> Result(HookInput, HookDecodeError) {
  case event {
    PreToolUse -> decode_pre_tool_use(input)
    PostToolUse -> decode_post_tool_use(input)
    UserPromptSubmit -> decode_user_prompt_submit(input)
    Stop -> decode_stop(input)
    SubagentStop -> decode_subagent_stop(input)
    PreCompact -> decode_pre_compact(input)
    CanUseTool -> decode_can_use_tool(input)
  }
}

/// Decode PreToolUse input to PreToolUseContext.
fn decode_pre_tool_use(input: Dynamic) -> Result(HookInput, HookDecodeError) {
  let decoder = {
    use tool_name <- decode.field("tool_name", decode.string)
    use tool_input <- decode.field("tool_input", decode.dynamic)
    use session_id <- decode.field("session_id", decode.string)
    decode.success(PreToolUseContext(tool_name:, tool_input:, session_id:))
  }
  case decode.run(input, decoder) {
    Ok(ctx) -> Ok(PreToolUseInput(ctx))
    Error(errors) -> Error(decode_errors_to_hook_error(errors))
  }
}

/// Decode PostToolUse input to PostToolUseContext.
fn decode_post_tool_use(input: Dynamic) -> Result(HookInput, HookDecodeError) {
  let decoder = {
    use tool_name <- decode.field("tool_name", decode.string)
    use tool_input <- decode.field("tool_input", decode.dynamic)
    use tool_output <- decode.field("tool_result", decode.dynamic)
    use session_id <- decode.field("session_id", decode.string)
    decode.success(PostToolUseContext(
      tool_name:,
      tool_input:,
      tool_output:,
      session_id:,
    ))
  }
  case decode.run(input, decoder) {
    Ok(ctx) -> Ok(PostToolUseInput(ctx))
    Error(errors) -> Error(decode_errors_to_hook_error(errors))
  }
}

/// Decode UserPromptSubmit input to UserPromptSubmitContext.
fn decode_user_prompt_submit(
  input: Dynamic,
) -> Result(HookInput, HookDecodeError) {
  let decoder = {
    use prompt <- decode.field("prompt", decode.string)
    use session_id <- decode.field("session_id", decode.string)
    decode.success(UserPromptSubmitContext(prompt:, session_id:))
  }
  case decode.run(input, decoder) {
    Ok(ctx) -> Ok(UserPromptSubmitInput(ctx))
    Error(errors) -> Error(decode_errors_to_hook_error(errors))
  }
}

/// Decode Stop input to StopContext.
fn decode_stop(input: Dynamic) -> Result(HookInput, HookDecodeError) {
  let decoder = {
    use reason <- decode.field("reason", decode.string)
    use session_id <- decode.field("session_id", decode.string)
    decode.success(StopContext(reason:, session_id:))
  }
  case decode.run(input, decoder) {
    Ok(ctx) -> Ok(StopInput(ctx))
    Error(errors) -> Error(decode_errors_to_hook_error(errors))
  }
}

/// Decode SubagentStop input to SubagentStopContext.
fn decode_subagent_stop(input: Dynamic) -> Result(HookInput, HookDecodeError) {
  let decoder = {
    use subagent_id <- decode.field("subagent_id", decode.string)
    use reason <- decode.field("reason", decode.string)
    use session_id <- decode.field("session_id", decode.string)
    decode.success(SubagentStopContext(subagent_id:, reason:, session_id:))
  }
  case decode.run(input, decoder) {
    Ok(ctx) -> Ok(SubagentStopInput(ctx))
    Error(errors) -> Error(decode_errors_to_hook_error(errors))
  }
}

/// Decode PreCompact input to PreCompactContext.
fn decode_pre_compact(input: Dynamic) -> Result(HookInput, HookDecodeError) {
  let decoder = {
    use session_id <- decode.field("session_id", decode.string)
    decode.success(PreCompactContext(session_id:))
  }
  case decode.run(input, decoder) {
    Ok(ctx) -> Ok(PreCompactInput(ctx))
    Error(errors) -> Error(decode_errors_to_hook_error(errors))
  }
}

/// Decode CanUseTool input to CanUseToolContext.
fn decode_can_use_tool(input: Dynamic) -> Result(HookInput, HookDecodeError) {
  let decoder = {
    use tool_name <- decode.field("tool_name", decode.string)
    use tool_input <- decode.field("tool_input", decode.dynamic)
    use session_id <- decode.field("session_id", decode.string)
    use permission_suggestions <- decode.optional_field(
      "permission_suggestions",
      [],
      decode.list(decode.string),
    )
    use blocked_path <- decode.optional_field(
      "blocked_path",
      None,
      decode.map(decode.string, Some),
    )
    decode.success(CanUseToolContext(
      tool_name:,
      tool_input:,
      session_id:,
      permission_suggestions:,
      blocked_path:,
    ))
  }
  case decode.run(input, decoder) {
    Ok(ctx) -> Ok(CanUseToolInput(ctx))
    Error(errors) -> Error(decode_errors_to_hook_error(errors))
  }
}

/// Convert decode errors to HookDecodeError.
/// Distinguishes between missing fields and type mismatches.
fn decode_errors_to_hook_error(
  errors: List(decode.DecodeError),
) -> HookDecodeError {
  case errors {
    [decode.DecodeError(expected: expected, found: found, path: path), ..] -> {
      let field = case path {
        [f, ..] -> f
        [] -> "unknown"
      }
      // "nothing" indicates the field was missing entirely
      case found {
        "nothing" -> MissingField(field)
        _ -> WrongType(field: field, expected: expected)
      }
    }
    [] -> MissingField("unknown")
  }
}
