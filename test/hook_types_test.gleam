/// Tests for hook context types.
///
/// These tests verify that all hook context types can be constructed correctly.
import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}
import gleeunit/should

import claude_agent_sdk/hook.{
  type HookExecutionResult, type PermissionCheckResult, Allow, Block,
  CanUseToolContext, CanUseToolInput, Continue, Deny, MissingField, ModifyInput,
  PostToolUse, PostToolUseContext, PostToolUseInput, PreCompact,
  PreCompactContext, PreCompactInput, PreToolUse, PreToolUseContext,
  PreToolUseInput, Stop, StopContext, StopInput, SubagentStop,
  SubagentStopContext, SubagentStopInput, UserPromptSubmit,
  UserPromptSubmitContext, UserPromptSubmitInput, WrongType, decode_hook_input,
}
import gleam/dynamic/decode
import gleam/json

/// Converts any value to Dynamic using Gleam stdlib identity.
/// This is the standard Gleam idiom for Erlang target.
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// =============================================================================
// Hook Event Tests
// =============================================================================

pub fn hook_event_pre_tool_use_test() {
  let event = PreToolUse
  should.equal(event, PreToolUse)
}

pub fn hook_event_post_tool_use_test() {
  let event = PostToolUse
  should.equal(event, PostToolUse)
}

pub fn hook_event_user_prompt_submit_test() {
  let event = UserPromptSubmit
  should.equal(event, UserPromptSubmit)
}

pub fn hook_event_stop_test() {
  let event = Stop
  should.equal(event, Stop)
}

pub fn hook_event_subagent_stop_test() {
  let event = SubagentStop
  should.equal(event, SubagentStop)
}

pub fn hook_event_pre_compact_test() {
  let event = PreCompact
  should.equal(event, PreCompact)
}

// =============================================================================
// Context Type Tests
// =============================================================================

pub fn pre_tool_use_context_test() {
  let ctx =
    PreToolUseContext(
      tool_name: "Bash",
      tool_input: to_dynamic("ls -la"),
      session_id: "session-123",
    )
  should.equal(ctx.tool_name, "Bash")
  should.equal(ctx.session_id, "session-123")
}

pub fn post_tool_use_context_test() {
  let ctx =
    PostToolUseContext(
      tool_name: "Read",
      tool_input: to_dynamic("/path/to/file"),
      tool_output: to_dynamic("file contents"),
      session_id: "session-456",
    )
  should.equal(ctx.tool_name, "Read")
  should.equal(ctx.session_id, "session-456")
}

pub fn can_use_tool_context_test() {
  let ctx =
    CanUseToolContext(
      tool_name: "Write",
      tool_input: to_dynamic("/path/to/file"),
      session_id: "session-789",
      permission_suggestions: ["allow_write", "allow_edit"],
      blocked_path: Some("/protected/path"),
    )
  should.equal(ctx.tool_name, "Write")
  should.equal(ctx.session_id, "session-789")
  should.equal(ctx.permission_suggestions, ["allow_write", "allow_edit"])
  should.equal(ctx.blocked_path, Some("/protected/path"))
}

pub fn can_use_tool_context_no_blocked_path_test() {
  let ctx =
    CanUseToolContext(
      tool_name: "Bash",
      tool_input: to_dynamic("echo hello"),
      session_id: "session-abc",
      permission_suggestions: [],
      blocked_path: None,
    )
  should.equal(ctx.blocked_path, None)
}

pub fn user_prompt_submit_context_test() {
  let ctx =
    UserPromptSubmitContext(prompt: "Hello Claude", session_id: "session-def")
  should.equal(ctx.prompt, "Hello Claude")
  should.equal(ctx.session_id, "session-def")
}

pub fn stop_context_test() {
  let ctx = StopContext(reason: "user_requested", session_id: "session-ghi")
  should.equal(ctx.reason, "user_requested")
  should.equal(ctx.session_id, "session-ghi")
}

pub fn subagent_stop_context_test() {
  let ctx =
    SubagentStopContext(
      subagent_id: "subagent-001",
      reason: "task_complete",
      session_id: "session-jkl",
    )
  should.equal(ctx.subagent_id, "subagent-001")
  should.equal(ctx.reason, "task_complete")
  should.equal(ctx.session_id, "session-jkl")
}

pub fn pre_compact_context_test() {
  let ctx = PreCompactContext(session_id: "session-mno")
  should.equal(ctx.session_id, "session-mno")
}

// =============================================================================
// Result Type Tests
// =============================================================================

pub fn hook_result_continue_test() {
  let result: HookExecutionResult = Continue
  should.equal(result, Continue)
}

pub fn hook_result_block_test() {
  let result = Block(reason: "Operation not allowed")
  let Block(reason) = result
  should.equal(reason, "Operation not allowed")
}

pub fn hook_result_modify_input_test() {
  let new_input = to_dynamic("modified input")
  let result = ModifyInput(new_input: new_input)
  let ModifyInput(input) = result
  should.equal(input, new_input)
}

pub fn permission_result_allow_test() {
  let result: PermissionCheckResult = Allow
  should.equal(result, Allow)
}

pub fn permission_result_deny_test() {
  let result = Deny(reason: "Insufficient permissions")
  let Deny(reason) = result
  should.equal(reason, "Insufficient permissions")
}

// =============================================================================
// Hook Input Tests
// =============================================================================

pub fn hook_input_pre_tool_use_test() {
  let ctx =
    PreToolUseContext(
      tool_name: "Bash",
      tool_input: to_dynamic("pwd"),
      session_id: "session-1",
    )
  let input = PreToolUseInput(ctx)
  let PreToolUseInput(c) = input
  should.equal(c.tool_name, "Bash")
}

pub fn hook_input_post_tool_use_test() {
  let ctx =
    PostToolUseContext(
      tool_name: "Read",
      tool_input: to_dynamic("/file"),
      tool_output: to_dynamic("content"),
      session_id: "session-2",
    )
  let input = PostToolUseInput(ctx)
  let PostToolUseInput(c) = input
  should.equal(c.tool_name, "Read")
}

pub fn hook_input_can_use_tool_test() {
  let ctx =
    CanUseToolContext(
      tool_name: "Write",
      tool_input: to_dynamic("/path/to/file"),
      session_id: "session-perm",
      permission_suggestions: ["allow_write"],
      blocked_path: Some("/path/to/file"),
    )
  let input = CanUseToolInput(ctx)
  let CanUseToolInput(c) = input
  should.equal(c.tool_name, "Write")
  should.equal(c.permission_suggestions, ["allow_write"])
}

pub fn hook_input_user_prompt_submit_test() {
  let ctx =
    UserPromptSubmitContext(prompt: "Test prompt", session_id: "session-3")
  let input = UserPromptSubmitInput(ctx)
  let UserPromptSubmitInput(c) = input
  should.equal(c.prompt, "Test prompt")
}

pub fn hook_input_stop_test() {
  let ctx = StopContext(reason: "done", session_id: "session-4")
  let input = StopInput(ctx)
  let StopInput(c) = input
  should.equal(c.reason, "done")
}

pub fn hook_input_subagent_stop_test() {
  let ctx =
    SubagentStopContext(
      subagent_id: "sub-1",
      reason: "finished",
      session_id: "session-5",
    )
  let input = SubagentStopInput(ctx)
  let SubagentStopInput(c) = input
  should.equal(c.subagent_id, "sub-1")
}

pub fn hook_input_pre_compact_test() {
  let ctx = PreCompactContext(session_id: "session-6")
  let input = PreCompactInput(ctx)
  let PreCompactInput(c) = input
  should.equal(c.session_id, "session-6")
}

// =============================================================================
// hook_event_name Validation Tests
// =============================================================================

/// Parse JSON string to Dynamic for tests
fn parse_json(json_string: String) -> Dynamic {
  case json.parse(json_string, decode.dynamic) {
    Ok(d) -> d
    Error(_) -> panic as "invalid json in test"
  }
}

/// When hook_event_name is missing, should return MissingField error
pub fn decode_hook_input_missing_hook_event_name_test() {
  // JSON with no hook_event_name field
  let json_str =
    "{\"session_id\":\"abc123\",\"tool_name\":\"Bash\",\"tool_input\":{}}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PreToolUse, input)

  case result {
    Error(MissingField(field)) -> should.equal(field, "hook_event_name")
    _ -> should.fail()
  }
}

/// When hook_event_name has wrong type (number instead of string), should return WrongType error
pub fn decode_hook_input_wrong_type_hook_event_name_test() {
  // JSON with hook_event_name as number instead of string
  let json_str =
    "{\"hook_event_name\":123,\"session_id\":\"abc123\",\"tool_name\":\"Bash\",\"tool_input\":{}}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PreToolUse, input)

  case result {
    Error(WrongType(field: field, expected: expected)) -> {
      should.equal(field, "hook_event_name")
      should.equal(expected, "String")
    }
    _ -> should.fail()
  }
}
