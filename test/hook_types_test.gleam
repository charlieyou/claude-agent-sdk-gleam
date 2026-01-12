/// Tests for hook context types.
///
/// These tests verify that all hook context types can be constructed correctly.
import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}
import gleeunit/should

import claude_agent_sdk/hook.{
  type HookResult, type PermissionResult, Allow, Block, CanUseToolContext,
  Continue, Deny, ModifyInput, PostToolUse, PostToolUseContext, PostToolUseInput,
  PreCompact, PreCompactContext, PreCompactInput, PreToolUse, PreToolUseContext,
  PreToolUseInput, Stop, StopContext, StopInput, SubagentStop,
  SubagentStopContext, SubagentStopInput, UserPromptSubmit,
  UserPromptSubmitContext, UserPromptSubmitInput,
}

// Helper to convert any value to Dynamic (identity function in Erlang)
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
  let result: HookResult = Continue
  should.equal(result, Continue)
}

pub fn hook_result_block_test() {
  let result = Block(reason: "Operation not allowed")
  case result {
    Block(reason) -> should.equal(reason, "Operation not allowed")
    _ -> should.fail()
  }
}

pub fn hook_result_modify_input_test() {
  let new_input = to_dynamic("modified input")
  let result = ModifyInput(new_input: new_input)
  case result {
    ModifyInput(input) -> should.equal(input, new_input)
    _ -> should.fail()
  }
}

pub fn permission_result_allow_test() {
  let result: PermissionResult = Allow
  should.equal(result, Allow)
}

pub fn permission_result_deny_test() {
  let result = Deny(reason: "Insufficient permissions")
  case result {
    Deny(reason) -> should.equal(reason, "Insufficient permissions")
    _ -> should.fail()
  }
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
  case input {
    PreToolUseInput(c) -> should.equal(c.tool_name, "Bash")
    _ -> should.fail()
  }
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
  case input {
    PostToolUseInput(c) -> should.equal(c.tool_name, "Read")
    _ -> should.fail()
  }
}

pub fn hook_input_user_prompt_submit_test() {
  let ctx =
    UserPromptSubmitContext(prompt: "Test prompt", session_id: "session-3")
  let input = UserPromptSubmitInput(ctx)
  case input {
    UserPromptSubmitInput(c) -> should.equal(c.prompt, "Test prompt")
    _ -> should.fail()
  }
}

pub fn hook_input_stop_test() {
  let ctx = StopContext(reason: "done", session_id: "session-4")
  let input = StopInput(ctx)
  case input {
    StopInput(c) -> should.equal(c.reason, "done")
    _ -> should.fail()
  }
}

pub fn hook_input_subagent_stop_test() {
  let ctx =
    SubagentStopContext(
      subagent_id: "sub-1",
      reason: "finished",
      session_id: "session-5",
    )
  let input = SubagentStopInput(ctx)
  case input {
    SubagentStopInput(c) -> should.equal(c.subagent_id, "sub-1")
    _ -> should.fail()
  }
}

pub fn hook_input_pre_compact_test() {
  let ctx = PreCompactContext(session_id: "session-6")
  let input = PreCompactInput(ctx)
  case input {
    PreCompactInput(c) -> should.equal(c.session_id, "session-6")
    _ -> should.fail()
  }
}
