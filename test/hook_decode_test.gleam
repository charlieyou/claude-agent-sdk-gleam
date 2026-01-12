/// Tests for hook input decoders.
///
/// TDD: Write failing tests first, then implement decoders.
import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/option.{None, Some}
import gleeunit/should

import claude_agent_sdk/hook.{
  type HookDecodeError, type HookEvent, type HookInput, CanUseTool,
  CanUseToolContext, CanUseToolInput, MissingField, PostToolUse,
  PostToolUseContext, PostToolUseInput, PreCompact, PreCompactContext,
  PreCompactInput, PreToolUse, PreToolUseContext, PreToolUseInput, Stop,
  StopContext, StopInput, SubagentStop, SubagentStopContext, SubagentStopInput,
  UnknownEventName, UserPromptSubmit, UserPromptSubmitContext,
  UserPromptSubmitInput, WrongType, decode_hook_input,
}

import gleam/dynamic/decode

/// Parse JSON string to Dynamic
fn parse_json(json_string: String) -> Dynamic {
  case json.parse(json_string, decode.dynamic) {
    Ok(d) -> d
    Error(_) -> panic as "invalid json in test"
  }
}

// =============================================================================
// PreToolUse Decoder Tests
// =============================================================================

pub fn decode_pre_tool_use_success_test() {
  let json_str =
    "{\"hook_event_name\":\"PreToolUse\",\"session_id\":\"abc123\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"ls\"}}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PreToolUse, input)

  case result {
    Ok(PreToolUseInput(ctx)) -> {
      should.equal(ctx.tool_name, "Bash")
      should.equal(ctx.session_id, "abc123")
    }
    _ -> should.fail()
  }
}

pub fn decode_pre_tool_use_missing_tool_name_test() {
  let json_str =
    "{\"hook_event_name\":\"PreToolUse\",\"session_id\":\"abc123\",\"tool_input\":{\"command\":\"ls\"}}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PreToolUse, input)

  case result {
    Error(MissingField(field)) -> should.equal(field, "tool_name")
    _ -> should.fail()
  }
}

pub fn decode_pre_tool_use_missing_session_id_test() {
  let json_str =
    "{\"hook_event_name\":\"PreToolUse\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"ls\"}}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PreToolUse, input)

  case result {
    Error(MissingField(field)) -> should.equal(field, "session_id")
    _ -> should.fail()
  }
}

// =============================================================================
// PostToolUse Decoder Tests
// =============================================================================

pub fn decode_post_tool_use_success_test() {
  let json_str =
    "{\"hook_event_name\":\"PostToolUse\",\"session_id\":\"abc123\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"ls\"},\"tool_result\":{\"output\":\"file1.txt\"}}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PostToolUse, input)

  case result {
    Ok(PostToolUseInput(ctx)) -> {
      should.equal(ctx.tool_name, "Bash")
      should.equal(ctx.session_id, "abc123")
    }
    _ -> should.fail()
  }
}

pub fn decode_post_tool_use_missing_tool_result_test() {
  let json_str =
    "{\"hook_event_name\":\"PostToolUse\",\"session_id\":\"abc123\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"ls\"}}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PostToolUse, input)

  case result {
    Error(MissingField(field)) -> should.equal(field, "tool_result")
    _ -> should.fail()
  }
}

// =============================================================================
// UserPromptSubmit Decoder Tests
// =============================================================================

pub fn decode_user_prompt_submit_success_test() {
  let json_str =
    "{\"hook_event_name\":\"UserPromptSubmit\",\"session_id\":\"abc123\",\"prompt\":\"Hello Claude\"}"
  let input = parse_json(json_str)

  let result = decode_hook_input(UserPromptSubmit, input)

  case result {
    Ok(UserPromptSubmitInput(ctx)) -> {
      should.equal(ctx.prompt, "Hello Claude")
      should.equal(ctx.session_id, "abc123")
    }
    _ -> should.fail()
  }
}

pub fn decode_user_prompt_submit_missing_prompt_test() {
  let json_str =
    "{\"hook_event_name\":\"UserPromptSubmit\",\"session_id\":\"abc123\"}"
  let input = parse_json(json_str)

  let result = decode_hook_input(UserPromptSubmit, input)

  case result {
    Error(MissingField(field)) -> should.equal(field, "prompt")
    _ -> should.fail()
  }
}

// =============================================================================
// Stop Decoder Tests
// =============================================================================

pub fn decode_stop_success_test() {
  let json_str =
    "{\"hook_event_name\":\"Stop\",\"session_id\":\"abc123\",\"reason\":\"task_complete\"}"
  let input = parse_json(json_str)

  let result = decode_hook_input(Stop, input)

  case result {
    Ok(StopInput(ctx)) -> {
      should.equal(ctx.reason, "task_complete")
      should.equal(ctx.session_id, "abc123")
    }
    _ -> should.fail()
  }
}

pub fn decode_stop_missing_reason_test() {
  let json_str = "{\"hook_event_name\":\"Stop\",\"session_id\":\"abc123\"}"
  let input = parse_json(json_str)

  let result = decode_hook_input(Stop, input)

  case result {
    Error(MissingField(field)) -> should.equal(field, "reason")
    _ -> should.fail()
  }
}

// =============================================================================
// SubagentStop Decoder Tests
// =============================================================================

pub fn decode_subagent_stop_success_test() {
  let json_str =
    "{\"hook_event_name\":\"SubagentStop\",\"session_id\":\"abc123\",\"subagent_id\":\"sub_1\",\"reason\":\"finished\"}"
  let input = parse_json(json_str)

  let result = decode_hook_input(SubagentStop, input)

  case result {
    Ok(SubagentStopInput(ctx)) -> {
      should.equal(ctx.subagent_id, "sub_1")
      should.equal(ctx.reason, "finished")
      should.equal(ctx.session_id, "abc123")
    }
    _ -> should.fail()
  }
}

pub fn decode_subagent_stop_missing_subagent_id_test() {
  let json_str =
    "{\"hook_event_name\":\"SubagentStop\",\"session_id\":\"abc123\",\"reason\":\"finished\"}"
  let input = parse_json(json_str)

  let result = decode_hook_input(SubagentStop, input)

  case result {
    Error(MissingField(field)) -> should.equal(field, "subagent_id")
    _ -> should.fail()
  }
}

// =============================================================================
// PreCompact Decoder Tests
// =============================================================================

pub fn decode_pre_compact_success_test() {
  let json_str =
    "{\"hook_event_name\":\"PreCompact\",\"session_id\":\"abc123\"}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PreCompact, input)

  case result {
    Ok(PreCompactInput(ctx)) -> {
      should.equal(ctx.session_id, "abc123")
    }
    _ -> should.fail()
  }
}

pub fn decode_pre_compact_missing_session_id_test() {
  let json_str = "{\"hook_event_name\":\"PreCompact\"}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PreCompact, input)

  case result {
    Error(MissingField(field)) -> should.equal(field, "session_id")
    _ -> should.fail()
  }
}

// =============================================================================
// CanUseTool Decoder Tests
// =============================================================================

pub fn decode_can_use_tool_success_test() {
  let json_str =
    "{\"hook_event_name\":\"CanUseTool\",\"session_id\":\"abc123\",\"tool_name\":\"Write\",\"tool_input\":{\"path\":\"/etc/passwd\"},\"permission_suggestions\":[\"allow_write\"],\"blocked_path\":\"/etc/passwd\"}"
  let input = parse_json(json_str)

  let result = decode_hook_input(CanUseTool, input)

  case result {
    Ok(CanUseToolInput(ctx)) -> {
      should.equal(ctx.tool_name, "Write")
      should.equal(ctx.session_id, "abc123")
      should.equal(ctx.permission_suggestions, ["allow_write"])
      should.equal(ctx.blocked_path, Some("/etc/passwd"))
    }
    _ -> should.fail()
  }
}

pub fn decode_can_use_tool_no_blocked_path_test() {
  let json_str =
    "{\"hook_event_name\":\"CanUseTool\",\"session_id\":\"abc123\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"ls\"},\"permission_suggestions\":[]}"
  let input = parse_json(json_str)

  let result = decode_hook_input(CanUseTool, input)

  case result {
    Ok(CanUseToolInput(ctx)) -> {
      should.equal(ctx.tool_name, "Bash")
      should.equal(ctx.blocked_path, None)
    }
    _ -> should.fail()
  }
}

// =============================================================================
// WrongType Error Tests
// =============================================================================

pub fn decode_wrong_type_session_id_test() {
  // session_id should be string, not number
  let json_str =
    "{\"hook_event_name\":\"PreToolUse\",\"session_id\":123,\"tool_name\":\"Bash\",\"tool_input\":{}}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PreToolUse, input)

  case result {
    Error(WrongType(field, expected)) -> {
      should.equal(field, "session_id")
      should.equal(expected, "String")
    }
    _ -> should.fail()
  }
}

pub fn decode_wrong_type_tool_name_test() {
  // tool_name should be string, not array
  let json_str =
    "{\"hook_event_name\":\"PreToolUse\",\"session_id\":\"abc\",\"tool_name\":[\"Bash\"],\"tool_input\":{}}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PreToolUse, input)

  case result {
    Error(WrongType(field, _)) -> should.equal(field, "tool_name")
    _ -> should.fail()
  }
}

// =============================================================================
// Forward Compatibility Tests
// =============================================================================

pub fn decode_ignores_unknown_fields_test() {
  // Unknown fields should be ignored for forward compatibility
  let json_str =
    "{\"hook_event_name\":\"PreToolUse\",\"session_id\":\"abc123\",\"tool_name\":\"Bash\",\"tool_input\":{},\"unknown_field\":\"should_be_ignored\",\"another_unknown\":123}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PreToolUse, input)

  case result {
    Ok(PreToolUseInput(ctx)) -> {
      should.equal(ctx.tool_name, "Bash")
      should.equal(ctx.session_id, "abc123")
    }
    _ -> should.fail()
  }
}

// =============================================================================
// Event Name Mismatch Tests
// =============================================================================

pub fn decode_event_name_mismatch_returns_unknown_event_name_test() {
  // Passing Stop event but payload has hook_event_name = "PreToolUse"
  let json_str =
    "{\"hook_event_name\":\"PreToolUse\",\"session_id\":\"abc123\",\"tool_name\":\"Bash\",\"tool_input\":{}}"
  let input = parse_json(json_str)

  let result = decode_hook_input(Stop, input)

  case result {
    Error(UnknownEventName(name)) -> should.equal(name, "PreToolUse")
    _ -> should.fail()
  }
}

pub fn decode_missing_hook_event_name_test() {
  // Payload is missing hook_event_name field entirely
  let json_str =
    "{\"session_id\":\"abc123\",\"tool_name\":\"Bash\",\"tool_input\":{}}"
  let input = parse_json(json_str)

  let result = decode_hook_input(PreToolUse, input)

  case result {
    Error(MissingField(field)) -> should.equal(field, "hook_event_name")
    _ -> should.fail()
  }
}
