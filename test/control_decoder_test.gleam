/// Tests for control message decoder.
///
/// These tests verify decode_line correctly parses JSON into IncomingMessage variants.
import gleam/option.{None, Some}
import gleam/result
import gleeunit/should

import claude_agent_sdk/control.{
  CanUseTool, ControlRequest, ControlResponse, Error as ControlError,
  HookCallback, McpMessage, RegularMessage, Success,
}
import claude_agent_sdk/internal/control_decoder.{
  InvalidType, JsonError, MissingField, UnknownSubtype, decode_line,
}
import claude_agent_sdk/message

// ============================================================================
// Control Request Decoding Tests
// ============================================================================

pub fn decode_hook_callback_test() {
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"hook_0\",\"input\":{\"hook_event_name\":\"PreToolUse\",\"session_id\":\"abc123\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"ls\"}},\"tool_use_id\":\"toolu_01ABC\"}}"
  case decode_line(json) {
    Ok(ControlRequest(HookCallback(req_id, cb_id, _input, tool_use_id))) -> {
      req_id |> should.equal("cli_1")
      cb_id |> should.equal("hook_0")
      tool_use_id |> should.equal(Some("toolu_01ABC"))
    }
    _ -> should.fail()
  }
}

pub fn decode_hook_callback_without_tool_use_id_test() {
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_2\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"hook_1\",\"input\":{\"event\":\"test\"}}}"
  case decode_line(json) {
    Ok(ControlRequest(HookCallback(req_id, cb_id, _input, tool_use_id))) -> {
      req_id |> should.equal("cli_2")
      cb_id |> should.equal("hook_1")
      tool_use_id |> should.equal(None)
    }
    _ -> should.fail()
  }
}

pub fn decode_can_use_tool_test() {
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_2\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"Write\",\"input\":{\"file_path\":\"/etc/passwd\"},\"permission_suggestions\":[\"deny\"],\"blocked_path\":\"/etc\"}}"
  case decode_line(json) {
    Ok(ControlRequest(CanUseTool(
      req_id,
      tool_name,
      _input,
      suggestions,
      blocked,
    ))) -> {
      req_id |> should.equal("cli_2")
      tool_name |> should.equal("Write")
      suggestions |> should.equal(["deny"])
      blocked |> should.equal(Some("/etc"))
    }
    _ -> should.fail()
  }
}

pub fn decode_can_use_tool_without_blocked_path_test() {
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_3\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"Bash\",\"input\":{\"command\":\"ls\"},\"permission_suggestions\":[\"allow\",\"deny\"]}}"
  case decode_line(json) {
    Ok(ControlRequest(CanUseTool(
      req_id,
      tool_name,
      _input,
      suggestions,
      blocked,
    ))) -> {
      req_id |> should.equal("cli_3")
      tool_name |> should.equal("Bash")
      suggestions |> should.equal(["allow", "deny"])
      blocked |> should.equal(None)
    }
    _ -> should.fail()
  }
}

pub fn decode_mcp_message_test() {
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_4\",\"request\":{\"subtype\":\"mcp_message\",\"server_name\":\"git-server\",\"message\":{\"method\":\"list_repos\"}}}"
  case decode_line(json) {
    Ok(ControlRequest(McpMessage(req_id, server_name, _message))) -> {
      req_id |> should.equal("cli_4")
      server_name |> should.equal("git-server")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// Control Response Decoding Tests
// ============================================================================

pub fn decode_success_response_test() {
  let json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"supported_commands\":[\"interrupt\"]}}}"
  case decode_line(json) {
    Ok(ControlResponse(Success(req_id, _payload))) -> {
      req_id |> should.equal("req_0")
    }
    _ -> should.fail()
  }
}

pub fn decode_error_response_test() {
  let json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req_1\",\"error\":\"Operation not supported\"}}"
  case decode_line(json) {
    Ok(ControlResponse(ControlError(req_id, err_message))) -> {
      req_id |> should.equal("req_1")
      err_message |> should.equal("Operation not supported")
    }
    _ -> should.fail()
  }
}

// Test tolerant decoding: request_id at top-level
pub fn decode_success_with_toplevel_request_id_test() {
  let json =
    "{\"type\":\"control_response\",\"request_id\":\"req_2\",\"response\":{\"subtype\":\"success\",\"response\":{\"ok\":true}}}"
  case decode_line(json) {
    Ok(ControlResponse(Success(req_id, _payload))) -> {
      req_id |> should.equal("req_2")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// Regular Message Decoding Tests
// ============================================================================

pub fn decode_system_message_test() {
  let json =
    "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"sess_1\"}"
  case decode_line(json) {
    Ok(RegularMessage(message.System(sys))) -> {
      sys.subtype |> should.equal(Some("init"))
      sys.session_id |> should.equal(Some("sess_1"))
    }
    _ -> should.fail()
  }
}

pub fn decode_assistant_message_test() {
  let json =
    "{\"type\":\"assistant\",\"uuid\":\"msg_1\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}}"
  case decode_line(json) {
    Ok(RegularMessage(message.Assistant(asst))) -> {
      asst.uuid |> should.equal(Some("msg_1"))
    }
    _ -> should.fail()
  }
}

pub fn decode_user_message_test() {
  let json =
    "{\"type\":\"user\",\"uuid\":\"msg_2\",\"message\":{\"role\":\"user\",\"content\":[{\"tool_use_id\":\"tu_1\",\"content\":\"ok\"}]}}"
  case decode_line(json) {
    Ok(RegularMessage(message.User(usr))) -> {
      usr.uuid |> should.equal(Some("msg_2"))
    }
    _ -> should.fail()
  }
}

pub fn decode_result_message_test() {
  let json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\",\"duration_ms\":1234}"
  case decode_line(json) {
    Ok(RegularMessage(message.Result(res))) -> {
      res.result |> should.equal(Some("done"))
      res.duration_ms |> should.equal(Some(1234))
    }
    _ -> should.fail()
  }
}

// ============================================================================
// Error Cases
// ============================================================================

pub fn decode_invalid_json_test() {
  let json = "not valid json {"
  decode_line(json)
  |> result.is_error
  |> should.be_true
  case decode_line(json) {
    Error(JsonError(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn decode_missing_type_test() {
  let json = "{\"request_id\":\"123\"}"
  case decode_line(json) {
    Error(MissingField(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn decode_invalid_type_test() {
  let json = "{\"type\":\"unknown_type_xyz\"}"
  case decode_line(json) {
    Error(InvalidType(t)) -> t |> should.equal("unknown_type_xyz")
    _ -> should.fail()
  }
}

pub fn decode_unknown_control_request_subtype_test() {
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_5\",\"request\":{\"subtype\":\"future_feature\"}}"
  case decode_line(json) {
    Error(UnknownSubtype(subtype:, message: _)) ->
      subtype |> should.equal("future_feature")
    _ -> should.fail()
  }
}

pub fn decode_unknown_control_response_subtype_test() {
  let json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"future_status\",\"request_id\":\"req_3\"}}"
  case decode_line(json) {
    Error(UnknownSubtype(subtype:, message: _)) ->
      subtype |> should.equal("future_status")
    _ -> should.fail()
  }
}

// ============================================================================
// Edge Cases - Forward Compatibility
// ============================================================================

pub fn decode_with_extra_fields_test() {
  // Unknown fields should be ignored
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_6\",\"unknown_field\":\"ignored\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"h1\",\"input\":{},\"extra_nested\":123}}"
  case decode_line(json) {
    Ok(ControlRequest(HookCallback(req_id, cb_id, _, _))) -> {
      req_id |> should.equal("cli_6")
      cb_id |> should.equal("h1")
    }
    _ -> should.fail()
  }
}
