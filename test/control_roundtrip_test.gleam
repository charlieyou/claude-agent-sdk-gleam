/// Round-trip encode/decode tests for control message types.
///
/// These tests verify that encoding then decoding (or the inverse) produces
/// equivalent data, ensuring wire format compatibility between encoder/decoder.
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{None, Some}
import gleeunit/should

import claude_agent_sdk/control.{
  AcceptEdits, Allow, AllowAll, AllowOnce, BypassPermissions, CanUseTool,
  ControlRequest, ControlResponse, Default, Deny, Edit, Error as ControlError,
  HookCallback, HookError, HookRegistration, HookResponse, HookSkip, HookSuccess,
  Initialize, Interrupt, McpMessage, McpResponse, PermissionResponse, Plan,
  RewindFiles, SetModel, SetPermissionMode, Success,
}
import claude_agent_sdk/internal/control_decoder
import claude_agent_sdk/internal/control_encoder

// Helper to convert any value to Dynamic
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// ============================================================================
// OutgoingControlRequest Round-Trip Tests
// ============================================================================
// These test: encode_request -> parse JSON -> verify structure matches original

pub fn roundtrip_initialize_request_test() {
  let hooks = [
    HookRegistration(hook_id: "hook_0", event: "PreToolUse", filter: None),
    HookRegistration(
      hook_id: "hook_1",
      event: "PostToolUse",
      filter: Some("Bash"),
    ),
  ]
  let original =
    Initialize(
      request_id: "req_init_0",
      hooks: hooks,
      mcp_servers: ["server-a", "server-b"],
      enable_file_checkpointing: True,
    )

  let encoded = control_encoder.encode_request(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  // Verify top-level structure
  parsed |> get_string("type") |> should.equal(Ok("control_request"))
  parsed |> get_string("request_id") |> should.equal(Ok("req_init_0"))

  // Verify inner request
  let assert Ok(request) = get_object(parsed, "request")
  request |> get_string("subtype") |> should.equal(Ok("initialize"))
  request |> get_bool("enable_file_checkpointing") |> should.equal(Ok(True))

  // Verify mcp_servers preserved
  let assert Ok(servers) = get_string_array(request, "mcp_servers")
  servers |> should.equal(["server-a", "server-b"])

  // Verify hooks structure preserved
  let assert Ok(hooks_obj) = get_object(request, "hooks")
  let assert Ok(pre_tool) = get_array(hooks_obj, "PreToolUse")
  pre_tool |> list_length |> should.equal(1)
  let assert Ok(post_tool) = get_array(hooks_obj, "PostToolUse")
  post_tool |> list_length |> should.equal(1)
}

pub fn roundtrip_initialize_empty_hooks_test() {
  let original =
    Initialize(
      request_id: "req_empty",
      hooks: [],
      mcp_servers: [],
      enable_file_checkpointing: False,
    )

  let encoded = control_encoder.encode_request(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  let assert Ok(request) = get_object(parsed, "request")
  request |> get_bool("enable_file_checkpointing") |> should.equal(Ok(False))

  // Empty mcp_servers should be empty array
  let assert Ok(servers) = get_string_array(request, "mcp_servers")
  servers |> should.equal([])
}

pub fn roundtrip_interrupt_request_test() {
  let original = Interrupt(request_id: "req_int_1")

  let encoded = control_encoder.encode_request(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  parsed |> get_string("type") |> should.equal(Ok("control_request"))
  parsed |> get_string("request_id") |> should.equal(Ok("req_int_1"))

  let assert Ok(request) = get_object(parsed, "request")
  request |> get_string("subtype") |> should.equal(Ok("interrupt"))
}

pub fn roundtrip_set_permission_mode_all_variants_test() {
  // Test all permission modes round-trip correctly
  roundtrip_permission_mode(Default, "default")
  roundtrip_permission_mode(AcceptEdits, "acceptEdits")
  roundtrip_permission_mode(BypassPermissions, "bypassPermissions")
  roundtrip_permission_mode(Plan, "plan")
}

fn roundtrip_permission_mode(mode: control.PermissionMode, expected: String) {
  let original = SetPermissionMode(request_id: "req_pm", mode: mode)
  let encoded = control_encoder.encode_request(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  let assert Ok(request) = get_object(parsed, "request")
  request |> get_string("subtype") |> should.equal(Ok("set_permission_mode"))
  request |> get_string("mode") |> should.equal(Ok(expected))
}

pub fn roundtrip_set_model_request_test() {
  let original = SetModel(request_id: "req_mod_1", model: "claude-3-opus")

  let encoded = control_encoder.encode_request(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  parsed |> get_string("request_id") |> should.equal(Ok("req_mod_1"))

  let assert Ok(request) = get_object(parsed, "request")
  request |> get_string("subtype") |> should.equal(Ok("set_model"))
  request |> get_string("model") |> should.equal(Ok("claude-3-opus"))
}

pub fn roundtrip_rewind_files_request_test() {
  let original =
    RewindFiles(request_id: "req_rw_1", user_message_id: "user_msg_42")

  let encoded = control_encoder.encode_request(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  parsed |> get_string("request_id") |> should.equal(Ok("req_rw_1"))

  let assert Ok(request) = get_object(parsed, "request")
  request |> get_string("subtype") |> should.equal(Ok("rewind_files"))
  request |> get_string("user_message_id") |> should.equal(Ok("user_msg_42"))
}

// ============================================================================
// OutgoingControlResponse Round-Trip Tests
// ============================================================================
// These test: encode_response -> parse JSON -> verify structure

pub fn roundtrip_hook_response_success_test() {
  let output =
    to_dynamic([
      #("continue", to_dynamic(True)),
      #("result", to_dynamic("done")),
    ])
  let original =
    HookResponse(request_id: "cli_hr_1", result: HookSuccess(output))

  let encoded = control_encoder.encode_response(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  parsed |> get_string("type") |> should.equal(Ok("control_response"))

  let assert Ok(response) = get_object(parsed, "response")
  response |> get_string("subtype") |> should.equal(Ok("success"))
  response |> get_string("request_id") |> should.equal(Ok("cli_hr_1"))

  // Verify inner response has our data
  let assert Ok(inner) = get_object(response, "response")
  inner |> get_bool("continue") |> should.equal(Ok(True))
  inner |> get_string("result") |> should.equal(Ok("done"))
}

pub fn roundtrip_hook_response_error_test() {
  let original =
    HookResponse(
      request_id: "cli_hr_2",
      result: HookError("Hook execution failed"),
    )

  let encoded = control_encoder.encode_response(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  let assert Ok(response) = get_object(parsed, "response")
  response |> get_string("subtype") |> should.equal(Ok("error"))
  response |> get_string("request_id") |> should.equal(Ok("cli_hr_2"))
  response |> get_string("message") |> should.equal(Ok("Hook execution failed"))
}

pub fn roundtrip_hook_response_skip_test() {
  let original =
    HookResponse(
      request_id: "cli_hr_3",
      result: HookSkip("Operation cancelled"),
    )

  let encoded = control_encoder.encode_response(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  let assert Ok(response) = get_object(parsed, "response")
  response |> get_string("subtype") |> should.equal(Ok("success"))
  response |> get_string("request_id") |> should.equal(Ok("cli_hr_3"))

  let assert Ok(inner) = get_object(response, "response")
  inner |> get_bool("continue") |> should.equal(Ok(False))
  inner |> get_string("stopReason") |> should.equal(Ok("Operation cancelled"))
}

pub fn roundtrip_permission_response_all_variants_test() {
  // Test all permission results
  roundtrip_permission_result(Allow, "allow")
  roundtrip_permission_result(Deny, "deny")
  roundtrip_permission_result(AllowOnce, "allowOnce")
  roundtrip_permission_result(AllowAll, "allowAll")
}

fn roundtrip_permission_result(
  result: control.PermissionResult,
  expected_behavior: String,
) {
  let original = PermissionResponse(request_id: "cli_pr", result: result)
  let encoded = control_encoder.encode_response(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  let assert Ok(response) = get_object(parsed, "response")
  response |> get_string("subtype") |> should.equal(Ok("success"))

  let assert Ok(inner) = get_object(response, "response")
  inner |> get_string("behavior") |> should.equal(Ok(expected_behavior))
}

pub fn roundtrip_permission_response_edit_test() {
  let modified =
    to_dynamic([#("command", to_dynamic("ls -la")), #("safe", to_dynamic(True))])
  let original =
    PermissionResponse(request_id: "cli_pr_edit", result: Edit(modified))

  let encoded = control_encoder.encode_response(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  let assert Ok(response) = get_object(parsed, "response")
  let assert Ok(inner) = get_object(response, "response")
  inner |> get_string("behavior") |> should.equal(Ok("edit"))

  let assert Ok(modified_input) = get_object(inner, "modified_input")
  modified_input |> get_string("command") |> should.equal(Ok("ls -la"))
  modified_input |> get_bool("safe") |> should.equal(Ok(True))
}

pub fn roundtrip_mcp_response_test() {
  let data =
    to_dynamic([#("result", to_dynamic("success")), #("count", to_dynamic(42))])
  let original = McpResponse(request_id: "cli_mcp_1", response: data)

  let encoded = control_encoder.encode_response(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  let assert Ok(response) = get_object(parsed, "response")
  response |> get_string("subtype") |> should.equal(Ok("success"))
  response |> get_string("request_id") |> should.equal(Ok("cli_mcp_1"))

  let assert Ok(inner) = get_object(response, "response")
  let assert Ok(mcp_resp) = get_object(inner, "mcp_response")
  mcp_resp |> get_string("result") |> should.equal(Ok("success"))
  mcp_resp |> get_int("count") |> should.equal(Ok(42))
}

// ============================================================================
// IncomingControlRequest Round-Trip Tests
// ============================================================================
// Build JSON string -> decode -> verify type matches expected

pub fn roundtrip_incoming_hook_callback_test() {
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_in_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"cb_0\",\"input\":{\"tool_name\":\"Bash\"},\"tool_use_id\":\"tu_123\"}}"

  let assert Ok(ControlRequest(HookCallback(
    request_id,
    callback_id,
    _input,
    tool_use_id,
  ))) = control_decoder.decode_line(json)

  request_id |> should.equal("cli_in_1")
  callback_id |> should.equal("cb_0")
  tool_use_id |> should.equal(Some("tu_123"))
}

pub fn roundtrip_incoming_hook_callback_no_tool_use_id_test() {
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_in_2\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"cb_1\",\"input\":{}}}"

  let assert Ok(ControlRequest(HookCallback(
    request_id,
    callback_id,
    _input,
    tool_use_id,
  ))) = control_decoder.decode_line(json)

  request_id |> should.equal("cli_in_2")
  callback_id |> should.equal("cb_1")
  tool_use_id |> should.equal(None)
}

pub fn roundtrip_incoming_can_use_tool_test() {
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_in_3\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"Write\",\"input\":{\"path\":\"/tmp/test\"},\"permission_suggestions\":[\"allow\",\"deny\"],\"blocked_path\":\"/etc\"}}"

  let assert Ok(ControlRequest(CanUseTool(
    request_id,
    tool_name,
    _input,
    suggestions,
    blocked_path,
  ))) = control_decoder.decode_line(json)

  request_id |> should.equal("cli_in_3")
  tool_name |> should.equal("Write")
  suggestions |> should.equal(["allow", "deny"])
  blocked_path |> should.equal(Some("/etc"))
}

pub fn roundtrip_incoming_can_use_tool_no_blocked_test() {
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_in_4\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"Read\",\"input\":{},\"permission_suggestions\":[]}}"

  let assert Ok(ControlRequest(CanUseTool(
    request_id,
    tool_name,
    _input,
    suggestions,
    blocked_path,
  ))) = control_decoder.decode_line(json)

  request_id |> should.equal("cli_in_4")
  tool_name |> should.equal("Read")
  suggestions |> should.equal([])
  blocked_path |> should.equal(None)
}

pub fn roundtrip_incoming_mcp_message_test() {
  let json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_in_5\",\"request\":{\"subtype\":\"mcp_message\",\"server_name\":\"my-mcp-server\",\"message\":{\"method\":\"tools/list\"}}}"

  let assert Ok(ControlRequest(McpMessage(request_id, server_name, _message))) =
    control_decoder.decode_line(json)

  request_id |> should.equal("cli_in_5")
  server_name |> should.equal("my-mcp-server")
}

// ============================================================================
// IncomingControlResponse Round-Trip Tests
// ============================================================================

pub fn roundtrip_incoming_success_response_test() {
  let json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"status\":\"ok\"}}}"

  let assert Ok(ControlResponse(Success(request_id, _payload))) =
    control_decoder.decode_line(json)

  request_id |> should.equal("req_0")
}

pub fn roundtrip_incoming_success_toplevel_request_id_test() {
  // Tolerant decoding: request_id at top level
  let json =
    "{\"type\":\"control_response\",\"request_id\":\"req_top\",\"response\":{\"subtype\":\"success\",\"response\":{}}}"

  let assert Ok(ControlResponse(Success(request_id, _payload))) =
    control_decoder.decode_line(json)

  request_id |> should.equal("req_top")
}

pub fn roundtrip_incoming_error_response_test() {
  let json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req_err\",\"error\":\"Something went wrong\"}}"

  let assert Ok(ControlResponse(ControlError(request_id, message))) =
    control_decoder.decode_line(json)

  request_id |> should.equal("req_err")
  message |> should.equal("Something went wrong")
}

pub fn roundtrip_incoming_error_message_field_test() {
  // Tolerant decoding: "message" field instead of "error"
  let json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req_err2\",\"message\":\"Alt format error\"}}"

  let assert Ok(ControlResponse(ControlError(request_id, message))) =
    control_decoder.decode_line(json)

  request_id |> should.equal("req_err2")
  message |> should.equal("Alt format error")
}

// ============================================================================
// Cross-encode/decode Integration Tests
// ============================================================================
// These verify that encoded outgoing responses can be re-parsed correctly

pub fn integration_encode_decode_hook_success_test() {
  // Encode an outgoing response, then decode it as if it were incoming
  let output = to_dynamic([#("continue", True)])
  let outgoing = HookResponse(request_id: "int_1", result: HookSuccess(output))

  let encoded = control_encoder.encode_response(outgoing)

  // The encoded format is control_response which should decode as ControlResponse
  let assert Ok(ControlResponse(Success(req_id, _payload))) =
    control_decoder.decode_line(encoded)

  req_id |> should.equal("int_1")
}

pub fn integration_encode_decode_hook_error_test() {
  let outgoing = HookResponse(request_id: "int_2", result: HookError("Failed"))
  let encoded = control_encoder.encode_response(outgoing)

  let assert Ok(ControlResponse(ControlError(req_id, msg))) =
    control_decoder.decode_line(encoded)

  req_id |> should.equal("int_2")
  msg |> should.equal("Failed")
}

pub fn integration_encode_decode_permission_response_test() {
  let outgoing = PermissionResponse(request_id: "int_3", result: Allow)
  let encoded = control_encoder.encode_response(outgoing)

  let assert Ok(ControlResponse(Success(req_id, _payload))) =
    control_decoder.decode_line(encoded)

  req_id |> should.equal("int_3")
}

pub fn integration_encode_decode_mcp_response_test() {
  let data = to_dynamic([#("tools", [])])
  let outgoing = McpResponse(request_id: "int_4", response: data)
  let encoded = control_encoder.encode_response(outgoing)

  let assert Ok(ControlResponse(Success(req_id, _payload))) =
    control_decoder.decode_line(encoded)

  req_id |> should.equal("int_4")
}

// ============================================================================
// Edge Cases
// ============================================================================

pub fn roundtrip_special_characters_in_strings_test() {
  // Test strings with special JSON characters
  let original =
    SetModel(
      request_id: "req_special",
      model: "model-with-\"quotes\"-and\\backslash",
    )

  let encoded = control_encoder.encode_request(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  let assert Ok(request) = get_object(parsed, "request")
  request
  |> get_string("model")
  |> should.equal(Ok("model-with-\"quotes\"-and\\backslash"))
}

pub fn roundtrip_unicode_in_strings_test() {
  let original =
    HookResponse(request_id: "req_unicode", result: HookError("エラー: 失敗"))
  let encoded = control_encoder.encode_response(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  let assert Ok(response) = get_object(parsed, "response")
  response |> get_string("message") |> should.equal(Ok("エラー: 失敗"))
}

pub fn roundtrip_empty_string_values_test() {
  let original = SetModel(request_id: "", model: "")
  let encoded = control_encoder.encode_request(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  parsed |> get_string("request_id") |> should.equal(Ok(""))

  let assert Ok(request) = get_object(parsed, "request")
  request |> get_string("model") |> should.equal(Ok(""))
}

pub fn roundtrip_complex_nested_dynamic_test() {
  // Test complex nested structure in Dynamic - use separate to_dynamic calls for different types
  let nested =
    to_dynamic([
      #("level1", to_dynamic([#("level2", to_dynamic([#("level3", "deep")]))])),
      #("array", to_dynamic([1, 2, 3])),
    ])
  let original =
    HookResponse(request_id: "req_nested", result: HookSuccess(nested))

  let encoded = control_encoder.encode_response(original)
  let assert Ok(parsed) = json.parse(encoded, decode.dynamic)

  // Just verify it parses - complex nesting preserved
  let assert Ok(response) = get_object(parsed, "response")
  response |> get_string("subtype") |> should.equal(Ok("success"))
}

// ============================================================================
// Test Helpers
// ============================================================================

fn get_string(dyn: Dynamic, key: String) -> Result(String, Nil) {
  let decoder = {
    use value <- decode.field(key, decode.string)
    decode.success(value)
  }
  case decode.run(dyn, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn get_bool(dyn: Dynamic, key: String) -> Result(Bool, Nil) {
  let decoder = {
    use value <- decode.field(key, decode.bool)
    decode.success(value)
  }
  case decode.run(dyn, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn get_int(dyn: Dynamic, key: String) -> Result(Int, Nil) {
  let decoder = {
    use value <- decode.field(key, decode.int)
    decode.success(value)
  }
  case decode.run(dyn, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn get_object(dyn: Dynamic, key: String) -> Result(Dynamic, Nil) {
  let decoder = {
    use value <- decode.field(key, decode.dynamic)
    decode.success(value)
  }
  case decode.run(dyn, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn get_array(dyn: Dynamic, key: String) -> Result(List(Dynamic), Nil) {
  let decoder = {
    use value <- decode.field(key, decode.list(decode.dynamic))
    decode.success(value)
  }
  case decode.run(dyn, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn get_string_array(dyn: Dynamic, key: String) -> Result(List(String), Nil) {
  let decoder = {
    use value <- decode.field(key, decode.list(decode.string))
    decode.success(value)
  }
  case decode.run(dyn, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn list_length(list: List(a)) -> Int {
  case list {
    [] -> 0
    [_, ..rest] -> 1 + list_length(rest)
  }
}
