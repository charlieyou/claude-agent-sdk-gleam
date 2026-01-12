/// Tests for control message JSON encoder.
///
/// Tests encode_request and encode_response produce correct NDJSON output
/// matching the wire format specification.
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

import claude_agent_sdk/control.{
  AcceptEdits, Allow, AllowAll, AllowOnce, BypassPermissions, Default, Deny,
  Edit, HookError, HookRegistration, HookResponse, HookSkip, HookSuccess,
  Initialize, Interrupt, McpResponse, PermissionResponse, Plan, RewindFiles,
  SetModel, SetPermissionMode,
}
import claude_agent_sdk/internal/control_encoder

// Helper to convert any value to Dynamic
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// ============================================================================
// OutgoingControlRequest Encoder Tests
// ============================================================================

pub fn encode_initialize_request_test() {
  let hooks = [
    HookRegistration(hook_id: "hook_0", event: "PreToolUse", filter: None),
  ]
  let req =
    Initialize(
      request_id: "req_0",
      hooks: hooks,
      mcp_servers: ["my-server"],
      enable_file_checkpointing: True,
    )
  let json_str = control_encoder.encode_request(req)

  // Should be valid single-line JSON
  json_str |> string.contains("\n") |> should.be_false

  // Parse and verify structure
  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  parsed |> get_string("type") |> should.equal(Ok("control_request"))
  parsed |> get_string("request_id") |> should.equal(Ok("req_0"))

  // Verify nested request object
  let assert Ok(request) = get_object(parsed, "request")
  request |> get_string("subtype") |> should.equal(Ok("initialize"))
  request |> get_bool("enable_file_checkpointing") |> should.equal(Ok(True))

  // Verify hooks is object keyed by event
  let assert Ok(hooks_obj) = get_object(request, "hooks")
  let assert Ok(pre_tool_use) = get_array(hooks_obj, "PreToolUse")
  // Should have one hook entry
  pre_tool_use |> should.not_equal([])
}

pub fn encode_initialize_hooks_wire_format_test() {
  // Test wire format: {"PreToolUse":[{"matcher":null,"hookCallbackIds":["hook_0"]}]}
  let hooks = [
    HookRegistration(hook_id: "hook_0", event: "PreToolUse", filter: None),
  ]
  let req =
    Initialize(
      request_id: "req_0",
      hooks: hooks,
      mcp_servers: [],
      enable_file_checkpointing: False,
    )
  let json_str = control_encoder.encode_request(req)

  // Verify wire format structure
  json_str |> string.contains("\"PreToolUse\"") |> should.be_true
  json_str |> string.contains("\"matcher\"") |> should.be_true
  json_str |> string.contains("\"hookCallbackIds\"") |> should.be_true
  json_str |> string.contains("\"hook_0\"") |> should.be_true
}

pub fn encode_initialize_hooks_with_matcher_test() {
  // Test hook with filter becomes matcher
  let hooks = [
    HookRegistration(
      hook_id: "hook_0",
      event: "PreToolUse",
      filter: Some("Bash"),
    ),
  ]
  let req =
    Initialize(
      request_id: "req_0",
      hooks: hooks,
      mcp_servers: [],
      enable_file_checkpointing: False,
    )
  let json_str = control_encoder.encode_request(req)

  // Verify matcher contains the filter value
  json_str |> string.contains("\"matcher\":\"Bash\"") |> should.be_true
}

pub fn encode_initialize_multiple_hooks_same_event_test() {
  // Multiple hooks for same event should group together
  let hooks = [
    HookRegistration(hook_id: "hook_0", event: "PreToolUse", filter: None),
    HookRegistration(
      hook_id: "hook_1",
      event: "PreToolUse",
      filter: Some("Read"),
    ),
  ]
  let req =
    Initialize(
      request_id: "req_0",
      hooks: hooks,
      mcp_servers: [],
      enable_file_checkpointing: False,
    )
  let json_str = control_encoder.encode_request(req)

  // Both hook IDs should be present under PreToolUse
  json_str |> string.contains("\"hook_0\"") |> should.be_true
  json_str |> string.contains("\"hook_1\"") |> should.be_true
}

pub fn encode_interrupt_request_test() {
  let req = Interrupt(request_id: "req_1")
  let json_str = control_encoder.encode_request(req)

  json_str |> string.contains("\n") |> should.be_false

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  parsed |> get_string("type") |> should.equal(Ok("control_request"))
  parsed |> get_string("request_id") |> should.equal(Ok("req_1"))

  let assert Ok(request) = get_object(parsed, "request")
  request |> get_string("subtype") |> should.equal(Ok("interrupt"))
}

pub fn encode_set_permission_mode_request_test() {
  let req = SetPermissionMode(request_id: "req_2", mode: AcceptEdits)
  let json_str = control_encoder.encode_request(req)

  json_str |> string.contains("\n") |> should.be_false

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  parsed |> get_string("type") |> should.equal(Ok("control_request"))

  let assert Ok(request) = get_object(parsed, "request")
  request |> get_string("subtype") |> should.equal(Ok("set_permission_mode"))
  request |> get_string("mode") |> should.equal(Ok("acceptEdits"))
}

pub fn encode_set_permission_mode_all_variants_test() {
  // Test all PermissionMode variants encode correctly
  let test_mode = fn(mode, expected_str) {
    let req = SetPermissionMode(request_id: "req", mode: mode)
    let json_str = control_encoder.encode_request(req)
    let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
    let assert Ok(request) = get_object(parsed, "request")
    request |> get_string("mode") |> should.equal(Ok(expected_str))
  }

  test_mode(Default, "default")
  test_mode(AcceptEdits, "acceptEdits")
  test_mode(BypassPermissions, "bypassPermissions")
  test_mode(Plan, "plan")
}

pub fn encode_set_model_request_test() {
  let req = SetModel(request_id: "req_3", model: "sonnet")
  let json_str = control_encoder.encode_request(req)

  json_str |> string.contains("\n") |> should.be_false

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  parsed |> get_string("type") |> should.equal(Ok("control_request"))

  let assert Ok(request) = get_object(parsed, "request")
  request |> get_string("subtype") |> should.equal(Ok("set_model"))
  request |> get_string("model") |> should.equal(Ok("sonnet"))
}

pub fn encode_rewind_files_request_test() {
  let req = RewindFiles(request_id: "req_4", user_message_id: "msg_123")
  let json_str = control_encoder.encode_request(req)

  json_str |> string.contains("\n") |> should.be_false

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  parsed |> get_string("type") |> should.equal(Ok("control_request"))

  let assert Ok(request) = get_object(parsed, "request")
  request |> get_string("subtype") |> should.equal(Ok("rewind_files"))
  request |> get_string("user_message_id") |> should.equal(Ok("msg_123"))
}

// ============================================================================
// OutgoingControlResponse Encoder Tests
// ============================================================================

pub fn encode_hook_response_success_test() {
  let output = to_dynamic([#("continue", True)])
  let resp = HookResponse(request_id: "cli_1", result: HookSuccess(output))
  let json_str = control_encoder.encode_response(resp)

  json_str |> string.contains("\n") |> should.be_false

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  parsed |> get_string("type") |> should.equal(Ok("control_response"))

  let assert Ok(response) = get_object(parsed, "response")
  response |> get_string("subtype") |> should.equal(Ok("success"))
  response |> get_string("request_id") |> should.equal(Ok("cli_1"))

  // Verify the response contains continue:true (as boolean, not string)
  let assert Ok(inner_response) = get_object(response, "response")
  inner_response |> get_bool("continue") |> should.equal(Ok(True))
}

pub fn encode_hook_response_error_test() {
  let resp = HookResponse(request_id: "cli_2", result: HookError("Hook failed"))
  let json_str = control_encoder.encode_response(resp)

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  parsed |> get_string("type") |> should.equal(Ok("control_response"))

  let assert Ok(response) = get_object(parsed, "response")
  response |> get_string("subtype") |> should.equal(Ok("error"))
  response |> get_string("message") |> should.equal(Ok("Hook failed"))
}

pub fn encode_hook_response_skip_test() {
  // HookSkip encodes as success with continue:false and stopReason
  let resp =
    HookResponse(request_id: "cli_3", result: HookSkip("User cancelled"))
  let json_str = control_encoder.encode_response(resp)

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  parsed |> get_string("type") |> should.equal(Ok("control_response"))

  let assert Ok(response) = get_object(parsed, "response")
  // HookSkip now encodes as success subtype
  response |> get_string("subtype") |> should.equal(Ok("success"))
  response |> get_string("request_id") |> should.equal(Ok("cli_3"))

  // Verify response contains continue:false and stopReason
  let assert Ok(inner_response) = get_object(response, "response")
  inner_response |> get_bool("continue") |> should.equal(Ok(False))
  inner_response
  |> get_string("stopReason")
  |> should.equal(Ok("User cancelled"))
}

pub fn encode_permission_response_allow_test() {
  let resp = PermissionResponse(request_id: "cli_4", result: Allow)
  let json_str = control_encoder.encode_response(resp)

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  let assert Ok(response) = get_object(parsed, "response")
  response |> get_string("subtype") |> should.equal(Ok("success"))

  let assert Ok(inner_resp) = get_object(response, "response")
  inner_resp |> get_string("behavior") |> should.equal(Ok("allow"))
}

pub fn encode_permission_response_deny_test() {
  let resp = PermissionResponse(request_id: "cli_5", result: Deny)
  let json_str = control_encoder.encode_response(resp)

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  let assert Ok(response) = get_object(parsed, "response")
  let assert Ok(inner_resp) = get_object(response, "response")
  inner_resp |> get_string("behavior") |> should.equal(Ok("deny"))
}

pub fn encode_permission_response_allow_once_test() {
  let resp = PermissionResponse(request_id: "cli_6", result: AllowOnce)
  let json_str = control_encoder.encode_response(resp)

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  let assert Ok(response) = get_object(parsed, "response")
  let assert Ok(inner_resp) = get_object(response, "response")
  inner_resp |> get_string("behavior") |> should.equal(Ok("allowOnce"))
}

pub fn encode_permission_response_allow_all_test() {
  let resp = PermissionResponse(request_id: "cli_7", result: AllowAll)
  let json_str = control_encoder.encode_response(resp)

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  let assert Ok(response) = get_object(parsed, "response")
  let assert Ok(inner_resp) = get_object(response, "response")
  inner_resp |> get_string("behavior") |> should.equal(Ok("allowAll"))
}

pub fn encode_permission_response_edit_test() {
  let modified = to_dynamic([#("command", "ls -la")])
  let resp = PermissionResponse(request_id: "cli_8", result: Edit(modified))
  let json_str = control_encoder.encode_response(resp)

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  let assert Ok(response) = get_object(parsed, "response")
  let assert Ok(inner_resp) = get_object(response, "response")
  inner_resp |> get_string("behavior") |> should.equal(Ok("edit"))
}

pub fn encode_mcp_response_test() {
  let mcp_data = to_dynamic([#("result", "success")])
  let resp = McpResponse(request_id: "cli_9", response: mcp_data)
  let json_str = control_encoder.encode_response(resp)

  let assert Ok(parsed) = json.parse(json_str, dynamic_decoder())
  parsed |> get_string("type") |> should.equal(Ok("control_response"))

  let assert Ok(response) = get_object(parsed, "response")
  response |> get_string("subtype") |> should.equal(Ok("success"))
  response |> get_string("request_id") |> should.equal(Ok("cli_9"))
}

// ============================================================================
// Dynamic Encoding Tests (verify FFI handles Gleam types correctly)
// ============================================================================

pub fn encode_dynamic_boolean_test() {
  // Verify booleans are encoded as JSON booleans, not strings
  let output = to_dynamic([#("flag", True)])
  let resp = HookResponse(request_id: "test", result: HookSuccess(output))
  let json_str = control_encoder.encode_response(resp)

  // Should contain true as JSON boolean, not "true" as string
  json_str |> string.contains(":true") |> should.be_true
  // Should NOT contain the string "true" (with quotes)
  json_str |> string.contains(":\"true\"") |> should.be_false
}

pub fn encode_dynamic_null_test() {
  // Verify nil encodes as JSON null
  let output = to_dynamic([#("value", Nil)])
  let resp = HookResponse(request_id: "test", result: HookSuccess(output))
  let json_str = control_encoder.encode_response(resp)

  // Should contain null as JSON null
  json_str |> string.contains(":null") |> should.be_true
}

pub fn encode_dynamic_empty_list_test() {
  // Verify empty list encodes as JSON array [], not object {}
  let output = to_dynamic([#("items", [])])
  let resp = HookResponse(request_id: "test", result: HookSuccess(output))
  let json_str = control_encoder.encode_response(resp)

  // Should contain [] as JSON array
  json_str |> string.contains(":[]") |> should.be_true
  // Should NOT contain {} (empty object)
  json_str |> string.contains(":{}") |> should.be_false
}

pub fn encode_dynamic_tuple_test() {
  // Verify non-proplist tuples encode as JSON arrays
  let output = to_dynamic([#("pair", #(1, 2))])
  let resp = HookResponse(request_id: "test", result: HookSuccess(output))
  let json_str = control_encoder.encode_response(resp)

  // Tuple should become array [1,2]
  json_str |> string.contains("[1,2]") |> should.be_true
}

// ============================================================================
// Test Helpers
// ============================================================================

fn dynamic_decoder() -> decode.Decoder(Dynamic) {
  decode.dynamic
}

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
