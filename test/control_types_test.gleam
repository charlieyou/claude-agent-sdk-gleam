/// Tests for control message type construction and pattern matching.
///
/// This module tests type-level construction for control protocol types.
/// The decode_line test is intentionally failing as a placeholder for
/// the decoder implementation (casg-de0.4).
import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}
import gleeunit/should

import claude_agent_sdk/control.{
  type HookResult, type IncomingMessage, type PermissionMode,
  type PermissionResult, AcceptEdits, Allow, AllowAll, AllowOnce,
  BypassPermissions, CanUseTool, ControlRequest, ControlResponse, Default, Deny,
  Edit, Error, HookCallback, HookError, HookRegistration, HookResponse, HookSkip,
  HookSuccess, Initialize, Interrupt, McpMessage, McpResponse,
  PermissionResponse, Plan, RegularMessage, RewindFiles, SetModel,
  SetPermissionMode, Success,
}
import claude_agent_sdk/message

// Helper to convert any value to Dynamic (identity function in Erlang)
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// ============================================================================
// OutgoingControlRequest Construction Tests
// ============================================================================

pub fn initialize_request_construction_test() {
  let hooks = [
    HookRegistration(hook_id: "hook1", event: "pre_tool", filter: Some("Bash")),
  ]
  let req =
    Initialize(
      request_id: "req-1",
      hooks: hooks,
      mcp_servers: ["server1"],
      enable_file_checkpointing: True,
    )
  case req {
    Initialize(id, h, servers, checkpointing) -> {
      id |> should.equal("req-1")
      h |> should.equal(hooks)
      servers |> should.equal(["server1"])
      checkpointing |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn interrupt_request_construction_test() {
  let req = Interrupt(request_id: "req-2")
  case req {
    Interrupt(id) -> id |> should.equal("req-2")
    _ -> should.fail()
  }
}

pub fn set_permission_mode_request_construction_test() {
  let req = SetPermissionMode(request_id: "req-3", mode: BypassPermissions)
  case req {
    SetPermissionMode(id, mode) -> {
      id |> should.equal("req-3")
      case mode {
        BypassPermissions -> should.be_true(True)
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn set_model_request_construction_test() {
  let req = SetModel(request_id: "req-4", model: "opus")
  case req {
    SetModel(id, model) -> {
      id |> should.equal("req-4")
      model |> should.equal("opus")
    }
    _ -> should.fail()
  }
}

pub fn rewind_files_request_construction_test() {
  let req = RewindFiles(request_id: "req-5", user_message_id: "msg-123")
  case req {
    RewindFiles(id, msg_id) -> {
      id |> should.equal("req-5")
      msg_id |> should.equal("msg-123")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// IncomingMessage Variant Tests
// ============================================================================

pub fn incoming_message_regular_variant_test() {
  // Construct a minimal system message to wrap
  let sys_msg =
    message.SystemMessage(
      subtype: Some("init"),
      uuid: None,
      session_id: None,
      cwd: None,
      model: None,
      tools: None,
      mcp_servers: None,
      permission_mode: None,
      api_key_source: None,
      slash_commands: None,
      agents: None,
      claude_code_version: None,
    )
  let msg: IncomingMessage = RegularMessage(message.System(sys_msg))
  case msg {
    RegularMessage(m) ->
      case m {
        message.System(_) -> should.be_true(True)
        _ -> should.fail()
      }
    _ -> should.fail()
  }
}

pub fn incoming_message_control_request_variant_test() {
  let ctrl_req =
    HookCallback(
      request_id: "cb-1",
      callback_id: "hook1",
      input: to_dynamic(Nil),
      tool_use_id: Some("tool-123"),
    )
  let msg: IncomingMessage = ControlRequest(ctrl_req)
  case msg {
    ControlRequest(r) ->
      case r {
        HookCallback(req_id, cb_id, _, tu_id) -> {
          req_id |> should.equal("cb-1")
          cb_id |> should.equal("hook1")
          tu_id |> should.equal(Some("tool-123"))
        }
        _ -> should.fail()
      }
    _ -> should.fail()
  }
}

pub fn incoming_message_control_response_variant_test() {
  let ctrl_resp = Success(request_id: "req-1", payload: to_dynamic("ok"))
  let msg: IncomingMessage = ControlResponse(ctrl_resp)
  case msg {
    ControlResponse(r) ->
      case r {
        Success(id, _) -> id |> should.equal("req-1")
        _ -> should.fail()
      }
    _ -> should.fail()
  }
}

// ============================================================================
// IncomingControlRequest Construction Tests
// ============================================================================

pub fn hook_callback_construction_test() {
  let req =
    HookCallback(
      request_id: "cb-2",
      callback_id: "my_hook",
      input: to_dynamic("test_input"),
      tool_use_id: None,
    )
  case req {
    HookCallback(req_id, cb_id, _, tu_id) -> {
      req_id |> should.equal("cb-2")
      cb_id |> should.equal("my_hook")
      tu_id |> should.equal(None)
    }
    _ -> should.fail()
  }
}

pub fn can_use_tool_construction_test() {
  let req =
    CanUseTool(
      request_id: "perm-1",
      tool_name: "Bash",
      input: to_dynamic("ls -la"),
      permission_suggestions: ["allow", "deny"],
      blocked_path: Some("/etc/passwd"),
    )
  case req {
    CanUseTool(req_id, tool, _, suggestions, blocked) -> {
      req_id |> should.equal("perm-1")
      tool |> should.equal("Bash")
      suggestions |> should.equal(["allow", "deny"])
      blocked |> should.equal(Some("/etc/passwd"))
    }
    _ -> should.fail()
  }
}

pub fn mcp_message_construction_test() {
  let req =
    McpMessage(
      request_id: "mcp-1",
      server_name: "git-server",
      message: to_dynamic("list_repos"),
    )
  case req {
    McpMessage(req_id, server, _) -> {
      req_id |> should.equal("mcp-1")
      server |> should.equal("git-server")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// IncomingControlResponse Construction Tests
// ============================================================================

pub fn incoming_success_response_test() {
  let resp = Success(request_id: "req-10", payload: to_dynamic(42))
  case resp {
    Success(id, _) -> id |> should.equal("req-10")
    Error(_, _) -> should.fail()
  }
}

pub fn incoming_error_response_test() {
  let resp = Error(request_id: "req-11", message: "Something went wrong")
  case resp {
    Error(id, msg) -> {
      id |> should.equal("req-11")
      msg |> should.equal("Something went wrong")
    }
    Success(_, _) -> should.fail()
  }
}

// ============================================================================
// OutgoingControlResponse Construction Tests
// ============================================================================

pub fn hook_response_construction_test() {
  let resp =
    HookResponse(request_id: "cb-1", result: HookSuccess(to_dynamic("done")))
  case resp {
    HookResponse(id, result) -> {
      id |> should.equal("cb-1")
      case result {
        HookSuccess(_) -> should.be_true(True)
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn permission_response_construction_test() {
  let resp = PermissionResponse(request_id: "perm-1", result: AllowOnce)
  case resp {
    PermissionResponse(id, result) -> {
      id |> should.equal("perm-1")
      case result {
        AllowOnce -> should.be_true(True)
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn mcp_response_construction_test() {
  let resp = McpResponse(request_id: "mcp-1", response: to_dynamic([1, 2, 3]))
  case resp {
    McpResponse(id, _) -> id |> should.equal("mcp-1")
    _ -> should.fail()
  }
}

// ============================================================================
// PermissionMode Variant Tests
// ============================================================================

pub fn permission_mode_all_variants_test() {
  // Verify all variants are constructable and matchable
  let modes: List(PermissionMode) = [
    Default,
    AcceptEdits,
    BypassPermissions,
    Plan,
  ]
  modes
  |> should.equal([Default, AcceptEdits, BypassPermissions, Plan])
}

// ============================================================================
// HookResult Variant Tests
// ============================================================================

pub fn hook_result_success_test() {
  let result: HookResult = HookSuccess(to_dynamic("output"))
  case result {
    HookSuccess(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn hook_result_error_test() {
  let result: HookResult = HookError("hook failed")
  case result {
    HookError(msg) -> msg |> should.equal("hook failed")
    _ -> should.fail()
  }
}

pub fn hook_result_skip_test() {
  let result: HookResult = HookSkip("user cancelled")
  case result {
    HookSkip(reason) -> reason |> should.equal("user cancelled")
    _ -> should.fail()
  }
}

// ============================================================================
// PermissionResult Variant Tests
// ============================================================================

pub fn permission_result_all_variants_test() {
  // Test Allow, Deny, AllowOnce, AllowAll
  let results: List(PermissionResult) = [Allow, Deny, AllowOnce, AllowAll]
  results |> should.equal([Allow, Deny, AllowOnce, AllowAll])
}

pub fn permission_result_edit_variant_test() {
  let result: PermissionResult = Edit(modified_input: to_dynamic("edited"))
  case result {
    Edit(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// HookRegistration Construction Tests
// ============================================================================

pub fn hook_registration_with_filter_test() {
  let reg =
    HookRegistration(hook_id: "h1", event: "pre_tool", filter: Some("Bash"))
  case reg {
    HookRegistration(hook_id, event, filter) -> {
      hook_id |> should.equal("h1")
      event |> should.equal("pre_tool")
      filter |> should.equal(Some("Bash"))
    }
  }
}

pub fn hook_registration_without_filter_test() {
  let reg = HookRegistration(hook_id: "h2", event: "on_message", filter: None)
  case reg {
    HookRegistration(hook_id, event, filter) -> {
      hook_id |> should.equal("h2")
      event |> should.equal("on_message")
      filter |> should.equal(None)
    }
  }
}
// ============================================================================
// Decoder Placeholder (casg-de0.4)
// ============================================================================
// decode_line tests will be added when the decoder is implemented in casg-de0.4.
// The decoder should:
// 1. Take a JSON line string
// 2. Return the correct IncomingMessage variant
