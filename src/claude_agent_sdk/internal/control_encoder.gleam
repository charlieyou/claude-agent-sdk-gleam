/// JSON encoder for outgoing control messages.
///
/// Produces NDJSON format for wire transmission to Claude CLI.
/// Each encoded message is a single-line JSON string without trailing newline.
import gleam/dynamic.{type Dynamic}
import gleam/json.{type Json}

import claude_agent_sdk/control.{
  type HookRegistration, type HookResult, type OutgoingControlRequest,
  type OutgoingControlResponse, type PermissionMode, type PermissionResult,
  AcceptEdits, Allow, AllowAll, AllowOnce, BypassPermissions, Default, Deny,
  Edit, HookError, HookRegistration, HookResponse, HookSkip, HookSuccess,
  Initialize, Interrupt, McpResponse, PermissionResponse, Plan, RewindFiles,
  SetModel, SetPermissionMode,
}

/// Encode outgoing control request to JSON string (no trailing newline).
pub fn encode_request(request: OutgoingControlRequest) -> String {
  request
  |> encode_request_json
  |> json.to_string
}

/// Encode outgoing control response to JSON string (no trailing newline).
pub fn encode_response(response: OutgoingControlResponse) -> String {
  response
  |> encode_response_json
  |> json.to_string
}

// ============================================================================
// Request Encoding
// ============================================================================

fn encode_request_json(request: OutgoingControlRequest) -> Json {
  let #(request_id, inner) = case request {
    Initialize(request_id, hooks, mcp_servers, enable_file_checkpointing) -> #(
      request_id,
      encode_initialize_inner(hooks, mcp_servers, enable_file_checkpointing),
    )
    Interrupt(request_id) -> #(request_id, encode_interrupt_inner())
    SetPermissionMode(request_id, mode) -> #(
      request_id,
      encode_set_permission_mode_inner(mode),
    )
    SetModel(request_id, model) -> #(request_id, encode_set_model_inner(model))
    RewindFiles(request_id, user_message_id) -> #(
      request_id,
      encode_rewind_files_inner(user_message_id),
    )
  }

  json.object([
    #("type", json.string("control_request")),
    #("request_id", json.string(request_id)),
    #("request", inner),
  ])
}

fn encode_initialize_inner(
  hooks: List(HookRegistration),
  mcp_servers: List(String),
  enable_file_checkpointing: Bool,
) -> Json {
  json.object([
    #("subtype", json.string("initialize")),
    #("hooks", json.array(hooks, encode_hook_registration)),
    #("mcp_servers", json.array(mcp_servers, json.string)),
    #("enable_file_checkpointing", json.bool(enable_file_checkpointing)),
  ])
}

fn encode_hook_registration(reg: HookRegistration) -> Json {
  let HookRegistration(hook_id, event, filter) = reg
  json.object([
    #("hook_id", json.string(hook_id)),
    #("event", json.string(event)),
    #("filter", json.nullable(filter, json.string)),
  ])
}

fn encode_interrupt_inner() -> Json {
  json.object([#("subtype", json.string("interrupt"))])
}

fn encode_set_permission_mode_inner(mode: PermissionMode) -> Json {
  json.object([
    #("subtype", json.string("set_permission_mode")),
    #("mode", json.string(encode_permission_mode(mode))),
  ])
}

fn encode_permission_mode(mode: PermissionMode) -> String {
  case mode {
    Default -> "default"
    AcceptEdits -> "acceptEdits"
    BypassPermissions -> "bypassPermissions"
    Plan -> "plan"
  }
}

fn encode_set_model_inner(model: String) -> Json {
  json.object([
    #("subtype", json.string("set_model")),
    #("model", json.string(model)),
  ])
}

fn encode_rewind_files_inner(user_message_id: String) -> Json {
  json.object([
    #("subtype", json.string("rewind_files")),
    #("user_message_id", json.string(user_message_id)),
  ])
}

// ============================================================================
// Response Encoding
// ============================================================================

fn encode_response_json(response: OutgoingControlResponse) -> Json {
  let inner = case response {
    HookResponse(request_id, result) ->
      encode_hook_response_inner(request_id, result)
    PermissionResponse(request_id, result) ->
      encode_permission_response_inner(request_id, result)
    McpResponse(request_id, response_data) ->
      encode_mcp_response_inner(request_id, response_data)
  }

  json.object([#("type", json.string("control_response")), #("response", inner)])
}

fn encode_hook_response_inner(request_id: String, result: HookResult) -> Json {
  case result {
    HookSuccess(output) ->
      json.object([
        #("subtype", json.string("success")),
        #("request_id", json.string(request_id)),
        #("response", dynamic_to_json(output)),
      ])
    HookError(message) ->
      json.object([
        #("subtype", json.string("error")),
        #("request_id", json.string(request_id)),
        #("message", json.string(message)),
      ])
    HookSkip(reason) ->
      json.object([
        #("subtype", json.string("skip")),
        #("request_id", json.string(request_id)),
        #("reason", json.string(reason)),
      ])
  }
}

fn encode_permission_response_inner(
  request_id: String,
  result: PermissionResult,
) -> Json {
  let response_obj = case result {
    Allow -> json.object([#("behavior", json.string("allow"))])
    Deny -> json.object([#("behavior", json.string("deny"))])
    AllowOnce -> json.object([#("behavior", json.string("allowOnce"))])
    AllowAll -> json.object([#("behavior", json.string("allowAll"))])
    Edit(modified_input) ->
      json.object([
        #("behavior", json.string("edit")),
        #("modified_input", dynamic_to_json(modified_input)),
      ])
  }

  json.object([
    #("subtype", json.string("success")),
    #("request_id", json.string(request_id)),
    #("response", response_obj),
  ])
}

fn encode_mcp_response_inner(request_id: String, response: Dynamic) -> Json {
  json.object([
    #("subtype", json.string("success")),
    #("request_id", json.string(request_id)),
    #("response", json.object([#("mcp_response", dynamic_to_json(response))])),
  ])
}

// ============================================================================
// Dynamic Encoding
// ============================================================================

/// Convert a Dynamic value to Json.
/// Uses Erlang's json:encode with custom encoder for Gleam types.
@external(erlang, "control_encoder_ffi", "encode_dynamic")
fn dynamic_to_json(value: Dynamic) -> Json
