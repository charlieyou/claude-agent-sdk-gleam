/// Unit tests for bidir/hooks.gleam - pure hook dispatch logic.
///
/// Tests the pure hook configuration and dispatch functions without
/// spawning any processes. Verifies:
/// - empty_hook_config creates valid empty configuration
/// - dispatch_hook finds handlers by callback_id
/// - dispatch_permission finds handlers by tool_name
/// - fail-open (HookType) vs fail-deny (PermissionType) semantics
import gleam/dict
import gleam/dynamic
import gleeunit/should

import claude_agent_sdk/internal/bidir/hooks.{
  DispatchNoHandler, DispatchOk, HookConfig, HookType, PermissionType,
  dispatch_hook, dispatch_permission, empty_hook_config,
}

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> dynamic.Dynamic

// =============================================================================
// empty_hook_config Tests
// =============================================================================

pub fn empty_hook_config_has_no_handlers_test() {
  let config = empty_hook_config()
  should.equal(dict.size(config.handlers), 0)
}

pub fn empty_hook_config_has_no_permission_handlers_test() {
  let config = empty_hook_config()
  should.equal(dict.size(config.permission_handlers), 0)
}

// =============================================================================
// dispatch_hook Tests
// =============================================================================

pub fn dispatch_hook_finds_registered_handler_test() {
  let handler = fn(_input: dynamic.Dynamic) -> dynamic.Dynamic {
    to_dynamic("result")
  }
  let config =
    HookConfig(
      handlers: dict.from_list([#("hook_0", handler)]),
      permission_handlers: dict.new(),
    )

  let result = dispatch_hook(config, "hook_0")
  case result {
    DispatchOk(_handler, callback_type) -> {
      should.equal(callback_type, HookType)
    }
    _ -> should.fail()
  }
}

pub fn dispatch_hook_returns_hook_type_test() {
  let handler = fn(_input: dynamic.Dynamic) -> dynamic.Dynamic {
    to_dynamic(True)
  }
  let config =
    HookConfig(
      handlers: dict.from_list([#("test_hook", handler)]),
      permission_handlers: dict.new(),
    )

  let result = dispatch_hook(config, "test_hook")
  case result {
    DispatchOk(_, callback_type) -> should.equal(callback_type, HookType)
    _ -> should.fail()
  }
}

pub fn dispatch_hook_returns_no_handler_for_unknown_id_test() {
  let config = empty_hook_config()
  let result = dispatch_hook(config, "unknown_hook")
  should.equal(result, DispatchNoHandler("unknown_hook"))
}

pub fn dispatch_hook_returns_no_handler_preserves_id_test() {
  let config = empty_hook_config()
  let result = dispatch_hook(config, "my_missing_callback")
  should.equal(result, DispatchNoHandler("my_missing_callback"))
}

// =============================================================================
// dispatch_permission Tests
// =============================================================================

pub fn dispatch_permission_finds_registered_handler_test() {
  let handler = fn(_input: dynamic.Dynamic) -> dynamic.Dynamic {
    to_dynamic("allow")
  }
  let config =
    HookConfig(
      handlers: dict.new(),
      permission_handlers: dict.from_list([#("bash", handler)]),
    )

  let result = dispatch_permission(config, "bash")
  case result {
    DispatchOk(_handler, callback_type) -> {
      should.equal(callback_type, PermissionType)
    }
    _ -> should.fail()
  }
}

pub fn dispatch_permission_returns_permission_type_test() {
  let handler = fn(_input: dynamic.Dynamic) -> dynamic.Dynamic {
    to_dynamic("deny")
  }
  let config =
    HookConfig(
      handlers: dict.new(),
      permission_handlers: dict.from_list([#("file_editor", handler)]),
    )

  let result = dispatch_permission(config, "file_editor")
  case result {
    DispatchOk(_, callback_type) -> should.equal(callback_type, PermissionType)
    _ -> should.fail()
  }
}

pub fn dispatch_permission_returns_no_handler_for_unknown_tool_test() {
  let config = empty_hook_config()
  let result = dispatch_permission(config, "unknown_tool")
  should.equal(result, DispatchNoHandler("unknown_tool"))
}

// =============================================================================
// Handler Invocation Tests
// =============================================================================

pub fn dispatch_hook_handler_can_be_invoked_test() {
  let handler = fn(input: dynamic.Dynamic) -> dynamic.Dynamic {
    // Return the input wrapped in a tuple to verify it was called
    to_dynamic(#("received", input))
  }
  let config =
    HookConfig(
      handlers: dict.from_list([#("invoke_test", handler)]),
      permission_handlers: dict.new(),
    )

  let result = dispatch_hook(config, "invoke_test")
  case result {
    DispatchOk(found_handler, _) -> {
      let output = found_handler(to_dynamic("test_input"))
      // Just verify we got some dynamic back
      should.be_true(dynamic.classify(output) != "Nil")
    }
    _ -> should.fail()
  }
}

pub fn dispatch_permission_handler_can_be_invoked_test() {
  let handler = fn(_input: dynamic.Dynamic) -> dynamic.Dynamic {
    to_dynamic("allowed")
  }
  let config =
    HookConfig(
      handlers: dict.new(),
      permission_handlers: dict.from_list([#("test_tool", handler)]),
    )

  let result = dispatch_permission(config, "test_tool")
  case result {
    DispatchOk(found_handler, _) -> {
      let output = found_handler(to_dynamic(Nil))
      should.be_true(dynamic.classify(output) != "Nil")
    }
    _ -> should.fail()
  }
}

// =============================================================================
// Multiple Handlers Tests
// =============================================================================

pub fn multiple_hooks_dispatched_correctly_test() {
  let handler1 = fn(_: dynamic.Dynamic) -> dynamic.Dynamic {
    to_dynamic("handler1")
  }
  let handler2 = fn(_: dynamic.Dynamic) -> dynamic.Dynamic {
    to_dynamic("handler2")
  }
  let config =
    HookConfig(
      handlers: dict.from_list([#("hook_a", handler1), #("hook_b", handler2)]),
      permission_handlers: dict.new(),
    )

  // Both hooks should be found
  case dispatch_hook(config, "hook_a") {
    DispatchOk(_, HookType) -> should.be_true(True)
    _ -> should.fail()
  }
  case dispatch_hook(config, "hook_b") {
    DispatchOk(_, HookType) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn hooks_and_permissions_dont_interfere_test() {
  let hook_handler = fn(_: dynamic.Dynamic) -> dynamic.Dynamic {
    to_dynamic("hook")
  }
  let perm_handler = fn(_: dynamic.Dynamic) -> dynamic.Dynamic {
    to_dynamic("perm")
  }
  let config =
    HookConfig(
      handlers: dict.from_list([#("test_id", hook_handler)]),
      permission_handlers: dict.from_list([#("test_id", perm_handler)]),
    )

  // Same ID in both maps should return appropriate type
  case dispatch_hook(config, "test_id") {
    DispatchOk(_, HookType) -> should.be_true(True)
    _ -> should.fail()
  }
  case dispatch_permission(config, "test_id") {
    DispatchOk(_, PermissionType) -> should.be_true(True)
    _ -> should.fail()
  }
}

// =============================================================================
// Fail-Open vs Fail-Deny Semantics (Type Indicator)
// =============================================================================

pub fn hook_dispatch_indicates_fail_open_semantics_test() {
  // HookType indicates fail-open: if handler crashes/times out, operation continues
  let handler = fn(_: dynamic.Dynamic) -> dynamic.Dynamic { to_dynamic(Nil) }
  let config =
    HookConfig(
      handlers: dict.from_list([#("hook", handler)]),
      permission_handlers: dict.new(),
    )

  case dispatch_hook(config, "hook") {
    DispatchOk(_, callback_type) -> {
      // HookType = fail-open semantics
      should.equal(callback_type, HookType)
    }
    _ -> should.fail()
  }
}

pub fn permission_dispatch_indicates_fail_deny_semantics_test() {
  // PermissionType indicates fail-deny: if handler crashes/times out, operation denied
  let handler = fn(_: dynamic.Dynamic) -> dynamic.Dynamic { to_dynamic(Nil) }
  let config =
    HookConfig(
      handlers: dict.new(),
      permission_handlers: dict.from_list([#("tool", handler)]),
    )

  case dispatch_permission(config, "tool") {
    DispatchOk(_, callback_type) -> {
      // PermissionType = fail-deny semantics
      should.equal(callback_type, PermissionType)
    }
    _ -> should.fail()
  }
}
