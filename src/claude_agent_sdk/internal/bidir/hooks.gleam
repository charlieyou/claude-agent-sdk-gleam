/// Pure hook types and dispatch signatures for bidirectional sessions.
///
/// This module contains the pure hook-related types that don't depend on OTP.
/// Actual hook execution with process spawning stays in bidir.gleam.
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}

/// Configuration for registered hooks and permission handlers.
///
/// This is a pure data type - no OTP dependencies.
pub type HookConfig {
  HookConfig(
    /// Map of callback_id -> handler function for hooks.
    handlers: Dict(String, fn(Dynamic) -> Dynamic),
    /// Map of tool_name -> permission handler for can_use_tool.
    /// Handler returns PermissionResult (Allow, Deny, etc.)
    permission_handlers: Dict(String, fn(Dynamic) -> Dynamic),
  )
}

/// Create an empty hook configuration.
pub fn empty_hook_config() -> HookConfig {
  HookConfig(handlers: dict.new(), permission_handlers: dict.new())
}

/// Callback type discriminator for fail-open vs fail-deny behavior.
///
/// Determines how timeout/crash errors are handled:
/// - HookType: fail-open (allow operation to continue)
/// - PermissionType: fail-deny (deny operation for security)
pub type CallbackType {
  /// Hook callbacks use fail-open semantics.
  HookType
  /// Permission callbacks use fail-deny semantics.
  PermissionType
}

/// Hook dispatch result indicating what action to take.
///
/// Returned by dispatch functions to tell the caller what to do.
pub type DispatchResult {
  /// Handler found, dispatch should proceed.
  DispatchOk(handler: fn(Dynamic) -> Dynamic, callback_type: CallbackType)
  /// No handler registered for this callback.
  DispatchNoHandler(callback_id: String)
  /// Dispatch failed due to missing callback_id.
  DispatchMissingId
}

/// Look up a hook handler in the configuration.
///
/// Returns DispatchOk if handler found, DispatchNoHandler otherwise.
pub fn dispatch_hook(config: HookConfig, callback_id: String) -> DispatchResult {
  case dict.get(config.handlers, callback_id) {
    Ok(handler) -> DispatchOk(handler, HookType)
    Error(_) -> DispatchNoHandler(callback_id)
  }
}

/// Look up a permission handler in the configuration.
///
/// Returns DispatchOk if handler found, DispatchNoHandler otherwise.
pub fn dispatch_permission(
  config: HookConfig,
  tool_name: String,
) -> DispatchResult {
  case dict.get(config.permission_handlers, tool_name) {
    Ok(handler) -> DispatchOk(handler, PermissionType)
    Error(_) -> DispatchNoHandler(tool_name)
  }
}
