/// Bidirectional session facade for Claude Agent SDK.
///
/// This module re-exports the public API from bidir/actor.gleam for backwards
/// compatibility. New code should import from bidir/actor directly.
///
/// ## Module Structure
///
/// - **bidir.gleam** (this file): Thin facade, re-exports from actor.gleam
/// - **bidir/actor.gleam**: OTP actor with all side effects (port I/O, timers, spawning)
/// - **bidir/state.gleam**: Pure lifecycle state machine
/// - **bidir/routing.gleam**: Pure message routing logic
/// - **bidir/hooks.gleam**: Pure hook types and dispatch
/// - **bidir/correlation.gleam**: Pure request/response correlation
import claude_agent_sdk/internal/bidir/actor

// =============================================================================
// Type Re-exports
// =============================================================================

pub type StopReason =
  actor.StopReason

pub type SessionLifecycle =
  actor.SessionLifecycle

pub type LifecycleEvent =
  actor.LifecycleEvent

pub type InvalidTransition =
  actor.InvalidTransition

pub type PendingRequest =
  actor.PendingRequest

pub type RequestResult =
  actor.RequestResult

pub type InterruptError =
  actor.InterruptError

pub type SetPermissionModeError =
  actor.SetPermissionModeError

pub type SetModelError =
  actor.SetModelError

pub type RewindFilesError =
  actor.RewindFilesError

pub type CallbackType =
  actor.CallbackType

pub type PendingHook =
  actor.PendingHook

pub type CleanupSource =
  actor.CleanupSource

pub type QueuedOperation =
  actor.QueuedOperation

pub type HookConfig =
  actor.HookConfig

pub type CliCapabilities =
  actor.CliCapabilities

pub type SubscriberMessage =
  actor.SubscriberMessage

pub type MessageRoute =
  actor.MessageRoute

pub type SessionState =
  actor.SessionState

pub type SessionConfig =
  actor.SessionConfig

pub type RuntimeState =
  actor.RuntimeState

pub type PendingOps =
  actor.PendingOps

pub type Timers =
  actor.Timers

pub type Buffers =
  actor.Buffers

pub type ActorMessage =
  actor.ActorMessage

pub type Response =
  actor.Response

pub type StartConfig =
  actor.StartConfig

// =============================================================================
// Function Re-exports
// =============================================================================

pub const transition = actor.transition

pub const empty_hook_config = actor.empty_hook_config

pub const route_incoming = actor.route_incoming

pub const resolve_pending = actor.resolve_pending

pub const default_config = actor.default_config

pub const start = actor.start

pub const start_for_testing = actor.start_for_testing

pub const start_with_hooks = actor.start_with_hooks

pub const inject_message = actor.inject_message

pub const flush_queued_ops = actor.flush_queued_ops

pub const get_lifecycle = actor.get_lifecycle

pub const get_pid = actor.get_pid

pub const ping = actor.ping

pub const get_capabilities = actor.get_capabilities

pub const shutdown = actor.shutdown

pub const inject_port_closed = actor.inject_port_closed

pub const send_control_request = actor.send_control_request

pub const send_user_message = actor.send_user_message

pub const cancel_pending_request = actor.cancel_pending_request

pub const interrupt = actor.interrupt

pub const set_permission_mode = actor.set_permission_mode

pub const set_model = actor.set_model

pub const rewind_files = actor.rewind_files

pub const queue_operation = actor.queue_operation

pub const add_pending_request = actor.add_pending_request

pub const add_pending_hook = actor.add_pending_hook

// =============================================================================
// Variant Constructor Re-exports
// =============================================================================
// Note: Gleam doesn't allow re-exporting variant constructors directly.
// Users pattern matching on types need to import bidir/actor for constructors.
// This is documented in the module header.

// Re-export commonly used constructors via factory functions
pub fn user_requested() -> StopReason {
  actor.UserRequested
}

pub fn cli_exited(code: Int) -> StopReason {
  actor.CliExited(code)
}

pub fn starting() -> SessionLifecycle {
  actor.Starting
}

pub fn init_sent() -> SessionLifecycle {
  actor.InitSent
}

pub fn running() -> SessionLifecycle {
  actor.Running
}

pub fn stopped() -> SessionLifecycle {
  actor.Stopped
}
