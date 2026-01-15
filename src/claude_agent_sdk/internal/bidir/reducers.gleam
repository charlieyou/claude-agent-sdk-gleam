/// Pure state transformation functions (reducers) for bidirectional sessions.
///
/// This module re-exports the pure functions from actor.gleam that can be
/// tested without OTP dependencies. The actual implementations are in actor.gleam
/// to avoid import cycles.
///
/// ## Design Principles
///
/// - **Pure functions only**: No process.send, no FFI, no timers
/// - **State in, state out**: Each function takes state and returns new state
/// - **Separated effects**: Side effects (sending messages, timers) stay in actor.gleam
/// - **Testable in isolation**: No OTP dependencies required for testing
///
/// ## Re-exported Functions
///
/// - `queue_operation`: Queue operations during initialization with backpressure
/// - `add_pending_request`: Add pending request with capacity check
/// - `add_pending_hook`: Add pending hook with fail-open overflow handling
///
/// ## Constants
///
/// - `max_queued_ops`: Maximum queued operations (16)
/// - `max_pending_requests`: Maximum pending requests (64)
/// - `max_pending_hooks`: Maximum pending hooks (32)
import claude_agent_sdk/internal/bidir/actor

// =============================================================================
// Constants (re-exported from actor - single source of truth)
// =============================================================================

/// Maximum number of operations that can be queued during initialization.
pub const max_queued_ops = actor.max_queued_ops

/// Maximum number of concurrent pending requests (SDK → CLI).
pub const max_pending_requests = actor.max_pending_requests

/// Maximum number of concurrent pending hooks (CLI → SDK).
pub const max_pending_hooks = actor.max_pending_hooks

// =============================================================================
// Re-exported Pure Functions
// =============================================================================

/// Queue an operation during initialization with backpressure limit.
/// See actor.queue_operation for full documentation.
pub const queue_operation = actor.queue_operation

/// Add a pending request with backpressure limit.
/// See actor.add_pending_request for full documentation.
pub const add_pending_request = actor.add_pending_request

/// Add a pending hook with backpressure limit.
/// See actor.add_pending_hook for full documentation.
pub const add_pending_hook = actor.add_pending_hook
