/// Unified async callback dispatch for bidirectional sessions.
///
/// This module provides a shared callback runner with configurable policy
/// (fail-open vs fail-deny) that unifies hook and permission callback dispatch.
///
/// ## Architecture
///
/// The module is pure (no OTP dependencies) and returns decisions:
/// - **CallbackPolicy**: Determines error behavior (fail-open vs fail-deny)
/// - **DispatchDecision**: What the caller should do (spawn, reject at capacity, reject unknown)
/// - **decide_dispatch**: Unified decision maker for capacity + handler lookup
///
/// The caller (actor.gleam) handles the actual OTP operations (spawn, timer, response).
///
/// ## Usage
///
/// ```gleam
/// case decide_dispatch(pending_count, max_pending, handlers, callback_id, FailOpen) {
///   SpawnCallback(handler) -> // spawn task, schedule timer, record pending
///   RejectAtCapacity(policy) -> // send policy-appropriate response
///   RejectUnknownHandler(policy) -> // send policy-appropriate response
/// }
/// ```
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}

/// Policy for handling errors (capacity exceeded, timeout, crash, unknown handler).
///
/// Determines what happens when a callback cannot complete normally:
/// - FailOpen: Allow operation to continue (hooks use this)
/// - FailDeny: Deny operation for security (permissions use this)
pub type CallbackPolicy {
  /// Fail-open: errors allow the operation to continue.
  /// Used by hook callbacks where safety is preferred over blocking.
  FailOpen
  /// Fail-deny: errors deny the operation.
  /// Used by permission callbacks where security is paramount.
  FailDeny
}

/// Decision from the unified dispatch logic.
///
/// Tells the caller what action to take based on capacity and handler lookup.
pub type DispatchDecision {
  /// Proceed with spawning the callback task.
  /// Handler is provided for the caller to spawn.
  SpawnCallback(handler: fn(Dynamic) -> Dynamic)
  /// Reject due to capacity limit.
  /// Policy indicates fail-open (continue) or fail-deny response.
  RejectAtCapacity(policy: CallbackPolicy)
  /// Reject due to unknown handler.
  /// Policy indicates fail-open (continue) or fail-deny response.
  RejectUnknownHandler(policy: CallbackPolicy)
}

/// Unified dispatch decision maker.
///
/// Checks capacity and handler lookup, returning a decision for the caller.
/// This pure function encapsulates the common logic between hook and permission dispatch.
///
/// ## Parameters
/// - `pending_count`: Current number of pending callbacks
/// - `max_pending`: Maximum allowed pending callbacks
/// - `handlers`: Handler dictionary to look up callback_id
/// - `callback_id`: Key to look up handler
/// - `policy`: FailOpen or FailDeny for error responses
///
/// ## Returns
/// - `SpawnCallback(handler)`: Handler found, proceed with spawn
/// - `RejectAtCapacity(policy)`: At capacity, send policy-appropriate response
/// - `RejectUnknownHandler(policy)`: No handler, send policy-appropriate response
pub fn decide_dispatch(
  pending_count: Int,
  max_pending: Int,
  handlers: Dict(String, fn(Dynamic) -> Dynamic),
  callback_id: String,
  policy: CallbackPolicy,
) -> DispatchDecision {
  // Check capacity first
  case pending_count >= max_pending {
    True -> RejectAtCapacity(policy)
    False -> {
      // Look up handler
      case dict.get(handlers, callback_id) {
        Ok(handler) -> SpawnCallback(handler)
        Error(_) -> RejectUnknownHandler(policy)
      }
    }
  }
}
