/// Tests for unified async callback dispatch (casg-crj.3).
///
/// Tests the pure decision logic in callbacks.gleam for both
/// fail-open and fail-deny policies.
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleeunit/should

import claude_agent_sdk/internal/bidir/callbacks.{
  FailDeny, FailOpen, RejectAtCapacity, RejectUnknownHandler, SpawnCallback,
  decide_dispatch,
}

/// Test handler that just returns input.
fn echo_handler(input: Dynamic) -> Dynamic {
  input
}

// =============================================================================
// SpawnCallback Tests - Handler Found
// =============================================================================

/// When capacity is available and handler exists, should return SpawnCallback.
pub fn spawn_callback_when_handler_found_test() {
  let handlers = dict.from_list([#("my_hook", echo_handler)])

  let result = decide_dispatch(0, 32, handlers, "my_hook", FailOpen)

  case result {
    SpawnCallback(_handler) -> should.be_true(True)
    _ -> should.fail()
  }
}

/// SpawnCallback works with FailDeny policy too.
pub fn spawn_callback_with_fail_deny_policy_test() {
  let handlers = dict.from_list([#("my_tool", echo_handler)])

  let result = decide_dispatch(0, 32, handlers, "my_tool", FailDeny)

  case result {
    SpawnCallback(_handler) -> should.be_true(True)
    _ -> should.fail()
  }
}

/// SpawnCallback works at capacity boundary (pending_count < max).
pub fn spawn_callback_at_capacity_boundary_test() {
  let handlers = dict.from_list([#("hook", echo_handler)])

  // 31 pending, max 32 - should still allow
  let result = decide_dispatch(31, 32, handlers, "hook", FailOpen)

  case result {
    SpawnCallback(_handler) -> should.be_true(True)
    _ -> should.fail()
  }
}

// =============================================================================
// RejectAtCapacity Tests
// =============================================================================

/// When at capacity, should return RejectAtCapacity with FailOpen policy.
pub fn reject_at_capacity_fail_open_test() {
  let handlers = dict.from_list([#("hook", echo_handler)])

  // At capacity (32 pending, max 32)
  let result = decide_dispatch(32, 32, handlers, "hook", FailOpen)

  result
  |> should.equal(RejectAtCapacity(FailOpen))
}

/// When at capacity, should return RejectAtCapacity with FailDeny policy.
pub fn reject_at_capacity_fail_deny_test() {
  let handlers = dict.from_list([#("tool", echo_handler)])

  // At capacity
  let result = decide_dispatch(32, 32, handlers, "tool", FailDeny)

  result
  |> should.equal(RejectAtCapacity(FailDeny))
}

/// When over capacity, should still reject.
pub fn reject_over_capacity_test() {
  let handlers = dict.from_list([#("hook", echo_handler)])

  // Over capacity (40 pending, max 32)
  let result = decide_dispatch(40, 32, handlers, "hook", FailOpen)

  result
  |> should.equal(RejectAtCapacity(FailOpen))
}

// =============================================================================
// RejectUnknownHandler Tests
// =============================================================================

/// When handler not found, should return RejectUnknownHandler with FailOpen.
pub fn reject_unknown_handler_fail_open_test() {
  let handlers = dict.from_list([#("other_hook", echo_handler)])

  let result = decide_dispatch(0, 32, handlers, "unknown_hook", FailOpen)

  result
  |> should.equal(RejectUnknownHandler(FailOpen))
}

/// When handler not found, should return RejectUnknownHandler with FailDeny.
pub fn reject_unknown_handler_fail_deny_test() {
  let handlers = dict.from_list([#("other_tool", echo_handler)])

  let result = decide_dispatch(0, 32, handlers, "unknown_tool", FailDeny)

  result
  |> should.equal(RejectUnknownHandler(FailDeny))
}

/// Empty handlers dict always returns RejectUnknownHandler.
pub fn reject_with_empty_handlers_test() {
  let handlers = dict.new()

  let result = decide_dispatch(0, 32, handlers, "any_callback", FailDeny)

  result
  |> should.equal(RejectUnknownHandler(FailDeny))
}

// =============================================================================
// Priority Tests - Capacity Checked Before Handler Lookup
// =============================================================================

/// Capacity is checked before handler lookup - at capacity returns RejectAtCapacity
/// even if handler doesn't exist.
pub fn capacity_checked_before_handler_lookup_test() {
  // Empty handlers - handler won't be found
  let handlers = dict.new()

  // At capacity - should return RejectAtCapacity, not RejectUnknownHandler
  let result = decide_dispatch(32, 32, handlers, "unknown", FailOpen)

  // Should be RejectAtCapacity (capacity checked first)
  result
  |> should.equal(RejectAtCapacity(FailOpen))
}

/// Capacity is checked before handler lookup - with FailDeny policy.
pub fn capacity_checked_first_fail_deny_test() {
  let handlers = dict.new()

  let result = decide_dispatch(32, 32, handlers, "unknown", FailDeny)

  result
  |> should.equal(RejectAtCapacity(FailDeny))
}

// =============================================================================
// Edge Cases
// =============================================================================

/// Zero max_pending always rejects at capacity.
pub fn zero_max_pending_always_rejects_test() {
  let handlers = dict.from_list([#("hook", echo_handler)])

  let result = decide_dispatch(0, 0, handlers, "hook", FailOpen)

  result
  |> should.equal(RejectAtCapacity(FailOpen))
}

/// Empty callback_id can still be looked up if registered.
pub fn empty_callback_id_test() {
  let handlers = dict.from_list([#("", echo_handler)])

  let result = decide_dispatch(0, 32, handlers, "", FailOpen)

  case result {
    SpawnCallback(_handler) -> should.be_true(True)
    _ -> should.fail()
  }
}
