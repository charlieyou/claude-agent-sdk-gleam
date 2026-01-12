/// Tests for request ID tracker implementation.
///
/// TDD approach: These tests are written first and should fail until
/// the implementation is complete.
import gleam/option.{None, Some}
import gleeunit/should

import claude_agent_sdk/internal/request_tracker.{
  InitializeOp, InterruptOp, RewindFilesOp, SetModelOp, SetPermissionModeOp,
}

// ============================================================================
// Test 1: generate_id produces unique IDs
// ============================================================================

pub fn generate_id_produces_unique_ids_test() {
  let tracker = request_tracker.new()
  let #(tracker, id1) = request_tracker.generate_id(tracker)
  let #(tracker, id2) = request_tracker.generate_id(tracker)
  let #(_tracker, id3) = request_tracker.generate_id(tracker)

  // IDs should follow req_<counter> pattern
  id1 |> should.equal("req_0")
  id2 |> should.equal("req_1")
  id3 |> should.equal("req_2")
}

// ============================================================================
// Test 2: add_pending stores request kind
// ============================================================================

pub fn add_pending_stores_request_kind_test() {
  let tracker = request_tracker.new()
  let #(tracker, id) = request_tracker.generate_id(tracker)
  let tracker = request_tracker.add_pending(tracker, id, InitializeOp)

  // Should be able to retrieve the pending request
  request_tracker.has_pending(tracker) |> should.be_true
}

// ============================================================================
// Test 3: get_pending retrieves by ID
// ============================================================================

pub fn get_pending_retrieves_by_id_test() {
  let tracker = request_tracker.new()
  let #(tracker, id1) = request_tracker.generate_id(tracker)
  let #(tracker, id2) = request_tracker.generate_id(tracker)

  let tracker = request_tracker.add_pending(tracker, id1, InitializeOp)
  let tracker = request_tracker.add_pending(tracker, id2, InterruptOp)

  request_tracker.get_pending(tracker, id1) |> should.equal(Some(InitializeOp))
  request_tracker.get_pending(tracker, id2) |> should.equal(Some(InterruptOp))
  request_tracker.get_pending(tracker, "req_999") |> should.equal(None)
}

// ============================================================================
// Test 4: remove_pending clears entry
// ============================================================================

pub fn remove_pending_clears_entry_test() {
  let tracker = request_tracker.new()
  let #(tracker, id) = request_tracker.generate_id(tracker)
  let tracker = request_tracker.add_pending(tracker, id, SetModelOp)

  // Verify it's there
  request_tracker.get_pending(tracker, id) |> should.equal(Some(SetModelOp))

  // Remove it
  let tracker = request_tracker.remove_pending(tracker, id)

  // Verify it's gone
  request_tracker.get_pending(tracker, id) |> should.equal(None)
  request_tracker.has_pending(tracker) |> should.be_false
}

// ============================================================================
// Additional tests for completeness
// ============================================================================

pub fn new_tracker_has_no_pending_test() {
  let tracker = request_tracker.new()
  request_tracker.has_pending(tracker) |> should.be_false
}

pub fn all_pending_request_kinds_test() {
  let tracker = request_tracker.new()
  let #(tracker, id1) = request_tracker.generate_id(tracker)
  let #(tracker, id2) = request_tracker.generate_id(tracker)
  let #(tracker, id3) = request_tracker.generate_id(tracker)
  let #(tracker, id4) = request_tracker.generate_id(tracker)
  let #(tracker, id5) = request_tracker.generate_id(tracker)

  let tracker = request_tracker.add_pending(tracker, id1, InitializeOp)
  let tracker = request_tracker.add_pending(tracker, id2, InterruptOp)
  let tracker = request_tracker.add_pending(tracker, id3, SetPermissionModeOp)
  let tracker = request_tracker.add_pending(tracker, id4, SetModelOp)
  let tracker = request_tracker.add_pending(tracker, id5, RewindFilesOp)

  request_tracker.get_pending(tracker, id1) |> should.equal(Some(InitializeOp))
  request_tracker.get_pending(tracker, id2) |> should.equal(Some(InterruptOp))
  request_tracker.get_pending(tracker, id3)
  |> should.equal(Some(SetPermissionModeOp))
  request_tracker.get_pending(tracker, id4) |> should.equal(Some(SetModelOp))
  request_tracker.get_pending(tracker, id5) |> should.equal(Some(RewindFilesOp))
}

pub fn remove_nonexistent_is_safe_test() {
  let tracker = request_tracker.new()
  // Removing a nonexistent ID should not crash
  let tracker = request_tracker.remove_pending(tracker, "req_999")
  request_tracker.has_pending(tracker) |> should.be_false
}
