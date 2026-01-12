/// Tests for backpressure limits on queued_ops, pending_requests, pending_hooks.
///
/// Verifies that maps/lists enforce size limits to prevent unbounded memory growth:
/// - queued_ops: max 16, returns InitQueueOverflow
/// - pending_requests: max 64, returns TooManyPendingRequests
/// - pending_hooks: max 32, returns immediate fail response (no spawn)
import gleam/dict
import gleam/dynamic
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

import claude_agent_sdk/internal/bidir.{
  type PendingHook, type PendingRequest, type SessionState,
  type SubscriberMessage, InitQueueOverflow, PendingHook, PendingRequest,
  QueuedRequest, RequestError, SessionState, Starting, TooManyPendingRequests,
  add_pending_hook, add_pending_request, empty_hook_config, flush_queued_ops,
  queue_operation,
}
import support/mock_bidir_runner

// =============================================================================
// Test Helpers
// =============================================================================

/// Create a minimal SessionState for testing backpressure functions.
fn test_state() -> SessionState {
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  SessionState(
    runner: mock.runner,
    lifecycle: Starting,
    pending_requests: dict.new(),
    pending_hooks: dict.new(),
    queued_ops: [],
    hooks: empty_hook_config(),
    mcp_handlers: dict.new(),
    next_request_id: 0,
    next_callback_id: 0,
    subscriber: subscriber,
    capabilities: None,
    default_timeout_ms: 60_000,
    hook_timeouts: dict.new(),
    default_hook_timeout_ms: 30_000,
    init_request_id: None,
    inject_subject: None,
    init_timeout_ms: 10_000,
    init_timer_ref: None,
    file_checkpointing_enabled: False,
  )
}

/// Create a PendingRequest with given ID.
fn make_pending_request(id: String) -> PendingRequest {
  let reply_to: process.Subject(bidir.RequestResult) = process.new_subject()
  PendingRequest(
    request_id: id,
    reply_to: reply_to,
    sent_at: 0,
    timer_ref: None,
  )
}

/// Create a PendingHook with given ID (used as request_id for map key).
/// Uses stub values for task_pid and monitor_ref since they're only needed for running hooks.
fn make_pending_hook(id: String) -> PendingHook {
  PendingHook(
    task_pid: stub_pid(),
    monitor_ref: stub_monitor(),
    timer_ref: dynamic.nil(),
    verify_ref: dynamic.nil(),
    callback_id: "cb-" <> id,
    request_id: "req-" <> id,
    received_at: 0,
  )
}

// FFI stubs for test - creates dummy Pid/Monitor that won't be used
@external(erlang, "backpressure_test_ffi", "stub_pid")
fn stub_pid() -> process.Pid

@external(erlang, "backpressure_test_ffi", "stub_monitor")
fn stub_monitor() -> process.Monitor

// =============================================================================
// queued_ops Tests (max 16)
// =============================================================================

pub fn queue_operation_accepts_up_to_16_test() {
  // Fill queue to exactly 16
  let reply_to: process.Subject(bidir.RequestResult) = process.new_subject()
  let state =
    list.range(1, 16)
    |> list.fold(test_state(), fn(s, i) {
      let op = QueuedRequest("req-" <> int.to_string(i), "", reply_to)
      let assert Ok(new_state) = queue_operation(s, op)
      new_state
    })

  // Verify all 16 were added
  should.equal(list.length(state.queued_ops), 16)
}

pub fn queue_operation_rejects_17th_with_overflow_error_test() {
  // Fill queue to 16
  let reply_to: process.Subject(bidir.RequestResult) = process.new_subject()
  let state =
    list.range(1, 16)
    |> list.fold(test_state(), fn(s, i) {
      let op = QueuedRequest("req-" <> int.to_string(i), "", reply_to)
      let assert Ok(new_state) = queue_operation(s, op)
      new_state
    })

  // 17th should fail
  let op17 = QueuedRequest("req-17", "", reply_to)
  let result = queue_operation(state, op17)

  case result {
    Error(InitQueueOverflow(msg)) -> {
      // Error message should mention the limit
      should.be_true(string.contains(msg, "16"))
    }
    _ -> should.fail()
  }
}

pub fn queue_operation_overflow_preserves_existing_test() {
  // Fill to 16
  let reply_to: process.Subject(bidir.RequestResult) = process.new_subject()
  let state =
    list.range(1, 16)
    |> list.fold(test_state(), fn(s, i) {
      let op = QueuedRequest("req-" <> int.to_string(i), "", reply_to)
      let assert Ok(new_state) = queue_operation(s, op)
      new_state
    })

  // Try to add 17th (should fail)
  let op17 = QueuedRequest("req-17", "", reply_to)
  let _result = queue_operation(state, op17)

  // Original state should still have exactly 16
  should.equal(list.length(state.queued_ops), 16)
}

// =============================================================================
// pending_requests Tests (max 64)
// =============================================================================

pub fn add_pending_request_accepts_up_to_64_test() {
  // Fill to exactly 64
  let state =
    list.range(1, 64)
    |> list.fold(test_state(), fn(s, i) {
      let id = "req-" <> int.to_string(i)
      let pending = make_pending_request(id)
      let assert Ok(new_state) = add_pending_request(s, id, pending)
      new_state
    })

  // Verify all 64 were added
  should.equal(dict.size(state.pending_requests), 64)
}

pub fn add_pending_request_rejects_65th_test() {
  // Fill to 64
  let state =
    list.range(1, 64)
    |> list.fold(test_state(), fn(s, i) {
      let id = "req-" <> int.to_string(i)
      let pending = make_pending_request(id)
      let assert Ok(new_state) = add_pending_request(s, id, pending)
      new_state
    })

  // 65th should fail
  let pending65 = make_pending_request("req-65")
  let result = add_pending_request(state, "req-65", pending65)

  case result {
    Error(TooManyPendingRequests(msg)) -> {
      // Error message should mention the limit
      should.be_true(string.contains(msg, "64"))
    }
    _ -> should.fail()
  }
}

pub fn add_pending_request_overflow_preserves_existing_test() {
  // Fill to 64
  let state =
    list.range(1, 64)
    |> list.fold(test_state(), fn(s, i) {
      let id = "req-" <> int.to_string(i)
      let pending = make_pending_request(id)
      let assert Ok(new_state) = add_pending_request(s, id, pending)
      new_state
    })

  // Try to add 65th (should fail)
  let pending65 = make_pending_request("req-65")
  let _result = add_pending_request(state, "req-65", pending65)

  // Original state should still have exactly 64
  should.equal(dict.size(state.pending_requests), 64)
}

// =============================================================================
// pending_hooks Tests (max 32)
// =============================================================================

pub fn add_pending_hook_accepts_up_to_32_test() {
  // Fill to exactly 32
  let state =
    list.range(1, 32)
    |> list.fold(test_state(), fn(s, i) {
      let id = "hook-" <> int.to_string(i)
      let hook = make_pending_hook(id)
      let #(new_state, response) = add_pending_hook(s, id, hook)
      // Should have no immediate response (successfully queued)
      should.equal(response, None)
      new_state
    })

  // Verify all 32 were added
  should.equal(dict.size(state.pending_hooks), 32)
}

pub fn add_pending_hook_33rd_returns_immediate_response_test() {
  // Fill to 32
  let state =
    list.range(1, 32)
    |> list.fold(test_state(), fn(s, i) {
      let id = "hook-" <> int.to_string(i)
      let hook = make_pending_hook(id)
      let #(new_state, _) = add_pending_hook(s, id, hook)
      new_state
    })

  // 33rd should return immediate response (not added to map)
  let hook33 = make_pending_hook("hook-33")
  let #(new_state, response) = add_pending_hook(state, "hook-33", hook33)

  // State should still have exactly 32
  should.equal(dict.size(new_state.pending_hooks), 32)

  // Should have returned an immediate response
  case response {
    Some(_) -> should.be_true(True)
    None -> should.fail()
  }
}

pub fn add_pending_hook_overflow_preserves_existing_test() {
  // Fill to 32
  let state =
    list.range(1, 32)
    |> list.fold(test_state(), fn(s, i) {
      let id = "hook-" <> int.to_string(i)
      let hook = make_pending_hook(id)
      let #(new_state, _) = add_pending_hook(s, id, hook)
      new_state
    })

  // Try to add 33rd
  let hook33 = make_pending_hook("hook-33")
  let #(_, _) = add_pending_hook(state, "hook-33", hook33)

  // Original state should still have exactly 32
  should.equal(dict.size(state.pending_hooks), 32)
}

// =============================================================================
// flush_queued_ops Tests (enforces max 64 pending_requests)
// =============================================================================

pub fn flush_queued_ops_respects_pending_limit_test() {
  // Start with 60 pending requests (near capacity of 64)
  let state =
    list.range(1, 60)
    |> list.fold(test_state(), fn(s, i) {
      let id = "req-" <> int.to_string(i)
      let pending = make_pending_request(id)
      let assert Ok(new_state) = add_pending_request(s, id, pending)
      new_state
    })

  // Queue 10 operations (only 4 should fit: 64 - 60 = 4)
  let reply_subjects =
    list.range(1, 10)
    |> list.map(fn(i) {
      let subj: process.Subject(bidir.RequestResult) = process.new_subject()
      #("queued-" <> int.to_string(i), subj)
    })

  let state_with_queued =
    list.fold(reply_subjects, state, fn(s, pair) {
      let #(id, reply_to) = pair
      let op = QueuedRequest(id, "{}", reply_to)
      let assert Ok(new_state) = queue_operation(s, op)
      new_state
    })

  // Flush queued ops
  let flushed_state = flush_queued_ops(state_with_queued)

  // Should have exactly 64 pending requests (60 existing + 4 new)
  should.equal(dict.size(flushed_state.pending_requests), 64)

  // Queued ops should be cleared
  should.equal(list.length(flushed_state.queued_ops), 0)

  // First 4 subjects should NOT have received error (they were added successfully)
  // Last 6 subjects should have received RequestError
  let overflow_subjects = list.drop(reply_subjects, 4)
  list.each(overflow_subjects, fn(pair) {
    let #(_id, reply_to) = pair
    case process.receive(reply_to, 100) {
      Ok(RequestError(msg)) -> {
        should.be_true(string.contains(msg, "64"))
      }
      _ -> should.fail()
    }
  })
}

pub fn flush_queued_ops_all_fit_when_space_available_test() {
  // Start with 50 pending requests (room for 14 more)
  let state =
    list.range(1, 50)
    |> list.fold(test_state(), fn(s, i) {
      let id = "req-" <> int.to_string(i)
      let pending = make_pending_request(id)
      let assert Ok(new_state) = add_pending_request(s, id, pending)
      new_state
    })

  // Queue 10 operations (all should fit)
  let reply_subjects =
    list.range(1, 10)
    |> list.map(fn(i) {
      let subj: process.Subject(bidir.RequestResult) = process.new_subject()
      #("queued-" <> int.to_string(i), subj)
    })

  let state_with_queued =
    list.fold(reply_subjects, state, fn(s, pair) {
      let #(id, reply_to) = pair
      let op = QueuedRequest(id, "{}", reply_to)
      let assert Ok(new_state) = queue_operation(s, op)
      new_state
    })

  // Flush queued ops
  let flushed_state = flush_queued_ops(state_with_queued)

  // Should have exactly 60 pending requests (50 existing + 10 new)
  should.equal(dict.size(flushed_state.pending_requests), 60)

  // Queued ops should be cleared
  should.equal(list.length(flushed_state.queued_ops), 0)

  // None of the subjects should have received an error
  list.each(reply_subjects, fn(pair) {
    let #(_id, reply_to) = pair
    case process.receive(reply_to, 0) {
      Error(Nil) -> {
        // No message received - correct, request is pending
        Nil
      }
      Ok(_) -> should.fail()
    }
  })
}
