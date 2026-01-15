/// Unit tests for bidir/reducers.gleam - pure state transformation functions.
///
/// Tests the pure reducer logic without spawning any processes.
/// Verifies:
/// - Queue operations with backpressure
/// - Pending request management
/// - Pending hook management
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{None}
import gleeunit/should

import claude_agent_sdk/error.{InitQueueOverflow, TooManyPendingRequests}
import claude_agent_sdk/internal/bidir/actor.{
  type PendingHook, type PendingRequest, type RequestResult, type RuntimeState,
  type SessionState, Buffers, HookConfig, HookType, PendingHook, PendingOps,
  PendingRequest, QueuedRequest, QueuedUserMessage, SessionConfig, SessionState,
  Timers,
}
import claude_agent_sdk/internal/bidir/reducers.{
  add_pending_hook, add_pending_request, max_pending_hooks, max_pending_requests,
  max_queued_ops, queue_operation,
}
import claude_agent_sdk/internal/line_framing.{LineBuffer}

// =============================================================================
// Test Helpers
// =============================================================================

/// Create a minimal test state for reducer testing.
///
/// Note: RuntimeState fields are stubs since reducers don't use them.
fn test_state() -> SessionState {
  SessionState(
    config: SessionConfig(
      hooks: HookConfig(handlers: dict.new(), permission_handlers: dict.new()),
      mcp_handlers: dict.new(),
      default_timeout_ms: 30_000,
      hook_timeouts: dict.new(),
      default_hook_timeout_ms: 5000,
      file_checkpointing_enabled: False,
    ),
    runtime: stub_runtime(),
    pending: PendingOps(
      pending_requests: dict.new(),
      pending_hooks: dict.new(),
      queued_ops: [],
      next_request_id: 1,
      next_callback_id: 1,
    ),
    timers: Timers(
      init_timeout_ms: 10_000,
      init_timer_ref: None,
      init_request_id: None,
    ),
    buffers: Buffers(line_buffer: LineBuffer(<<>>)),
  )
}

/// Stub RuntimeState - reducers don't access these fields.
@external(erlang, "reducers_test_ffi", "stub_runtime")
fn stub_runtime() -> RuntimeState

/// Create a stub PendingRequest for testing.
fn stub_pending_request(request_id: String) -> PendingRequest {
  PendingRequest(
    request_id: request_id,
    reply_to: stub_subject(),
    sent_at: 0,
    timer_ref: None,
  )
}

/// Create a stub PendingHook for testing.
fn stub_pending_hook(request_id: String) -> PendingHook {
  PendingHook(
    task_pid: stub_pid(),
    monitor_ref: stub_monitor(),
    timer_ref: to_dynamic(Nil),
    verify_ref: to_dynamic(Nil),
    callback_id: "test_callback",
    request_id: request_id,
    received_at: 0,
    callback_type: HookType,
  )
}

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

@external(erlang, "reducers_test_ffi", "stub_subject")
fn stub_subject() -> Subject(RequestResult)

@external(erlang, "reducers_test_ffi", "stub_pid")
fn stub_pid() -> Pid

@external(erlang, "reducers_test_ffi", "stub_monitor")
fn stub_monitor() -> process.Monitor

// =============================================================================
// queue_operation Tests
// =============================================================================

pub fn queue_operation_adds_to_empty_queue_test() {
  let state = test_state()
  let op = QueuedUserMessage("{}")

  let result = queue_operation(state, op)

  should.be_ok(result)
  let assert Ok(new_state) = result
  should.equal(list.length(new_state.pending.queued_ops), 1)
}

pub fn queue_operation_prepends_to_queue_test() {
  let state = test_state()
  let op1 = QueuedUserMessage("first")
  let op2 = QueuedUserMessage("second")

  let assert Ok(s1) = queue_operation(state, op1)
  let assert Ok(s2) = queue_operation(s1, op2)

  // Operations are prepended (LIFO for insert, reversed on flush)
  should.equal(list.length(s2.pending.queued_ops), 2)
}

pub fn queue_operation_rejects_at_capacity_test() {
  // Fill queue to capacity
  let state =
    list.fold(list.range(1, max_queued_ops), test_state(), fn(s, i) {
      let op = QueuedUserMessage("msg" <> int_to_string(i))
      let assert Ok(new_s) = queue_operation(s, op)
      new_s
    })

  // Next operation should fail
  let op = QueuedUserMessage("overflow")
  let result = queue_operation(state, op)

  should.be_error(result)
  let assert Error(InitQueueOverflow(_msg)) = result
}

// =============================================================================
// add_pending_request Tests
// =============================================================================

pub fn add_pending_request_adds_to_empty_dict_test() {
  let state = test_state()
  let pending = stub_pending_request("req-1")

  let result = add_pending_request(state, "req-1", pending)

  should.be_ok(result)
  let assert Ok(new_state) = result
  should.equal(dict.size(new_state.pending.pending_requests), 1)
  should.be_ok(dict.get(new_state.pending.pending_requests, "req-1"))
}

pub fn add_pending_request_rejects_at_capacity_test() {
  // Fill to capacity
  let state =
    list.fold(list.range(1, max_pending_requests), test_state(), fn(s, i) {
      let id = "req-" <> int_to_string(i)
      let pending = stub_pending_request(id)
      let assert Ok(new_s) = add_pending_request(s, id, pending)
      new_s
    })

  // Next should fail
  let pending = stub_pending_request("overflow")
  let result = add_pending_request(state, "overflow", pending)

  should.be_error(result)
  let assert Error(TooManyPendingRequests(_msg)) = result
}

// =============================================================================
// add_pending_hook Tests
// =============================================================================

pub fn add_pending_hook_adds_to_empty_dict_test() {
  let state = test_state()
  let hook = stub_pending_hook("hook-1")

  let #(new_state, response) = add_pending_hook(state, "callback", hook)

  should.equal(response, None)
  should.equal(dict.size(new_state.pending.pending_hooks), 1)
}

pub fn add_pending_hook_returns_response_at_capacity_test() {
  // Fill to capacity
  let state =
    list.fold(list.range(1, max_pending_hooks), test_state(), fn(s, i) {
      let hook = stub_pending_hook("hook-" <> int_to_string(i))
      let #(new_s, _) = add_pending_hook(s, "cb", hook)
      new_s
    })

  // Next should return immediate response
  let hook = stub_pending_hook("overflow")
  let #(new_state, response) = add_pending_hook(state, "cb", hook)

  should.be_some(response)
  // State should be unchanged (hook not added)
  should.equal(dict.size(new_state.pending.pending_hooks), max_pending_hooks)
}

// =============================================================================
// Helper Functions
// =============================================================================

fn int_to_string(i: Int) -> String {
  case i {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    _ if i >= 10 && i < 100 -> int_to_string(i / 10) <> int_to_string(i % 10)
    _ if i >= 100 -> int_to_string(i / 100) <> int_to_string(i % 100)
    _ -> "?"
  }
}
