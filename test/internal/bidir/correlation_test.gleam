/// Unit tests for bidir/correlation.gleam - pure request correlation logic.
///
/// Tests the pure correlation functions without spawning any processes.
/// Verifies:
/// - add_pending adds requests to the map
/// - resolve_pending finds matching requests
/// - cancel_pending removes and returns requests
/// - timeout_pending removes and returns requests for timeout handling
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}
import gleeunit/should

import claude_agent_sdk/internal/bidir/correlation.{
  type PendingRequest, Found, NotFound, PendingRequest, add_pending,
  cancel_pending, remove_pending, resolve_pending, timeout_pending,
}

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// =============================================================================
// Test Helpers
// =============================================================================

fn make_pending(id: String) -> PendingRequest {
  PendingRequest(
    request_id: id,
    reply_subject: to_dynamic(Nil),
    sent_at: 1000,
    timer_ref: None,
  )
}

fn make_pending_with_timer(id: String, timer: Dynamic) -> PendingRequest {
  PendingRequest(
    request_id: id,
    reply_subject: to_dynamic(Nil),
    sent_at: 1000,
    timer_ref: Some(timer),
  )
}

// =============================================================================
// add_pending Tests
// =============================================================================

pub fn add_pending_to_empty_map_test() {
  let pending = dict.new()
  let req = make_pending("req-1")
  let new_pending = add_pending(pending, req)

  should.equal(dict.size(new_pending), 1)
  should.be_ok(dict.get(new_pending, "req-1"))
}

pub fn add_pending_preserves_existing_test() {
  let req1 = make_pending("req-1")
  let req2 = make_pending("req-2")
  let pending = add_pending(dict.new(), req1)
  let new_pending = add_pending(pending, req2)

  should.equal(dict.size(new_pending), 2)
  should.be_ok(dict.get(new_pending, "req-1"))
  should.be_ok(dict.get(new_pending, "req-2"))
}

pub fn add_pending_overwrites_same_id_test() {
  let req1 =
    PendingRequest(
      request_id: "req-1",
      reply_subject: to_dynamic("first"),
      sent_at: 1000,
      timer_ref: None,
    )
  let req2 =
    PendingRequest(
      request_id: "req-1",
      reply_subject: to_dynamic("second"),
      sent_at: 2000,
      timer_ref: None,
    )
  let pending = add_pending(dict.new(), req1)
  let new_pending = add_pending(pending, req2)

  should.equal(dict.size(new_pending), 1)
  case dict.get(new_pending, "req-1") {
    Ok(req) -> should.equal(req.sent_at, 2000)
    Error(_) -> should.fail()
  }
}

// =============================================================================
// resolve_pending Tests
// =============================================================================

pub fn resolve_pending_finds_existing_test() {
  let req = make_pending("req-100")
  let pending = add_pending(dict.new(), req)

  let result = resolve_pending(pending, "req-100")
  case result {
    Found(found_req) -> should.equal(found_req.request_id, "req-100")
    NotFound -> should.fail()
  }
}

pub fn resolve_pending_returns_not_found_for_missing_test() {
  let pending = dict.new()
  let result = resolve_pending(pending, "req-999")
  should.equal(result, NotFound)
}

pub fn resolve_pending_returns_correct_request_test() {
  let req1 = make_pending("req-1")
  let req2 = make_pending_with_timer("req-2", to_dynamic("timer-ref"))
  let pending = add_pending(add_pending(dict.new(), req1), req2)

  case resolve_pending(pending, "req-2") {
    Found(req) -> {
      should.equal(req.request_id, "req-2")
      should.equal(req.timer_ref, Some(to_dynamic("timer-ref")))
    }
    NotFound -> should.fail()
  }
}

// =============================================================================
// remove_pending Tests
// =============================================================================

pub fn remove_pending_removes_existing_test() {
  let req = make_pending("req-1")
  let pending = add_pending(dict.new(), req)
  let new_pending = remove_pending(pending, "req-1")

  should.equal(dict.size(new_pending), 0)
  should.be_error(dict.get(new_pending, "req-1"))
}

pub fn remove_pending_preserves_other_requests_test() {
  let req1 = make_pending("req-1")
  let req2 = make_pending("req-2")
  let pending = add_pending(add_pending(dict.new(), req1), req2)
  let new_pending = remove_pending(pending, "req-1")

  should.equal(dict.size(new_pending), 1)
  should.be_ok(dict.get(new_pending, "req-2"))
}

pub fn remove_pending_nonexistent_is_noop_test() {
  let req = make_pending("req-1")
  let pending = add_pending(dict.new(), req)
  let new_pending = remove_pending(pending, "req-999")

  should.equal(dict.size(new_pending), 1)
  should.be_ok(dict.get(new_pending, "req-1"))
}

// =============================================================================
// cancel_pending Tests
// =============================================================================

pub fn cancel_pending_removes_and_returns_existing_test() {
  let req = make_pending("req-1")
  let pending = add_pending(dict.new(), req)
  let #(new_pending, maybe_req) = cancel_pending(pending, "req-1")

  should.equal(dict.size(new_pending), 0)
  case maybe_req {
    Some(cancelled) -> should.equal(cancelled.request_id, "req-1")
    None -> should.fail()
  }
}

pub fn cancel_pending_returns_none_for_missing_test() {
  let pending = dict.new()
  let #(new_pending, maybe_req) = cancel_pending(pending, "req-999")

  should.equal(dict.size(new_pending), 0)
  should.equal(maybe_req, None)
}

pub fn cancel_pending_preserves_other_requests_test() {
  let req1 = make_pending("req-1")
  let req2 = make_pending("req-2")
  let pending = add_pending(add_pending(dict.new(), req1), req2)
  let #(new_pending, maybe_req) = cancel_pending(pending, "req-1")

  should.equal(dict.size(new_pending), 1)
  should.be_ok(dict.get(new_pending, "req-2"))
  should.equal(maybe_req, Some(req1))
}

// =============================================================================
// timeout_pending Tests
// =============================================================================

pub fn timeout_pending_removes_and_returns_existing_test() {
  let req = make_pending("req-1")
  let pending = add_pending(dict.new(), req)
  let #(new_pending, maybe_req) = timeout_pending(pending, "req-1")

  should.equal(dict.size(new_pending), 0)
  case maybe_req {
    Some(timed_out) -> should.equal(timed_out.request_id, "req-1")
    None -> should.fail()
  }
}

pub fn timeout_pending_returns_none_for_missing_test() {
  let pending = dict.new()
  let #(new_pending, maybe_req) = timeout_pending(pending, "req-999")

  should.equal(dict.size(new_pending), 0)
  should.equal(maybe_req, None)
}

pub fn timeout_pending_preserves_other_requests_test() {
  let req1 = make_pending("req-1")
  let req2 = make_pending("req-2")
  let pending = add_pending(add_pending(dict.new(), req1), req2)
  let #(new_pending, _) = timeout_pending(pending, "req-1")

  should.equal(dict.size(new_pending), 1)
  should.be_ok(dict.get(new_pending, "req-2"))
}

pub fn timeout_pending_returns_request_with_timer_ref_test() {
  let timer = to_dynamic("timer-123")
  let req = make_pending_with_timer("req-1", timer)
  let pending = add_pending(dict.new(), req)
  let #(_, maybe_req) = timeout_pending(pending, "req-1")

  case maybe_req {
    Some(timed_out) -> {
      // Timer ref should be preserved for cancellation
      should.equal(timed_out.timer_ref, Some(timer))
    }
    None -> should.fail()
  }
}

// =============================================================================
// PendingRequest Field Access Tests
// =============================================================================

pub fn pending_request_fields_accessible_test() {
  let timer = to_dynamic("my-timer")
  let reply = to_dynamic("my-subject")
  let req =
    PendingRequest(
      request_id: "test-id",
      reply_subject: reply,
      sent_at: 12_345,
      timer_ref: Some(timer),
    )

  should.equal(req.request_id, "test-id")
  should.equal(req.sent_at, 12_345)
  should.equal(req.timer_ref, Some(timer))
}

// =============================================================================
// RequestResult Type Tests
// =============================================================================

pub fn request_success_type_test() {
  let result = correlation.RequestSuccess(to_dynamic("payload"))
  let correlation.RequestSuccess(_) = result
  should.be_true(True)
}

pub fn request_error_type_test() {
  let result = correlation.RequestError("something went wrong")
  let correlation.RequestError(msg) = result
  should.equal(msg, "something went wrong")
}

pub fn request_timeout_type_test() {
  let result = correlation.RequestTimeout
  let correlation.RequestTimeout = result
  should.be_true(True)
}

pub fn request_session_stopped_type_test() {
  let result = correlation.RequestSessionStopped
  let correlation.RequestSessionStopped = result
  should.be_true(True)
}

// =============================================================================
// CorrelationResult Type Tests
// =============================================================================

pub fn correlation_found_contains_request_test() {
  let req = make_pending("test")
  let result = Found(req)
  let Found(r) = result
  should.equal(r.request_id, "test")
}

pub fn correlation_not_found_test() {
  let result: correlation.CorrelationResult = NotFound
  should.equal(result, NotFound)
}
