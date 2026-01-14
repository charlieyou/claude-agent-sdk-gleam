/// Pure correlation types and logic for request/response matching.
///
/// This module contains types and functions for correlating outgoing SDK
/// requests with incoming CLI responses. The actual Process/Subject handling
/// stays in bidir.gleam.
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

/// A pending request awaiting a response from the CLI.
///
/// Tracks SDK-initiated control requests that need response correlation.
/// Note: reply_subject and timer_ref are stored as Dynamic to avoid
/// OTP dependencies in this pure module.
pub type PendingRequest {
  PendingRequest(
    /// The request ID for correlation.
    request_id: String,
    /// Subject to send the response to (stored as Dynamic to avoid OTP deps).
    reply_subject: Dynamic,
    /// When the request was sent (monotonic ms).
    sent_at: Int,
    /// Timer reference for timeout cancellation.
    timer_ref: Option(Dynamic),
  )
}

/// Result of a control request.
pub type RequestResult {
  /// Request completed successfully.
  RequestSuccess(Dynamic)
  /// Request failed with error message.
  RequestError(String)
  /// Request timed out.
  RequestTimeout
  /// Session was stopped before request completed.
  RequestSessionStopped
}

/// Correlation lookup result.
pub type CorrelationResult {
  /// Found pending request.
  Found(PendingRequest)
  /// No pending request with this ID.
  NotFound
}

/// Look up a pending request by ID.
pub fn resolve_pending(
  pending: Dict(String, PendingRequest),
  request_id: String,
) -> CorrelationResult {
  case dict.get(pending, request_id) {
    Ok(req) -> Found(req)
    Error(_) -> NotFound
  }
}

/// Add a pending request to the map.
pub fn add_pending(
  pending: Dict(String, PendingRequest),
  request: PendingRequest,
) -> Dict(String, PendingRequest) {
  dict.insert(pending, request.request_id, request)
}

/// Remove a pending request from the map.
pub fn remove_pending(
  pending: Dict(String, PendingRequest),
  request_id: String,
) -> Dict(String, PendingRequest) {
  dict.delete(pending, request_id)
}

/// Cancel a pending request (remove and return it for cleanup).
///
/// Returns the removed request if found, or None if not present.
pub fn cancel_pending(
  pending: Dict(String, PendingRequest),
  request_id: String,
) -> #(Dict(String, PendingRequest), Option(PendingRequest)) {
  case dict.get(pending, request_id) {
    Ok(req) -> #(dict.delete(pending, request_id), option.Some(req))
    Error(_) -> #(pending, option.None)
  }
}

/// Mark a pending request as timed out (remove and return for notification).
///
/// Returns the removed request if found, or None if not present.
/// The caller should send RequestTimeout to the reply_subject.
pub fn timeout_pending(
  pending: Dict(String, PendingRequest),
  request_id: String,
) -> #(Dict(String, PendingRequest), Option(PendingRequest)) {
  case dict.get(pending, request_id) {
    Ok(req) -> #(dict.delete(pending, request_id), option.Some(req))
    Error(_) -> #(pending, option.None)
  }
}
