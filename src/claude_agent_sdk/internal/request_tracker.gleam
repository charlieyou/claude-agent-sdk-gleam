/// Request ID tracker for correlating SDK-initiated requests with CLI responses.
///
/// This module provides request ID generation and tracking for the bidirectional
/// protocol. Each request sent to the CLI gets a unique ID that can be used to
/// match incoming responses.
import gleam/dict.{type Dict}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string

/// Types of pending control operations.
pub type PendingRequestKind {
  InitializeOp
  InterruptOp
  SetPermissionModeOp
  SetModelOp
  RewindFilesOp
}

/// Request tracker state.
///
/// Tracks the next ID to generate and all pending requests awaiting responses.
pub type RequestTracker {
  RequestTracker(next_id: Int, pending: Dict(String, PendingRequestKind))
}

/// Create a new request tracker.
pub fn new() -> RequestTracker {
  RequestTracker(next_id: 0, pending: dict.new())
}

/// Generate a new unique request ID.
///
/// Returns the updated tracker and the generated ID in format "req_<counter>".
pub fn generate_id(tracker: RequestTracker) -> #(RequestTracker, String) {
  let id = string.concat(["req_", int.to_string(tracker.next_id)])
  let new_tracker = RequestTracker(..tracker, next_id: tracker.next_id + 1)
  #(new_tracker, id)
}

/// Add a pending request.
///
/// Stores the request kind so we know what operation the response corresponds to.
pub fn add_pending(
  tracker: RequestTracker,
  request_id: String,
  kind: PendingRequestKind,
) -> RequestTracker {
  RequestTracker(
    ..tracker,
    pending: dict.insert(tracker.pending, request_id, kind),
  )
}

/// Get pending request kind by ID.
///
/// Returns Some(kind) if the request is pending, None otherwise.
pub fn get_pending(
  tracker: RequestTracker,
  request_id: String,
) -> Option(PendingRequestKind) {
  case dict.get(tracker.pending, request_id) {
    Ok(kind) -> Some(kind)
    Error(_) -> None
  }
}

/// Remove pending request (after response received).
///
/// Idempotent - removing a non-existent ID is safe.
pub fn remove_pending(
  tracker: RequestTracker,
  request_id: String,
) -> RequestTracker {
  RequestTracker(..tracker, pending: dict.delete(tracker.pending, request_id))
}

/// Check if any requests are pending.
pub fn has_pending(tracker: RequestTracker) -> Bool {
  !dict.is_empty(tracker.pending)
}
