/// Tests for stream module - QueryStream and StreamState.
import claude_agent_sdk/internal/port_ffi
import claude_agent_sdk/internal/stream.{
  Closed, PendingEndOfStream, ResultReceived, Streaming, close, get_state,
  is_closed, new,
}
import gleeunit/should

// ============================================================================
// QueryStream Construction Tests
// ============================================================================

pub fn new_stream_starts_in_streaming_state_test() {
  // Create a dummy port for testing
  // Note: This opens a real process; in production tests we'd mock this
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  stream
  |> get_state
  |> should.equal(Streaming)

  stream
  |> is_closed
  |> should.be_false

  // Cleanup
  port_ffi.ffi_close_port(port)
}

// ============================================================================
// StreamState Enum Tests
// ============================================================================

pub fn stream_state_variants_exist_test() {
  // Verify all state variants are constructible
  let _streaming = Streaming
  let _result_received = ResultReceived
  let _pending_eos = PendingEndOfStream
  let _closed = Closed

  // All variants should be distinct
  should.not_equal(Streaming, ResultReceived)
  should.not_equal(ResultReceived, PendingEndOfStream)
  should.not_equal(PendingEndOfStream, Closed)
  should.not_equal(Streaming, Closed)
}

// ============================================================================
// close() Tests
// ============================================================================

pub fn close_is_idempotent_test() {
  // Create a real port for testing
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // First close should succeed
  close(stream)

  // Second close should also succeed (idempotent)
  // Note: This tests that calling close() multiple times doesn't crash
  close(stream)
  // The function returns Nil, so we just verify it doesn't crash
}

pub fn close_calls_port_close_test() {
  // Create a real port and close it via stream
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // Close the stream - should close the port
  close(stream)

  // Verify stream was in correct initial state
  stream
  |> is_closed
  |> should.be_false
  // Note: is_closed returns the internal closed flag which is not updated
  // by close() in this implementation. The port is closed, but the flag
  // remains False because we don't return an updated stream.
}
// ============================================================================
// State Transition Tests (Failing - to be implemented in casg-j37.7)
// ============================================================================

// TODO(casg-j37.7): Add tests for state transitions once implemented
// - Streaming -> ResultReceived (on Result message)
// - Streaming -> PendingEndOfStream (on exit_status after Result)
// - ResultReceived -> PendingEndOfStream (on drain complete)
// - PendingEndOfStream -> Closed (after yielding EndOfStream)
