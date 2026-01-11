/// Tests for stream module - QueryStream and StreamState.
import claude_agent_sdk/internal/constants
import claude_agent_sdk/internal/port_ffi
import claude_agent_sdk/internal/stream.{
  BufferOverflow, Closed, CompleteLine, NeedMoreData, PendingEndOfStream,
  ReadError, ResultReceived, Streaming, append_to_buffer, close, get_buffer,
  get_state, is_closed, new, normalize_crlf, read_line, set_buffer,
}
import gleam/bit_array
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

  // First close should succeed and return updated stream with closed=True
  let stream1 = close(stream)
  stream1 |> is_closed |> should.be_true

  // Second close on already-closed stream should be a no-op
  let stream2 = close(stream1)
  stream2 |> is_closed |> should.be_true
}

pub fn close_sets_closed_flag_and_state_test() {
  // Create a real port and close it via stream
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // Verify stream starts as not closed and in Streaming state
  stream |> is_closed |> should.be_false
  stream |> get_state |> should.equal(Streaming)

  // Close the stream - should close the port, set closed=True, and state=Closed
  let closed_stream = close(stream)

  // Verify the returned stream has closed=True and state=Closed
  closed_stream |> is_closed |> should.be_true
  closed_stream |> get_state |> should.equal(Closed)
}

// ============================================================================
// CRLF Normalization Tests
// ============================================================================

pub fn normalize_crlf_empty_buffer_test() {
  normalize_crlf(<<>>)
  |> should.equal(<<>>)
}

pub fn normalize_crlf_no_crlf_test() {
  normalize_crlf(<<"hello world":utf8>>)
  |> should.equal(<<"hello world":utf8>>)
}

pub fn normalize_crlf_single_crlf_test() {
  normalize_crlf(<<"hello\r\nworld":utf8>>)
  |> should.equal(<<"hello\nworld":utf8>>)
}

pub fn normalize_crlf_multiple_crlf_test() {
  normalize_crlf(<<"a\r\nb\r\nc":utf8>>)
  |> should.equal(<<"a\nb\nc":utf8>>)
}

pub fn normalize_crlf_preserves_lone_cr_test() {
  // Lone \r (not followed by \n) should be preserved
  normalize_crlf(<<"hello\rworld":utf8>>)
  |> should.equal(<<"hello\rworld":utf8>>)
}

pub fn normalize_crlf_preserves_lone_lf_test() {
  // Lone \n should be preserved
  normalize_crlf(<<"hello\nworld":utf8>>)
  |> should.equal(<<"hello\nworld":utf8>>)
}

pub fn normalize_crlf_at_end_test() {
  normalize_crlf(<<"line\r\n":utf8>>)
  |> should.equal(<<"line\n":utf8>>)
}

pub fn normalize_crlf_at_start_test() {
  normalize_crlf(<<"\r\nline":utf8>>)
  |> should.equal(<<"\nline":utf8>>)
}

// ============================================================================
// read_line Tests
// ============================================================================

pub fn read_line_empty_buffer_test() {
  let #(result, remaining) = read_line(<<>>)
  result |> should.equal(NeedMoreData)
  remaining |> should.equal(<<>>)
}

pub fn read_line_no_newline_test() {
  let #(result, remaining) = read_line(<<"partial":utf8>>)
  result |> should.equal(NeedMoreData)
  remaining |> should.equal(<<"partial":utf8>>)
}

pub fn read_line_single_line_test() {
  let #(result, remaining) = read_line(<<"hello\n":utf8>>)
  result |> should.equal(CompleteLine("hello"))
  remaining |> should.equal(<<>>)
}

pub fn read_line_with_crlf_test() {
  // read_line normalizes CRLF -> LF internally
  let #(result, remaining) = read_line(<<"hello\r\n":utf8>>)
  result |> should.equal(CompleteLine("hello"))
  remaining |> should.equal(<<>>)
}

pub fn read_line_multiple_lines_test() {
  let #(result, remaining) = read_line(<<"first\nsecond\n":utf8>>)
  result |> should.equal(CompleteLine("first"))
  // Should have "second\n" remaining
  remaining |> should.equal(<<"second\n":utf8>>)

  // Read the second line
  let #(result2, remaining2) = read_line(remaining)
  result2 |> should.equal(CompleteLine("second"))
  remaining2 |> should.equal(<<>>)
}

pub fn read_line_empty_line_test() {
  let #(result, remaining) = read_line(<<"\n":utf8>>)
  result |> should.equal(CompleteLine(""))
  remaining |> should.equal(<<>>)
}

pub fn read_line_partial_after_complete_test() {
  let #(result, remaining) = read_line(<<"first\npartial":utf8>>)
  result |> should.equal(CompleteLine("first"))
  remaining |> should.equal(<<"partial":utf8>>)

  // The partial line needs more data
  let #(result2, _remaining2) = read_line(remaining)
  result2 |> should.equal(NeedMoreData)
}

// ============================================================================
// Chunk Reassembly Tests
// ============================================================================

pub fn chunk_reassembly_across_multiple_data_messages_test() {
  // Simulate receiving data in chunks
  let chunk1 = <<"hel":utf8>>
  let chunk2 = <<"lo\nwor":utf8>>
  let chunk3 = <<"ld\n":utf8>>

  // First chunk: no complete line
  let #(result1, remaining1) = read_line(chunk1)
  result1 |> should.equal(NeedMoreData)

  // Append second chunk
  let buffer2 = bit_array.append(remaining1, chunk2)
  let #(result2, remaining2) = read_line(buffer2)
  result2 |> should.equal(CompleteLine("hello"))

  // Append third chunk to remaining
  let buffer3 = bit_array.append(remaining2, chunk3)
  let #(result3, remaining3) = read_line(buffer3)
  result3 |> should.equal(CompleteLine("world"))
  remaining3 |> should.equal(<<>>)
}

pub fn chunk_reassembly_crlf_split_across_chunks_test() {
  // CRLF split: \r at end of chunk1, \n at start of chunk2
  let chunk1 = <<"hello\r":utf8>>
  let chunk2 = <<"\n":utf8>>

  // First chunk alone: no complete line (need to see \n to complete \r\n)
  let #(result1, remaining1) = read_line(chunk1)
  result1 |> should.equal(NeedMoreData)

  // Append second chunk - read_line normalizes the combined buffer
  let buffer2 = bit_array.append(remaining1, chunk2)
  let #(result2, remaining2) = read_line(buffer2)
  result2 |> should.equal(CompleteLine("hello"))
  remaining2 |> should.equal(<<>>)
}

pub fn chunk_reassembly_mixed_crlf_test() {
  // Test CRLF in middle of chunk plus partial line
  let #(result1, remaining1) = read_line(<<"hello\r\nwo":utf8>>)
  result1 |> should.equal(CompleteLine("hello"))
  remaining1 |> should.equal(<<"wo":utf8>>)

  // Complete the second line
  let buffer2 = bit_array.append(remaining1, <<"rld\n":utf8>>)
  let #(result2, remaining2) = read_line(buffer2)
  result2 |> should.equal(CompleteLine("world"))
  remaining2 |> should.equal(<<>>)
}

// ============================================================================
// Buffer Overflow Tests
// ============================================================================

pub fn append_to_buffer_normal_test() {
  let buffer = <<"hello":utf8>>
  let data = <<" world":utf8>>
  case append_to_buffer(buffer, data) {
    Ok(new_buffer) -> new_buffer |> should.equal(<<"hello world":utf8>>)
    Error(_) -> should.fail()
  }
}

pub fn append_to_buffer_overflow_test() {
  // Create a buffer near the limit
  // max_line_size_bytes is 10MB (10_485_760)
  // We'll test with a smaller buffer to verify the logic
  let buffer = <<>>
  // Create data that would exceed the limit
  let large_data = create_large_bit_array(constants.max_line_size_bytes + 1)
  case append_to_buffer(buffer, large_data) {
    Ok(_) -> should.fail()
    Error(BufferOverflow) -> should.be_ok(Ok(Nil))
    Error(_) -> should.fail()
  }
}

pub fn append_to_buffer_at_exact_limit_test() {
  // At exact limit should be OK
  let large_data = create_large_bit_array(constants.max_line_size_bytes)
  case append_to_buffer(<<>>, large_data) {
    Ok(_) -> should.be_ok(Ok(Nil))
    Error(_) -> should.fail()
  }
}

// Helper to create a large bit array
fn create_large_bit_array(size: Int) -> BitArray {
  create_large_bit_array_loop(size, <<>>)
}

fn create_large_bit_array_loop(remaining: Int, acc: BitArray) -> BitArray {
  case remaining {
    0 -> acc
    _ -> create_large_bit_array_loop(remaining - 1, <<acc:bits, 0x61>>)
  }
}

// ============================================================================
// QueryStream Buffer Accessor Tests
// ============================================================================

pub fn get_buffer_initial_empty_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)
  stream
  |> get_buffer
  |> should.equal(<<>>)
  port_ffi.ffi_close_port(port)
}

pub fn set_buffer_updates_stream_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  let updated = set_buffer(stream, <<"hello":utf8>>)
  updated
  |> get_buffer
  |> should.equal(<<"hello":utf8>>)

  port_ffi.ffi_close_port(port)
}

// ============================================================================
// Invalid UTF-8 Tests
// ============================================================================

pub fn read_line_invalid_utf8_test() {
  // 0xFF is invalid UTF-8
  let buffer = <<0xFF, 0x0A>>
  let #(result, _remaining) = read_line(buffer)
  case result {
    ReadError(msg) -> {
      // Should contain "Invalid UTF-8"
      case msg {
        "Invalid UTF-8 in line: " <> _ -> Nil
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}
// ============================================================================
// State Transition Tests (Failing - to be implemented in casg-j37.7)
// ============================================================================

// TODO(casg-j37.7): Add tests for state transitions once implemented
// - Streaming -> ResultReceived (on Result message)
// - Streaming -> PendingEndOfStream (on exit_status after Result)
// - ResultReceived -> PendingEndOfStream (on drain complete)
// - PendingEndOfStream -> Closed (after yielding EndOfStream)
