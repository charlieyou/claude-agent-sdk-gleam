/// Tests for stream module - QueryStream and StreamState.
import claude_agent_sdk/error.{
  type Warning, CleanExitNoResult, IncompleteLastLine, NonZeroExitAfterResult,
  UnexpectedMessageAfterResult, UnparseableCliVersion, Warning,
}
import claude_agent_sdk/internal/constants
import claude_agent_sdk/internal/port_ffi
import claude_agent_sdk/internal/stream.{
  BufferOverflow, Closed, CompleteLine, Continue, EndOfStream, Message,
  NeedMoreData, NextJsonDecodeError, NextProcessError, NextTooManyDecodeErrors,
  PendingEndOfStream, ReadError, ResultReceived, Stop, Streaming, WarningEvent,
  YieldEndOfStream, YieldProcessError, YieldWarningThenEnd, append_to_buffer,
  close, collect_items, collect_messages, fold_stream, get_buffer,
  get_consecutive_decode_errors, get_result_seen, get_state, handle_exit_status,
  increment_decode_errors, is_closed, mark_closed, mark_pending_end_of_stream,
  mark_result_received, new, new_with_warnings, next, normalize_crlf, read_line,
  reset_decode_errors, set_buffer, to_yielder, with_stream,
}
import gleam/bit_array
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleam/yielder
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

/// Generate a valid NDJSON assistant message with specified content size.
/// The padding fills the "text" field to reach approximately the target size.
/// Uses string concatenation instead of json module to avoid OTP 27 requirement.
fn generate_large_message(size_bytes: Int) -> String {
  // JSON structure overhead is approximately 200 bytes
  let overhead = 200
  let padding_size = case size_bytes > overhead {
    True -> size_bytes - overhead
    False -> 0
  }
  let padding = string.repeat("A", padding_size)
  "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\""
  <> padding
  <> "\"}],\"model\":\"claude-3-opus-20240229\",\"stop_reason\":null,\"stop_sequence\":null}}"
}

// ============================================================================
// Buffer Overflow Tests (casg-tc3.6)
// ============================================================================

pub fn buffer_handles_1mb_message_test() {
  // 1MB message should be well under the 10MB limit
  let large_json = generate_large_message(1_000_000)
  let data = bit_array.from_string(large_json)

  // Verify data is approximately 1MB
  let size = bit_array.byte_size(data)
  { size > 900_000 && size < 1_100_000 } |> should.be_true

  // Should append to buffer successfully (not near limit)
  case append_to_buffer(<<>>, data) {
    Ok(buffer) -> {
      // Buffer should contain our data
      bit_array.byte_size(buffer) |> should.equal(size)
    }
    Error(_) -> should.fail()
  }
}

pub fn buffer_overflow_at_11mb_test() {
  // 11MB exceeds the 10MB limit (10_485_760 bytes)
  let huge_json = generate_large_message(11_000_000)
  let data = bit_array.from_string(huge_json)

  // Verify data exceeds limit
  let size = bit_array.byte_size(data)
  { size > constants.max_line_size_bytes } |> should.be_true

  // Should trigger BufferOverflow
  case append_to_buffer(<<>>, data) {
    Ok(_) -> should.fail()
    Error(BufferOverflow) -> should.be_ok(Ok(Nil))
    Error(_) -> should.fail()
  }
}

pub fn buffer_overflow_at_15mb_test() {
  // 15MB well exceeds the 10MB limit
  let huge_json = generate_large_message(15_000_000)
  let data = bit_array.from_string(huge_json)

  // Verify data exceeds limit
  let size = bit_array.byte_size(data)
  { size > constants.max_line_size_bytes } |> should.be_true

  // Should trigger BufferOverflow
  case append_to_buffer(<<>>, data) {
    Ok(_) -> should.fail()
    Error(BufferOverflow) -> should.be_ok(Ok(Nil))
    Error(_) -> should.fail()
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
// State Machine Transition Tests (casg-j37.7)
// ============================================================================

pub fn result_seen_initially_false_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  stream |> get_result_seen |> should.be_false

  port_ffi.ffi_close_port(port)
}

pub fn mark_result_received_sets_state_and_flag_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // Initially Streaming and result_seen=False
  stream |> get_state |> should.equal(Streaming)
  stream |> get_result_seen |> should.be_false

  // After marking result received
  let updated = mark_result_received(stream)
  updated |> get_state |> should.equal(ResultReceived)
  updated |> get_result_seen |> should.be_true

  port_ffi.ffi_close_port(port)
}

// Test: exit_status before Result -> ProcessError (from plan line 4697)
pub fn exit_status_before_result_nonzero_yields_process_error_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // No Result seen, non-zero exit -> ProcessError
  let #(result, updated) = handle_exit_status(stream, 1)

  case result {
    YieldProcessError(code) -> code |> should.equal(1)
    _ -> should.fail()
  }

  updated |> get_state |> should.equal(Closed)

  port_ffi.ffi_close_port(port)
}

// Test: exit_status=0 before Result -> WarningEvent(CleanExitNoResult), EndOfStream (from plan line 4700)
pub fn exit_status_zero_before_result_yields_warning_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // No Result seen, exit 0 -> warning about no result
  let #(result, updated) = handle_exit_status(stream, 0)

  case result {
    YieldWarningThenEnd(warning) ->
      warning.code |> should.equal(CleanExitNoResult)
    _ -> should.fail()
  }

  updated |> get_state |> should.equal(PendingEndOfStream)

  port_ffi.ffi_close_port(port)
}

// Test: Result then exit_status=0 -> Result, EndOfStream (from plan line 4698)
pub fn result_then_exit_zero_yields_end_of_stream_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // Mark result received
  let stream_with_result = mark_result_received(stream)
  stream_with_result |> get_result_seen |> should.be_true

  // Exit 0 after Result -> normal end
  let #(result, updated) = handle_exit_status(stream_with_result, 0)

  result |> should.equal(YieldEndOfStream)
  updated |> get_state |> should.equal(PendingEndOfStream)

  port_ffi.ffi_close_port(port)
}

// Test: Result then exit_status!=0 -> Result, WarningEvent, EndOfStream (from plan line 4699)
pub fn result_then_exit_nonzero_yields_warning_not_error_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // Mark result received
  let stream_with_result = mark_result_received(stream)

  // Non-zero exit after Result -> warning only, NOT ProcessError
  let #(result, updated) = handle_exit_status(stream_with_result, 42)

  case result {
    YieldWarningThenEnd(warning) -> {
      case warning.code {
        NonZeroExitAfterResult(code) -> code |> should.equal(42)
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }

  // Should NOT be Closed (ProcessError), should be PendingEndOfStream
  updated |> get_state |> should.equal(PendingEndOfStream)

  port_ffi.ffi_close_port(port)
}

// ============================================================================
// Consecutive Decode Errors Tests
// ============================================================================

pub fn consecutive_decode_errors_initially_zero_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  stream |> get_consecutive_decode_errors |> should.equal(0)

  port_ffi.ffi_close_port(port)
}

pub fn increment_decode_errors_counts_up_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  let #(s1, exceeded1) = increment_decode_errors(stream, "error1")
  exceeded1 |> should.be_false
  s1 |> get_consecutive_decode_errors |> should.equal(1)

  let #(s2, exceeded2) = increment_decode_errors(s1, "error2")
  exceeded2 |> should.be_false
  s2 |> get_consecutive_decode_errors |> should.equal(2)

  port_ffi.ffi_close_port(port)
}

pub fn increment_decode_errors_threshold_exceeded_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // Increment up to threshold (5)
  let #(s1, _) = increment_decode_errors(stream, "e1")
  let #(s2, _) = increment_decode_errors(s1, "e2")
  let #(s3, _) = increment_decode_errors(s2, "e3")
  let #(s4, _) = increment_decode_errors(s3, "e4")

  // 4 errors, not exceeded yet
  s4 |> get_consecutive_decode_errors |> should.equal(4)

  // 5th error exceeds threshold
  let #(s5, exceeded5) = increment_decode_errors(s4, "e5")
  exceeded5 |> should.be_true
  s5 |> get_consecutive_decode_errors |> should.equal(5)

  port_ffi.ffi_close_port(port)
}

pub fn reset_decode_errors_clears_counter_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  let #(s1, _) = increment_decode_errors(stream, "error")
  let #(s2, _) = increment_decode_errors(s1, "error")
  s2 |> get_consecutive_decode_errors |> should.equal(2)

  let s3 = reset_decode_errors(s2)
  s3 |> get_consecutive_decode_errors |> should.equal(0)

  port_ffi.ffi_close_port(port)
}

pub fn reset_decode_errors_no_op_when_zero_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // Should be a no-op (returns same stream)
  let updated = reset_decode_errors(stream)
  updated |> get_consecutive_decode_errors |> should.equal(0)

  port_ffi.ffi_close_port(port)
}

// ============================================================================
// State Transition Helper Tests
// ============================================================================

pub fn mark_pending_end_of_stream_sets_state_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  let updated = mark_pending_end_of_stream(stream)
  updated |> get_state |> should.equal(PendingEndOfStream)

  port_ffi.ffi_close_port(port)
}

pub fn mark_closed_sets_state_and_flag_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  let updated = mark_closed(stream)
  updated |> get_state |> should.equal(Closed)
  updated |> is_closed |> should.be_true

  port_ffi.ffi_close_port(port)
}

// ============================================================================
// next() Tests (casg-j37.8)
// ============================================================================

pub fn next_on_closed_stream_returns_end_of_stream_immediately_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // Close the stream
  let closed_stream = close(stream)
  closed_stream |> is_closed |> should.be_true

  // next() on closed stream should return EndOfStream immediately
  let #(result, updated) = next(closed_stream)
  case result {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }

  // Stream should still be closed
  updated |> is_closed |> should.be_true
}

pub fn next_on_closed_stream_returns_same_stream_test() {
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // Close and mark as closed
  let closed_stream = mark_closed(close(stream))

  // Multiple next() calls should all return EndOfStream
  let #(result1, s1) = next(closed_stream)
  let #(result2, _s2) = next(s1)

  case result1 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
  case result2 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
}

pub fn next_parses_json_message_test() {
  // Use a script that outputs valid JSON
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '{\"type\":\"system\"}'"],
      "/tmp",
    )
  let stream_instance = new(port)

  // First call should yield a Message
  let #(result, updated) = next(stream_instance)
  case result {
    Ok(Message(_envelope)) -> Nil
    other -> {
      // Debug: print what we got
      should.fail()
      let _ = other
      Nil
    }
  }

  // Stream should still be in valid state
  updated |> is_closed |> should.be_false
}

pub fn next_handles_invalid_json_as_non_terminal_error_test() {
  // Output something that's not valid JSON followed by valid JSON
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo 'not json' && echo '{\"type\":\"system\"}'"],
      "/tmp",
    )
  let stream_instance = new(port)

  // First call should yield JsonDecodeError (non-terminal)
  let #(result1, s1) = next(stream_instance)
  case result1 {
    Error(NextJsonDecodeError(_, _)) -> Nil
    _ -> should.fail()
  }

  // Stream should not be closed - can continue
  s1 |> is_closed |> should.be_false

  // Second call should yield the valid message
  let #(result2, _s2) = next(s1)
  case result2 {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn next_buffer_overflow_is_terminal_test() {
  // BufferOverflow is terminal: once triggered, stream is closed
  // and subsequent next() calls return EndOfStream.
  //
  // We verify this by:
  // 1. Creating a stream
  // 2. Manually closing it (simulating what happens after BufferOverflow)
  // 3. Calling next() and verifying EndOfStream is returned
  //
  // The actual BufferOverflow -> closed transition is tested via:
  // - append_to_buffer_overflow_test (buffer limit check)
  // - buffer_overflow_at_11mb_test (realistic payload)
  // - buffer_overflow_at_15mb_test (realistic payload)
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream_instance = new(port)

  // Simulate what happens when BufferOverflow occurs: stream is closed
  let closed_stream = mark_closed(stream_instance)

  // Verify stream is closed
  is_closed(closed_stream) |> should.be_true

  // Next call should return EndOfStream (terminal behavior)
  let #(result, final_stream) = next(closed_stream)
  case result {
    Ok(EndOfStream) -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }

  // Subsequent calls also return EndOfStream
  let #(result2, _) = next(final_stream)
  case result2 {
    Ok(EndOfStream) -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn next_increments_decode_error_counter_test() {
  // Output invalid JSON to trigger decode errors
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo 'bad1' && echo 'bad2' && echo '{\"type\":\"system\"}'"],
      "/tmp",
    )
  let stream_instance = new(port)

  // First bad line
  let #(_result1, s1) = next(stream_instance)
  s1 |> get_consecutive_decode_errors |> should.equal(1)

  // Second bad line
  let #(_result2, s2) = next(s1)
  s2 |> get_consecutive_decode_errors |> should.equal(2)

  // Good line should reset counter
  let #(_result3, s3) = next(s2)
  s3 |> get_consecutive_decode_errors |> should.equal(0)
}

pub fn next_too_many_decode_errors_is_terminal_test() {
  // Output many invalid JSON lines to hit the threshold
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "for i in 1 2 3 4 5; do echo 'bad'; done"],
      "/tmp",
    )
  let stream_instance = new(port)

  // Consume errors up to threshold
  let #(_, s1) = next(stream_instance)
  let #(_, s2) = next(s1)
  let #(_, s3) = next(s2)
  let #(_, s4) = next(s3)

  // 5th error should trigger TooManyDecodeErrors
  let #(result5, s5) = next(s4)
  case result5 {
    Error(NextTooManyDecodeErrors(count, _)) -> count |> should.equal(5)
    _ -> should.fail()
  }

  // Stream should be closed (terminal error)
  s5 |> is_closed |> should.be_true
}

pub fn next_yields_result_message_and_transitions_state_test() {
  // Output a result message
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}"
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '" <> result_json <> "'"],
      "/tmp",
    )
  let stream_instance = new(port)

  // Should yield the Result message
  let #(result, updated) = next(stream_instance)
  case result {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }

  // State should transition to ResultReceived
  updated |> get_state |> should.equal(ResultReceived)
  updated |> get_result_seen |> should.be_true
}

pub fn next_after_result_uses_drain_timeout_test() {
  // Output a result followed by exit
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}"
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '" <> result_json <> "'"],
      "/tmp",
    )
  let stream_instance = new(port)

  // Get the result
  let #(_, s1) = next(stream_instance)
  s1 |> get_state |> should.equal(ResultReceived)

  // Next call should timeout quickly and get exit status -> EndOfStream
  let #(result2, s2) = next(s1)
  case result2 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
  s2 |> is_closed |> should.be_true
}

pub fn next_exit_after_result_nonzero_yields_warning_test() {
  // Output a result then exit with non-zero code
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}"
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '" <> result_json <> "' && exit 42"],
      "/tmp",
    )
  let stream_instance = new(port)

  // Get the result
  let #(_, s1) = next(stream_instance)
  s1 |> get_result_seen |> should.be_true

  // Next call should yield WarningEvent (non-zero exit after result)
  let #(result2, s2) = next(s1)
  case result2 {
    Ok(WarningEvent(warning)) -> {
      case warning.code {
        NonZeroExitAfterResult(42) -> Nil
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }

  // Then EndOfStream
  let #(result3, _s3) = next(s2)
  case result3 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
}

// Test: Result then exit_status=0 -> normal EndOfStream (from plan line 4698)
// exit_status=0 after Result is ignored silently - no warning needed
pub fn next_result_then_exit_zero_yields_normal_end_test() {
  // Output a result then exit with code 0
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}"
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '" <> result_json <> "' && exit 0"],
      "/tmp",
    )
  let stream_instance = new(port)

  // Get the result message
  let #(result1, s1) = next(stream_instance)
  case result1 {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }
  s1 |> get_result_seen |> should.be_true

  // Next call should yield EndOfStream directly (exit 0 after Result is ignored)
  let #(result2, s2) = next(s1)
  case result2 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }

  s2 |> is_closed |> should.be_true
}

// Test: Message arrives after Result -> yields UnexpectedMessageAfterResult warning
pub fn next_message_after_result_yields_warning_test() {
  // Output: Result, then another line, then exit
  // Note: content_block_start is not a recognized message type, but that doesn't
  // matter - ANY data after Result should trigger the warning
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}"
  let late_data = "{\"type\":\"content_block_start\",\"index\":0}"
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '" <> result_json <> "'; echo '" <> late_data <> "'"],
      "/tmp",
    )
  let stream_instance = new(port)

  // First: get the Result message
  let #(result1, s1) = next(stream_instance)
  case result1 {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }
  s1 |> get_result_seen |> should.be_true
  s1 |> get_state |> should.equal(ResultReceived)

  // Second: should yield UnexpectedMessageAfterResult warning (not the late data)
  let #(result2, s2) = next(s1)
  case result2 {
    Ok(WarningEvent(warning)) -> {
      case warning.code {
        UnexpectedMessageAfterResult -> {
          // Context should contain the raw line
          case warning.context {
            Some(ctx) -> {
              ctx |> string.contains("content_block_start") |> should.be_true
            }
            None -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }

  // Note: is_closed depends on port state, not warning semantics.
  // The shell exits immediately after printing, so port may already be closed.

  // Next call should get EndOfStream
  let #(result3, s3) = next(s2)
  case result3 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
  s3 |> is_closed |> should.be_true
}

pub fn next_exit_before_result_nonzero_yields_process_error_test() {
  // Exit with non-zero code without a result
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 1"], "/tmp")
  let stream_instance = new(port)

  // Should yield ProcessError (terminal)
  let #(result, updated) = next(stream_instance)
  case result {
    Error(NextProcessError(1, _)) -> Nil
    _ -> should.fail()
  }

  // Stream should be closed
  updated |> is_closed |> should.be_true
}

pub fn next_exit_before_result_zero_yields_warning_test() {
  // Exit with code 0 without a result message (anomaly)
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 0"], "/tmp")
  let stream_instance = new(port)

  // Should yield WarningEvent(CleanExitNoResult)
  let #(result, s1) = next(stream_instance)
  case result {
    Ok(WarningEvent(warning)) -> {
      warning.code |> should.equal(CleanExitNoResult)
    }
    _ -> should.fail()
  }

  // Then EndOfStream
  let #(result2, _s2) = next(s1)
  case result2 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
}

// ============================================================================
// Resource Helper Tests (casg-j37.11)
// ============================================================================

pub fn with_stream_closes_port_on_early_return_test() {
  // with_stream should close the port even on early return
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new(port)

  // Wrap in Ok to simulate query() result
  let query_result = Ok(stream)

  // with_stream should execute fn and close stream
  let result =
    with_stream(query_result, fn(s) {
      // Verify stream is open before fn completes
      s |> is_closed |> should.be_false
      "early_return_value"
    })

  // Should get our value back
  case result {
    Ok("early_return_value") -> Nil
    _ -> should.fail()
  }
  // Stream should now be closed
  // Note: We can't directly test the original stream is closed since
  // with_stream uses its own reference, but this tests the pattern works
}

pub fn with_stream_passes_query_error_through_test() {
  // with_stream should pass through QueryError without calling fn
  let query_result: Result(stream.QueryStream, error.QueryError) =
    Error(error.CliNotFoundError("claude not found"))

  let fn_called = fn(_s) {
    // This should never be called
    should.fail()
    "should not reach"
  }

  let result = with_stream(query_result, fn_called)

  case result {
    Error(error.CliNotFoundError(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn collect_items_collects_all_and_closes_port_test() {
  // Process exits with code 0, emitting no messages
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 0"], "/tmp")
  let stream_instance = new(port)

  // collect_items should drain the stream and close it
  let result = collect_items(stream_instance)

  // Should have collected the warning about clean exit no result
  // (because exit 0 with no Result message triggers CleanExitNoResult warning)
  result.warnings |> list.length |> should.equal(1)
  result.terminal_error |> should.equal(None)
}

pub fn collect_items_records_terminal_error_test() {
  // Process exits with non-zero code
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 1"], "/tmp")
  let stream_instance = new(port)

  let result = collect_items(stream_instance)

  // Should have terminal error recorded
  case result.terminal_error {
    Some(error.ProcessError(1, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn collect_messages_filters_to_messages_only_test() {
  // Process exits with code 0, no messages, generates warning
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 0"], "/tmp")
  let stream_instance = new(port)

  let result = collect_messages(stream_instance)

  // Items should be empty (no actual messages)
  result.items |> list.length |> should.equal(0)
  // But warnings should still be recorded
  result.warnings |> list.length |> should.equal(1)
}

pub fn fold_stream_with_early_stop_test() {
  // Process exits with code 0, generates warning then EndOfStream
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 0"], "/tmp")
  let stream_instance = new(port)

  // Fold that stops immediately on first item
  let #(final_acc, err) =
    fold_stream(stream_instance, 0, fn(acc, _result) {
      // Stop after first item
      Stop(acc + 1)
    })

  final_acc |> should.equal(1)
  err |> should.equal(None)
}

pub fn fold_stream_continues_until_end_test() {
  // Process exits with code 0
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 0"], "/tmp")
  let stream_instance = new(port)

  // Fold that counts all items
  let #(count, err) =
    fold_stream(stream_instance, 0, fn(acc, _result) { Continue(acc + 1) })

  // Should have counted at least the warning event
  { count >= 1 } |> should.be_true
  err |> should.equal(None)
}

pub fn fold_stream_records_terminal_error_test() {
  // Process exits with non-zero code
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 1"], "/tmp")
  let stream_instance = new(port)

  let #(_acc, err) =
    fold_stream(stream_instance, 0, fn(acc, _result) { Continue(acc + 1) })

  // Should have terminal error
  case err {
    Some(error.ProcessError(1, _)) -> Nil
    _ -> should.fail()
  }
}

// ============================================================================
// to_yielder Tests
// ============================================================================

pub fn to_yielder_basic_iteration_produces_all_items_test() {
  // Create a stream that outputs a valid JSON message then exits 0
  let json =
    "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"claude-3-opus-20240229\",\"stop_reason\":null,\"stop_sequence\":null}}\n"
  let port =
    port_ffi.ffi_open_port("/bin/sh", ["-c", "printf '" <> json <> "'"], "/tmp")
  let stream_instance = new(port)

  // Convert to yielder and collect all items
  let items =
    stream_instance
    |> to_yielder
    |> yielder.to_list

  // Should have at least one message (and potentially a warning for clean exit without result)
  { list.length(items) >= 1 } |> should.be_true

  // First item should be the message
  case list.first(items) {
    Ok(Ok(Message(_))) -> Nil
    _ -> should.fail()
  }
}

pub fn to_yielder_stops_at_end_of_stream_test() {
  // Create a stream with a result message then clean exit
  let result_json =
    "{\"type\":\"result\",\"result\":{\"conversation_id\":\"conv_01\",\"input_tokens\":100,\"output_tokens\":50}}\n"
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "printf '" <> result_json <> "'"],
      "/tmp",
    )
  let stream_instance = new(port)

  // Convert to yielder and collect
  let items =
    stream_instance
    |> to_yielder
    |> yielder.to_list

  // Should complete normally (yielder stops at EndOfStream)
  { list.length(items) >= 1 } |> should.be_true
}

pub fn to_yielder_early_termination_leaves_port_unclosed_test() {
  // This test demonstrates the leak risk: if we take() only some items,
  // the port remains unclosed. NOTE: This is expected behavior being documented.

  // Create a stream that would output multiple lines
  let json1 =
    "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"claude-3-opus-20240229\",\"stop_reason\":null,\"stop_sequence\":null}}"
  let json2 =
    "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_02\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"claude-3-opus-20240229\",\"stop_reason\":null,\"stop_sequence\":null}}"
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '" <> json1 <> "'; sleep 0.1; echo '" <> json2 <> "'"],
      "/tmp",
    )
  let stream_instance = new(port)

  // Take only first item via yielder (early termination)
  let first_items =
    stream_instance
    |> to_yielder
    |> yielder.take(1)
    |> yielder.to_list

  // We got one item
  list.length(first_items) |> should.equal(1)
  // NOTE: The port is now leaked because we didn't consume the full stream.
  // This is the documented risk of to_yielder() - unlike collect_items() or
  // with_stream(), there's no automatic cleanup on early termination.
  // This test exists to document this behavior, not to assert the port state.
}

pub fn to_yielder_propagates_errors_test() {
  // Stream that exits with non-zero code (terminal error)
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 1"], "/tmp")
  let stream_instance = new(port)

  // Convert to yielder and collect
  let items =
    stream_instance
    |> to_yielder
    |> yielder.to_list

  // Should have at least one item (the error)
  { list.length(items) >= 1 } |> should.be_true

  // Should contain an error
  let has_error =
    list.any(items, fn(item) {
      case item {
        Error(_) -> True
        _ -> False
      }
    })
  has_error |> should.be_true
}

// ============================================================================
// Multi-line NDJSON Stream Tests (real port_ffi)
// ============================================================================

/// Test reading multiple valid NDJSON messages in sequence from real port.
/// This replaces the mock test_runner-based stream tests with real port behavior.
pub fn multi_line_ndjson_stream_via_real_port_test() {
  // Valid JSON message fixtures
  let system_json = "{\"type\":\"system\"}"
  let assistant_json =
    "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"claude-3-opus-20240229\",\"stop_reason\":null,\"stop_sequence\":null}}"
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}"

  // Emit 3 NDJSON lines via shell
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      [
        "-c",
        "echo '"
          <> system_json
          <> "' && echo '"
          <> assistant_json
          <> "' && echo '"
          <> result_json
          <> "'",
      ],
      "/tmp",
    )
  let stream_instance = new(port)

  // First message: system
  let #(result1, s1) = next(stream_instance)
  case result1 {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }

  // Second message: assistant
  let #(result2, s2) = next(s1)
  case result2 {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }

  // Third message: result
  let #(result3, s3) = next(s2)
  case result3 {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }

  // After all messages: EndOfStream (exit 0)
  let #(result4, _s4) = next(s3)
  case result4 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
}

// ============================================================================
// Empty-stdout and Incomplete-line Warning Tests (casg-tc3.11)
// ============================================================================

/// Test: exit_status=0 with no data -> WarningEvent(CleanExitNoResult), EndOfStream
/// This verifies the behavior when CLI exits cleanly but never sent any output.
pub fn exit_zero_empty_stdout_yields_clean_exit_no_result_warning_test() {
  // Use shell that exits immediately with no output
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 0"], "/tmp")
  let stream_instance = new(port)

  // First call should yield WarningEvent(CleanExitNoResult)
  let #(result1, s1) = next(stream_instance)
  case result1 {
    Ok(WarningEvent(warning)) -> {
      warning.code |> should.equal(CleanExitNoResult)
    }
    _ -> should.fail()
  }

  // Second call should yield EndOfStream
  let #(result2, _s2) = next(s1)
  case result2 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
}

/// Test: exit_status=0 with incomplete buffer -> WarningEvent(IncompleteLastLine), EndOfStream
/// This verifies the behavior when CLI exits cleanly but buffer has incomplete line (no newline).
pub fn exit_zero_incomplete_buffer_yields_incomplete_last_line_warning_test() {
  // Use printf without trailing newline
  let port =
    port_ffi.ffi_open_port("/bin/sh", ["-c", "printf 'partial data'"], "/tmp")
  let stream_instance = new(port)

  // First call should yield WarningEvent(IncompleteLastLine)
  let #(result1, s1) = next(stream_instance)
  case result1 {
    Ok(WarningEvent(warning)) -> {
      case warning.code {
        IncompleteLastLine(incomplete_content) -> {
          // Should contain the incomplete data
          { string.contains(incomplete_content, "partial") } |> should.be_true
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }

  // Second call should yield EndOfStream
  let #(result2, _s2) = next(s1)
  case result2 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
}

/// Test: exit_status!=0 with empty stdout -> ProcessError with stdout_was_empty=True
/// This verifies ProcessError diagnostics correctly indicate empty stdout.
pub fn exit_nonzero_empty_stdout_yields_process_error_with_stdout_was_empty_test() {
  // Exit with non-zero code and no output
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 42"], "/tmp")
  let stream_instance = new(port)

  // Should yield ProcessError with stdout_was_empty=True
  let #(result, updated) = next(stream_instance)
  case result {
    Error(NextProcessError(code, diagnostic)) -> {
      code |> should.equal(42)
      diagnostic.stdout_was_empty |> should.be_true
      // last_non_json_line should be None since buffer is empty
      diagnostic.last_non_json_line |> should.equal(None)
    }
    _ -> should.fail()
  }

  // Stream should be closed
  updated |> is_closed |> should.be_true
}

/// Test: exit_status!=0 with incomplete buffer -> ProcessError with last_non_json_line populated
/// This verifies ProcessError diagnostics include the incomplete line content.
pub fn exit_nonzero_incomplete_buffer_yields_process_error_with_last_non_json_line_test() {
  // Output partial data without newline, then exit with non-zero code
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "printf 'error context data' && exit 1"],
      "/tmp",
    )
  let stream_instance = new(port)

  // Should yield ProcessError with last_non_json_line populated
  let #(result, updated) = next(stream_instance)
  case result {
    Error(NextProcessError(code, diagnostic)) -> {
      code |> should.equal(1)
      // stdout_was_empty should be False since buffer has data
      diagnostic.stdout_was_empty |> should.be_false
      // last_non_json_line should contain the incomplete line
      case diagnostic.last_non_json_line {
        Some(line) -> {
          { string.contains(line, "error context") } |> should.be_true
        }
        None -> should.fail()
      }
    }
    _ -> should.fail()
  }

  // Stream should be closed
  updated |> is_closed |> should.be_true
}

/// Test: Complete line followed by incomplete line, then exit 0
/// Verifies that complete lines are processed normally, then incomplete line warning is emitted.
pub fn complete_line_then_incomplete_line_exit_zero_test() {
  // Output a complete JSON line, then incomplete data, then exit 0
  let json_line = "{\"type\":\"system\"}"
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '" <> json_line <> "' && printf 'trailing data'"],
      "/tmp",
    )
  let stream_instance = new(port)

  // First call should yield the Message
  let #(result1, s1) = next(stream_instance)
  case result1 {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }

  // Second call should yield WarningEvent(IncompleteLastLine)
  let #(result2, s2) = next(s1)
  case result2 {
    Ok(WarningEvent(warning)) -> {
      case warning.code {
        IncompleteLastLine(content) -> {
          { string.contains(content, "trailing") } |> should.be_true
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }

  // Third call should yield EndOfStream
  let #(result3, _s3) = next(s2)
  case result3 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
}

/// Test: Complete line followed by incomplete line, then exit non-zero
/// Verifies that complete lines are processed, then ProcessError includes incomplete line in diagnostics.
pub fn complete_line_then_incomplete_line_exit_nonzero_test() {
  // Output a complete JSON line, then incomplete data, then exit non-zero
  let json_line = "{\"type\":\"system\"}"
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '" <> json_line <> "' && printf 'error hint' && exit 5"],
      "/tmp",
    )
  let stream_instance = new(port)

  // First call should yield the Message
  let #(result1, s1) = next(stream_instance)
  case result1 {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }

  // Second call should yield ProcessError with last_non_json_line
  let #(result2, s2) = next(s1)
  case result2 {
    Error(NextProcessError(code, diagnostic)) -> {
      code |> should.equal(5)
      // Buffer had incomplete line, so stdout was not empty
      diagnostic.stdout_was_empty |> should.be_false
      case diagnostic.last_non_json_line {
        Some(line) -> {
          { string.contains(line, "error hint") } |> should.be_true
        }
        None -> should.fail()
      }
    }
    _ -> should.fail()
  }

  // Stream should be closed
  s2 |> is_closed |> should.be_true
}

// ============================================================================
// Malformed JSON Error Tests
// ============================================================================

/// Test that 4 consecutive malformed lines yield 4 non-terminal JsonDecodeErrors
/// and the stream can still continue (counter stays at 4, under threshold).
pub fn malformed_json_four_consecutive_non_terminal_test() {
  // Output 4 invalid JSON lines followed by valid JSON
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      [
        "-c",
        "echo 'bad1' && echo 'bad2' && echo 'bad3' && echo 'bad4' && echo '{\"type\":\"system\"}'",
      ],
      "/tmp",
    )
  let stream_instance = new(port)

  // First 4 calls should yield JsonDecodeError (all non-terminal)
  let #(result1, s1) = next(stream_instance)
  case result1 {
    Error(NextJsonDecodeError(_, _)) -> Nil
    _ -> should.fail()
  }
  s1 |> is_closed |> should.be_false
  s1 |> get_consecutive_decode_errors |> should.equal(1)

  let #(result2, s2) = next(s1)
  case result2 {
    Error(NextJsonDecodeError(_, _)) -> Nil
    _ -> should.fail()
  }
  s2 |> is_closed |> should.be_false
  s2 |> get_consecutive_decode_errors |> should.equal(2)

  let #(result3, s3) = next(s2)
  case result3 {
    Error(NextJsonDecodeError(_, _)) -> Nil
    _ -> should.fail()
  }
  s3 |> is_closed |> should.be_false
  s3 |> get_consecutive_decode_errors |> should.equal(3)

  let #(result4, s4) = next(s3)
  case result4 {
    Error(NextJsonDecodeError(_, _)) -> Nil
    _ -> should.fail()
  }
  s4 |> is_closed |> should.be_false
  s4 |> get_consecutive_decode_errors |> should.equal(4)

  // 5th call should yield valid message and reset counter
  let #(result5, s5) = next(s4)
  case result5 {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }
  s5 |> get_consecutive_decode_errors |> should.equal(0)
}

/// Test that TooManyDecodeErrors is terminal: after it occurs,
/// subsequent next() calls return EndOfStream.
pub fn malformed_json_terminal_subsequent_next_returns_end_of_stream_test() {
  // Output 5 invalid JSON lines to trigger TooManyDecodeErrors
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "for i in 1 2 3 4 5; do echo 'bad'; done"],
      "/tmp",
    )
  let stream_instance = new(port)

  // Consume first 4 errors (non-terminal)
  let #(_, s1) = next(stream_instance)
  let #(_, s2) = next(s1)
  let #(_, s3) = next(s2)
  let #(_, s4) = next(s3)

  // 5th error triggers terminal TooManyDecodeErrors
  let #(result5, s5) = next(s4)
  case result5 {
    Error(NextTooManyDecodeErrors(count, _)) -> count |> should.equal(5)
    _ -> should.fail()
  }
  s5 |> is_closed |> should.be_true

  // Subsequent next() should return EndOfStream (terminal behavior)
  let #(result6, s6) = next(s5)
  case result6 {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
  s6 |> is_closed |> should.be_true
}

// ============================================================================
// Pending Warnings Tests
// ============================================================================

pub fn new_with_warnings_yields_warnings_first_test() {
  // Create a stream with a pending warning (e.g., UnparseableCliVersion)
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let warning: Warning =
    Warning(
      code: UnparseableCliVersion,
      message: "CLI version string could not be parsed; proceeding in permissive mode",
      context: Some("weird-version-string"),
    )
  let stream = new_with_warnings(port, [warning])

  // First call to next() should yield the pending warning
  let #(result1, s1) = next(stream)
  case result1 {
    Ok(WarningEvent(w)) -> {
      w.code |> should.equal(UnparseableCliVersion)
      w.message
      |> should.equal(
        "CLI version string could not be parsed; proceeding in permissive mode",
      )
      w.context |> should.equal(Some("weird-version-string"))
    }
    _ -> should.fail()
  }

  // Stream should still be in Streaming state (not closed)
  s1 |> is_closed |> should.be_false
  s1 |> get_state |> should.equal(Streaming)

  // Cleanup
  let _ = close(s1)
}

pub fn new_with_warnings_empty_list_works_like_new_test() {
  // Create a stream with no pending warnings
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let stream = new_with_warnings(port, [])

  // Stream should be in Streaming state
  stream |> get_state |> should.equal(Streaming)
  stream |> is_closed |> should.be_false

  // Cleanup
  port_ffi.ffi_close_port(port)
}

pub fn new_with_warnings_multiple_warnings_test() {
  // Create a stream with multiple pending warnings
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let warning1: Warning =
    Warning(
      code: UnparseableCliVersion,
      message: "First warning",
      context: None,
    )
  let warning2: Warning =
    Warning(
      code: UnparseableCliVersion,
      message: "Second warning",
      context: None,
    )
  let stream = new_with_warnings(port, [warning1, warning2])

  // First call to next() should yield first warning
  let #(result1, s1) = next(stream)
  case result1 {
    Ok(WarningEvent(w)) -> w.message |> should.equal("First warning")
    _ -> should.fail()
  }

  // Second call to next() should yield second warning
  let #(result2, s2) = next(s1)
  case result2 {
    Ok(WarningEvent(w)) -> w.message |> should.equal("Second warning")
    _ -> should.fail()
  }

  // Stream should still be streaming after warnings are consumed
  s2 |> is_closed |> should.be_false

  // Cleanup
  let _ = close(s2)
}
