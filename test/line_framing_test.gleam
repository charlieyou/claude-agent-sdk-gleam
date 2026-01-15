/// Unit tests for line_framing module: CRLF normalization, empty input, overflow.
import claude_agent_sdk/internal/constants
import claude_agent_sdk/internal/line_framing.{
  BufferOverflow, CompleteLine, LineBuffer, Lines, NeedMoreData,
  PushBufferOverflow, append_to_buffer, handle_port_data, normalize_crlf,
  read_line,
}
import gleam/bit_array
import gleam/string
import gleeunit/should

// ============================================================================
// CRLF Normalization Tests
// ============================================================================

pub fn crlf_to_lf_normalization_test() {
  // \r\n should become \n
  let input = <<"hello\r\nworld\r\n":utf8>>
  let result = normalize_crlf(input)
  should.equal(result, <<"hello\nworld\n":utf8>>)
}

pub fn lone_cr_preserved_test() {
  // Standalone \r should be preserved (not part of \r\n)
  let input = <<"hello\rworld":utf8>>
  let result = normalize_crlf(input)
  should.equal(result, <<"hello\rworld":utf8>>)
}

pub fn lone_lf_preserved_test() {
  // Standalone \n should be preserved
  let input = <<"hello\nworld":utf8>>
  let result = normalize_crlf(input)
  should.equal(result, <<"hello\nworld":utf8>>)
}

pub fn mixed_line_endings_test() {
  // Mix of \r\n, \n, and \r
  let input = <<"a\r\nb\nc\rd":utf8>>
  let result = normalize_crlf(input)
  should.equal(result, <<"a\nb\nc\rd":utf8>>)
}

pub fn consecutive_crlf_test() {
  // Multiple consecutive CRLF pairs
  let input = <<"\r\n\r\n\r\n":utf8>>
  let result = normalize_crlf(input)
  should.equal(result, <<"\n\n\n":utf8>>)
}

pub fn crlf_at_start_test() {
  let input = <<"\r\nhello":utf8>>
  let result = normalize_crlf(input)
  should.equal(result, <<"\nhello":utf8>>)
}

pub fn crlf_at_end_test() {
  let input = <<"hello\r\n":utf8>>
  let result = normalize_crlf(input)
  should.equal(result, <<"hello\n":utf8>>)
}

// ============================================================================
// Empty Input Tests
// ============================================================================

pub fn empty_buffer_normalize_test() {
  let result = normalize_crlf(<<>>)
  should.equal(result, <<>>)
}

pub fn empty_buffer_read_line_test() {
  let #(result, remaining) = read_line(<<>>)
  should.equal(result, NeedMoreData)
  should.equal(remaining, <<>>)
}

pub fn empty_data_handle_port_data_test() {
  let buf = LineBuffer(<<"existing":utf8>>)
  let result = handle_port_data(buf, <<>>)
  should.equal(
    result,
    Lines(lines: [], new_buffer: LineBuffer(<<"existing":utf8>>)),
  )
}

pub fn empty_buffer_empty_data_test() {
  let buf = LineBuffer(<<>>)
  let result = handle_port_data(buf, <<>>)
  should.equal(result, Lines(lines: [], new_buffer: LineBuffer(<<>>)))
}

// ============================================================================
// Overflow Behavior Tests
// ============================================================================

pub fn append_buffer_overflow_test() {
  // Create buffer at max size
  let max_data = string.repeat("x", constants.max_line_size_bytes)
  let buffer = bit_array.from_string(max_data)

  // Adding any byte should overflow
  let result = append_to_buffer(buffer, <<"y":utf8>>)
  should.equal(result, Error(BufferOverflow))
}

pub fn append_exactly_at_limit_test() {
  // Create buffer just under limit
  let almost_max = string.repeat("x", constants.max_line_size_bytes - 1)
  let buffer = bit_array.from_string(almost_max)

  // Adding one byte should succeed
  let result = append_to_buffer(buffer, <<"y":utf8>>)
  should.be_ok(result)
}

pub fn handle_port_data_overflow_test() {
  // Start with buffer at max size
  let max_data = string.repeat("x", constants.max_line_size_bytes)
  let buf = LineBuffer(bit_array.from_string(max_data))

  // Adding any byte should return PushBufferOverflow
  let result = handle_port_data(buf, <<"y":utf8>>)
  should.equal(result, PushBufferOverflow)
}

pub fn overflow_with_partial_line_test() {
  // Partial line at max size (no newline)
  let max_data = string.repeat("x", constants.max_line_size_bytes)
  let buf = LineBuffer(bit_array.from_string(max_data))

  // Try to add more data without completing the line
  let result = handle_port_data(buf, <<"more":utf8>>)
  should.equal(result, PushBufferOverflow)
}

// ============================================================================
// read_line Edge Cases
// ============================================================================

pub fn read_line_simple_test() {
  let #(result, remaining) = read_line(<<"hello\nworld":utf8>>)
  should.equal(result, CompleteLine("hello"))
  should.equal(remaining, <<"world":utf8>>)
}

pub fn read_line_with_crlf_test() {
  let #(result, remaining) = read_line(<<"hello\r\nworld":utf8>>)
  should.equal(result, CompleteLine("hello"))
  should.equal(remaining, <<"world":utf8>>)
}

pub fn read_line_no_newline_test() {
  let #(result, remaining) = read_line(<<"hello":utf8>>)
  should.equal(result, NeedMoreData)
  should.equal(remaining, <<"hello":utf8>>)
}

pub fn read_line_empty_line_test() {
  let #(result, remaining) = read_line(<<"\nrest":utf8>>)
  should.equal(result, CompleteLine(""))
  should.equal(remaining, <<"rest":utf8>>)
}

pub fn read_line_multiple_empty_lines_test() {
  let #(result1, rest1) = read_line(<<"\n\n\n":utf8>>)
  should.equal(result1, CompleteLine(""))

  let #(result2, rest2) = read_line(rest1)
  should.equal(result2, CompleteLine(""))

  let #(result3, rest3) = read_line(rest2)
  should.equal(result3, CompleteLine(""))

  let #(result4, _) = read_line(rest3)
  should.equal(result4, NeedMoreData)
}
