/// Tests for push-based line buffering (LineBuffer + handle_port_data)
import claude_agent_sdk/internal/stream.{
  LineBuffer, Lines, PushBufferOverflow, handle_port_data,
}
import gleam/bit_array
import gleam/string
import gleeunit/should

pub fn partial_line_delivery_test() {
  let buf = LineBuffer(<<>>)
  // Receive "hel"
  let result1 = handle_port_data(buf, <<"hel":utf8>>)
  should.equal(
    result1,
    Lines(lines: [], new_buffer: LineBuffer(<<"hel":utf8>>)),
  )

  // Receive "lo\n"
  let assert Lines(_, buf2) = result1
  let result2 = handle_port_data(buf2, <<"lo\n":utf8>>)
  should.equal(result2, Lines(lines: ["hello"], new_buffer: LineBuffer(<<>>)))
}

pub fn multiple_lines_in_chunk_test() {
  let buf = LineBuffer(<<>>)
  let result = handle_port_data(buf, <<"line1\nline2\nline3\n":utf8>>)
  should.equal(
    result,
    Lines(lines: ["line1", "line2", "line3"], new_buffer: LineBuffer(<<>>)),
  )
}

pub fn oversized_line_rejection_test() {
  let buf = LineBuffer(<<>>)
  // 11 MB > 10 MB limit
  let huge = string.repeat("x", 11_000_000)
  let result = handle_port_data(buf, bit_array.from_string(huge))
  should.equal(result, PushBufferOverflow)
}

pub fn empty_data_test() {
  let buf = LineBuffer(<<"partial":utf8>>)
  let result = handle_port_data(buf, <<>>)
  should.equal(
    result,
    Lines(lines: [], new_buffer: LineBuffer(<<"partial":utf8>>)),
  )
}

pub fn multiple_lines_with_trailing_partial_test() {
  let buf = LineBuffer(<<>>)
  let result = handle_port_data(buf, <<"a\nb\npartial":utf8>>)
  should.equal(
    result,
    Lines(lines: ["a", "b"], new_buffer: LineBuffer(<<"partial":utf8>>)),
  )
}

pub fn crlf_normalization_test() {
  let buf = LineBuffer(<<>>)
  let result = handle_port_data(buf, <<"line1\r\nline2\r\n":utf8>>)
  should.equal(
    result,
    Lines(lines: ["line1", "line2"], new_buffer: LineBuffer(<<>>)),
  )
}

pub fn empty_lines_test() {
  let buf = LineBuffer(<<>>)
  let result = handle_port_data(buf, <<"\n\n\n":utf8>>)
  should.equal(result, Lines(lines: ["", "", ""], new_buffer: LineBuffer(<<>>)))
}

pub fn buffer_accumulates_across_calls_test() {
  let buf = LineBuffer(<<>>)

  // First chunk: "abc"
  let assert Lines(lines: [], new_buffer: buf1) =
    handle_port_data(buf, <<"abc":utf8>>)

  // Second chunk: "def"
  let assert Lines(lines: [], new_buffer: buf2) =
    handle_port_data(buf1, <<"def":utf8>>)

  // Third chunk: "\n"
  let result = handle_port_data(buf2, <<"\n":utf8>>)
  should.equal(result, Lines(lines: ["abcdef"], new_buffer: LineBuffer(<<>>)))
}

pub fn overflow_with_existing_buffer_test() {
  // Start with buffer just under limit
  let initial = string.repeat("x", 10_000_000)
  let buf = LineBuffer(bit_array.from_string(initial))

  // Add more data that exceeds limit
  let extra = string.repeat("y", 1_000_000)
  let result = handle_port_data(buf, bit_array.from_string(extra))
  should.equal(result, PushBufferOverflow)
}
