/// Line framing module for push-based line buffering and CRLF normalization.
///
/// This module provides line buffering logic used by BidirRunner's GenServer
/// which receives port messages asynchronously.
import claude_agent_sdk/internal/constants
import gleam/bit_array
import gleam/list
import gleam/string

// ============================================================================
// ReadLineResult: Result of reading a line from the buffer
// ============================================================================

/// Result of attempting to read a complete line from the buffer.
pub type ReadLineResult {
  /// A complete line was found and decoded as UTF-8
  CompleteLine(String)
  /// Port was closed (exit_status received with exit code 0 and empty buffer)
  PortClosed
  /// Exit status received with non-zero code
  ExitReceived(Int)
  /// Buffer exceeded max_line_size_bytes
  BufferOverflow
  /// An error occurred (e.g., invalid UTF-8)
  ReadError(String)
  /// No complete line yet; need more data
  NeedMoreData
}

// ============================================================================
// LineBuffer: Push-based line buffering for GenServer use
// ============================================================================

/// Buffer state for line accumulation in push-based processing.
/// Used by BidirRunner's GenServer which receives port messages asynchronously.
pub type LineBuffer {
  LineBuffer(buffer: BitArray)
}

/// Result of handling port data in push-based mode.
pub type HandlePortDataResult {
  /// Complete lines extracted, with remaining buffer for incomplete data
  Lines(lines: List(String), new_buffer: LineBuffer)
  /// Buffer overflow - combined buffer + data exceeds max_line_size_bytes
  PushBufferOverflow
}

/// Handle incoming port data, extract all complete lines.
/// This is a stateless function for push-based GenServer processing.
/// Reuses existing append_to_buffer and read_line functions.
pub fn handle_port_data(
  buffer: LineBuffer,
  data: BitArray,
) -> HandlePortDataResult {
  let LineBuffer(buf) = buffer
  // Append new data, checking for overflow
  case append_to_buffer(buf, data) {
    Error(_) -> PushBufferOverflow
    Ok(combined) -> extract_all_lines(combined, [])
  }
}

/// Extract all complete lines from buffer, accumulating in reverse order.
/// On ReadError (invalid UTF-8), the invalid line is skipped and processing continues.
pub fn extract_all_lines(
  buffer: BitArray,
  acc: List(String),
) -> HandlePortDataResult {
  case read_line(buffer) {
    #(CompleteLine(line), rest) -> extract_all_lines(rest, [line, ..acc])
    #(NeedMoreData, remaining) ->
      Lines(lines: list.reverse(acc), new_buffer: LineBuffer(remaining))
    #(BufferOverflow, _) -> PushBufferOverflow
    // ReadError: invalid UTF-8 line is dropped, continue processing rest
    #(ReadError(_), rest) -> extract_all_lines(rest, acc)
    // Other cases (PortClosed, ExitReceived): return accumulated lines
    #(_, remaining) ->
      Lines(lines: list.reverse(acc), new_buffer: LineBuffer(remaining))
  }
}

// ============================================================================
// Line Buffer Operations
// ============================================================================

/// Normalize CRLF to LF for Windows compatibility.
/// Replaces all \r\n sequences with \n at the byte level.
pub fn normalize_crlf(buffer: BitArray) -> BitArray {
  normalize_crlf_loop(buffer, <<>>)
}

fn normalize_crlf_loop(input: BitArray, acc: BitArray) -> BitArray {
  case input {
    // CRLF sequence: skip \r, keep \n
    <<0x0D, 0x0A, rest:bits>> -> normalize_crlf_loop(rest, <<acc:bits, 0x0A>>)
    // Single byte: copy through
    <<byte, rest:bits>> -> normalize_crlf_loop(rest, <<acc:bits, byte>>)
    // Empty: done
    <<>> -> acc
    // Fallback for any other pattern (shouldn't happen with valid BitArray)
    _ -> acc
  }
}

/// Append data to the buffer, checking for overflow.
/// Returns updated buffer or BufferOverflow error.
/// Note: CRLF normalization is deferred to read_line() to handle sequences
/// that span chunk boundaries correctly.
pub fn append_to_buffer(
  buffer: BitArray,
  data: BitArray,
) -> Result(BitArray, ReadLineResult) {
  // Check sizes BEFORE allocation to fail fast
  let buffer_size = bit_array.byte_size(buffer)
  let data_size = bit_array.byte_size(data)
  case buffer_size + data_size > constants.max_line_size_bytes {
    True -> Error(BufferOverflow)
    False -> Ok(bit_array.append(buffer, data))
  }
}

/// Try to extract a complete line from the buffer.
/// Returns the line (without newline) and remaining buffer, or NeedMoreData.
/// Normalizes CRLF to LF before scanning to handle Windows line endings.
/// Uses O(n) pattern matching instead of O(n²) slicing.
pub fn read_line(buffer: BitArray) -> #(ReadLineResult, BitArray) {
  // Normalize CRLF first to handle Windows line endings
  let normalized = normalize_crlf(buffer)
  // Use O(n) scanning via pattern matching
  find_newline_pattern(normalized, <<>>)
}

fn find_newline_pattern(
  remaining: BitArray,
  acc: BitArray,
) -> #(ReadLineResult, BitArray) {
  case remaining {
    <<>> -> #(NeedMoreData, acc)
    <<0x0A, rest:bits>> -> {
      // Found newline - acc contains the line bytes
      case bit_array.to_string(acc) {
        Ok(line) -> #(CompleteLine(line), rest)
        Error(_) -> #(
          ReadError("Invalid UTF-8 in line: " <> string.inspect(acc)),
          rest,
        )
      }
    }
    <<byte, rest:bits>> -> find_newline_pattern(rest, <<acc:bits, byte>>)
    // Fallback for non-byte-aligned data (shouldn't happen)
    _ -> #(NeedMoreData, bit_array.append(acc, remaining))
  }
}
