/// Tests for public stream.next() mapping from internal to public types.
///
/// These tests verify that the public stream.next() correctly maps internal
/// StreamItem and NextError types to the public StreamItem and StreamError types.
/// The internal module uses NextProcessError, NextBufferOverflow, etc. while
/// the public module exposes ProcessError, BufferOverflow, etc.
import claude_agent_sdk/error.{
  EndOfStream, JsonDecodeError, Message, ProcessError, TooManyDecodeErrors,
  UnexpectedMessageError, WarningEvent,
}
import claude_agent_sdk/internal/port_ffi
import claude_agent_sdk/internal/stream as internal_stream
import claude_agent_sdk/stream
import gleeunit/should

// ============================================================================
// Public stream.next() Mapping Tests (casg-3wy.4)
// ============================================================================

/// Test: Message variant is correctly mapped from internal to public type.
/// Verifies: Ok(internal_stream.Message(envelope)) -> Ok(error.Message(envelope))
pub fn next_maps_message_to_public_type_test() {
  // Output valid JSON that parses as a system message
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '{\"type\":\"system\"}'"],
      "/tmp",
    )
  let internal = internal_stream.new(port)

  // Call PUBLIC stream.next() which wraps internal and maps types
  let #(result, _) = stream.next(internal)

  // Verify we get the PUBLIC Message type (from error module)
  case result {
    Ok(Message(_envelope)) -> Nil
    _ -> should.fail()
  }
}

/// Test: WarningEvent variant is correctly mapped from internal to public type.
/// Verifies: Ok(internal_stream.WarningEvent(warning)) -> Ok(error.WarningEvent(warning))
pub fn next_maps_warning_event_to_public_type_test() {
  // Create a stream that will exit cleanly without a result (triggers CleanExitNoResult warning)
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 0"], "/tmp")
  let internal = internal_stream.new(port)

  // Call PUBLIC stream.next()
  let #(result, _) = stream.next(internal)

  // Verify we get the PUBLIC WarningEvent type
  case result {
    Ok(WarningEvent(warning)) -> {
      warning.code |> should.equal(error.CleanExitNoResult)
    }
    _ -> should.fail()
  }
}

/// Test: EndOfStream variant is correctly mapped from internal to public type.
/// Verifies: Ok(internal_stream.EndOfStream) -> Ok(error.EndOfStream)
pub fn next_maps_end_of_stream_to_public_type_test() {
  // Create and immediately close a stream
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let internal = internal_stream.new(port)
  let closed = internal_stream.close(internal)

  // Call PUBLIC stream.next() on closed stream
  let #(result, _) = stream.next(closed)

  // Verify we get the PUBLIC EndOfStream type
  case result {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
}

/// Test: ProcessError is correctly mapped from internal to public type.
/// Verifies: Error(internal_stream.NextProcessError(exit_code, diagnostic)) -> Error(error.ProcessError(exit_code, diagnostic))
pub fn next_maps_process_error_to_public_type_test() {
  // Exit with non-zero code without a result
  let port = port_ffi.ffi_open_port("/bin/sh", ["-c", "exit 42"], "/tmp")
  let internal = internal_stream.new(port)

  // Call PUBLIC stream.next()
  let #(result, _) = stream.next(internal)

  // Verify we get the PUBLIC ProcessError type with correct exit code
  case result {
    Error(ProcessError(exit_code, _diagnostic)) -> {
      exit_code |> should.equal(42)
    }
    _ -> should.fail()
  }
}

/// Test: JsonDecodeError is correctly mapped from internal to public type.
/// Verifies: Error(internal_stream.NextJsonDecodeError(line, err)) -> Error(error.JsonDecodeError(line, err))
pub fn next_maps_json_decode_error_to_public_type_test() {
  // Output invalid JSON
  let port =
    port_ffi.ffi_open_port("/bin/sh", ["-c", "echo 'not valid json'"], "/tmp")
  let internal = internal_stream.new(port)

  // Call PUBLIC stream.next()
  let #(result, _) = stream.next(internal)

  // Verify we get the PUBLIC JsonDecodeError type
  case result {
    Error(JsonDecodeError(line, _err)) -> {
      line |> should.equal("not valid json")
    }
    _ -> should.fail()
  }
}

/// Test: TooManyDecodeErrors is correctly mapped from internal to public type.
/// Verifies: Error(internal_stream.NextTooManyDecodeErrors(count, last_error)) -> Error(error.TooManyDecodeErrors(count, last_error))
pub fn next_maps_too_many_decode_errors_to_public_type_test() {
  // Output 5 invalid JSON lines to hit the threshold
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "for i in 1 2 3 4 5; do echo 'bad'; done"],
      "/tmp",
    )
  let internal = internal_stream.new(port)

  // Consume first 4 errors
  let #(_, s1) = stream.next(internal)
  let #(_, s2) = stream.next(s1)
  let #(_, s3) = stream.next(s2)
  let #(_, s4) = stream.next(s3)

  // 5th call should trigger TooManyDecodeErrors
  let #(result5, _) = stream.next(s4)

  // Verify we get the PUBLIC TooManyDecodeErrors type
  case result5 {
    Error(TooManyDecodeErrors(count, _last_error)) -> {
      count |> should.equal(5)
    }
    _ -> should.fail()
  }
}

/// Test: UnexpectedMessageError is correctly mapped from internal to public type.
/// Verifies: Error(internal_stream.NextUnexpectedMessageError(raw_json)) -> Error(error.UnexpectedMessageError(raw_json))
pub fn next_maps_unexpected_message_error_to_public_type_test() {
  // Output valid JSON but with an unknown message type
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '{\"type\":\"unknown_type_xyz\"}'"],
      "/tmp",
    )
  let internal = internal_stream.new(port)

  // Call PUBLIC stream.next()
  let #(result, _) = stream.next(internal)

  // Verify we get the PUBLIC UnexpectedMessageError type
  case result {
    Error(UnexpectedMessageError(raw_json)) -> {
      raw_json |> should.equal("{\"type\":\"unknown_type_xyz\"}")
    }
    _ -> should.fail()
  }
}

/// Test: Returned stream is the same type (QueryStream) after mapping.
/// Verifies that the stream returned from public next() is usable for further iteration.
pub fn next_returns_updated_stream_test() {
  // Output two valid JSON messages
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '{\"type\":\"system\"}' && echo '{\"type\":\"system\"}'"],
      "/tmp",
    )
  let internal = internal_stream.new(port)

  // First call
  let #(result1, s1) = stream.next(internal)
  case result1 {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }

  // Second call using returned stream
  let #(result2, _s2) = stream.next(s1)
  case result2 {
    Ok(Message(_)) -> Nil
    _ -> should.fail()
  }
}

/// Test: BufferOverflow leads to closed stream which yields EndOfStream on next call.
/// Since BufferOverflow is terminal, subsequent next() calls should return EndOfStream.
/// (Direct BufferOverflow testing requires a 10MB+ line which is impractical,
/// so we test the terminal behavior by simulating a closed stream state.)
pub fn next_buffer_overflow_terminal_behavior_test() {
  // Create a stream and close it (simulating what happens after BufferOverflow)
  let port = port_ffi.ffi_open_port("/bin/echo", ["test"], "/tmp")
  let internal = internal_stream.new(port)
  let closed = internal_stream.mark_closed(internal)

  // PUBLIC stream.next() on closed stream
  let #(result, _) = stream.next(closed)

  // Should get EndOfStream (the public type)
  case result {
    Ok(EndOfStream) -> Nil
    _ -> should.fail()
  }
}

/// Test: NonZeroExitAfterResult warning is correctly mapped.
/// When CLI exits with non-zero after sending a result, we get a warning, not an error.
pub fn next_maps_nonzero_exit_after_result_warning_test() {
  // Output a result then exit with non-zero code
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}"
  let port =
    port_ffi.ffi_open_port(
      "/bin/sh",
      ["-c", "echo '" <> result_json <> "' && exit 42"],
      "/tmp",
    )
  let internal = internal_stream.new(port)

  // Get the result message first
  let #(_, s1) = stream.next(internal)

  // Next call should yield WarningEvent with NonZeroExitAfterResult
  let #(result2, _) = stream.next(s1)
  case result2 {
    Ok(WarningEvent(warning)) -> {
      case warning.code {
        error.NonZeroExitAfterResult(42) -> Nil
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}
