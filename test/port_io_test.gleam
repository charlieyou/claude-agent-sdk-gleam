/// Tests for port_io module interface.
///
/// These tests verify the port_io abstraction layer correctly wraps port_ffi
/// without changing behavior.
import claude_agent_sdk/internal/port_io
import gleam/result
import gleeunit/should

// ============================================================================
// Type Re-export Tests
// ============================================================================

pub fn port_message_data_constructor_test() {
  // Verify Data constructor is accessible
  let msg = port_io.Data(<<1, 2, 3>>)
  case msg {
    port_io.Data(bytes) -> bytes |> should.equal(<<1, 2, 3>>)
  }
}

pub fn port_message_exit_status_constructor_test() {
  // Verify ExitStatus constructor is accessible
  let msg = port_io.ExitStatus(0)
  case msg {
    port_io.ExitStatus(code) -> code |> should.equal(0)
  }
}

pub fn port_message_eof_constructor_test() {
  // Verify Eof constructor is accessible
  let msg = port_io.Eof
  case msg {
    port_io.Eof -> should.be_ok(Ok(Nil))
  }
}

pub fn port_message_timeout_constructor_test() {
  // Verify Timeout constructor is accessible
  let msg = port_io.Timeout
  case msg {
    port_io.Timeout -> should.be_ok(Ok(Nil))
  }
}

pub fn write_error_port_closed_constructor_test() {
  // Verify PortClosed constructor is accessible
  let err = port_io.PortClosed
  case err {
    port_io.PortClosed -> should.be_ok(Ok(Nil))
  }
}

// ============================================================================
// CLI Discovery Tests
// ============================================================================

pub fn find_cli_path_not_found_test() {
  // Searching for non-existent CLI should return error
  port_io.find_cli_path("nonexistent_cli_that_does_not_exist_12345")
  |> result.is_error
  |> should.be_true
}

// ============================================================================
// Monotonic Time Tests
// ============================================================================

pub fn monotonic_time_ms_returns_integer_test() {
  // Monotonic time should be an integer (can be negative - starts from arbitrary point)
  let time = port_io.monotonic_time_ms()
  // Just verify we can call it and get an integer
  { time + 1 > time } |> should.be_true
}

pub fn monotonic_time_ms_is_monotonic_test() {
  // Time should never go backwards
  let t1 = port_io.monotonic_time_ms()
  let t2 = port_io.monotonic_time_ms()
  { t2 >= t1 } |> should.be_true
}
