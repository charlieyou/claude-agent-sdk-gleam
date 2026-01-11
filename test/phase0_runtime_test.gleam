/// Phase 0 Runtime Port Validation Tests
///
/// These tests validate actual port spawn/read/close operations.
/// Opt-in only via PHASE0_RUNTIME=1 environment variable to avoid
/// failures in restricted environments.
import gleam/erlang/atom
import gleam/io
import gleeunit

import claude_agent_sdk/internal/port_ffi
import support/env_helpers

pub fn main() -> Nil {
  gleeunit.main()
}

/// Print skip message to stdout (no file writes)
fn record_phase0_skip(test_name: String, reason: String) -> Nil {
  let line = "[SKIP:PHASE0] " <> test_name <> ": " <> reason
  io.println(line)
}

/// OS family for platform detection
type OsFamily {
  Unix
  Win32
}

/// Get os:type/0 as tuple of atoms
@external(erlang, "os", "type")
fn ffi_os_type() -> #(atom.Atom, atom.Atom)

/// Decode os:type/0 result to get OS family
fn decode_os_family() -> OsFamily {
  // os:type() returns {unix, linux} or {win32, nt} etc.
  let #(family, _) = ffi_os_type()
  case atom.to_string(family) {
    "win32" -> Win32
    _ -> Unix
  }
}

/// Get platform-specific test command
/// Returns #(executable, args, expected_output_bytes)
fn phase0_test_command() -> #(String, List(String), Int) {
  case decode_os_family() {
    Win32 -> #("cmd.exe", ["/c", "echo test"], 6)
    Unix -> #("/bin/echo", ["test"], 5)
  }
}

/// Runtime test: spawn -> read -> exit_status -> close flow
pub fn phase0_runtime_spawn_test() {
  case env_helpers.get_env("PHASE0_RUNTIME") {
    Ok("1") -> run_spawn_test()
    _ ->
      record_phase0_skip("phase0_runtime_spawn_test", "PHASE0_RUNTIME not set")
  }
}

fn run_spawn_test() -> Nil {
  let #(executable, args, expected_bytes) = phase0_test_command()

  // Spawn the port
  let port = port_ffi.ffi_open_port(executable, args, "")

  // Read data message
  case port_ffi.receive_blocking(port) {
    Ok(port_ffi.Data(data)) -> {
      let size = byte_size(data)
      assert size == expected_bytes
    }
    Ok(other) -> {
      io.println("Expected Data, got: " <> debug_port_message(other))
      panic as "Expected Data message"
    }
    Error(err) -> {
      io.println("Receive error: " <> err)
      panic as "Failed to receive data"
    }
  }

  // Read exit status message
  case port_ffi.receive_blocking(port) {
    Ok(port_ffi.ExitStatus(code)) -> {
      assert code == 0
    }
    Ok(other) -> {
      io.println("Expected ExitStatus, got: " <> debug_port_message(other))
      panic as "Expected ExitStatus message"
    }
    Error(err) -> {
      io.println("Receive error: " <> err)
      panic as "Failed to receive exit status"
    }
  }

  // Close the port
  port_ffi.ffi_close_port(port)
}

/// Runtime test: receive_timeout returns Timeout after process exits
pub fn phase0_timed_receive_test() {
  case env_helpers.get_env("PHASE0_RUNTIME") {
    Ok("1") -> run_timed_receive_test()
    _ ->
      record_phase0_skip("phase0_timed_receive_test", "PHASE0_RUNTIME not set")
  }
}

fn run_timed_receive_test() -> Nil {
  let #(executable, args, _) = phase0_test_command()

  // Spawn the port
  let port = port_ffi.ffi_open_port(executable, args, "")

  // Drain data message
  case port_ffi.receive_blocking(port) {
    Ok(port_ffi.Data(_)) -> Nil
    _ -> panic as "Expected Data message"
  }

  // Drain exit status message
  case port_ffi.receive_blocking(port) {
    Ok(port_ffi.ExitStatus(_)) -> Nil
    _ -> panic as "Expected ExitStatus message"
  }

  // Now port is closed - receive_timeout should return Timeout
  case port_ffi.receive_timeout(port, 100) {
    Ok(port_ffi.Timeout) -> Nil
    Ok(other) -> {
      io.println("Expected Timeout, got: " <> debug_port_message(other))
      panic as "Expected Timeout after exit"
    }
    Error(err) -> {
      io.println("Receive error: " <> err)
      panic as "Failed during timed receive"
    }
  }

  // Close the port
  port_ffi.ffi_close_port(port)
}

fn debug_port_message(msg: port_ffi.PortMessage) -> String {
  case msg {
    port_ffi.Data(_) -> "Data(...)"
    port_ffi.ExitStatus(code) -> "ExitStatus(" <> int_to_string(code) <> ")"
    port_ffi.Eof -> "Eof"
    port_ffi.Timeout -> "Timeout"
  }
}

@external(erlang, "erlang", "byte_size")
fn byte_size(data: BitArray) -> Int

@external(erlang, "erlang", "integer_to_list")
fn int_to_list(n: Int) -> List(Int)

fn int_to_string(n: Int) -> String {
  int_to_list(n)
  |> list_to_string
}

@external(erlang, "erlang", "list_to_binary")
fn list_to_binary(chars: List(Int)) -> BitArray

fn list_to_string(chars: List(Int)) -> String {
  chars
  |> list_to_binary
  |> bitarray_to_string
}

@external(erlang, "erlang", "binary_to_list")
fn bitarray_to_string(bin: BitArray) -> String
