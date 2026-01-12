/// Runtime Port Validation Tests
///
/// These tests validate actual port spawn/read/close operations.
import gleam/bit_array
import gleam/erlang/atom
import gleam/io
import gleeunit

import claude_agent_sdk/internal/port_ffi

pub fn main() -> Nil {
  gleeunit.main()
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
fn runtime_test_command() -> #(String, List(String), Int) {
  case decode_os_family() {
    Win32 -> #("cmd.exe", ["/c", "echo test"], 6)
    Unix -> #("/bin/echo", ["test"], 5)
  }
}

/// Runtime test: spawn -> read -> exit_status -> close flow
pub fn runtime_spawn_test() {
  run_spawn_test()
}

fn run_spawn_test() -> Nil {
  let #(executable, args, expected_bytes) = runtime_test_command()

  // Spawn the port
  let port = port_ffi.ffi_open_port(executable, args, "")

  // Run test and capture result
  let result = run_spawn_test_body(port, expected_bytes)

  // Always close the port, even on failure
  port_ffi.ffi_close_port(port)

  // Now panic if test failed
  case result {
    Ok(Nil) -> Nil
    Error(msg) -> panic as msg
  }
}

fn run_spawn_test_body(
  port: port_ffi.Port,
  expected_bytes: Int,
) -> Result(Nil, String) {
  // Read data message
  case port_ffi.receive_blocking(port) {
    Ok(port_ffi.Data(data)) -> {
      let size = byte_size(data)
      case size == expected_bytes {
        True -> {
          // Read exit status message
          case port_ffi.receive_blocking(port) {
            Ok(port_ffi.ExitStatus(code)) -> {
              case code == 0 {
                True -> Ok(Nil)
                False -> {
                  io.println("Exit code was: " <> int_to_string(code))
                  Error("Expected exit code 0")
                }
              }
            }
            Ok(other) -> {
              io.println(
                "Expected ExitStatus, got: " <> debug_port_message(other),
              )
              Error("Expected ExitStatus message")
            }
            Error(err) -> {
              io.println("Receive error: " <> err)
              Error("Failed to receive exit status")
            }
          }
        }
        False -> {
          io.println(
            "Size mismatch: got "
            <> int_to_string(size)
            <> ", expected "
            <> int_to_string(expected_bytes),
          )
          Error("Size mismatch")
        }
      }
    }
    Ok(other) -> {
      io.println("Expected Data, got: " <> debug_port_message(other))
      Error("Expected Data message")
    }
    Error(err) -> {
      io.println("Receive error: " <> err)
      Error("Failed to receive data")
    }
  }
}

/// Runtime test: receive_timeout returns Timeout after process exits
pub fn runtime_timed_receive_test() {
  run_timed_receive_test()
}

fn run_timed_receive_test() -> Nil {
  let #(executable, args, _) = runtime_test_command()

  // Spawn the port
  let port = port_ffi.ffi_open_port(executable, args, "")

  // Run test and capture result
  let result = run_timed_receive_test_body(port)

  // Always close the port, even on failure
  port_ffi.ffi_close_port(port)

  // Now panic if test failed
  case result {
    Ok(Nil) -> Nil
    Error(msg) -> panic as msg
  }
}

fn run_timed_receive_test_body(port: port_ffi.Port) -> Result(Nil, String) {
  // Drain data message
  case port_ffi.receive_blocking(port) {
    Ok(port_ffi.Data(_)) -> {
      // Drain exit status message
      case port_ffi.receive_blocking(port) {
        Ok(port_ffi.ExitStatus(_)) -> {
          // Now port is closed - receive_timeout should return Timeout
          case port_ffi.receive_timeout(port, 100) {
            Ok(port_ffi.Timeout) -> Ok(Nil)
            Ok(other) -> {
              io.println("Expected Timeout, got: " <> debug_port_message(other))
              Error("Expected Timeout after exit")
            }
            Error(err) -> {
              io.println("Receive error: " <> err)
              Error("Failed during timed receive")
            }
          }
        }
        _ -> Error("Expected ExitStatus message")
      }
    }
    _ -> Error("Expected Data message")
  }
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
  let bin = list_to_binary(chars)
  case bit_array.to_string(bin) {
    Ok(s) -> s
    Error(_) -> "<invalid UTF-8>"
  }
}
