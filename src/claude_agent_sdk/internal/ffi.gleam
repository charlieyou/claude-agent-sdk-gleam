/// FFI bindings to Erlang port operations.
/// This module provides the Gleam interface to claude_agent_sdk_ffi.erl
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode

/// Opaque type representing an Erlang port
pub type Port

/// Message received from a port
pub type PortMessage {
  Data(BitArray)
  ExitStatus(Int)
  Eof
  Timeout
}

/// Opens a port to spawn an executable with given args and working directory.
@external(erlang, "claude_agent_sdk_ffi", "open_port")
pub fn open_port(
  executable: String,
  args: List(String),
  working_dir: String,
) -> Port

/// Blocking receive for port messages.
@external(erlang, "claude_agent_sdk_ffi", "receive_port_msg_blocking")
fn receive_port_msg_blocking_raw(port: Port) -> #(String, Dynamic)

/// Timed receive for port messages.
@external(erlang, "claude_agent_sdk_ffi", "receive_port_msg_timeout")
fn receive_port_msg_timeout_raw(
  port: Port,
  timeout_ms: Int,
) -> #(String, Dynamic)

/// Closes the port and drains any remaining messages.
@external(erlang, "claude_agent_sdk_ffi", "close_port")
pub fn close_port(port: Port) -> Nil

/// Receives a message from the port, blocking until one arrives.
pub fn receive_blocking(port: Port) -> PortMessage {
  decode_port_message(receive_port_msg_blocking_raw(port))
}

/// Receives a message from the port with a timeout.
pub fn receive_timeout(port: Port, timeout_ms: Int) -> PortMessage {
  decode_port_message(receive_port_msg_timeout_raw(port, timeout_ms))
}

fn decode_port_message(raw: #(String, Dynamic)) -> PortMessage {
  case raw.0 {
    "data" -> {
      case decode.run(raw.1, decode.bit_array) {
        Ok(bytes) -> Data(bytes)
        Error(_) -> panic as "expected bit_array for data"
      }
    }
    "exit_status" -> {
      case decode.run(raw.1, decode.int) {
        Ok(code) -> ExitStatus(code)
        Error(_) -> panic as "expected int for exit_status"
      }
    }
    "eof" -> Eof
    "timeout" -> Timeout
    _ -> panic as "unexpected port message tag"
  }
}
