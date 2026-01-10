/// FFI bindings to Erlang port operations.
/// This module provides the Gleam interface to claude_agent_sdk_ffi.erl
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode

/// Port reference - opaque wrapper around Erlang port term
/// Represented as Dynamic internally; type safety comes from the opaque API
pub opaque type Port {
  Port(inner: Dynamic)
}

/// Internal message type (NOT public API)
pub type PortMessage {
  Data(BitArray)
  ExitStatus(Int)
  Eof
  Timeout
}

/// FFI binding - returns Dynamic which we wrap in opaque Port type
@external(erlang, "claude_agent_sdk_ffi", "open_port")
fn ffi_open_port_raw(path: String, args: List(String), cwd: String) -> Dynamic

/// Open port with type-safe wrapper
pub fn ffi_open_port(path: String, args: List(String), cwd: String) -> Port {
  Port(ffi_open_port_raw(path, args, cwd))
}

/// Extract inner Dynamic for FFI calls
fn port_to_dynamic(port: Port) -> Dynamic {
  let Port(inner) = port
  inner
}

@external(erlang, "claude_agent_sdk_ffi", "receive_port_msg_blocking")
fn ffi_receive_blocking_raw(port: Dynamic) -> Dynamic

@external(erlang, "claude_agent_sdk_ffi", "receive_port_msg_timeout")
fn ffi_receive_timeout_raw(port: Dynamic, timeout_ms: Int) -> Dynamic

@external(erlang, "claude_agent_sdk_ffi", "close_port")
fn ffi_close_port_raw(port: Dynamic) -> Nil

pub fn ffi_close_port(port: Port) -> Nil {
  ffi_close_port_raw(port_to_dynamic(port))
}

/// Blocking receive - returns Result to propagate decode errors
pub fn receive_blocking(port: Port) -> Result(PortMessage, String) {
  ffi_receive_blocking_raw(port_to_dynamic(port)) |> decode_port_message
}

/// Timed receive - returns Result to propagate decode errors
pub fn receive_timeout(
  port: Port,
  timeout_ms: Int,
) -> Result(PortMessage, String) {
  ffi_receive_timeout_raw(port_to_dynamic(port), timeout_ms)
  |> decode_port_message
}

/// Canonical decoder for port messages returned by Erlang FFI.
/// FFI returns 2-tuples with string tags: {"data", Bytes}, {"exit_status", Code},
/// {"eof", nil}, {"timeout", nil}
fn decode_port_message(raw: Dynamic) -> Result(PortMessage, String) {
  // Decode the tuple: element 0 is tag (string), element 1 is payload
  let tag_decoder = {
    use tag <- decode.field(0, decode.string)
    use payload <- decode.field(1, decode.dynamic)
    decode.success(#(tag, payload))
  }
  case decode.run(raw, tag_decoder) {
    Ok(#("data", payload)) ->
      case decode.run(payload, decode.bit_array) {
        Ok(bytes) -> Ok(Data(bytes))
        Error(_) -> Error("Invalid data payload: expected BitArray")
      }
    Ok(#("exit_status", payload)) ->
      case decode.run(payload, decode.int) {
        Ok(code) -> Ok(ExitStatus(code))
        Error(_) -> Error("Invalid exit_status: expected Int")
      }
    Ok(#("eof", _)) -> Ok(Eof)
    Ok(#("timeout", _)) -> Ok(Timeout)
    Ok(#(_, _)) -> Error("Unknown FFI message tag")
    Error(_) -> Error("Unknown FFI message format")
  }
}
