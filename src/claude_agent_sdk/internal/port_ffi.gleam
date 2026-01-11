/// FFI bindings to Erlang port operations.
/// This module provides the Gleam interface to claude_agent_sdk_ffi.erl
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/string

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

/// FFI binding for safe open - returns {ok, Port} | {error, Reason}
@external(erlang, "claude_agent_sdk_ffi", "open_port_safe")
fn ffi_open_port_safe_raw(
  path: String,
  args: List(String),
  cwd: String,
) -> Dynamic

/// Open port safely - returns Result instead of crashing on spawn failure.
/// Use this for version detection where spawn failures should be handled gracefully.
pub fn ffi_open_port_safe(
  path: String,
  args: List(String),
  cwd: String,
) -> Result(Port, String) {
  let result = ffi_open_port_safe_raw(path, args, cwd)
  // Decode the tuple: element 0 is tag atom (ok/error), element 1 is payload
  let result_decoder = {
    use tag <- decode.field(0, decode.string)
    use payload <- decode.field(1, decode.dynamic)
    decode.success(#(tag, payload))
  }
  case decode.run(result, result_decoder) {
    Ok(#("ok", port_dynamic)) -> Ok(Port(port_dynamic))
    Ok(#("error", reason_dynamic)) ->
      case decode.run(reason_dynamic, decode.string) {
        Ok(reason) -> Error(reason)
        Error(_) -> Error("unknown spawn error")
      }
    Ok(_) -> Error("invalid FFI response tag")
    Error(_) -> Error("invalid FFI response format")
  }
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

// ============================================================================
// CLI Path Discovery
// ============================================================================

/// FFI binding for finding CLI in PATH
@external(erlang, "claude_agent_sdk_ffi", "find_cli_path")
fn ffi_find_cli_path_raw(name: String) -> Dynamic

/// Find an executable by name in PATH.
/// Returns Ok(absolute_path) if found, Error("not_found") otherwise.
pub fn find_cli_path(name: String) -> Result(String, String) {
  let result = ffi_find_cli_path_raw(name)
  // Decode the tuple: element 0 is tag atom (ok/error), element 1 is payload
  let result_decoder = {
    use tag <- decode.field(0, decode.string)
    use payload <- decode.field(1, decode.string)
    decode.success(#(tag, payload))
  }
  case decode.run(result, result_decoder) {
    Ok(#("ok", path)) -> Ok(path)
    Ok(#("error", reason)) -> Error(reason)
    Ok(_) -> Error("invalid FFI response tag")
    Error(_) -> Error("invalid FFI response format")
  }
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
        Error(errors) ->
          Error(
            "Invalid data payload: expected BitArray. Details: "
            <> string.inspect(errors),
          )
      }
    Ok(#("exit_status", payload)) ->
      case decode.run(payload, decode.int) {
        Ok(code) -> Ok(ExitStatus(code))
        Error(errors) ->
          Error(
            "Invalid exit_status: expected Int. Details: "
            <> string.inspect(errors),
          )
      }
    Ok(#("eof", _)) -> Ok(Eof)
    Ok(#("timeout", _)) -> Ok(Timeout)
    Ok(#(_, _)) -> Error("Unknown FFI message tag")
    Error(errors) ->
      Error(
        "Invalid FFI message: expected 2-tuple with string tag. Details: "
        <> string.inspect(errors),
      )
  }
}
