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

/// Error type for port write operations
pub type WriteError {
  PortClosed
}

/// FFI binding for port_write - returns {ok, nil} | {error, reason}
@external(erlang, "claude_agent_sdk_ffi", "port_write")
fn ffi_port_write_raw(port: Dynamic, data: String) -> Dynamic

/// Write data to a port's stdin.
/// Returns Ok(Nil) on success, Error(PortClosed) if the port is closed.
pub fn port_write(port: Port, data: String) -> Result(Nil, WriteError) {
  let result = ffi_port_write_raw(port_to_dynamic(port), data)
  let result_decoder = {
    use tag <- decode.field(0, decode.string)
    decode.success(tag)
  }
  case decode.run(result, result_decoder) {
    Ok("ok") -> Ok(Nil)
    Ok("error") -> Error(PortClosed)
    Ok(_) -> Error(PortClosed)
    Error(_) -> Error(PortClosed)
  }
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

// ============================================================================
// Panic Recovery (for resource safety)
// ============================================================================

/// Rescue a function call, catching panics and other exceptions.
/// Returns Ok(result) on success, Error(message) on panic/throw/exit.
/// Used to ensure cleanup (e.g., with_stream closes port) even when callbacks panic.
@external(erlang, "claude_agent_sdk_ffi", "rescue")
pub fn rescue(thunk: fn() -> a) -> Result(a, String)

/// Returns monotonic time in milliseconds.
/// Use for deadline-based timeouts that don't reset on each message.
/// Monotonic time is guaranteed to never go backwards (unlike wall clock time).
@external(erlang, "claude_agent_sdk_ffi", "monotonic_time_ms")
pub fn monotonic_time_ms() -> Int

// ============================================================================
// OTP Version Detection
// ============================================================================

/// Returns the OTP major version as an integer (e.g., 25, 26, 27).
/// Use this to check runtime capabilities that depend on OTP version.
@external(erlang, "claude_agent_sdk_ffi", "otp_version")
pub fn get_otp_version() -> Int

/// FFI binding for check_stderr_support - returns {supported, Bool}
@external(erlang, "claude_agent_sdk_ffi", "check_stderr_support")
fn ffi_check_stderr_support() -> Dynamic

/// Check if the current OTP version supports the stderr_to_stdout port option.
/// Returns True for OTP >= 25, False otherwise.
///
/// The stderr_to_stdout option allows capturing both stdout and stderr from
/// a spawned process through a single port. Without this option (OTP < 25),
/// stderr from the CLI goes directly to the terminal and is not captured by
/// the SDK.
///
/// Fallback behavior for OTP < 25:
/// - CLI logs (written to stderr) will appear on the terminal, not in SDK
/// - This is acceptable for debugging but means logs aren't programmatically accessible
/// - Recommend OTP 25+ for full functionality
pub fn supports_stderr_to_stdout() -> Bool {
  let result = ffi_check_stderr_support()
  let result_decoder = {
    use _tag <- decode.field(0, decode.string)
    use supported <- decode.field(1, decode.bool)
    decode.success(supported)
  }
  case decode.run(result, result_decoder) {
    Ok(supported) -> supported
    // If decode fails, assume not supported (safe default)
    Error(_) -> False
  }
}
