/// Internal bidirectional runner for start_session() path.
/// This module is NOT part of the public API - it's used internally by GenServer.
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom

import claude_agent_sdk/internal/port_ffi.{
  type Port, type WriteError, ffi_close_port, find_cli_path, port_write,
  wrap_port,
}

/// FFI binding to erlang:make_ref/0 for creating unique references.
@external(erlang, "erlang", "make_ref")
fn make_ref() -> Dynamic

/// Push-based runner for start_session() (bidir mode only).
/// This is INTERNAL - not exported from the public API.
pub type BidirRunner {
  BidirRunner(
    port: Port,
    write: fn(String) -> Result(Nil, WriteError),
    close: fn() -> Nil,
  )
}

/// Error type for BidirRunner operations.
pub type StartError {
  /// Placeholder error - start() not yet implemented.
  NotImplemented
  /// Failed to spawn the CLI process.
  SpawnFailed(String)
}

/// FFI binding for open_port_bidir - spawns executable with bidir options.
/// Returns {ok, Port} | {error, Reason}.
@external(erlang, "claude_agent_sdk_ffi", "open_port_bidir")
fn ffi_open_port_bidir(executable: String, args: List(String)) -> Dynamic

/// Open a port with bidirectional protocol options.
/// Uses spawn_executable with binary, exit_status, use_stdio,
/// and stderr_to_stdout (on OTP >= 25).
fn open_port_bidir(
  executable: String,
  args: List(String),
) -> Result(Port, String) {
  let result = ffi_open_port_bidir(executable, args)
  // Decode the tuple: element 0 is tag (ok/error), element 1 is payload
  let result_decoder = {
    use tag <- decode.field(0, decode.string)
    use payload <- decode.field(1, decode.dynamic)
    decode.success(#(tag, payload))
  }
  case decode.run(result, result_decoder) {
    Ok(#("ok", port_dynamic)) -> Ok(wrap_port(port_dynamic))
    Ok(#("error", reason_dynamic)) ->
      case decode.run(reason_dynamic, decode.string) {
        Ok(reason) -> Error(reason)
        Error(_) -> Error("unknown spawn error")
      }
    Ok(_) -> Error("invalid FFI response tag")
    Error(_) -> Error("invalid FFI response format")
  }
}

/// Start a BidirRunner with the given CLI arguments.
/// Spawns the Claude CLI with --output-format stream-json --input-format stream-json.
///
/// IMPORTANT: This function must be called inside an actor.init callback
/// to ensure the port is owned by the GenServer process.
pub fn start(args: List(String)) -> Result(BidirRunner, StartError) {
  // Find the Claude CLI executable
  case find_cli_path("claude") {
    Error(reason) -> Error(SpawnFailed("claude not found: " <> reason))
    Ok(claude_path) -> start_with_path(claude_path, args)
  }
}

/// Start a BidirRunner with an explicit executable path.
/// Prepends --output-format stream-json --input-format stream-json to args.
pub fn start_with_path(
  executable_path: String,
  args: List(String),
) -> Result(BidirRunner, StartError) {
  // Build full args: --output-format stream-json --input-format stream-json + user args
  let full_args = [
    "--output-format",
    "stream-json",
    "--input-format",
    "stream-json",
    ..args
  ]
  start_raw(executable_path, full_args)
}

/// Start a BidirRunner with raw arguments (no stream-json args prepended).
/// Used for testing with non-claude executables that don't accept those flags.
pub fn start_raw(
  executable_path: String,
  args: List(String),
) -> Result(BidirRunner, StartError) {
  case open_port_bidir(executable_path, args) {
    Error(reason) -> Error(SpawnFailed(reason))
    Ok(port) -> {
      // Create the write function that sends data to the port
      let write_fn = fn(data: String) -> Result(Nil, WriteError) {
        port_write(port, data)
      }
      // Create the close function that closes the port (uses safe wrapper with drain)
      let close_fn = fn() -> Nil { ffi_close_port(port) }
      Ok(BidirRunner(port: port, write: write_fn, close: close_fn))
    }
  }
}

/// Port message type for native Erlang port tuples.
/// These are the raw messages received by GenServer in handle_info.
pub type PortMessage {
  PortData(BitArray)
  PortExitStatus(Int)
}

/// Decode native Erlang port tuples into typed PortMessage values.
///
/// Port messages arrive as: {Port, {data, Binary}} or {Port, {exit_status, Code}}
/// Returns Error(Nil) if the message doesn't match expected patterns or wrong port.
pub fn decode_port_message(msg: Dynamic, port: Port) -> Result(PortMessage, Nil) {
  // Decode outer tuple: {Port, Payload}
  let outer_decoder = {
    use msg_port <- decode.field(0, decode.dynamic)
    use payload <- decode.field(1, decode.dynamic)
    decode.success(#(msg_port, payload))
  }

  case decode.run(msg, outer_decoder) {
    Ok(#(msg_port, payload)) -> {
      // Check if the port matches (compare underlying dynamic values)
      case ports_equal(msg_port, port) {
        False -> Error(Nil)
        True -> decode_payload(payload)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Compare a Dynamic port value with a typed Port
fn ports_equal(dynamic_port: Dynamic, port: Port) -> Bool {
  // Use public accessor to get raw port from wrapper
  let port_inner = port_ffi.port_to_dynamic(port)
  exact_equals(dynamic_port, port_inner)
}

/// FFI to compare two dynamic values for exact equality
@external(erlang, "claude_agent_sdk_ffi", "exact_equals")
fn exact_equals(a: Dynamic, b: Dynamic) -> Bool

/// Decode the inner payload tuple: {atom, value}
fn decode_payload(payload: Dynamic) -> Result(PortMessage, Nil) {
  let payload_decoder = {
    use tag <- decode.field(0, atom.decoder())
    use value <- decode.field(1, decode.dynamic)
    decode.success(#(tag, value))
  }

  case decode.run(payload, payload_decoder) {
    Ok(#(tag, value)) -> {
      let tag_str = atom.to_string(tag)
      case tag_str {
        "data" -> decode_data(value)
        "exit_status" -> decode_exit_status(value)
        _ -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Decode data payload as BitArray
fn decode_data(value: Dynamic) -> Result(PortMessage, Nil) {
  case decode.run(value, decode.bit_array) {
    Ok(bytes) -> Ok(PortData(bytes))
    Error(_) -> Error(Nil)
  }
}

/// Decode exit_status payload as Int
fn decode_exit_status(value: Dynamic) -> Result(PortMessage, Nil) {
  case decode.run(value, decode.int) {
    Ok(code) -> Ok(PortExitStatus(code))
    Error(_) -> Error(Nil)
  }
}

/// Create a mock BidirRunner for testing.
///
/// The mock runner uses the provided callbacks instead of a real port:
/// - `on_write` is called when write() is invoked
/// - `on_close` is called when close() is invoked
///
/// The port field contains a unique reference that can be used for pattern
/// matching in tests (e.g., with decode_port_message).
pub fn mock(
  on_write on_write: fn(String) -> Result(Nil, WriteError),
  on_close on_close: fn() -> Nil,
) -> BidirRunner {
  // Create a unique reference as a pseudo-port for pattern matching
  let mock_port = wrap_port(make_ref())
  BidirRunner(port: mock_port, write: on_write, close: on_close)
}
