/// Internal bidirectional runner for start_session() path.
/// This module is NOT part of the public API - it's used internally by GenServer.
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom

import claude_agent_sdk/internal/port_ffi.{type Port, type WriteError}

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

/// Start a BidirRunner with the given CLI arguments.
///
/// NOTE: This is a stub that returns Error(NotImplemented).
/// Actual implementation will come in a subsequent task.
pub fn start(_args: List(String)) -> Result(BidirRunner, StartError) {
  Error(NotImplemented)
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
  // Extract the inner dynamic from the opaque Port and compare
  let port_dynamic = port_to_dynamic(port)
  dynamic_equals(dynamic_port, port_dynamic)
}

/// FFI to extract inner Dynamic from opaque Port type
@external(erlang, "gleam_stdlib", "identity")
fn port_to_dynamic(port: Port) -> Dynamic

/// FFI to compare two dynamic values for equality
@external(erlang, "erlang", "=:=")
fn dynamic_equals(a: Dynamic, b: Dynamic) -> Bool

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
