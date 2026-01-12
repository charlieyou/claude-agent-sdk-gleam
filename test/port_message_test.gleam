/// Tests for port message decoder
/// The decoder handles native Erlang port tuples: {Port, {data, Binary}}, {Port, {exit_status, Code}}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom

import gleeunit

import claude_agent_sdk/internal/bidir_runner.{
  PortData, PortExitStatus, decode_port_message,
}
import claude_agent_sdk/internal/port_ffi

pub fn main() -> Nil {
  gleeunit.main()
}

/// Helper to convert any value to Dynamic (identity function in Erlang)
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic

/// Extract raw Erlang port from Port wrapper for constructing test messages.
/// Port runtime shape is {port, RawPort}, so element(2, Port) gets the raw port.
/// Real Erlang port messages contain the raw port, not the Gleam wrapper.
fn extract_raw_port(port: port_ffi.Port) -> Dynamic {
  ffi_element(2, port)
}

@external(erlang, "erlang", "element")
fn ffi_element(index: Int, tuple: port_ffi.Port) -> Dynamic

/// Create a mock port for testing by spawning a quick command
fn create_mock_port() -> port_ffi.Port {
  // Spawn a no-op process that exits immediately
  port_ffi.ffi_open_port("/bin/true", [], "")
}

/// Test decoding {Port, {data, Binary}} message
pub fn decode_port_data_test() {
  let mock_port = create_mock_port()
  // Build the tuple {RawPort, {data, <<"hello\n">>}} - use raw port, not wrapper
  let data_atom = atom.create("data")
  let payload = <<"hello\n":utf8>>
  let inner_tuple = #(atom.to_dynamic(data_atom), to_dynamic(payload))
  let data_msg = #(extract_raw_port(mock_port), to_dynamic(inner_tuple))

  let result = decode_port_message(to_dynamic(data_msg), mock_port)

  // Cleanup
  port_ffi.ffi_close_port(mock_port)

  case result {
    Ok(PortData(bytes)) ->
      case bytes == <<"hello\n":utf8>> {
        True -> Nil
        False -> panic as "Expected PortData with 'hello\\n'"
      }
    _ -> panic as "Expected Ok(PortData(...))"
  }
}

/// Test decoding {Port, {exit_status, Code}} message
pub fn decode_port_exit_status_test() {
  let mock_port = create_mock_port()
  // Build the tuple {RawPort, {exit_status, 0}} - use raw port, not wrapper
  let exit_atom = atom.create("exit_status")
  let inner_tuple = #(atom.to_dynamic(exit_atom), to_dynamic(0))
  let exit_msg = #(extract_raw_port(mock_port), to_dynamic(inner_tuple))

  let result = decode_port_message(to_dynamic(exit_msg), mock_port)

  // Cleanup
  port_ffi.ffi_close_port(mock_port)

  case result {
    Ok(PortExitStatus(code)) ->
      case code == 0 {
        True -> Nil
        False -> panic as "Expected PortExitStatus(0)"
      }
    _ -> panic as "Expected Ok(PortExitStatus(...))"
  }
}

/// Test decoding with wrong port returns Error
pub fn decode_wrong_port_test() {
  let mock_port = create_mock_port()
  let other_port = create_mock_port()
  // Build tuple with other_port's raw port but try to decode for mock_port
  let data_atom = atom.create("data")
  let inner_tuple = #(atom.to_dynamic(data_atom), to_dynamic(<<"x":utf8>>))
  let wrong_port_msg = #(extract_raw_port(other_port), to_dynamic(inner_tuple))

  let result = decode_port_message(to_dynamic(wrong_port_msg), mock_port)

  // Cleanup
  port_ffi.ffi_close_port(mock_port)
  port_ffi.ffi_close_port(other_port)

  case result {
    Error(Nil) -> Nil
    _ -> panic as "Expected Error(Nil) for wrong port"
  }
}

/// Test decoding unknown message shape returns Error
pub fn decode_unknown_shape_test() {
  let mock_port = create_mock_port()
  // Build unknown tuple {RawPort, {unknown, data}} - use raw port
  let unknown_atom = atom.create("unknown")
  let inner_tuple = #(atom.to_dynamic(unknown_atom), to_dynamic("data"))
  let unknown_msg = #(extract_raw_port(mock_port), to_dynamic(inner_tuple))

  let result = decode_port_message(to_dynamic(unknown_msg), mock_port)

  // Cleanup
  port_ffi.ffi_close_port(mock_port)

  case result {
    Error(Nil) -> Nil
    _ -> panic as "Expected Error(Nil) for unknown message shape"
  }
}

/// Test decoding non-tuple returns Error
pub fn decode_non_tuple_test() {
  let mock_port = create_mock_port()

  let result = decode_port_message(to_dynamic("not a tuple"), mock_port)

  // Cleanup
  port_ffi.ffi_close_port(mock_port)

  case result {
    Error(Nil) -> Nil
    _ -> panic as "Expected Error(Nil) for non-tuple"
  }
}
