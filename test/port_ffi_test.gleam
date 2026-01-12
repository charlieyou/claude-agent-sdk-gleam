import claude_agent_sdk/internal/port_ffi.{
  Data, ExitStatus, ffi_open_port, port_write, receive_timeout,
}
import gleam/bit_array
import gleeunit/should

/// Test port_write to a valid port (echo process)
pub fn port_write_to_valid_port_test() {
  // Spawn cat which echoes stdin to stdout
  let port = ffi_open_port("/bin/cat", [], "")

  // Write data to port
  let write_result = port_write(port, "hello\n")
  write_result |> should.be_ok

  // Read back the echoed data
  let read_result = receive_timeout(port, 1000)
  read_result |> should.be_ok
  case read_result {
    Ok(Data(bytes)) -> {
      let text = bit_array.to_string(bytes)
      text |> should.be_ok
      case text {
        Ok(s) -> s |> should.equal("hello\n")
        Error(_) -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

/// Test port_write to a closed port returns error
pub fn port_write_to_closed_port_test() {
  // Spawn a process that exits immediately
  let port = ffi_open_port("/bin/true", [], "")

  // Wait for it to exit
  let exit_result = receive_timeout(port, 1000)
  case exit_result {
    Ok(ExitStatus(0)) -> Nil
    _ -> should.fail()
  }

  // Write to closed port should return error
  let write_result = port_write(port, "hello")
  write_result |> should.be_error
}
