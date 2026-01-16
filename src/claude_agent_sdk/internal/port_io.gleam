/// Port I/O abstraction layer.
///
/// This module provides the ONLY access point to port_ffi for production code
/// (src/). All port operations go through this interface to enable testability
/// via mock implementations.
///
/// Note: Test code (test/) may import port_ffi directly when testing the FFI
/// layer itself or when direct port access is needed for test fixtures.
///
/// Functions exposed:
/// - open_port, open_port_safe: Port spawning
/// - close_port: Port cleanup
/// - connect_port: Connect port to a process
/// - receive_timeout, receive_blocking: Port message receiving
/// - write: Port write operations
/// - monotonic_time_ms: Timing for deadlines
/// - find_cli_path: CLI discovery
/// - rescue: Panic recovery
///
/// Types re-exported from port_ffi:
/// - Port, PortMessage (Data, ExitStatus, Eof, Timeout), WriteError
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid}

import claude_agent_sdk/internal/port_ffi

// ============================================================================
// Type Re-exports
// ============================================================================

/// Port reference - opaque wrapper around Erlang port term
pub type Port =
  port_ffi.Port

/// Port message variants - re-exported from port_ffi for pattern matching
pub type PortMessage {
  Data(BitArray)
  ExitStatus(Int)
  Eof
  Timeout
}

/// Error type for port write operations
pub type WriteError {
  PortClosed
}

// ============================================================================
// Port Lifecycle
// ============================================================================

/// Open a port (can crash on spawn failure).
/// Use open_port_safe for graceful error handling.
pub fn open_port(path: String, args: List(String), cwd: String) -> Port {
  port_ffi.ffi_open_port(path, args, cwd)
}

/// Open a port safely - returns Result instead of crashing on spawn failure.
pub fn open_port_safe(
  path: String,
  args: List(String),
  cwd: String,
) -> Result(Port, String) {
  port_ffi.ffi_open_port_safe(path, args, cwd)
}

/// Close a port.
pub fn close_port(port: Port) -> Nil {
  port_ffi.ffi_close_port(port)
}

/// Connect a port to another process so it receives port messages.
pub fn connect_port(port: Port, pid: Pid) -> Nil {
  port_ffi.connect_port(port, pid)
}

// ============================================================================
// Port I/O
// ============================================================================

/// Receive a port message with timeout.
pub fn receive_timeout(
  port: Port,
  timeout_ms: Int,
) -> Result(PortMessage, String) {
  case port_ffi.receive_timeout(port, timeout_ms) {
    Ok(port_ffi.Data(bytes)) -> Ok(Data(bytes))
    Ok(port_ffi.ExitStatus(code)) -> Ok(ExitStatus(code))
    Ok(port_ffi.Eof) -> Ok(Eof)
    Ok(port_ffi.Timeout) -> Ok(Timeout)
    Error(e) -> Error(e)
  }
}

/// Receive a port message, blocking indefinitely.
pub fn receive_blocking(port: Port) -> Result(PortMessage, String) {
  case port_ffi.receive_blocking(port) {
    Ok(port_ffi.Data(bytes)) -> Ok(Data(bytes))
    Ok(port_ffi.ExitStatus(code)) -> Ok(ExitStatus(code))
    Ok(port_ffi.Eof) -> Ok(Eof)
    Ok(port_ffi.Timeout) -> Ok(Timeout)
    Error(e) -> Error(e)
  }
}

/// Write data to a port's stdin.
pub fn write(port: Port, data: String) -> Result(Nil, WriteError) {
  case port_ffi.port_write(port, data) {
    Ok(Nil) -> Ok(Nil)
    Error(port_ffi.PortClosed) -> Error(PortClosed)
  }
}

// ============================================================================
// Utilities
// ============================================================================

/// Returns monotonic time in milliseconds.
/// Use for deadline-based timeouts that don't reset on each message.
pub fn monotonic_time_ms() -> Int {
  port_ffi.monotonic_time_ms()
}

/// Find an executable by name in PATH.
pub fn find_cli_path(name: String) -> Result(String, String) {
  port_ffi.find_cli_path(name)
}

/// Rescue a function call, catching panics and other exceptions.
pub fn rescue(thunk: fn() -> a) -> Result(a, String) {
  port_ffi.rescue(thunk)
}

// ============================================================================
// Internal accessors (for FFI interop)
// ============================================================================

/// Extract raw Erlang port reference from the Port wrapper.
/// Used for FFI calls and comparing port references in decoded messages.
pub fn port_to_dynamic(port: Port) -> Dynamic {
  port_ffi.port_to_dynamic(port)
}

/// Wrap a Dynamic value as a Port.
/// Used when decoding port references from FFI responses.
pub fn wrap_port(dynamic: Dynamic) -> Port {
  port_ffi.wrap_port(dynamic)
}

// ============================================================================
// Process Signal Operations
// ============================================================================

/// Get the OS process ID for a spawned port.
/// Returns Ok(pid) if the port has an OS process, Error otherwise.
pub fn get_port_os_pid(port: Port) -> Result(Int, String) {
  port_ffi.get_port_os_pid(port)
}

/// Send a signal to an OS process.
/// Returns Ok(Nil) on success, Error with reason on failure.
pub fn os_kill(os_pid: Int, signal: Int) -> Result(Nil, String) {
  port_ffi.os_kill(os_pid, signal)
}

/// SIGTERM signal number (15).
pub const sigterm: Int = 15

/// SIGKILL signal number (9).
pub const sigkill: Int = 9
