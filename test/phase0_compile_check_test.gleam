/// Phase 0 Compile Check Test
///
/// This test file imports FFI modules that don't exist yet.
/// It should fail to compile until FFI bindings are implemented.
/// Once FFI is complete, these imports will succeed and tests will pass.
import claude_agent_sdk
import gleeunit

// This import will fail until FFI module is implemented:
import claude_agent_sdk/internal/port_ffi

pub fn main() -> Nil {
  gleeunit.main()
}

/// Verifies SDK version constant is accessible
pub fn sdk_version_test() {
  let v = claude_agent_sdk.version()
  assert v == "0.1.0"
}

/// Verifies port_ffi module types are accessible and usable.
/// This ensures FFI bindings compiled correctly.
pub fn port_ffi_types_exist_test() {
  // Verify Port type and FFI functions exist by opening a simple port
  let port = port_ffi.ffi_open_port("/bin/echo", ["hello"], "")
  // Clean up the port
  port_ffi.ffi_close_port(port)
}
