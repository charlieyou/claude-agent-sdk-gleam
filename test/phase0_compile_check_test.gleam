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

/// Placeholder test - will be replaced with actual FFI verification tests
/// once the FFI module exists.
pub fn sdk_version_test() {
  let v = claude_agent_sdk.version()
  assert v == "0.1.0"
}
