/// Phase 0 Compile Check Tests (Suite A)
///
/// These tests verify that required APIs exist at the pinned dependency versions.
/// Suite A validates: gleam build succeeds, FFI bindings exist and typecheck.
/// No subprocess spawning - pure compile-time and basic runtime checks.
import claude_agent_sdk
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/string
import gleeunit

// This import verifies FFI module compiles and typechecks:
import claude_agent_sdk/internal/port_ffi

pub fn main() -> Nil {
  gleeunit.main()
}

/// Verifies SDK version constant is accessible
pub fn sdk_version_test() {
  let v = claude_agent_sdk.version()
  assert v == "0.1.0"
}

/// Verifies required stdlib APIs exist by referencing their function types.
/// If compilation succeeds, the APIs exist in the pinned dependency versions.
/// No runtime behavior tested - just existence.
pub fn api_exists_compile_check_test() {
  // bit_array functions - used for port data handling
  let _ = bit_array.from_string
  let _ = bit_array.byte_size
  let _ = bit_array.to_string

  // dynamic module - used for FFI message decoding
  let _ = dynamic.string
  let _ = dynamic.int
  let _ = dynamic.bit_array
  let _ = dynamic.classify

  // dynamic/decode module - used for tagged tuple decoding
  let _ = decode.field
  let _ = decode.string
  let _ = decode.int
  let _ = decode.bit_array
  let _ = decode.dynamic
  let _ = decode.success
  let _ = decode.run

  // string module - used for error formatting
  let _ = string.inspect

  Nil
}

/// Verifies port_ffi module loads and function types are accessible.
/// This is a compile-time check - if it compiles, FFI bindings exist.
/// No actual port operations (no subprocess spawning).
pub fn ffi_bindings_typecheck_test() {
  // Reference the Port type exists (opaque, so we reference functions that use it)
  let _ = port_ffi.ffi_open_port
  let _ = port_ffi.ffi_close_port
  let _ = port_ffi.receive_blocking
  let _ = port_ffi.receive_timeout

  // Reference the PortMessage type constructors exist
  let _ = port_ffi.Data
  let _ = port_ffi.ExitStatus
  let _ = port_ffi.Eof
  let _ = port_ffi.Timeout

  Nil
}

/// Runtime verification of stdlib behaviors at pinned versions.
/// These test actual functionality, not just existence.
pub fn api_confirmation_test() {
  // bit_array: from_string and to_string roundtrip
  let bytes = bit_array.from_string("hello")
  assert bit_array.byte_size(bytes) == 5
  assert bit_array.to_string(bytes) == Ok("hello")

  // dynamic/decode: decode a tagged tuple (simulates FFI message format)
  let tagged: Dynamic = dynamic.string("test_tag")
  let decoder = decode.string
  case decode.run(tagged, decoder) {
    Ok(val) -> {
      assert val == "test_tag"
    }
    Error(_) -> panic as "dynamic decode failed"
  }

  // Test tuple decoding (matches port_ffi message format)
  let tuple_decoder = {
    use tag <- decode.field(0, decode.string)
    use payload <- decode.field(1, decode.bit_array)
    decode.success(#(tag, payload))
  }
  // Note: Can't create Dynamic tuples directly, so we verify the decoder compiles
  let _ = tuple_decoder

  // string.inspect works for error messages
  let inspected = string.inspect([1, 2, 3])
  assert inspected == "[1, 2, 3]"

  Nil
}
