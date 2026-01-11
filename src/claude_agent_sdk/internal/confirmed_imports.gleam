// Phase 0 verified these imports exist at pinned versions.
// All modules should import from here to ensure consistency.
//
// Verified: 2026-01-11
// Environment: Gleam 1.14.0, OTP 25, Linux

// Standard library (verified at gleam_stdlib == 0.68.1)
import gleam/bit_array

// byte_size, from_string, to_string
import gleam/dynamic.{type Dynamic}

// Dynamic type for FFI
import gleam/dynamic/decode

// field, string, int, bit_array
import gleam/list

// map, filter, fold
import gleam/option

// Some, None, unwrap
import gleam/result

// map, try, unwrap
import gleam/string

// inspect, concat

// JSON (verified at gleam_json == 3.1.0)
import gleam/json

// decode, object, array, string, int

// Erlang interop (verified at gleam_erlang == 1.3.0)
import gleam/erlang/process

// Subject, send, receive, start

// Re-export version info for documentation
pub fn verified_stdlib_version() -> String {
  "0.68.1"
}

pub fn verified_json_version() -> String {
  "3.1.0"
}

pub fn verified_erlang_version() -> String {
  "1.3.0"
}

// Ensure imports are used (prevents unused import warnings)
pub fn verify_imports() -> Bool {
  let _ = bit_array.byte_size(<<>>)
  let _: Dynamic = dynamic.string("test")
  let _ = decode.string
  let _ = list.map([], fn(x) { x })
  let _ = option.None
  let _ = result.map(Ok(1), fn(x) { x })
  let _ = string.inspect(1)
  let _ = json.string("")
  let _ = process.self()
  True
}
