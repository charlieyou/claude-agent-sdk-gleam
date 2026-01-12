//// Tests for OTP version detection and stderr_to_stdout support checking.
//// These functions detect whether the runtime supports the stderr_to_stdout
//// port option (OTP 25+).

import claude_agent_sdk/internal/port_ffi.{
  get_otp_version, get_otp_version_safe, supports_stderr_to_stdout,
}
import gleeunit/should

// ============================================================================
// get_otp_version tests
// ============================================================================

pub fn get_otp_version_returns_positive_integer_test() {
  let version = get_otp_version()
  // OTP version should be a positive integer (e.g., 25, 26, 27)
  { version > 0 } |> should.be_true
}

pub fn get_otp_version_returns_reasonable_value_test() {
  let version = get_otp_version()
  // Should be at least 20 (ancient OTP) and less than 50 (far future)
  // This test ensures we're parsing correctly, not getting garbage
  { version >= 20 && version < 50 } |> should.be_true
}

// ============================================================================
// get_otp_version_safe tests
// ============================================================================

pub fn get_otp_version_safe_returns_ok_test() {
  let result = get_otp_version_safe()
  // Should return Ok with a version on standard OTP builds
  case result {
    Ok(version) -> { version > 0 } |> should.be_true
    Error(_) -> should.fail()
  }
}

pub fn get_otp_version_safe_matches_legacy_function_test() {
  // The safe version should return the same value as the legacy function
  // on standard OTP builds
  let safe_result = get_otp_version_safe()
  let legacy_result = get_otp_version()
  case safe_result {
    Ok(version) -> version |> should.equal(legacy_result)
    Error(_) -> legacy_result |> should.equal(0)
  }
}

// ============================================================================
// supports_stderr_to_stdout tests
// ============================================================================

pub fn supports_stderr_to_stdout_returns_bool_test() {
  // Just verify it returns a boolean without crashing
  let _result = supports_stderr_to_stdout()
  // If we get here, function works
  should.be_true(True)
}

pub fn supports_stderr_to_stdout_consistent_with_version_test() {
  let version = get_otp_version()
  let supports = supports_stderr_to_stdout()
  // stderr_to_stdout is supported in OTP >= 25
  case version >= 25 {
    True -> supports |> should.be_true
    False -> supports |> should.be_false
  }
}
