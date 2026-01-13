/// Tests for error module - is_terminal(), error_to_string, stream_error_to_string.
import claude_agent_sdk/error.{
  BufferOverflow, CliNotFoundError, ErrorDiagnostic, JsonDecodeError,
  ProcessError, SpawnError, TestModeError, TooManyDecodeErrors,
  UnexpectedMessageError, UnknownVersionError, UnsupportedCliVersionError,
  VersionDetectionError, diagnose_exit_code, error_to_string, is_terminal,
  stream_error_to_string,
}
import gleam/option.{None, Some}
import gleeunit/should

// ============================================================================
// is_terminal() Tests
// ============================================================================

pub fn process_error_is_terminal_test() {
  let diagnostic =
    ErrorDiagnostic(
      last_non_json_line: None,
      stdout_was_empty: True,
      exit_code_hint: "test",
      troubleshooting: "test",
    )
  ProcessError(1, diagnostic)
  |> is_terminal
  |> should.be_true
}

pub fn buffer_overflow_is_terminal_test() {
  BufferOverflow
  |> is_terminal
  |> should.be_true
}

pub fn too_many_decode_errors_is_terminal_test() {
  TooManyDecodeErrors(5, "last error")
  |> is_terminal
  |> should.be_true
}

pub fn json_decode_error_is_not_terminal_test() {
  JsonDecodeError("invalid json", "parse error")
  |> is_terminal
  |> should.be_false
}

pub fn unexpected_message_error_is_not_terminal_test() {
  UnexpectedMessageError("{\"type\":\"unknown\"}")
  |> is_terminal
  |> should.be_false
}

// ============================================================================
// diagnose_exit_code() Tests
// ============================================================================

pub fn diagnose_exit_code_1_empty_stdout_test() {
  let diagnostic = diagnose_exit_code(1, True)
  diagnostic.stdout_was_empty
  |> should.be_true
  diagnostic.exit_code_hint
  |> should.equal("Authentication required")
}

pub fn diagnose_exit_code_1_with_output_test() {
  let diagnostic = diagnose_exit_code(1, False)
  diagnostic.stdout_was_empty
  |> should.be_false
  diagnostic.exit_code_hint
  |> should.equal("General error (check if CLI is authenticated)")
}

pub fn diagnose_exit_code_2_test() {
  let diagnostic = diagnose_exit_code(2, False)
  diagnostic.exit_code_hint
  |> should.equal("Invalid arguments (check SDK version compatibility)")
}

pub fn diagnose_exit_code_126_test() {
  let diagnostic = diagnose_exit_code(126, False)
  diagnostic.exit_code_hint
  |> should.equal("Permission denied (check file permissions)")
}

pub fn diagnose_exit_code_127_test() {
  let diagnostic = diagnose_exit_code(127, False)
  diagnostic.exit_code_hint
  |> should.equal("Command not found (CLI may have moved)")
}

pub fn diagnose_exit_code_unknown_empty_stdout_test() {
  let diagnostic = diagnose_exit_code(42, True)
  diagnostic.exit_code_hint
  |> should.equal("CLI error (no output)")
}

pub fn diagnose_exit_code_unknown_with_output_test() {
  let diagnostic = diagnose_exit_code(42, False)
  diagnostic.exit_code_hint
  |> should.equal("Unknown error")
}

// ============================================================================
// ErrorDiagnostic Construction Tests
// ============================================================================

pub fn error_diagnostic_with_last_line_test() {
  let diagnostic =
    ErrorDiagnostic(
      last_non_json_line: Some("Error: something went wrong"),
      stdout_was_empty: False,
      exit_code_hint: "test hint",
      troubleshooting: "test guidance",
    )
  diagnostic.last_non_json_line
  |> should.equal(Some("Error: something went wrong"))
}

// ============================================================================
// error_to_string() Tests (QueryError)
// ============================================================================

pub fn error_to_string_cli_not_found_test() {
  CliNotFoundError("claude not in PATH")
  |> error_to_string
  |> should.equal("CLI not found: claude not in PATH")
}

pub fn error_to_string_unsupported_version_test() {
  UnsupportedCliVersionError("0.1.0", "1.0.0", "Please upgrade")
  |> error_to_string
  |> should.equal(
    "Unsupported CLI version: 0.1.0 (requires 1.0.0). Please upgrade",
  )
}

pub fn error_to_string_unknown_version_test() {
  UnknownVersionError("weird-output", "Check format")
  |> error_to_string
  |> should.equal("Unknown CLI version format: weird-output. Check format")
}

pub fn error_to_string_version_detection_test() {
  VersionDetectionError("timeout after 5s")
  |> error_to_string
  |> should.equal("Version detection failed: timeout after 5s")
}

pub fn error_to_string_spawn_error_test() {
  SpawnError("EMFILE too many open files")
  |> error_to_string
  |> should.equal("Failed to spawn process: EMFILE too many open files")
}

pub fn error_to_string_test_mode_error_test() {
  TestModeError("test_runner not provided")
  |> error_to_string
  |> should.equal("Test mode error: test_runner not provided")
}

// ============================================================================
// stream_error_to_string() Tests (StreamError)
// ============================================================================

pub fn stream_error_to_string_process_error_test() {
  let diagnostic =
    ErrorDiagnostic(
      last_non_json_line: None,
      stdout_was_empty: True,
      exit_code_hint: "Authentication required",
      troubleshooting: "Run 'claude auth'",
    )
  ProcessError(1, diagnostic)
  |> stream_error_to_string
  |> should.equal("Process exited with code 1: Authentication required")
}

pub fn stream_error_to_string_buffer_overflow_test() {
  BufferOverflow
  |> stream_error_to_string
  |> should.equal("Buffer overflow: single line exceeded 10MB limit")
}

pub fn stream_error_to_string_too_many_decode_errors_test() {
  TooManyDecodeErrors(5, "unexpected EOF")
  |> stream_error_to_string
  |> should.equal("Too many decode errors (5): unexpected EOF")
}

pub fn stream_error_to_string_json_decode_error_test() {
  JsonDecodeError("{bad json", "expected closing brace")
  |> stream_error_to_string
  |> should.equal(
    "JSON decode error: expected closing brace in line: {bad json",
  )
}

pub fn stream_error_to_string_unexpected_message_test() {
  UnexpectedMessageError("{\"type\":\"alien\"}")
  |> stream_error_to_string
  |> should.equal("Unexpected message type in: {\"type\":\"alien\"}")
}
