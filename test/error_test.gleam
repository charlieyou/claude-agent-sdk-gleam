/// Tests for error module - specifically is_terminal() behavior.
import claude_agent_sdk/error.{
  BufferOverflow, ErrorDiagnostic, JsonDecodeError, ProcessError,
  TooManyDecodeErrors, UnexpectedMessageError, diagnose_exit_code, is_terminal,
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
