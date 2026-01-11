/// Error types for Claude Agent SDK.
///
/// This module defines all error, warning, and stream item types used for SDK error handling.
/// Errors are categorized into:
/// - QueryError: Startup errors that prevent query from starting
/// - StreamError: Runtime errors during stream iteration
/// - Warning: Non-fatal warnings that can be yielded from the stream
import gleam/int
import gleam/option.{type Option, None}

import claude_agent_sdk/message.{type MessageEnvelope}

// ============================================================================
// StreamError Type (Runtime Errors)
// ============================================================================

/// Errors during stream iteration (does NOT include normal end-of-stream)
pub type StreamError {
  // --- Terminal errors (port closed, stream done) ---
  /// CLI process exited with non-zero code
  ProcessError(exit_code: Int, diagnostic: ErrorDiagnostic)
  /// Single line exceeded 10MB buffer limit
  BufferOverflow
  /// Too many consecutive JSON decode failures (default: 5)
  TooManyDecodeErrors(count: Int, last_error: String)

  // --- Non-terminal errors (stream continues) ---
  /// Line was not valid JSON (continues until TooManyDecodeErrors threshold)
  JsonDecodeError(line: String, error: String)
  /// Valid JSON but unknown message type
  UnexpectedMessageError(raw_json: String)
}

/// Diagnostic context for ProcessError (since stderr is not captured in v1)
pub type ErrorDiagnostic {
  ErrorDiagnostic(
    /// Last non-JSON line seen on stdout (if any) — may contain error hints
    last_non_json_line: Option(String),
    /// True if stdout was completely empty (common for auth failures)
    stdout_was_empty: Bool,
    /// Common exit code interpretations
    exit_code_hint: String,
    /// Guidance for users
    troubleshooting: String,
  )
}

/// Check if an error is terminal (stream closed, no more items)
pub fn is_terminal(error: StreamError) -> Bool {
  case error {
    ProcessError(_, _) | BufferOverflow | TooManyDecodeErrors(_, _) -> True
    JsonDecodeError(_, _) | UnexpectedMessageError(_) -> False
  }
}

/// Generate diagnostic context based on exit code and stdout state.
///
/// **Empty stdout handling (P1 concern)**:
/// Many CLI failures (auth, network, config) emit ONLY stderr and exit non-zero,
/// producing no stdout lines. This is a common first-run failure mode.
/// When stdout is empty, we provide enhanced guidance.
pub fn diagnose_exit_code(
  exit_code: Int,
  stdout_was_empty: Bool,
) -> ErrorDiagnostic {
  let #(hint, troubleshooting) = case exit_code, stdout_was_empty {
    // First-run auth failure: exit 1, no stdout (stderr has auth error)
    1, True -> #(
      "Authentication required",
      "Run 'claude login' to authenticate. The CLI produced no output, which typically means authentication failed. If running non-interactively (CI, daemon, container), ensure ANTHROPIC_API_KEY is set or redirect stderr for diagnostics: your_app 2>/tmp/stderr.log",
    )
    // Exit 1 with some output: more specific error in stdout
    1, False -> #(
      "General error (check if CLI is authenticated)",
      "Run 'claude --version' to verify CLI. Stderr output (if any) appears in parent terminal.",
    )
    // Invalid args: usually has stdout explaining which arg
    2, _ -> #(
      "Invalid arguments (check SDK version compatibility)",
      "The CLI rejected command arguments. Check that your SDK version matches your CLI version. Run 'claude --help' to verify supported flags.",
    )
    126, _ -> #(
      "Permission denied (check file permissions)",
      "Check that the claude binary is executable and accessible.",
    )
    127, _ -> #(
      "Command not found (CLI may have moved)",
      "The CLI was not found. Reinstall with: npm install -g @anthropic-ai/claude-code",
    )
    // Unknown error with empty stdout: likely stderr-only
    _, True -> #(
      "CLI error (no output)",
      "The CLI exited with an error but produced no stdout. Stderr may contain details. For non-interactive environments, redirect stderr: your_app 2>/tmp/stderr.log",
    )
    _, False -> #(
      "Unknown error",
      "Run 'claude --version' to verify CLI. Stderr output (if any) appears in parent terminal.",
    )
  }
  ErrorDiagnostic(
    last_non_json_line: None,
    stdout_was_empty: stdout_was_empty,
    exit_code_hint: hint,
    troubleshooting: troubleshooting,
  )
}

// ============================================================================
// QueryError Type (Startup Errors)
// ============================================================================

/// Errors that prevent query from starting
pub type QueryError {
  /// Claude CLI not found in PATH
  CliNotFoundError(message: String)
  /// CLI version too old or incompatible
  UnsupportedCliVersionError(
    detected_version: String,
    minimum_required: String,
    suggestion: String,
  )
  /// CLI version could not be parsed (fail-fast default behavior)
  UnknownVersionError(raw_output: String, suggestion: String)
  /// Version detection failed (timeout, spawn error, etc.)
  VersionDetectionError(reason: String)
  /// Failed to spawn process
  SpawnError(reason: String)
}

// ============================================================================
// Warning Type
// ============================================================================

/// Non-fatal warning that can be yielded from the stream
pub type Warning {
  Warning(code: WarningCode, message: String, context: Option(String))
}

pub type WarningCode {
  /// CLI version string didn't match expected format
  UnparseableCliVersion
  /// CLI exited 0 but no Result message received
  CleanExitNoResult
  /// Non-zero exit code after Result message (includes exit code)
  NonZeroExitAfterResult(Int)
  /// Data received after Result message
  UnexpectedMessageAfterResult
  /// Reserved for future use
  DeprecatedOption
}

// ============================================================================
// StreamItem Type (Unified Stream Contract)
// ============================================================================

/// What the stream can yield on success.
/// Note: MessageEnvelope will be defined in messages.gleam; using opaque placeholder for now.
pub type StreamItem {
  /// A parsed message from the CLI
  Message(MessageEnvelope)
  /// A non-fatal warning (stream continues)
  WarningEvent(Warning)
  /// Normal end of stream (terminal - no more items)
  EndOfStream
}

// ============================================================================
// Error to String Functions
// ============================================================================

/// Convert a QueryError to a human-readable string
pub fn error_to_string(error: QueryError) -> String {
  case error {
    CliNotFoundError(message) -> "CLI not found: " <> message
    UnsupportedCliVersionError(detected, required, suggestion) ->
      "Unsupported CLI version: "
      <> detected
      <> " (requires "
      <> required
      <> "). "
      <> suggestion
    UnknownVersionError(raw_output, suggestion) ->
      "Unknown CLI version format: " <> raw_output <> ". " <> suggestion
    VersionDetectionError(reason) -> "Version detection failed: " <> reason
    SpawnError(reason) -> "Failed to spawn process: " <> reason
  }
}

/// Convert a StreamError to a human-readable string
pub fn stream_error_to_string(error: StreamError) -> String {
  case error {
    ProcessError(exit_code, diagnostic) ->
      "Process exited with code "
      <> int.to_string(exit_code)
      <> ": "
      <> diagnostic.exit_code_hint
    BufferOverflow -> "Buffer overflow: single line exceeded 10MB limit"
    TooManyDecodeErrors(count, last_error) ->
      "Too many decode errors (" <> int.to_string(count) <> "): " <> last_error
    JsonDecodeError(line, err) ->
      "JSON decode error: " <> err <> " in line: " <> line
    UnexpectedMessageError(raw_json) ->
      "Unexpected message type in: " <> raw_json
  }
}
