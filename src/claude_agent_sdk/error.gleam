/// Error types for Claude Agent SDK.
///
/// This module defines all error, warning, and stream item types for error handling.
/// Import this module when pattern matching on error variants returned by SDK functions.
///
/// ## Error Categories
///
/// - **QueryError**: Startup errors that prevent `query()` from returning a stream
///   (CLI not found, version incompatible, spawn failed)
/// - **StreamError**: Runtime errors during stream iteration via `next()`
///   (process exited, decode errors, buffer overflow)
/// - **Warning**: Non-fatal conditions that can be yielded from the stream
///   (unparseable version, incomplete line)
///
/// ## StreamItem
///
/// `StreamItem` is the success type returned by `stream.next()`:
/// - `Message(envelope)`: A parsed message from the CLI
/// - `WarningEvent(warning)`: A non-fatal warning
/// - `EndOfStream`: Normal completion marker
///
/// ## Example
///
/// ```gleam
/// import claude_agent_sdk/error
///
/// case stream.next(stream) {
///   #(Ok(error.Message(envelope)), stream) -> // handle message
///   #(Ok(error.EndOfStream), stream) -> // done
///   #(Error(err), stream) ->
///     case error.is_terminal(err) {
///       True -> // stop iteration
///       False -> // continue, maybe log
///     }
/// }
/// ```
import gleam/int
import gleam/option.{type Option, None}

import claude_agent_sdk/message.{type MessageEnvelope}

// ============================================================================
// StreamError Type (Runtime Errors)
// ============================================================================

/// Errors during stream iteration.
///
/// StreamError represents problems encountered during `next()` calls. Errors
/// are classified as either **terminal** (stream is done) or **non-terminal**
/// (stream continues, call `next()` again).
///
/// Use `is_terminal()` to check if iteration should stop.
pub type StreamError {
  // --- Terminal errors (port closed, stream done) ---
  /// CLI process exited with non-zero code.
  /// The `diagnostic` field provides troubleshooting guidance.
  ProcessError(exit_code: Int, diagnostic: ErrorDiagnostic)
  /// Single line exceeded 10MB buffer limit.
  /// This is a safety limit to prevent memory exhaustion.
  BufferOverflow
  /// Too many consecutive JSON decode failures (default threshold: 5).
  /// Indicates the CLI is producing malformed output.
  TooManyDecodeErrors(count: Int, last_error: String)

  // --- Non-terminal errors (stream continues) ---
  /// Line was not valid JSON.
  /// Non-terminal: continues until `TooManyDecodeErrors` threshold is reached.
  JsonDecodeError(line: String, error: String)
  /// Valid JSON but unrecognized message type.
  /// Non-terminal: the message is logged but iteration continues.
  UnexpectedMessageError(raw_json: String)
}

/// Diagnostic context for ProcessError.
///
/// Since the SDK captures only stdout (not stderr), this type provides
/// contextual hints to help diagnose CLI failures. The `diagnose_exit_code()`
/// function populates these fields based on common failure patterns.
pub type ErrorDiagnostic {
  ErrorDiagnostic(
    /// Last non-JSON line seen on stdout (if any).
    /// May contain error hints or partial output before the crash.
    last_non_json_line: Option(String),
    /// True if stdout was completely empty.
    /// Common for auth failures where CLI writes only to stderr.
    stdout_was_empty: Bool,
    /// Human-readable interpretation of the exit code.
    /// Examples: "Authentication required", "Invalid arguments"
    exit_code_hint: String,
    /// Actionable troubleshooting guidance for the user.
    troubleshooting: String,
  )
}

/// Check if an error is terminal (stream closed, no more items).
///
/// Terminal errors mean the stream is done and `next()` should not be called again.
/// Non-terminal errors allow iteration to continue.
///
/// ## Returns
///
/// - `True` for: `ProcessError`, `BufferOverflow`, `TooManyDecodeErrors`
/// - `False` for: `JsonDecodeError`, `UnexpectedMessageError`
///
/// ## Example
///
/// ```gleam
/// case stream.next(stream) {
///   #(Error(err), stream) ->
///     case error.is_terminal(err) {
///       True -> stream.close(stream)  // Done
///       False -> iterate(stream)       // Continue
///     }
///   // ...
/// }
/// ```
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
      "Authenticate the Claude CLI. The CLI produced no output, which typically means authentication failed. If running non-interactively (CI, daemon, container), ensure the CLI is authenticated or redirect stderr for diagnostics: your_app 2>/tmp/stderr.log",
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

/// Errors that prevent query from starting.
///
/// QueryError is returned by `query()` when the SDK cannot start a CLI session.
/// These errors occur before streaming begins, so no stream cleanup is needed.
pub type QueryError {
  /// Claude CLI not found in PATH.
  /// Install with: `npm install -g @anthropic-ai/claude-code`
  CliNotFoundError(message: String)
  /// CLI version too old or incompatible.
  /// The `suggestion` field contains upgrade instructions.
  UnsupportedCliVersionError(
    detected_version: String,
    minimum_required: String,
    suggestion: String,
  )
  /// CLI version could not be parsed.
  /// Default behavior is fail-fast; use `with_permissive_version_check()`
  /// to allow unknown versions with a warning.
  UnknownVersionError(raw_output: String, suggestion: String)
  /// Version detection failed (timeout, spawn error, etc.).
  VersionDetectionError(reason: String)
  /// Failed to spawn the CLI process.
  /// May indicate permission issues, missing dependencies, or resource limits.
  SpawnError(reason: String)
  /// Test mode configuration error.
  /// Occurs when test_mode is enabled but test_runner is not provided.
  TestModeError(reason: String)
}

// ============================================================================
// Warning Type
// ============================================================================

/// Non-fatal warning that can be yielded from the stream.
///
/// Warnings indicate conditions that don't prevent stream iteration from
/// continuing but may require attention. They are yielded as `WarningEvent`
/// items from `next()`.
///
/// ## Fields
///
/// - `code`: Machine-readable warning category for programmatic handling
/// - `message`: Human-readable description of the warning
/// - `context`: Optional additional context (e.g., the malformed line)
pub type Warning {
  Warning(code: WarningCode, message: String, context: Option(String))
}

/// Warning codes for categorizing warnings.
///
/// Use these codes for programmatic warning handling rather than parsing
/// message strings. Each code represents a specific condition that occurred
/// during stream processing.
pub type WarningCode {
  /// CLI version string didn't match expected format.
  /// Occurs when `permissive_version_check` is enabled.
  UnparseableCliVersion
  /// CLI exited with status 0 but no Result message was received.
  /// The query may have been interrupted or the CLI output was truncated.
  CleanExitNoResult
  /// Non-zero exit code received after Result message.
  /// The Result was successfully parsed, but the CLI reported an error on exit.
  NonZeroExitAfterResult(Int)
  /// Data received after Result message (unexpected trailing output).
  UnexpectedMessageAfterResult
  /// Incomplete line in buffer when exit_status=0 received.
  /// The partial line is discarded; content is included in the warning.
  IncompleteLastLine(String)
  /// Reserved for future deprecation warnings.
  DeprecatedOption
  /// Bidirectional option was ignored by query().
  /// Hooks and can_use_tool require start_session() for bidirectional mode.
  BidirOptionIgnored
}

// ============================================================================
// StreamItem Type (Unified Stream Contract)
// ============================================================================

/// What the stream can yield on success.
///
/// StreamItem is the success type in the Result returned by `stream.next()`.
/// It represents the three possible outcomes when reading from the stream succeeds.
pub type StreamItem {
  /// A parsed message from the CLI.
  /// The envelope contains both the parsed `Message` and raw JSON for debugging.
  Message(MessageEnvelope)
  /// A non-fatal warning (stream continues).
  /// Call `next()` again to get the next item.
  WarningEvent(Warning)
  /// Normal end of stream (terminal).
  /// The CLI has finished and the stream is complete. Do not call `next()` again.
  EndOfStream
}

// ============================================================================
// Error to String Functions
// ============================================================================

/// Convert a QueryError to a human-readable string.
///
/// Useful for logging or displaying errors to users.
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
    TestModeError(reason) -> "Test mode error: " <> reason
  }
}

// ============================================================================
// StartError Type (Session Startup Errors)
// ============================================================================

/// Errors that can occur when starting a bidirectional session.
///
/// StartError is returned by `start_session()` when the SDK cannot establish
/// a bidirectional CLI session. These errors occur during session initialization.
pub type StartError {
  /// Initialization timed out waiting for CLI response.
  /// The CLI process may be slow to start or unresponsive.
  Timeout
  /// Failed to spawn the CLI process.
  /// May indicate permission issues, missing dependencies, or resource limits.
  SpawnFailed(reason: String)
  /// Placeholder for TDD: start_session is not yet implemented.
  /// This variant will be removed once the actual implementation is complete.
  NotImplemented
}

/// Convert a StartError to a human-readable string.
///
/// Useful for logging or displaying errors to users.
pub fn start_error_to_string(error: StartError) -> String {
  case error {
    Timeout -> "Session initialization timed out"
    SpawnFailed(reason) -> "Failed to spawn CLI process: " <> reason
    NotImplemented -> "start_session is not yet implemented"
  }
}

/// Convert a StreamError to a human-readable string.
///
/// Useful for logging or displaying errors to users.
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

// ============================================================================
// ControlError Type (Control Operation Errors)
// ============================================================================

/// Errors that can occur during control operations.
///
/// ControlError is returned by control operations like `interrupt()`,
/// `set_permission_mode()`, `set_model()`, and `rewind_files()`.
pub type ControlError {
  /// Operation timed out waiting for CLI acknowledgment.
  ControlTimeout
  /// Session is closed and cannot accept control operations.
  ControlSessionClosed
  /// Placeholder for TDD: control operation is not yet implemented.
  /// This variant will be removed once the actual implementation is complete.
  ControlNotImplemented
}

/// Convert a ControlError to a human-readable string.
///
/// Useful for logging or displaying errors to users.
pub fn control_error_to_string(error: ControlError) -> String {
  case error {
    ControlTimeout -> "Control operation timed out"
    ControlSessionClosed -> "Session is closed"
    ControlNotImplemented -> "Control operation is not yet implemented"
  }
}

// ============================================================================
// StopError Type (Stop Operation Errors)
// ============================================================================

/// Errors that can occur when stopping a session.
///
/// StopError is returned by `stop()` when the SDK cannot initiate
/// a graceful session shutdown.
pub type StopError {
  /// Session is already closed.
  StopSessionClosed
  /// Placeholder for TDD: stop is not yet implemented.
  /// This variant will be removed once the actual implementation is complete.
  StopNotImplemented
}

/// Convert a StopError to a human-readable string.
///
/// Useful for logging or displaying errors to users.
pub fn stop_error_to_string(error: StopError) -> String {
  case error {
    StopSessionClosed -> "Session is already closed"
    StopNotImplemented -> "stop is not yet implemented"
  }
}
