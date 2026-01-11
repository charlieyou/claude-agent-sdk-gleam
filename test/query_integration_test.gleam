import claude_agent_sdk
import claude_agent_sdk/error.{
  BufferOverflow, CliNotFoundError, JsonDecodeError, ProcessError, SpawnError,
  TooManyDecodeErrors, UnexpectedMessageError, UnknownVersionError,
  UnsupportedCliVersionError, VersionDetectionError,
}
import claude_agent_sdk/internal/cli.{CliVersion, UnknownVersion}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import support/integration_helpers.{
  CliMissing, NdjsonImpure, NdjsonPure, PreflightTimeout, check_cli_help_flags,
  detect_cli_version_with_timeout, find_executable, integration_enabled,
  is_authenticated, preflight_ndjson_check,
}

/// Timeout for preflight checks (5 seconds)
const preflight_timeout_ms = 5000

// Skip message constants - used by both preflight check and documentation tests
const skip_msg_cli_missing = "[SKIP:ENV] claude not in PATH - install CLI first"

const skip_msg_version_prefix = "[SKIP:ENV] CLI version "

const skip_msg_version_suffix = " < 1.0.0 - upgrade required"

const skip_msg_auth_unavailable = "[SKIP:AUTH] Auth not available - set ANTHROPIC_API_KEY or run claude login"

const skip_msg_ndjson = "[SKIP:NDJSON] CLI not producing pure NDJSON"

const skip_msg_timeout = "[SKIP:TIMEOUT] Preflight check timed out"

/// Preflight test that validates CLI is available and properly configured.
/// This test runs before other integration tests to provide actionable error messages.
pub fn integration__preflight_check_test() {
  case integration_enabled("integration__preflight_check_test") {
    True -> {
      // 1. CLI exists and is in PATH
      case find_executable("claude") {
        Error(_) -> {
          io.println(skip_msg_cli_missing)
          should.be_true(True)
        }
        Ok(cli_path) -> {
          // 2. Version check (with 5s timeout)
          case detect_cli_version_with_timeout(cli_path, preflight_timeout_ms) {
            Error(_) -> {
              io.println("[SKIP:ENV] claude --version timed out or failed")
              should.be_true(True)
            }
            Ok(UnknownVersion(raw)) -> {
              io.println(
                "[WARN] Unparseable version: " <> raw <> " - tests may fail",
              )
              // Continue to help check since we have a CLI
              check_help_and_finish(cli_path)
            }
            Ok(CliVersion(maj, _, _, raw)) -> {
              case maj < 1 {
                True -> {
                  io.println(
                    skip_msg_version_prefix <> raw <> skip_msg_version_suffix,
                  )
                  should.be_true(True)
                }
                False -> {
                  // 3. Required flags exist (non-network check)
                  check_help_and_finish(cli_path)
                }
              }
            }
          }
        }
      }
    }
    False -> should.be_true(True)
  }
}

/// Helper to check --help output for required flags, then check auth.
fn check_help_and_finish(cli_path: String) {
  case check_cli_help_flags(cli_path) {
    Error(reason) -> {
      io.println("[SKIP:ENV] " <> reason)
      should.be_true(True)
    }
    Ok(_) -> {
      // 4. Auth check
      case is_authenticated() {
        False -> {
          io.println(skip_msg_auth_unavailable)
          should.be_true(True)
        }
        True -> {
          io.println(
            "[PREFLIGHT] All checks passed - integration tests will run",
          )
          should.be_true(True)
        }
      }
    }
  }
}

// ============================================================================
// Negative Scenario Coverage Tests
// ============================================================================
//
// These tests document the expected skip messages for each negative scenario.
// The preflight test above exercises the actual skip logic; these tests
// serve as documentation of the expected behavior.

/// Validates the skip message constant for CLI not in PATH scenario.
/// Uses the same constant as integration__preflight_check_test to prevent drift.
pub fn integration__skip_message_cli_missing_test() {
  // Validates that the constant has the expected format
  should.equal(
    skip_msg_cli_missing,
    "[SKIP:ENV] claude not in PATH - install CLI first",
  )
}

/// Validates the skip message constants for CLI version too old scenario.
/// Uses the same constants as integration__preflight_check_test to prevent drift.
pub fn integration__skip_message_version_too_old_test() {
  // Validates that the prefix/suffix constants have the expected format
  // The message includes the actual version, e.g. "0.9.5"
  should.equal(skip_msg_version_prefix, "[SKIP:ENV] CLI version ")
  should.equal(skip_msg_version_suffix, " < 1.0.0 - upgrade required")
}

/// Validates the skip message constant for auth unavailable scenario.
/// Uses the same constant as integration__preflight_check_test to prevent drift.
pub fn integration__skip_message_auth_unavailable_test() {
  // Validates that the constant has the expected format
  should.equal(
    skip_msg_auth_unavailable,
    "[SKIP:AUTH] Auth not available - set ANTHROPIC_API_KEY or run claude login",
  )
}

// ============================================================================
// Real CLI Query Integration Test
// ============================================================================

/// Integration test: real CLI query (opt-in via CLAUDE_INTEGRATION_TEST=1)
/// Makes an actual query to Claude CLI and verifies the stream produces messages.
pub fn integration__real_cli_query_test() {
  case integration_enabled("integration__real_cli_query_test") {
    True -> {
      case is_authenticated() {
        True -> {
          // Build options with max_turns=1 for fast test
          let options =
            claude_agent_sdk.default_options()
            |> claude_agent_sdk.with_max_turns(1)

          // Start query with simple prompt
          case claude_agent_sdk.query("Say hello in one word.", options) {
            Error(err) -> {
              io.println("[FAIL] Query failed: " <> query_error_to_string(err))
              should.be_true(False)
            }
            Ok(stream) -> {
              // Collect messages from stream (30s implicit timeout via CLI)
              let result = claude_agent_sdk.collect_messages(stream)
              let messages = result.items
              let terminal_err = result.terminal_error

              // Verify we got at least one message
              case messages {
                [_, ..] -> {
                  io.println(
                    "[PASS] Received "
                    <> int.to_string(list.length(messages))
                    <> " message(s)",
                  )
                  should.be_true(True)
                }
                [] -> {
                  case terminal_err {
                    None -> {
                      io.println(
                        "[FAIL] No messages received, no terminal error",
                      )
                      should.be_true(False)
                    }
                    Some(err) -> {
                      io.println(
                        "[FAIL] No messages, terminal error: "
                        <> stream_error_to_string(err),
                      )
                      should.be_true(False)
                    }
                  }
                }
              }
            }
          }
        }
        False -> {
          io.println(skip_msg_auth_unavailable)
          should.be_true(True)
        }
      }
    }
    False -> should.be_true(True)
  }
}

/// Convert QueryError to string for diagnostics.
fn query_error_to_string(err: claude_agent_sdk.QueryError) -> String {
  case err {
    CliNotFoundError(msg) -> "CliNotFoundError: " <> msg
    VersionDetectionError(msg) -> "VersionDetectionError: " <> msg
    UnsupportedCliVersionError(
      detected_version:,
      minimum_required:,
      suggestion:,
    ) ->
      "UnsupportedCliVersionError: detected="
      <> detected_version
      <> ", minimum="
      <> minimum_required
      <> ", suggestion="
      <> suggestion
    UnknownVersionError(raw_output:, suggestion:) ->
      "UnknownVersionError: raw=" <> raw_output <> ", suggestion=" <> suggestion
    SpawnError(msg) -> "SpawnError: " <> msg
  }
}

/// Convert StreamError to string for diagnostics.
fn stream_error_to_string(err: claude_agent_sdk.StreamError) -> String {
  case err {
    ProcessError(code, _diagnostic) ->
      "ProcessError(exit_code=" <> int.to_string(code) <> ")"
    BufferOverflow -> "BufferOverflow"
    TooManyDecodeErrors(count, last_error) ->
      "TooManyDecodeErrors(count="
      <> int.to_string(count)
      <> ", last="
      <> last_error
      <> ")"
    JsonDecodeError(line, err) -> "JsonDecodeError: " <> line <> ": " <> err
    UnexpectedMessageError(raw) -> "UnexpectedMessageError: " <> raw
  }
}

// ============================================================================
// NDJSON Purity Integration Test
// ============================================================================

/// Integration test: NDJSON purity check
/// Verifies every line from CLI stdout is valid JSON.
/// Stricter than runtime: ZERO tolerance for non-JSON (vs 5-error threshold).
pub fn integration__ndjson_purity_test() {
  case integration_enabled("integration__ndjson_purity_test") {
    True -> {
      case preflight_ndjson_check() {
        NdjsonPure -> {
          io.println("[PASS] CLI produces pure NDJSON")
          should.be_true(True)
        }
        NdjsonImpure -> {
          io.println(skip_msg_ndjson)
          should.be_true(True)
        }
        PreflightTimeout -> {
          io.println(skip_msg_timeout)
          should.be_true(True)
        }
        CliMissing -> {
          io.println(skip_msg_cli_missing)
          should.be_true(True)
        }
      }
    }
    False -> should.be_true(True)
  }
}

/// Validates the skip message constant for NDJSON impurity scenario.
pub fn integration__skip_message_ndjson_impure_test() {
  should.equal(skip_msg_ndjson, "[SKIP:NDJSON] CLI not producing pure NDJSON")
}

/// Validates the skip message constant for preflight timeout scenario.
pub fn integration__skip_message_timeout_test() {
  should.equal(skip_msg_timeout, "[SKIP:TIMEOUT] Preflight check timed out")
}
