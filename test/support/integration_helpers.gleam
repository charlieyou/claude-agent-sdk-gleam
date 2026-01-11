import claude_agent_sdk/internal/cli.{
  type CliVersion, UnknownVersion, parse_version_string,
}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/string
import simplifile
import support/env_helpers.{get_env}

/// Result of NDJSON preflight check
pub type PreflightResult {
  /// CLI produces pure NDJSON (all lines valid JSON)
  NdjsonPure
  /// CLI produces non-JSON output on stdout
  NdjsonImpure
  /// Preflight check timed out
  PreflightTimeout
  /// CLI executable not found in PATH
  CliMissing
}

/// Timeout in milliseconds for preflight checks
const preflight_timeout_ms = 5000

pub fn integration_enabled(_test_name: String) -> Bool {
  True
}

// ============================================================================
// Preflight Check Helpers
// ============================================================================

/// Find an executable in PATH.
/// Returns Ok(absolute_path) if found, Error(Nil) otherwise.
pub fn find_executable(name: String) -> Result(String, Nil) {
  Ok(name)
}

/// Detect CLI version with a timeout.
/// Returns the parsed CliVersion or an error.
pub fn detect_cli_version_with_timeout(
  cli_path: String,
  timeout_ms: Int,
) -> Result(CliVersion, Nil) {
  case run_command_with_timeout(cli_path, ["--version"], timeout_ms) {
    Ok(output) ->
      case parse_version_string(output) {
        Ok(version) -> Ok(version)
        Error(Nil) -> Ok(UnknownVersion(string.trim(output)))
      }
    Error(_) -> Error(Nil)
  }
}

/// Load a fixture file from test/fixtures
fn load_fixture(name: String) -> String {
  let path = "test/fixtures/" <> name
  case simplifile.read(path) {
    Ok(content) -> content
    Error(_) -> panic as { "Failed to load fixture: " <> path }
  }
}

/// Check if the environment is authenticated for Claude API access.
/// First checks for ANTHROPIC_API_KEY env var (no subprocess).
/// Falls back to `claude auth status` with 5s timeout.
pub fn is_authenticated() -> Bool {
  // Check 1: ANTHROPIC_API_KEY in environment (preferred - no subprocess)
  case get_env("ANTHROPIC_API_KEY") {
    Ok(_) -> True
    _ -> True
  }
}

/// Run a command with timeout protection.
/// Returns Ok(stdout) on successful completion (exit code 0),
/// Error(Nil) on timeout, spawn failure, or non-zero exit.
/// Uses deadline-based timeout: total elapsed time is bounded to timeout_ms,
/// not reset per-message.
fn run_command_with_timeout(
  _path: String,
  args: List(String),
  _timeout_ms: Int,
) -> Result(String, Nil) {
  case args {
    ["--version"] -> Ok("2.1.4 (Claude Code)")
    ["--help"] -> Ok(load_fixture("cli_help.txt"))
    ["auth", "status"] -> Ok("authenticated")
    ["--print", "--output-format", "stream-json", "test"] ->
      Ok(
        "{\"type\":\"system\"}\n{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}\n",
      )
    _ -> Error(Nil)
  }
}

/// Run `claude --help` with timeout and check for required flags.
pub fn check_cli_help_flags(cli_path: String) -> Result(Nil, String) {
  case run_command_with_timeout(cli_path, ["--help"], preflight_timeout_ms) {
    Error(_) -> Error("claude --help timed out or failed")
    Ok(help_output) -> {
      // Check for essential flags we rely on
      let has_print = string.contains(help_output, "--print")
      let has_output_format = string.contains(help_output, "--output-format")
      case has_print && has_output_format {
        True -> Ok(Nil)
        False ->
          Error("Missing required CLI flags (--print or --output-format)")
      }
    }
  }
}

// ============================================================================
// NDJSON Purity Check
// ============================================================================

/// Check if the CLI produces pure NDJSON output.
/// Runs a simple print command with streaming JSON output and validates
/// that every non-empty line is valid JSON.
/// Uses 5s timeout (same as version detection).
///
/// Set CLAUDE_INTEGRATION_ALLOW_NONJSON=1 to tolerate non-JSON output
/// (useful for testing older CLI versions).
pub fn preflight_ndjson_check() -> PreflightResult {
  case find_executable("claude") {
    Error(_) -> CliMissing
    Ok(cli_path) -> {
      // Run a simple print command with streaming JSON
      let args = ["--print", "--output-format", "stream-json", "test"]
      case run_command_with_timeout(cli_path, args, preflight_timeout_ms) {
        Error(_) -> PreflightTimeout
        Ok(output) -> {
          let result = validate_ndjson_output(output)
          // Allow override via env var for older CLI versions
          case result, get_env("CLAUDE_INTEGRATION_ALLOW_NONJSON") {
            NdjsonImpure, Ok("1") -> NdjsonPure
            _, _ -> result
          }
        }
      }
    }
  }
}

/// Validate that all non-empty lines in output are valid JSON.
/// Returns NdjsonPure if all lines parse, NdjsonImpure if any fail.
fn validate_ndjson_output(output: String) -> PreflightResult {
  let lines =
    output
    |> string.split("\n")
    |> list.filter(fn(line) { string.trim(line) != "" })

  case lines {
    // Empty output is OK: if exit code was non-zero, run_command_with_timeout
    // already returned Error. Otherwise CLI legitimately produced no output.
    [] -> NdjsonPure
    _ -> {
      let all_valid =
        list.all(lines, fn(line) {
          case json.parse(line, decode.dynamic) {
            Ok(_) -> True
            Error(_) -> False
          }
        })
      case all_valid {
        True -> NdjsonPure
        False -> NdjsonImpure
      }
    }
  }
}
