import claude_agent_sdk/internal/cli.{
  type CliVersion, UnknownVersion, parse_version_string,
}
import claude_agent_sdk/internal/port_ffi.{Data, Eof, ExitStatus, Timeout}
import gleam/bit_array
import gleam/dynamic/decode
import gleam/io
import gleam/json
import gleam/list
import gleam/string
import support/env_helpers.{get_env}

/// Result of NDJSON preflight check
pub type PreflightResult {
  /// CLI produces pure NDJSON (all lines valid JSON)
  NdjsonPure
  /// CLI produces non-JSON output on stdout
  NdjsonImpure
  /// Preflight check timed out
  PreflightTimeout
}

/// Timeout in milliseconds for preflight checks
const preflight_timeout_ms = 5000

/// Check if integration tests should run for the given test name.
/// Returns True when CLAUDE_INTEGRATION_TEST=1, otherwise prints skip message and returns False.
pub fn integration_enabled(test_name: String) -> Bool {
  case get_env("CLAUDE_INTEGRATION_TEST") {
    Ok("1") -> True
    _ -> {
      io.println(
        "[SKIP] " <> test_name <> " (set CLAUDE_INTEGRATION_TEST=1 to run)",
      )
      False
    }
  }
}

// ============================================================================
// Preflight Check Helpers
// ============================================================================

/// Find an executable in PATH.
/// Returns Ok(absolute_path) if found, Error(Nil) otherwise.
pub fn find_executable(name: String) -> Result(String, Nil) {
  case port_ffi.find_cli_path(name) {
    Ok(path) -> Ok(path)
    Error(_) -> Error(Nil)
  }
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

/// Check if the environment is authenticated for Claude API access.
/// First checks for ANTHROPIC_API_KEY env var (no subprocess).
/// Falls back to `claude auth status` with 5s timeout.
pub fn is_authenticated() -> Bool {
  // Check 1: ANTHROPIC_API_KEY in environment (preferred - no subprocess)
  case get_env("ANTHROPIC_API_KEY") {
    Ok(_) -> True
    _ ->
      // Check 2: `claude auth status` with 5s timeout (if CLI supports it)
      case find_executable("claude") {
        Error(_) -> False
        Ok(cli_path) ->
          case run_command_with_timeout(cli_path, ["auth", "status"], 5000) {
            Ok(output) -> string.contains(output, "authenticated")
            Error(_) -> False
          }
      }
  }
}

/// Run a command with timeout protection.
/// Returns Ok(stdout) on successful completion (exit code 0),
/// Error(Nil) on timeout, spawn failure, or non-zero exit.
/// Uses deadline-based timeout: total elapsed time is bounded to timeout_ms,
/// not reset per-message.
fn run_command_with_timeout(
  path: String,
  args: List(String),
  timeout_ms: Int,
) -> Result(String, Nil) {
  case port_ffi.ffi_open_port_safe(path, args, ".") {
    Error(_) -> Error(Nil)
    Ok(port) -> {
      let deadline_ms = port_ffi.monotonic_time_ms() + timeout_ms
      let result = collect_output_with_deadline(port, <<>>, deadline_ms)
      port_ffi.ffi_close_port(port)
      result
    }
  }
}

/// Collect output from a port until exit or deadline exceeded.
/// Uses deadline-based timeout: tracks remaining time across iterations.
/// Returns Error(Nil) for non-zero exit codes, timeout, or invalid UTF-8.
fn collect_output_with_deadline(
  port: port_ffi.Port,
  acc: BitArray,
  deadline_ms: Int,
) -> Result(String, Nil) {
  let remaining_ms = deadline_ms - port_ffi.monotonic_time_ms()
  case remaining_ms <= 0 {
    True -> Error(Nil)
    False ->
      case port_ffi.receive_timeout(port, remaining_ms) {
        Ok(msg) ->
          case msg {
            Data(bytes) ->
              collect_output_with_deadline(
                port,
                bit_array.append(acc, bytes),
                deadline_ms,
              )
            ExitStatus(code) -> {
              // Only return Ok for exit code 0
              case code {
                0 ->
                  case bit_array.to_string(acc) {
                    Ok(s) -> Ok(s)
                    Error(Nil) -> Error(Nil)
                  }
                _ -> Error(Nil)
              }
            }
            Eof -> {
              // EOF without ExitStatus - convert accumulated bytes
              case bit_array.to_string(acc) {
                Ok(s) -> Ok(s)
                Error(Nil) -> Error(Nil)
              }
            }
            Timeout -> Error(Nil)
          }
        Error(_) -> Error(Nil)
      }
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
    Error(_) -> PreflightTimeout
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
