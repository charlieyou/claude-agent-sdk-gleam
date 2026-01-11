import claude_agent_sdk/internal/cli.{
  type CliVersion, UnknownVersion, parse_version_string,
}
import claude_agent_sdk/internal/port_ffi.{Data, Eof, ExitStatus, Timeout}
import gleam/bit_array
import gleam/io
import gleam/string
import support/env_helpers.{get_env}

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
/// Returns Ok(stdout) on success, Error(Nil) on timeout or failure.
fn run_command_with_timeout(
  path: String,
  args: List(String),
  timeout_ms: Int,
) -> Result(String, Nil) {
  case port_ffi.ffi_open_port_safe(path, args, ".") {
    Error(_) -> Error(Nil)
    Ok(port) -> {
      let result = collect_output_with_timeout(port, <<>>, timeout_ms)
      port_ffi.ffi_close_port(port)
      result
    }
  }
}

/// Collect output from a port until exit or timeout.
fn collect_output_with_timeout(
  port: port_ffi.Port,
  acc: BitArray,
  timeout_ms: Int,
) -> Result(String, Nil) {
  case port_ffi.receive_timeout(port, timeout_ms) {
    Ok(msg) ->
      case msg {
        Data(bytes) ->
          collect_output_with_timeout(
            port,
            bit_array.append(acc, bytes),
            timeout_ms,
          )
        ExitStatus(_code) -> {
          // Process exited - convert accumulated bytes to string
          case bit_array.to_string(acc) {
            Ok(s) -> Ok(s)
            Error(Nil) -> Error(Nil)
          }
        }
        Eof -> {
          // EOF received - convert accumulated bytes to string
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
