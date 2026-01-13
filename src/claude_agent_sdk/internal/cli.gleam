//// CLI argument building and version detection types.
//// Internal module - converts QueryOptions to List(String) for CLI invocation,
//// and provides pure version parsing functions for unit testing.

import claude_agent_sdk/internal/constants
import claude_agent_sdk/internal/port_ffi.{
  type Port, Data, Eof, ExitStatus, Timeout,
}
import claude_agent_sdk/options.{
  type PermissionMode, type QueryOptions, AcceptEdits, BypassPermissions, Plan,
}
import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// Version Detection Types
// ============================================================================

/// CLI version type with two variants:
/// - CliVersion: Successfully parsed semantic version
/// - UnknownVersion: Could not parse version from output
pub type CliVersion {
  CliVersion(major: Int, minor: Int, patch: Int, raw: String)
  UnknownVersion(raw: String)
}

/// Minimum CLI version required by SDK (1.0.0)
pub const minimum_cli_version = CliVersion(1, 0, 0, "1.0.0")

/// Minimum CLI version required for bidirectional protocol support (1.1.0)
pub const minimum_bidir_cli_version = CliVersion(1, 1, 0, "1.1.0")

/// Errors from version detection
pub type VersionCheckError {
  /// Timeout waiting for CLI response
  VersionCheckTimeout
  /// Failed to spawn CLI process
  SpawnFailed(reason: String)
  /// CLI output could not be parsed as a version
  ParseFailed(raw_output: String)
}

/// Default timeout for version detection (5000ms)
pub fn default_version_timeout_ms() -> Int {
  constants.version_detection_timeout_ms
}

// ============================================================================
// Version Detection
// ============================================================================

/// Detect CLI version by spawning `cli_path --version` and parsing output.
/// Uses port_ffi directly with a 5-second timeout.
pub fn detect_cli_version(
  cli_path: String,
) -> Result(CliVersion, VersionCheckError) {
  // Spawn the CLI with --version (using safe version to handle spawn failures)
  case port_ffi.ffi_open_port_safe(cli_path, ["--version"], ".") {
    Error(reason) -> Error(SpawnFailed(reason))
    Ok(port) -> {
      // Collect output with timeout
      case collect_version_output(port, <<>>) {
        Ok(output) -> {
          port_ffi.ffi_close_port(port)
          case parse_version_string(output) {
            Ok(version) -> Ok(version)
            Error(Nil) -> Error(ParseFailed(output))
          }
        }
        Error(err) -> {
          port_ffi.ffi_close_port(port)
          Error(err)
        }
      }
    }
  }
}

/// Collect all output from the version command until exit or timeout.
fn collect_version_output(
  port: Port,
  acc: BitArray,
) -> Result(String, VersionCheckError) {
  case port_ffi.receive_timeout(port, constants.version_detection_timeout_ms) {
    Ok(msg) ->
      case msg {
        Data(bytes) ->
          collect_version_output(port, bit_array.append(acc, bytes))
        ExitStatus(_code) -> {
          // Process exited - convert accumulated bytes to string
          case bit_array.to_string(acc) {
            Ok(s) -> Ok(s)
            Error(Nil) -> Error(ParseFailed("<invalid utf-8>"))
          }
        }
        Eof -> {
          // EOF received - convert accumulated bytes to string
          case bit_array.to_string(acc) {
            Ok(s) -> Ok(s)
            Error(Nil) -> Error(ParseFailed("<invalid utf-8>"))
          }
        }
        Timeout -> Error(VersionCheckTimeout)
      }
    Error(reason) -> Error(SpawnFailed(reason))
  }
}

// ============================================================================
// Version Parsing Functions
// ============================================================================

/// Parse version string from CLI output.
/// Extracts first semver pattern (major.minor.patch) from various formats:
/// - "claude v1.2.3\n" -> Ok(CliVersion(1,2,3,"1.2.3"))
/// - "1.2.3" -> Ok(CliVersion(1,2,3,"1.2.3"))
/// - "v1.2.3" -> Ok(CliVersion(1,2,3,"1.2.3"))
/// - "Claude Code CLI 1.2.3-beta.1" -> Ok(CliVersion(1,2,3,"1.2.3"))
/// - "  1.2.3  \n" -> Ok (whitespace tolerant)
/// - "garbage" -> Error(Nil)
/// - "" -> Error(Nil)
pub fn parse_version_string(output: String) -> Result(CliVersion, Nil) {
  // Trim whitespace and convert to graphemes for scanning
  let trimmed = string.trim(output)
  find_version_in_string(trimmed)
}

/// Scan string for first occurrence of digit sequence that could be a version.
fn find_version_in_string(s: String) -> Result(CliVersion, Nil) {
  let graphemes = string.to_graphemes(s)
  scan_for_digit_start(graphemes)
}

/// Scan graphemes looking for a digit that might start a version number.
fn scan_for_digit_start(graphemes: List(String)) -> Result(CliVersion, Nil) {
  case graphemes {
    [] -> Error(Nil)
    [g, ..rest] ->
      case is_digit(g) {
        True -> {
          // Try to parse version starting here
          case try_parse_version_at(graphemes) {
            Ok(version) -> Ok(version)
            Error(_) -> scan_for_digit_start(rest)
          }
        }
        False -> scan_for_digit_start(rest)
      }
  }
}

/// Try to parse "major.minor.patch" starting at current position.
fn try_parse_version_at(graphemes: List(String)) -> Result(CliVersion, Nil) {
  // Parse major
  use #(major_str, rest1) <- result.try(take_digits(graphemes, ""))
  use major <- result.try(int.parse(major_str))

  // Expect dot
  use rest2 <- result.try(expect_dot(rest1))

  // Parse minor
  use #(minor_str, rest3) <- result.try(take_digits(rest2, ""))
  use minor <- result.try(int.parse(minor_str))

  // Expect dot
  use rest4 <- result.try(expect_dot(rest3))

  // Parse patch
  use #(patch_str, _rest5) <- result.try(take_digits(rest4, ""))
  use patch <- result.try(int.parse(patch_str))

  let raw = major_str <> "." <> minor_str <> "." <> patch_str
  Ok(CliVersion(major, minor, patch, raw))
}

/// Take consecutive digits from the start of the list.
fn take_digits(
  graphemes: List(String),
  acc: String,
) -> Result(#(String, List(String)), Nil) {
  case graphemes {
    [] ->
      case acc {
        "" -> Error(Nil)
        _ -> Ok(#(acc, []))
      }
    [g, ..rest] ->
      case is_digit(g) {
        True -> take_digits(rest, acc <> g)
        False ->
          case acc {
            "" -> Error(Nil)
            _ -> Ok(#(acc, graphemes))
          }
      }
  }
}

/// Expect a dot at the start of the list.
fn expect_dot(graphemes: List(String)) -> Result(List(String), Nil) {
  case graphemes {
    [".", ..rest] -> Ok(rest)
    _ -> Error(Nil)
  }
}

/// Check if a single-character string is a digit 0-9.
fn is_digit(s: String) -> Bool {
  case s {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

/// Single source of truth for version comparison.
/// Returns True if `version >= minimum` using semantic versioning.
/// Only accepts known CliVersion variants - UnknownVersion must be handled separately.
pub fn version_meets_minimum(version: CliVersion, minimum: CliVersion) -> Bool {
  case version, minimum {
    CliVersion(maj, min, patch, _), CliVersion(min_maj, min_min, min_patch, _) ->
      maj > min_maj
      || { maj == min_maj && min > min_min }
      || { maj == min_maj && min == min_min && patch >= min_patch }
    // UnknownVersion cannot be compared - return False to be safe
    _, _ -> False
  }
}

/// Format a human-readable error message for version mismatch.
pub fn format_version_error(
  detected: CliVersion,
  required: CliVersion,
) -> String {
  let detected_str = case detected {
    CliVersion(_, _, _, raw) -> raw
    UnknownVersion(raw) -> raw
  }
  let required_str = case required {
    CliVersion(_, _, _, raw) -> raw
    UnknownVersion(raw) -> raw
  }
  "CLI version "
  <> detected_str
  <> " is below minimum required "
  <> required_str
  <> ". Run: npm update -g @anthropic-ai/claude-code"
}

/// Build CLI arguments from QueryOptions and prompt.
/// Fixed arguments are always included: --print --output-format stream-json --verbose
/// Prompt is added at the end after -- separator.
pub fn build_cli_args(options: QueryOptions, prompt: String) -> List(String) {
  // Start with fixed arguments for standard (unidirectional) mode
  let base_args = ["--print", "--output-format", "stream-json", "--verbose"]
  build_args_with_base(base_args, options, Some(prompt))
}

/// Internal helper to build CLI arguments from a base set of flags.
/// Shared by both build_cli_args and build_bidir_cli_args.
fn build_args_with_base(
  base_args: List(String),
  options: QueryOptions,
  prompt: Option(String),
) -> List(String) {
  // Add optional model
  let args = case options.model {
    Some(m) -> list.append(base_args, ["--model", m])
    None -> base_args
  }

  // Add optional max_turns
  let args = case options.max_turns {
    Some(n) -> list.append(args, ["--max-turns", int.to_string(n)])
    None -> args
  }

  // Add optional max_budget_usd
  let args = case options.max_budget_usd {
    Some(usd) -> list.append(args, ["--max-budget-usd", float.to_string(usd)])
    None -> args
  }

  // Precedence: system_prompt > append_system_prompt
  let args = case options.system_prompt {
    Some(p) -> list.append(args, ["--system-prompt", p])
    None ->
      case options.append_system_prompt {
        Some(p) -> list.append(args, ["--append-system-prompt", p])
        None -> args
      }
  }

  // Precedence: allowed_tools > disallowed_tools
  let args = case options.allowed_tools {
    Some(tools) ->
      list.append(args, ["--allowed-tools", string.join(tools, ",")])
    None ->
      case options.disallowed_tools {
        Some(tools) ->
          list.append(args, ["--disallowed-tools", string.join(tools, ",")])
        None -> args
      }
  }

  // Add optional mcp_config_path
  let args = case options.mcp_config_path {
    Some(path) -> list.append(args, ["--mcp-config", path])
    None -> args
  }

  // Add optional permission_mode
  let args = case options.permission_mode {
    Some(mode) -> add_permission_mode(args, mode)
    None -> args
  }

  // Precedence: resume_session_id > continue_session
  let args = case options.resume_session_id {
    Some(id) -> list.append(args, ["--resume", id])
    None ->
      case options.continue_session {
        True -> list.append(args, ["--continue"])
        False -> args
      }
  }

  // Add prompt separator and prompt at the end (if provided)
  case prompt {
    Some(p) -> list.append(args, ["--", p])
    None -> args
  }
}

/// Add permission mode flags to argument list.
fn add_permission_mode(args: List(String), mode: PermissionMode) -> List(String) {
  case mode {
    AcceptEdits -> list.append(args, ["--permission-mode", "acceptEdits"])
    BypassPermissions -> list.append(args, ["--dangerously-skip-permissions"])
    Plan -> list.append(args, ["--permission-mode", "plan"])
    // Default permission mode - don't add any flag
    options.Default -> args
  }
}

// ============================================================================
// Bidirectional Mode Support
// ============================================================================

/// Check if options contain any bidirectional features.
/// Returns True if any hooks, can_use_tool handler, mcp_servers, or timeout_ms are set.
/// Note: mcp_config_path is NOT a bidir-only feature - query() supports it via --mcp-config.
pub fn has_bidir_features(options: QueryOptions) -> Bool {
  option.is_some(options.on_pre_tool_use)
  || option.is_some(options.on_post_tool_use)
  || option.is_some(options.on_user_prompt_submit)
  || option.is_some(options.on_stop)
  || option.is_some(options.on_subagent_stop)
  || option.is_some(options.on_pre_compact)
  || option.is_some(options.on_can_use_tool)
  || !list.is_empty(options.mcp_servers)
  || option.is_some(options.timeout_ms)
}

/// Build CLI arguments for bidirectional mode.
/// Includes --input-format stream-json in addition to all standard args.
pub fn build_bidir_cli_args(
  options: QueryOptions,
  _prompt: String,
) -> List(String) {
  // Start with fixed arguments for bidir mode (includes --input-format)
  let base_args = [
    "--output-format",
    "stream-json",
    "--input-format",
    "stream-json",
    "--verbose",
  ]
  build_args_with_base(base_args, options, None)
}
