//// Claude Agent SDK for Gleam
////
//// This module provides the main entry point for the Claude Agent SDK.
//// Re-exports all public types for convenient imports.
////
//// ## Pattern Matching on Variant Types
////
//// Due to Gleam's module system, variant constructors cannot be re-exported.
//// This means you can import types from this module and use factory functions
//// to create values, but pattern matching requires importing the source module:
////
//// ```gleam
//// import claude_agent_sdk.{type ContentBlock}
//// import claude_agent_sdk/content  // Required for pattern matching
////
//// // Creating values - use factory functions from main module
//// let block = claude_agent_sdk.text_block("Hello")
////
//// // Pattern matching - use constructors from source module
//// case block {
////   content.TextBlock(text) -> // handle text
////   content.ToolUseBlock(id, name, input) -> // handle tool use
////   content.UnknownBlock(raw) -> // handle unknown
//// }
//// ```
////
//// This applies to: ContentBlock, ToolResultBlock, Message, QueryError,
//// StreamError, and other variant types.

import gleam/dynamic
import gleam/option.{None, Some}
import gleam/string

import claude_agent_sdk/internal/cli
import claude_agent_sdk/internal/port_ffi
import claude_agent_sdk/internal/stream as internal_stream

// =============================================================================
// Query Options (from options.gleam)
// =============================================================================

import claude_agent_sdk/options

pub type QueryOptions =
  options.QueryOptions

pub type PermissionMode =
  options.PermissionMode

pub type TestRunner =
  options.TestRunner

/// Create default query options with all fields unset.
pub const default_options = options.default_options

/// Set the model to use.
pub const with_model = options.with_model

/// Set maximum number of agent turns.
pub const with_max_turns = options.with_max_turns

/// Set maximum budget in USD.
pub const with_max_budget = options.with_max_budget

/// Set the system prompt (replaces default).
pub const with_system_prompt = options.with_system_prompt

/// Append to the default system prompt.
pub const with_append_system_prompt = options.with_append_system_prompt

/// Set allowed tools (whitelist).
pub const with_allowed_tools = options.with_allowed_tools

/// Set disallowed tools (blacklist).
pub const with_disallowed_tools = options.with_disallowed_tools

/// Set path to MCP configuration file.
pub const with_mcp_config = options.with_mcp_config

/// Set permission mode for tool execution.
pub const with_permission_mode = options.with_permission_mode

/// Resume a specific session by ID.
pub const with_resume = options.with_resume

/// Continue the most recent session.
pub const with_continue = options.with_continue

/// Set working directory for the CLI process.
pub const with_cwd = options.with_cwd

/// Enable test mode with a mock runner.
pub const with_test_mode = options.with_test_mode

/// Skip CLI version check entirely.
pub const with_skip_version_check = options.with_skip_version_check

/// Allow unknown CLI versions with a warning.
pub const with_permissive_version_check = options.with_permissive_version_check

// =============================================================================
// Message Types (from message.gleam)
// =============================================================================

import claude_agent_sdk/message

pub type Message =
  message.Message

pub type MessageEnvelope =
  message.MessageEnvelope

pub type SystemMessage =
  message.SystemMessage

pub type AssistantMessage =
  message.AssistantMessage

pub type AssistantMessageContent =
  message.AssistantMessageContent

pub type UserMessage =
  message.UserMessage

pub type UserMessageContent =
  message.UserMessageContent

pub type ResultMessage =
  message.ResultMessage

pub type ResultSubtype =
  message.ResultSubtype

pub type Usage =
  message.Usage

pub type McpServerStatus =
  message.McpServerStatus

pub type PermissionDenial =
  message.PermissionDenial

// =============================================================================
// Content Block Types (from content.gleam)
// =============================================================================

import claude_agent_sdk/content

pub type ContentBlock =
  content.ContentBlock

/// Creates a TextBlock content block
pub fn text_block(text: String) -> ContentBlock {
  content.TextBlock(text)
}

/// Creates a ToolUseBlock content block
pub fn tool_use_block(
  id: String,
  name: String,
  input: dynamic.Dynamic,
) -> ContentBlock {
  content.ToolUseBlock(id, name, input)
}

/// Creates an UnknownBlock content block for forward compatibility
pub fn unknown_block(raw: dynamic.Dynamic) -> ContentBlock {
  content.UnknownBlock(raw)
}

pub type ToolResultBlock =
  content.ToolResultBlock

/// Creates a ToolResultBlock
pub fn tool_result_block(
  tool_use_id: String,
  content_text: String,
  is_error: option.Option(Bool),
) -> ToolResultBlock {
  content.ToolResultBlock(tool_use_id, content_text, is_error)
}

// =============================================================================
// Error Types (from error.gleam)
// =============================================================================

import claude_agent_sdk/error

pub type QueryError =
  error.QueryError

pub type StreamError =
  error.StreamError

pub type ErrorDiagnostic =
  error.ErrorDiagnostic

pub type StreamItem =
  error.StreamItem

pub type Warning =
  error.Warning

pub type WarningCode =
  error.WarningCode

/// Check if a StreamError is terminal (stream closed, no more items)
pub const is_terminal = error.is_terminal

// =============================================================================
// Stream Helpers (from claude_agent_sdk/stream.gleam)
// =============================================================================

import claude_agent_sdk/stream

pub type QueryStream =
  stream.QueryStream

/// Advance the stream by one item.
/// **STABLE API**: Breaking changes require major version bump.
pub const next = stream.next

/// Close the stream and release resources. Idempotent.
/// **STABLE API**: Breaking changes require major version bump.
pub const close = stream.close

/// Check if the stream is closed.
/// **STABLE API**: Breaking changes require major version bump.
pub const is_closed = stream.is_closed

pub type CollectResult(a) =
  internal_stream.CollectResult(a)

pub type FoldAction(a) =
  internal_stream.FoldAction(a)

/// Execute a function with a stream, ensuring cleanup on any exit path.
/// **STABLE API**: Breaking changes require major version bump.
pub const with_stream = internal_stream.with_stream

/// Collect all items from a stream.
/// **STABLE API**: Breaking changes require major version bump.
pub const collect_items = internal_stream.collect_items

/// Collect only messages from a stream (filtering out warnings).
/// **STABLE API**: Breaking changes require major version bump.
pub const collect_messages = internal_stream.collect_messages

/// Fold over stream items with custom accumulation logic.
/// **STABLE API**: Breaking changes require major version bump.
pub const fold_stream = internal_stream.fold_stream

/// Convert a QueryStream to a Yielder for gleam/yielder combinator interop.
/// **WARNING**: May leak resources if not fully consumed. See documentation.
/// **STABLE API**: Breaking changes require major version bump.
pub const to_yielder = internal_stream.to_yielder

// =============================================================================
// SDK Version
// =============================================================================

pub fn version() -> String {
  "0.1.0"
}

// =============================================================================
// Query Entry Point
// =============================================================================

/// The CLI executable name to search for in PATH.
const cli_name = "claude"

/// Start a query to Claude CLI and return a stream for iteration.
///
/// ## Implementation Flow
/// 1. Find CLI in PATH (returns CliNotFoundError if not found)
/// 2. Check version (unless skip_version_check=True)
///    - Detects CLI version via `claude --version`
///    - Verifies version meets minimum requirement (1.0.0)
///    - Handles permissive_version_check for unknown versions
/// 3. Build CLI arguments from options
/// 4. Spawn CLI process
/// 5. Return Ok(QueryStream) for iteration
///
/// ## cwd Handling
/// | cwd value | Behavior |
/// |-----------|----------|
/// | None | Inherit current working directory |
/// | Some("") | Inherit current working directory |
/// | Some(path) | Change to specified directory |
///
/// ## Example
/// ```gleam
/// import claude_agent_sdk
///
/// let options = claude_agent_sdk.default_options()
///   |> claude_agent_sdk.with_max_turns(3)
///
/// case claude_agent_sdk.query("Hello, Claude!", options) {
///   Ok(stream) -> {
///     // Iterate with next() or use collect_messages()
///     claude_agent_sdk.close(stream)
///   }
///   Error(err) -> {
///     // Handle QueryError
///   }
/// }
/// ```
pub fn query(
  prompt: String,
  options: QueryOptions,
) -> Result(QueryStream, QueryError) {
  // Step 1: Find CLI in PATH
  case port_ffi.find_cli_path(cli_name) {
    Error(_) ->
      Error(error.CliNotFoundError(
        "Claude CLI not found in PATH. Install with: npm install -g @anthropic-ai/claude-code",
      ))
    Ok(cli_path) -> query_with_cli_path(prompt, options, cli_path)
  }
}

/// Internal: Continue query flow with known CLI path.
fn query_with_cli_path(
  prompt: String,
  options: QueryOptions,
  cli_path: String,
) -> Result(QueryStream, QueryError) {
  // Step 2: Version check (unless skipped)
  case options.skip_version_check {
    True -> spawn_query(prompt, options, cli_path)
    False -> {
      case cli.detect_cli_version(cli_path) {
        Error(cli.VersionCheckTimeout) ->
          Error(error.VersionDetectionError("Timeout waiting for CLI response"))
        Error(cli.SpawnFailed(reason)) ->
          Error(error.VersionDetectionError("Failed to spawn CLI: " <> reason))
        Error(cli.ParseFailed(raw)) ->
          // Handle parse failure based on permissive mode
          case options.permissive_version_check {
            True ->
              // Permissive: allow unknown versions with warning (proceed)
              spawn_query(prompt, options, cli_path)
            False ->
              Error(error.UnknownVersionError(
                raw,
                "Run 'claude --version' to check CLI output format",
              ))
          }
        Ok(version) ->
          check_version_and_spawn(prompt, options, cli_path, version)
      }
    }
  }
}

/// Internal: Check version meets minimum and spawn if OK.
fn check_version_and_spawn(
  prompt: String,
  options: QueryOptions,
  cli_path: String,
  version: cli.CliVersion,
) -> Result(QueryStream, QueryError) {
  case version {
    cli.UnknownVersion(raw) ->
      // Unknown version format
      case options.permissive_version_check {
        True ->
          // Permissive: allow with warning (proceed)
          spawn_query(prompt, options, cli_path)
        False ->
          Error(error.UnknownVersionError(
            raw,
            "Run 'claude --version' to check CLI output format",
          ))
      }
    cli.CliVersion(_, _, _, _) as known_version ->
      case cli.version_meets_minimum(known_version, cli.minimum_cli_version) {
        True -> spawn_query(prompt, options, cli_path)
        False -> {
          let detected_str = format_version(known_version)
          let required_str = format_version(cli.minimum_cli_version)
          Error(error.UnsupportedCliVersionError(
            detected_version: detected_str,
            minimum_required: required_str,
            suggestion: "Run: npm update -g @anthropic-ai/claude-code",
          ))
        }
      }
  }
}

/// Internal: Format CliVersion to string.
fn format_version(version: cli.CliVersion) -> String {
  case version {
    cli.CliVersion(_, _, _, raw) -> raw
    cli.UnknownVersion(raw) -> raw
  }
}

/// Internal: Build args, spawn port, return QueryStream.
fn spawn_query(
  prompt: String,
  options: QueryOptions,
  cli_path: String,
) -> Result(QueryStream, QueryError) {
  // Build CLI arguments
  let args = cli.build_cli_args(options, prompt)

  // Determine cwd - empty string means inherit
  let cwd = case options.cwd {
    None -> ""
    Some(path) ->
      case string.is_empty(path) {
        True -> ""
        False -> path
      }
  }

  // Spawn port (production path uses port_ffi directly)
  // Note: test_mode path would use test_runner here, but TestRunner is
  // currently an opaque placeholder. When implemented, this would be:
  // case options.test_mode {
  //   True -> test_runner_spawn(options.test_runner, args, cwd)
  //   False -> ... production spawn ...
  // }
  case port_ffi.ffi_open_port_safe(cli_path, args, cwd) {
    Error(reason) -> Error(error.SpawnError(reason))
    Ok(port) -> Ok(internal_stream.new(port))
  }
}
