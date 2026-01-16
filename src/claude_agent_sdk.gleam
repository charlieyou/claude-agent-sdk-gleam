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
import gleam/list
import gleam/option.{None, Some}
import gleam/string

import gleam/erlang/process.{type Subject}

import claude_agent_sdk/control
import claude_agent_sdk/event
import claude_agent_sdk/internal/bidir
import claude_agent_sdk/internal/bidir/actor
import claude_agent_sdk/internal/bidir_runner
import claude_agent_sdk/internal/cli
import claude_agent_sdk/internal/port_io
import claude_agent_sdk/internal/stream as internal_stream
import claude_agent_sdk/session

// =============================================================================
// Query Options (from options.gleam)
// =============================================================================

import claude_agent_sdk/options
import claude_agent_sdk/runner

/// New split option types (recommended)
pub type CliOptions =
  options.CliOptions

pub type SdkOptions =
  options.SdkOptions

pub type BidirOptions =
  options.BidirOptions

/// Legacy combined option type (deprecated, for backwards compatibility)
pub type QueryOptions =
  options.QueryOptions

pub type PermissionMode =
  options.PermissionMode

/// Runner type for process execution abstraction.
/// Use `test_runner()` to create a mock runner for testing.
pub type Runner =
  runner.Runner

/// Handle type for process references (opaque).
pub type Handle =
  runner.Handle

/// Result of reading from a process handle.
pub type ReadResult =
  runner.ReadResult

// =============================================================================
// Session Types (from session.gleam)
// =============================================================================

/// Opaque session handle for bidirectional mode.
/// Wraps a Subject for communicating with the session GenServer.
pub type Session =
  session.Session

/// Session lifecycle event variants.
/// Use events() to get a Subject for receiving these.
pub type SessionEvent =
  event.SessionEvent

/// Create a test runner with user-provided callbacks.
pub const test_runner = runner.test_runner

// =============================================================================
// New Option Builders (recommended API)
// =============================================================================

/// Create default CLI options.
pub const cli_options = options.cli_options

/// Create default SDK options.
pub const sdk_options = options.sdk_options

/// Create default bidirectional options.
pub const bidir_options = options.bidir_options

// =============================================================================
// Legacy Option Builders (deprecated, for backwards compatibility)
// =============================================================================

/// Create default query options with all fields unset.
pub const default_options = options.default_options

/// Set the model to use.
pub const with_model = options.with_model_query

/// Set maximum number of agent turns.
pub const with_max_turns = options.with_max_turns_query

/// Set maximum budget in USD.
pub const with_max_budget = options.with_max_budget_query

/// Set the system prompt (replaces default).
pub const with_system_prompt = options.with_system_prompt_query

/// Append to the default system prompt.
pub const with_append_system_prompt = options.with_append_system_prompt_query

/// Set allowed tools (whitelist).
pub const with_allowed_tools = options.with_allowed_tools_query

/// Set disallowed tools (blacklist).
pub const with_disallowed_tools = options.with_disallowed_tools_query

/// Set path to MCP configuration file.
pub const with_mcp_config = options.with_mcp_config_query

/// Set permission mode for tool execution.
pub const with_permission_mode = options.with_permission_mode_query

/// Resume a specific session by ID.
pub const with_resume = options.with_resume_query

/// Continue the most recent session.
pub const with_continue = options.with_continue_query

/// Set working directory for the CLI process.
pub const with_cwd = options.with_cwd_query

/// Enable test mode with a mock runner.
pub const with_test_mode = options.with_test_mode_query

/// Skip CLI version check entirely.
pub const with_skip_version_check = options.with_skip_version_check_query

/// Allow unknown CLI versions with a warning.
pub const with_permissive_version_check = options.with_permissive_version_check_query

/// Set the global timeout in milliseconds.
pub const with_timeout = options.with_timeout_query

/// Set a per-hook timeout override.
pub const with_hook_timeout = options.with_hook_timeout_query

/// Set a factory function for creating BidirRunner instances (testing seam).
pub const with_bidir_runner_factory = options.with_bidir_runner_factory_query

/// Set a pre-tool-use hook callback.
pub const with_pre_tool_use = options.with_pre_tool_use_query

/// Set a post-tool-use hook callback.
pub const with_post_tool_use = options.with_post_tool_use_query

/// Set a user-prompt-submit hook callback.
pub const with_user_prompt_submit = options.with_user_prompt_submit_query

/// Set a stop hook callback.
pub const with_stop = options.with_stop_query

/// Set a subagent-stop hook callback.
pub const with_subagent_stop = options.with_subagent_stop_query

/// Set a pre-compact hook callback.
pub const with_pre_compact = options.with_pre_compact_query

/// Set the permission callback for tool execution control.
pub const with_can_use_tool = options.with_can_use_tool_query

/// Add an MCP server handler.
pub const with_mcp_server = options.with_mcp_server_query

/// Enable file checkpointing for rewind_files support.
pub const with_file_checkpointing = options.with_file_checkpointing_query

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

pub type StartError =
  error.StartError

pub type ControlError =
  error.ControlError

pub type StopError =
  error.StopError

/// Check if a StreamError is terminal (stream closed, no more items)
pub const is_terminal = error.is_terminal

/// Convert a StartError to a human-readable string
pub const start_error_to_string = error.start_error_to_string

/// Convert a ControlError to a human-readable string
pub const control_error_to_string = error.control_error_to_string

/// Convert a StopError to a human-readable string
pub const stop_error_to_string = error.stop_error_to_string

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
/// ## Example (new API)
/// ```gleam
/// import claude_agent_sdk
/// import claude_agent_sdk/options
///
/// let cli_opts = options.cli_options()
///   |> options.with_model("sonnet")
/// let sdk_opts = options.sdk_options()
///
/// case claude_agent_sdk.query_new("Hello, Claude!", cli_opts, sdk_opts) {
///   Ok(stream) -> {
///     // Iterate with next() or use collect_messages()
///     claude_agent_sdk.close(stream)
///   }
///   Error(err) -> {
///     // Handle QueryError
///   }
/// }
/// ```
///
/// ## Example (legacy API)
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
/// Query with split options (new API).
///
/// Takes separate CliOptions and SdkOptions instead of combined QueryOptions.
/// This API does not accept bidir options since query() is for one-shot queries.
pub fn query_new(
  prompt: String,
  cli_opts: CliOptions,
  sdk_opts: SdkOptions,
) -> Result(QueryStream, QueryError) {
  case sdk_opts.test_mode {
    True -> {
      case sdk_opts.test_runner {
        Some(_) -> spawn_query_new_cli(prompt, cli_opts, sdk_opts, cli_name, [])
        None ->
          Error(error.TestModeError(
            "test_mode enabled but no test_runner provided. Use with_test_mode(runner) to configure.",
          ))
      }
    }
    False -> {
      // Use cli_opts.cli_path if provided, otherwise discover via PATH
      case cli_opts.cli_path {
        Some(custom_path) ->
          query_new_with_cli_path(prompt, cli_opts, sdk_opts, custom_path)
        None ->
          case port_io.find_cli_path(cli_name) {
            Error(_) ->
              Error(error.CliNotFoundError(
                "Claude CLI not found in PATH. Install with: npm install -g @anthropic-ai/claude-code",
              ))
            Ok(cli_path) ->
              query_new_with_cli_path(prompt, cli_opts, sdk_opts, cli_path)
          }
      }
    }
  }
}

/// Internal: Continue query_new flow with known CLI path (version check).
fn query_new_with_cli_path(
  prompt: String,
  cli_opts: CliOptions,
  sdk_opts: SdkOptions,
  cli_path: String,
) -> Result(QueryStream, QueryError) {
  // Determine cwd for version check - empty string means inherit
  let cwd = case cli_opts.cwd {
    None -> ""
    Some(path) ->
      case string.is_empty(path) {
        True -> ""
        False -> path
      }
  }
  case sdk_opts.skip_version_check {
    True -> spawn_query_new_cli(prompt, cli_opts, sdk_opts, cli_path, [])
    False -> {
      case cli.detect_cli_version(cli_path, cwd) {
        Error(cli.VersionCheckTimeout) ->
          Error(error.VersionDetectionError("Timeout waiting for CLI response"))
        Error(cli.SpawnFailed(reason)) ->
          Error(error.VersionDetectionError("Failed to spawn CLI: " <> reason))
        Error(cli.ParseFailed(raw)) ->
          case sdk_opts.permissive_version_check {
            True -> {
              let warning =
                error.Warning(
                  code: error.UnparseableCliVersion,
                  message: "CLI version string could not be parsed; proceeding in permissive mode",
                  context: Some(raw),
                )
              spawn_query_new_cli(prompt, cli_opts, sdk_opts, cli_path, [
                warning,
              ])
            }
            False ->
              Error(error.UnknownVersionError(
                raw,
                "Run 'claude --version' to check CLI output format",
              ))
          }
        Ok(version) ->
          check_version_and_spawn_new(
            prompt,
            cli_opts,
            sdk_opts,
            cli_path,
            version,
          )
      }
    }
  }
}

/// Internal: Check version and spawn for new API.
fn check_version_and_spawn_new(
  prompt: String,
  cli_opts: CliOptions,
  sdk_opts: SdkOptions,
  cli_path: String,
  version: cli.CliVersion,
) -> Result(QueryStream, QueryError) {
  case version {
    cli.UnknownVersion(raw) ->
      case sdk_opts.permissive_version_check {
        True -> {
          let warning =
            error.Warning(
              code: error.UnparseableCliVersion,
              message: "CLI version string could not be parsed; proceeding in permissive mode",
              context: Some(raw),
            )
          spawn_query_new_cli(prompt, cli_opts, sdk_opts, cli_path, [warning])
        }
        False ->
          Error(error.UnknownVersionError(
            raw,
            "Run 'claude --version' to check CLI output format",
          ))
      }
    cli.CliVersion(_, _, _, _) as known_version -> {
      case cli.version_meets_minimum(known_version, cli.minimum_cli_version) {
        True -> spawn_query_new_cli(prompt, cli_opts, sdk_opts, cli_path, [])
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
}

/// Internal: Spawn query using CliOptions directly (new API path).
fn spawn_query_new_cli(
  prompt: String,
  cli_opts: CliOptions,
  sdk_opts: SdkOptions,
  cli_path: String,
  warnings: List(error.Warning),
) -> Result(QueryStream, QueryError) {
  // Build CLI arguments from CliOptions directly
  let args = cli.build_cli_args_new(cli_opts, prompt)

  // Determine cwd - empty string means inherit
  let cwd = case cli_opts.cwd {
    None -> ""
    Some(path) ->
      case string.is_empty(path) {
        True -> ""
        False -> path
      }
  }

  case sdk_opts.test_mode {
    True -> {
      case sdk_opts.test_runner {
        Some(test_runner) -> {
          case runner.spawn(test_runner, cli_path, args, cwd) {
            Error(reason) -> Error(error.SpawnError(reason))
            Ok(handle) ->
              Ok(internal_stream.new_from_runner_with_warnings(
                test_runner,
                handle,
                warnings,
              ))
          }
        }
        None ->
          Error(error.TestModeError(
            "test_mode enabled but no test_runner provided. Use with_test_mode(runner) to configure.",
          ))
      }
    }
    False ->
      case port_io.open_port_safe(cli_path, args, cwd) {
        Error(reason) -> Error(error.SpawnError(reason))
        Ok(port) -> Ok(internal_stream.new_with_warnings(port, warnings))
      }
  }
}

/// Query with legacy combined QueryOptions (deprecated).
///
/// **DEPRECATED**: Use `query_new(prompt, cli_opts, sdk_opts)` instead.
pub fn query(
  prompt: String,
  options: QueryOptions,
) -> Result(QueryStream, QueryError) {
  case options.test_mode {
    True -> {
      // Test mode: validate test_runner is provided, then spawn without CLI discovery
      case options.test_runner {
        Some(_) -> spawn_query(prompt, options, cli_name)
        None ->
          Error(error.TestModeError(
            "test_mode enabled but no test_runner provided. Use with_test_mode(runner) to configure.",
          ))
      }
    }
    False -> {
      // Production mode: Find CLI in PATH
      case port_io.find_cli_path(cli_name) {
        Error(_) ->
          Error(error.CliNotFoundError(
            "Claude CLI not found in PATH. Install with: npm install -g @anthropic-ai/claude-code",
          ))
        Ok(cli_path) -> query_with_cli_path(prompt, options, cli_path)
      }
    }
  }
}

/// Internal: Continue query flow with known CLI path.
fn query_with_cli_path(
  prompt: String,
  options: QueryOptions,
  cli_path: String,
) -> Result(QueryStream, QueryError) {
  // Determine cwd for version check - empty string means inherit
  let cwd = case options.cwd {
    None -> ""
    Some(path) ->
      case string.is_empty(path) {
        True -> ""
        False -> path
      }
  }
  // Step 2: Version check (unless skipped)
  case options.skip_version_check {
    True -> spawn_query(prompt, options, cli_path)
    False -> {
      case cli.detect_cli_version(cli_path, cwd) {
        Error(cli.VersionCheckTimeout) ->
          Error(error.VersionDetectionError("Timeout waiting for CLI response"))
        Error(cli.SpawnFailed(reason)) ->
          Error(error.VersionDetectionError("Failed to spawn CLI: " <> reason))
        Error(cli.ParseFailed(raw)) ->
          // Handle parse failure based on permissive mode
          case options.permissive_version_check {
            True -> {
              // Permissive: allow unknown versions with warning (proceed)
              let warning =
                error.Warning(
                  code: error.UnparseableCliVersion,
                  message: "CLI version string could not be parsed; proceeding in permissive mode",
                  context: Some(raw),
                )
              spawn_query_with_warnings(prompt, options, cli_path, [warning])
            }
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
        True -> {
          // Permissive: allow with warning (proceed)
          let warning =
            error.Warning(
              code: error.UnparseableCliVersion,
              message: "CLI version string could not be parsed; proceeding in permissive mode",
              context: Some(raw),
            )
          spawn_query_with_warnings(prompt, options, cli_path, [warning])
        }
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
  spawn_query_with_warnings(prompt, options, cli_path, [])
}

/// Internal: Build args, spawn port, return QueryStream with initial warnings.
/// Legacy function that checks for bidir features and adds warning.
fn spawn_query_with_warnings(
  prompt: String,
  options: QueryOptions,
  cli_path: String,
  warnings: List(error.Warning),
) -> Result(QueryStream, QueryError) {
  // Add bidir warning if bidir features are set but using query()
  let all_warnings = case cli.has_bidir_features(options) {
    True -> {
      let bidir_warning =
        error.Warning(
          code: error.BidirOptionIgnored,
          message: "query() ignores hooks/can_use_tool/timeout_ms. Use start_session() for bidirectional features.",
          context: None,
        )
      list.append(warnings, [bidir_warning])
    }
    False -> warnings
  }

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

  case options.test_mode {
    True -> {
      case options.test_runner {
        Some(test_runner) -> {
          case runner.spawn(test_runner, cli_path, args, cwd) {
            Error(reason) -> Error(error.SpawnError(reason))
            Ok(handle) ->
              Ok(internal_stream.new_from_runner_with_warnings(
                test_runner,
                handle,
                all_warnings,
              ))
          }
        }
        None ->
          Error(error.TestModeError(
            "test_mode enabled but no test_runner provided. Use with_test_mode(runner) to configure.",
          ))
      }
    }
    False ->
      case port_io.open_port_safe(cli_path, args, cwd) {
        Error(reason) -> Error(error.SpawnError(reason))
        Ok(port) -> Ok(internal_stream.new_with_warnings(port, all_warnings))
      }
  }
}

// =============================================================================
// Bidirectional Session Entry Point
// =============================================================================

/// Start a bidirectional session with Claude CLI (new API).
///
/// Takes separate CliOptions, SdkOptions, and BidirOptions for clear separation
/// of concerns. Use this for hooks, MCP servers, and control operations.
///
/// ## Parameters
///
/// - `cli_opts`: CLI-specific options (model, tools, etc.)
/// - `sdk_opts`: SDK behavior options (test mode, version checks)
/// - `bidir_opts`: Bidirectional features (hooks, MCP, timeouts)
///
/// ## Example
///
/// ```gleam
/// import claude_agent_sdk
/// import claude_agent_sdk/options
///
/// let cli_opts = options.cli_options()
///   |> options.with_model("sonnet")
/// let sdk_opts = options.sdk_options()
/// let bidir_opts = options.bidir_options()
///   |> options.with_pre_tool_use(my_hook)
///
/// case claude_agent_sdk.start_session_new(cli_opts, sdk_opts, bidir_opts) {
///   Ok(session) -> {
///     // Use session for bidirectional communication
///   }
///   Error(err) -> {
///     // Handle StartError
///   }
/// }
/// ```
pub fn start_session_new(
  cli_opts: CliOptions,
  _sdk_opts: SdkOptions,
  bidir_opts: BidirOptions,
) -> Result(Session, StartError) {
  // Create subjects for public API - these will receive forwarded messages
  let messages = process.new_subject()
  let events = process.new_subject()

  // Create a subject for setup coordination
  let setup_subject: Subject(SetupResult) = process.new_subject()

  // Get or create runner
  let runner_result = case bidir_opts.bidir_runner_factory {
    Some(factory) -> Ok(factory())
    None -> {
      let args = cli.build_bidir_cli_args_new(cli_opts)
      // Use cli_opts.cli_path if provided, otherwise discover via PATH
      case cli_opts.cli_path {
        Some(custom_path) -> bidir_runner.start_with_path(custom_path, args)
        None -> bidir_runner.start(args)
      }
    }
  }

  case runner_result {
    Error(err) -> Error(err)
    Ok(runner) -> {
      // Build StartConfig from BidirOptions using default_config + record update
      let default_timeout = case bidir_opts.timeout_ms {
        Some(ms) -> ms
        None -> 60_000
      }

      // Spawn the bridge process which will:
      // 1. Create its own subscriber subject (which it owns)
      // 2. Start the actor with that subscriber
      // 3. Forward messages to the public subjects
      let _ =
        process.spawn_unlinked(fn() {
          start_bridge_and_actor(
            runner,
            default_timeout,
            bidir_opts,
            messages,
            events,
            setup_subject,
          )
        })

      // Wait for the bridge to report back with the actor subject.
      // Use default_timeout for consistency with user-configured timeout.
      case process.receive(setup_subject, default_timeout) {
        Ok(SetupOk(actor_subject, subscriber)) -> {
          Ok(session.new(actor_subject, messages, events, subscriber))
        }
        Ok(SetupFailed(err)) -> Error(err)
        Error(Nil) -> Error(error.Timeout)
      }
    }
  }
}

/// Internal type for bridge setup coordination.
type SetupResult {
  SetupOk(Subject(actor.ActorMessage), Subject(actor.SubscriberMessage))
  SetupFailed(StartError)
}

/// Bridge process entry point: creates subscriber, starts actor, then loops.
fn start_bridge_and_actor(
  runner: bidir_runner.BidirRunner,
  default_timeout: Int,
  bidir_opts: BidirOptions,
  messages: Subject(message.Message),
  events: Subject(event.SessionEvent),
  setup_subject: Subject(SetupResult),
) -> Nil {
  // Create subscriber subject - this process owns it and can receive from it
  let subscriber = process.new_subject()

  let config =
    actor.StartConfig(
      ..bidir.default_config(subscriber),
      default_timeout_ms: default_timeout,
      hook_timeouts: bidir_opts.hook_timeouts,
      enable_file_checkpointing: bidir_opts.file_checkpointing_enabled,
      mcp_servers: bidir_opts.mcp_servers,
    )

  // Start the actor
  case bidir.start(runner, config) {
    Ok(actor_subject) -> {
      // Report success back to parent
      process.send(setup_subject, SetupOk(actor_subject, subscriber))
      // Now loop forwarding messages
      subscriber_bridge_loop(subscriber, messages, events)
    }
    Error(err) -> {
      // Report failure back to parent
      process.send(setup_subject, SetupFailed(err))
    }
  }
}

/// Bridge loop: receive SubscriberMessage and forward to appropriate subject.
fn subscriber_bridge_loop(
  subscriber: Subject(actor.SubscriberMessage),
  messages: Subject(message.Message),
  events: Subject(event.SessionEvent),
) -> Nil {
  // Block waiting for next message from actor's subscriber
  case process.receive_forever(subscriber) {
    actor.CliMessage(payload) -> {
      // The payload is a Message that was converted to Dynamic via identity.
      // Cast it back to Message (safe because Gleam types are erased at runtime).
      let msg: message.Message = unsafe_coerce_dynamic(payload)
      process.send(messages, msg)
      subscriber_bridge_loop(subscriber, messages, events)
    }
    actor.SessionEnded(stop_reason) -> {
      // Convert StopReason to SessionEvent and forward to events subject
      let session_event = case stop_reason {
        actor.UserRequested -> event.SessionStopped
        actor.CliExited(code) ->
          event.SessionFailed("CLI exited with code " <> string.inspect(code))
        actor.InitFailed(err) ->
          event.SessionFailed("Init failed: " <> string.inspect(err))
      }
      process.send(events, session_event)
      // SessionEnded is terminal - stop the bridge
      Nil
    }
  }
}

/// FFI: Unsafe coerce Dynamic back to its original type (identity at runtime).
/// Used to reverse the to_dynamic() call in the actor.
@external(erlang, "gleam_stdlib", "identity")
fn unsafe_coerce_dynamic(a: dynamic.Dynamic) -> b

/// Start a bidirectional session with Claude CLI (legacy API).
///
/// **DEPRECATED**: Use `start_session_new(cli_opts, sdk_opts, bidir_opts)`.
///
/// Note: The prompt parameter is ignored in bidirectional mode as messages
/// are sent through the session interface.
pub fn start_session(
  _prompt: String,
  query_options: QueryOptions,
) -> Result(Session, StartError) {
  let cli_opts = options.cli_options_from_query(query_options)
  let sdk_opts = options.sdk_options_from_query(query_options)
  let bidir_opts = options.bidir_options_from_query(query_options)
  start_session_new(cli_opts, sdk_opts, bidir_opts)
}

// =============================================================================
// Control Operations (Bidirectional Mode)
// =============================================================================

/// Send interrupt signal to stop current operation.
///
/// Blocks up to 5s waiting for CLI acknowledgment.
///
/// ## Parameters
///
/// - `session`: The session handle from `start_session()`
///
/// ## Returns
///
/// - `Ok(Nil)`: Interrupt was acknowledged
/// - `Error(ControlTimeout)`: CLI did not acknowledge within 5s
/// - `Error(ControlSessionClosed)`: Session is closed
/// - `Error(ControlRejected)`: CLI rejected the interrupt request
pub fn interrupt(sess: Session) -> Result(Nil, ControlError) {
  let actor_subject = session.get_actor(sess)
  case bidir.interrupt(actor_subject) {
    Ok(Nil) -> Ok(Nil)
    Error(actor.CliError(message)) ->
      Error(error.ControlRejected("interrupt", message))
    Error(actor.InterruptTimeout) -> Error(error.ControlTimeout)
    Error(actor.SessionStopped) -> Error(error.ControlSessionClosed)
  }
}

/// Change permission mode during session.
///
/// Blocks up to 5s waiting for CLI acknowledgment.
///
/// ## Parameters
///
/// - `session`: The session handle from `start_session()`
/// - `mode`: The new permission mode to set
///
/// ## Returns
///
/// - `Ok(Nil)`: Permission mode was changed
/// - `Error(ControlTimeout)`: CLI did not acknowledge within 5s
/// - `Error(ControlSessionClosed)`: Session is closed
/// - `Error(ControlRejected)`: CLI rejected the permission mode change
pub fn set_permission_mode(
  sess: Session,
  mode: PermissionMode,
) -> Result(Nil, ControlError) {
  let actor_subject = session.get_actor(sess)
  // Convert options.PermissionMode to control.PermissionMode
  let control_mode = case mode {
    options.Default -> control.Default
    options.AcceptEdits -> control.AcceptEdits
    options.BypassPermissions -> control.BypassPermissions
    options.Plan -> control.Plan
  }
  case bidir.set_permission_mode(actor_subject, control_mode) {
    Ok(Nil) -> Ok(Nil)
    Error(actor.SetPermissionModeCliError(message)) ->
      Error(error.ControlRejected("set_permission_mode", message))
    Error(actor.SetPermissionModeTimeout) -> Error(error.ControlTimeout)
    Error(actor.SetPermissionModeSessionStopped) ->
      Error(error.ControlSessionClosed)
  }
}

/// Change model during session.
///
/// Blocks up to 5s waiting for CLI acknowledgment.
///
/// ## Parameters
///
/// - `session`: The session handle from `start_session()`
/// - `model`: The model identifier to switch to
///
/// ## Returns
///
/// - `Ok(Nil)`: Model was changed
/// - `Error(ControlTimeout)`: CLI did not acknowledge within 5s
/// - `Error(ControlSessionClosed)`: Session is closed
/// - `Error(ControlRejected)`: CLI rejected the model change
pub fn set_model(sess: Session, model: String) -> Result(Nil, ControlError) {
  let actor_subject = session.get_actor(sess)
  case bidir.set_model(actor_subject, model) {
    Ok(Nil) -> Ok(Nil)
    Error(actor.SetModelCliError(message)) ->
      Error(error.ControlRejected("set_model", message))
    Error(actor.SetModelTimeout) -> Error(error.ControlTimeout)
    Error(actor.SetModelSessionStopped) -> Error(error.ControlSessionClosed)
  }
}

/// Rewind files to checkpoint at specified message.
///
/// Requires `enable_file_checkpointing` option to be set when starting the session.
/// Blocks up to 30s waiting for CLI acknowledgment.
///
/// ## Parameters
///
/// - `session`: The session handle from `start_session()`
/// - `user_message_id`: The ID of the user message to rewind to
///
/// ## Returns
///
/// - `Ok(Nil)`: Files were rewound to checkpoint
/// - `Error(ControlTimeout)`: CLI did not acknowledge within 30s
/// - `Error(ControlSessionClosed)`: Session is closed
/// - `Error(ControlCheckpointingNotEnabled)`: File checkpointing was not enabled
/// - `Error(ControlRejected)`: CLI rejected the rewind request
pub fn rewind_files(
  sess: Session,
  user_message_id: String,
) -> Result(Nil, ControlError) {
  let actor_subject = session.get_actor(sess)
  // Use default timeout (30s) for rewind_files
  case bidir.rewind_files(actor_subject, user_message_id, 30_000) {
    Ok(Nil) -> Ok(Nil)
    Error(actor.CheckpointingNotEnabled) ->
      Error(error.ControlCheckpointingNotEnabled)
    Error(actor.RewindFilesCliError(message)) ->
      Error(error.ControlRejected("rewind_files", message))
    Error(actor.RewindFilesTimeout) -> Error(error.ControlTimeout)
    Error(actor.RewindFilesSessionStopped) -> Error(error.ControlSessionClosed)
  }
}

/// Gracefully stop the session.
///
/// Non-blocking; initiates shutdown and returns immediately.
///
/// ## Current Status: Skeleton (TDD Phase 1)
///
/// This function currently returns `Error(StopNotImplemented)` as a placeholder.
/// The actual implementation will be added in Epic 8.
///
/// ## Parameters
///
/// - `session`: The session handle from `start_session()`
///
/// ## Returns
///
/// - `Ok(Nil)`: Shutdown initiated
/// - `Error(StopSessionClosed)`: Session is already closed
/// - `Error(StopNotImplemented)`: Not yet implemented (current)
pub fn stop(_session: Session) -> Result(Nil, StopError) {
  Error(error.StopNotImplemented)
}

// =============================================================================
// Session Subscriptions (Bidirectional Mode)
// =============================================================================

/// Get the message stream from an active session.
///
/// Non-blocking; returns immediately with Subject for receiving messages.
/// The GenServer pushes Message values to this Subject as they arrive.
///
/// ## Parameters
///
/// - `session`: The session handle from `start_session()`
///
/// ## Returns
///
/// - `Subject(Message)`: Subject for receiving message pushes
///
/// ## Example
///
/// ```gleam
/// import gleam/erlang/process
/// import claude_agent_sdk
///
/// let messages_subject = claude_agent_sdk.messages(session)
///
/// // Receive messages with timeout
/// case process.receive(messages_subject, 5000) {
///   Ok(message) -> handle_message(message)
///   Error(Nil) -> handle_timeout()
/// }
/// ```
pub fn messages(session: Session) -> Subject(Message) {
  session.get_messages(session)
}

/// Get session lifecycle events (completion, stop, failure).
///
/// Non-blocking; returns immediately with Subject for receiving events.
/// Separate from messages() to avoid changing the Message type.
///
/// ## Parameters
///
/// - `session`: The session handle from `start_session()`
///
/// ## Returns
///
/// - `Subject(SessionEvent)`: Subject for receiving lifecycle events
///
/// ## Example
///
/// ```gleam
/// import gleam/erlang/process
/// import claude_agent_sdk
/// import claude_agent_sdk/event
///
/// let events_subject = claude_agent_sdk.events(session)
///
/// // Receive events with timeout
/// case process.receive(events_subject, 5000) {
///   Ok(event.SessionCompleted(result)) -> handle_complete(result)
///   Ok(event.SessionStopped) -> handle_stop()
///   Ok(event.SessionFailed(error)) -> handle_error(error)
///   Error(Nil) -> handle_timeout()
/// }
/// ```
pub fn events(session: Session) -> Subject(SessionEvent) {
  session.get_events(session)
}
