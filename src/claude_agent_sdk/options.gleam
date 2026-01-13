/// Query options for configuring Claude CLI invocations.
///
/// This module provides the `QueryOptions` type and builder functions for
/// configuring how the SDK invokes the Claude CLI. Use `default_options()`
/// to create a base configuration, then chain builder functions to customize.
///
/// ## Quick Start
///
/// ```gleam
/// import claude_agent_sdk/options
///
/// let opts = options.default_options()
///   |> options.with_model("sonnet")
///   |> options.with_max_turns(5)
///   |> options.with_permission_mode(options.AcceptEdits)
/// ```
///
/// ## Option Categories
///
/// **CLI Options** (passed to the Claude CLI):
/// - `model`, `max_turns`, `max_budget_usd`: Control model and limits
/// - `system_prompt`, `append_system_prompt`: Customize system instructions
/// - `allowed_tools`, `disallowed_tools`: Tool access control
/// - `mcp_config_path`: MCP server configuration
/// - `permission_mode`: Tool execution permission handling
/// - `resume_session_id`, `continue_session`: Session management
/// - `cwd`: Working directory for the CLI process
///
/// **SDK Options** (control SDK behavior, not passed to CLI):
/// - `test_mode`, `test_runner`: Enable mock testing
/// - `skip_version_check`, `permissive_version_check`: Version validation control
///
/// ## Defaults
///
/// All `Option` fields default to `None`, all `Bool` fields default to `False`.
/// The CLI uses its own defaults when options are unset.
import claude_agent_sdk/hook.{
  type CanUseToolContext, type HookEvent, type HookExecutionResult,
  type PermissionCheckResult, type PostToolUseContext, type PreCompactContext,
  type PreToolUseContext, type StopContext, type SubagentStopContext,
  type UserPromptSubmitContext,
}
import claude_agent_sdk/internal/bidir_runner.{type BidirRunner}
import claude_agent_sdk/runner.{type Runner}
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/option.{type Option, None, Some}

/// Permission mode for controlling tool execution behavior.
///
/// Controls how the CLI handles permission prompts for tool execution.
/// This maps directly to the `--permission-mode` CLI flag.
///
/// ## Security Considerations
///
/// `BypassPermissions` grants full access to all tools without user confirmation.
/// Use only in trusted environments (CI, containers with limited access, etc.).
pub type PermissionMode {
  /// Default behavior - prompts for permission interactively
  Default
  /// Automatically accept file edits (maps to `--dangerously-skip-permissions`)
  AcceptEdits
  /// Skip all permission prompts (maps to `--dangerously-skip-permissions`)
  BypassPermissions
  /// Plan mode - read-only exploration (maps to `--permission-mode plan`)
  Plan
}

/// Query options for configuring Claude CLI invocations.
///
/// ## cwd handling rules
///
/// | cwd value | Port option | Behavior |
/// |-----------|-------------|----------|
/// | `None` | Omit `{cd, ...}` | Inherit current working directory |
/// | `Some("")` | Treated as `None` | Empty string = inherit cwd |
/// | `Some(path)` | `{cd, path}` | Change to specified directory |
pub type QueryOptions {
  QueryOptions(
    // --- CLI options ---
    model: Option(String),
    max_turns: Option(Int),
    max_budget_usd: Option(Float),
    system_prompt: Option(String),
    append_system_prompt: Option(String),
    allowed_tools: Option(List(String)),
    disallowed_tools: Option(List(String)),
    mcp_config_path: Option(String),
    permission_mode: Option(PermissionMode),
    resume_session_id: Option(String),
    continue_session: Bool,
    cwd: Option(String),
    // --- SDK options (not passed to CLI) ---
    test_mode: Bool,
    test_runner: Option(Runner),
    skip_version_check: Bool,
    permissive_version_check: Bool,
    // --- Hook callbacks (for bidirectional mode) ---
    on_pre_tool_use: Option(fn(PreToolUseContext) -> HookExecutionResult),
    on_post_tool_use: Option(fn(PostToolUseContext) -> HookExecutionResult),
    on_user_prompt_submit: Option(
      fn(UserPromptSubmitContext) -> HookExecutionResult,
    ),
    on_stop: Option(fn(StopContext) -> HookExecutionResult),
    on_subagent_stop: Option(fn(SubagentStopContext) -> HookExecutionResult),
    on_pre_compact: Option(fn(PreCompactContext) -> HookExecutionResult),
    on_can_use_tool: Option(fn(CanUseToolContext) -> PermissionCheckResult),
    // --- MCP server configuration (for bidirectional mode) ---
    /// List of MCP servers configured via with_mcp_server. Each entry is (name, handler).
    mcp_servers: List(#(String, fn(dynamic.Dynamic) -> dynamic.Dynamic)),
    // --- File checkpointing configuration ---
    /// Whether file checkpointing is enabled for rewind_files support.
    file_checkpointing_enabled: Bool,
    // --- Timeout configuration ---
    /// Global timeout in milliseconds for operations. Applied in GenServer.
    timeout_ms: Result(Int, Nil),
    /// Per-hook timeout overrides. Keys are HookEvent, values are timeout in ms.
    hook_timeouts: Dict(HookEvent, Int),
    // --- Testing seam for bidirectional mode ---
    /// Factory function for creating BidirRunner. Used by start_session() tests.
    bidir_runner_factory: Result(fn() -> BidirRunner, Nil),
  )
}

/// Create default query options with all fields unset.
///
/// All `Option` fields are `None`, all `Bool` fields are `False`.
/// The CLI will use its own defaults for any unset options.
///
/// ## Example
///
/// ```gleam
/// // Minimal usage - CLI defaults
/// let opts = default_options()
///
/// // Customized options
/// let opts = default_options()
///   |> with_model("opus")
///   |> with_max_turns(10)
/// ```
pub fn default_options() -> QueryOptions {
  QueryOptions(
    model: None,
    max_turns: None,
    max_budget_usd: None,
    system_prompt: None,
    append_system_prompt: None,
    allowed_tools: None,
    disallowed_tools: None,
    mcp_config_path: None,
    permission_mode: None,
    resume_session_id: None,
    continue_session: False,
    cwd: None,
    test_mode: False,
    test_runner: None,
    skip_version_check: False,
    permissive_version_check: False,
    on_pre_tool_use: None,
    on_post_tool_use: None,
    on_user_prompt_submit: None,
    on_stop: None,
    on_subagent_stop: None,
    on_pre_compact: None,
    on_can_use_tool: None,
    mcp_servers: [],
    file_checkpointing_enabled: False,
    timeout_ms: Error(Nil),
    hook_timeouts: dict.new(),
    bidir_runner_factory: Error(Nil),
  )
}

// =============================================================================
// CLI Option Builders
// =============================================================================

/// Set the model to use.
///
/// ## Parameters
///
/// - `model`: Model identifier (e.g., "opus", "sonnet", "haiku")
///
/// Maps to CLI flag: `--model`
pub fn with_model(options: QueryOptions, model: String) -> QueryOptions {
  QueryOptions(..options, model: Some(model))
}

/// Set maximum number of agent turns.
///
/// A "turn" is one round of Claude responding to the prompt. Use this to limit
/// how many tool-use cycles Claude can perform.
///
/// ## Parameters
///
/// - `n`: Maximum number of turns (positive integer)
///
/// Maps to CLI flag: `--max-turns`
pub fn with_max_turns(options: QueryOptions, n: Int) -> QueryOptions {
  QueryOptions(..options, max_turns: Some(n))
}

/// Set maximum budget in USD.
///
/// Limits total API cost for this query. The query will stop if the budget
/// is exceeded, returning a `ResultSubtype.ErrorMaxBudget` result.
///
/// ## Parameters
///
/// - `usd`: Maximum cost in US dollars (positive float)
///
/// Maps to CLI flag: `--max-budget-usd`
pub fn with_max_budget(options: QueryOptions, usd: Float) -> QueryOptions {
  QueryOptions(..options, max_budget_usd: Some(usd))
}

/// Set the system prompt (replaces default).
///
/// Completely replaces the default Claude Code system prompt. Use this when
/// you need full control over Claude's instructions.
///
/// **Note**: Takes precedence over `append_system_prompt` if both are set.
///
/// ## Parameters
///
/// - `prompt`: The complete system prompt text
///
/// Maps to CLI flag: `--system-prompt`
///
/// See also: `with_append_system_prompt()` to add to the default prompt
pub fn with_system_prompt(options: QueryOptions, prompt: String) -> QueryOptions {
  QueryOptions(..options, system_prompt: Some(prompt))
}

/// Append to the default system prompt.
///
/// Adds text after the default Claude Code system prompt. Useful for adding
/// project-specific instructions while keeping the base capabilities.
///
/// **Note**: Ignored if `system_prompt` is set.
///
/// ## Parameters
///
/// - `prompt`: Text to append to the default system prompt
///
/// Maps to CLI flag: `--append-system-prompt`
///
/// See also: `with_system_prompt()` to completely replace the default
pub fn with_append_system_prompt(
  options: QueryOptions,
  prompt: String,
) -> QueryOptions {
  QueryOptions(..options, append_system_prompt: Some(prompt))
}

/// Set allowed tools (whitelist).
///
/// Restricts Claude to only use the specified tools. All other tools are blocked.
///
/// **Note**: Takes precedence over `disallowed_tools` if both are set.
///
/// ## Parameters
///
/// - `tools`: List of tool names to allow (e.g., `["Read", "Grep", "Bash"]`)
///
/// Maps to CLI flag: `--allowed-tools`
///
/// See also: `with_disallowed_tools()` for a blacklist approach
pub fn with_allowed_tools(
  options: QueryOptions,
  tools: List(String),
) -> QueryOptions {
  QueryOptions(..options, allowed_tools: Some(tools))
}

/// Set disallowed tools (blacklist).
///
/// Blocks Claude from using the specified tools. All other tools remain available.
///
/// **Note**: Ignored if `allowed_tools` is set.
///
/// ## Parameters
///
/// - `tools`: List of tool names to block (e.g., `["Bash", "Write"]`)
///
/// Maps to CLI flag: `--disallowed-tools`
///
/// See also: `with_allowed_tools()` for a whitelist approach
pub fn with_disallowed_tools(
  options: QueryOptions,
  tools: List(String),
) -> QueryOptions {
  QueryOptions(..options, disallowed_tools: Some(tools))
}

/// Set path to MCP configuration file.
///
/// The MCP (Model Context Protocol) config file specifies external tool servers
/// that Claude can connect to.
///
/// ## Parameters
///
/// - `path`: Absolute or relative path to the MCP config JSON file
///
/// Maps to CLI flag: `--mcp-config`
pub fn with_mcp_config(options: QueryOptions, path: String) -> QueryOptions {
  QueryOptions(..options, mcp_config_path: Some(path))
}

/// Set permission mode for tool execution.
///
/// Controls how Claude handles permission prompts when using tools.
///
/// ## Parameters
///
/// - `mode`: One of `Default`, `AcceptEdits`, `BypassPermissions`, or `Plan`
///
/// See `PermissionMode` for detailed descriptions of each mode.
pub fn with_permission_mode(
  options: QueryOptions,
  mode: PermissionMode,
) -> QueryOptions {
  QueryOptions(..options, permission_mode: Some(mode))
}

/// Resume a specific session by ID.
///
/// Continues a previous conversation session, maintaining context from
/// earlier interactions.
///
/// **Note**: Takes precedence over `continue_session` if both are set.
///
/// ## Parameters
///
/// - `session_id`: UUID of the session to resume (from `SystemMessage.session_id`)
///
/// Maps to CLI flag: `--resume`
///
/// See also: `with_continue()` to resume the most recent session
pub fn with_resume(options: QueryOptions, session_id: String) -> QueryOptions {
  QueryOptions(..options, resume_session_id: Some(session_id))
}

/// Continue the most recent session.
///
/// Resumes the last active session without needing to know its ID.
///
/// **Note**: Ignored if `resume_session_id` is set.
///
/// Maps to CLI flag: `--continue`
///
/// See also: `with_resume()` to resume a specific session by ID
pub fn with_continue(options: QueryOptions) -> QueryOptions {
  QueryOptions(..options, continue_session: True)
}

/// Set working directory for the CLI process.
///
/// The CLI will execute in this directory, affecting relative paths in
/// file operations and tool execution.
///
/// ## Parameters
///
/// - `path`: Absolute path to the working directory
///
/// ## Behavior
///
/// - `None` or `Some("")`: Inherit current working directory
/// - `Some(path)`: Change to specified directory before execution
pub fn with_cwd(options: QueryOptions, path: String) -> QueryOptions {
  QueryOptions(..options, cwd: Some(path))
}

// =============================================================================
// SDK Option Builders
// =============================================================================

/// Enable test mode with a mock runner.
///
/// When enabled, the SDK uses the provided `Runner` instead of spawning the
/// actual Claude CLI. This allows unit testing stream semantics without
/// external dependencies.
///
/// ## Parameters
///
/// - `runner`: A `Runner` created via `runner.test_runner()` with mock callbacks
///
/// ## Example
///
/// ```gleam
/// import claude_agent_sdk/runner
///
/// let mock_runner = runner.test_runner(
///   on_spawn: fn(_, _, _) { Ok(dynamic.from(Nil)) },
///   on_read: fn(_) { runner.ExitStatus(0) },
///   on_close: fn(_) { Nil },
/// )
/// let opts = default_options() |> with_test_mode(mock_runner)
/// ```
///
/// See `claude_agent_sdk/runner` for detailed test runner documentation.
pub fn with_test_mode(options: QueryOptions, runner: Runner) -> QueryOptions {
  QueryOptions(..options, test_mode: True, test_runner: Some(runner))
}

/// Skip CLI version check entirely.
///
/// Bypasses all version detection and validation. The SDK will not run
/// `claude --version` or check compatibility.
///
/// **Use with caution**: Incompatible CLI versions may cause runtime errors,
/// malformed output, or unexpected behavior.
///
/// See also: `with_permissive_version_check()` for a less strict option
pub fn with_skip_version_check(options: QueryOptions) -> QueryOptions {
  QueryOptions(..options, skip_version_check: True)
}

/// Allow unknown CLI versions with a warning instead of failing.
///
/// If the CLI version cannot be parsed or is unrecognized, the SDK will
/// proceed with a warning instead of returning an `UnknownVersionError`.
///
/// This is less strict than `skip_version_check` but still verifies that
/// recognized versions meet the minimum requirement.
///
/// See also: `with_skip_version_check()` to skip version checks entirely
pub fn with_permissive_version_check(options: QueryOptions) -> QueryOptions {
  QueryOptions(..options, permissive_version_check: True)
}

// =============================================================================
// Hook Callback Builders
// =============================================================================

/// Set a pre-tool-use hook callback.
///
/// Called before each tool is executed. Return `Continue` to proceed,
/// `Block(reason)` to prevent execution, or `ModifyInput(new_input)`
/// to modify the tool input before execution.
///
/// ## Parameters
///
/// - `callback`: Function receiving `PreToolUseContext` and returning `HookExecutionResult`
///
/// ## Example
///
/// ```gleam
/// let opts = default_options()
///   |> with_pre_tool_use(fn(ctx) {
///     case ctx.tool_name {
///       "Bash" -> hook.Block("Bash disabled")
///       _ -> hook.Continue
///     }
///   })
/// ```
pub fn with_pre_tool_use(
  options: QueryOptions,
  callback: fn(PreToolUseContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_pre_tool_use: Some(callback))
}

/// Set a post-tool-use hook callback.
///
/// Called after each tool completes execution. Return `Continue` to proceed,
/// `Block(reason)` to halt, or `ModifyInput(new_output)` to modify the output.
///
/// ## Parameters
///
/// - `callback`: Function receiving `PostToolUseContext` and returning `HookExecutionResult`
///
/// ## Example
///
/// ```gleam
/// let opts = default_options()
///   |> with_post_tool_use(fn(ctx) {
///     // Log all tool executions
///     io.println("Tool " <> ctx.tool_name <> " completed")
///     hook.Continue
///   })
/// ```
pub fn with_post_tool_use(
  options: QueryOptions,
  callback: fn(PostToolUseContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_post_tool_use: Some(callback))
}

/// Set a user-prompt-submit hook callback.
///
/// Called when user submits a prompt. Return `Continue` to proceed,
/// `Block(reason)` to reject the prompt, or `ModifyInput(new_prompt)`
/// to modify the prompt before processing.
///
/// ## Parameters
///
/// - `callback`: Function receiving `UserPromptSubmitContext` and returning `HookExecutionResult`
///
/// ## Example
///
/// ```gleam
/// let opts = default_options()
///   |> with_user_prompt_submit(fn(ctx) {
///     case string.contains(ctx.prompt, "secret") {
///       True -> hook.Block("Prompt contains sensitive content")
///       False -> hook.Continue
///     }
///   })
/// ```
pub fn with_user_prompt_submit(
  options: QueryOptions,
  callback: fn(UserPromptSubmitContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_user_prompt_submit: Some(callback))
}

/// Set a stop hook callback.
///
/// Called when the session stops. Return `Continue` to allow stop,
/// or `Block(reason)` to log the stop reason (stop still proceeds).
///
/// ## Parameters
///
/// - `callback`: Function receiving `StopContext` and returning `HookExecutionResult`
///
/// ## Example
///
/// ```gleam
/// let opts = default_options()
///   |> with_stop(fn(ctx) {
///     io.println("Session stopped: " <> ctx.reason)
///     hook.Continue
///   })
/// ```
pub fn with_stop(
  options: QueryOptions,
  callback: fn(StopContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_stop: Some(callback))
}

/// Set a subagent-stop hook callback.
///
/// Called when a subagent stops. Return `Continue` to proceed,
/// or `Block(reason)` to log the stop event.
///
/// ## Parameters
///
/// - `callback`: Function receiving `SubagentStopContext` and returning `HookExecutionResult`
///
/// ## Example
///
/// ```gleam
/// let opts = default_options()
///   |> with_subagent_stop(fn(ctx) {
///     io.println("Subagent " <> ctx.subagent_id <> " stopped")
///     hook.Continue
///   })
/// ```
pub fn with_subagent_stop(
  options: QueryOptions,
  callback: fn(SubagentStopContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_subagent_stop: Some(callback))
}

/// Set a pre-compact hook callback.
///
/// Called before context compaction occurs. Return `Continue` to proceed,
/// or `Block(reason)` to prevent compaction.
///
/// ## Parameters
///
/// - `callback`: Function receiving `PreCompactContext` and returning `HookExecutionResult`
///
/// ## Example
///
/// ```gleam
/// let opts = default_options()
///   |> with_pre_compact(fn(ctx) {
///     io.println("Compacting session: " <> ctx.session_id)
///     hook.Continue
///   })
/// ```
pub fn with_pre_compact(
  options: QueryOptions,
  callback: fn(PreCompactContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_pre_compact: Some(callback))
}

/// Set a can-use-tool permission check callback.
///
/// Called to check if a tool can be used. Return `Allow` to permit,
/// or `Deny(reason)` to block tool usage.
///
/// ## Parameters
///
/// - `callback`: Function receiving `CanUseToolContext` and returning `PermissionCheckResult`
///
/// ## Example
///
/// ```gleam
/// let opts = default_options()
///   |> with_can_use_tool(fn(ctx) {
///     case ctx.tool_name {
///       "Write" -> hook.Deny("Write access disabled")
///       _ -> hook.Allow
///     }
///   })
/// ```
pub fn with_can_use_tool(
  options: QueryOptions,
  callback: fn(CanUseToolContext) -> PermissionCheckResult,
) -> QueryOptions {
  QueryOptions(..options, on_can_use_tool: Some(callback))
}

// =============================================================================
// MCP Server Builders
// =============================================================================

/// Add an MCP server handler.
///
/// Adds an MCP server to the list of SDK-hosted MCP servers. Multiple calls
/// accumulate servers in the list. The handler receives MCP requests and
/// returns responses.
///
/// ## Parameters
///
/// - `name`: Unique name for the MCP server
/// - `handler`: Function receiving MCP requests (Dynamic) and returning responses (Dynamic)
///
/// ## Example
///
/// ```gleam
/// let opts = default_options()
///   |> with_mcp_server("my-tools", fn(request) {
///     // Handle MCP request and return response
///     dynamic.from(Nil)
///   })
///   |> with_mcp_server("data-server", my_data_handler)
/// ```
pub fn with_mcp_server(
  options: QueryOptions,
  name: String,
  handler: fn(dynamic.Dynamic) -> dynamic.Dynamic,
) -> QueryOptions {
  let new_servers = [#(name, handler), ..options.mcp_servers]
  QueryOptions(..options, mcp_servers: new_servers)
}

// =============================================================================
// File Checkpointing Builder
// =============================================================================

/// Enable file checkpointing for rewind_files support.
///
/// When enabled, the CLI tracks file changes made during the session,
/// allowing them to be reverted using `rewind_files()`.
///
/// ## Example
///
/// ```gleam
/// let opts = default_options()
///   |> with_file_checkpointing()
/// ```
pub fn with_file_checkpointing(options: QueryOptions) -> QueryOptions {
  QueryOptions(..options, file_checkpointing_enabled: True)
}

// =============================================================================
// Timeout Configuration Builders
// =============================================================================

/// Set the global timeout in milliseconds.
///
/// This timeout is applied to all operations unless overridden by
/// a per-hook timeout via `with_hook_timeout`.
///
/// ## Parameters
///
/// - `timeout_ms`: Timeout in milliseconds (positive integer)
///
/// ## Example
///
/// ```gleam
/// let opts = default_options()
///   |> with_timeout(60_000)  // 60 seconds
/// ```
pub fn with_timeout(options: QueryOptions, timeout_ms: Int) -> QueryOptions {
  QueryOptions(..options, timeout_ms: Ok(timeout_ms))
}

/// Set a per-hook timeout override.
///
/// Overrides the global timeout for a specific hook event type.
/// Multiple calls with different events accumulate; same event overwrites.
///
/// ## Parameters
///
/// - `event`: The hook event type to configure
/// - `timeout_ms`: Timeout in milliseconds for this hook type
///
/// ## Example
///
/// ```gleam
/// let opts = default_options()
///   |> with_timeout(60_000)  // 60 second global default
///   |> with_hook_timeout(hook.PreToolUse, 5000)  // 5 second override for PreToolUse
/// ```
pub fn with_hook_timeout(
  options: QueryOptions,
  event: HookEvent,
  timeout_ms: Int,
) -> QueryOptions {
  let new_timeouts = dict.insert(options.hook_timeouts, event, timeout_ms)
  QueryOptions(..options, hook_timeouts: new_timeouts)
}

/// Set a factory function for creating BidirRunner instances.
///
/// This is a testing seam for `start_session()`. When set, `start_session()`
/// uses this factory to create the BidirRunner instead of spawning a real
/// CLI process. This allows unit testing bidirectional sessions.
///
/// **Note**: Only used by `start_session()`. Ignored by `query()`.
///
/// ## Parameters
///
/// - `factory`: Function that returns a new BidirRunner instance
///
/// ## Example
///
/// ```gleam
/// import claude_agent_sdk/internal/bidir_runner
///
/// let mock_factory = fn() {
///   bidir_runner.mock(
///     on_write: fn(_) { Ok(Nil) },
///     on_close: fn() { Nil },
///   )
/// }
///
/// let opts = default_options()
///   |> with_bidir_runner_factory(mock_factory)
/// ```
pub fn with_bidir_runner_factory(
  options: QueryOptions,
  factory: fn() -> BidirRunner,
) -> QueryOptions {
  QueryOptions(..options, bidir_runner_factory: Ok(factory))
}
