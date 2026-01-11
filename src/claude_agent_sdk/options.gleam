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
import claude_agent_sdk/runner.{type Runner}
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
