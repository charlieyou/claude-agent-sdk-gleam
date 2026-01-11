/// Query options for configuring Claude CLI invocations.
///
/// This module provides the QueryOptions type and builder functions for
/// constructing CLI arguments. All option fields default to None (or False
/// for boolean fields), meaning the CLI uses its own defaults.
import claude_agent_sdk/runner.{type Runner}
import gleam/option.{type Option, None, Some}

/// Permission mode for controlling tool execution behavior.
pub type PermissionMode {
  /// Default behavior - prompts for permission
  Default
  /// Automatically accept file edits
  AcceptEdits
  /// Skip all permission prompts (dangerous)
  BypassPermissions
  /// Plan mode - read-only exploration
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
/// All Option fields are None, all Bool fields are False.
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

/// Set the model to use (e.g., "opus", "sonnet").
pub fn with_model(options: QueryOptions, model: String) -> QueryOptions {
  QueryOptions(..options, model: Some(model))
}

/// Set maximum number of agent turns.
pub fn with_max_turns(options: QueryOptions, n: Int) -> QueryOptions {
  QueryOptions(..options, max_turns: Some(n))
}

/// Set maximum budget in USD.
pub fn with_max_budget(options: QueryOptions, usd: Float) -> QueryOptions {
  QueryOptions(..options, max_budget_usd: Some(usd))
}

/// Set the system prompt (replaces default).
/// Takes precedence over append_system_prompt.
pub fn with_system_prompt(options: QueryOptions, prompt: String) -> QueryOptions {
  QueryOptions(..options, system_prompt: Some(prompt))
}

/// Append to the default system prompt.
/// Ignored if system_prompt is set.
pub fn with_append_system_prompt(
  options: QueryOptions,
  prompt: String,
) -> QueryOptions {
  QueryOptions(..options, append_system_prompt: Some(prompt))
}

/// Set allowed tools (whitelist).
/// Takes precedence over disallowed_tools.
pub fn with_allowed_tools(
  options: QueryOptions,
  tools: List(String),
) -> QueryOptions {
  QueryOptions(..options, allowed_tools: Some(tools))
}

/// Set disallowed tools (blacklist).
/// Ignored if allowed_tools is set.
pub fn with_disallowed_tools(
  options: QueryOptions,
  tools: List(String),
) -> QueryOptions {
  QueryOptions(..options, disallowed_tools: Some(tools))
}

/// Set path to MCP configuration file.
pub fn with_mcp_config(options: QueryOptions, path: String) -> QueryOptions {
  QueryOptions(..options, mcp_config_path: Some(path))
}

/// Set permission mode for tool execution.
pub fn with_permission_mode(
  options: QueryOptions,
  mode: PermissionMode,
) -> QueryOptions {
  QueryOptions(..options, permission_mode: Some(mode))
}

/// Resume a specific session by ID.
/// Takes precedence over continue_session.
pub fn with_resume(options: QueryOptions, session_id: String) -> QueryOptions {
  QueryOptions(..options, resume_session_id: Some(session_id))
}

/// Continue the most recent session.
/// Ignored if resume_session_id is set.
pub fn with_continue(options: QueryOptions) -> QueryOptions {
  QueryOptions(..options, continue_session: True)
}

/// Set working directory for the CLI process.
pub fn with_cwd(options: QueryOptions, path: String) -> QueryOptions {
  QueryOptions(..options, cwd: Some(path))
}

// =============================================================================
// SDK Option Builders
// =============================================================================

/// Enable test mode with a mock runner.
/// When test_mode is True, the SDK uses the provided Runner
/// instead of spawning the actual Claude CLI.
pub fn with_test_mode(options: QueryOptions, runner: Runner) -> QueryOptions {
  QueryOptions(..options, test_mode: True, test_runner: Some(runner))
}

/// Skip CLI version check entirely.
/// Use with caution - incompatible CLI versions may cause runtime errors.
pub fn with_skip_version_check(options: QueryOptions) -> QueryOptions {
  QueryOptions(..options, skip_version_check: True)
}

/// Allow unknown CLI versions with a warning instead of failing.
/// Less strict than skip_version_check but still provides some protection.
pub fn with_permissive_version_check(options: QueryOptions) -> QueryOptions {
  QueryOptions(..options, permissive_version_check: True)
}
