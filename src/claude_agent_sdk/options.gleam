/// Query options for configuring Claude CLI invocations.
///
/// This module provides separate option types for different concerns:
/// - `CliOptions`: Options passed to the Claude CLI
/// - `SdkOptions`: Options controlling SDK behavior (not passed to CLI)
/// - `BidirOptions`: Options for bidirectional session mode
///
/// ## Quick Start
///
/// ```gleam
/// import claude_agent_sdk/options
///
/// // For query() - simple one-shot queries
/// let cli_opts = options.cli_options()
///   |> options.with_model("sonnet")
///   |> options.with_max_turns(5)
/// let sdk_opts = options.sdk_options()
///
/// // For start_session() - bidirectional mode
/// let bidir_opts = options.bidir_options()
///   |> options.with_pre_tool_use(my_hook)
/// ```
///
/// ## Option Categories
///
/// **CliOptions** (passed to the Claude CLI):
/// - `model`, `max_turns`, `max_budget_usd`: Control model and limits
/// - `system_prompt`, `append_system_prompt`: Customize system instructions
/// - `allowed_tools`, `disallowed_tools`: Tool access control
/// - `mcp_config_path`: MCP server configuration
/// - `permission_mode`: Tool execution permission handling
/// - `resume_session_id`, `continue_session`: Session management
/// - `cwd`: Working directory for the CLI process
///
/// **SdkOptions** (control SDK behavior, not passed to CLI):
/// - `test_mode`, `test_runner`: Enable mock testing
/// - `skip_version_check`, `permissive_version_check`: Version validation control
///
/// **BidirOptions** (bidirectional session mode):
/// - Hook callbacks: `on_pre_tool_use`, `on_post_tool_use`, etc.
/// - Permission handler: `on_can_use_tool`
/// - MCP servers: `mcp_servers`
/// - Timeouts: `timeout_ms`, `hook_timeouts`
/// - File checkpointing: `file_checkpointing_enabled`
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
import gleam/list
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
  /// Automatically accept file edits (maps to `--permission-mode acceptEdits`)
  AcceptEdits
  /// Skip all permission prompts (maps to `--dangerously-skip-permissions`)
  BypassPermissions
  /// Plan mode - read-only exploration (maps to `--permission-mode plan`)
  Plan
}

// =============================================================================
// CliOptions - Options passed to the Claude CLI
// =============================================================================

/// CLI options for configuring Claude CLI invocations.
///
/// These options are converted to CLI arguments when spawning the Claude process.
///
/// ## cwd handling rules
///
/// | cwd value | Port option | Behavior |
/// |-----------|-------------|----------|
/// | `None` | Omit `{cd, ...}` | Inherit current working directory |
/// | `Some("")` | Treated as `None` | Empty string = inherit cwd |
/// | `Some(path)` | `{cd, path}` | Change to specified directory |
pub type CliOptions {
  CliOptions(
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
    // New fields for Python SDK parity
    cli_path: Option(String),
    fallback_model: Option(String),
    betas: Option(List(String)),
    permission_prompt_tool_name: Option(String),
    settings: Option(Dict(String, dynamic.Dynamic)),
    add_dirs: Option(List(String)),
    env: Option(Dict(String, String)),
    extra_args: Option(List(String)),
  )
}

/// Create default CLI options with all fields unset.
pub fn cli_options() -> CliOptions {
  CliOptions(
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
    cli_path: None,
    fallback_model: None,
    betas: None,
    permission_prompt_tool_name: None,
    settings: None,
    add_dirs: None,
    env: None,
    extra_args: None,
  )
}

// =============================================================================
// SdkOptions - Options controlling SDK behavior
// =============================================================================

/// SDK options for controlling SDK behavior.
///
/// These options are NOT passed to the CLI - they control how the SDK
/// itself behaves (testing, version checks, etc.).
pub type SdkOptions {
  SdkOptions(
    test_mode: Bool,
    test_runner: Option(Runner),
    skip_version_check: Bool,
    permissive_version_check: Bool,
  )
}

/// Create default SDK options with all fields unset.
pub fn sdk_options() -> SdkOptions {
  SdkOptions(
    test_mode: False,
    test_runner: None,
    skip_version_check: False,
    permissive_version_check: False,
  )
}

// =============================================================================
// AgentConfig - Configuration for custom agents
// =============================================================================

/// Configuration for a custom agent (subagent).
///
/// Custom agents can be defined with a description and prompt that determines
/// their behavior when invoked via the Task tool.
pub type AgentConfig {
  AgentConfig(
    /// Name identifier for the agent
    name: String,
    /// Brief description of the agent's purpose
    description: String,
    /// System prompt defining the agent's behavior
    prompt: String,
  )
}

// =============================================================================
// SandboxConfig - Sandbox environment configuration
// =============================================================================

/// Configuration for sandbox execution environment.
///
/// Sandboxes provide isolated execution environments for code execution
/// and tool use, enhancing security for untrusted operations.
pub type SandboxConfig {
  SandboxConfig(
    /// Type of sandbox (e.g., "docker", "container", "none")
    sandbox_type: String,
    /// Additional sandbox-specific configuration as key-value pairs
    config: Dict(String, dynamic.Dynamic),
  )
}

// =============================================================================
// BidirOptions - Options for bidirectional session mode
// =============================================================================

/// Bidirectional session options.
///
/// These options configure hooks, permissions, MCP servers, and timeouts
/// for bidirectional session mode (`start_session()`).
pub type BidirOptions {
  BidirOptions(
    on_pre_tool_use: Option(fn(PreToolUseContext) -> HookExecutionResult),
    on_post_tool_use: Option(fn(PostToolUseContext) -> HookExecutionResult),
    on_user_prompt_submit: Option(
      fn(UserPromptSubmitContext) -> HookExecutionResult,
    ),
    on_stop: Option(fn(StopContext) -> HookExecutionResult),
    on_subagent_stop: Option(fn(SubagentStopContext) -> HookExecutionResult),
    on_pre_compact: Option(fn(PreCompactContext) -> HookExecutionResult),
    on_can_use_tool: Option(fn(CanUseToolContext) -> PermissionCheckResult),
    mcp_servers: List(#(String, fn(dynamic.Dynamic) -> dynamic.Dynamic)),
    file_checkpointing_enabled: Bool,
    timeout_ms: Option(Int),
    hook_timeouts: Dict(HookEvent, Int),
    bidir_runner_factory: Option(fn() -> BidirRunner),
    // New fields for Python SDK parity
    include_partial_messages: Bool,
    fork_session: Option(String),
    agents: Option(List(AgentConfig)),
    setting_sources: Option(List(String)),
    sandbox: Option(SandboxConfig),
    plugins: Option(List(String)),
    max_thinking_tokens: Option(Int),
    output_format: Option(String),
  )
}

/// Create default bidirectional options with all fields unset.
pub fn bidir_options() -> BidirOptions {
  BidirOptions(
    on_pre_tool_use: None,
    on_post_tool_use: None,
    on_user_prompt_submit: None,
    on_stop: None,
    on_subagent_stop: None,
    on_pre_compact: None,
    on_can_use_tool: None,
    mcp_servers: [],
    file_checkpointing_enabled: False,
    timeout_ms: None,
    hook_timeouts: dict.new(),
    bidir_runner_factory: None,
    include_partial_messages: False,
    fork_session: None,
    agents: None,
    setting_sources: None,
    sandbox: None,
    plugins: None,
    max_thinking_tokens: None,
    output_format: None,
  )
}

// =============================================================================
// Legacy QueryOptions - For backwards compatibility
// =============================================================================

/// Legacy query options combining all option categories.
///
/// **DEPRECATED**: Use `CliOptions`, `SdkOptions`, and `BidirOptions` separately.
/// This type is kept for backwards compatibility during migration.
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
    mcp_servers: List(#(String, fn(dynamic.Dynamic) -> dynamic.Dynamic)),
    // --- File checkpointing configuration ---
    file_checkpointing_enabled: Bool,
    // --- Timeout configuration ---
    timeout_ms: Option(Int),
    hook_timeouts: Dict(HookEvent, Int),
    // --- Testing seam for bidirectional mode ---
    bidir_runner_factory: Option(fn() -> BidirRunner),
  )
}

/// Create default query options with all fields unset.
///
/// **DEPRECATED**: Use `cli_options()`, `sdk_options()`, `bidir_options()`.
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
    timeout_ms: None,
    hook_timeouts: dict.new(),
    bidir_runner_factory: None,
  )
}

// =============================================================================
// Conversion Functions
// =============================================================================

/// Extract CliOptions from QueryOptions.
/// Note: New fields (cli_path, fallback_model, etc.) default to None as
/// QueryOptions predates their addition to CliOptions.
pub fn cli_options_from_query(opts: QueryOptions) -> CliOptions {
  CliOptions(
    model: opts.model,
    max_turns: opts.max_turns,
    max_budget_usd: opts.max_budget_usd,
    system_prompt: opts.system_prompt,
    append_system_prompt: opts.append_system_prompt,
    allowed_tools: opts.allowed_tools,
    disallowed_tools: opts.disallowed_tools,
    mcp_config_path: opts.mcp_config_path,
    permission_mode: opts.permission_mode,
    resume_session_id: opts.resume_session_id,
    continue_session: opts.continue_session,
    cwd: opts.cwd,
    // New fields default to None (QueryOptions predates these)
    cli_path: None,
    fallback_model: None,
    betas: None,
    permission_prompt_tool_name: None,
    settings: None,
    add_dirs: None,
    env: None,
    extra_args: None,
  )
}

/// Extract SdkOptions from QueryOptions.
pub fn sdk_options_from_query(opts: QueryOptions) -> SdkOptions {
  SdkOptions(
    test_mode: opts.test_mode,
    test_runner: opts.test_runner,
    skip_version_check: opts.skip_version_check,
    permissive_version_check: opts.permissive_version_check,
  )
}

/// Extract BidirOptions from QueryOptions.
pub fn bidir_options_from_query(opts: QueryOptions) -> BidirOptions {
  // Note: QueryOptions doesn't have the new bidir-specific fields,
  // so we use defaults for the Python SDK parity fields.
  BidirOptions(
    on_pre_tool_use: opts.on_pre_tool_use,
    on_post_tool_use: opts.on_post_tool_use,
    on_user_prompt_submit: opts.on_user_prompt_submit,
    on_stop: opts.on_stop,
    on_subagent_stop: opts.on_subagent_stop,
    on_pre_compact: opts.on_pre_compact,
    on_can_use_tool: opts.on_can_use_tool,
    mcp_servers: opts.mcp_servers,
    file_checkpointing_enabled: opts.file_checkpointing_enabled,
    timeout_ms: opts.timeout_ms,
    hook_timeouts: opts.hook_timeouts,
    bidir_runner_factory: opts.bidir_runner_factory,
    include_partial_messages: False,
    fork_session: None,
    agents: None,
    setting_sources: None,
    sandbox: None,
    plugins: None,
    max_thinking_tokens: None,
    output_format: None,
  )
}

/// Build QueryOptions from CliOptions and SdkOptions (for query()).
/// Used by the new API to create legacy QueryOptions for internal functions.
pub fn query_options_from_cli_sdk(
  cli: CliOptions,
  sdk: SdkOptions,
) -> QueryOptions {
  QueryOptions(
    model: cli.model,
    max_turns: cli.max_turns,
    max_budget_usd: cli.max_budget_usd,
    system_prompt: cli.system_prompt,
    append_system_prompt: cli.append_system_prompt,
    allowed_tools: cli.allowed_tools,
    disallowed_tools: cli.disallowed_tools,
    mcp_config_path: cli.mcp_config_path,
    permission_mode: cli.permission_mode,
    resume_session_id: cli.resume_session_id,
    continue_session: cli.continue_session,
    cwd: cli.cwd,
    test_mode: sdk.test_mode,
    test_runner: sdk.test_runner,
    skip_version_check: sdk.skip_version_check,
    permissive_version_check: sdk.permissive_version_check,
    // Bidir options are empty for query()
    on_pre_tool_use: None,
    on_post_tool_use: None,
    on_user_prompt_submit: None,
    on_stop: None,
    on_subagent_stop: None,
    on_pre_compact: None,
    on_can_use_tool: None,
    mcp_servers: [],
    file_checkpointing_enabled: False,
    timeout_ms: None,
    hook_timeouts: dict.new(),
    bidir_runner_factory: None,
  )
}

// =============================================================================
// CLI Option Builders
// =============================================================================

/// Set the model to use.
///
/// Maps to CLI flag: `--model`
pub fn with_model(options: CliOptions, model: String) -> CliOptions {
  CliOptions(..options, model: Some(model))
}

/// Set maximum number of agent turns.
///
/// Maps to CLI flag: `--max-turns`
pub fn with_max_turns(options: CliOptions, n: Int) -> CliOptions {
  CliOptions(..options, max_turns: Some(n))
}

/// Set maximum budget in USD.
///
/// Maps to CLI flag: `--max-budget-usd`
pub fn with_max_budget(options: CliOptions, usd: Float) -> CliOptions {
  CliOptions(..options, max_budget_usd: Some(usd))
}

/// Set the system prompt (replaces default).
///
/// Maps to CLI flag: `--system-prompt`
pub fn with_system_prompt(options: CliOptions, prompt: String) -> CliOptions {
  CliOptions(..options, system_prompt: Some(prompt))
}

/// Append to the default system prompt.
///
/// Maps to CLI flag: `--append-system-prompt`
pub fn with_append_system_prompt(
  options: CliOptions,
  prompt: String,
) -> CliOptions {
  CliOptions(..options, append_system_prompt: Some(prompt))
}

/// Set allowed tools (whitelist).
///
/// Maps to CLI flag: `--allowed-tools`
pub fn with_allowed_tools(
  options: CliOptions,
  tools: List(String),
) -> CliOptions {
  CliOptions(..options, allowed_tools: Some(tools))
}

/// Set disallowed tools (blacklist).
///
/// Maps to CLI flag: `--disallowed-tools`
pub fn with_disallowed_tools(
  options: CliOptions,
  tools: List(String),
) -> CliOptions {
  CliOptions(..options, disallowed_tools: Some(tools))
}

/// Set path to MCP configuration file.
///
/// Maps to CLI flag: `--mcp-config`
pub fn with_mcp_config(options: CliOptions, path: String) -> CliOptions {
  CliOptions(..options, mcp_config_path: Some(path))
}

/// Set permission mode for tool execution.
pub fn with_permission_mode(
  options: CliOptions,
  mode: PermissionMode,
) -> CliOptions {
  CliOptions(..options, permission_mode: Some(mode))
}

/// Resume a specific session by ID.
///
/// Maps to CLI flag: `--resume`
pub fn with_resume(options: CliOptions, session_id: String) -> CliOptions {
  CliOptions(..options, resume_session_id: Some(session_id))
}

/// Continue the most recent session.
///
/// Maps to CLI flag: `--continue`
pub fn with_continue(options: CliOptions) -> CliOptions {
  CliOptions(..options, continue_session: True)
}

/// Set working directory for the CLI process.
pub fn with_cwd(options: CliOptions, path: String) -> CliOptions {
  CliOptions(..options, cwd: Some(path))
}

/// Set custom CLI path override.
pub fn with_cli_path(options: CliOptions, path: String) -> CliOptions {
  CliOptions(..options, cli_path: Some(path))
}

/// Set fallback model to use if primary model fails.
pub fn with_fallback_model(options: CliOptions, model: String) -> CliOptions {
  CliOptions(..options, fallback_model: Some(model))
}

/// Set beta features to enable.
///
/// Maps to CLI flag: `--beta` (repeated for each beta)
pub fn with_betas(options: CliOptions, betas: List(String)) -> CliOptions {
  CliOptions(..options, betas: Some(betas))
}

/// Set permission prompt tool name.
pub fn with_permission_prompt_tool_name(
  options: CliOptions,
  name: String,
) -> CliOptions {
  CliOptions(..options, permission_prompt_tool_name: Some(name))
}

/// Set custom settings dictionary.
pub fn with_settings(
  options: CliOptions,
  settings: Dict(String, dynamic.Dynamic),
) -> CliOptions {
  CliOptions(..options, settings: Some(settings))
}

/// Set additional directories for the CLI.
///
/// Maps to CLI flag: `--add-dir` (repeated for each directory)
pub fn with_add_dirs(options: CliOptions, dirs: List(String)) -> CliOptions {
  CliOptions(..options, add_dirs: Some(dirs))
}

/// Set environment variables for the CLI process.
///
/// Maps to CLI flag: `--env KEY=VALUE` (repeated for each entry)
pub fn with_env(options: CliOptions, env: Dict(String, String)) -> CliOptions {
  CliOptions(..options, env: Some(env))
}

/// Set extra CLI arguments to append at the end.
pub fn with_extra_args(options: CliOptions, args: List(String)) -> CliOptions {
  CliOptions(..options, extra_args: Some(args))
}

// =============================================================================
// SDK Option Builders
// =============================================================================

/// Enable test mode with a mock runner.
pub fn with_test_mode(options: SdkOptions, runner: Runner) -> SdkOptions {
  SdkOptions(..options, test_mode: True, test_runner: Some(runner))
}

/// Skip CLI version check entirely.
pub fn with_skip_version_check(options: SdkOptions) -> SdkOptions {
  SdkOptions(..options, skip_version_check: True)
}

/// Allow unknown CLI versions with a warning instead of failing.
pub fn with_permissive_version_check(options: SdkOptions) -> SdkOptions {
  SdkOptions(..options, permissive_version_check: True)
}

// =============================================================================
// Bidir Option Builders
// =============================================================================

/// Set a pre-tool-use hook callback.
pub fn with_pre_tool_use(
  options: BidirOptions,
  callback: fn(PreToolUseContext) -> HookExecutionResult,
) -> BidirOptions {
  BidirOptions(..options, on_pre_tool_use: Some(callback))
}

/// Set a post-tool-use hook callback.
pub fn with_post_tool_use(
  options: BidirOptions,
  callback: fn(PostToolUseContext) -> HookExecutionResult,
) -> BidirOptions {
  BidirOptions(..options, on_post_tool_use: Some(callback))
}

/// Set a user-prompt-submit hook callback.
pub fn with_user_prompt_submit(
  options: BidirOptions,
  callback: fn(UserPromptSubmitContext) -> HookExecutionResult,
) -> BidirOptions {
  BidirOptions(..options, on_user_prompt_submit: Some(callback))
}

/// Set a stop hook callback.
pub fn with_stop(
  options: BidirOptions,
  callback: fn(StopContext) -> HookExecutionResult,
) -> BidirOptions {
  BidirOptions(..options, on_stop: Some(callback))
}

/// Set a subagent-stop hook callback.
pub fn with_subagent_stop(
  options: BidirOptions,
  callback: fn(SubagentStopContext) -> HookExecutionResult,
) -> BidirOptions {
  BidirOptions(..options, on_subagent_stop: Some(callback))
}

/// Set a pre-compact hook callback.
pub fn with_pre_compact(
  options: BidirOptions,
  callback: fn(PreCompactContext) -> HookExecutionResult,
) -> BidirOptions {
  BidirOptions(..options, on_pre_compact: Some(callback))
}

/// Set a can-use-tool permission check callback.
pub fn with_can_use_tool(
  options: BidirOptions,
  callback: fn(CanUseToolContext) -> PermissionCheckResult,
) -> BidirOptions {
  BidirOptions(..options, on_can_use_tool: Some(callback))
}

/// Add an MCP server handler.
pub fn with_mcp_server(
  options: BidirOptions,
  name: String,
  handler: fn(dynamic.Dynamic) -> dynamic.Dynamic,
) -> BidirOptions {
  let new_servers = list.append(options.mcp_servers, [#(name, handler)])
  BidirOptions(..options, mcp_servers: new_servers)
}

/// Enable file checkpointing for rewind_files support.
pub fn with_file_checkpointing(options: BidirOptions) -> BidirOptions {
  BidirOptions(..options, file_checkpointing_enabled: True)
}

/// Set the global timeout in milliseconds.
pub fn with_timeout(options: BidirOptions, timeout_ms: Int) -> BidirOptions {
  BidirOptions(..options, timeout_ms: Some(timeout_ms))
}

/// Set a per-hook timeout override.
pub fn with_hook_timeout(
  options: BidirOptions,
  event: HookEvent,
  timeout_ms: Int,
) -> BidirOptions {
  let new_timeouts = dict.insert(options.hook_timeouts, event, timeout_ms)
  BidirOptions(..options, hook_timeouts: new_timeouts)
}

/// Set a factory function for creating BidirRunner instances.
pub fn with_bidir_runner_factory(
  options: BidirOptions,
  factory: fn() -> BidirRunner,
) -> BidirOptions {
  BidirOptions(..options, bidir_runner_factory: Some(factory))
}

/// Enable including partial messages in the response stream.
///
/// When enabled, partial message content is streamed as it is generated,
/// rather than waiting for complete messages. This is useful for real-time
/// display of AI responses.
pub fn with_partial_messages(options: BidirOptions) -> BidirOptions {
  BidirOptions(..options, include_partial_messages: True)
}

/// Set a session ID to fork from.
///
/// This creates a new session starting from the state of an existing session,
/// allowing branching of conversations.
pub fn with_fork_session(
  options: BidirOptions,
  session_id: String,
) -> BidirOptions {
  BidirOptions(..options, fork_session: Some(session_id))
}

/// Set custom agent configurations.
///
/// Custom agents can be invoked via the Task tool and have their own
/// descriptions and system prompts.
pub fn with_agents(
  options: BidirOptions,
  agents: List(AgentConfig),
) -> BidirOptions {
  BidirOptions(..options, agents: Some(agents))
}

/// Add a single agent configuration.
pub fn with_agent(options: BidirOptions, agent: AgentConfig) -> BidirOptions {
  let new_agents = case options.agents {
    Some(existing) -> Some(list.append(existing, [agent]))
    None -> Some([agent])
  }
  BidirOptions(..options, agents: new_agents)
}

/// Set the setting sources for configuration resolution.
///
/// Setting sources define where configuration values are read from
/// and their precedence order.
pub fn with_setting_sources(
  options: BidirOptions,
  sources: List(String),
) -> BidirOptions {
  BidirOptions(..options, setting_sources: Some(sources))
}

/// Set sandbox configuration for isolated code execution.
///
/// Sandboxes provide security isolation for code execution and tool use.
pub fn with_sandbox(
  options: BidirOptions,
  sandbox: SandboxConfig,
) -> BidirOptions {
  BidirOptions(..options, sandbox: Some(sandbox))
}

/// Set the list of plugins to enable.
///
/// Plugins extend Claude's capabilities with additional tools and features.
pub fn with_plugins(
  options: BidirOptions,
  plugins: List(String),
) -> BidirOptions {
  BidirOptions(..options, plugins: Some(plugins))
}

/// Add a single plugin.
pub fn with_plugin(options: BidirOptions, plugin: String) -> BidirOptions {
  let new_plugins = case options.plugins {
    Some(existing) -> Some(list.append(existing, [plugin]))
    None -> Some([plugin])
  }
  BidirOptions(..options, plugins: new_plugins)
}

/// Set maximum thinking tokens for extended thinking mode.
///
/// Extended thinking allows the model to use additional tokens for
/// internal reasoning before producing a response.
pub fn with_max_thinking_tokens(
  options: BidirOptions,
  tokens: Int,
) -> BidirOptions {
  BidirOptions(..options, max_thinking_tokens: Some(tokens))
}

/// Set the output format.
///
/// Controls the format of the response output (e.g., "text", "json", "stream-json").
pub fn with_output_format(options: BidirOptions, format: String) -> BidirOptions {
  BidirOptions(..options, output_format: Some(format))
}

// =============================================================================
// AgentConfig Builder
// =============================================================================

/// Create a new agent configuration.
pub fn agent_config(
  name: String,
  description: String,
  prompt: String,
) -> AgentConfig {
  AgentConfig(name:, description:, prompt:)
}

// =============================================================================
// SandboxConfig Builder
// =============================================================================

/// Create a new sandbox configuration.
pub fn sandbox_config(sandbox_type: String) -> SandboxConfig {
  SandboxConfig(sandbox_type:, config: dict.new())
}

/// Add a configuration option to a sandbox configuration.
pub fn with_sandbox_config(
  sandbox: SandboxConfig,
  key: String,
  value: dynamic.Dynamic,
) -> SandboxConfig {
  SandboxConfig(..sandbox, config: dict.insert(sandbox.config, key, value))
}

// =============================================================================
// Legacy QueryOptions Builders (for backwards compatibility)
// =============================================================================

/// Set the model to use (legacy).
pub fn with_model_query(options: QueryOptions, model: String) -> QueryOptions {
  QueryOptions(..options, model: Some(model))
}

/// Set maximum number of agent turns (legacy).
pub fn with_max_turns_query(options: QueryOptions, n: Int) -> QueryOptions {
  QueryOptions(..options, max_turns: Some(n))
}

/// Set maximum budget in USD (legacy).
pub fn with_max_budget_query(options: QueryOptions, usd: Float) -> QueryOptions {
  QueryOptions(..options, max_budget_usd: Some(usd))
}

/// Set the system prompt (legacy).
pub fn with_system_prompt_query(
  options: QueryOptions,
  prompt: String,
) -> QueryOptions {
  QueryOptions(..options, system_prompt: Some(prompt))
}

/// Append to the default system prompt (legacy).
pub fn with_append_system_prompt_query(
  options: QueryOptions,
  prompt: String,
) -> QueryOptions {
  QueryOptions(..options, append_system_prompt: Some(prompt))
}

/// Set allowed tools (legacy).
pub fn with_allowed_tools_query(
  options: QueryOptions,
  tools: List(String),
) -> QueryOptions {
  QueryOptions(..options, allowed_tools: Some(tools))
}

/// Set disallowed tools (legacy).
pub fn with_disallowed_tools_query(
  options: QueryOptions,
  tools: List(String),
) -> QueryOptions {
  QueryOptions(..options, disallowed_tools: Some(tools))
}

/// Set path to MCP configuration file (legacy).
pub fn with_mcp_config_query(
  options: QueryOptions,
  path: String,
) -> QueryOptions {
  QueryOptions(..options, mcp_config_path: Some(path))
}

/// Set permission mode (legacy).
pub fn with_permission_mode_query(
  options: QueryOptions,
  mode: PermissionMode,
) -> QueryOptions {
  QueryOptions(..options, permission_mode: Some(mode))
}

/// Resume a specific session by ID (legacy).
pub fn with_resume_query(
  options: QueryOptions,
  session_id: String,
) -> QueryOptions {
  QueryOptions(..options, resume_session_id: Some(session_id))
}

/// Continue the most recent session (legacy).
pub fn with_continue_query(options: QueryOptions) -> QueryOptions {
  QueryOptions(..options, continue_session: True)
}

/// Set working directory (legacy).
pub fn with_cwd_query(options: QueryOptions, path: String) -> QueryOptions {
  QueryOptions(..options, cwd: Some(path))
}

/// Enable test mode (legacy).
pub fn with_test_mode_query(
  options: QueryOptions,
  runner: Runner,
) -> QueryOptions {
  QueryOptions(..options, test_mode: True, test_runner: Some(runner))
}

/// Skip CLI version check (legacy).
pub fn with_skip_version_check_query(options: QueryOptions) -> QueryOptions {
  QueryOptions(..options, skip_version_check: True)
}

/// Allow unknown CLI versions (legacy).
pub fn with_permissive_version_check_query(
  options: QueryOptions,
) -> QueryOptions {
  QueryOptions(..options, permissive_version_check: True)
}

/// Set a pre-tool-use hook callback (legacy).
pub fn with_pre_tool_use_query(
  options: QueryOptions,
  callback: fn(PreToolUseContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_pre_tool_use: Some(callback))
}

/// Set a post-tool-use hook callback (legacy).
pub fn with_post_tool_use_query(
  options: QueryOptions,
  callback: fn(PostToolUseContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_post_tool_use: Some(callback))
}

/// Set a user-prompt-submit hook callback (legacy).
pub fn with_user_prompt_submit_query(
  options: QueryOptions,
  callback: fn(UserPromptSubmitContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_user_prompt_submit: Some(callback))
}

/// Set a stop hook callback (legacy).
pub fn with_stop_query(
  options: QueryOptions,
  callback: fn(StopContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_stop: Some(callback))
}

/// Set a subagent-stop hook callback (legacy).
pub fn with_subagent_stop_query(
  options: QueryOptions,
  callback: fn(SubagentStopContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_subagent_stop: Some(callback))
}

/// Set a pre-compact hook callback (legacy).
pub fn with_pre_compact_query(
  options: QueryOptions,
  callback: fn(PreCompactContext) -> HookExecutionResult,
) -> QueryOptions {
  QueryOptions(..options, on_pre_compact: Some(callback))
}

/// Set a can-use-tool permission check callback (legacy).
pub fn with_can_use_tool_query(
  options: QueryOptions,
  callback: fn(CanUseToolContext) -> PermissionCheckResult,
) -> QueryOptions {
  QueryOptions(..options, on_can_use_tool: Some(callback))
}

/// Add an MCP server handler (legacy).
pub fn with_mcp_server_query(
  options: QueryOptions,
  name: String,
  handler: fn(dynamic.Dynamic) -> dynamic.Dynamic,
) -> QueryOptions {
  let new_servers = list.append(options.mcp_servers, [#(name, handler)])
  QueryOptions(..options, mcp_servers: new_servers)
}

/// Enable file checkpointing (legacy).
pub fn with_file_checkpointing_query(options: QueryOptions) -> QueryOptions {
  QueryOptions(..options, file_checkpointing_enabled: True)
}

/// Set the global timeout (legacy).
pub fn with_timeout_query(
  options: QueryOptions,
  timeout_ms: Int,
) -> QueryOptions {
  QueryOptions(..options, timeout_ms: Some(timeout_ms))
}

/// Set a per-hook timeout override (legacy).
pub fn with_hook_timeout_query(
  options: QueryOptions,
  event: HookEvent,
  timeout_ms: Int,
) -> QueryOptions {
  let new_timeouts = dict.insert(options.hook_timeouts, event, timeout_ms)
  QueryOptions(..options, hook_timeouts: new_timeouts)
}

/// Set a factory function for creating BidirRunner instances (legacy).
pub fn with_bidir_runner_factory_query(
  options: QueryOptions,
  factory: fn() -> BidirRunner,
) -> QueryOptions {
  QueryOptions(..options, bidir_runner_factory: Some(factory))
}

// =============================================================================
// Option Merge Functions
// =============================================================================

/// Merge two CliOptions, with `override` values taking precedence.
///
/// Merge rules:
/// - Scalar options (Option(T)): override wins if Some, else base
/// - Bool fields: OR (True in either = True)
/// - env, settings (Dict): Deep merge, override keys win
/// - add_dirs, extra_args (List): Append (base ++ override)
/// - betas, allowed_tools, disallowed_tools (List): Replace (override wins if Some)
pub fn merge_cli_options(base: CliOptions, override: CliOptions) -> CliOptions {
  CliOptions(
    // Scalar fields - override wins if Some
    model: merge_option(base.model, override.model),
    max_turns: merge_option(base.max_turns, override.max_turns),
    max_budget_usd: merge_option(base.max_budget_usd, override.max_budget_usd),
    system_prompt: merge_option(base.system_prompt, override.system_prompt),
    append_system_prompt: merge_option(
      base.append_system_prompt,
      override.append_system_prompt,
    ),
    mcp_config_path: merge_option(
      base.mcp_config_path,
      override.mcp_config_path,
    ),
    permission_mode: merge_option(
      base.permission_mode,
      override.permission_mode,
    ),
    resume_session_id: merge_option(
      base.resume_session_id,
      override.resume_session_id,
    ),
    cwd: merge_option(base.cwd, override.cwd),
    cli_path: merge_option(base.cli_path, override.cli_path),
    fallback_model: merge_option(base.fallback_model, override.fallback_model),
    permission_prompt_tool_name: merge_option(
      base.permission_prompt_tool_name,
      override.permission_prompt_tool_name,
    ),
    // Bool fields - OR
    continue_session: base.continue_session || override.continue_session,
    // Dict fields - deep merge
    env: merge_dict_option(base.env, override.env),
    settings: merge_dict_option(base.settings, override.settings),
    // List fields - append
    add_dirs: append_list_option(base.add_dirs, override.add_dirs),
    extra_args: append_list_option(base.extra_args, override.extra_args),
    // List fields - replace (override wins if non-empty)
    betas: merge_replace_list_option(base.betas, override.betas),
    allowed_tools: merge_replace_list_option(
      base.allowed_tools,
      override.allowed_tools,
    ),
    disallowed_tools: merge_replace_list_option(
      base.disallowed_tools,
      override.disallowed_tools,
    ),
  )
}

/// Merge two BidirOptions, with `override` values taking precedence.
///
/// Merge rules:
/// - Scalar options (Option(T)): override wins if Some, else base
/// - Bool fields: OR (True in either = True)
/// - hook_timeouts (Dict): Deep merge, override keys win
/// - mcp_servers, plugins (List): Append (base ++ override)
/// - agents, setting_sources (List): Replace (override wins if Some)
/// - Hook callbacks: override wins if Some (not merged)
pub fn merge_bidir_options(
  base: BidirOptions,
  override: BidirOptions,
) -> BidirOptions {
  BidirOptions(
    // Hook callbacks - override wins if Some
    on_pre_tool_use: merge_option(
      base.on_pre_tool_use,
      override.on_pre_tool_use,
    ),
    on_post_tool_use: merge_option(
      base.on_post_tool_use,
      override.on_post_tool_use,
    ),
    on_user_prompt_submit: merge_option(
      base.on_user_prompt_submit,
      override.on_user_prompt_submit,
    ),
    on_stop: merge_option(base.on_stop, override.on_stop),
    on_subagent_stop: merge_option(
      base.on_subagent_stop,
      override.on_subagent_stop,
    ),
    on_pre_compact: merge_option(base.on_pre_compact, override.on_pre_compact),
    on_can_use_tool: merge_option(
      base.on_can_use_tool,
      override.on_can_use_tool,
    ),
    // List fields - append
    mcp_servers: list.append(base.mcp_servers, override.mcp_servers),
    // Bool fields - OR
    file_checkpointing_enabled: base.file_checkpointing_enabled
      || override.file_checkpointing_enabled,
    include_partial_messages: base.include_partial_messages
      || override.include_partial_messages,
    // Scalar fields - override wins if Some
    timeout_ms: merge_option(base.timeout_ms, override.timeout_ms),
    bidir_runner_factory: merge_option(
      base.bidir_runner_factory,
      override.bidir_runner_factory,
    ),
    fork_session: merge_option(base.fork_session, override.fork_session),
    sandbox: merge_option(base.sandbox, override.sandbox),
    max_thinking_tokens: merge_option(
      base.max_thinking_tokens,
      override.max_thinking_tokens,
    ),
    output_format: merge_option(base.output_format, override.output_format),
    // Dict fields - deep merge
    hook_timeouts: dict.merge(base.hook_timeouts, override.hook_timeouts),
    // List fields - append
    plugins: append_list_option(base.plugins, override.plugins),
    // List fields - replace (override wins if non-empty)
    agents: merge_replace_list_option(base.agents, override.agents),
    setting_sources: merge_replace_list_option(
      base.setting_sources,
      override.setting_sources,
    ),
  )
}

/// Helper: merge two Option values, override wins if Some.
fn merge_option(base: Option(a), override: Option(a)) -> Option(a) {
  case override {
    Some(_) -> override
    None -> base
  }
}

/// Helper: deep merge two Option(Dict) values.
fn merge_dict_option(
  base: Option(Dict(k, v)),
  override: Option(Dict(k, v)),
) -> Option(Dict(k, v)) {
  case base, override {
    Some(b), Some(o) -> Some(dict.merge(b, o))
    Some(b), None -> Some(b)
    None, Some(o) -> Some(o)
    None, None -> None
  }
}

/// Helper: append two Option(List) values.
fn append_list_option(
  base: Option(List(a)),
  override: Option(List(a)),
) -> Option(List(a)) {
  case base, override {
    Some(b), Some(o) -> Some(list.append(b, o))
    Some(b), None -> Some(b)
    None, Some(o) -> Some(o)
    None, None -> None
  }
}

/// Helper: replace Option(List) only if override is non-empty.
/// Per spec: "Replace; highest priority source wins entirely" but only when non-empty.
fn merge_replace_list_option(
  base: Option(List(a)),
  override: Option(List(a)),
) -> Option(List(a)) {
  case override {
    Some([_, ..]) -> override
    _ -> base
  }
}

// =============================================================================
// Check for bidir features
// =============================================================================

/// Check if BidirOptions contain any bidirectional features.
pub fn has_bidir_features_bidir(options: BidirOptions) -> Bool {
  // Check hook callbacks
  option.is_some(options.on_pre_tool_use)
  || option.is_some(options.on_post_tool_use)
  || option.is_some(options.on_user_prompt_submit)
  || option.is_some(options.on_stop)
  || option.is_some(options.on_subagent_stop)
  || option.is_some(options.on_pre_compact)
  || option.is_some(options.on_can_use_tool)
  // Check MCP and timeout configuration
  || !list.is_empty(options.mcp_servers)
  || option.is_some(options.timeout_ms)
  || !dict.is_empty(options.hook_timeouts)
  // Check file checkpointing and runner factory
  || options.file_checkpointing_enabled
  || option.is_some(options.bidir_runner_factory)
  // Check new Python SDK parity fields
  || options.include_partial_messages
  || option.is_some(options.fork_session)
  || option.is_some(options.agents)
  || option.is_some(options.setting_sources)
  || option.is_some(options.sandbox)
  || option.is_some(options.plugins)
  || option.is_some(options.max_thinking_tokens)
  || option.is_some(options.output_format)
}
