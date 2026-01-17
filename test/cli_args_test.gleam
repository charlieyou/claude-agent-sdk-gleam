/// Tests for CLI argument building from QueryOptions.
import claude_agent_sdk/internal/cli
import claude_agent_sdk/options.{
  type AgentConfig, type BidirOptions, type CliOptions, type SandboxConfig,
  AcceptEdits, AgentConfig, BidirOptions, BypassPermissions, CliOptions, Plan,
  SandboxConfig, bidir_options, cli_options, default_options, sandbox_config,
  with_allowed_tools_query as with_allowed_tools,
  with_append_system_prompt_query as with_append_system_prompt,
  with_continue_query as with_continue,
  with_disallowed_tools_query as with_disallowed_tools,
  with_max_budget_query as with_max_budget,
  with_max_turns_query as with_max_turns,
  with_mcp_config_query as with_mcp_config, with_model_query as with_model,
  with_permission_mode_query as with_permission_mode,
  with_resume_query as with_resume, with_sandbox_config,
  with_system_prompt_query as with_system_prompt,
}
import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit/should
import simplifile

/// Convert any value to Dynamic using Erlang identity function.
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> dynamic.Dynamic

// =============================================================================
// Helper functions
// =============================================================================

/// Assert that a list contains a specific element.
fn assert_contains(lst: List(String), elem: String) -> Nil {
  list.contains(lst, elem) |> should.be_true
}

/// Assert that a list does NOT contain a specific element.
fn assert_not_contains(lst: List(String), elem: String) -> Nil {
  list.contains(lst, elem) |> should.be_false
}

/// Find the index of an element in a list.
fn index_of(lst: List(String), elem: String) -> Result(Int, Nil) {
  find_index_helper(lst, elem, 0)
}

fn find_index_helper(
  lst: List(String),
  elem: String,
  idx: Int,
) -> Result(Int, Nil) {
  case lst {
    [] -> Error(Nil)
    [head, ..tail] ->
      case head == elem {
        True -> Ok(idx)
        False -> find_index_helper(tail, elem, idx + 1)
      }
  }
}

// =============================================================================
// Helper: delegate to actual implementation
// =============================================================================

fn build_args(opts: options.QueryOptions, prompt: String) -> List(String) {
  cli.build_cli_args(opts, prompt)
}

// =============================================================================
// Tests for basic/fixed CLI arguments
// =============================================================================

pub fn basic_args_test() {
  let opts = default_options()
  let args = build_args(opts, "Hello Claude")

  // Verify fixed flags are present
  assert_contains(args, "--print")
  assert_contains(args, "--output-format")
  assert_contains(args, "stream-json")
  assert_contains(args, "--verbose")

  // Verify prompt separator and prompt are at the end
  // Note: Prompt is unquoted; shell quoting is handled by the process spawner
  let args_str = string.join(args, " ")
  string.contains(args_str, "-- Hello Claude") |> should.be_true
}

// =============================================================================
// Tests for model option
// =============================================================================

pub fn model_option_test() {
  let opts =
    default_options()
    |> with_model("opus")
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--model")
  assert_contains(args, "opus")

  // Verify --model comes before opus
  let model_idx = index_of(args, "--model")
  let opus_idx = index_of(args, "opus")
  case model_idx, opus_idx {
    Ok(m), Ok(o) -> { m + 1 } |> should.equal(o)
    _, _ -> should.fail()
  }
}

// =============================================================================
// Tests for max_turns option
// =============================================================================

pub fn max_turns_option_test() {
  let opts =
    default_options()
    |> with_max_turns(5)
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--max-turns")
  assert_contains(args, "5")
}

// =============================================================================
// Tests for max_budget option
// =============================================================================

pub fn max_budget_option_test() {
  let opts =
    default_options()
    |> with_max_budget(10.5)
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--max-budget-usd")
  assert_contains(args, "10.5")
}

// =============================================================================
// Tests for system prompt options (with precedence)
// =============================================================================

pub fn system_prompt_option_test() {
  let opts =
    default_options()
    |> with_system_prompt("You are a helpful assistant")
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--system-prompt")
  assert_contains(args, "You are a helpful assistant")
}

pub fn append_system_prompt_option_test() {
  let opts =
    default_options()
    |> with_append_system_prompt("Be concise")
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--append-system-prompt")
  assert_contains(args, "Be concise")
}

/// Precedence: system_prompt wins over append_system_prompt
pub fn system_prompt_precedence_test() {
  let opts =
    default_options()
    |> with_system_prompt("Full replacement")
    |> with_append_system_prompt("This should be ignored")
  let args = build_args(opts, "test prompt")

  // system_prompt should be present
  assert_contains(args, "--system-prompt")
  assert_contains(args, "Full replacement")

  // append_system_prompt should NOT be present
  assert_not_contains(args, "--append-system-prompt")
}

// =============================================================================
// Tests for tool options (with precedence)
// =============================================================================

pub fn allowed_tools_option_test() {
  let opts =
    default_options()
    |> with_allowed_tools(["Read", "Write", "Bash"])
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--allowed-tools")
  assert_contains(args, "Read,Write,Bash")
}

pub fn disallowed_tools_option_test() {
  let opts =
    default_options()
    |> with_disallowed_tools(["Bash", "Edit"])
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--disallowed-tools")
  assert_contains(args, "Bash,Edit")
}

/// Precedence: allowed_tools wins over disallowed_tools
pub fn tools_precedence_test() {
  let opts =
    default_options()
    |> with_allowed_tools(["Read"])
    |> with_disallowed_tools(["Write"])
  let args = build_args(opts, "test prompt")

  // allowed_tools should be present
  assert_contains(args, "--allowed-tools")
  assert_contains(args, "Read")

  // disallowed_tools should NOT be present
  assert_not_contains(args, "--disallowed-tools")
}

// =============================================================================
// Tests for MCP config option
// =============================================================================

pub fn mcp_config_option_test() {
  let opts =
    default_options()
    |> with_mcp_config("/path/to/mcp.json")
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--mcp-config")
  assert_contains(args, "/path/to/mcp.json")
}

// =============================================================================
// Tests for permission mode options
// =============================================================================

pub fn permission_mode_accept_edits_test() {
  let opts =
    default_options()
    |> with_permission_mode(AcceptEdits)
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--permission-mode")
  assert_contains(args, "acceptEdits")
}

pub fn permission_mode_bypass_test() {
  let opts =
    default_options()
    |> with_permission_mode(BypassPermissions)
  let args = build_args(opts, "test prompt")

  // BypassPermissions uses a different flag name
  assert_contains(args, "--dangerously-skip-permissions")
}

pub fn permission_mode_plan_test() {
  let opts =
    default_options()
    |> with_permission_mode(Plan)
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--permission-mode")
  assert_contains(args, "plan")
}

// =============================================================================
// Tests for session options (with precedence)
// =============================================================================

pub fn resume_session_test() {
  let opts =
    default_options()
    |> with_resume("session-uuid-123")
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--resume")
  assert_contains(args, "session-uuid-123")
}

pub fn continue_session_test() {
  let opts =
    default_options()
    |> with_continue()
  let args = build_args(opts, "test prompt")

  assert_contains(args, "--continue")
}

/// Precedence: resume_session_id wins over continue_session
pub fn session_precedence_test() {
  let opts =
    default_options()
    |> with_resume("specific-session")
    |> with_continue()
  let args = build_args(opts, "test prompt")

  // resume should be present
  assert_contains(args, "--resume")
  assert_contains(args, "specific-session")

  // continue should NOT be present
  assert_not_contains(args, "--continue")
}

// =============================================================================
// Tests for combined options
// =============================================================================

pub fn combined_options_test() {
  let opts =
    default_options()
    |> with_model("sonnet")
    |> with_max_turns(10)
    |> with_system_prompt("Be helpful")
    |> with_allowed_tools(["Read", "Write"])
    |> with_permission_mode(AcceptEdits)
  let args = build_args(opts, "Do something useful")

  // All options should be present
  assert_contains(args, "--model")
  assert_contains(args, "sonnet")
  assert_contains(args, "--max-turns")
  assert_contains(args, "10")
  assert_contains(args, "--system-prompt")
  assert_contains(args, "Be helpful")
  assert_contains(args, "--allowed-tools")
  assert_contains(args, "Read,Write")
  assert_contains(args, "--permission-mode")
  assert_contains(args, "acceptEdits")

  // Fixed args should still be present
  assert_contains(args, "--print")
  assert_contains(args, "--output-format")
  assert_contains(args, "stream-json")
  assert_contains(args, "--verbose")
}

// =============================================================================
// Tests for build_bidir_cli_args_new with BidirOptions
// =============================================================================

/// Helper to build bidir args with default CLI options.
/// Unwraps the Result and returns just the args.
fn build_bidir_args(bidir_opts: BidirOptions) -> List(String) {
  cli.build_bidir_cli_args_new(cli_options(), bidir_opts)
  |> result.map(fn(r) { r.args })
  |> result.unwrap([])
}

/// Helper to build bidir args and return the full result (for cleanup testing).
fn build_bidir_args_result(
  bidir_opts: BidirOptions,
) -> Result(cli.BidirArgsResult, cli.AgentsSerializationError) {
  cli.build_bidir_cli_args_new(cli_options(), bidir_opts)
}

/// Default bidir options should not emit extra flags
pub fn bidir_default_options_test() {
  let args = build_bidir_args(bidir_options())

  // Fixed bidir args should be present
  assert_contains(args, "--output-format")
  assert_contains(args, "stream-json")
  assert_contains(args, "--input-format")
  assert_contains(args, "--verbose")

  // No extra BidirOptions flags
  assert_not_contains(args, "--include-partial-messages")
  assert_not_contains(args, "--fork-session")
  assert_not_contains(args, "--setting-sources")
  assert_not_contains(args, "--max-thinking-tokens")
}

/// include_partial_messages=true emits --include-partial-messages flag
pub fn bidir_include_partial_messages_test() {
  let opts = BidirOptions(..bidir_options(), include_partial_messages: True)
  let args = build_bidir_args(opts)

  assert_contains(args, "--include-partial-messages")
}

/// include_partial_messages=false does not emit flag
pub fn bidir_include_partial_messages_false_test() {
  let opts = BidirOptions(..bidir_options(), include_partial_messages: False)
  let args = build_bidir_args(opts)

  assert_not_contains(args, "--include-partial-messages")
}

/// fork_session emits --fork-session with session ID
pub fn bidir_fork_session_test() {
  let opts = BidirOptions(..bidir_options(), fork_session: Some("abc-123"))
  let args = build_bidir_args(opts)

  assert_contains(args, "--fork-session")
  assert_contains(args, "abc-123")

  // Verify flag and value are adjacent by checking the joined args string
  let args_str = string.join(args, " ")
  string.contains(args_str, "--fork-session abc-123") |> should.be_true
}

/// setting_sources emits --setting-sources with comma-separated list
pub fn bidir_setting_sources_test() {
  let opts =
    BidirOptions(
      ..bidir_options(),
      setting_sources: Some(["user", "project", "global"]),
    )
  let args = build_bidir_args(opts)

  assert_contains(args, "--setting-sources")
  assert_contains(args, "user,project,global")
}

/// max_thinking_tokens emits --max-thinking-tokens with integer value
pub fn bidir_max_thinking_tokens_test() {
  let opts = BidirOptions(..bidir_options(), max_thinking_tokens: Some(16_384))
  let args = build_bidir_args(opts)

  assert_contains(args, "--max-thinking-tokens")
  assert_contains(args, "16384")
}

/// output_format overrides default stream-json
pub fn bidir_output_format_override_test() {
  let opts = BidirOptions(..bidir_options(), output_format: Some("text"))
  let args = build_bidir_args(opts)

  assert_contains(args, "--output-format")
  assert_contains(args, "text")

  // Count occurrences of stream-json - should only appear for input-format
  let stream_json_count =
    list.filter(args, fn(a) { a == "stream-json" }) |> list.length
  stream_json_count |> should.equal(1)
}

/// Combined BidirOptions produces all expected flags
pub fn bidir_combined_options_test() {
  let opts =
    BidirOptions(
      ..bidir_options(),
      include_partial_messages: True,
      fork_session: Some("session-xyz"),
      setting_sources: Some(["user", "project"]),
      max_thinking_tokens: Some(8192),
      output_format: Some("json"),
    )
  let args = build_bidir_args(opts)

  // All BidirOptions flags should be present
  assert_contains(args, "--include-partial-messages")
  assert_contains(args, "--fork-session")
  assert_contains(args, "session-xyz")
  assert_contains(args, "--setting-sources")
  assert_contains(args, "user,project")
  assert_contains(args, "--max-thinking-tokens")
  assert_contains(args, "8192")
  assert_contains(args, "--output-format")
  assert_contains(args, "json")

  // Fixed bidir args should still be present
  assert_contains(args, "--input-format")
  assert_contains(args, "--verbose")
}

// =============================================================================
// Tests for agents serialization (inline vs temp file)
// =============================================================================

/// Helper to create a test agent config
fn test_agent(name: String) -> AgentConfig {
  AgentConfig(
    name: name,
    description: "Test agent " <> name,
    prompt: "You are " <> name,
  )
}

/// agents=None produces no --agents flag
pub fn bidir_agents_none_test() {
  let opts = BidirOptions(..bidir_options(), agents: None)
  let args = build_bidir_args(opts)

  assert_not_contains(args, "--agents")
}

/// agents=Some([]) produces no --agents flag (not --agents '[]')
pub fn bidir_agents_empty_list_test() {
  let opts = BidirOptions(..bidir_options(), agents: Some([]))
  let args = build_bidir_args(opts)

  assert_not_contains(args, "--agents")
}

/// agents with 1 entry produces inline JSON
pub fn bidir_agents_one_inline_test() {
  let opts =
    BidirOptions(..bidir_options(), agents: Some([test_agent("alpha")]))
  let result = build_bidir_args_result(opts)

  // Should succeed with no cleanup file
  result |> should.be_ok
  let r = result |> result.unwrap(cli.BidirArgsResult([], None))

  // Should have --agents with inline JSON
  assert_contains(r.args, "--agents")
  r.cleanup_file |> should.equal(None)

  // Find the value after --agents
  let args_str = string.join(r.args, " ")
  string.contains(args_str, "--agents [") |> should.be_true
  string.contains(args_str, "\"name\":\"alpha\"") |> should.be_true
}

/// agents with exactly 3 entries produces inline JSON (boundary case)
pub fn bidir_agents_three_inline_test() {
  let agents = [test_agent("alpha"), test_agent("beta"), test_agent("gamma")]
  let opts = BidirOptions(..bidir_options(), agents: Some(agents))
  let result = build_bidir_args_result(opts)

  // Should succeed with no cleanup file (3 is threshold)
  result |> should.be_ok
  let r = result |> result.unwrap(cli.BidirArgsResult([], None))

  // Should have inline JSON, not file reference
  assert_contains(r.args, "--agents")
  r.cleanup_file |> should.equal(None)

  // Verify inline format (not @file)
  let args_str = string.join(r.args, " ")
  string.contains(args_str, "--agents [") |> should.be_true
  string.contains(args_str, "--agents @") |> should.be_false
}

/// agents with 4 entries produces temp file reference
pub fn bidir_agents_four_tempfile_test() {
  let agents = [
    test_agent("alpha"),
    test_agent("beta"),
    test_agent("gamma"),
    test_agent("delta"),
  ]
  let opts = BidirOptions(..bidir_options(), agents: Some(agents))
  let result = build_bidir_args_result(opts)

  // Should succeed with cleanup file
  result |> should.be_ok
  let r = result |> result.unwrap(cli.BidirArgsResult([], None))

  // Should have --agents with @file reference
  assert_contains(r.args, "--agents")
  r.cleanup_file |> should.be_some

  // Verify @file format
  let args_str = string.join(r.args, " ")
  string.contains(args_str, "--agents @/tmp/agents-") |> should.be_true
  string.contains(args_str, ".json") |> should.be_true

  // Verify temp file exists and contains valid JSON
  case r.cleanup_file {
    Some(path) -> {
      // File should exist
      simplifile.is_file(path) |> should.be_ok
      let is_file = simplifile.is_file(path) |> result.unwrap(False)
      is_file |> should.be_true

      // File should contain JSON array with 4 agents
      let content = simplifile.read(path) |> result.unwrap("")
      string.contains(content, "\"name\":\"alpha\"") |> should.be_true
      string.contains(content, "\"name\":\"delta\"") |> should.be_true

      // Clean up the temp file
      cli.cleanup_agents_file(path)

      // File should be deleted
      simplifile.is_file(path)
      |> result.unwrap(True)
      |> should.be_false
    }
    None -> should.fail()
  }
}

/// cleanup_agents_file is idempotent (no error on missing file)
pub fn bidir_agents_cleanup_idempotent_test() {
  // Call cleanup on non-existent file - should not fail
  cli.cleanup_agents_file("/tmp/nonexistent-agents-file-xyz123.json")
  // If we get here without exception, the test passes
  True |> should.be_true
}

/// Temp file paths are unique (cryptographically random)
pub fn bidir_agents_unique_paths_test() {
  let agents = [
    test_agent("alpha"),
    test_agent("beta"),
    test_agent("gamma"),
    test_agent("delta"),
  ]
  let opts = BidirOptions(..bidir_options(), agents: Some(agents))

  // Generate two temp files
  let result1 = build_bidir_args_result(opts)
  let result2 = build_bidir_args_result(opts)

  result1 |> should.be_ok
  result2 |> should.be_ok

  let r1 = result1 |> result.unwrap(cli.BidirArgsResult([], None))
  let r2 = result2 |> result.unwrap(cli.BidirArgsResult([], None))

  // Paths should be different
  case r1.cleanup_file, r2.cleanup_file {
    Some(path1), Some(path2) -> {
      path1 |> should.not_equal(path2)
      // Clean up both
      cli.cleanup_agents_file(path1)
      cli.cleanup_agents_file(path2)
    }
    _, _ -> should.fail()
  }
}

// =============================================================================
// Tests for plugins (repeated --plugin flag)
// =============================================================================

/// plugins=None produces no --plugin flag
pub fn bidir_plugins_none_test() {
  let opts = BidirOptions(..bidir_options(), plugins: None)
  let args = build_bidir_args(opts)

  assert_not_contains(args, "--plugin")
}

/// plugins=Some([]) produces no --plugin flags
pub fn bidir_plugins_empty_list_test() {
  let opts = BidirOptions(..bidir_options(), plugins: Some([]))
  let args = build_bidir_args(opts)

  assert_not_contains(args, "--plugin")
}

/// plugins with single entry produces one --plugin flag
pub fn bidir_plugins_single_test() {
  let opts = BidirOptions(..bidir_options(), plugins: Some(["my-plugin"]))
  let args = build_bidir_args(opts)

  assert_contains(args, "--plugin")
  assert_contains(args, "my-plugin")

  // Verify flag and value are adjacent
  let args_str = string.join(args, " ")
  string.contains(args_str, "--plugin my-plugin") |> should.be_true
}

/// plugins with multiple entries produces repeated --plugin flags
pub fn bidir_plugins_multiple_test() {
  let opts =
    BidirOptions(
      ..bidir_options(),
      plugins: Some(["plugin-a", "plugin-b", "plugin-c"]),
    )
  let args = build_bidir_args(opts)

  // Each plugin should have its own --plugin flag
  let args_str = string.join(args, " ")
  string.contains(args_str, "--plugin plugin-a") |> should.be_true
  string.contains(args_str, "--plugin plugin-b") |> should.be_true
  string.contains(args_str, "--plugin plugin-c") |> should.be_true

  // Count --plugin occurrences - should be 3
  let plugin_count = list.filter(args, fn(a) { a == "--plugin" }) |> list.length
  plugin_count |> should.equal(3)
}

// =============================================================================
// Tests for sandbox (merged into --settings JSON)
// =============================================================================

/// Helper to build bidir args with custom CLI options (for settings tests)
fn build_bidir_args_with_cli(
  cli_opts: CliOptions,
  bidir_opts: BidirOptions,
) -> List(String) {
  cli.build_bidir_cli_args_new(cli_opts, bidir_opts)
  |> result.map(fn(r) { r.args })
  |> result.unwrap([])
}

/// sandbox=None produces no sandbox in --settings
pub fn bidir_sandbox_none_test() {
  let opts = BidirOptions(..bidir_options(), sandbox: None)
  let args = build_bidir_args(opts)

  // Should not have --settings flag (since no settings and no sandbox)
  assert_not_contains(args, "--settings")
}

/// sandbox with basic type produces --settings with sandbox object
pub fn bidir_sandbox_basic_test() {
  let sandbox = sandbox_config("docker")
  let opts = BidirOptions(..bidir_options(), sandbox: Some(sandbox))
  let args = build_bidir_args(opts)

  assert_contains(args, "--settings")

  // Find the settings value
  let args_str = string.join(args, " ")
  // Should contain sandbox with type
  string.contains(args_str, "\"sandbox\"") |> should.be_true
  string.contains(args_str, "\"type\":\"docker\"") |> should.be_true
}

/// sandbox with additional config options
pub fn bidir_sandbox_with_config_test() {
  let sandbox =
    sandbox_config("container")
    |> with_sandbox_config("image", to_dynamic("ubuntu:latest"))
    |> with_sandbox_config("memory", to_dynamic("2g"))
  let opts = BidirOptions(..bidir_options(), sandbox: Some(sandbox))
  let args = build_bidir_args(opts)

  assert_contains(args, "--settings")

  let args_str = string.join(args, " ")
  string.contains(args_str, "\"type\":\"container\"") |> should.be_true
  string.contains(args_str, "\"image\":\"ubuntu:latest\"") |> should.be_true
  string.contains(args_str, "\"memory\":\"2g\"") |> should.be_true
}

/// sandbox merges with existing settings
pub fn bidir_sandbox_merge_with_settings_test() {
  // Create CLI options with existing settings
  let existing_settings =
    dict.new()
    |> dict.insert("theme", to_dynamic("dark"))
    |> dict.insert("fontSize", to_dynamic(14))
  let cli_opts = CliOptions(..cli_options(), settings: Some(existing_settings))

  // Create bidir options with sandbox
  let sandbox = sandbox_config("docker")
  let bidir_opts = BidirOptions(..bidir_options(), sandbox: Some(sandbox))

  let args = build_bidir_args_with_cli(cli_opts, bidir_opts)

  assert_contains(args, "--settings")

  // Settings should contain both original settings and sandbox
  let args_str = string.join(args, " ")
  string.contains(args_str, "\"theme\":\"dark\"") |> should.be_true
  string.contains(args_str, "\"fontSize\":14") |> should.be_true
  string.contains(args_str, "\"sandbox\"") |> should.be_true
  string.contains(args_str, "\"type\":\"docker\"") |> should.be_true

  // Should only have ONE --settings flag
  let settings_count =
    list.filter(args, fn(a) { a == "--settings" }) |> list.length
  settings_count |> should.equal(1)
}

/// sandbox=None with existing settings preserves original settings
pub fn bidir_sandbox_none_with_settings_test() {
  // Create CLI options with existing settings
  let existing_settings =
    dict.new()
    |> dict.insert("theme", to_dynamic("light"))
  let cli_opts = CliOptions(..cli_options(), settings: Some(existing_settings))

  // No sandbox
  let bidir_opts = BidirOptions(..bidir_options(), sandbox: None)

  let args = build_bidir_args_with_cli(cli_opts, bidir_opts)

  assert_contains(args, "--settings")

  // Settings should contain original settings without sandbox
  let args_str = string.join(args, " ")
  string.contains(args_str, "\"theme\":\"light\"") |> should.be_true
  // sandbox key should not be present
  string.contains(args_str, "\"sandbox\"") |> should.be_false
}

// =============================================================================
// Tests for combined plugins and sandbox
// =============================================================================

/// Both plugins and sandbox work together
pub fn bidir_plugins_and_sandbox_combined_test() {
  let sandbox =
    sandbox_config("docker")
    |> with_sandbox_config("privileged", to_dynamic(False))
  let opts =
    BidirOptions(
      ..bidir_options(),
      plugins: Some(["auth-plugin", "logging-plugin"]),
      sandbox: Some(sandbox),
    )
  let args = build_bidir_args(opts)

  // Plugins should be present
  let args_str = string.join(args, " ")
  string.contains(args_str, "--plugin auth-plugin") |> should.be_true
  string.contains(args_str, "--plugin logging-plugin") |> should.be_true

  // Sandbox should be in settings
  assert_contains(args, "--settings")
  string.contains(args_str, "\"sandbox\"") |> should.be_true
  string.contains(args_str, "\"type\":\"docker\"") |> should.be_true
  string.contains(args_str, "\"privileged\":false") |> should.be_true
}

/// Regression test: sandbox with extra_args containing --settings should produce single merged --settings
pub fn bidir_sandbox_with_extra_args_settings_test() {
  // Create CLI options with extra_args that includes --settings
  let cli_opts =
    CliOptions(..cli_options(), extra_args: Some(["--settings", "{\"foo\":1}"]))

  // Create bidir options with sandbox
  let sandbox = sandbox_config("docker")
  let bidir_opts = BidirOptions(..bidir_options(), sandbox: Some(sandbox))

  let args = build_bidir_args_with_cli(cli_opts, bidir_opts)

  // Should only have ONE --settings flag (the merged one with sandbox)
  let settings_count =
    list.filter(args, fn(a) { a == "--settings" }) |> list.length
  settings_count |> should.equal(1)

  // The single --settings should contain both foo and sandbox
  let args_str = string.join(args, " ")
  string.contains(args_str, "\"sandbox\"") |> should.be_true
  string.contains(args_str, "\"type\":\"docker\"") |> should.be_true
  // Note: foo from extra_args should be merged in
  string.contains(args_str, "\"foo\":1") |> should.be_true
}
