/// Tests for CLI argument building from QueryOptions.
import claude_agent_sdk/internal/cli
import claude_agent_sdk/options.{
  type BidirOptions, AcceptEdits, BidirOptions, BypassPermissions, Plan,
  bidir_options, cli_options, default_options,
  with_allowed_tools_query as with_allowed_tools,
  with_append_system_prompt_query as with_append_system_prompt,
  with_continue_query as with_continue,
  with_disallowed_tools_query as with_disallowed_tools,
  with_max_budget_query as with_max_budget,
  with_max_turns_query as with_max_turns,
  with_mcp_config_query as with_mcp_config, with_model_query as with_model,
  with_permission_mode_query as with_permission_mode,
  with_resume_query as with_resume,
  with_system_prompt_query as with_system_prompt,
}
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleeunit/should

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

/// Helper to build bidir args with default CLI options
fn build_bidir_args(bidir_opts: BidirOptions) -> List(String) {
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
