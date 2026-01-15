/// Tests for hook option builders.
///
/// Verifies that hook callback builders correctly store callbacks in QueryOptions
/// and that builders compose correctly in pipelines.
import claude_agent_sdk/hook
import claude_agent_sdk/options
import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}
import gleeunit/should

// Helper to convert any value to Dynamic (identity function in Erlang)
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// =============================================================================
// Individual Builder Tests
// =============================================================================

pub fn with_pre_tool_use_sets_callback_test() {
  let opts =
    options.bidir_options()
    |> options.with_pre_tool_use(fn(_ctx) { hook.Continue })

  // Callback should be set
  opts.on_pre_tool_use
  |> option.is_some
  |> should.be_true
}

pub fn with_post_tool_use_sets_callback_test() {
  let opts =
    options.bidir_options()
    |> options.with_post_tool_use(fn(_ctx) { hook.Continue })

  opts.on_post_tool_use
  |> option.is_some
  |> should.be_true
}

pub fn with_user_prompt_submit_sets_callback_test() {
  let opts =
    options.bidir_options()
    |> options.with_user_prompt_submit(fn(_ctx) { hook.Continue })

  opts.on_user_prompt_submit
  |> option.is_some
  |> should.be_true
}

pub fn with_stop_sets_callback_test() {
  let opts =
    options.bidir_options()
    |> options.with_stop(fn(_ctx) { hook.Continue })

  opts.on_stop
  |> option.is_some
  |> should.be_true
}

pub fn with_subagent_stop_sets_callback_test() {
  let opts =
    options.bidir_options()
    |> options.with_subagent_stop(fn(_ctx) { hook.Continue })

  opts.on_subagent_stop
  |> option.is_some
  |> should.be_true
}

pub fn with_pre_compact_sets_callback_test() {
  let opts =
    options.bidir_options()
    |> options.with_pre_compact(fn(_ctx) { hook.Continue })

  opts.on_pre_compact
  |> option.is_some
  |> should.be_true
}

pub fn with_can_use_tool_sets_callback_test() {
  let opts =
    options.bidir_options()
    |> options.with_can_use_tool(fn(_ctx) { hook.Allow })

  opts.on_can_use_tool
  |> option.is_some
  |> should.be_true
}

// =============================================================================
// Pipeline Composition Tests
// =============================================================================

pub fn builders_compose_in_pipeline_test() {
  // Verify all hook builders can be chained together
  let opts =
    options.bidir_options()
    |> options.with_pre_tool_use(fn(_ctx) { hook.Continue })
    |> options.with_post_tool_use(fn(_ctx) { hook.Continue })
    |> options.with_user_prompt_submit(fn(_ctx) { hook.Continue })
    |> options.with_stop(fn(_ctx) { hook.Continue })
    |> options.with_subagent_stop(fn(_ctx) { hook.Continue })
    |> options.with_pre_compact(fn(_ctx) { hook.Continue })
    |> options.with_can_use_tool(fn(_ctx) { hook.Allow })

  // All callbacks should be set
  opts.on_pre_tool_use |> option.is_some |> should.be_true
  opts.on_post_tool_use |> option.is_some |> should.be_true
  opts.on_user_prompt_submit |> option.is_some |> should.be_true
  opts.on_stop |> option.is_some |> should.be_true
  opts.on_subagent_stop |> option.is_some |> should.be_true
  opts.on_pre_compact |> option.is_some |> should.be_true
  opts.on_can_use_tool |> option.is_some |> should.be_true
}

pub fn hook_builders_compose_with_other_builders_test() {
  // Verify multiple hook builders compose correctly
  let opts =
    options.bidir_options()
    |> options.with_pre_tool_use(fn(_ctx) { hook.Continue })
    |> options.with_post_tool_use(fn(_ctx) { hook.Continue })
    |> options.with_stop(fn(_ctx) { hook.Continue })

  // Hook callbacks should be set
  opts.on_pre_tool_use |> option.is_some |> should.be_true
  opts.on_post_tool_use |> option.is_some |> should.be_true
  opts.on_stop |> option.is_some |> should.be_true

  // Unset hooks should remain None
  opts.on_user_prompt_submit |> should.equal(None)
  opts.on_subagent_stop |> should.equal(None)
}

// =============================================================================
// Callback Invocation Tests
// =============================================================================

pub fn pre_tool_use_callback_returns_block_test() {
  let opts =
    options.bidir_options()
    |> options.with_pre_tool_use(fn(ctx) {
      case ctx.tool_name {
        "Bash" -> hook.Block("Bash disabled")
        _ -> hook.Continue
      }
    })

  // Extract and invoke the callback
  let assert Some(callback) = opts.on_pre_tool_use
  let ctx =
    hook.PreToolUseContext(
      tool_name: "Bash",
      tool_input: to_dynamic(Nil),
      session_id: "test-session",
    )

  callback(ctx) |> should.equal(hook.Block("Bash disabled"))
}

pub fn pre_tool_use_callback_returns_continue_test() {
  let opts =
    options.bidir_options()
    |> options.with_pre_tool_use(fn(ctx) {
      case ctx.tool_name {
        "Bash" -> hook.Block("Bash disabled")
        _ -> hook.Continue
      }
    })

  let assert Some(callback) = opts.on_pre_tool_use
  let ctx =
    hook.PreToolUseContext(
      tool_name: "Read",
      tool_input: to_dynamic(Nil),
      session_id: "test-session",
    )

  callback(ctx) |> should.equal(hook.Continue)
}

pub fn can_use_tool_callback_returns_deny_test() {
  let opts =
    options.bidir_options()
    |> options.with_can_use_tool(fn(ctx) {
      case ctx.tool_name {
        "Write" -> hook.Deny("Write access disabled")
        _ -> hook.Allow
      }
    })

  let assert Some(callback) = opts.on_can_use_tool
  let ctx =
    hook.CanUseToolContext(
      tool_name: "Write",
      tool_input: to_dynamic(Nil),
      session_id: "test-session",
      permission_suggestions: [],
      blocked_path: None,
    )

  callback(ctx) |> should.equal(hook.Deny("Write access disabled"))
}

pub fn can_use_tool_callback_returns_allow_test() {
  let opts =
    options.bidir_options()
    |> options.with_can_use_tool(fn(ctx) {
      case ctx.tool_name {
        "Write" -> hook.Deny("Write access disabled")
        _ -> hook.Allow
      }
    })

  let assert Some(callback) = opts.on_can_use_tool
  let ctx =
    hook.CanUseToolContext(
      tool_name: "Read",
      tool_input: to_dynamic(Nil),
      session_id: "test-session",
      permission_suggestions: [],
      blocked_path: None,
    )

  callback(ctx) |> should.equal(hook.Allow)
}

// =============================================================================
// Default State Tests
// =============================================================================

pub fn default_options_has_no_hooks_test() {
  let opts = options.bidir_options()

  opts.on_pre_tool_use |> should.equal(None)
  opts.on_post_tool_use |> should.equal(None)
  opts.on_user_prompt_submit |> should.equal(None)
  opts.on_stop |> should.equal(None)
  opts.on_subagent_stop |> should.equal(None)
  opts.on_pre_compact |> should.equal(None)
  opts.on_can_use_tool |> should.equal(None)
}

pub fn callback_overwrites_previous_test() {
  let first_callback = fn(_ctx: hook.PreToolUseContext) { hook.Continue }
  let second_callback = fn(_ctx: hook.PreToolUseContext) {
    hook.Block("blocked")
  }

  let opts =
    options.bidir_options()
    |> options.with_pre_tool_use(first_callback)
    |> options.with_pre_tool_use(second_callback)

  // Should use the second callback
  let assert Some(callback) = opts.on_pre_tool_use
  let ctx =
    hook.PreToolUseContext(
      tool_name: "Test",
      tool_input: to_dynamic(Nil),
      session_id: "test",
    )

  callback(ctx) |> should.equal(hook.Block("blocked"))
}
