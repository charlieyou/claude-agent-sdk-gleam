/// Tests for option precedence and merge rules.
///
/// Per the Technical Design:
/// | Priority | Source | Example |
/// |----------|--------|---------|
/// | 1 (highest) | Explicit function arguments | start_session(model: Some("opus")) |
/// | 2 | Environment variables | CLAUDE_MODEL=opus |
/// | 3 | Config file settings | settings.model in config |
/// | 4 (lowest) | SDK defaults | CliOptions.default() |
///
/// Merge Rules for Composite Options:
/// | Option | Merge Behavior |
/// |--------|----------------|
/// | env: Dict(String, String) | Deep merge; explicit args override env vars override config |
/// | settings: Dict(String, Json) | Deep merge; same precedence as env |
/// | add_dirs: List(String) | Append; all sources concatenated (no deduplication) |
/// | extra_args: List(String) | Append; all sources concatenated |
/// | betas: List(String) | Replace; highest priority source wins entirely |
/// | agents: List(AgentConfig) | Replace; highest priority source wins entirely |
import claude_agent_sdk/hook
import claude_agent_sdk/options.{CliOptions}
import gleam/dict
import gleam/dynamic
import gleam/option.{None, Some}
import gleeunit/should

// =============================================================================
// Scalar Field Precedence Tests
// =============================================================================

pub fn merge_model_override_wins_test() {
  let base =
    options.cli_options()
    |> options.with_model("sonnet")
  let override =
    options.cli_options()
    |> options.with_model("opus")

  let result = options.merge_cli_options(base, override)

  result.model
  |> should.equal(Some("opus"))
}

pub fn merge_model_base_when_override_none_test() {
  let base =
    options.cli_options()
    |> options.with_model("sonnet")
  let override = options.cli_options()

  let result = options.merge_cli_options(base, override)

  result.model
  |> should.equal(Some("sonnet"))
}

pub fn merge_max_turns_override_wins_test() {
  let base =
    options.cli_options()
    |> options.with_max_turns(5)
  let override =
    options.cli_options()
    |> options.with_max_turns(10)

  let result = options.merge_cli_options(base, override)

  result.max_turns
  |> should.equal(Some(10))
}

pub fn merge_system_prompt_override_wins_test() {
  let base =
    options.cli_options()
    |> options.with_system_prompt("base prompt")
  let override =
    options.cli_options()
    |> options.with_system_prompt("override prompt")

  let result = options.merge_cli_options(base, override)

  result.system_prompt
  |> should.equal(Some("override prompt"))
}

pub fn merge_cwd_override_wins_test() {
  let base =
    options.cli_options()
    |> options.with_cwd("/base/path")
  let override =
    options.cli_options()
    |> options.with_cwd("/override/path")

  let result = options.merge_cli_options(base, override)

  result.cwd
  |> should.equal(Some("/override/path"))
}

pub fn merge_continue_session_or_test() {
  // Bool fields should OR (True in either = True)
  let base = CliOptions(..options.cli_options(), continue_session: True)
  let override = CliOptions(..options.cli_options(), continue_session: False)

  let result = options.merge_cli_options(base, override)

  result.continue_session
  |> should.be_true
}

// =============================================================================
// Dict Deep Merge Tests (env, settings)
// =============================================================================

pub fn merge_env_deep_merge_test() {
  let base_env = dict.from_list([#("FOO", "1"), #("BAR", "base")])
  let override_env = dict.from_list([#("BAR", "override"), #("BAZ", "3")])

  let base =
    options.cli_options()
    |> options.with_env(base_env)
  let override =
    options.cli_options()
    |> options.with_env(override_env)

  let result = options.merge_cli_options(base, override)

  // Result should have FOO=1, BAR=override, BAZ=3
  case result.env {
    Some(env) -> {
      dict.get(env, "FOO") |> should.equal(Ok("1"))
      dict.get(env, "BAR") |> should.equal(Ok("override"))
      dict.get(env, "BAZ") |> should.equal(Ok("3"))
    }
    None -> should.fail()
  }
}

pub fn merge_env_base_only_test() {
  let base_env = dict.from_list([#("FOO", "1")])

  let base =
    options.cli_options()
    |> options.with_env(base_env)
  let override = options.cli_options()

  let result = options.merge_cli_options(base, override)

  case result.env {
    Some(env) -> dict.get(env, "FOO") |> should.equal(Ok("1"))
    None -> should.fail()
  }
}

pub fn merge_env_override_only_test() {
  let override_env = dict.from_list([#("BAZ", "3")])

  let base = options.cli_options()
  let override =
    options.cli_options()
    |> options.with_env(override_env)

  let result = options.merge_cli_options(base, override)

  case result.env {
    Some(env) -> dict.get(env, "BAZ") |> should.equal(Ok("3"))
    None -> should.fail()
  }
}

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> dynamic.Dynamic

pub fn merge_settings_deep_merge_test() {
  let base_settings =
    dict.from_list([
      #("key1", to_dynamic("val1")),
      #("key2", to_dynamic("base")),
    ])
  let override_settings =
    dict.from_list([
      #("key2", to_dynamic("override")),
      #("key3", to_dynamic("val3")),
    ])

  let base =
    options.cli_options()
    |> options.with_settings(base_settings)
  let override =
    options.cli_options()
    |> options.with_settings(override_settings)

  let result = options.merge_cli_options(base, override)

  case result.settings {
    Some(settings) -> {
      // Should have key1, key2 (overridden), key3
      dict.size(settings)
      |> should.equal(3)
      dict.has_key(settings, "key1")
      |> should.be_true
      dict.has_key(settings, "key2")
      |> should.be_true
      dict.has_key(settings, "key3")
      |> should.be_true
    }
    None -> should.fail()
  }
}

// =============================================================================
// List Append Tests (add_dirs, extra_args)
// =============================================================================

pub fn merge_add_dirs_append_test() {
  let base =
    options.cli_options()
    |> options.with_add_dirs(["/base/dir1", "/base/dir2"])
  let override =
    options.cli_options()
    |> options.with_add_dirs(["/override/dir3"])

  let result = options.merge_cli_options(base, override)

  case result.add_dirs {
    Some(dirs) ->
      dirs
      |> should.equal(["/base/dir1", "/base/dir2", "/override/dir3"])
    None -> should.fail()
  }
}

pub fn merge_add_dirs_base_only_test() {
  let base =
    options.cli_options()
    |> options.with_add_dirs(["/base/dir1"])
  let override = options.cli_options()

  let result = options.merge_cli_options(base, override)

  case result.add_dirs {
    Some(dirs) -> dirs |> should.equal(["/base/dir1"])
    None -> should.fail()
  }
}

pub fn merge_extra_args_append_test() {
  let base =
    options.cli_options()
    |> options.with_extra_args(["--arg1", "--arg2"])
  let override =
    options.cli_options()
    |> options.with_extra_args(["--arg3"])

  let result = options.merge_cli_options(base, override)

  case result.extra_args {
    Some(args) -> args |> should.equal(["--arg1", "--arg2", "--arg3"])
    None -> should.fail()
  }
}

// =============================================================================
// List Replace Tests (betas)
// =============================================================================

pub fn merge_betas_replace_with_override_test() {
  let base =
    options.cli_options()
    |> options.with_betas(["beta1", "beta2"])
  let override =
    options.cli_options()
    |> options.with_betas(["beta3"])

  let result = options.merge_cli_options(base, override)

  case result.betas {
    Some(betas) -> betas |> should.equal(["beta3"])
    None -> should.fail()
  }
}

pub fn merge_betas_base_when_override_none_test() {
  let base =
    options.cli_options()
    |> options.with_betas(["beta1", "beta2"])
  let override = options.cli_options()

  let result = options.merge_cli_options(base, override)

  case result.betas {
    Some(betas) -> betas |> should.equal(["beta1", "beta2"])
    None -> should.fail()
  }
}

/// Regression test: Some([]) should NOT replace base (spec says "non-empty wins")
pub fn merge_betas_base_when_override_empty_list_test() {
  let base =
    options.cli_options()
    |> options.with_betas(["beta1", "beta2"])
  let override =
    options.cli_options()
    |> options.with_betas([])

  let result = options.merge_cli_options(base, override)

  // Empty list override should NOT replace base
  case result.betas {
    Some(betas) -> betas |> should.equal(["beta1", "beta2"])
    None -> should.fail()
  }
}

// =============================================================================
// BidirOptions Merge Tests (agents - replace behavior)
// =============================================================================

pub fn merge_bidir_agents_replace_with_override_test() {
  let agent1 = options.agent_config("agent1", "desc1", "prompt1")
  let agent2 = options.agent_config("agent2", "desc2", "prompt2")

  let base =
    options.bidir_options()
    |> options.with_agents([agent1])
  let override =
    options.bidir_options()
    |> options.with_agents([agent2])

  let result = options.merge_bidir_options(base, override)

  case result.agents {
    Some(agents) -> {
      case agents {
        [agent] -> agent.name |> should.equal("agent2")
        _ -> should.fail()
      }
    }
    None -> should.fail()
  }
}

pub fn merge_bidir_agents_base_when_override_none_test() {
  let agent1 = options.agent_config("agent1", "desc1", "prompt1")

  let base =
    options.bidir_options()
    |> options.with_agents([agent1])
  let override = options.bidir_options()

  let result = options.merge_bidir_options(base, override)

  case result.agents {
    Some(agents) -> {
      case agents {
        [agent] -> agent.name |> should.equal("agent1")
        _ -> should.fail()
      }
    }
    None -> should.fail()
  }
}

/// Regression test: Some([]) should NOT replace base for agents
pub fn merge_bidir_agents_base_when_override_empty_list_test() {
  let agent1 = options.agent_config("agent1", "desc1", "prompt1")

  let base =
    options.bidir_options()
    |> options.with_agents([agent1])
  let override =
    options.bidir_options()
    |> options.with_agents([])

  let result = options.merge_bidir_options(base, override)

  // Empty list override should NOT replace base
  case result.agents {
    Some(agents) -> {
      case agents {
        [agent] -> agent.name |> should.equal("agent1")
        _ -> should.fail()
      }
    }
    None -> should.fail()
  }
}

// =============================================================================
// BidirOptions Scalar Field Precedence Tests
// =============================================================================

pub fn merge_bidir_timeout_override_wins_test() {
  let base =
    options.bidir_options()
    |> options.with_timeout(1000)
  let override =
    options.bidir_options()
    |> options.with_timeout(5000)

  let result = options.merge_bidir_options(base, override)

  result.timeout_ms
  |> should.equal(Some(5000))
}

pub fn merge_bidir_fork_session_override_wins_test() {
  let base =
    options.bidir_options()
    |> options.with_fork_session("session-base")
  let override =
    options.bidir_options()
    |> options.with_fork_session("session-override")

  let result = options.merge_bidir_options(base, override)

  result.fork_session
  |> should.equal(Some("session-override"))
}

pub fn merge_bidir_max_thinking_tokens_override_wins_test() {
  let base =
    options.bidir_options()
    |> options.with_max_thinking_tokens(100)
  let override =
    options.bidir_options()
    |> options.with_max_thinking_tokens(500)

  let result = options.merge_bidir_options(base, override)

  result.max_thinking_tokens
  |> should.equal(Some(500))
}

pub fn merge_bidir_include_partial_messages_or_test() {
  // Bool fields should OR
  let base =
    options.bidir_options()
    |> options.with_partial_messages
  let override = options.bidir_options()

  let result = options.merge_bidir_options(base, override)

  result.include_partial_messages
  |> should.be_true
}

// =============================================================================
// BidirOptions Dict Deep Merge Tests (hook_timeouts)
// =============================================================================

pub fn merge_bidir_hook_timeouts_deep_merge_test() {
  let base =
    options.bidir_options()
    |> options.with_hook_timeout(hook.PreToolUse, 1000)
    |> options.with_hook_timeout(hook.PostToolUse, 2000)
  let override =
    options.bidir_options()
    |> options.with_hook_timeout(hook.PostToolUse, 5000)
    |> options.with_hook_timeout(hook.Stop, 3000)

  let result = options.merge_bidir_options(base, override)

  // Should have: pre_tool_use=1000, post_tool_use=5000 (overridden), stop=3000
  dict.get(result.hook_timeouts, hook.PreToolUse)
  |> should.equal(Ok(1000))
  dict.get(result.hook_timeouts, hook.PostToolUse)
  |> should.equal(Ok(5000))
  dict.get(result.hook_timeouts, hook.Stop)
  |> should.equal(Ok(3000))
}

// =============================================================================
// BidirOptions List Append Tests (mcp_servers, plugins)
// =============================================================================

pub fn merge_bidir_mcp_servers_append_test() {
  let handler1 = fn(d: dynamic.Dynamic) -> dynamic.Dynamic { d }
  let handler2 = fn(d: dynamic.Dynamic) -> dynamic.Dynamic { d }

  let base =
    options.bidir_options()
    |> options.with_mcp_server("server1", handler1)
  let override =
    options.bidir_options()
    |> options.with_mcp_server("server2", handler2)

  let result = options.merge_bidir_options(base, override)

  case result.mcp_servers {
    [#("server1", _), #("server2", _)] -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn merge_bidir_plugins_append_test() {
  let base =
    options.bidir_options()
    |> options.with_plugins(["plugin1", "plugin2"])
  let override =
    options.bidir_options()
    |> options.with_plugins(["plugin3"])

  let result = options.merge_bidir_options(base, override)

  case result.plugins {
    Some(plugins) -> plugins |> should.equal(["plugin1", "plugin2", "plugin3"])
    None -> should.fail()
  }
}

// =============================================================================
// BidirOptions List Replace Tests (setting_sources)
// =============================================================================

pub fn merge_bidir_setting_sources_replace_test() {
  let base =
    options.bidir_options()
    |> options.with_setting_sources(["source1", "source2"])
  let override =
    options.bidir_options()
    |> options.with_setting_sources(["source3"])

  let result = options.merge_bidir_options(base, override)

  case result.setting_sources {
    Some(sources) -> sources |> should.equal(["source3"])
    None -> should.fail()
  }
}

// =============================================================================
// Edge Case Tests
// =============================================================================

pub fn merge_cli_options_both_empty_test() {
  let base = options.cli_options()
  let override = options.cli_options()

  let result = options.merge_cli_options(base, override)

  result.model |> should.equal(None)
  result.env |> should.equal(None)
  result.add_dirs |> should.equal(None)
  result.betas |> should.equal(None)
}

pub fn merge_bidir_options_both_empty_test() {
  let base = options.bidir_options()
  let override = options.bidir_options()

  let result = options.merge_bidir_options(base, override)

  result.timeout_ms |> should.equal(None)
  result.agents |> should.equal(None)
  result.plugins |> should.equal(None)
}

// =============================================================================
// Multi-Level Precedence Tests (simulating config -> env -> function args)
// =============================================================================

pub fn three_level_precedence_test() {
  // Simulate: config (lowest) -> env vars (middle) -> function args (highest)
  let config_level =
    options.cli_options()
    |> options.with_model("config-model")
    |> options.with_max_turns(3)
    |> options.with_env(dict.from_list([#("CONFIG_VAR", "config")]))

  let env_level =
    options.cli_options()
    |> options.with_model("env-model")
    |> options.with_env(dict.from_list([#("ENV_VAR", "env")]))

  let function_args =
    options.cli_options()
    |> options.with_model("function-model")

  // Merge: config <- env <- function_args
  let merged_1 = options.merge_cli_options(config_level, env_level)
  let final = options.merge_cli_options(merged_1, function_args)

  // Function args should win for model
  final.model |> should.equal(Some("function-model"))

  // Env level should win for max_turns (function_args has None)
  // But env_level also has None, so config_level's value should persist
  final.max_turns |> should.equal(Some(3))

  // Env should have all three vars merged
  case final.env {
    Some(env) -> {
      dict.get(env, "CONFIG_VAR") |> should.equal(Ok("config"))
      dict.get(env, "ENV_VAR") |> should.equal(Ok("env"))
    }
    None -> should.fail()
  }
}

// =============================================================================
// Acceptance Criteria Tests
// =============================================================================

/// AC1: Given model set in config file as "sonnet" and function arg as "opus",
/// when session starts, then CLI receives --model opus (function arg wins)
pub fn acceptance_criteria_1_model_precedence_test() {
  let config =
    options.cli_options()
    |> options.with_model("sonnet")
  let function_arg =
    options.cli_options()
    |> options.with_model("opus")

  let result = options.merge_cli_options(config, function_arg)

  result.model |> should.equal(Some("opus"))
}

/// AC2: Given env var FOO=1 in config and BAR=2 in function arg,
/// when session starts, then CLI environment contains both FOO=1 and BAR=2
pub fn acceptance_criteria_2_env_merge_test() {
  let config =
    options.cli_options()
    |> options.with_env(dict.from_list([#("FOO", "1")]))
  let function_arg =
    options.cli_options()
    |> options.with_env(dict.from_list([#("BAR", "2")]))

  let result = options.merge_cli_options(config, function_arg)

  case result.env {
    Some(env) -> {
      dict.get(env, "FOO") |> should.equal(Ok("1"))
      dict.get(env, "BAR") |> should.equal(Ok("2"))
    }
    None -> should.fail()
  }
}
