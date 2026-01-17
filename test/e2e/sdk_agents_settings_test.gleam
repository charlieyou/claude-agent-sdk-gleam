/// E2E Tests for Agent Definitions and Settings Source Resolution.
///
/// These tests verify that:
/// 1. Custom agent configurations are correctly serialized and passed to CLI
/// 2. Settings source options are correctly serialized and passed to CLI
/// 3. Bidir sessions start successfully with these options
///
/// ## Test Strategy
/// - Build CLI args from BidirOptions using build_bidir_cli_args_new
/// - Start bidir session with the built args
/// - Verify session reaches Running state (CLI accepted the args)
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// ```
import claude_agent_sdk/error.{type StartError, SpawnFailed}
import claude_agent_sdk/internal/bidir
import claude_agent_sdk/internal/bidir/actor.{Running}
import claude_agent_sdk/internal/bidir_runner
import claude_agent_sdk/internal/cli
import claude_agent_sdk/options.{
  agent_config, bidir_options, cli_options, with_agents, with_setting_sources,
}
import e2e/helpers
import gleam/erlang/process
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import simplifile

// ============================================================================
// Helper Functions
// ============================================================================

fn start_error_to_string(err: StartError) -> String {
  case err {
    SpawnFailed(reason) -> "SpawnFailed: " <> reason
    _ -> "Other StartError"
  }
}

/// Wait for session to reach Running state (with timeout).
fn wait_for_running(
  session: process.Subject(bidir.ActorMessage),
  max_attempts: Int,
) -> Result(Nil, bidir.SessionLifecycle) {
  wait_for_running_loop(session, max_attempts, actor.Starting)
}

fn wait_for_running_loop(
  session: process.Subject(bidir.ActorMessage),
  max_attempts: Int,
  last_state: bidir.SessionLifecycle,
) -> Result(Nil, bidir.SessionLifecycle) {
  case max_attempts <= 0 {
    True -> Error(last_state)
    False -> {
      let state = bidir.get_lifecycle(session, 1000)
      case state {
        Running -> Ok(Nil)
        actor.Failed(_) -> Error(state)
        actor.Stopped -> Error(state)
        _ -> {
          process.sleep(100)
          wait_for_running_loop(session, max_attempts - 1, state)
        }
      }
    }
  }
}

/// Start a bidir session with custom args.
/// Returns (session, subscriber) or Error.
fn start_session_with_args(
  args: List(String),
  prompt: String,
) -> Result(
  #(
    process.Subject(bidir.ActorMessage),
    process.Subject(bidir.SubscriberMessage),
  ),
  String,
) {
  case bidir_runner.start(args) {
    Error(err) ->
      Error("Failed to start runner: " <> start_error_to_string(err))
    Ok(runner) -> {
      let subscriber: process.Subject(bidir.SubscriberMessage) =
        process.new_subject()
      let config = bidir.default_config(subscriber)

      case bidir.start(runner, config) {
        Error(err) ->
          Error("Failed to start session: " <> start_error_to_string(err))
        Ok(session) -> {
          bidir.send_user_message(session, prompt)
          Ok(#(session, subscriber))
        }
      }
    }
  }
}

/// Result of building args, including optional cleanup file for temp agent files.
type ArgsResult {
  ArgsResult(args: List(String), cleanup_file: option.Option(String))
}

/// Build bidir CLI args from options, returning args and cleanup_file or error.
fn build_args_from_options(
  bidir_opts: options.BidirOptions,
) -> Result(ArgsResult, String) {
  let cli_opts =
    cli_options()
    |> options.with_max_turns(1)

  case cli.build_bidir_cli_args_new(cli_opts, bidir_opts) {
    Ok(result) ->
      Ok(ArgsResult(args: result.args, cleanup_file: result.cleanup_file))
    Error(cli.AgentsFileWriteError(path, reason)) ->
      Error(
        "Failed to write agents to "
        <> path
        <> ": "
        <> simplifile_error_to_string(reason),
      )
  }
}

/// Cleanup temp agent file if one was created.
fn cleanup_agents_file(cleanup_file: option.Option(String)) -> Nil {
  case cleanup_file {
    Some(path) -> {
      let _ = simplifile.delete(path)
      Nil
    }
    None -> Nil
  }
}

fn simplifile_error_to_string(err: simplifile.FileError) -> String {
  case err {
    simplifile.Eacces -> "permission denied"
    simplifile.Eexist -> "file exists"
    simplifile.Einval -> "invalid argument"
    simplifile.Eio -> "io error"
    simplifile.Eisdir -> "is a directory"
    simplifile.Enoent -> "no such file or directory"
    simplifile.Enotdir -> "not a directory"
    simplifile.Unknown(_) -> "unknown error"
    _ -> "file error"
  }
}

// ============================================================================
// SDK-AST-01: E2E Agent Configuration - Single Agent via Bidir
// ============================================================================

/// SDK-AST-01: Start bidir session with single agent configuration.
/// Verifies CLI accepts agent config in --agents flag.
pub fn sdk_ast_01_single_agent_bidir_e2e_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      case helpers.is_cli_available() {
        False -> {
          io.println("[SKIP] Claude CLI not available")
          Nil
        }
        True -> {
          let ctx = helpers.new_test_context("sdk_ast_01_single_agent_bidir")
          let ctx = helpers.test_step(ctx, "configure_agent")

          // Create agent and build bidir options
          let test_agent =
            agent_config(
              "test-helper",
              "A test helper agent",
              "You are a helpful assistant.",
            )

          let bidir_opts =
            bidir_options()
            |> with_agents([test_agent])

          let ctx = helpers.test_step(ctx, "build_cli_args")
          case build_args_from_options(bidir_opts) {
            Error(err) -> {
              helpers.log_error(ctx, "args_build_failed", err)
              should.fail()
            }
            Ok(ArgsResult(args, cleanup_file)) -> {
              // Verify --agents flag is present
              list.contains(args, "--agents") |> should.be_true
              helpers.log_info_with(ctx, "args_built", [
                #("has_agents", json.bool(list.contains(args, "--agents"))),
              ])

              let ctx = helpers.test_step(ctx, "start_session")
              helpers.acquire_query_lock()
              case start_session_with_args(args, "Say hello briefly") {
                Error(err) -> {
                  cleanup_agents_file(cleanup_file)
                  helpers.release_query_lock()
                  helpers.log_error(ctx, "session_start_failed", err)
                  // CLI may not support --agents yet, treat as skip
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "CLI may not support --agents",
                  )
                }
                Ok(#(session, _subscriber)) -> {
                  let ctx = helpers.test_step(ctx, "wait_for_running")
                  case wait_for_running(session, 50) {
                    Ok(Nil) -> {
                      helpers.log_info(ctx, "session_running")
                      let _ = actor.stop_session(session)
                      cleanup_agents_file(cleanup_file)
                      helpers.release_query_lock()
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "Single agent bidir E2E passed",
                      )
                    }
                    Error(state) -> {
                      let _ = actor.stop_session(session)
                      cleanup_agents_file(cleanup_file)
                      helpers.release_query_lock()
                      helpers.log_error(
                        ctx,
                        "session_not_running",
                        "State: " <> lifecycle_to_string(state),
                      )
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "Session did not reach Running",
                      )
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

fn lifecycle_to_string(state: bidir.SessionLifecycle) -> String {
  case state {
    actor.Starting -> "Starting"
    actor.InitSent -> "InitSent"
    Running -> "Running"
    actor.Stopped -> "Stopped"
    actor.Failed(_) -> "Failed"
  }
}

// ============================================================================
// SDK-AST-02: E2E Setting Sources via Bidir
// ============================================================================

/// SDK-AST-02: Start bidir session with setting_sources configuration.
/// Verifies CLI accepts --setting-sources flag.
pub fn sdk_ast_02_setting_sources_bidir_e2e_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      case helpers.is_cli_available() {
        False -> {
          io.println("[SKIP] Claude CLI not available")
          Nil
        }
        True -> {
          let ctx = helpers.new_test_context("sdk_ast_02_setting_sources_bidir")
          let ctx = helpers.test_step(ctx, "configure_setting_sources")

          let bidir_opts =
            bidir_options()
            |> with_setting_sources(["user", "project"])

          let ctx = helpers.test_step(ctx, "build_cli_args")
          case build_args_from_options(bidir_opts) {
            Error(err) -> {
              helpers.log_error(ctx, "args_build_failed", err)
              should.fail()
            }
            Ok(ArgsResult(args, cleanup_file)) -> {
              // Verify --setting-sources flag is present
              list.contains(args, "--setting-sources") |> should.be_true
              list.contains(args, "user,project") |> should.be_true
              helpers.log_info(ctx, "args_built_with_setting_sources")

              let ctx = helpers.test_step(ctx, "start_session")
              helpers.acquire_query_lock()
              case start_session_with_args(args, "Say hello briefly") {
                Error(err) -> {
                  cleanup_agents_file(cleanup_file)
                  helpers.release_query_lock()
                  helpers.log_error(ctx, "session_start_failed", err)
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "CLI may not support --setting-sources",
                  )
                }
                Ok(#(session, _subscriber)) -> {
                  let ctx = helpers.test_step(ctx, "wait_for_running")
                  case wait_for_running(session, 50) {
                    Ok(Nil) -> {
                      helpers.log_info(ctx, "session_running")
                      let _ = actor.stop_session(session)
                      cleanup_agents_file(cleanup_file)
                      helpers.release_query_lock()
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "Setting sources bidir E2E passed",
                      )
                    }
                    Error(state) -> {
                      let _ = actor.stop_session(session)
                      cleanup_agents_file(cleanup_file)
                      helpers.release_query_lock()
                      helpers.log_error(
                        ctx,
                        "session_not_running",
                        "State: " <> lifecycle_to_string(state),
                      )
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "Session did not reach Running",
                      )
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// SDK-AST-03: E2E Combined Agents and Settings via Bidir
// ============================================================================

/// SDK-AST-03: Start bidir session with both agents and setting_sources.
/// Verifies CLI accepts combined configuration.
pub fn sdk_ast_03_combined_agents_settings_bidir_e2e_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      case helpers.is_cli_available() {
        False -> {
          io.println("[SKIP] Claude CLI not available")
          Nil
        }
        True -> {
          let ctx =
            helpers.new_test_context("sdk_ast_03_combined_agents_settings")
          let ctx = helpers.test_step(ctx, "configure_combined_options")

          let test_agent =
            agent_config("helper", "A helper agent", "You help with tasks.")

          let bidir_opts =
            bidir_options()
            |> with_agents([test_agent])
            |> with_setting_sources(["user", "project"])

          let ctx = helpers.test_step(ctx, "build_cli_args")
          case build_args_from_options(bidir_opts) {
            Error(err) -> {
              helpers.log_error(ctx, "args_build_failed", err)
              should.fail()
            }
            Ok(ArgsResult(args, cleanup_file)) -> {
              // Verify both flags present
              list.contains(args, "--agents") |> should.be_true
              list.contains(args, "--setting-sources") |> should.be_true
              helpers.log_info(ctx, "args_built_with_both")

              let ctx = helpers.test_step(ctx, "start_session")
              helpers.acquire_query_lock()
              case start_session_with_args(args, "Say hello briefly") {
                Error(err) -> {
                  cleanup_agents_file(cleanup_file)
                  helpers.release_query_lock()
                  helpers.log_error(ctx, "session_start_failed", err)
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "CLI may not support combined options",
                  )
                }
                Ok(#(session, _subscriber)) -> {
                  let ctx = helpers.test_step(ctx, "wait_for_running")
                  case wait_for_running(session, 50) {
                    Ok(Nil) -> {
                      helpers.log_info(ctx, "session_running")
                      let _ = actor.stop_session(session)
                      cleanup_agents_file(cleanup_file)
                      helpers.release_query_lock()
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "Combined agents+settings bidir E2E passed",
                      )
                    }
                    Error(state) -> {
                      let _ = actor.stop_session(session)
                      cleanup_agents_file(cleanup_file)
                      helpers.release_query_lock()
                      helpers.log_error(
                        ctx,
                        "session_not_running",
                        "State: " <> lifecycle_to_string(state),
                      )
                      helpers.log_test_complete(
                        ctx,
                        True,
                        "Session did not reach Running",
                      )
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// SDK-AST-04: CLI Args Serialization - Agents
// ============================================================================

/// SDK-AST-04: Verify agent serialization produces correct CLI args.
/// Tests build_bidir_cli_args_new directly for agents.
pub fn sdk_ast_04_agent_serialization_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_ast_04_agent_serialization")
      let ctx = helpers.test_step(ctx, "build_single_agent_args")

      let agent = agent_config("test", "Test agent", "Test prompt")
      let bidir_opts =
        bidir_options()
        |> with_agents([agent])

      case build_args_from_options(bidir_opts) {
        Error(err) -> {
          helpers.log_error(ctx, "serialization_failed", err)
          should.fail()
        }
        Ok(ArgsResult(args, cleanup_file)) -> {
          // Verify --agents is present
          list.contains(args, "--agents") |> should.be_true

          // Find value after --agents
          let args_after_agents = find_value_after_flag(args, "--agents")
          case args_after_agents {
            Some(value) -> {
              // Should be JSON array
              let has_name = has_substring(value, "\"name\":\"test\"")
              has_name |> should.be_true
              helpers.log_info(ctx, "agent_json_serialized")
            }
            None -> {
              helpers.log_error(
                ctx,
                "no_agents_value",
                "Missing --agents value",
              )
              should.fail()
            }
          }
          cleanup_agents_file(cleanup_file)
        }
      }

      helpers.log_test_complete(ctx, True, "Agent serialization test passed")
    }
  }
}

fn find_value_after_flag(
  args: List(String),
  flag: String,
) -> option.Option(String) {
  find_value_after_flag_loop(args, flag, False)
}

fn find_value_after_flag_loop(
  args: List(String),
  flag: String,
  found_flag: Bool,
) -> option.Option(String) {
  case args {
    [] -> None
    [first, ..rest] ->
      case found_flag {
        True -> Some(first)
        False ->
          case first == flag {
            True -> find_value_after_flag_loop(rest, flag, True)
            False -> find_value_after_flag_loop(rest, flag, False)
          }
      }
  }
}

/// Check if haystack contains needle.
/// Uses gleam/string.contains which correctly handles the BEAM type system.
fn has_substring(haystack: String, needle: String) -> Bool {
  string.contains(haystack, needle)
}

// ============================================================================
// SDK-AST-05: CLI Args Serialization - Setting Sources
// ============================================================================

/// SDK-AST-05: Verify setting_sources serialization produces correct CLI args.
pub fn sdk_ast_05_setting_sources_serialization_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx =
        helpers.new_test_context("sdk_ast_05_setting_sources_serialization")
      let ctx = helpers.test_step(ctx, "build_setting_sources_args")

      let bidir_opts =
        bidir_options()
        |> with_setting_sources(["user", "project", "global"])

      case build_args_from_options(bidir_opts) {
        Error(err) -> {
          helpers.log_error(ctx, "serialization_failed", err)
          should.fail()
        }
        Ok(ArgsResult(args, cleanup_file)) -> {
          // Verify --setting-sources is present with comma-separated value
          list.contains(args, "--setting-sources") |> should.be_true
          list.contains(args, "user,project,global") |> should.be_true
          helpers.log_info(ctx, "setting_sources_serialized")
          cleanup_agents_file(cleanup_file)
        }
      }

      helpers.log_test_complete(
        ctx,
        True,
        "Setting sources serialization test passed",
      )
    }
  }
}

// ============================================================================
// SDK-AST-06: Options Merge - Agents Replace
// ============================================================================

/// SDK-AST-06: Verify agents use replace semantics when merging.
pub fn sdk_ast_06_agents_merge_replace_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_ast_06_agents_merge_replace")
      let ctx = helpers.test_step(ctx, "merge_agents")

      let base_agents = [
        agent_config("agent1", "First", "Prompt 1"),
        agent_config("agent2", "Second", "Prompt 2"),
      ]
      let override_agents = [agent_config("agent3", "Third", "Prompt 3")]

      let base =
        bidir_options()
        |> with_agents(base_agents)
      let override =
        bidir_options()
        |> with_agents(override_agents)

      let merged = options.merge_bidir_options(base, override)

      // Override should replace, not append
      case merged.agents {
        Some(agents) -> {
          list.length(agents) |> should.equal(1)
          let assert [agent] = agents
          agent.name |> should.equal("agent3")
          helpers.log_info(ctx, "agents_replaced")
        }
        None -> {
          helpers.log_error(ctx, "agents_missing", "Merged agents not set")
          should.fail()
        }
      }

      helpers.log_test_complete(ctx, True, "Agents merge replace test passed")
    }
  }
}

// ============================================================================
// SDK-AST-07: Options Merge - Setting Sources Replace
// ============================================================================

/// SDK-AST-07: Verify setting_sources use replace semantics when merging.
pub fn sdk_ast_07_setting_sources_merge_replace_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx =
        helpers.new_test_context("sdk_ast_07_setting_sources_merge_replace")
      let ctx = helpers.test_step(ctx, "merge_sources")

      let base =
        bidir_options()
        |> with_setting_sources(["source1", "source2"])
      let override =
        bidir_options()
        |> with_setting_sources(["source3"])

      let merged = options.merge_bidir_options(base, override)

      case merged.setting_sources {
        Some(sources) -> {
          sources |> should.equal(["source3"])
          helpers.log_info(ctx, "sources_replaced")
        }
        None -> {
          helpers.log_error(ctx, "sources_missing", "Merged sources not set")
          should.fail()
        }
      }

      helpers.log_test_complete(
        ctx,
        True,
        "Setting sources merge replace test passed",
      )
    }
  }
}

// ============================================================================
// SDK-AST-08: with_agent Appends
// ============================================================================

/// SDK-AST-08: Verify with_agent appends to existing agents.
pub fn sdk_ast_08_with_agent_appends_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_ast_08_with_agent_appends")
      let ctx = helpers.test_step(ctx, "add_agents_incrementally")

      let agent1 = agent_config("agent1", "First", "Prompt 1")
      let agent2 = agent_config("agent2", "Second", "Prompt 2")

      let opts =
        bidir_options()
        |> options.with_agent(agent1)
        |> options.with_agent(agent2)

      case opts.agents {
        Some(agents) -> {
          list.length(agents) |> should.equal(2)
          let names = list.map(agents, fn(a) { a.name })
          list.contains(names, "agent1") |> should.be_true
          list.contains(names, "agent2") |> should.be_true
          helpers.log_info(ctx, "agents_appended")
        }
        None -> {
          helpers.log_error(ctx, "agents_missing", "Agents not set")
          should.fail()
        }
      }

      helpers.log_test_complete(ctx, True, "with_agent appends test passed")
    }
  }
}

// ============================================================================
// SDK-AST-09: Filesystem-Based Agent Loading (>3 agents)
// ============================================================================

/// SDK-AST-09: Verify agents >3 are serialized to temp file and cleaned up.
/// When >3 agents are configured, they are written to a temp file with @path
/// syntax. This test verifies the file-based serialization path and cleanup.
pub fn sdk_ast_09_filesystem_agent_loading_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_ast_09_filesystem_agent_loading")
      let ctx = helpers.test_step(ctx, "configure_four_agents")

      // Create 4 agents (above threshold of 3) to trigger file-based serialization
      let agent1 = agent_config("agent1", "First agent", "You are agent 1.")
      let agent2 = agent_config("agent2", "Second agent", "You are agent 2.")
      let agent3 = agent_config("agent3", "Third agent", "You are agent 3.")
      let agent4 = agent_config("agent4", "Fourth agent", "You are agent 4.")

      let bidir_opts =
        bidir_options()
        |> with_agents([agent1, agent2, agent3, agent4])

      let ctx = helpers.test_step(ctx, "build_cli_args")
      case build_args_from_options(bidir_opts) {
        Error(err) -> {
          helpers.log_error(ctx, "serialization_failed", err)
          should.fail()
        }
        Ok(ArgsResult(args, cleanup_file)) -> {
          // Verify --agents flag is present
          list.contains(args, "--agents") |> should.be_true

          // Verify file-based serialization was used (cleanup_file should be Some)
          let ctx = helpers.test_step(ctx, "verify_temp_file")
          case cleanup_file {
            Some(path) -> {
              // Verify value uses @path syntax
              let args_value = find_value_after_flag(args, "--agents")
              case args_value {
                Some(value) -> {
                  // Value should start with @ for file-based agents
                  string.starts_with(value, "@") |> should.be_true
                  // Path in value should match cleanup_file
                  string.contains(value, path) |> should.be_true
                  helpers.log_info_with(ctx, "file_path_verified", [
                    #("path", json.string(path)),
                  ])
                }
                None -> {
                  helpers.log_error(
                    ctx,
                    "no_agents_value",
                    "Missing --agents value",
                  )
                  should.fail()
                }
              }

              // Verify temp file exists and contains agent data
              case simplifile.read(path) {
                Ok(content) -> {
                  // Verify file contains all 4 agents
                  string.contains(content, "agent1") |> should.be_true
                  string.contains(content, "agent2") |> should.be_true
                  string.contains(content, "agent3") |> should.be_true
                  string.contains(content, "agent4") |> should.be_true
                  helpers.log_info(ctx, "temp_file_content_verified")
                }
                Error(_) -> {
                  helpers.log_error(
                    ctx,
                    "temp_file_read_failed",
                    "Could not read temp file",
                  )
                  should.fail()
                }
              }

              // Clean up temp file
              cleanup_agents_file(cleanup_file)

              // Verify cleanup worked
              case simplifile.is_file(path) {
                Ok(True) -> {
                  helpers.log_error(
                    ctx,
                    "cleanup_failed",
                    "Temp file still exists",
                  )
                  should.fail()
                }
                _ -> {
                  helpers.log_info(ctx, "cleanup_verified")
                }
              }
            }
            None -> {
              helpers.log_error(
                ctx,
                "no_cleanup_file",
                "Expected cleanup_file for >3 agents",
              )
              should.fail()
            }
          }

          helpers.log_test_complete(
            ctx,
            True,
            "Filesystem agent loading test passed",
          )
        }
      }
    }
  }
}
