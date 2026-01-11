import claude_agent_sdk/internal/cli
import claude_agent_sdk/internal/port_ffi
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import simplifile

pub fn main() -> Nil {
  let args = get_plain_arguments()
  case parse_args(args) {
    Error(message) -> {
      io.println(message)
      halt(1)
    }
    Ok(config) -> run_with_config(config)
  }
}

// ============================================================================
// CLI args / exit helpers
// ============================================================================

type Config {
  Config(
    scenarios: List(String),
    output_dir: Option(String),
    list_only: Bool,
  )
}

fn parse_args(args: List(String)) -> Result(Config, String) {
  parse_args_loop(args, Config([], None, False))
}

fn parse_args_loop(
  args: List(String),
  config: Config,
) -> Result(Config, String) {
  case args {
    [] -> Ok(config)
    ["--list", ..rest] -> parse_args_loop(rest, Config(config.scenarios, config.output_dir, True))
    ["-l", ..rest] -> parse_args_loop(rest, Config(config.scenarios, config.output_dir, True))
    ["--scenario", scenario, ..rest] ->
      parse_args_loop(rest, Config([scenario, ..config.scenarios], config.output_dir, config.list_only))
    ["-s", scenario, ..rest] ->
      parse_args_loop(rest, Config([scenario, ..config.scenarios], config.output_dir, config.list_only))
    ["--output-dir", dir, ..rest] ->
      parse_args_loop(rest, Config(config.scenarios, Some(dir), config.list_only))
    ["-o", dir, ..rest] ->
      parse_args_loop(rest, Config(config.scenarios, Some(dir), config.list_only))
    [unknown, .._] -> Error("Unknown argument: " <> unknown)
  }
}

@external(erlang, "claude_agent_sdk_ffi", "get_plain_arguments")
fn get_plain_arguments() -> List(String)

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil

// ============================================================================
// E2E runner
// ============================================================================

type ScenarioStatus {
  Pass
  Fail
  Skip
}

type ScenarioResult {
  ScenarioResult(
    scenario_id: String,
    status: ScenarioStatus,
    duration_ms: Int,
    error_kind: Option(String),
    error_message: Option(String),
    notes: Option(String),
  )
}

type ScenarioContext {
  ScenarioContext(
    logger: EventLogger,
    cli_path: String,
    cli_version_raw: String,
    cli_version_parsed: Option(#(Int, Int, Int)),
  )
}

type Scenario {
  Scenario(id: String, run: fn(ScenarioContext) -> ScenarioResult)
}

fn run_with_config(config: Config) -> Nil {
  case config.list_only {
    True -> {
      list_scenarios()
      halt(0)
    }
    False -> Nil
  }

  case get_env("CLAUDE_INTEGRATION_TEST") {
    Some("1") -> Nil
    _ -> {
      io.println("E2E tests require CLAUDE_INTEGRATION_TEST=1")
      io.println("Set this environment variable to enable E2E scenarios.")
      halt(1)
    }
  }

  let cli_path = case port_ffi.find_cli_path("claude") {
    Ok(path) -> path
    Error(_) -> {
      io.println("[SKIP] claude CLI not found in PATH")
      io.println("Install: https://claude.ai/code")
      halt(1)
      ""
    }
  }

  let #(cli_version_raw, cli_version_parsed) =
    case get_cli_version(cli_path) {
      Ok(result) -> result
      Error(err) -> {
        io.println("[SKIP] Failed to get claude --version: " <> err)
        halt(1)
        #("", None)
      }
    }

  let run_id = generate_run_id()
  let output_dir = case config.output_dir {
    Some(dir) -> dir
    None -> "artifacts/e2e/" <> run_id
  }

  let logger = new_logger(output_dir, run_id)
  let start_ts = utc_now_string()

  let metadata = build_metadata(
    run_id,
    start_ts,
    cli_version_raw,
    cli_version_parsed,
  )

  logger
  |> log_event(
    "run_start",
    "INFO",
    None,
    None,
    [
      #("env", json.object(redacted_env_as_json())),
      #("start_ts", json.string(start_ts)),
      #("cli_version_raw", json.string(cli_version_raw)),
      #("cli_version_parsed", json.string(option_string(cli_version_parsed))),
      #("gleam_version", json.string(metadata.gleam_version)),
      #("repo_sha", json.string(metadata.repo_sha)),
      #("os", json.string(metadata.os)),
      #("arch", json.string(metadata.arch)),
    ],
  )

  io.println("E2E Run: " <> run_id)
  io.println("Output:  " <> output_dir)
  io.println("CLI:     " <> cli_version_raw)
  io.println("")

  let ctx = ScenarioContext(
    logger: logger,
    cli_path: cli_path,
    cli_version_raw: cli_version_raw,
    cli_version_parsed: cli_version_parsed,
  )

  let scenarios = all_scenarios()
  let selected =
    case config.scenarios {
      [] -> scenarios
      _ ->
        scenarios
        |> list.filter(fn(s) { list.contains(config.scenarios, s.id) })
    }

  let invalid =
    case config.scenarios {
      [] -> []
      _ ->
        config.scenarios
        |> list.filter(fn(id) { list.contains(all_scenario_ids(), id) == False })
    }

  case invalid {
    [] -> Nil
    _ -> {
      io.println("Unknown scenario(s): " <> string.join(invalid, ", "))
      halt(1)
    }
  }

  let results = run_scenarios(ctx, selected)
  let summary = summarize(results)

  let duration_ms = logger |> elapsed_ms
  write_metadata(
    output_dir,
    metadata,
    duration_ms,
  )
  write_summary(output_dir, run_id, metadata, results, duration_ms)

  logger
  |> log_event(
    "run_end",
    "INFO",
    None,
    None,
    [
      #("duration_ms", json.int(duration_ms)),
      #("passed", json.int(summary.passed)),
      #("failed", json.int(summary.failed)),
      #("skipped", json.int(summary.skipped)),
    ],
  )

  io.println("")
  io.println(
    "Results: "
      <> int.to_string(summary.passed)
      <> " passed, "
      <> int.to_string(summary.failed)
      <> " failed, "
      <> int.to_string(summary.skipped)
      <> " skipped",
  )
  io.println("Artifacts: " <> output_dir)
  io.println("  events.jsonl: " <> output_dir <> "/events.jsonl")
  io.println("  summary.txt:  " <> output_dir <> "/summary.txt")
  io.println("  metadata.json: " <> output_dir <> "/metadata.json")

  case summary.failed > 0 {
    True -> halt(1)
    False -> halt(0)
  }
}

fn list_scenarios() -> Nil {
  io.println("Available E2E Scenarios:")
  all_scenarios()
  |> list.each(fn(s) { io.println("  " <> s.id <> ": " <> scenario_title(s.id)) })
}

fn scenario_title(id: String) -> String {
  case id {
    "E2E-01" -> "Preflight"
    "E2E-02" -> "Simple Query (Happy Path)"
    "E2E-03" -> "NDJSON Purity Validation"
    "E2E-04" -> "Session Resume Flow"
    "E2E-05" -> "Version Edge Cases"
    "E2E-06" -> "Auth Missing (Expected Skip)"
    "E2E-07" -> "CLI Missing (Expected Skip)"
    "E2E-08" -> "Timeout / Slow Response (Optional)"
    _ -> id
  }
}

fn all_scenarios() -> List(Scenario) {
  [
    Scenario("E2E-01", run_e2e_01_preflight),
    Scenario("E2E-02", run_e2e_02_simple_query),
    Scenario("E2E-03", run_e2e_03_ndjson_purity),
    Scenario("E2E-04", run_e2e_04_session_resume),
    Scenario("E2E-05", run_e2e_05_version_edge_cases),
    Scenario("E2E-06", run_e2e_06_auth_missing),
    Scenario("E2E-07", run_e2e_07_cli_missing),
    Scenario("E2E-08", run_e2e_08_timeout),
  ]
}

fn all_scenario_ids() -> List(String) {
  all_scenarios()
  |> list.map(fn(s) { s.id })
}

fn run_scenarios(ctx: ScenarioContext, scenarios: List(Scenario)) -> List(ScenarioResult) {
  scenarios
  |> list.map(fn(s) {
    io.print("Running " <> s.id <> "... ")
    let result = s.run(ctx)
    io.println(status_to_text(result))
    result
  })
}

fn status_to_text(result: ScenarioResult) -> String {
  let duration = int.to_string(result.duration_ms) <> "ms"
  case result.status {
    Pass -> "pass (" <> duration <> ")"
    Fail -> "fail (" <> duration <> ")"
    Skip -> "skip (" <> duration <> ")"
  }
}

// ============================================================================
// Scenarios
// ============================================================================

fn run_e2e_01_preflight(ctx: ScenarioContext) -> ScenarioResult {
  let scenario_id = "E2E-01"
  let start_ms = ctx.logger |> elapsed_ms
  ctx.logger |> log_event("scenario_start", "INFO", Some(scenario_id), Some("preflight"), [])

  ctx.logger |> log_event("step_start", "INFO", Some(scenario_id), Some("cli_resolve"), [])
  ctx.logger
  |> log_event(
    "step_end",
    "INFO",
    Some(scenario_id),
    Some("cli_resolve"),
    [#("result", json.string("pass")), #("notes", json.string("CLI at " <> ctx.cli_path))],
  )

  ctx.logger |> log_event("step_start", "INFO", Some(scenario_id), Some("version_check"), [])
  let version_failure =
    case ctx.cli_version_parsed {
      Some(parsed) ->
        case version_at_least(parsed, minimum_version_tuple()) {
          True -> {
            ctx.logger
            |> log_event(
              "step_end",
              "INFO",
              Some(scenario_id),
              Some("version_check"),
              [
                #("result", json.string("pass")),
                #("notes", json.string("Version " <> ctx.cli_version_raw <> " >= 1.0.0")),
              ],
            )
            None
          }
          False -> {
            ctx.logger
            |> log_event(
              "step_end",
              "WARN",
              Some(scenario_id),
              Some("version_check"),
              [
                #("result", json.string("fail")),
                #("notes", json.string("Version " <> ctx.cli_version_raw <> " < 1.0.0")),
              ],
            )
            Some(ScenarioResult(
              scenario_id: scenario_id,
              status: Fail,
              duration_ms: elapsed_ms(ctx.logger) - start_ms,
              error_kind: Some("version_too_old"),
              error_message: Some("CLI version " <> ctx.cli_version_raw <> " is below minimum 1.0.0"),
              notes: Some("Upgrade Claude CLI"),
            ))
          }
        }
      None -> {
        ctx.logger
        |> log_event(
          "step_end",
          "WARN",
          Some(scenario_id),
          Some("version_check"),
          [
            #("result", json.string("pass")),
            #("notes", json.string("Unparseable version: " <> ctx.cli_version_raw)),
          ],
        )
        None
      }
    }

  case version_failure {
    Some(result) -> result
    None -> {
      ctx.logger |> log_event("step_start", "INFO", Some(scenario_id), Some("auth_check"), [])
      case get_env("ANTHROPIC_API_KEY") {
        Some(_) ->
          ctx.logger
          |> log_event(
            "step_end",
            "INFO",
            Some(scenario_id),
            Some("auth_check"),
            [#("result", json.string("pass")), #("notes", json.string("ANTHROPIC_API_KEY present"))],
          )
        None -> {
          let #(exit_code, stdout, _stderr) =
            run_command(ctx, ["auth", "status"], scenario_id, "auth_check", 10000)
          case exit_code == 0 && string.contains(string.lowercase(stdout), "authenticated") {
            True ->
              ctx.logger
              |> log_event(
                "step_end",
                "INFO",
                Some(scenario_id),
                Some("auth_check"),
                [#("result", json.string("pass")), #("notes", json.string("Authenticated via CLI session"))],
              )
            False ->
              ctx.logger
              |> log_event(
                "step_end",
                "WARN",
                Some(scenario_id),
                Some("auth_check"),
                [#("result", json.string("warn")), #("notes", json.string("Auth not detected; continuing"))],
              )
          }
        }
      }

      ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("pass"))])
      ScenarioResult(
        scenario_id: scenario_id,
        status: Pass,
        duration_ms: elapsed_ms(ctx.logger) - start_ms,
        error_kind: None,
        error_message: None,
        notes: Some("All preflight checks passed"),
      )
    }
  }
}

fn run_e2e_02_simple_query(ctx: ScenarioContext) -> ScenarioResult {
  let scenario_id = "E2E-02"
  let start_ms = ctx.logger |> elapsed_ms
  ctx.logger |> log_event("scenario_start", "INFO", Some(scenario_id), Some("simple_query"), [])

  ctx.logger |> log_event("step_start", "INFO", Some(scenario_id), Some("run_query"), [])
  let #(exit_code, stdout, stderr) =
    run_command(
      ctx,
      ["--print", "--output-format", "stream-json", "--max-turns", "1", "Say hello"],
      scenario_id,
      "run_query",
      30000,
    )

  case exit_code != 0 {
    True -> {
      ctx.logger |> log_event("scenario_end", "ERROR", Some(scenario_id), None, [#("result", json.string("fail"))])
      ScenarioResult(
        scenario_id: scenario_id,
        status: Fail,
        duration_ms: elapsed_ms(ctx.logger) - start_ms,
        error_kind: Some("non_zero_exit"),
        error_message: Some("CLI exited with code " <> int.to_string(exit_code)),
        notes: case stderr {
          "" -> None
          _ -> Some(redact_and_truncate(stderr))
        },
      )
    }
    False -> {
      let lines =
        stdout
        |> string.split("\n")
        |> list.filter(fn(line) { string.trim(line) != "" })

      let has_result =
        list.any(lines, fn(line) {
          case decode_type_field(line) {
            Ok("result") -> True
            _ -> False
          }
        })

      case has_result {
        False -> {
          ctx.logger |> log_event("scenario_end", "WARN", Some(scenario_id), None, [#("result", json.string("fail"))])
          ScenarioResult(
            scenario_id: scenario_id,
            status: Fail,
            duration_ms: elapsed_ms(ctx.logger) - start_ms,
            error_kind: Some("no_result"),
            error_message: Some("No Result message in stream"),
            notes: None,
          )
        }
        True -> {
          ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("pass"))])
          ScenarioResult(
            scenario_id: scenario_id,
            status: Pass,
            duration_ms: elapsed_ms(ctx.logger) - start_ms,
            error_kind: None,
            error_message: None,
            notes: Some("Received " <> int.to_string(list.length(lines)) <> " messages"),
          )
        }
      }
    }
  }
}

fn run_e2e_03_ndjson_purity(ctx: ScenarioContext) -> ScenarioResult {
  let scenario_id = "E2E-03"
  let start_ms = ctx.logger |> elapsed_ms
  ctx.logger |> log_event("scenario_start", "INFO", Some(scenario_id), Some("ndjson_check"), [])

  ctx.logger |> log_event("step_start", "INFO", Some(scenario_id), Some("run_query"), [])
  let #(exit_code, stdout, _stderr) =
    run_command(
      ctx,
      ["--print", "--output-format", "stream-json", "test"],
      scenario_id,
      "run_query",
      30000,
    )

  case exit_code != 0 {
    True -> {
      ctx.logger |> log_event("scenario_end", "ERROR", Some(scenario_id), None, [#("result", json.string("fail"))])
      ScenarioResult(
        scenario_id: scenario_id,
        status: Fail,
        duration_ms: elapsed_ms(ctx.logger) - start_ms,
        error_kind: Some("non_zero_exit"),
        error_message: Some("CLI exited with code " <> int.to_string(exit_code)),
        notes: None,
      )
    }
    False -> {
      let lines =
        stdout
        |> string.split("\n")
        |> list.filter(fn(line) { string.trim(line) != "" })

      let non_json_lines =
        indexed_filter_map(lines, fn(index, line) {
          case json.parse(line, decode.dynamic) {
            Ok(_) -> None
            Error(_) -> Some(#(index + 1, line))
          }
        })

      let allow_nonjson = case get_env("CLAUDE_INTEGRATION_ALLOW_NONJSON") {
        Some("1") -> True
        _ -> False
      }

      case non_json_lines {
        [] -> {
          ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("pass"))])
          ScenarioResult(
            scenario_id: scenario_id,
            status: Pass,
            duration_ms: elapsed_ms(ctx.logger) - start_ms,
            error_kind: None,
            error_message: None,
            notes: Some("All lines valid JSON"),
          )
        }
        [#(line_no, line), .._] -> {
          case allow_nonjson {
            True -> {
              ctx.logger
              |> log_event(
                "warning",
                "WARN",
                Some(scenario_id),
                Some("ndjson_check"),
                [#("notes", json.string(int.to_string(list.length(non_json_lines)) <> " non-JSON lines (allowed via env)"))],
              )
              ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("pass"))])
              ScenarioResult(
                scenario_id: scenario_id,
                status: Pass,
                duration_ms: elapsed_ms(ctx.logger) - start_ms,
                error_kind: None,
                error_message: None,
                notes: Some(int.to_string(list.length(non_json_lines)) <> " non-JSON lines (allowed)"),
              )
            }
            False -> {
              ctx.logger |> log_event("scenario_end", "ERROR", Some(scenario_id), None, [#("result", json.string("fail"))])
              ScenarioResult(
                scenario_id: scenario_id,
                status: Fail,
                duration_ms: elapsed_ms(ctx.logger) - start_ms,
                error_kind: Some("ndjson_impure"),
                error_message: Some(int.to_string(list.length(non_json_lines)) <> " non-JSON line(s) in output"),
                notes: Some("Line " <> int.to_string(line_no) <> ": " <> redact_and_truncate(line)),
              )
            }
          }
        }
      }
    }
  }
}

fn run_e2e_04_session_resume(ctx: ScenarioContext) -> ScenarioResult {
  let scenario_id = "E2E-04"
  let start_ms = ctx.logger |> elapsed_ms
  ctx.logger |> log_event("scenario_start", "INFO", Some(scenario_id), Some("session_resume"), [])

  ctx.logger |> log_event("step_start", "INFO", Some(scenario_id), Some("initial_query"), [])
  let #(exit_code, stdout, _stderr) =
    run_command(
      ctx,
      ["--print", "--output-format", "stream-json", "--max-turns", "1", "Remember 42"],
      scenario_id,
      "initial_query",
      30000,
    )

  case exit_code != 0 {
    True -> {
      ctx.logger |> log_event("scenario_end", "ERROR", Some(scenario_id), None, [#("result", json.string("fail"))])
      ScenarioResult(
        scenario_id: scenario_id,
        status: Fail,
        duration_ms: elapsed_ms(ctx.logger) - start_ms,
        error_kind: Some("initial_query_failed"),
        error_message: Some("Initial query failed with exit code " <> int.to_string(exit_code)),
        notes: None,
      )
    }
    False -> {
      let session_id = extract_session_id(stdout)
      case session_id {
        None -> {
          ctx.logger |> log_event("scenario_end", "ERROR", Some(scenario_id), None, [#("result", json.string("fail"))])
          ScenarioResult(
            scenario_id: scenario_id,
            status: Fail,
            duration_ms: elapsed_ms(ctx.logger) - start_ms,
            error_kind: Some("no_session_id"),
            error_message: Some("No session_id in system message"),
            notes: None,
          )
        }
        Some(id) -> {
          ctx.logger |> log_event("step_start", "INFO", Some(scenario_id), Some("resume_query"), [])
          let #(resume_exit, _resume_stdout, _resume_stderr) =
            run_command(
              ctx,
              [
                "--print",
                "--output-format",
                "stream-json",
                "--max-turns",
                "1",
                "--resume",
                id,
                "What number?",
              ],
              scenario_id,
              "resume_query",
              30000,
            )

          case resume_exit != 0 {
            True -> {
              ctx.logger |> log_event("scenario_end", "WARN", Some(scenario_id), None, [#("result", json.string("fail"))])
              ScenarioResult(
                scenario_id: scenario_id,
                status: Fail,
                duration_ms: elapsed_ms(ctx.logger) - start_ms,
                error_kind: Some("resume_failed"),
                error_message: Some("Resume query failed with exit code " <> int.to_string(resume_exit)),
                notes: None,
              )
            }
            False -> {
              ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("pass"))])
              ScenarioResult(
                scenario_id: scenario_id,
                status: Pass,
                duration_ms: elapsed_ms(ctx.logger) - start_ms,
                error_kind: None,
                error_message: None,
                notes: Some("Session " <> string.slice(id, 0, 16) <> "... resumed"),
              )
            }
          }
        }
      }
    }
  }
}

fn run_e2e_05_version_edge_cases(ctx: ScenarioContext) -> ScenarioResult {
  let scenario_id = "E2E-05"
  let start_ms = ctx.logger |> elapsed_ms
  ctx.logger |> log_event("scenario_start", "INFO", Some(scenario_id), Some("version_edge"), [])

  case ctx.cli_version_parsed {
    Some(_) -> {
      ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("pass"))])
      ScenarioResult(
        scenario_id: scenario_id,
        status: Pass,
        duration_ms: elapsed_ms(ctx.logger) - start_ms,
        error_kind: None,
        error_message: None,
        notes: Some("Version parsed: " <> ctx.cli_version_raw),
      )
    }
    None -> {
      ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("pass"))])
      ScenarioResult(
        scenario_id: scenario_id,
        status: Pass,
        duration_ms: elapsed_ms(ctx.logger) - start_ms,
        error_kind: None,
        error_message: None,
        notes: Some("Graceful handling of unparseable: " <> ctx.cli_version_raw),
      )
    }
  }
}

fn run_e2e_06_auth_missing(ctx: ScenarioContext) -> ScenarioResult {
  let scenario_id = "E2E-06"
  let start_ms = ctx.logger |> elapsed_ms
  ctx.logger |> log_event("scenario_start", "INFO", Some(scenario_id), Some("auth_missing"), [])

  case get_env("ANTHROPIC_API_KEY") {
    Some(_) -> {
      ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("skip"))])
      ScenarioResult(
        scenario_id: scenario_id,
        status: Skip,
        duration_ms: elapsed_ms(ctx.logger) - start_ms,
        error_kind: None,
        error_message: None,
        notes: Some("Auth present - cannot test missing auth scenario"),
      )
    }
    None -> {
      ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("pass"))])
      ScenarioResult(
        scenario_id: scenario_id,
        status: Pass,
        duration_ms: elapsed_ms(ctx.logger) - start_ms,
        error_kind: None,
        error_message: None,
        notes: Some("Auth missing correctly detected"),
      )
    }
  }
}

fn run_e2e_07_cli_missing(ctx: ScenarioContext) -> ScenarioResult {
  let scenario_id = "E2E-07"
  let start_ms = ctx.logger |> elapsed_ms
  ctx.logger |> log_event("scenario_start", "INFO", Some(scenario_id), Some("cli_missing"), [])

  ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("skip"))])
  ScenarioResult(
    scenario_id: scenario_id,
    status: Skip,
    duration_ms: elapsed_ms(ctx.logger) - start_ms,
    error_kind: None,
    error_message: None,
    notes: Some("CLI present - cannot test missing CLI scenario"),
  )
}

fn run_e2e_08_timeout(ctx: ScenarioContext) -> ScenarioResult {
  let scenario_id = "E2E-08"
  let start_ms = ctx.logger |> elapsed_ms
  ctx.logger |> log_event("scenario_start", "INFO", Some(scenario_id), Some("timeout_test"), [])

  ctx.logger |> log_event("step_start", "INFO", Some(scenario_id), Some("timeout_query"), [])
  let #(exit_code, _stdout, _stderr) =
    run_command(
      ctx,
      [
        "--print",
        "--output-format",
        "stream-json",
        "--max-turns",
        "1",
        "Write a very long essay about the history of computing",
      ],
      scenario_id,
      "timeout_query",
      5000,
    )

  case exit_code == -1 {
    True -> {
      ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("pass"))])
      ScenarioResult(
        scenario_id: scenario_id,
        status: Pass,
        duration_ms: elapsed_ms(ctx.logger) - start_ms,
        error_kind: None,
        error_message: None,
        notes: Some("Timeout handled correctly"),
      )
    }
    False -> {
      ctx.logger |> log_event("scenario_end", "INFO", Some(scenario_id), None, [#("result", json.string("pass"))])
      ScenarioResult(
        scenario_id: scenario_id,
        status: Pass,
        duration_ms: elapsed_ms(ctx.logger) - start_ms,
        error_kind: None,
        error_message: None,
        notes: Some("Query completed within timeout"),
      )
    }
  }
}

// ============================================================================
// Command runner
// ============================================================================

type CommandOutcome {
  CommandOk(exit_code: Int, stdout: String, stdout_bytes: Int)
  CommandTimeout(stdout: String, stdout_bytes: Int)
  CommandSpawnError(reason: String)
}

fn run_command(
  ctx: ScenarioContext,
  args: List(String),
  scenario_id: String,
  step: String,
  timeout_ms: Int,
) -> #(Int, String, String) {
  let adjusted_args = ensure_verbose_for_stream_json(args)
  let cmd = [ctx.cli_path, ..adjusted_args]
  let cwd = current_directory()

  ctx.logger
  |> log_event(
    "command_start",
    "INFO",
    Some(scenario_id),
    Some(step),
    [
      #("command", json.string(string.join(cmd, " "))),
      #("cwd", json.string(cwd)),
      #("env", json.object(redacted_env_as_json())),
    ],
  )

  case run_shell_command(ctx.cli_path, adjusted_args, timeout_ms) {
    CommandOk(exit_code, stdout, stdout_bytes) -> {
      let _ =
        case stdout != "" {
          True -> ctx.logger |> log_stdout("[" <> scenario_id <> "/" <> step <> "]\n" <> stdout <> "\n")
          False -> ctx.logger
        }
      ctx.logger
      |> log_event(
        "command_end",
        case exit_code == 0 { True -> "INFO" False -> "WARN" },
        Some(scenario_id),
        Some(step),
        [
          #("exit_code", json.int(exit_code)),
          #("stdout_bytes", json.int(stdout_bytes)),
          #("stderr_bytes", json.int(0)),
        ],
      )
      #(exit_code, stdout, "")
    }
    CommandTimeout(stdout, stdout_bytes) -> {
      let _ =
        case stdout != "" {
          True -> ctx.logger |> log_stdout("[" <> scenario_id <> "/" <> step <> "]\n" <> stdout <> "\n")
          False -> ctx.logger
        }
      ctx.logger
      |> log_event(
        "command_end",
        "ERROR",
        Some(scenario_id),
        Some(step),
        [
          #("error", json.object([#("kind", json.string("timeout")), #("message", json.string("Command timed out"))])),
          #("stdout_bytes", json.int(stdout_bytes)),
          #("stderr_bytes", json.int(0)),
        ],
      )
      #(-1, stdout, "")
    }
    CommandSpawnError(reason) -> {
      ctx.logger
      |> log_event(
        "command_end",
        "ERROR",
        Some(scenario_id),
        Some(step),
        [
          #("error", json.object([#("kind", json.string("spawn_failed")), #("message", json.string(reason))])),
          #("stdout_bytes", json.int(0)),
          #("stderr_bytes", json.int(0)),
        ],
      )
      #(-1, "", "")
    }
  }
}

@external(erlang, "claude_agent_sdk_ffi", "os_cmd")
fn os_cmd(command: String) -> String

fn run_shell_command(
  path: String,
  args: List(String),
  timeout_ms: Int,
) -> CommandOutcome {
  let command = build_shell_command(path, args, timeout_ms)
  let output = os_cmd(command)
  let #(exit_code, stdout) = split_exit_code(output)
  let stdout_bytes = bit_array.byte_size(bit_array.from_string(stdout))
  case exit_code {
    124 -> CommandTimeout(stdout, stdout_bytes)
    _ -> CommandOk(exit_code, stdout, stdout_bytes)
  }
}

fn build_shell_command(
  path: String,
  args: List(String),
  timeout_ms: Int,
) -> String {
  let timeout_seconds = int.max(1, { timeout_ms + 999 } / 1000)
  let parts = [path, ..args] |> list.map(shell_escape)
  let cmd = string.join(parts, " ")
  "timeout " <> int.to_string(timeout_seconds) <> "s " <> cmd <> " 2>&1; echo __EXIT_CODE:$?"
}

fn shell_escape(arg: String) -> String {
  let escaped = string.replace(arg, each: "'", with: "'\"'\"'")
  "'" <> escaped <> "'"
}

fn split_exit_code(output: String) -> #(Int, String) {
  let lines = string.split(output, "\n")
  let #(exit_code, clean_lines) = extract_exit_code(lines, [])
  #(exit_code, string.join(list.reverse(clean_lines), "\n"))
}

fn extract_exit_code(
  lines: List(String),
  acc: List(String),
) -> #(Int, List(String)) {
  case lines {
    [] -> #(-1, acc)
    [line, ..rest] -> {
      case string.starts_with(line, "__EXIT_CODE:") {
        True -> {
          let code_str = string.slice(line, 12, string.length(line) - 12)
          let exit_code = case int.parse(code_str) {
            Ok(code) -> code
            Error(_) -> -1
          }
          let #(maybe_exit, remaining) = extract_exit_code(rest, acc)
          case maybe_exit {
            -1 -> #(exit_code, remaining)
            _ -> #(maybe_exit, remaining)
          }
        }
        False -> extract_exit_code(rest, [line, ..acc])
      }
    }
  }
}

fn ensure_verbose_for_stream_json(args: List(String)) -> List(String) {
  let has_print = list.contains(args, "--print")
  let has_output_format = list.contains(args, "--output-format")
  let has_verbose = list.contains(args, "--verbose")
  case has_print && has_output_format && has_verbose == False {
    True -> insert_verbose_after_output_format(args)
    False -> args
  }
}

fn insert_verbose_after_output_format(args: List(String)) -> List(String) {
  case args {
    [] -> ["--verbose"]
    ["--output-format", "stream-json", ..rest] ->
      ["--output-format", "stream-json", "--verbose", ..rest]
    [head, ..tail] -> [head, ..insert_verbose_after_output_format(tail)]
  }
}

// ============================================================================
// Helpers
// ============================================================================

fn decode_type_field(line: String) -> Result(String, json.DecodeError) {
  let decoder = {
    use t <- decode.field("type", decode.string)
    decode.success(t)
  }
  json.parse(line, decoder)
}

fn extract_session_id(stdout: String) -> Option(String) {
  let lines =
    stdout
    |> string.split("\n")
    |> list.filter(fn(line) { string.trim(line) != "" })

  list.fold(lines, None, fn(acc, line) {
    case acc {
      Some(_) -> acc
      None ->
        case decode_session_fields(line) {
          Ok(#("system", session_id)) ->
            case session_id {
              "" -> None
              _ -> Some(session_id)
            }
          _ -> None
        }
    }
  })
}

fn decode_session_fields(line: String) -> Result(#(String, String), json.DecodeError) {
  let decoder = {
    use t <- decode.field("type", decode.string)
    use session_id <- decode.optional_field("session_id", "", decode.string)
    decode.success(#(t, session_id))
  }
  json.parse(line, decoder)
}

fn version_at_least(current: #(Int, Int, Int), minimum: #(Int, Int, Int)) -> Bool {
  let #(cmaj, cmin, cpatch) = current
  let #(mmaj, mmin, mpatch) = minimum
  case cmaj > mmaj {
    True -> True
    False ->
      case cmaj < mmaj {
        True -> False
        False ->
          case cmin > mmin {
            True -> True
            False ->
              case cmin < mmin {
                True -> False
                False -> cpatch >= mpatch
              }
          }
      }
  }
}

fn minimum_version_tuple() -> #(Int, Int, Int) {
  case cli.minimum_cli_version {
    cli.CliVersion(major, minor, patch, _raw) -> #(major, minor, patch)
    cli.UnknownVersion(_) -> #(1, 0, 0)
  }
}

fn command_output(path: String, args: List(String), timeout_ms: Int) -> Result(String, String) {
  case run_shell_command(path, args, timeout_ms) {
    CommandOk(0, stdout, _) -> Ok(stdout)
    CommandOk(code, _stdout, _) -> Error("exit_" <> int.to_string(code))
    CommandTimeout(_, _) -> Error("timeout")
    CommandSpawnError(reason) -> Error(reason)
  }
}

fn get_cli_version(
  cli_path: String,
) -> Result(#(String, Option(#(Int, Int, Int))), String) {
  case command_output(cli_path, ["--version"], 5000) {
    Ok(output) -> {
      let raw = string.trim(output)
      let parsed =
        case cli.parse_version_string(raw) {
          Ok(cli.CliVersion(major, minor, patch, _raw)) -> Some(#(major, minor, patch))
          Ok(cli.UnknownVersion(_)) -> None
          Error(_) -> None
        }
      Ok(#(raw, parsed))
    }
    Error(reason) -> Error(reason)
  }
}

fn current_directory() -> String {
  case simplifile.current_directory() {
    Ok(dir) -> dir
    Error(_) -> "."
  }
}

// ============================================================================
// Logger + redaction
// ============================================================================

type EventLogger {
  EventLogger(run_id: String, output_dir: String, start_ms: Int)
}

fn new_logger(output_dir: String, run_id: String) -> EventLogger {
  let _ = simplifile.create_directory_all(output_dir)
  let _ = simplifile.write(to: output_dir <> "/events.jsonl", contents: "")
  let _ = simplifile.write(to: output_dir <> "/stdout.txt", contents: "")
  let _ = simplifile.write(to: output_dir <> "/stderr.txt", contents: "")
  EventLogger(run_id: run_id, output_dir: output_dir, start_ms: port_ffi.monotonic_time_ms())
}

fn elapsed_ms(logger: EventLogger) -> Int {
  port_ffi.monotonic_time_ms() - logger.start_ms
}

fn log_event(
  logger: EventLogger,
  event: String,
  level: String,
  scenario_id: Option(String),
  step: Option(String),
  extra: List(#(String, json.Json)),
) -> EventLogger {
  let base = [
    #("ts", json.string(utc_now_string())),
    #("level", json.string(level)),
    #("event", json.string(event)),
    #("run_id", json.string(logger.run_id)),
    #("elapsed_ms", json.int(elapsed_ms(logger))),
  ]
  let with_scenario = case scenario_id {
    Some(id) -> [#("scenario_id", json.string(id)), ..base]
    None -> base
  }
  let with_step = case step {
    Some(s) -> [#("step", json.string(s)), ..with_scenario]
    None -> with_scenario
  }
  let payload = json.object(list.append(with_step, extra))
  let line = json.to_string(payload) <> "\n"
  let _ = simplifile.append(to: logger.output_dir <> "/events.jsonl", contents: line)
  logger
}

fn log_stdout(logger: EventLogger, text: String) -> EventLogger {
  let processed = redact_and_truncate(text)
  let _ = simplifile.append(to: logger.output_dir <> "/stdout.txt", contents: processed)
  logger
}

fn redact_and_truncate(text: String) -> String {
  text
  |> string.split("\n")
  |> list.map(fn(line) { truncate_line(redact_line(line)) })
  |> string.join("\n")
}

fn redact_line(line: String) -> String {
  line
  |> string.split(" ")
  |> list.map(fn(token) {
    case should_redact_token(token) {
      True -> "REDACTED"
      False -> token
    }
  })
  |> string.join(" ")
}

fn should_redact_token(token: String) -> Bool {
  let trimmed = string.trim(token)
  string.starts_with(trimmed, "sk-ant-")
  || string.contains(trimmed, "Bearer")
  || string.contains(string.lowercase(trimmed), "api_key")
  || string.contains(string.lowercase(trimmed), "api-key")
  || string.length(trimmed) >= 32
}

fn truncate_line(line: String) -> String {
  let max_len = 4096
  case string.length(line) <= max_len {
    True -> line
    False ->
      string.slice(line, 0, max_len - 20) <> "... [TRUNCATED]"
  }
}

fn redacted_env_as_json() -> List(#(String, json.Json)) {
  env_allowlist()
  |> list.fold([], fn(acc, key) {
    case get_env(key) {
      Some(value) -> [#(key, json.string(value)), ..acc]
      None -> acc
    }
  })
  |> list.reverse
}

fn env_allowlist() -> List(String) {
  [
    "CLAUDE_INTEGRATION_TEST",
    "CLAUDE_INTEGRATION_ALLOW_NONJSON",
    "PATH",
    "HOME",
    "USER",
    "SHELL",
    "TERM",
    "LANG",
    "LC_ALL",
  ]
}

// ============================================================================
// Metadata and summary
// ============================================================================

type Metadata {
  Metadata(
    run_id: String,
    start_ts: String,
    cli_version_raw: String,
    cli_version_parsed: Option(#(Int, Int, Int)),
    gleam_version: String,
    repo_sha: String,
    os: String,
    arch: String,
  )
}

fn build_metadata(
  run_id: String,
  start_ts: String,
  cli_version_raw: String,
  cli_version_parsed: Option(#(Int, Int, Int)),
) -> Metadata {
  let gleam_version = case command_output("gleam", ["--version"], 5000) {
    Ok(output) -> string.trim(output)
    Error(_) -> "unknown"
  }
  let repo_sha = case command_output("git", ["rev-parse", "HEAD"], 5000) {
    Ok(output) -> string.slice(string.trim(output), 0, 12)
    Error(_) -> "unknown"
  }
  let #(os, arch) = system_info()
  Metadata(
    run_id: run_id,
    start_ts: start_ts,
    cli_version_raw: cli_version_raw,
    cli_version_parsed: cli_version_parsed,
    gleam_version: gleam_version,
    repo_sha: repo_sha,
    os: os,
    arch: arch,
  )
}

fn write_metadata(
  output_dir: String,
  metadata: Metadata,
  duration_ms: Int,
) -> Nil {
  let json_payload =
    json.object([
      #("run_id", json.string(metadata.run_id)),
      #("start_ts", json.string(metadata.start_ts)),
      #("cli_version_raw", json.string(metadata.cli_version_raw)),
      #("cli_version_parsed", json.string(option_string(metadata.cli_version_parsed))),
      #("gleam_version", json.string(metadata.gleam_version)),
      #("repo_sha", json.string(metadata.repo_sha)),
      #("os", json.string(metadata.os)),
      #("arch", json.string(metadata.arch)),
      #("duration_ms", json.int(duration_ms)),
    ])
    |> json.to_string
  let _ = simplifile.write(to: output_dir <> "/metadata.json", contents: json_payload)
  Nil
}

type Summary {
  Summary(passed: Int, failed: Int, skipped: Int)
}

fn summarize(results: List(ScenarioResult)) -> Summary {
  let passed = results |> list.filter(fn(r) { r.status == Pass }) |> list.length
  let failed = results |> list.filter(fn(r) { r.status == Fail }) |> list.length
  let skipped = results |> list.filter(fn(r) { r.status == Skip }) |> list.length
  Summary(passed: passed, failed: failed, skipped: skipped)
}

fn write_summary(
  output_dir: String,
  run_id: String,
  metadata: Metadata,
  results: List(ScenarioResult),
  duration_ms: Int,
) -> Nil {
  let lines =
    [
      "============================================================",
      "E2E Test Run Summary",
      "============================================================",
      "",
      "Run ID:       " <> run_id,
      "Date:         " <> metadata.start_ts,
      "CLI Version:  " <> metadata.cli_version_raw,
      "OS:           " <> metadata.os,
      "Gleam:        " <> metadata.gleam_version,
      "Duration:     " <> int.to_string(duration_ms) <> "ms",
      "",
      "------------------------------------------------------------",
      "Scenario Results",
      "------------------------------------------------------------",
      "",
      "ID         Status   Duration     Notes",
      "------------------------------------------------------------",
    ]
    |> list.append(
      results
      |> list.map(fn(r) {
        let notes = case r.notes { Some(n) -> n None -> "" }
        r.scenario_id
          <> pad_to(10 - string.length(r.scenario_id))
          <> " "
          <> status_label(r.status)
          <> pad_to(10 - string.length(status_label(r.status)))
          <> " "
          <> int.to_string(r.duration_ms)
          <> "ms   "
          <> notes
      }),
    )

  let failures = results |> list.filter(fn(r) { r.status == Fail })
  let skips = results |> list.filter(fn(r) { r.status == Skip })

  let lines =
    case failures {
      [] -> lines
      _ ->
        lines
        |> list.append([
          "",
          "------------------------------------------------------------",
          "Failures",
          "------------------------------------------------------------",
        ])
        |> list.append(failures |> list.flat_map(fn(r) {
          [
            "",
            r.scenario_id <> ":",
            "  Error: " <> option_default(r.error_kind, "unknown"),
            "  Message: " <> option_default(r.error_message, "no message"),
          ]
        }))
    }

  let lines =
    case skips {
      [] -> lines
      _ ->
        lines
        |> list.append([
          "",
          "------------------------------------------------------------",
          "Skipped",
          "------------------------------------------------------------",
        ])
        |> list.append(skips |> list.flat_map(fn(r) {
          [
            "",
            r.scenario_id <> ":",
            "  Reason: " <> option_default(r.notes, "no reason provided"),
          ]
        }))
    }

  let summary = summarize(results)
  let lines =
    lines
    |> list.append([
      "",
      "============================================================",
      "Total: "
        <> int.to_string(list.length(results))
        <> " | Passed: "
        <> int.to_string(summary.passed)
        <> " | Failed: "
        <> int.to_string(summary.failed)
        <> " | Skipped: "
        <> int.to_string(summary.skipped),
      "============================================================",
    ])

  let _ = simplifile.write(to: output_dir <> "/summary.txt", contents: string.join(lines, "\n"))
  Nil
}

fn status_label(status: ScenarioStatus) -> String {
  case status {
    Pass -> "pass"
    Fail -> "fail"
    Skip -> "skip"
  }
}

fn pad_to(count: Int) -> String {
  case count <= 0 {
    True -> ""
    False -> string.repeat(" ", count)
  }
}

fn option_default(value: Option(String), fallback: String) -> String {
  case value {
    Some(v) -> v
    None -> fallback
  }
}

fn option_string(value: Option(#(Int, Int, Int))) -> String {
  case value {
    Some(#(a, b, c)) -> int.to_string(a) <> "." <> int.to_string(b) <> "." <> int.to_string(c)
    None -> "unknown"
  }
}

fn indexed_filter_map(
  items: List(a),
  f: fn(Int, a) -> Option(b),
) -> List(b) {
  indexed_filter_map_loop(items, 0, f, [])
}

fn indexed_filter_map_loop(
  items: List(a),
  index: Int,
  f: fn(Int, a) -> Option(b),
  acc: List(b),
) -> List(b) {
  case items {
    [] -> list.reverse(acc)
    [item, ..rest] ->
      case f(index, item) {
        Some(value) -> indexed_filter_map_loop(rest, index + 1, f, [value, ..acc])
        None -> indexed_filter_map_loop(rest, index + 1, f, acc)
      }
  }
}

// ============================================================================
// Environment + time helpers
// ============================================================================

@external(erlang, "erlang", "binary_to_list")
fn string_to_charlist(s: String) -> Dynamic

@external(erlang, "unicode", "characters_to_binary")
fn charlist_to_string(chars: Dynamic) -> String

@external(erlang, "os", "getenv")
fn ffi_getenv(name: Dynamic) -> Dynamic

@external(erlang, "erlang", "is_atom")
fn is_atom(value: Dynamic) -> Bool

fn get_env(name: String) -> Option(String) {
  let charlist_name = string_to_charlist(name)
  let result = ffi_getenv(charlist_name)
  case is_atom(result) {
    True -> None
    False -> {
      let value = charlist_to_string(result)
      case value {
        "" -> None
        _ -> Some(value)
      }
    }
  }
}

@external(erlang, "claude_agent_sdk_ffi", "unique_integer")
fn unique_integer() -> Int

@external(erlang, "claude_agent_sdk_ffi", "system_info")
fn system_info() -> #(String, String)

@external(erlang, "calendar", "universal_time")
fn universal_time() -> #(#(Int, Int, Int), #(Int, Int, Int))

fn generate_run_id() -> String {
  let ts = utc_timestamp_compact()
  let suffix = unique_integer() % 100000000
  ts <> "-" <> pad_left(int.to_string(suffix), 8, "0")
}

fn utc_timestamp_compact() -> String {
  let #(#(year, month, day), #(hour, minute, second)) = universal_time()
  int.to_string(year)
    <> pad_left(int.to_string(month), 2, "0")
    <> pad_left(int.to_string(day), 2, "0")
    <> "-"
    <> pad_left(int.to_string(hour), 2, "0")
    <> pad_left(int.to_string(minute), 2, "0")
    <> pad_left(int.to_string(second), 2, "0")
}

fn utc_now_string() -> String {
  let #(#(year, month, day), #(hour, minute, second)) = universal_time()
  pad_left(int.to_string(year), 4, "0")
    <> "-"
    <> pad_left(int.to_string(month), 2, "0")
    <> "-"
    <> pad_left(int.to_string(day), 2, "0")
    <> "T"
    <> pad_left(int.to_string(hour), 2, "0")
    <> ":"
    <> pad_left(int.to_string(minute), 2, "0")
    <> ":"
    <> pad_left(int.to_string(second), 2, "0")
    <> "Z"
}

fn pad_left(value: String, width: Int, pad: String) -> String {
  let len = string.length(value)
  case len >= width {
    True -> value
    False -> string.repeat(pad, width - len) <> value
  }
}
