/// E2E Tests for Concurrent Session Isolation.
///
/// Tests that multiple concurrent SDK queries have session isolation -
/// no cross-session contamination. Each session should have a unique
/// session_id and isolated state.
///
/// ## Running Tests
/// ```bash
/// E2E_ALLOW_CONCURRENT=1 gleam test -- --e2e
/// ```
import claude_agent_sdk
import claude_agent_sdk/error.{error_to_string}
import e2e/helpers.{type QueryOutcome, QueryFailure, QuerySuccess, QueryTimedOut}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set
import gleam/string
import gleeunit/should

// ============================================================================
// Concurrent Session Isolation Test
// ============================================================================

/// Type to track per-session results.
type SessionResult {
  SessionResult(
    session_number: Int,
    session_id: Option(String),
    terminated_normally: Bool,
    error: Option(String),
  )
}

/// Test concurrent session isolation.
/// Spawns 3 concurrent queries and verifies each has a distinct session_id.
pub fn sdk_concurrent_session_isolation_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      helpers.with_concurrent_mode(fn() {
        let ctx =
          helpers.new_test_context_timestamped(
            "sdk_concurrent_session_isolation",
          )
        let ctx = helpers.test_step(ctx, "spawn_concurrent_queries")

        // Create a subject to collect results from spawned processes
        let result_subject: process.Subject(SessionResult) =
          process.new_subject()

        // Number of concurrent sessions to test
        let session_count = 3
        let timeout_ms = 30_000

        // Spawn concurrent queries with distinct prompts
        list.range(1, session_count)
        |> list.each(fn(session_num) {
          process.spawn_unlinked(fn() {
            let result = run_session_query(session_num, timeout_ms)
            process.send(result_subject, result)
          })
        })

        let ctx = helpers.test_step(ctx, "collect_results")

        // Collect all results (wait for all sessions) - fail if any timeout
        let collect_outcome =
          collect_results(result_subject, session_count, timeout_ms)

        let results = case collect_outcome {
          Ok(r) -> r
          Error(#(collected, missing)) -> {
            helpers.log_error(
              ctx,
              "collect_timeout",
              "Timed out waiting for "
                <> int.to_string(missing)
                <> " session(s) to report; collected "
                <> int.to_string(list.length(collected)),
            )
            helpers.log_test_complete(ctx, False, "result collection timeout")
            should.fail()
            // Unreachable but needed for type
            collected
          }
        }

        helpers.log_info_with(ctx, "results_collected", [
          #("count", json.int(list.length(results))),
        ])

        let ctx = helpers.test_step(ctx, "validate_isolation")

        // Log each result
        list.each(results, fn(r) {
          let session_id_str = case r.session_id {
            Some(id) -> id
            None -> "none"
          }
          helpers.log_info_with(ctx, "session_result", [
            #("session_number", json.int(r.session_number)),
            #("session_id", json.string(session_id_str)),
            #("terminated_normally", json.bool(r.terminated_normally)),
            #(
              "error",
              json.string(case r.error {
                Some(e) -> e
                None -> "none"
              }),
            ),
          ])
        })

        // Extract session IDs (only from successful queries with session_id)
        let session_ids =
          results
          |> list.filter_map(fn(r) {
            case r.session_id {
              Some(id) -> Ok(id)
              None -> Error(Nil)
            }
          })

        // Check: we need at least 2 successful sessions to verify isolation
        case list.length(session_ids) >= 2 {
          False -> {
            helpers.log_error(
              ctx,
              "insufficient_sessions",
              "Need at least 2 successful sessions to verify isolation, got "
                <> int.to_string(list.length(session_ids)),
            )
            helpers.log_test_complete(
              ctx,
              False,
              "insufficient successful sessions",
            )
            should.fail()
          }
          True -> {
            // Verify all session_ids are distinct (no contamination)
            let unique_ids = set.from_list(session_ids)
            let all_unique = set.size(unique_ids) == list.length(session_ids)

            helpers.log_info_with(ctx, "session_id_uniqueness", [
              #("total_ids", json.int(list.length(session_ids))),
              #("unique_ids", json.int(set.size(unique_ids))),
              #("all_unique", json.bool(all_unique)),
            ])

            case all_unique {
              False -> {
                helpers.log_error(
                  ctx,
                  "session_contamination",
                  "Duplicate session_ids found - possible cross-session contamination",
                )
                helpers.log_test_complete(
                  ctx,
                  False,
                  "session isolation violated",
                )
                should.fail()
              }
              True -> {
                // Check that ALL sessions succeeded (no errors/timeouts)
                let all_succeeded =
                  list.all(results, fn(r) {
                    r.terminated_normally && option.is_none(r.error)
                  })

                case all_succeeded {
                  False -> {
                    // Find which sessions failed
                    let failed =
                      results
                      |> list.filter(fn(r) {
                        !r.terminated_normally || option.is_some(r.error)
                      })
                    let failed_nums =
                      failed
                      |> list.map(fn(r) { int.to_string(r.session_number) })
                      |> string.join(", ")
                    helpers.log_error(
                      ctx,
                      "session_failure",
                      "Sessions failed or timed out: " <> failed_nums,
                    )
                    helpers.log_test_complete(
                      ctx,
                      False,
                      "session failure or timeout",
                    )
                    should.fail()
                  }
                  True -> {
                    helpers.log_info_with(ctx, "isolation_verified", [
                      #("distinct_session_ids", json.int(set.size(unique_ids))),
                    ])
                    helpers.log_test_complete(
                      ctx,
                      True,
                      "all session_ids distinct, no contamination",
                    )
                  }
                }
              }
            }
          }
        }
      })
    }
  }
}

/// Run a single session query and return the result.
fn run_session_query(session_num: Int, timeout_ms: Int) -> SessionResult {
  let prompt = "Say 'session-" <> int.to_string(session_num) <> "'"

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_max_turns(1)

  // Use direct query without lock (concurrent mode bypasses it)
  let outcome = query_without_lock(prompt, opts, timeout_ms)

  case outcome {
    QuerySuccess(consume_result) -> {
      let session_id = helpers.extract_session_id(consume_result.messages)
      SessionResult(
        session_number: session_num,
        session_id: session_id,
        terminated_normally: consume_result.terminated_normally,
        error: None,
      )
    }
    QueryFailure(err) -> {
      let error_str = error_to_string(err)
      SessionResult(
        session_number: session_num,
        session_id: None,
        terminated_normally: False,
        error: Some(error_str),
      )
    }
    QueryTimedOut ->
      SessionResult(
        session_number: session_num,
        session_id: None,
        terminated_normally: False,
        error: Some("timeout"),
      )
  }
}

/// Run query without acquiring the global lock (for concurrent testing).
/// This is similar to helpers.query_and_consume_with_timeout but skips the lock.
fn query_without_lock(
  prompt: String,
  options: claude_agent_sdk.QueryOptions,
  timeout_ms: Int,
) -> QueryOutcome {
  let subject: process.Subject(QueryOutcome) = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let outcome = case claude_agent_sdk.query(prompt, options) {
        Ok(stream) -> QuerySuccess(helpers.consume_stream(stream))
        Error(err) -> QueryFailure(err)
      }
      process.send(subject, outcome)
    })

  case process.receive(subject, timeout_ms) {
    Ok(outcome) -> outcome
    Error(Nil) -> {
      helpers.kill_pid(pid)
      QueryTimedOut
    }
  }
}

/// Collect results from the subject with a timeout.
/// Returns Ok(results) if all expected results arrived,
/// or Error(#(collected, remaining)) if timeout occurred.
fn collect_results(
  subject: process.Subject(SessionResult),
  count: Int,
  timeout_ms: Int,
) -> Result(List(SessionResult), #(List(SessionResult), Int)) {
  collect_results_loop(subject, count, timeout_ms, [])
}

fn collect_results_loop(
  subject: process.Subject(SessionResult),
  remaining: Int,
  timeout_ms: Int,
  acc: List(SessionResult),
) -> Result(List(SessionResult), #(List(SessionResult), Int)) {
  case remaining <= 0 {
    True -> Ok(acc)
    False -> {
      case process.receive(subject, timeout_ms) {
        Ok(result) ->
          collect_results_loop(subject, remaining - 1, timeout_ms, [
            result,
            ..acc
          ])
        Error(Nil) ->
          // Timeout waiting for result - return error with partial results
          Error(#(acc, remaining))
      }
    }
  }
}
