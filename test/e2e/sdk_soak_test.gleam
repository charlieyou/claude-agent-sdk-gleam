/// Soak Test for SDK Stability (SOAK-01).
///
/// Long-running stability test to detect resource leaks over many queries.
/// Runs 20-50 sequential queries and monitors process, ETS, and port counts.
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e        # Does NOT run soak test (verify skip message)
/// gleam test -- --e2e --soak # Runs soak test
/// ```
import claude_agent_sdk
import claude_agent_sdk/error.{error_to_string}
import e2e/helpers
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleeunit/should

// ============================================================================
// SOAK-01: Resource Stability Test
// ============================================================================

/// Soak test iteration count (20-50 queries).
const soak_iterations = 25

/// Maximum allowed growth in process count from baseline.
const max_process_growth = 50

/// Maximum allowed growth in ETS table count from baseline.
const max_ets_growth = 10

/// Maximum allowed growth in port count from baseline.
const max_port_growth = 10

/// Resource counts snapshot.
pub type ResourceCounts {
  ResourceCounts(process_count: Int, ets_count: Int, port_count: Int)
}

/// SOAK-01: Run multiple sequential queries and verify resource stability.
/// Skips unless --soak flag provided.
pub fn soak_01_resource_stability_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      case helpers.skip_if_no_soak() {
        Error(msg) -> {
          io.println(msg)
          Nil
        }
        Ok(Nil) -> run_soak_test()
      }
    }
  }
}

/// Execute the actual soak test after flag checks pass.
fn run_soak_test() -> Nil {
  let ctx = helpers.new_test_context("soak_01_resource_stability")

  // Take baseline resource counts
  let ctx = helpers.test_step(ctx, "baseline_resources")
  let baseline = get_resource_counts()
  log_resources(ctx, "baseline", 0, baseline)

  // Run iterations
  let ctx = helpers.test_step(ctx, "run_iterations")
  let result = run_iterations(ctx, 1, soak_iterations, baseline, [])

  case result {
    Error(#(ctx, iteration, error_msg)) -> {
      helpers.log_error(ctx, "soak_failed", error_msg)
      helpers.log_test_complete(
        ctx,
        False,
        "Soak test failed at iteration " <> int.to_string(iteration),
      )
      should.fail()
    }
    Ok(#(ctx, _resource_history)) -> {
      // Take final resource counts
      let ctx = helpers.test_step(ctx, "final_resources")
      let final_counts = get_resource_counts()
      log_resources(ctx, "final", soak_iterations, final_counts)

      // Verify resource stability
      let ctx = helpers.test_step(ctx, "verify_stability")
      let process_growth = final_counts.process_count - baseline.process_count
      let ets_growth = final_counts.ets_count - baseline.ets_count
      let port_growth = final_counts.port_count - baseline.port_count

      helpers.log_info_with(ctx, "resource_growth", [
        #("process_growth", json.int(process_growth)),
        #("ets_growth", json.int(ets_growth)),
        #("port_growth", json.int(port_growth)),
        #("iterations", json.int(soak_iterations)),
      ])

      // Assert: counts don't grow unbounded
      let process_ok = process_growth <= max_process_growth
      let ets_ok = ets_growth <= max_ets_growth
      let port_ok = port_growth <= max_port_growth

      case process_ok && ets_ok && port_ok {
        True -> {
          helpers.log_test_complete(
            ctx,
            True,
            "All "
              <> int.to_string(soak_iterations)
              <> " iterations completed, resources stable",
          )
          Nil
        }
        False -> {
          let failure_reason =
            build_failure_reason(
              process_ok,
              process_growth,
              ets_ok,
              ets_growth,
              port_ok,
              port_growth,
            )
          helpers.log_error(ctx, "resource_leak_detected", failure_reason)
          helpers.log_test_complete(ctx, False, failure_reason)
          should.fail()
        }
      }
    }
  }
}

/// Run soak test iterations recursively.
/// Returns Ok with context and resource history on success,
/// Error with context, iteration number, and error message on failure.
fn run_iterations(
  ctx: helpers.TestContext,
  current: Int,
  total: Int,
  baseline: ResourceCounts,
  history: List(ResourceCounts),
) -> Result(
  #(helpers.TestContext, List(ResourceCounts)),
  #(helpers.TestContext, Int, String),
) {
  case current > total {
    True -> Ok(#(ctx, list.reverse(history)))
    False -> {
      // Run a simple query
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)

      case helpers.query_and_consume_with_timeout("Say 'pong'", opts, 30_000) {
        helpers.QueryFailure(err) -> {
          Error(#(ctx, current, "Query failed: " <> error_to_string(err)))
        }
        helpers.QueryTimedOut -> {
          // Timeout is acceptable - log and continue
          helpers.log_info_with(ctx, "iteration_timeout", [
            #("iteration", json.int(current)),
          ])
          |> fn(_) { Nil }

          // Get resources after timeout
          let counts = get_resource_counts()
          log_resources(ctx, "iteration_timeout", current, counts)
          run_iterations(ctx, current + 1, total, baseline, [counts, ..history])
        }
        helpers.QuerySuccess(_result) -> {
          // Get resource counts after query completes
          let counts = get_resource_counts()
          log_resources(ctx, "iteration", current, counts)

          // Continue to next iteration
          run_iterations(ctx, current + 1, total, baseline, [counts, ..history])
        }
      }
    }
  }
}

/// Log resource counts for an iteration.
fn log_resources(
  ctx: helpers.TestContext,
  phase: String,
  iteration: Int,
  counts: ResourceCounts,
) -> Nil {
  helpers.log_info_with(ctx, "resource_snapshot", [
    #("phase", json.string(phase)),
    #("iteration", json.int(iteration)),
    #("process_count", json.int(counts.process_count)),
    #("ets_count", json.int(counts.ets_count)),
    #("port_count", json.int(counts.port_count)),
  ])
}

/// Build failure reason string for resource leak detection.
fn build_failure_reason(
  process_ok: Bool,
  process_growth: Int,
  ets_ok: Bool,
  ets_growth: Int,
  port_ok: Bool,
  port_growth: Int,
) -> String {
  let parts = []

  let parts = case process_ok {
    True -> parts
    False -> [
      "process leak ("
        <> int.to_string(process_growth)
        <> " > "
        <> int.to_string(max_process_growth)
        <> ")",
      ..parts
    ]
  }

  let parts = case ets_ok {
    True -> parts
    False -> [
      "ETS leak ("
        <> int.to_string(ets_growth)
        <> " > "
        <> int.to_string(max_ets_growth)
        <> ")",
      ..parts
    ]
  }

  let parts = case port_ok {
    True -> parts
    False -> [
      "port leak ("
        <> int.to_string(port_growth)
        <> " > "
        <> int.to_string(max_port_growth)
        <> ")",
      ..parts
    ]
  }

  case parts {
    [] -> "Unknown resource issue"
    _ -> "Resource leaks detected: " <> string_join(parts, ", ")
  }
}

/// Join strings with separator.
fn string_join(parts: List(String), sep: String) -> String {
  case parts {
    [] -> ""
    [only] -> only
    [first, ..rest] -> first <> sep <> string_join(rest, sep)
  }
}

// ============================================================================
// Resource Counting FFI
// ============================================================================

/// Get current resource counts (process, ETS tables, ports).
fn get_resource_counts() -> ResourceCounts {
  ResourceCounts(
    process_count: get_process_count(),
    ets_count: get_ets_count(),
    port_count: get_port_count(),
  )
}

/// Get current Erlang process count.
@external(erlang, "e2e_helpers_ffi", "get_process_count")
fn get_process_count() -> Int

/// Get current ETS table count.
@external(erlang, "e2e_helpers_ffi", "get_ets_count")
fn get_ets_count() -> Int

/// Get current Erlang port count.
@external(erlang, "e2e_helpers_ffi", "get_port_count")
fn get_port_count() -> Int
