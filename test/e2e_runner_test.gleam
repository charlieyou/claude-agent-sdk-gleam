import e2e/run_e2e
import gleam/io
import gleeunit/should
import support/env_helpers.{get_env}

pub fn e2e_runner_test() {
  case get_env("CLAUDE_INTEGRATION_TEST") {
    Ok("1") -> {
      case run_e2e.run([]) {
        Ok(summary) -> run_e2e.failed(summary) |> should.equal(0)
        Error(message) -> {
          io.println("E2E runner error: " <> message)
          should.equal(1, 0)
        }
      }
    }
    _ -> {
      io.println("[SKIP] E2E runner disabled (set CLAUDE_INTEGRATION_TEST=1)")
    }
  }
}
