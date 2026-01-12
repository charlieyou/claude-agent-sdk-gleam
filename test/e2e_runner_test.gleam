import e2e/run_e2e
import gleam/io
import gleam/list
import gleeunit/should

@external(erlang, "claude_agent_sdk_ffi", "get_plain_arguments")
fn get_plain_arguments() -> List(String)

pub fn e2e_runner_test() {
  let args = get_plain_arguments()
  case list.contains(args, "--e2e") {
    True -> {
      let filtered_args = args |> list.filter(fn(arg) { arg != "--e2e" })
      case run_e2e.run(filtered_args) {
        Ok(summary) -> run_e2e.failed(summary) |> should.equal(0)
        Error(message) -> {
          io.println("E2E runner error: " <> message)
          should.equal(1, 0)
        }
      }
    }
    False -> {
      io.println("[SKIP] E2E runner disabled (pass --e2e to gleam test)")
    }
  }
}
