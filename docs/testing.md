# Testing

The SDK can run without a real CLI process by enabling test mode. This lets you
unit-test your stream handling logic deterministically.

## Test mode basics

- Create a `Runner` with `runner.test_runner(...)`
- Enable it with `with_test_mode(runner)`
- `query()` will use your runner instead of spawning the CLI

```gleam
import claude_agent_sdk
import claude_agent_sdk/runner
import gleam/dynamic

let runner = runner.test_runner(
  on_spawn: fn(_cmd, _args, _cwd) { Ok(dynamic.from("handle")) },
  on_read: fn(_handle) { runner.Eof },
  on_close: fn(_handle) { Nil },
)

let options = claude_agent_sdk.default_options()
  |> claude_agent_sdk.with_test_mode(runner)
```

`ReadResult` options:

- `Data(BitArray)`
- `ExitStatus(Int)`
- `ReadError(String)`
- `Eof`

## Simulating a full response

A realistic test runner emits JSON lines followed by `ExitStatus(0)`.
The SDK expects each JSON object to be terminated by a newline.

Example sketch:

```gleam
import claude_agent_sdk/runner
import gleam/bit_array
import gleam/dynamic

// Pseudocode structure: store remaining chunks per handle
let runner = runner.test_runner(
  on_spawn: fn(_, _, _) {
    // return a handle key and store state in ETS or another store
    Ok(dynamic.from("handle"))
  },
  on_read: fn(handle) {
    // fetch next chunk from your store
    runner.Data(bit_array.from_string("{\"type\":\"result\"}\n"))
  },
  on_close: fn(_handle) { Nil },
)
```

## When to use test mode

- You want to test message parsing and stream handling
- You want deterministic test data without CLI/network dependencies
- You want to simulate error conditions (exit code, malformed JSON, etc.)
