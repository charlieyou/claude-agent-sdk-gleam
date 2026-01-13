# Streaming API

The SDK returns a `QueryStream` that yields a sequence of items from the CLI.
Use `next()` for incremental processing, or `collect_*` helpers for full
collection.

## Stream lifecycle

- `query(prompt, options)` returns `Result(QueryStream, QueryError)`
- `next(stream)` returns `#(Result(StreamItem, StreamError), QueryStream)`
- `close(stream)` releases resources (idempotent)

Always use the updated stream value returned by `next()`.

## Stream items

`StreamItem` has three variants:

- `Message(envelope)`
- `WarningEvent(warning)`
- `EndOfStream`

`WarningEvent` is non-fatal; continue iterating. `EndOfStream` means the CLI
completed normally.

## Incremental iteration

```gleam
import claude_agent_sdk
import claude_agent_sdk/error

fn loop(stream: claude_agent_sdk.QueryStream) -> Nil {
  let #(result, stream) = claude_agent_sdk.next(stream)
  case result {
    Ok(error.Message(_)) -> loop(stream)
    Ok(error.WarningEvent(_)) -> loop(stream)
    Ok(error.EndOfStream) -> {
      let _ = claude_agent_sdk.close(stream)
      Nil
    }
    Error(err) ->
      case error.is_terminal(err) {
        True -> {
          let _ = claude_agent_sdk.close(stream)
          Nil
        }
        False -> loop(stream)
      }
  }
}
```

## Collecting helpers

Use these when you want the whole conversation before processing:

- `with_stream(result, fn(stream) { ... })` ensures cleanup even if you panic
- `collect_items(stream)` returns all items including warnings
- `collect_messages(stream)` returns only `MessageEnvelope` values
- `fold_stream(stream, acc, fn(acc, item) { ... })` custom iteration

```gleam
let result = claude_agent_sdk.with_stream(
  claude_agent_sdk.query(prompt, options),
  claude_agent_sdk.collect_messages,
)
```

## Process ownership (important)

`QueryStream` must be used from the same process that called `query()`. The
underlying port delivers messages to the owning process only. Calling `next()`
or `close()` from another process will block or no-op.

If you need cross-process cancellation or fan-out, have the owner process
forward messages to other processes.

## Resource cleanup

- Always call `close(stream)` when done
- Prefer `with_stream` to ensure cleanup on errors

## Converting to yielder

`to_yielder(stream)` converts a stream into a `yielder`. Consume it fully to
avoid leaking resources.
