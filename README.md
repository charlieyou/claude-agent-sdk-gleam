# claude_agent_sdk_gleam

[![Package Version](https://img.shields.io/hexpm/v/claude_agent_sdk_gleam)](https://hex.pm/packages/claude_agent_sdk_gleam)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/claude_agent_sdk_gleam/)

```sh
gleam add claude_agent_sdk_gleam@1
```
```gleam
import claude_agent_sdk_gleam

pub fn main() -> Nil {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/claude_agent_sdk_gleam>.

## Stream Operations

The SDK provides several ways to consume query streams:

### Safe Resource Helpers (Recommended)

These functions guarantee cleanup even with early termination:

- `with_stream(stream, callback)` - Execute callback with automatic cleanup
- `collect_items(stream)` - Collect all stream items into a list
- `collect_messages(stream)` - Collect only message envelopes
- `fold_stream(stream, acc, f)` - Fold with early termination support

### Advanced: Iterator/Yielder Interop

```gleam
to_yielder(stream) -> Yielder(Result(StreamItem, StreamError))
```

**⚠️ WARNING: Leak Risk**

`to_yielder()` does NOT guarantee cleanup. Port resources may leak if:
- The yielder is not fully consumed
- An exception occurs during iteration
- Early break from iteration (e.g., via `take()`)

**Prefer** `with_stream()`, `collect_items()`, or `fold_stream()` for guaranteed cleanup.

**Use `to_yielder()` only when:**
- You need gleam/yielder combinators (map, filter, zip, etc.)
- Cleanup is handled externally (e.g., supervisor process)
- You guarantee full consumption of the yielder

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

Spec from https://gist.github.com/SamSaffron/603648958a8c18ceae34939a8951d417
