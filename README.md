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

## Testing

### Test Categories

| Category | Description | External Dependencies |
|----------|-------------|----------------------|
| **Unit tests** | Schema parsing, type construction | None |
| **Integration tests** | Real CLI interaction | Claude CLI + auth |
| **Phase 0 tests** | FFI/runtime validation | Erlang runtime |

### Running Tests

```bash
# Unit tests only (default, no CLI required)
gleam test

# Integration tests (requires Claude CLI installed and authenticated)
CLAUDE_INTEGRATION_TEST=1 gleam test

# Phase 0 FFI validation tests
PHASE0_RUNTIME=1 gleam test

# Integration tests with non-JSON tolerance (for CLI versions with extra output)
CLAUDE_INTEGRATION_TEST=1 CLAUDE_INTEGRATION_ALLOW_NONJSON=1 gleam test
```

### Environment Variables

| Variable | Purpose |
|----------|---------|
| `ANTHROPIC_API_KEY` | API key for CLI authentication (alternative to `claude login`) |
| `CLAUDE_INTEGRATION_TEST` | Set to `1` to enable integration tests |
| `CLAUDE_INTEGRATION_ALLOW_NONJSON` | Set to `1` to tolerate non-JSON CLI output |
| `PHASE0_RUNTIME` | Set to `1` to enable Phase 0 FFI validation tests |

### Prerequisites for Integration Tests

1. Claude CLI installed (`claude --version` works)
2. Authentication via one of:
   - `claude login` (interactive)
   - `ANTHROPIC_API_KEY` environment variable

Spec from https://gist.github.com/SamSaffron/603648958a8c18ceae34939a8951d417
