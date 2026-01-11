# claude_agent_sdk

[![Package Version](https://img.shields.io/hexpm/v/claude_agent_sdk)](https://hex.pm/packages/claude_agent_sdk)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/claude_agent_sdk/)

```sh
gleam add claude_agent_sdk@1
```
```gleam
import claude_agent_sdk

pub fn main() -> Nil {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/claude_agent_sdk>.

## Process Ownership Contract

**Critical**: QueryStream is not process-safe; use from the process that created it.

Behavior is **UNDEFINED** if:
- `next()` is called from a different process than `query()`
- `next()` is called from multiple processes concurrently
- `close()` is called from a different process than `query()`

### Why This Matters

Erlang ports deliver messages to the spawning process's mailbox. This creates fundamental constraints:
- `next()` can only receive messages in the process that called `query()`
- `close()` from another process closes the port but doesn't unblock a waiting `next()`
- Cross-process use leads to deadlock or resource leaks

The SDK does not detect cross-process use; violating this contract results in undefined behavior (typically deadlock).

### Cancellation

Because `next()` blocks, cross-process cancellation requires an OTP wrapper. The `close()` function is only effective when called by the process that spawned the query. See the [Cancellation Recipe](#cancellation-recipe) for patterns to safely cancel from another process.

See the `stream.gleam` module documentation for detailed API constraints.

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

## Advanced Usage

### Cancellation Recipe

Because `next()` blocks waiting for messages, you cannot cancel a query from the same process. The solution is to spawn a dedicated process to own the stream and communicate via OTP subjects.

**Dependencies:** This pattern requires `gleam_otp`:
```bash
gleam add gleam_otp
```

**Implementation:**

```gleam
import gleam/erlang/process.{type Subject}
import gleam/otp/task
import claude_agent_sdk.{
  type QueryOptions, type QueryStream, type StreamError, type StreamItem,
  close, next, query,
}
import claude_agent_sdk/error.{EndOfStream}

/// Message type for cancellation signal
pub type Cancel {
  Cancel
}

/// Error wrapper for query failures
pub type StreamResult {
  QueryStartError(claude_agent_sdk.QueryError)
  StreamReadError(claude_agent_sdk.StreamError)
}

/// Run a query with cancellation support.
/// Stream items are sent to result_subject.
/// Send Cancel to cancel_subject to abort the query.
pub fn query_with_cancellation(
  prompt: String,
  options: QueryOptions,
  result_subject: Subject(Result(StreamItem, StreamResult)),
  cancel_subject: Subject(Cancel),
) -> task.Task(Nil) {
  task.async(fn() {
    // This spawned process owns the stream
    case query(prompt, options) {
      Error(e) -> {
        process.send(result_subject, Error(QueryStartError(e)))
      }
      Ok(stream) -> {
        consume_with_cancellation(stream, result_subject, cancel_subject)
      }
    }
  })
}

fn consume_with_cancellation(
  stream: QueryStream,
  result_subject: Subject(Result(StreamItem, StreamResult)),
  cancel_subject: Subject(Cancel),
) -> Nil {
  // Check for cancellation before each blocking read
  case process.receive(cancel_subject, 0) {
    Ok(Cancel) -> {
      // Cancellation requested - close and exit
      close(stream)
      process.send(result_subject, Ok(EndOfStream))
    }
    Error(Nil) -> {
      // No cancellation - proceed with blocking read
      case next(stream) {
        #(Ok(EndOfStream), _) -> {
          process.send(result_subject, Ok(EndOfStream))
        }
        #(Ok(item), new_stream) -> {
          process.send(result_subject, Ok(item))
          consume_with_cancellation(new_stream, result_subject, cancel_subject)
        }
        #(Error(e), _) -> {
          process.send(result_subject, Error(StreamReadError(e)))
        }
      }
    }
  }
}
```

**Usage:**

```gleam
import gleam/erlang/process
import gleam/otp/task

pub fn main() {
  let result_subject = process.new_subject()
  let cancel_subject = process.new_subject()

  // Start the cancellable query
  let _task = query_with_cancellation(
    "What is 2 + 2?",
    default_options(),
    result_subject,
    cancel_subject,
  )

  // To cancel at any time:
  // process.send(cancel_subject, Cancel)

  // Receive results with a timeout
  case process.receive(result_subject, 30_000) {
    Ok(Ok(item)) -> handle_item(item)
    Ok(Error(e)) -> handle_error(e)
    Error(Nil) -> handle_timeout()
  }
}
```

**Key points:**
1. The spawned task process owns the stream (created via `task.async`)
2. Parent communicates via subjects—no shared mutable state
3. Cancellation is checked between blocking `next()` calls (cooperative)
4. `close()` is always called when cancelling to release port resources
5. This pattern keeps the SDK non-actor while enabling cancellation

### Basic Streaming Loop

For simple consumption without cancellation:

```gleam
import claude_agent_sdk.{
  type QueryStream, type StreamItem, close, next, query, default_options,
}
import claude_agent_sdk/error.{EndOfStream, Message}
import gleam/io
import gleam/string

pub fn stream_query(prompt: String) {
  case query(prompt, default_options()) {
    Error(e) -> io.println("Query failed: " <> string.inspect(e))
    Ok(stream) -> consume_stream(stream)
  }
}

fn consume_stream(stream: QueryStream) -> Nil {
  case next(stream) {
    #(Ok(EndOfStream), _) -> {
      io.println("Stream complete")
    }
    #(Ok(Message(envelope)), new_stream) -> {
      io.println("Received message with raw_json: " <> envelope.raw_json)
      consume_stream(new_stream)
    }
    #(Ok(_other), new_stream) -> {
      // Skip non-message items (warnings, etc.)
      consume_stream(new_stream)
    }
    #(Error(e), stream) -> {
      io.println("Error: " <> string.inspect(e))
      close(stream)
    }
  }
}
```

### Error Handling Patterns

Handle recoverable vs terminal errors:

```gleam
import claude_agent_sdk.{type StreamError}
import claude_agent_sdk/error.{
  BufferOverflow, JsonDecodeError, ProcessError,
  TooManyDecodeErrors, UnexpectedMessageError,
}

fn handle_stream_error(error: StreamError) -> ErrorAction {
  case error {
    // JSON decode errors may be recoverable (skip malformed message)
    JsonDecodeError(_, _) -> Continue

    // Process errors are terminal - the CLI exited
    ProcessError(_, _) -> Stop

    // Unexpected messages can be logged and skipped
    UnexpectedMessageError(_) -> Continue

    // Buffer overflow is terminal
    BufferOverflow -> Stop

    // Too many decode errors is terminal
    TooManyDecodeErrors(_, _) -> Stop
  }
}

type ErrorAction {
  Continue
  Stop
}
```

### CI/Automation: Strict Warning Mode

For pipelines where warnings should fail the build:

```gleam
import claude_agent_sdk.{
  type StreamItem, with_stream, next, query, default_options,
}
import claude_agent_sdk/error.{EndOfStream, Message, WarningEvent}
import claude_agent_sdk/message.{Result as ResultMsg}
import gleam/string

pub fn ci_query(prompt: String) -> Result(String, String) {
  case query(prompt, default_options()) {
    Error(e) -> Error("Query failed: " <> string.inspect(e))
    Ok(stream) -> {
      with_stream(stream, fn(s) {
        check_for_warnings(s, "")
      })
    }
  }
}

fn check_for_warnings(stream, acc) {
  case next(stream) {
    #(Ok(EndOfStream), _) -> Ok(acc)
    #(Ok(Message(envelope)), new_stream) -> {
      // Check if this is a result message with warnings
      case envelope.message {
        ResultMsg(result_msg) -> {
          case result_msg.warnings {
            [] -> check_for_warnings(new_stream, acc <> result_msg.result)
            warnings -> Error("Warnings detected: " <> string.inspect(warnings))
          }
        }
        _ -> check_for_warnings(new_stream, acc)
      }
    }
    #(Ok(WarningEvent(warning)), _) -> {
      // Fail immediately on any warning in CI mode
      Error("Warning: " <> warning.message)
    }
    #(Error(e), _) -> Error("Stream error: " <> string.inspect(e))
  }
}
