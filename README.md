# claude_agent_sdk_gleam

[![Package Version](https://img.shields.io/hexpm/v/claude_agent_sdk_gleam)](https://hex.pm/packages/claude_agent_sdk_gleam)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/claude_agent_sdk_gleam/)

A Gleam SDK for Claude Code.

Hex package: `claude_agent_sdk_gleam`  
Gleam module namespace: `claude_agent_sdk`

## Prerequisites

- **Claude Code** installed and accessible in PATH (`claude --version` works)
- **Authentication** via `claude auth login`
- **Gleam** 1.0 or later

## Installation

```sh
gleam add claude_agent_sdk_gleam
```

## Quick Start

```gleam
import claude_agent_sdk.{query, default_options, with_max_turns, next, close}
import claude_agent_sdk/error
import gleam/io

pub fn main() {
  let options = default_options()
    |> with_max_turns(1)

  case query("Hello, Claude!", options) {
    Ok(stream) -> consume_stream(stream)
    Error(e) -> io.println("Error: " <> error.error_to_string(e))
  }
}

fn consume_stream(stream) {
  case next(stream) {
    #(Ok(error.EndOfStream), _) -> io.println("Done!")
    #(Ok(error.Message(envelope)), new_stream) -> {
      io.println("Message: " <> envelope.raw_json)
      consume_stream(new_stream)
    }
    #(Ok(_), new_stream) -> consume_stream(new_stream)
    #(Error(e), _) -> io.println("Stream error: " <> error.stream_error_to_string(e))
  }
}
```

## Documentation

- [Docs index](docs/README.md)
- [Quickstart](docs/quickstart.md)
- [Building an app](docs/building-an-app.md)
- [Streaming API](docs/streaming.md)
- [Options and configuration](docs/options.md)
- [Messages and content](docs/messages-and-content.md)
- [Tools and permissions](docs/tools-and-permissions.md)
- [Errors and troubleshooting](docs/errors-and-troubleshooting.md)
- [Testing](docs/testing.md)

## API Overview

### Core Functions

| Function | Description |
|----------|-------------|
| `query(prompt, options)` | Start a streaming query, returns `Result(QueryStream, QueryError)` |
| `next(stream)` | Read next item from stream, returns `#(Result(StreamItem, StreamError), QueryStream)` |
| `close(stream)` | Close the stream and release resources |
| `is_closed(stream)` | Check if stream is closed |

### Types

| Type | Description |
|------|-------------|
| `QueryOptions` | Configuration for queries (model, max_turns, system_prompt, etc.) |
| `QueryStream` | Opaque handle to a streaming query |
| `StreamItem` | One of: `Message(MessageEnvelope)`, `WarningEvent(Warning)`, `EndOfStream` |
| `Message` | One of: `System`, `Assistant`, `User`, `Result` |
| `QueryError` | Query start failures (CLI not found, spawn failed, version mismatch) |
| `StreamError` | Stream read failures (process exit, decode errors, buffer overflow) |

### Option Builders

```gleam
default_options()
  |> with_model("sonnet")
  |> with_max_turns(5)
  |> with_max_budget(1.0)
  |> with_system_prompt("You are a helpful assistant.")
  |> with_allowed_tools(["Read", "Write"])
  |> with_permission_mode(AcceptEdits)
```

### Stream Helpers

| Function | Description |
|----------|-------------|
| `with_stream(stream, callback)` | Execute callback with automatic cleanup |
| `collect_items(stream)` | Collect all items into `CollectResult` |
| `collect_messages(stream)` | Collect only message envelopes |
| `fold_stream(stream, acc, f)` | Fold with early termination support |
| `to_yielder(stream)` | Convert to `Yielder` (leak risk - see below) |

### Error Handling

```gleam
import claude_agent_sdk/error.{is_terminal}

// Check if error is terminal (stream cannot continue)
case is_terminal(error) {
  True -> // Must close stream
  False -> // Can continue reading
}
```

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

### Running Tests

```bash
# Unit tests only (default, no CLI required)
gleam test

# E2E tests (requires Claude CLI + auth)
gleam test -- --e2e
```

### Prerequisites for E2E Tests

1. Claude CLI installed (`claude --version` works)
2. Authentication via `claude auth login` (interactive)

### E2E Tests

End-to-end tests validate the full SDK against a real Claude CLI. See [`docs/E2E_TESTING.md`](docs/E2E_TESTING.md) for detailed documentation.

**Note on permission-deny behavior:** The SDK sends permission-deny responses over the control protocol, but the real Claude CLI may still execute tools in some environments. E2E tests therefore treat deny enforcement as best-effort and focus on verifying that permission callbacks are invoked and responses are sent, not that the CLI always blocks execution.

#### Running Locally

```bash
# Install Claude CLI (if not already installed)
npm install -g @anthropic-ai/claude-code

# Authenticate
claude auth login

# Run E2E via gleam test (runs unit + E2E)
gleam test -- --e2e
```

#### Running in CI

E2E tests are **opt-in** in CI to avoid running on every PR. They run when:

| Trigger | How to Use |
|---------|-----------|
| **Label** | Add `run-e2e` label to a PR |
| **Manual** | Use "Run workflow" button in Actions tab |
| **Schedule** | Automatic weekly run (Sundays 02:00 UTC) |

**CI requirements:**
- Claude CLI must be installed and authenticated in the runner
- Run tests with `gleam test -- --e2e`

#### E2E Artifacts

Each E2E run produces artifacts in `artifacts/e2e/<run_id>/`:
- `events.jsonl` - Structured event log
- `summary.txt` - Human-readable results
- `metadata.json` - Run metadata (versions, timing)
- `stdout.txt`, `stderr.txt` - Redacted CLI output

In CI, artifacts are uploaded and retained for 7 days.

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
import claude_agent_sdk/error

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
      process.send(result_subject, Ok(error.EndOfStream))
    }
    Error(Nil) -> {
      // No cancellation - proceed with blocking read
      case next(stream) {
        #(Ok(error.EndOfStream), _) -> {
          process.send(result_subject, Ok(error.EndOfStream))
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
import gleam/io
import gleam/otp/task
import gleam/string
import claude_agent_sdk.{default_options}

pub fn main() {
  let result_subject = process.new_subject()
  let cancel_subject = process.new_subject()

  // Start the cancellable query (using query_with_cancellation from above)
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
    Ok(Ok(item)) -> io.println("Received: " <> string.inspect(item))
    Ok(Error(e)) -> io.println("Error: " <> string.inspect(e))
    Error(Nil) -> io.println("Timeout waiting for response")
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
import claude_agent_sdk/error
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
    #(Ok(error.EndOfStream), _) -> {
      io.println("Stream complete")
    }
    #(Ok(error.Message(envelope)), new_stream) -> {
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
import claude_agent_sdk/error

fn handle_stream_error(err: StreamError) -> ErrorAction {
  case err {
    // JSON decode errors may be recoverable (skip malformed message)
    error.JsonDecodeError(_, _) -> Continue

    // Process errors are terminal - the CLI exited
    error.ProcessError(_, _) -> Stop

    // Unexpected messages can be logged and skipped
    error.UnexpectedMessageError(_) -> Continue

    // Buffer overflow is terminal
    error.BufferOverflow -> Stop

    // Too many decode errors is terminal
    error.TooManyDecodeErrors(_, _) -> Stop
  }
}

type ErrorAction {
  Continue
  Stop
}
```

## Success Semantics in Automation

**Important:** By default, the SDK treats `Result` as authoritative for success/failure.
A non-zero exit code AFTER a `Result` message produces only a *warning*, not an error.

**For CI pipelines, automation, or data reliability contexts**, we recommend treating
any warnings as potential failures:

```gleam
import claude_agent_sdk.{collect_messages}
import claude_agent_sdk/error
import gleam/list

pub fn strict_query(stream) {
  let result = collect_messages(stream)
  case list.is_empty(result.warnings) {
    True -> // Clean success
      Ok(result.items)
    False -> // Has warnings - fail in strict contexts
      case list.any(result.warnings, is_exit_warning) {
        True -> Error("Non-zero exit after Result - possible data integrity issue")
        False -> Ok(result.items)  // Other warnings may be acceptable
      }
  }
}

fn is_exit_warning(w) -> Bool {
  case w.code {
    error.NonZeroExitAfterResult(_) -> True
    _ -> False
  }
}
```

This ensures pipeline-grade success semantics without waiting for a future `strict_exit_status` option.

### CI/Automation: Strict Warning Mode

For pipelines where warnings should fail the build:

```gleam
import claude_agent_sdk.{
  type StreamItem, with_stream, next, query, default_options,
}
import claude_agent_sdk/error
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
    #(Ok(error.EndOfStream), _) -> Ok(acc)
    #(Ok(error.Message(envelope)), new_stream) -> {
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
    #(Ok(error.WarningEvent(warning)), _) -> {
      // Fail immediately on any warning in CI mode
      Error("Warning: " <> warning.message)
    }
    #(Error(e), _) -> Error("Stream error: " <> string.inspect(e))
  }
}
```

## Feature Comparison with Python SDK

This table compares functionality between this Gleam SDK and the official [Python SDK](https://github.com/anthropics/claude-agent-sdk-python).

| Feature | Gleam | Python | Notes |
|---------|:-----:|:------:|-------|
| **Core Query API** | ✅ | ✅ | Both support streaming queries |
| **Unidirectional Streaming** | ✅ | ✅ | Send prompt, receive message stream |
| **Bidirectional Client** | ❌ | ✅ | Python has `ClaudeSDKClient` for interactive sessions |
| **Session Resume** | ✅ | ✅ | Resume by session ID |
| **Session Continue** | ✅ | ✅ | Continue most recent session |
| **Session Fork** | ❌ | ✅ | Fork to new session from existing |
| **File Checkpointing** | ❌ | ✅ | Track and rewind file changes |
| **Permission Modes** | ✅ | ✅ | default, acceptEdits, plan, bypassPermissions |
| **Dynamic Permission Mode** | ❌ | ✅ | Change permission mode mid-session |
| **Tool Filtering** | ✅ | ✅ | `allowed_tools`, `disallowed_tools` |
| **Runtime Permission Callback** | ❌ | ✅ | Python has `can_use_tool` callback |
| **Hook System** | ❌ | ✅ | PreToolUse, PostToolUse, Stop, etc. |
| **MCP Config File** | ✅ | ✅ | External MCP server configuration |
| **In-Process MCP Tools** | ❌ | ✅ | Python has `@tool` decorator |
| **Model Selection** | ✅ | ✅ | Set model in options |
| **Dynamic Model Switching** | ❌ | ✅ | Change model mid-session |
| **System Prompt** | ✅ | ✅ | Replace or append |
| **Max Turns** | ✅ | ✅ | Limit agent turns |
| **Max Budget** | ✅ | ✅ | Cost limit in USD |
| **Extended Thinking** | ❌ | ✅ | `max_thinking_tokens` |
| **Structured Output** | ❌ | ✅ | JSON schema output format |
| **Interrupt Execution** | ❌ | ✅ | Stop execution mid-stream |
| **Custom Agents** | ❌ | ✅ | Define sub-agents |
| **Bash Sandboxing** | ❌ | ✅ | Sandbox configuration |
| **Test Mode** | ✅ | ✅ | Mock runner / transport for testing |
| **Warning Collection** | ✅ | ✅ | Non-fatal issue reporting |
| **Error Diagnostics** | ✅ | ✅ | Detailed error context |

### Architecture Differences

- **Gleam**: Synchronous blocking reads via Erlang ports. Single-process ownership model.
- **Python**: Async/await with bidirectional control protocol. Supports interactive sessions.

The Gleam SDK focuses on reliable unidirectional streaming suitable for automation and batch processing. For interactive applications requiring mid-session control, consider the Python SDK.

## Troubleshooting

### Empty stdout errors in CI/containers

When running in non-interactive environments, stderr may not be visible.
To diagnose errors, redirect stderr to a file:

```bash
your_gleam_app 2>/tmp/claude_stderr.log
```

Common causes of empty stdout with non-zero exit:
- **Not authenticated**: Authenticate the CLI (e.g., `claude auth login`)
- **Network issues**: Check connectivity to Anthropic API
- **Config errors**: Check `~/.claude/` configuration

### Stderr visibility across environments

The SDK relies on stderr being visible in your terminal for detailed CLI error messages. However, stderr may be unavailable in certain deployment contexts:

| Environment | Stderr Behavior | Solution |
|-------------|----------------|----------|
| Interactive terminal | Visible directly | Default case, no action needed |
| Daemon/service | May be lost or redirected to syslog | Redirect stderr: `my_app 2>/var/log/my_app.stderr` |
| CI/CD pipelines | Usually captured in job logs | Check job output for CLI messages |
| GUI applications | May be discarded | Redirect stderr before launching |
| Containers | Depends on logging driver | Check container logs |

### ProcessError diagnostics

When you receive a `ProcessError`, check the `ErrorDiagnostic` for guidance:

```gleam
case error {
  ProcessError(exit_code, diagnostic) -> {
    io.println("Exit code: " <> int.to_string(exit_code))
    io.println("Hint: " <> diagnostic.hint)
    io.println("Action: " <> diagnostic.troubleshooting)
  }
  _ -> // Other error types
}
```

If running without an attached terminal (daemon, service, container), stderr output may not be visible. Rerun with stderr redirected to a file: `your_app 2>/tmp/stderr.log`
