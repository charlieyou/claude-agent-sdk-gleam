# Errors and Troubleshooting

The SDK separates startup errors (before streaming begins) from runtime errors
that can occur while iterating a stream.

## QueryError (startup)

Returned by `query()` when a stream cannot be created.

Common causes:

- CLI not installed or not on PATH
- CLI version too old
- Version detection timed out
- Process spawn failure
- Test mode without a runner

```gleam
import claude_agent_sdk
import claude_agent_sdk/error
import gleam/io

case claude_agent_sdk.query(prompt, options) {
  Ok(stream) -> // iterate
  Error(err) -> io.println(error.error_to_string(err))
}
```

## StreamError (runtime)

Returned by `next()` when the stream encounters issues.

- Terminal: `ProcessError`, `BufferOverflow`, `TooManyDecodeErrors`
- Non-terminal: `JsonDecodeError`, `UnexpectedMessageError`

```gleam
import claude_agent_sdk
import claude_agent_sdk/error

case claude_agent_sdk.next(stream) {
  #(Error(err), stream) ->
    case error.is_terminal(err) {
      True -> claude_agent_sdk.close(stream)
      False -> // continue
    }
  _ -> // handle items
}
```

Use `error.stream_error_to_string(err)` to format messages for logs.

## Warnings

Warnings are surfaced as `WarningEvent` items during streaming. Common warnings:

- Unparseable CLI version (when permissive mode is enabled)
- Incomplete last line
- Exit status anomalies after a Result message

Warnings do not stop the stream.

## Troubleshooting checklist

### CLI not found

- Install: `npm install -g @anthropic-ai/claude-code`
- Verify: `claude --version`

### Authentication errors

The CLI may write errors to stderr only. If stdout is empty and the process
exits with code 1, check authentication:

- Run `claude auth login`

### Version errors

If you see `UnsupportedCliVersionError` or `UnknownVersionError`:

- Run `claude --version` manually
- Upgrade CLI: `npm update -g @anthropic-ai/claude-code`
- For development against unknown versions, enable
  `with_permissive_version_check()`

### Invalid arguments

Exit code 2 typically indicates unsupported flags. Ensure your SDK version
matches CLI capabilities.

### Long outputs / buffer overflow

The SDK enforces a 10MB per-line buffer limit for safety. If you expect very
large outputs, adjust your CLI usage to stream smaller chunks.
