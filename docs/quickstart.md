# Quickstart

This guide walks you from installation to the first streamed response.

## 1) Install the Claude Code CLI

```sh
npm install -g @anthropic-ai/claude-code
```

Authenticate:

```sh
claude auth login
```

## 2) Add the SDK dependency

```toml
[dependencies]
claude_agent_sdk_gleam = "~> 0.1.0"
```

## 3) Run a prompt and collect messages

Use `with_stream` + `collect_messages` for the simplest flow. This returns all
messages along with any warnings or non-terminal errors.

```gleam
import claude_agent_sdk
import claude_agent_sdk/error
import claude_agent_sdk/message
import claude_agent_sdk/content
import gleam/io
import gleam/list
import gleam/option

pub fn main() -> Nil {
  let options = claude_agent_sdk.default_options()

  let result = claude_agent_sdk.with_stream(
    claude_agent_sdk.query("Write a short commit message", options),
    claude_agent_sdk.collect_messages,
  )

  case result {
    Ok(collected) ->
      collected.items
      |> list.each(fn(envelope) {
        print_assistant(envelope.message)
      })
    Error(err) -> io.println(error.error_to_string(err))
  }
}

fn print_assistant(msg: message.Message) -> Nil {
  case msg {
    message.Assistant(asst) ->
      case asst.message {
        option.Some(content.AssistantMessageContent(content: option.Some(blocks), ..)) ->
          blocks
          |> list.each(fn(block) {
            case block {
              content.TextBlock(text) -> io.println(text)
              _ -> Nil
            }
          })
        _ -> Nil
      }
    _ -> Nil
  }
}
```

## 4) Stream incrementally

If you want to handle tokens and tool-use events as they arrive, use `next()`:

```gleam
import claude_agent_sdk
import claude_agent_sdk/error
import claude_agent_sdk/message
import gleam/io

fn consume(stream: claude_agent_sdk.QueryStream) -> Nil {
  let #(result, stream) = claude_agent_sdk.next(stream)
  case result {
    Ok(error.Message(envelope)) -> {
      case envelope.message {
        message.Assistant(_) -> io.println("assistant message")
        message.Result(_) -> io.println("done")
        _ -> Nil
      }
      consume(stream)
    }
    Ok(error.EndOfStream) -> {
      let _ = claude_agent_sdk.close(stream)
      Nil
    }
    Ok(error.WarningEvent(warning)) -> {
      io.println("warning: " <> warning.message)
      consume(stream)
    }
    Error(err) ->
      case error.is_terminal(err) {
        True -> {
          let _ = claude_agent_sdk.close(stream)
          Nil
        }
        False -> consume(stream)
      }
  }
}
```

## Next

- `docs/streaming.md` for stream semantics and lifecycle
- `docs/options.md` for configuration
- `docs/messages-and-content.md` for message schemas
- `docs/errors-and-troubleshooting.md` for failure modes
