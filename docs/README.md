# Claude Agent SDK for Gleam

This SDK wraps the Claude Code CLI and gives you a typed, streaming API from Gleam.
It launches the CLI, reads its JSON-lines stream, and exposes messages, warnings,
and errors in a structured way.

## Requirements

- Claude Code CLI installed and available on PATH
- Authentication via `claude login` or `ANTHROPIC_API_KEY`

Install the CLI:

```sh
npm install -g @anthropic-ai/claude-code
```

## Install the SDK

Add the dependency to `gleam.toml`:

```toml
[dependencies]
claude_agent_sdk_gleam = "~> 0.1.0"
```

## Quickstart

```gleam
import claude_agent_sdk
import claude_agent_sdk/content
import claude_agent_sdk/error
import claude_agent_sdk/message
import gleam/io
import gleam/list
import gleam/option

pub fn main() -> Nil {
  let options = claude_agent_sdk.default_options()

  case claude_agent_sdk.query("Summarize this repo", options) {
    Ok(stream) -> loop(stream)
    Error(err) -> io.println(error.error_to_string(err))
  }
}

fn loop(stream: claude_agent_sdk.QueryStream) -> Nil {
  let #(result, stream) = claude_agent_sdk.next(stream)
  case result {
    Ok(error.Message(envelope)) -> {
      case envelope.message {
        message.Assistant(asst) ->
          case asst.message {
            option.Some(content.AssistantMessageContent(content: option.Some(blocks), ..)) ->
              print_blocks(blocks)
            _ -> Nil
          }
        _ -> Nil
      }
      loop(stream)
    }
    Ok(error.EndOfStream) -> claude_agent_sdk.close(stream)
    Ok(error.WarningEvent(_)) -> loop(stream)
    Error(err) ->
      case error.is_terminal(err) {
        True -> claude_agent_sdk.close(stream)
        False -> loop(stream)
      }
  }
}

fn print_blocks(blocks: List(content.ContentBlock)) -> Nil {
  blocks
  |> list.each(fn(block) {
    case block {
      content.TextBlock(text) -> io.println(text)
      _ -> Nil
    }
  })
}
```

## Docs

- `docs/quickstart.md`
- `docs/building-an-app.md`
- `docs/streaming.md`
- `docs/options.md`
- `docs/messages-and-content.md`
- `docs/tools-and-permissions.md`
- `docs/errors-and-troubleshooting.md`
- `docs/testing.md`
