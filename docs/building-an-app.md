# Building an App

This guide shows a practical structure for integrating the SDK into a Gleam app.

## Suggested structure

- `app/agent.gleam` handles CLI calls and message parsing
- `app/view.gleam` formats output
- `app/main.gleam` wires the CLI input/output

## A simple agent module

```gleam
import claude_agent_sdk
import claude_agent_sdk/error
import claude_agent_sdk/message
import claude_agent_sdk/content
import gleam/list
import gleam/option

pub fn run(prompt: String) -> Result(List(String), String) {
  let options = claude_agent_sdk.default_options()

  let result = claude_agent_sdk.with_stream(
    claude_agent_sdk.query(prompt, options),
    fn(stream) { collect_text(stream, []) },
  )

  case result {
    Ok(lines) -> Ok(lines)
    Error(err) -> Error(error.error_to_string(err))
  }
}

fn collect_text(
  stream: claude_agent_sdk.QueryStream,
  acc: List(String),
) -> List(String) {
  let #(result, stream) = claude_agent_sdk.next(stream)
  case result {
    Ok(error.Message(envelope)) ->
      case envelope.message {
        message.Assistant(asst) ->
          case asst.message {
            option.Some(content.AssistantMessageContent(content: option.Some(blocks), ..)) ->
              let text =
                blocks
                |> list.filter_map(fn(block) {
                  case block {
                    content.TextBlock(t) -> option.Some(t)
                    _ -> option.None
                  }
                })
              collect_text(stream, list.append(acc, text))
            _ -> collect_text(stream, acc)
          }
        _ -> collect_text(stream, acc)
      }
    Ok(error.WarningEvent(_)) -> collect_text(stream, acc)
    Ok(error.EndOfStream) -> acc
    Error(err) ->
      case error.is_terminal(err) {
        True -> acc
        False -> collect_text(stream, acc)
      }
  }
}
```

## UI considerations

- Show partial assistant responses as they arrive
- Surface warnings in logs
- Use permission modes appropriate for your environment

## Common patterns

- Limit tool access with `with_allowed_tools`
- Use `with_max_turns` for predictable workflows
- Add a system prompt for consistent behavior
- Enable `with_permissive_version_check` for dev builds
