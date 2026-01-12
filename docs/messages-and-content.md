# Messages and Content

The CLI outputs JSON lines. The SDK parses these into `MessageEnvelope` values
that contain both the parsed `Message` and the raw JSON/bytes.

## Message flow

A typical session emits messages in this order:

1) `System` (session metadata)
2) `Assistant` (Claude response, tool requests)
3) `User` (tool results)
4) repeat 2-3 as needed
5) `Result` (final summary and usage)

## Pattern matching

Variant constructors are not re-exported from the main module. For pattern
matching, import the source modules:

- `claude_agent_sdk/message` for `Message` variants
- `claude_agent_sdk/content` for `ContentBlock`
- `claude_agent_sdk/error` for `StreamItem`, `StreamError`, `Warning`

```gleam
import claude_agent_sdk/message
import claude_agent_sdk/content
import gleam/option

case envelope.message {
  message.System(sys) -> // session metadata
  message.Assistant(asst) ->
    case asst.message {
      option.Some(content.AssistantMessageContent(content: option.Some(blocks), ..)) ->
        handle_blocks(blocks)
      _ -> Nil
    }
  message.User(user) -> // tool results
  message.Result(res) -> // final result
}
```

## Content blocks

Assistant messages contain a list of `ContentBlock` values:

- `TextBlock(text)`
- `ToolUseBlock(id, name, input)`
- `UnknownBlock(raw)`

```gleam
import claude_agent_sdk/content
import gleam/io

case block {
  content.TextBlock(text) -> io.println(text)
  content.ToolUseBlock(id, name, input) -> // run tool or record
  content.UnknownBlock(raw) -> // forward compatibility
}
```

## Tool results

User messages contain `ToolResultBlock` values, which correspond to previous
`ToolUseBlock` requests.

```gleam
case tool_result {
  content.ToolResultBlock(tool_use_id, content, is_error) -> // handle output
}
```

## Result message

The final `Result` message includes:

- Completion subtype (success, max turns, budget, etc.)
- Usage statistics
- Final text content

Use this message as the definitive end of a successful conversation.
