# Tools and Permissions

Claude Code can invoke tools (Read, Write, Bash, etc.). The SDK surfaces tool
requests and tool results through streamed messages.

## Tool use flow

1) Claude emits an `Assistant` message containing `ToolUseBlock` entries
2) The CLI executes tools (according to permission mode)
3) The CLI emits a `User` message with `ToolResultBlock` entries

The SDK does not execute tools itself; it reports what the CLI did.

## Restricting tools

Use `with_allowed_tools` or `with_disallowed_tools` to control tool access.

```gleam
import claude_agent_sdk

let options = claude_agent_sdk.default_options()
  |> claude_agent_sdk.with_allowed_tools(["Read", "Grep"])
```

Precedence: `allowed_tools` overrides `disallowed_tools` if both are set.

## Permission modes

```gleam
import claude_agent_sdk/options

options.Default
options.AcceptEdits
options.BypassPermissions
options.Plan
```

- `Default`: interactive prompts
- `AcceptEdits`: auto-accept file edits (permission mode `acceptEdits`)
- `BypassPermissions`: skip all prompts (`--dangerously-skip-permissions`)
- `Plan`: read-only exploration

Use `BypassPermissions` only in trusted environments.

## Detecting tool use in the stream

```gleam
import claude_agent_sdk/content
import claude_agent_sdk/message
import gleam/list
import gleam/option

fn handle(msg: message.Message) -> Nil {
  case msg {
    message.Assistant(asst) ->
      case asst.message {
        option.Some(content.AssistantMessageContent(content: option.Some(blocks), ..)) ->
          blocks
          |> list.each(fn(block) {
            case block {
              content.ToolUseBlock(id, name, input) -> {
                // log or audit tool request
                Nil
              }
              _ -> Nil
            }
          })
        _ -> Nil
      }
    _ -> Nil
  }
}
```

## MCP configuration

If you use MCP servers, provide a config file path:

```gleam
let options = claude_agent_sdk.default_options()
  |> claude_agent_sdk.with_mcp_config("./mcp.json")
```

MCP server status appears in the `System` message on session start.
