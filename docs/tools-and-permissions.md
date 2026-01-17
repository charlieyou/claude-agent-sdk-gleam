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

## Hooks wiring

In bidirectional mode, hooks provide fine-grained control over tool execution.
Configure hooks in `BidirOptions` to observe, modify, or block operations.

### Tool use hooks

Pre and post tool use hooks let you observe every tool invocation:

```gleam
import claude_agent_sdk
import claude_agent_sdk/options
import claude_agent_sdk/hook

let bidir_opts = claude_agent_sdk.bidir_options()
  |> options.bidir_with_on_pre_tool_use(fn(ctx: hook.PreToolUseContext) {
    // ctx.tool_name: "Bash", "Read", "Write", etc.
    // ctx.tool_input: tool parameters as Dynamic
    // ctx.session_id: current session ID
    hook.Continue
  })
  |> options.bidir_with_on_post_tool_use(fn(ctx: hook.PostToolUseContext) {
    // ctx.tool_output: result as Dynamic
    hook.Continue
  })
```

### Permission callback (on_can_use_tool)

The `on_can_use_tool` hook provides programmatic permission decisions:

```gleam
import claude_agent_sdk/options
import claude_agent_sdk/hook

let bidir_opts = claude_agent_sdk.bidir_options()
  |> options.bidir_with_on_can_use_tool(fn(ctx: hook.CanUseToolContext) {
    case ctx.tool_name {
      "Bash" -> {
        // Check command, maybe deny dangerous operations
        hook.Allow
      }
      "Write" -> {
        // Could check ctx.blocked_path for path-based restrictions
        hook.Allow
      }
      _ -> hook.Allow
    }
  })
```

`CanUseToolContext` fields:
- `tool_name` — the tool being requested
- `tool_input` — tool parameters (Dynamic)
- `session_id` — current session
- `permission_suggestions` — hints from tool definition
- `blocked_path` — path that triggered the check, if applicable

Return `hook.Allow` to permit or `hook.Deny(reason)` to block.

### Hook timeouts

Configure per-hook timeouts to prevent slow hooks from blocking:

```gleam
import claude_agent_sdk/options
import claude_agent_sdk/hook
import gleam/dict

let timeouts = dict.from_list([
  #(hook.PreToolUse, 5000),   // 5 second timeout
  #(hook.PostToolUse, 3000),
])

let bidir_opts = claude_agent_sdk.bidir_options()
  |> options.bidir_with_hook_timeouts(timeouts)
```

If a hook times out, the operation proceeds (fail-open behavior).
