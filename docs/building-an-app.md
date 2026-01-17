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
                    content.TextBlock(t) -> Ok(t)
                    _ -> Error(Nil)
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

## Bidirectional sessions

For interactive or long-running scenarios, use bidirectional mode. This enables
multi-turn conversations, hooks for observability, and permission callbacks.

### Starting a session

```gleam
import claude_agent_sdk
import claude_agent_sdk/options
import gleam/erlang/process

pub fn main() {
  let cli_opts = claude_agent_sdk.cli_options()
    |> options.with_model("sonnet")
    |> options.with_permission_mode(options.AcceptEdits)

  let sdk_opts = claude_agent_sdk.sdk_options()
  let bidir_opts = claude_agent_sdk.bidir_options()

  case claude_agent_sdk.start_session_new(cli_opts, sdk_opts, bidir_opts) {
    Ok(session) -> handle_session(session)
    Error(err) -> panic as "Failed to start session"
  }
}
```

### Sending messages

Use `send_user_message` to send follow-up prompts during an active session:

```gleam
import claude_agent_sdk
import claude_agent_sdk/error

pub fn send_followup(session: claude_agent_sdk.Session) {
  case claude_agent_sdk.send_user_message(session, "What files did you change?") {
    Ok(Nil) -> Nil  // Message accepted for delivery
    Error(error.ControlSessionClosed) -> {
      // Session ended; can't send more messages
      Nil
    }
  }
}
```

The session queues messages internally; responses arrive via the messages subject.

### Receiving messages and events

```gleam
import claude_agent_sdk
import gleam/erlang/process

pub fn receive_loop(session: claude_agent_sdk.Session) {
  let messages_subject = claude_agent_sdk.messages(session)
  let events_subject = claude_agent_sdk.events(session)

  // Receive messages with timeout
  case process.receive(messages_subject, 5000) {
    Ok(message) -> {
      // Handle assistant/user/system message
      receive_loop(session)
    }
    Error(Nil) -> {
      // Check for lifecycle events
      case process.receive(events_subject, 0) {
        Ok(event) -> handle_event(event)
        Error(Nil) -> receive_loop(session)
      }
    }
  }
}
```

### Session lifecycle

Sessions emit `SessionEvent` values for lifecycle changes:

- `Stopped(reason)` - Session ended normally
- `Error(err)` - Session failed
- `Started` - Session initialized

Use `stop_session` for graceful shutdown or `interrupt` to cancel current work.

## Hooks configuration

Hooks let you observe and control session behavior. Configure them in `BidirOptions`:

```gleam
import claude_agent_sdk
import claude_agent_sdk/options
import claude_agent_sdk/hook

let bidir_opts = claude_agent_sdk.bidir_options()
  |> options.bidir_with_on_pre_tool_use(fn(ctx: hook.PreToolUseContext) {
    // Log tool invocations
    io.println("Tool: " <> ctx.tool_name)
    hook.Continue
  })
  |> options.bidir_with_on_post_tool_use(fn(ctx: hook.PostToolUseContext) {
    // Audit tool results
    hook.Continue
  })
```

### Available hooks

| Hook | Context | Returns | When |
|------|---------|---------|------|
| `on_pre_tool_use` | `PreToolUseContext` | `HookExecutionResult` | Before tool executes |
| `on_post_tool_use` | `PostToolUseContext` | `HookExecutionResult` | After tool completes |
| `on_user_prompt_submit` | `UserPromptSubmitContext` | `HookExecutionResult` | Before prompt processing |
| `on_stop` | `StopContext` | `HookExecutionResult` | Session stopping |
| `on_subagent_stop` | `SubagentStopContext` | `HookExecutionResult` | Subagent stopping |
| `on_pre_compact` | `PreCompactContext` | `HookExecutionResult` | Before context compaction |
| `on_can_use_tool` | `CanUseToolContext` | `PermissionCheckResult` | Permission check |

### Hook return values

`HookExecutionResult` controls flow:
- `Continue` — proceed normally
- `Block(reason)` — stop the operation
- `ModifyInput(new_input)` — change the input before proceeding

`PermissionCheckResult` for permission hooks:
- `Allow` — permit the operation
- `Deny(reason)` — reject the operation
