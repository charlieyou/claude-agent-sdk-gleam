# Bidirectional Additions (New Content Only)

This document contains only the new content added to extend the spec from unidirectional to bidirectional control. It is organized by where the content was inserted in the spec.

## New Section: Bidirectional Control Protocol

## 6. Bidirectional Control Protocol

The bidirectional control protocol enables the SDK to send control requests to the CLI
and receive CLI-initiated control requests (hooks, permissions, SDK-hosted MCP servers).

### 6.1 Protocol Overview

Bidirectional mode is enabled when the CLI is launched with `--input-format stream-json`
and `--output-format stream-json`. In this mode:

- **Transport:** SDK → CLI uses stdin; CLI → SDK uses stdout.
- **Framing:** Newline-delimited JSON (NDJSON) in both directions.
- **Concurrency:** Control requests can be in-flight while normal messages stream.
- **Correlation:** Each request carries a `request_id`, echoed in the response.
- **IDs:** Any unique string is valid; SDKs typically use `req_<counter>_<hex>`.

### 6.2 Message Envelope Format

#### SDK → CLI (control_request)

```json
{
  "type": "control_request",
  "request_id": "req_1_a1b2c3d4",
  "request": {
    "subtype": "set_permission_mode",
    "mode": "acceptEdits"
  }
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `type` | string | yes | Always `"control_request"` |
| `request_id` | string | yes | Unique ID for correlation |
| `request` | object | yes | Subtype-specific payload |

#### CLI → SDK (control_response)

```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_1_a1b2c3d4",
    "response": {}
  }
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `type` | string | yes | Always `"control_response"` |
| `response.subtype` | string | yes | `"success"` or `"error"` |
| `response.request_id` | string | yes | Echoes the request ID |
| `response.response` | object | no | Success payload (may be empty) |
| `response.error` | string | no | Error message (if subtype is `"error"`) |

#### CLI → SDK (CLI-initiated control_request)

```json
{
  "type": "control_request",
  "request_id": "req_9_9f8e7d6c",
  "request": {
    "subtype": "hook_callback",
    "callback_id": "hook_3",
    "input": {"hook_event_name": "PreToolUse", "tool_name": "Bash", "tool_input": {"command": "ls"}},
    "tool_use_id": "toolu_01ABC"
  }
}
```

### 6.3 Initialization Handshake

At stream start, the SDK sends an `initialize` control request to register hooks
and discover CLI capabilities. The CLI responds with its supported commands and
capabilities. The SDK should treat unknown fields as opaque and forward-compatible.

**Initialize Request:**
```json
{
  "type": "control_request",
  "request_id": "req_0_init",
  "request": {
    "subtype": "initialize",
    "hooks": {
      "PreToolUse": [
        {
          "matcher": "Write|Edit|MultiEdit",
          "hookCallbackIds": ["hook_0", "hook_1"],
          "timeout": 60
        }
      ],
      "UserPromptSubmit": [
        {
          "matcher": null,
          "hookCallbackIds": ["hook_2"]
        }
      ]
    }
  }
}
```

**Initialize Response (typical):**
```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_0_init",
    "response": {
      "supported_commands": ["interrupt", "set_permission_mode", "set_model", "rewind_files"],
      "capabilities": {
        "hooks": true,
        "permissions": true,
        "mcp_sdk_servers": true
      }
    }
  }
}
```

#### Hook Registration Format

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `hooks` | object | no | Map of hook event name → list of matchers |
| `hooks.<event>[]` | object | yes | Hook matcher configuration |
| `hooks.<event>[].matcher` | string \| null | no | Tool matcher (see below) |
| `hooks.<event>[].hookCallbackIds` | string[] | yes | Callback IDs assigned by SDK |
| `hooks.<event>[].timeout` | integer | no | Timeout in seconds (default 60) |

### 6.4 Hook System

Hooks allow the SDK to observe and modify the agent's behavior at key points.

#### 6.4.1 Hook Events

| Event | Description |
|-------|-------------|
| `PreToolUse` | Before a tool call is executed |
| `PostToolUse` | After a tool call completes |
| `UserPromptSubmit` | User prompt submitted to Claude |
| `Stop` | Main agent is stopping |
| `SubagentStop` | Subagent is stopping |
| `PreCompact` | Before compaction (auto or manual) |

#### 6.4.2 Hook Matcher

The matcher is a CLI string pattern that filters tool events. Examples:

- `"Bash"` (single tool)
- `"Write|Edit|MultiEdit"` (multiple tools)
- `null` (match all tools for tool-related events)

#### 6.4.3 Hook Callback Flow

1. CLI emits `control_request` with subtype `hook_callback`.
2. SDK invokes the corresponding callback function.
3. SDK responds with `control_response` containing the hook output.

**Hook Callback Request:**
```json
{
  "type": "control_request",
  "request_id": "req_10_hook",
  "request": {
    "subtype": "hook_callback",
    "callback_id": "hook_0",
    "input": {
      "hook_event_name": "PreToolUse",
      "session_id": "550e8400-e29b-41d4-a716-446655440001",
      "transcript_path": "/home/user/.claude/transcripts/abc.jsonl",
      "cwd": "/repo",
      "tool_name": "Write",
      "tool_input": {"file_path": "README.md", "content": "Hi"}
    },
    "tool_use_id": "toolu_01ABC"
  }
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `callback_id` | string | yes | ID from initialize hook registration |
| `input` | object | yes | Hook input payload (event-specific) |
| `tool_use_id` | string | no | Tool use ID (if applicable) |

**Hook Callback Response:**
```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_10_hook",
    "response": {
      "continue": true,
      "hookSpecificOutput": {
        "hookEventName": "PreToolUse",
        "permissionDecision": "allow"
      }
    }
  }
}
```

#### 6.4.4 Hook Input Types

Common fields (present in most hook inputs):

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `session_id` | string | yes | Session UUID |
| `transcript_path` | string | yes | Path to transcript file |
| `cwd` | string | yes | Working directory |
| `permission_mode` | string | no | Current permission mode |

Event-specific fields:

| Event | Fields |
|-------|--------|
| `PreToolUse` | `hook_event_name`, `tool_name`, `tool_input` |
| `PostToolUse` | `hook_event_name`, `tool_name`, `tool_input`, `tool_response` |
| `UserPromptSubmit` | `hook_event_name`, `prompt` |
| `Stop` | `hook_event_name`, `stop_hook_active` |
| `SubagentStop` | `hook_event_name`, `stop_hook_active` |
| `PreCompact` | `hook_event_name`, `trigger`, `custom_instructions` |

`PreCompact.trigger` is `"manual"` or `"auto"`. `custom_instructions` may be null.

#### 6.4.5 Hook Output Format

Hook outputs control flow and can supply additional context.

**Common control fields:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `continue` | boolean | no | Whether Claude should proceed (default true) |
| `stopReason` | string | no | Message shown if `continue` is false |
| `suppressOutput` | boolean | no | Hide stdout from transcript mode |

**Decision fields:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `decision` | string | no | `"block"` to block execution |
| `systemMessage` | string | no | Message to user |
| `reason` | string | no | Feedback for Claude |

**Hook-specific output (`hookSpecificOutput`):**

| Event | Fields |
|-------|--------|
| `PreToolUse` | `hookEventName`, `permissionDecision`, `permissionDecisionReason`, `updatedInput` |
| `PostToolUse` | `hookEventName`, `additionalContext` |
| `UserPromptSubmit` | `hookEventName`, `additionalContext` |

**Async output (deferred execution):**
```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_10_hook",
    "response": {
      "async": true,
      "asyncTimeout": 5000
    }
  }
}
```

`additionalContext` is appended to Claude's context for the current turn.
`permissionDecision` values: `allow`, `deny`, or `ask`.
`updatedInput` replaces the tool input when `PreToolUse` is allowed.

### 6.5 Permission Callbacks

When permission checks are enabled, the CLI asks the SDK whether a tool may execute
via `can_use_tool`.

**Permission Request:**
```json
{
  "type": "control_request",
  "request_id": "req_20_perm",
  "request": {
    "subtype": "can_use_tool",
    "tool_name": "Write",
    "input": {"file_path": "/etc/hosts", "content": "127.0.0.1 localhost"},
    "permission_suggestions": ["deny"],
    "blocked_path": "/etc"
  }
}
```

**Permission Response (allow):**
```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_20_perm",
    "response": {
      "behavior": "allow",
      "updatedInput": {"file_path": "/tmp/hosts", "content": "127.0.0.1 localhost"},
      "updatedPermissions": []
    }
  }
}
```

**Permission Response (deny):**
```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_20_perm",
    "response": {
      "behavior": "deny",
      "message": "Policy disallows writing to /etc",
      "interrupt": false
    }
  }
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tool_name` | string | yes | Tool being invoked |
| `input` | object | yes | Tool input |
| `permission_suggestions` | string[] | no | CLI suggestions (e.g., `["deny"]`) |
| `blocked_path` | string | no | Path that triggered the check |
| `behavior` | string | yes | `allow`, `deny`, or `ask` |
| `updatedInput` | object | no | Modified tool input |
| `updatedPermissions` | object[] | no | Updated permission rules |
| `message` | string | no | Human-readable denial reason |
| `interrupt` | boolean | no | If true, abort current execution |

### 6.6 SDK-Hosted MCP Servers

The SDK may host in-process MCP servers by registering them as `type: "sdk"` in
the MCP configuration. The CLI routes JSON-RPC messages to the SDK via the
`mcp_message` control request subtype.

**MCP Message Request:**
```json
{
  "type": "control_request",
  "request_id": "req_30_mcp",
  "request": {
    "subtype": "mcp_message",
    "server_name": "local-tools",
    "message": {
      "jsonrpc": "2.0",
      "id": 1,
      "method": "tools/list",
      "params": {}
    }
  }
}
```

**MCP Message Response:**
```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_30_mcp",
    "response": {
      "mcp_response": {
        "jsonrpc": "2.0",
        "id": 1,
        "result": {"tools": []}
      }
    }
  }
}
```

### 6.7 Other Control Operations

| Subtype | Direction | Request Fields | Notes |
|---------|-----------|----------------|-------|
| `interrupt` | SDK → CLI | (none) | Cancel current execution |
| `set_permission_mode` | SDK → CLI | `mode` | See [Permission System](#10-permission-system) |
| `set_model` | SDK → CLI | `model` | `null` resets to default model |
| `rewind_files` | SDK → CLI | `user_message_id` | Requires file checkpointing |

**Interrupt Request:**
```json
{
  "type": "control_request",
  "request_id": "req_40_interrupt",
  "request": {"subtype": "interrupt"}
}
```

**Set Permission Mode Request:**
```json
{
  "type": "control_request",
  "request_id": "req_41_perm",
  "request": {"subtype": "set_permission_mode", "mode": "acceptEdits"}
}
```

**Set Model Request:**
```json
{
  "type": "control_request",
  "request_id": "req_42_model",
  "request": {"subtype": "set_model", "model": "claude-sonnet-4-5-20250929"}
}
```

**Rewind Files Request:**
```json
{
  "type": "control_request",
  "request_id": "req_43_rewind",
  "request": {"subtype": "rewind_files", "user_message_id": "550e8400-e29b-41d4-a716-446655440010"}
}
```

### 6.8 Error Handling

Control requests respond with `subtype: "error"` on failure:

```json
{
  "type": "control_response",
  "response": {
    "subtype": "error",
    "request_id": "req_10_hook",
    "error": "Hook callback timed out"
  }
}
```

- **Timeouts:** Default 60s per request; SDKs should allow configuration.
- **Hook errors:** Recommended fail-open behavior (treat as `continue: true`).
- **Permission errors:** If the callback fails, default to deny or ask depending on policy.

### 6.9 Implementation Considerations for Gleam

- Use an Erlang port with `{packet, line}` or manual line framing to preserve NDJSON boundaries.
- The port owner process must read stdout and write stdin concurrently; avoid blocking reads.
- Maintain a `request_id -> waiter` map to correlate responses with requests.
- Use selective receive patterns for concurrent control requests and streaming messages.
- Convert field names directly to CLI expectations (`continue`, `async`, `hookEventName`).
- Treat hook callbacks as asynchronous tasks; never block the main reader loop.

## Other Added Inserts Outside the New Section

### Metadata

**Last Updated:** 2026-01-12

### CLI Interface

Added optional flag row:

| `--input-format stream-json` | string | Enable bidirectional streaming on stdin |

Added new subsection:

### 3.5 Bidirectional Input Mode

To enable SDK → CLI streaming (hooks, permissions, SDK MCP servers), the CLI must be launched
with `--input-format stream-json`. In this mode, the SDK writes NDJSON control messages to stdin
and reads NDJSON messages from stdout. Regular prompt strings are not passed via `-- -- "prompt"`;
instead the SDK streams input messages and control requests.

**Example (bidirectional mode):**
```bash
claude --output-format stream-json --input-format stream-json --verbose
```

### Message Protocol

Extended message type union:

```
MessageType = "system" | "assistant" | "user" | "result" | "stream_event" | "control_request" | "control_response"
```

`control_request` / `control_response` are only present when using bidirectional streaming
(see [Bidirectional Control Protocol](#6-bidirectional-control-protocol)).

### MCP Integration

Added MCP server type row:

| `sdk` | In-process (control protocol) | SDK-hosted MCP servers (see [Bidirectional Control Protocol](#6-bidirectional-control-protocol)) |

### Permission System

Added cross-reference:

For programmatic control, see [Permission Callbacks](#65-permission-callbacks).

### Configuration Options

Added SDK-only options:

| `can_use_tool` | function | null | (SDK only) | Permission callback (bidirectional) |
| `hooks` | object | null | (SDK only) | Hook matchers and callbacks |
| `enable_file_checkpointing` | boolean | false | (SDK only) | Track file checkpoints for `rewind_files` |
