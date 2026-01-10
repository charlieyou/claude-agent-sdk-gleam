# Claude Agent SDK Technical Specification

**Version:** 1.0.0
**Protocol Version:** 2024-11-05
**Last Updated:** 2026-01-10

## Table of Contents

1. [Overview](#1-overview)
2. [Architecture](#2-architecture)
3. [CLI Interface](#3-cli-interface)
4. [Message Protocol](#4-message-protocol)
5. [Streaming Modes](#5-streaming-modes)
6. [Tool System](#6-tool-system)
7. [MCP Integration](#7-mcp-integration)
8. [Session Management](#8-session-management)
9. [Permission System](#9-permission-system)
10. [Configuration Options](#10-configuration-options)
11. [Error Handling](#11-error-handling)
12. [Implementation Guide](#12-implementation-guide)

---

## 1. Overview

The Claude Agent SDK provides programmatic access to Claude Code's agentic capabilities. It enables applications to:

- Execute natural language prompts with Claude
- Stream responses token-by-token or message-by-message
- Use built-in tools (file operations, shell commands, web access)
- Define custom tools via Model Context Protocol (MCP)
- Manage multi-turn conversations with session persistence
- Control permissions and sandboxing

### 1.1 Core Concepts

| Concept | Description |
|---------|-------------|
| **Query** | A single prompt execution that may span multiple turns |
| **Turn** | One request-response cycle, possibly including tool use |
| **Message** | A discrete unit of communication (system, assistant, user, result) |
| **Tool** | A capability Claude can invoke (Read, Write, Bash, custom MCP tools) |
| **Session** | A persistent conversation context identified by UUID |

### 1.2 Interaction Model

```
┌─────────────────┐                      ┌─────────────────┐
│  Your Application│                      │   Claude Code    │
│     (SDK)       │                      │      CLI         │
└────────┬────────┘                      └────────┬─────────┘
         │                                        │
         │  spawn process with args               │
         │───────────────────────────────────────>│
         │                                        │
         │  stdout: JSON messages (streaming)     │
         │<───────────────────────────────────────│
         │                                        │
         │  [process exits]                       │
         │<───────────────────────────────────────│
         │                                        │
```

---

## 2. Architecture

### 2.1 Component Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                        Your Application                          │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐  │
│  │   Query     │  │   Options   │  │    Message Parser       │  │
│  │   Builder   │  │   Config    │  │                         │  │
│  └──────┬──────┘  └──────┬──────┘  └────────────┬────────────┘  │
│         │                │                      │               │
│         └────────────────┼──────────────────────┘               │
│                          │                                      │
│                          ▼                                      │
│                 ┌─────────────────┐                             │
│                 │  Process Spawn  │                             │
│                 │   (stdio)       │                             │
│                 └────────┬────────┘                             │
└──────────────────────────┼──────────────────────────────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────────┐
│                     Claude Code CLI                               │
├──────────────────────────────────────────────────────────────────┤
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────────────┐  │
│  │  Claude  │  │  Tools   │  │  MCP     │  │  Session         │  │
│  │  API     │  │  Engine  │  │  Client  │  │  Manager         │  │
│  └──────────┘  └──────────┘  └──────────┘  └──────────────────┘  │
└──────────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────────┐
│                     MCP Servers (optional)                        │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐   │
│  │  stdio server   │  │  HTTP server    │  │  SSE server     │   │
│  │  (your tools)   │  │                 │  │                 │   │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘   │
└──────────────────────────────────────────────────────────────────┘
```

### 2.2 Data Flow

1. **Application** constructs query with prompt and options
2. **SDK** spawns Claude Code CLI as subprocess
3. **CLI** streams JSON messages to stdout
4. **SDK** parses JSON lines and yields typed messages
5. **Application** processes messages (display, logging, etc.)
6. **CLI** exits when query completes or errors

---

## 3. CLI Interface

The SDK communicates with Claude Code via its command-line interface.

### 3.1 Base Command

```bash
claude --print --output-format stream-json --verbose [OPTIONS] -- "prompt"
```

### 3.2 Required Flags

| Flag | Description |
|------|-------------|
| `--print` / `-p` | Non-interactive mode, outputs to stdout |
| `--output-format stream-json` | Emit newline-delimited JSON messages |
| `--verbose` | Required for `stream-json` format |
| `--` | Separator between options and prompt |

### 3.3 Optional Flags

| Flag | Type | Description |
|------|------|-------------|
| `--model <model>` | string | Model alias (`opus`, `sonnet`, `haiku`) or full name |
| `--max-turns <n>` | integer | Maximum conversation turns |
| `--max-budget-usd <n>` | float | Maximum spend in USD |
| `--system-prompt <text>` | string | Custom system prompt (replaces default) |
| `--append-system-prompt <text>` | string | Append to default system prompt |
| `--allowed-tools <tools>` | string | Comma-separated whitelist |
| `--disallowed-tools <tools>` | string | Comma-separated blacklist |
| `--mcp-config <path>` | string | Path to MCP server config JSON |
| `--include-partial-messages` | boolean | Enable token-level streaming |
| `--dangerously-skip-permissions` | boolean | Bypass permission checks |
| `--resume <session-id>` | string | Resume existing session |
| `--continue` | boolean | Continue most recent session |

### 3.4 Output Formats

| Format | Flag | Description |
|--------|------|-------------|
| Text | `--output-format text` | Human-readable output (default) |
| JSON | `--output-format json` | Single JSON object on completion |
| Stream JSON | `--output-format stream-json` | Newline-delimited JSON messages |

---

## 4. Message Protocol

All messages are JSON objects with a `type` field. Messages are emitted as newline-delimited JSON (NDJSON).

### 4.1 Message Types

```
MessageType = "system" | "assistant" | "user" | "result" | "stream_event"
```

### 4.2 System Message

Emitted once at session start.

```json
{
  "type": "system",
  "subtype": "init",
  "uuid": "550e8400-e29b-41d4-a716-446655440000",
  "session_id": "550e8400-e29b-41d4-a716-446655440001",
  "cwd": "/path/to/working/directory",
  "model": "claude-sonnet-4-5-20250929",
  "tools": ["Task", "Bash", "Glob", "Grep", "Read", "Edit", "Write", ...],
  "mcp_servers": [
    {"name": "ruby-tools", "status": "connected"}
  ],
  "permissionMode": "default",
  "apiKeySource": "none",
  "slash_commands": ["compact", "context", "cost", ...],
  "agents": ["Bash", "general-purpose", "Explore", "Plan", ...],
  "claude_code_version": "2.1.3"
}
```

#### System Message Fields

| Field | Type | Description |
|-------|------|-------------|
| `subtype` | `"init"` | Always "init" for session start |
| `uuid` | string | Unique message identifier |
| `session_id` | string | Session identifier (UUID) |
| `cwd` | string | Working directory |
| `model` | string | Active model name |
| `tools` | string[] | Available built-in tools |
| `mcp_servers` | object[] | Connected MCP servers with status |
| `permissionMode` | string | Current permission mode |
| `apiKeySource` | string | Authentication source (see below) |
| `slash_commands` | string[] | Available slash commands |
| `agents` | string[] | Available subagent types |
| `claude_code_version` | string | CLI version |

#### API Key Source Values

| Value | Description |
|-------|-------------|
| `ANTHROPIC_API_KEY` | Using API key from environment variable |
| `none` | Using OAuth authentication (user login via `claude login`) |
| `config` | Using API key from config file |

**Note:** When `apiKeySource` is `"none"`, Claude Code authenticates via OAuth tokens from a prior `claude login` session. This enables usage without exposing API keys.

### 4.3 Assistant Message

Claude's response, may contain text and/or tool use.

```json
{
  "type": "assistant",
  "uuid": "550e8400-e29b-41d4-a716-446655440002",
  "session_id": "550e8400-e29b-41d4-a716-446655440001",
  "parent_tool_use_id": null,
  "message": {
    "model": "claude-sonnet-4-5-20250929",
    "id": "msg_01ABC123",
    "type": "message",
    "role": "assistant",
    "content": [
      {
        "type": "text",
        "text": "I'll help you with that."
      },
      {
        "type": "tool_use",
        "id": "toolu_01XYZ789",
        "name": "Glob",
        "input": {
          "pattern": "**/*.rb"
        }
      }
    ],
    "stop_reason": null,
    "usage": {
      "input_tokens": 150,
      "output_tokens": 42,
      "cache_creation_input_tokens": 500,
      "cache_read_input_tokens": 12000
    }
  }
}
```

#### Content Block Types

**Text Block:**
```json
{
  "type": "text",
  "text": "The response text..."
}
```

**Tool Use Block:**
```json
{
  "type": "tool_use",
  "id": "toolu_01XYZ789",
  "name": "ToolName",
  "input": { ... }
}
```

### 4.4 User Message

Tool results returned to Claude.

```json
{
  "type": "user",
  "uuid": "550e8400-e29b-41d4-a716-446655440003",
  "session_id": "550e8400-e29b-41d4-a716-446655440001",
  "parent_tool_use_id": null,
  "message": {
    "role": "user",
    "content": [
      {
        "type": "tool_result",
        "tool_use_id": "toolu_01XYZ789",
        "content": "/path/to/file1.rb\n/path/to/file2.rb"
      }
    ]
  },
  "tool_use_result": {
    "filenames": ["/path/to/file1.rb", "/path/to/file2.rb"],
    "durationMs": 45,
    "numFiles": 2,
    "truncated": false
  }
}
```

### 4.5 Result Message

Final message indicating query completion.

```json
{
  "type": "result",
  "subtype": "success",
  "uuid": "550e8400-e29b-41d4-a716-446655440004",
  "session_id": "550e8400-e29b-41d4-a716-446655440001",
  "is_error": false,
  "duration_ms": 5432,
  "duration_api_ms": 12000,
  "num_turns": 2,
  "result": "I found 3 Ruby files in the directory...",
  "total_cost_usd": 0.0234,
  "usage": {
    "input_tokens": 200,
    "output_tokens": 150,
    "cache_creation_input_tokens": 500,
    "cache_read_input_tokens": 15000,
    "server_tool_use": {
      "web_search_requests": 0,
      "web_fetch_requests": 0
    }
  },
  "modelUsage": {
    "claude-sonnet-4-5-20250929": {
      "inputTokens": 200,
      "outputTokens": 150,
      "cacheReadInputTokens": 15000,
      "cacheCreationInputTokens": 500,
      "webSearchRequests": 0,
      "costUSD": 0.0234,
      "contextWindow": 200000,
      "maxOutputTokens": 64000
    }
  },
  "permission_denials": []
}
```

#### Result Subtypes

| Subtype | Description |
|---------|-------------|
| `success` | Query completed successfully |
| `error_max_turns` | Exceeded maximum turns |
| `error_during_execution` | Runtime error occurred |
| `error_max_budget_usd` | Exceeded budget limit |

#### Result Fields

| Field | Type | Description |
|-------|------|-------------|
| `is_error` | boolean | Whether the query ended in error |
| `duration_ms` | integer | Total wall-clock time |
| `duration_api_ms` | integer | Time spent in API calls |
| `num_turns` | integer | Number of conversation turns |
| `result` | string | Final text result |
| `total_cost_usd` | float | Total cost in USD |
| `usage` | object | Aggregate token usage |
| `modelUsage` | object | Per-model usage breakdown |
| `permission_denials` | array | Tools that were denied permission |
| `structured_output` | any | Parsed JSON if using structured output |
| `errors` | string[] | Error messages (if `is_error: true`) |

### 4.6 Stream Event Message

Token-level streaming (requires `--include-partial-messages`).

```json
{
  "type": "stream_event",
  "uuid": "550e8400-e29b-41d4-a716-446655440005",
  "session_id": "550e8400-e29b-41d4-a716-446655440001",
  "parent_tool_use_id": null,
  "event": {
    "type": "content_block_delta",
    "index": 0,
    "delta": {
      "type": "text_delta",
      "text": "Hello"
    }
  }
}
```

#### Stream Event Types

| Event Type | Description |
|------------|-------------|
| `message_start` | Beginning of assistant message |
| `content_block_start` | Beginning of content block |
| `content_block_delta` | Incremental content update |
| `content_block_stop` | End of content block |
| `message_delta` | Message metadata update |
| `message_stop` | End of assistant message |

---

## 5. Streaming Modes

### 5.1 Message-Level Streaming (Default)

Without `--include-partial-messages`, messages are delivered complete:

```
system → assistant → user → assistant → user → assistant → result
```

**Message Sequence Example:**
```
{"type":"system","subtype":"init",...}
{"type":"assistant","message":{"content":[{"type":"text","text":"Full response here"}]},...}
{"type":"result","subtype":"success",...}
```

### 5.2 Token-Level Streaming

With `--include-partial-messages`, tokens stream in real-time:

```
system → stream_event* → assistant → stream_event* → assistant → result
```

**Token Sequence Example:**
```
{"type":"system","subtype":"init",...}
{"type":"stream_event","event":{"type":"message_start",...}}
{"type":"stream_event","event":{"type":"content_block_start",...}}
{"type":"stream_event","event":{"type":"content_block_delta","delta":{"text":"Hello"}}}
{"type":"stream_event","event":{"type":"content_block_delta","delta":{"text":" world"}}}
{"type":"stream_event","event":{"type":"content_block_stop",...}}
{"type":"assistant","message":{"content":[{"type":"text","text":"Hello world"}]},...}
{"type":"result","subtype":"success",...}
```

### 5.3 Extracting Streaming Text

Pseudocode for real-time text display:

```
for each message in stream:
    if message.type == "stream_event":
        event = message.event
        if event.type == "content_block_delta":
            if event.delta.type == "text_delta":
                print(event.delta.text)  // No newline, append to output
```

---

## 6. Tool System

### 6.1 Built-in Tools

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| `Read` | Read file contents | `file_path`, `offset`, `limit` |
| `Write` | Create/overwrite file | `file_path`, `content` |
| `Edit` | Find-and-replace in file | `file_path`, `old_string`, `new_string` |
| `Glob` | Pattern-match files | `pattern`, `path` |
| `Grep` | Search file contents | `pattern`, `path`, `output_mode` |
| `Bash` | Execute shell command | `command`, `timeout` |
| `WebFetch` | Fetch URL content | `url`, `prompt` |
| `WebSearch` | Web search | `query` |
| `Task` | Spawn subagent | `prompt`, `subagent_type` |
| `TodoWrite` | Manage task list | `todos` |

### 6.2 Tool Input Schemas

**Read Tool:**
```json
{
  "file_path": "/absolute/path/to/file",
  "offset": 0,
  "limit": 2000
}
```

**Edit Tool:**
```json
{
  "file_path": "/absolute/path/to/file",
  "old_string": "text to find",
  "new_string": "replacement text",
  "replace_all": false
}
```

**Bash Tool:**
```json
{
  "command": "ls -la",
  "timeout": 120000,
  "description": "List directory contents",
  "run_in_background": false
}
```

**Glob Tool:**
```json
{
  "pattern": "**/*.rb",
  "path": "/search/directory"
}
```

**Grep Tool:**
```json
{
  "pattern": "function\\s+\\w+",
  "path": "/search/path",
  "glob": "*.js",
  "output_mode": "content",
  "-i": true,
  "-n": true,
  "-C": 3,
  "head_limit": 100
}
```

### 6.3 Tool Filtering

**Whitelist (only these tools available):**
```bash
claude -p --allowed-tools "Read,Glob,Grep" -- "Find files"
```

**Blacklist (these tools disabled):**
```bash
claude -p --disallowed-tools "Bash,Write,Edit" -- "Analyze code"
```

---

## 7. MCP Integration

Model Context Protocol (MCP) enables custom tool integration.

### 7.1 MCP Server Types

| Type | Transport | Use Case |
|------|-----------|----------|
| `stdio` | stdin/stdout | Local process, custom tools |
| `sse` | Server-Sent Events | Remote streaming server |
| `http` | HTTP POST | Remote request-response server |

### 7.2 MCP Configuration File

```json
{
  "mcpServers": {
    "my-tools": {
      "command": "python",
      "args": ["mcp_server.py"],
      "env": {
        "API_KEY": "secret"
      }
    },
    "remote-tools": {
      "type": "sse",
      "url": "https://mcp.example.com/sse",
      "headers": {
        "Authorization": "Bearer token"
      }
    }
  }
}
```

**Usage:**
```bash
claude -p --mcp-config /path/to/config.json -- "Use my custom tools"
```

### 7.3 MCP Tool Naming

MCP tools are namespaced: `mcp__<server>__<tool>`

Example: `mcp__ruby-tools__calculator`

### 7.4 MCP Server Protocol (stdio)

MCP servers communicate via JSON-RPC 2.0 over stdin/stdout.

#### Initialize Handshake

**Client Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "clientInfo": {
      "name": "claude-code",
      "version": "2.1.3"
    }
  }
}
```

**Server Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2024-11-05",
    "serverInfo": {
      "name": "my-tools",
      "version": "1.0.0"
    },
    "capabilities": {
      "tools": {"listChanged": false}
    }
  }
}
```

#### List Tools

**Request:**
```json
{"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "tools": [
      {
        "name": "calculator",
        "description": "Performs arithmetic operations",
        "inputSchema": {
          "type": "object",
          "properties": {
            "operation": {"type": "string", "enum": ["add", "subtract", "multiply", "divide"]},
            "a": {"type": "number"},
            "b": {"type": "number"}
          },
          "required": ["operation", "a", "b"]
        }
      }
    ]
  }
}
```

#### Call Tool

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "calculator",
    "arguments": {"operation": "multiply", "a": 7, "b": 6}
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [
      {"type": "text", "text": "7 multiply 6 = 42"}
    ]
  }
}
```

### 7.5 Implementing an MCP Server

Minimal stdio MCP server pseudocode:

```
function main():
    while line = read_stdin():
        request = json_parse(line)
        response = handle_request(request)
        if response:
            write_stdout(json_stringify(response))

function handle_request(request):
    match request.method:
        case "initialize":
            return {
                jsonrpc: "2.0",
                id: request.id,
                result: {
                    protocolVersion: "2024-11-05",
                    serverInfo: {name: "my-server", version: "1.0"},
                    capabilities: {tools: {listChanged: false}}
                }
            }
        case "initialized":
            return null  // Notification, no response
        case "tools/list":
            return {
                jsonrpc: "2.0",
                id: request.id,
                result: {tools: get_tool_definitions()}
            }
        case "tools/call":
            result = call_tool(request.params.name, request.params.arguments)
            return {
                jsonrpc: "2.0",
                id: request.id,
                result: {content: [{type: "text", text: result}]}
            }
```

---

## 8. Session Management

### 8.1 Session Lifecycle

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Start     │────>│   Active    │────>│   Ended     │
│  (new UUID) │     │  (queries)  │     │  (stored)   │
└─────────────┘     └──────┬──────┘     └──────┬──────┘
                           │                   │
                           │    resume/        │
                           │    continue       │
                           │<──────────────────┘
```

### 8.2 Starting a New Session

```bash
claude -p --output-format stream-json --verbose -- "Hello"
```

The `system` message contains the new `session_id`.

### 8.3 Resuming a Session

**By Session ID:**
```bash
claude -p --resume "550e8400-e29b-41d4-a716-446655440001" -- "Continue our work"
```

**Most Recent Session:**
```bash
claude -p --continue -- "Continue where we left off"
```

### 8.4 Session Storage

Sessions are stored locally at:
- **macOS/Linux:** `~/.claude/sessions/`
- **Windows:** `%USERPROFILE%\.claude\sessions\`

---

## 9. Permission System

### 9.1 Permission Modes

| Mode | Description |
|------|-------------|
| `default` | Prompt for sensitive operations |
| `acceptEdits` | Auto-approve file edits |
| `bypassPermissions` | Skip all permission checks |
| `plan` | Planning mode, no execution |

### 9.2 Using Permission Modes

```bash
# Bypass all permissions (use in sandboxed environments only)
claude -p --dangerously-skip-permissions -- "Do something"

# Accept edits automatically
claude -p --permission-mode acceptEdits -- "Refactor code"
```

### 9.3 Permission Denials

When a tool is denied, it appears in the result:

```json
{
  "type": "result",
  "permission_denials": [
    {
      "tool_name": "Bash",
      "tool_use_id": "toolu_01ABC",
      "tool_input": {"command": "rm -rf /"}
    }
  ]
}
```

---

## 10. Configuration Options

### 10.1 Complete Options Reference

| Option | Type | Default | CLI Flag | Description |
|--------|------|---------|----------|-------------|
| `model` | string | auto | `--model` | Model to use |
| `max_turns` | integer | unlimited | `--max-turns` | Maximum conversation turns |
| `max_budget_usd` | float | unlimited | `--max-budget-usd` | Maximum cost |
| `system_prompt` | string | built-in | `--system-prompt` | Custom system prompt |
| `allowed_tools` | string[] | all | `--allowed-tools` | Tool whitelist |
| `disallowed_tools` | string[] | none | `--disallowed-tools` | Tool blacklist |
| `permission_mode` | string | "default" | `--permission-mode` | Permission mode |
| `mcp_servers` | object | {} | `--mcp-config` | MCP server configs |
| `include_partial_messages` | boolean | false | `--include-partial-messages` | Token streaming |
| `resume` | string | null | `--resume` | Session ID to resume |
| `continue_session` | boolean | false | `--continue` | Continue most recent |
| `cwd` | string | process cwd | (process cwd) | Working directory |

### 10.2 Model Selection

| Alias | Full Name | Characteristics |
|-------|-----------|-----------------|
| `opus` | claude-opus-4-5-20251101 | Most capable, highest cost |
| `sonnet` | claude-sonnet-4-5-20250929 | Balanced performance/cost |
| `haiku` | claude-haiku-4-5-20251001 | Fastest, lowest cost |

### 10.3 System Prompt Options

**Replace default:**
```bash
claude -p --system-prompt "You are a helpful assistant" -- "Hello"
```

**Append to default:**
```bash
claude -p --append-system-prompt "Focus on Ruby code" -- "Hello"
```

---

## 11. Error Handling

### 11.1 Error Result Types

| Subtype | Cause | Recovery |
|---------|-------|----------|
| `error_max_turns` | Turn limit reached | Increase `max_turns` |
| `error_max_budget_usd` | Budget exhausted | Increase budget |
| `error_during_execution` | Runtime failure | Check `errors` field |

### 11.2 Error Response Example

```json
{
  "type": "result",
  "subtype": "error_during_execution",
  "is_error": true,
  "errors": [
    "Failed to connect to MCP server: connection refused"
  ],
  "total_cost_usd": 0.001,
  "num_turns": 0
}
```

### 11.3 JSON-RPC Errors (MCP)

| Code | Message | Meaning |
|------|---------|---------|
| -32700 | Parse error | Invalid JSON |
| -32600 | Invalid Request | Malformed request |
| -32601 | Method not found | Unknown method |
| -32602 | Invalid params | Bad parameters |
| -32603 | Internal error | Server error |

---

## 12. Implementation Guide

### 12.1 Minimal Client (Pseudocode)

```
function query(prompt, options):
    cmd = build_command(prompt, options)
    process = spawn(cmd)

    for line in process.stdout:
        if line is empty:
            continue

        message = json_parse(line)
        yield message

        if message.type == "result":
            break

function build_command(prompt, options):
    cmd = ["claude", "--print", "--output-format", "stream-json", "--verbose"]

    if options.model:
        cmd.append("--model", options.model)

    if options.max_turns:
        cmd.append("--max-turns", string(options.max_turns))

    if options.system_prompt:
        cmd.append("--system-prompt", options.system_prompt)

    if options.include_partial_messages:
        cmd.append("--include-partial-messages")

    if options.permission_mode == "bypassPermissions":
        cmd.append("--dangerously-skip-permissions")

    if options.mcp_config_path:
        cmd.append("--mcp-config", options.mcp_config_path)

    cmd.append("--")
    cmd.append(prompt)

    return cmd
```

### 12.2 Message Handler Pattern

```
function handle_messages(stream):
    session_id = null

    for message in stream:
        match message.type:
            case "system":
                session_id = message.session_id
                log("Session started:", session_id)
                log("Model:", message.model)
                log("Tools:", message.tools)

            case "assistant":
                for block in message.message.content:
                    if block.type == "text":
                        print(block.text)
                    elif block.type == "tool_use":
                        log("[Tool:", block.name, "]")

            case "user":
                for block in message.message.content:
                    if block.type == "tool_result":
                        log("[Result received]")

            case "stream_event":
                event = message.event
                if event.type == "content_block_delta":
                    if event.delta.type == "text_delta":
                        print_no_newline(event.delta.text)

            case "result":
                print("")
                log("Completed:", message.subtype)
                log("Turns:", message.num_turns)
                log("Cost: $", message.total_cost_usd)
                return message
```

### 12.3 MCP Server Template

```
class MCPServer:
    tools = {}

    function register_tool(name, description, schema, handler):
        tools[name] = {
            name: name,
            description: description,
            schema: schema,
            handler: handler
        }

    function run():
        while line = stdin.readline():
            request = json_parse(line)
            response = dispatch(request)
            if response:
                stdout.writeline(json_stringify(response))

    function dispatch(request):
        match request.method:
            case "initialize":
                return init_response(request.id)
            case "initialized":
                return null
            case "tools/list":
                return list_tools_response(request.id)
            case "tools/call":
                return call_tool_response(request.id, request.params)
            default:
                return error_response(request.id, -32601, "Method not found")

    function init_response(id):
        return {
            jsonrpc: "2.0",
            id: id,
            result: {
                protocolVersion: "2024-11-05",
                serverInfo: {name: "server", version: "1.0"},
                capabilities: {tools: {listChanged: false}}
            }
        }

    function list_tools_response(id):
        tool_list = []
        for name, tool in tools:
            tool_list.append({
                name: tool.name,
                description: tool.description,
                inputSchema: tool.schema
            })
        return {jsonrpc: "2.0", id: id, result: {tools: tool_list}}

    function call_tool_response(id, params):
        tool = tools[params.name]
        if not tool:
            return {
                jsonrpc: "2.0",
                id: id,
                result: {
                    content: [{type: "text", text: "Unknown tool"}],
                    isError: true
                }
            }

        try:
            result = tool.handler(params.arguments)
            return {
                jsonrpc: "2.0",
                id: id,
                result: {
                    content: [{type: "text", text: string(result)}]
                }
            }
        catch error:
            return {
                jsonrpc: "2.0",
                id: id,
                result: {
                    content: [{type: "text", text: error.message}],
                    isError: true
                }
            }
```

### 12.4 Token Cost Calculation

```
function calculate_cost(usage, model):
    // Prices per million tokens (example rates)
    rates = {
        "claude-opus-4-5": {input: 15.00, output: 75.00},
        "claude-sonnet-4-5": {input: 3.00, output: 15.00},
        "claude-haiku-4-5": {input: 0.80, output: 4.00}
    }

    rate = rates[model]
    input_cost = (usage.input_tokens / 1_000_000) * rate.input
    output_cost = (usage.output_tokens / 1_000_000) * rate.output

    // Cache reads are cheaper (typically 10% of input cost)
    cache_read_cost = (usage.cache_read_input_tokens / 1_000_000) * (rate.input * 0.1)

    // Cache creation costs same as input
    cache_create_cost = (usage.cache_creation_input_tokens / 1_000_000) * rate.input

    return input_cost + output_cost + cache_read_cost + cache_create_cost
```

---

## Appendix A: Real-World JSON Examples

### A.1 Actual System Message

```json
{
  "type": "system",
  "subtype": "init",
  "cwd": "/private/tmp/playing",
  "session_id": "5620625c-b4c7-4185-9b2b-8de430dd2184",
  "tools": [
    "Task", "TaskOutput", "Bash", "Glob", "Grep", "ExitPlanMode",
    "Read", "Edit", "Write", "NotebookEdit", "WebFetch", "TodoWrite",
    "WebSearch", "KillShell", "AskUserQuestion", "Skill", "EnterPlanMode"
  ],
  "mcp_servers": [
    {"name": "ruby-tools", "status": "connected"}
  ],
  "model": "claude-sonnet-4-5-20250929",
  "permissionMode": "default",
  "slash_commands": ["compact", "context", "cost", "init", "pr-comments", "release-notes", "review", "security-review"],
  "apiKeySource": "ANTHROPIC_API_KEY",
  "claude_code_version": "2.1.3",
  "output_style": "default",
  "agents": ["Bash", "general-purpose", "statusline-setup", "Explore", "Plan", "claude-code-guide"],
  "skills": [],
  "plugins": [{"name": "gopls-lsp", "path": "/Users/sam/.claude/plugins/cache/claude-plugins-official/gopls-lsp/1.0.0"}],
  "uuid": "95625b7e-3117-483b-95c9-47e54bb9ec70"
}
```

### A.2 Actual Assistant Message with Tool Use

```json
{
  "type": "assistant",
  "message": {
    "model": "claude-sonnet-4-5-20250929",
    "id": "msg_01Rf5Yc8FdberfJBxNjTNk3W",
    "type": "message",
    "role": "assistant",
    "content": [
      {"type": "text", "text": "I'll use the ruby-tools MCP server to get the current time and generate a random number."},
      {"type": "tool_use", "id": "toolu_017K5vf", "name": "mcp__ruby-tools__current_time", "input": {}},
      {"type": "tool_use", "id": "toolu_018ABC", "name": "mcp__ruby-tools__random_number", "input": {"min": 1, "max": 100}}
    ],
    "stop_reason": null,
    "stop_sequence": null,
    "usage": {
      "input_tokens": 2,
      "cache_creation_input_tokens": 4722,
      "cache_read_input_tokens": 13367,
      "cache_creation": {"ephemeral_5m_input_tokens": 4722, "ephemeral_1h_input_tokens": 0},
      "output_tokens": 28,
      "service_tier": "standard"
    },
    "context_management": null
  },
  "parent_tool_use_id": null,
  "session_id": "5620625c-b4c7-4185-9b2b-8de430dd2184",
  "uuid": "eda52225-597f-4a1f-8ca6-a6bcd94934ac"
}
```

### A.3 Actual Tool Result Message

```json
{
  "type": "user",
  "message": {
    "role": "user",
    "content": [
      {
        "tool_use_id": "toolu_01LsrzxpC42FnYPxepJfr9pg",
        "type": "tool_result",
        "content": "/private/tmp/playing/quick_start.rb\n/private/tmp/playing/advanced_examples.rb\n/private/tmp/playing/claude_agent_sdk_demo.rb"
      }
    ]
  },
  "parent_tool_use_id": null,
  "session_id": "c8775347-af93-45c7-b9bf-a6e009483fa5",
  "uuid": "4ffd3635-d0fb-4057-85e2-0f0a4c302fec",
  "tool_use_result": {
    "filenames": [
      "/private/tmp/playing/quick_start.rb",
      "/private/tmp/playing/advanced_examples.rb",
      "/private/tmp/playing/claude_agent_sdk_demo.rb"
    ],
    "durationMs": 345,
    "numFiles": 3,
    "truncated": false
  }
}
```

### A.4 Actual Result Message

```json
{
  "type": "result",
  "subtype": "success",
  "is_error": false,
  "duration_ms": 7040,
  "duration_api_ms": 12311,
  "num_turns": 2,
  "result": "I found 3 Ruby files in the directory:\n\n1. `quick_start.rb`\n2. `advanced_examples.rb`\n3. `claude_agent_sdk_demo.rb`",
  "session_id": "c8775347-af93-45c7-b9bf-a6e009483fa5",
  "total_cost_usd": 0.0186724,
  "usage": {
    "input_tokens": 7,
    "cache_creation_input_tokens": 440,
    "cache_read_input_tokens": 35858,
    "output_tokens": 114,
    "server_tool_use": {"web_search_requests": 0, "web_fetch_requests": 0},
    "service_tier": "standard",
    "cache_creation": {"ephemeral_1h_input_tokens": 0, "ephemeral_5m_input_tokens": 440}
  },
  "modelUsage": {
    "claude-haiku-4-5-20251001": {
      "inputTokens": 2,
      "outputTokens": 170,
      "cacheReadInputTokens": 10531,
      "cacheCreationInputTokens": 0,
      "webSearchRequests": 0,
      "costUSD": 0.0019051,
      "contextWindow": 200000,
      "maxOutputTokens": 64000
    },
    "claude-sonnet-4-5-20250929": {
      "inputTokens": 9,
      "outputTokens": 143,
      "cacheReadInputTokens": 39900,
      "cacheCreationInputTokens": 439,
      "webSearchRequests": 0,
      "costUSD": 0.0157882,
      "contextWindow": 200000,
      "maxOutputTokens": 64000
    }
  },
  "permission_denials": [],
  "uuid": "34cf1a3e-8b9f-481e-ba6f-c9e934b09424"
}
```

### A.5 Actual Stream Event (Token Delta)

```json
{
  "type": "stream_event",
  "event": {
    "type": "content_block_delta",
    "index": 0,
    "delta": {
      "type": "text_delta",
      "text": "Dogs are loyal"
    }
  },
  "session_id": "4a7c99c6-e08a-4e3c-b6ce-17c33ae8bb92",
  "parent_tool_use_id": null,
  "uuid": "2bc3e3c8-d9f2-48e8-bd72-b828bbbf3732"
}
```

---

## Appendix B: Complete Message Flow Example

```
# Query: "List Ruby files and count them"

# 1. System init
{"type":"system","subtype":"init","session_id":"abc-123","model":"claude-sonnet-4-5-20250929","tools":["Glob","Bash",...],...}

# 2. Claude responds with tool use
{"type":"assistant","message":{"content":[{"type":"text","text":"I'll find the Ruby files."},{"type":"tool_use","id":"toolu_1","name":"Glob","input":{"pattern":"**/*.rb"}}]},...}

# 3. Tool result
{"type":"user","message":{"content":[{"type":"tool_result","tool_use_id":"toolu_1","content":"file1.rb\nfile2.rb\nfile3.rb"}]},...}

# 4. Claude's final response
{"type":"assistant","message":{"content":[{"type":"text","text":"I found 3 Ruby files:\n1. file1.rb\n2. file2.rb\n3. file3.rb"}]},...}

# 5. Query complete
{"type":"result","subtype":"success","num_turns":2,"result":"I found 3 Ruby files...","total_cost_usd":0.0156,...}
```

---

## Appendix B: Token Streaming Flow Example

```
# With --include-partial-messages

{"type":"system","subtype":"init",...}
{"type":"stream_event","event":{"type":"message_start","message":{"model":"claude-sonnet-4-5-20250929",...}}}
{"type":"stream_event","event":{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}}
{"type":"stream_event","event":{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hello"}}}
{"type":"stream_event","event":{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":" world"}}}
{"type":"stream_event","event":{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"!"}}}
{"type":"stream_event","event":{"type":"content_block_stop","index":0}}
{"type":"stream_event","event":{"type":"message_delta","delta":{"stop_reason":"end_turn"},...}}
{"type":"stream_event","event":{"type":"message_stop"}}
{"type":"assistant","message":{"content":[{"type":"text","text":"Hello world!"}]},...}
{"type":"result","subtype":"success",...}
```

---

## Appendix C: References

- [Claude Agent SDK TypeScript](https://github.com/anthropics/claude-agent-sdk-typescript)
- [Claude Code Documentation](https://docs.anthropic.com/claude-code)
- [Model Context Protocol](https://modelcontextprotocol.io)
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
