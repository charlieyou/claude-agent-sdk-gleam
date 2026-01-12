# Implementation Plan: Bidirectional Control Protocol

## Context & Goals
- **Spec**: `plans/CLAUDE_AGENT_SDK_BIDIR_ADDITIONS.md`
- Implement bidirectional control protocol for SDK <-> CLI communication
- Enable hook system (PreToolUse, PostToolUse, UserPromptSubmit, Stop, SubagentStop, PreCompact)
- Add permission callbacks (`can_use_tool`) for programmatic tool authorization
- Support SDK-hosted MCP servers via control protocol routing
- Implement control operations (interrupt, set_permission_mode, set_model, rewind_files)
- Target users: Gleam developers building agentic applications requiring fine-grained control over Claude's behavior

## Scope & Non-Goals

### In Scope
- Bidirectional streaming via `--input-format stream-json`
- Control message types (control_request, control_response) with request_id correlation
- Initialization handshake with hook registration
- Hook callback system with all 6 event types
- Permission callback (can_use_tool) support
- SDK-hosted MCP server message routing (opaque pass-through, no validation)
- Control operations: interrupt, set_permission_mode, set_model, rewind_files
- File checkpointing support via `enable_file_checkpointing` option
- Request ID correlation for async responses
- Error handling per spec (timeouts, fail-open for hooks, fail-deny for permissions)
- Auto-enable bidirectional mode when hooks/can_use_tool/mcp_servers configured; legacy mode otherwise
- Testing with mock runner and real CLI (protocol-focused behavioral verification)

### Out of Scope (Non-Goals)
- Forced migration to bidirectional mode (legacy mode auto-selected when no bidir features used)
- Token-level streaming changes (already supported)
- MCP server implementation (SDK routes messages only)
- MCP message validation (pass through opaquely)
- Interactive permission prompts (SDK is programmatic)
- Custom transport mechanisms beyond Erlang ports
- MCP server discovery/registration beyond what CLI provides
- Automatic retry/reconnection logic for crashed sessions
- Session persistence changes
- CLI modifications (SDK-side only)
- **Async hook responses** (`async: true` with `asyncTimeout` deferred execution) — MVP uses synchronous callbacks only
- **Hook matchers** (filtering by tool name pattern) — MVP registers hooks for all tools; matcher support deferred to future version

## Assumptions & Constraints

### Implementation Constraints
- **Architecture**: Must use `gleam_otp/actor` for session management (GenServer-like pattern)
- **Concurrency Model**: Serialize writes via `actor.call`, reads via message handler (port messages)
- **Dual Runner Model** (key decision):
  - **Legacy `query()`**: Uses existing public `Runner` type in `runner.gleam` (pull-based, blocking `read()`)
  - **Bidir `start_session()`**: Uses new internal `BidirRunner` type in `internal/bidir_runner.gleam` (push-based)
  - **Public `runner.gleam` is NOT modified** — existing test mocks continue to work
- **IO Abstraction for Bidir Mode** (internal `BidirRunner`):
  - `BidirRunner.start(cli_args)` is called **inside actor init** — captures `self()` for port ownership
  - Production BidirRunner wraps an Erlang Port; Port ownership is the actor process
  - Port sends native Erlang messages: `{Port, {data, Binary}}`, `{Port, {exit_status, Code}}` (see "Port Message Protocol" section)
  - Actor receives these via `handle_info` (non-blocking)
  - `BidirRunner.write(runner, data)` is synchronous (port_command) and returns immediately
  - For tests: pass `with_bidir_runner_factory(fn() -> BidirRunner)` into options; factory is invoked inside actor init
  - Actor never calls a blocking read — all reads are push-based via message handler
- **Hook Execution**: Callbacks execute in **spawned OTP tasks** with timeout supervision. GenServer spawns a task for each hook, awaits result with timeout, and sends fail-open/fail-deny response if task exceeds timeout or crashes. This allows the GenServer to remain responsive and enforce timeouts.
- **Error Handling**:
  - Hooks: Fail-open (`continue: true`) on error/timeout
  - Permissions: Fail-deny (`deny`) on error/exception
- **CLI Compatibility**:
  - If hooks, can_use_tool, or mcp_servers are configured, bidirectional mode is required and CLI version is checked. Fail fast with clear error on CLI version incompatibility.
  - If no bidirectional features are configured, SDK uses legacy unidirectional mode for maximum compatibility with older CLIs.
- Request ID generation must be unique per session (format: `req_<counter>`)
- Existing message types remain backwards compatible (additive changes only)
- Control messages use same NDJSON framing as regular messages

### Testing Constraints
- Tests must verify the *protocol* (messages sent/received) using mock runner, not just internal state
- **Required protocol tests**:
  - All control message types round-trip correctly (encode → decode → match original)
  - Initialize handshake sends correct NDJSON and parses response/timeout correctly
  - Each hook type receives correct context and sends correct response JSON
  - Request ID correlation matches responses to pending requests
  - Timeout and exception paths return correct fail-open/fail-deny responses
- Both mock runner and real CLI integration tests required
- Real CLI tests are opt-in (run when CLI available)
- Coverage percentage is a monitoring metric, not a gate (focus on behavioral verification above)

## Integration Analysis

### Existing Mechanisms Considered

| Existing Mechanism | Could Serve Feature? | Decision | Rationale |
|--------------------|---------------------|----------|-----------|
| `src/claude_agent_sdk/internal/stream.gleam` | Partial | Extend | Already handles JSON-lines parsing and state machine; add control message routing |
| `src/claude_agent_sdk/runner.gleam` | No | **Keep unchanged** | Existing pull-based Runner supports legacy `query()`. Bidir uses new internal `BidirRunner`. |
| `src/claude_agent_sdk/options.gleam` | Yes | Extend | Builder pattern fits hook/callback registration |
| `src/claude_agent_sdk_ffi.erl` | Partial | Extend | Add port_command for writes alongside existing receive |
| `src/claude_agent_sdk/internal/decoder.gleam` | Partial | Extend | Add control message variant decoding |
| `src/claude_agent_sdk/internal/cli.gleam` | Yes | Extend | Add --input-format flag to argument builder |
| `QueryStream` (iterator-based) | No | Keep for query() | Iterator is pull-based/passive. Bidir uses GenServer + Subject instead. |

### Integration Approach

The bidirectional protocol uses a **parallel code path** rather than modifying existing infrastructure:

1. **Legacy `query()` path** (unchanged):
   - Uses existing public `Runner` (pull-based)
   - Uses existing `QueryStream` iterator
   - No `--input-format stream-json` flag
   - No control messages, no hooks

2. **Bidir `start_session()` path** (new):
   - **Port layer**: New `internal/bidir_runner.gleam` wraps Erlang Port with push-based IO
   - **Stream layer**: Extend `stream.gleam` to recognize control messages and dispatch to GenServer
   - **Session layer**: New `internal/bidir.gleam` GenServer manages state, correlates requests, invokes callbacks
   - **Options layer**: Extend `options.gleam` with hook registration builder functions
   - Uses `--input-format stream-json` flag for bidirectional control protocol

**Key principle**: The two paths share decoder/stream logic where possible but have separate IO layers (Runner vs BidirRunner) to avoid breaking existing behavior.

## Prerequisites
- [ ] Add `gleam_otp` (version TBD) to `gleam.toml` — `gleam_erlang = "== 1.3.0"` already present
- [ ] Verify `gleam_otp` package provides:
  - `gleam/otp/actor` with `start`, `call`, `send` for GenServer-like pattern
  - `gleam/otp/task` for spawning supervised tasks
  - Timer primitives (or use `gleam_erlang` timers)
- [ ] **OTP choice: Use `gleam_otp` exclusively** for actor/task/supervision. Use `gleam_erlang` only for low-level FFI (port, timer, process primitives).
- [ ] Verify minimum Claude CLI version supporting `--input-format stream-json`
- [ ] Ensure existing test suite passes before modifications
- [ ] Review Claude CLI bidirectional protocol documentation for any undocumented behaviors

## High-Level Approach

1. **Foundation**: Add OTP dependencies and implement low-level Port FFI for writing data
2. **Internal BidirRunner**: Create new `internal/bidir_runner.gleam` with push-based IO (public `runner.gleam` unchanged)
3. **Types & Decoders**: Define `ControlRequest`, `ControlResponse`, `Hook` types and their JSON encoders/decoders
4. **Request Tracking**: Implement request ID generation and response correlation
5. **Actor Implementation**: Implement the `BidirSession` GenServer to handle:
   - Port messages (stdout data) via `handle_info` using `BidirRunner`
   - Control operations (interrupt, etc.) via `handle_call`
   - Internal state (pending requests, registered hooks, pending hook tasks)
   - Message routing (User/Assistant/Tool vs Control)
   - Hook callback execution via **spawned OTP tasks** with timeout supervision
6. **CLI Argument Builder**: Add `--input-format stream-json` flag for bidir mode
7. **Initialization Handshake**: Send initialize control_request at session start
8. **Hook Dispatch**: Route incoming control_requests to user callbacks, send responses
9. **Control Operations**: Implement interrupt, set_permission_mode, set_model, rewind_files
10. **Integration**: Expose `start_session()` API; ensure `query()` remains unchanged (uses legacy `Runner`)
11. **Testing**: Create mock `BidirRunner` for bidir tests; existing `Runner` mocks unaffected

## Technical Design

### Architecture

**Two code paths**: `query()` (legacy) and `start_session()` (bidir) use separate IO layers.

```
┌─────────────────────────────────────────────────────────────────┐
│                        User Application                          │
├─────────────────────────────────────────────────────────────────┤
│  claude_agent_sdk.gleam (PUBLIC)                                 │
│  - start_session(prompt, options) -> Session  [bidir path]       │
│  - query(prompt, options) -> Iterator         [legacy path]      │
│  - interrupt(session) / set_model(session, model) / etc.         │
├─────────────────────────────────────────────────────────────────┤
│  options.gleam (PUBLIC)                                          │
│  - with_pre_tool_use(callback)                                   │
│  - with_post_tool_use(callback)                                  │
│  - with_can_use_tool(callback)                                   │
│  - with_mcp_server(name, handler)                                │
│  - with_hook_timeout(event, ms)                                  │
│  - with_file_checkpointing()                                     │
│  - with_bidir_runner_factory(factory)  [for testing bidir]       │
├─────────────────────────────────────────────────────────────────┤
│  runner.gleam (PUBLIC - UNCHANGED)                               │
│  - Runner type: read() + close() (pull-based for query())        │
│  - Used ONLY by query() path                                     │
│  - Existing test mocks continue to work                          │
├─────────────────────────────────────────────────────────────────┤
│  internal/bidir_runner.gleam (INTERNAL - NEW)                    │
│  - BidirRunner type: write() + close() (push-based)              │
│  - Used ONLY by start_session() path                             │
│  - Exposes raw Port; GenServer matches native {Port, {data,..}}  │
├─────────────────────────────────────────────────────────────────┤
│  internal/bidir.gleam (INTERNAL - GenServer for start_session)   │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │ init: spawn BidirRunner (port owned by GenServer)            │ │
│  ├─────────────────────────────────────────────────────────────┤ │
│  │ State:                                                       │ │
│  │  - runner: BidirRunner  (ALL IO via BidirRunner, push)       │ │
│  │  - pending_requests: Map<RequestId, PendingRequest>          │ │
│  │  - pending_hooks: Map<RequestId, PendingHook>                │ │
│  │  - hooks: HookConfig                                         │ │
│  │  - lifecycle: Starting | InitSent | Running | Stopped        │ │
│  ├─────────────────────────────────────────────────────────────┤ │
│  │ handle_call: send_control_request (interrupt, etc.)          │ │
│  │ handle_info:                                                 │ │
│  │  - {Port, {data, Binary}} → buffer → parse → dispatch        │ │
│  │  - {Port, {exit_status, Code}} → terminate session           │ │
│  │  - {hook_done, ReqId, Result} → send response, cleanup       │ │
│  │  - {hook_timeout, ReqId} → fail-open, kill task, cleanup     │ │
│  │  - {DOWN, Ref, _, _, _} → fail-open/deny, cleanup            │ │
│  └─────────────────────────────────────────────────────────────┘ │
├─────────────────────────────────────────────────────────────────┤
│  internal/stream.gleam (INTERNAL - shared)                       │
│  - Parse NDJSON line → IncomingMessage                           │
│  - Line buffering for partial reads                              │
├─────────────────────────────────────────────────────────────────┤
│  claude_agent_sdk_ffi.erl (INTERNAL)                             │
│  - open_port/1, port_write/2, close_port/1                       │
│  - Port sends {Port, {data, Data}} to owner (GenServer)          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │   Claude CLI    │
                    │ (stream-json)   │
                    └─────────────────┘
```

**API Visibility Legend:**
- **PUBLIC**: Stable API. Changes require semver major bump.
- **INTERNAL**: Implementation detail. May change in minor versions.

**Port Ownership**: Runner is instantiated inside GenServer's `init/1`, ensuring the Port is owned by the GenServer process and messages are delivered to its mailbox.

### Data Model

#### Control Message Types

**Wire format note**: Both SDK→CLI and CLI→SDK use `type: "control_request"` on the wire. The `request.subtype` field discriminates the message kind. The decoder produces a unified `IncomingMessage` type.

```gleam
// New types in control.gleam

/// ============================================================
/// OUTGOING: SDK → CLI (requests we send)
/// ============================================================

/// SDK-initiated control request sent to CLI
pub type OutgoingControlRequest {
  Initialize(
    request_id: String,
    hooks: List(HookRegistration),
    mcp_servers: List(String),
    enable_file_checkpointing: Bool,
  )
  Interrupt(request_id: String)
  SetPermissionMode(request_id: String, mode: PermissionMode)
  SetModel(request_id: String, model: String)
  RewindFiles(request_id: String, user_message_id: String)
}

/// ============================================================
/// INCOMING: CLI → SDK (what the decoder produces)
/// ============================================================

/// Canonical decoder output for all incoming messages
/// The decoder parses `type` field first, then discriminates:
/// - "control_request" → parse `request.subtype` → IncomingControlRequest
/// - "control_response" → parse `response.subtype` → IncomingControlResponse
/// - other → regular Message (system/assistant/user/result)
pub type IncomingMessage {
  /// Regular conversation message (forward to subscriber)
  RegularMessage(Message)
  /// Control request from CLI (hooks, permissions, MCP)
  ControlRequest(IncomingControlRequest)
  /// Control response from CLI (to our pending operations)
  ControlResponse(IncomingControlResponse)
}

/// CLI-initiated control request (hooks, permissions, MCP)
/// Wire: `type: "control_request"` with `request.subtype` in {hook_callback, can_use_tool, mcp_message}
pub type IncomingControlRequest {
  /// Hook callback invocation (subtype: "hook_callback")
  HookCallback(
    request_id: String,
    callback_id: String,
    input: HookInput,
    tool_use_id: Option(String),
  )
  /// Permission request (subtype: "can_use_tool")
  CanUseTool(
    request_id: String,
    tool_name: String,
    input: Dynamic,
    permission_suggestions: List(String),
    blocked_path: Option(String),
  )
  /// MCP server message (subtype: "mcp_message")
  McpMessage(
    request_id: String,
    server_name: String,
    message: Dynamic,
  )
}

/// CLI response to our pending operation
/// Wire: `type: "control_response"` with `response.subtype` in {success, error}
pub type IncomingControlResponse {
  Success(request_id: String, payload: Dynamic)
  Error(request_id: String, message: String)
}

/// ============================================================
/// OUTGOING RESPONSES: SDK → CLI (responses to CLI requests)
/// ============================================================

/// Response we send back to CLI for hooks/permissions/MCP
pub type OutgoingControlResponse {
  HookResponse(request_id: String, result: HookResult)
  PermissionResponse(request_id: String, result: PermissionResult)
  McpResponse(request_id: String, response: Dynamic)
}
```

#### Hook Context Types (per event)

```gleam
// New types in hook.gleam

pub type PreToolUseContext {
  PreToolUseContext(
    tool_name: String,
    tool_input: Dynamic,
    session_id: String,
  )
}

pub type PostToolUseContext {
  PostToolUseContext(
    tool_name: String,
    tool_input: Dynamic,
    tool_output: Dynamic,
    session_id: String,
  )
}

/// Context for can_use_tool permission callback (mirrors wire fields)
pub type CanUseToolContext {
  CanUseToolContext(
    tool_name: String,
    tool_input: Dynamic,
    session_id: String,
    /// CLI-suggested permission behaviors (may vary by CLI version)
    permission_suggestions: List(String),
    /// Path that triggered the permission check (optional, may be absent)
    blocked_path: Option(String),
  )
}

/// Context for hook-related tool uses (simpler than CanUseToolContext)
pub type ToolUseContext {
  ToolUseContext(
    tool_name: String,
    tool_input: Dynamic,
    session_id: String,
  )
}

pub type UserPromptSubmitContext {
  UserPromptSubmitContext(
    prompt: String,
    session_id: String,
  )
}

pub type StopContext {
  StopContext(
    reason: String,
    session_id: String,
  )
}

pub type SubagentStopContext {
  SubagentStopContext(
    subagent_id: String,
    reason: String,
    session_id: String,
  )
}

pub type PreCompactContext {
  PreCompactContext(
    session_id: String,
  )
}
```

#### Hook Results

```gleam
pub type HookResult {
  Continue  // continue: true
  Block(reason: String)  // continue: false, reason
  ModifyInput(new_input: Dynamic)  // For PreToolUse modification
}

pub type PermissionResult {
  Allow
  Deny(reason: String)
}
```

#### Hook Input Typed Decoding

**Where typed decoding happens:** In `hook.gleam`, NOT in `control_decoder.gleam`.

The decoder flow is:
1. `control_decoder.gleam` decodes the wire message → `IncomingControlRequest.HookCallback(request_id, callback_id, input: Dynamic, tool_use_id)`
2. GenServer looks up the callback_id → finds the hook event type and user callback
3. `hook.gleam` decodes `input: Dynamic` → typed context (e.g., `PreToolUseContext`)
4. User callback receives typed context, returns `HookResult`

**Why not decode in control_decoder?**
- `control_decoder` doesn't know which event type a callback_id maps to
- Decoding is deferred until dispatch, when we know the expected shape
- Keeps decoder simple and event-agnostic

**Typed decoder per event:**

```gleam
// hook.gleam

/// Decode hook input to typed context based on event type
pub fn decode_hook_input(
  event: HookEvent,
  input: Dynamic,
) -> Result(HookInputContext, DecodeError) {
  case event {
    PreToolUse -> decode_pre_tool_use_context(input)
    PostToolUse -> decode_post_tool_use_context(input)
    UserPromptSubmit -> decode_user_prompt_submit_context(input)
    Stop -> decode_stop_context(input)
    SubagentStop -> decode_subagent_stop_context(input)
    PreCompact -> decode_pre_compact_context(input)
  }
}

fn decode_pre_tool_use_context(input: Dynamic) -> Result(PreToolUseContext, DecodeError) {
  use tool_name <- result.try(dynamic.field("tool_name", dynamic.string)(input))
  use tool_input <- result.try(dynamic.field("tool_input", dynamic.dynamic)(input))
  use session_id <- result.try(dynamic.field("session_id", dynamic.string)(input))
  Ok(PreToolUseContext(tool_name, tool_input, session_id))
}

// ... similar for each event type
```

**Fallback on decode failure:**

If decoding fails for a hook callback:
1. Log error: "Failed to decode {event} hook input: {decode_error}"
2. Return **fail-open** response (`continue: true`) for hooks
3. Return **fail-deny** response (`behavior: deny`) for permissions
4. Include error details in log for debugging, but don't crash

```gleam
// In GenServer hook dispatch:
case hook.decode_hook_input(event, input) {
  Ok(typed_context) ->
    // Spawn task to call user callback with typed context
    spawn_hook_task(callback, typed_context)
  Error(decode_error) ->
    // Log and fail-open/deny
    log.error("Hook input decode failed", [#("event", event), #("error", decode_error)])
    send_fail_response(request_id, event)
}
```

**Test per event type:**

```gleam
// test/hook_decode_test.gleam

pub fn pre_tool_use_context_decode_test() {
  let input = dynamic.from(#{
    "hook_event_name" => "PreToolUse",
    "tool_name" => "Bash",
    "tool_input" => #{"command" => "ls"},
    "session_id" => "abc123",
    "transcript_path" => "/tmp/transcript.jsonl",
    "cwd" => "/home/user",
  })

  let result = hook.decode_pre_tool_use_context(input)
  should.be_ok(result)

  let ctx = result |> result.unwrap
  should.equal(ctx.tool_name, "Bash")
  should.equal(ctx.session_id, "abc123")
}

pub fn user_prompt_submit_context_decode_test() {
  let input = dynamic.from(#{
    "hook_event_name" => "UserPromptSubmit",
    "prompt" => "Hello Claude",
    "session_id" => "xyz789",
  })

  let result = hook.decode_user_prompt_submit_context(input)
  should.be_ok(result)

  let ctx = result |> result.unwrap
  should.equal(ctx.prompt, "Hello Claude")
}

// ... one test per event type
```

#### GenServer State

```gleam
pub type SessionLifecycle {
  Starting       // CLI spawn in progress
  InitSent       // Initialize request sent, awaiting response
  Running        // Initialization confirmed, normal operation
  Stopped        // Session terminated
  Failed(reason: SessionError)  // Terminal error state
}

/// Pending SDK-initiated operation with typed kind for response validation
pub type PendingRequest {
  PendingRequest(
    kind: PendingRequestKind,
    reply_to: Subject(OperationResult),
    deadline: Int,  // Monotonic time in ms
    timer_ref: Reference,  // Timer for timeout; canceled on response
  )
}

pub type PendingRequestKind {
  InitializeOp
  InterruptOp
  SetPermissionModeOp
  SetModelOp
  RewindFilesOp
}

/// Pending hook task with cleanup references
pub type PendingHook {
  PendingHook(
    task_pid: Pid,
    monitor_ref: Reference,
    timer_ref: Reference,
    callback_id: String,
    is_permission: Bool,  // True for can_use_tool (fail-deny), false for hooks (fail-open)
  )
}

pub type SessionState {
  SessionState(
    runner: Runner,  // Runner owns ALL IO; push model
    lifecycle: SessionLifecycle,
    pending_requests: Map(String, PendingRequest),  // SDK-initiated ops with typed kind
    pending_hooks: Map(String, PendingHook),  // CLI-initiated hooks awaiting callback
    queued_ops: List(QueuedOperation),  // Ops queued during InitSent (max 16)
    hooks: HookConfig,  // callback_id -> handler mapping
    mcp_handlers: Map(String, fn(Dynamic) -> Dynamic),
    next_request_id: Int,
    next_callback_id: Int,  // For generating hook_0, hook_1, etc.
    subscriber: Subject(Message),  // Where to forward regular messages
    capabilities: Option(CliCapabilities),  // Populated after init success
    default_timeout_ms: Int,  // Default 60000
    hook_timeouts: Map(HookEvent, Int),  // Per-hook overrides
  )
}
```

#### Hook Callback ID Generation and Mapping

Callback IDs are generated deterministically at session initialization:

```gleam
/// HookConfig stores the mapping from callback_id to handler
/// MVP: One callback per event type (builders overwrite, not append)
pub type HookConfig {
  HookConfig(
    handlers: Map(String, HookHandler),  // "hook_0" -> handler function
    event_callback: Map(HookEvent, Option(String)),  // PreToolUse -> Some("hook_0") (single)
  )
}

pub type HookHandler {
  HookHandler(
    event: HookEvent,
    callback: fn(Dynamic) -> HookResult,
    timeout_ms: Int,
  )
}
```

**Generation rules (MVP - single callback per event):**
1. Callback IDs are generated as `hook_<N>` where N is a monotonically increasing counter starting at 0
2. Each call to `with_pre_tool_use()`, `with_post_tool_use()`, etc. **overwrites** any existing callback for that event
3. The mapping is built at `start_session()` time and stored in `HookConfig.handlers`
4. **MVP constraint**: One callback per event type. Calling `with_pre_tool_use()` twice replaces the first callback.
5. `can_use_tool` uses ID `perm_0` (single permission callback)
6. IDs are stable within a session but not across sessions (no persistence)
7. Future version may support multiple callbacks per event with ordering/short-circuit semantics

**Dispatch flow:**
1. CLI sends `hook_callback` with `callback_id: "hook_0"`
2. GenServer looks up `hooks.handlers["hook_0"]` to find handler
3. If not found, log error and send fail-open response
4. If found, spawn task to execute handler with the provided input

### Control Message Wire Format (NDJSON)

All control messages are single-line JSON objects terminated by `\n`. The envelope uses **nested `request`/`response` objects** with `subtype` discrimination per the spec.

#### Canonical Envelope Schema

**SDK → CLI (control_request)**:
```
{
  "type": "control_request",
  "request_id": "<unique_id>",
  "request": {
    "subtype": "<operation_type>",
    ...operation-specific fields...
  }
}
```

**CLI → SDK (control_response)**:
```
{
  "type": "control_response",
  "response": {
    "subtype": "success" | "error",
    "request_id": "<echoed_id>",
    "response": { ...payload... },  // if success
    "error": "<message>"            // if error
  }
}
```

**CLI → SDK (control_request for hooks/permissions/MCP)**:
```
{
  "type": "control_request",
  "request_id": "<cli_generated_id>",
  "request": {
    "subtype": "hook_callback" | "can_use_tool" | "mcp_message",
    ...subtype-specific fields...
  }
}
```

#### SDK → CLI: Control Operations

```json
// Initialize handshake (sent immediately after spawn)
{"type":"control_request","request_id":"req_0","request":{"subtype":"initialize","hooks":{"PreToolUse":[{"matcher":null,"hookCallbackIds":["hook_0"]}],"PostToolUse":[{"matcher":null,"hookCallbackIds":["hook_1"]}]},"mcp_servers":["my-server"],"enable_file_checkpointing":true}}

// Interrupt
{"type":"control_request","request_id":"req_1","request":{"subtype":"interrupt"}}

// Set permission mode
{"type":"control_request","request_id":"req_2","request":{"subtype":"set_permission_mode","mode":"acceptEdits"}}

// Set model
{"type":"control_request","request_id":"req_3","request":{"subtype":"set_model","model":"sonnet"}}

// Rewind files
{"type":"control_request","request_id":"req_4","request":{"subtype":"rewind_files","user_message_id":"msg_123"}}
```

#### CLI → SDK: Hook/Permission/MCP Invocations

```json
// Hook callback (subtype: hook_callback)
{"type":"control_request","request_id":"cli_1","request":{"subtype":"hook_callback","callback_id":"hook_0","input":{"hook_event_name":"PreToolUse","session_id":"abc123","tool_name":"Bash","tool_input":{"command":"ls"}},"tool_use_id":"toolu_01ABC"}}

// Permission callback (subtype: can_use_tool)
{"type":"control_request","request_id":"cli_2","request":{"subtype":"can_use_tool","tool_name":"Write","input":{"file_path":"/etc/passwd"},"permission_suggestions":["deny"],"blocked_path":"/etc"}}

// MCP message (subtype: mcp_message)
{"type":"control_request","request_id":"cli_3","request":{"subtype":"mcp_message","server_name":"my-server","message":{"jsonrpc":"2.0","id":1,"method":"tools/list"}}}
```

#### SDK → CLI: Responses to CLI-Initiated Requests

```json
// Hook response (continue)
{"type":"control_response","response":{"subtype":"success","request_id":"cli_1","response":{"continue":true}}}

// Hook response (block)
{"type":"control_response","response":{"subtype":"success","request_id":"cli_1","response":{"continue":false,"stopReason":"Blocked by policy"}}}

// Hook response (modify input - PreToolUse only)
{"type":"control_response","response":{"subtype":"success","request_id":"cli_1","response":{"continue":true,"hookSpecificOutput":{"hookEventName":"PreToolUse","updatedInput":{"command":"ls -la"}}}}}

// Permission response (allow)
{"type":"control_response","response":{"subtype":"success","request_id":"cli_2","response":{"behavior":"allow"}}}

// Permission response (deny)
{"type":"control_response","response":{"subtype":"success","request_id":"cli_2","response":{"behavior":"deny","message":"Write to /etc not permitted"}}}

// MCP response
{"type":"control_response","response":{"subtype":"success","request_id":"cli_3","response":{"mcp_response":{"jsonrpc":"2.0","id":1,"result":{"tools":[]}}}}}

// MCP error response (handler crashed)
{"type":"control_response","response":{"subtype":"success","request_id":"cli_3","response":{"mcp_response":{"jsonrpc":"2.0","id":1,"error":{"code":-32603,"message":"Handler crashed"}}}}}
```

#### CLI → SDK: Operation Responses

```json
// Success response (to SDK-initiated request)
{"type":"control_response","response":{"subtype":"success","request_id":"req_0","response":{"supported_commands":["interrupt","set_permission_mode"],"capabilities":{"hooks":true}}}}

// Error response
{"type":"control_response","response":{"subtype":"error","request_id":"req_1","error":"Operation not supported"}}
```

#### Decoder Responsibilities (`control_decoder.gleam`)

The decoder produces a unified `IncomingMessage` type for all messages from CLI:

```gleam
/// Main entry point: decode NDJSON line → IncomingMessage
pub fn decode_line(json: String) -> Result(IncomingMessage, DecodeError)
```

**Decoding flow:**

1. **Top-level discrimination**: Check `type` field:
   - `"control_request"` → CLI is invoking a hook/permission/MCP (parse as `IncomingControlRequest`)
   - `"control_response"` → CLI is responding to our pending operation (parse as `IncomingControlResponse`)
   - `"system"` / `"assistant"` / `"user"` / `"result"` → Regular message (parse as `RegularMessage(Message)`)

2. **For `type: "control_request"`** (CLI → SDK invocations):
   - Extract `request.subtype` and dispatch:
     - `"hook_callback"` → `IncomingControlRequest.HookCallback(...)`
     - `"can_use_tool"` → `IncomingControlRequest.CanUseTool(...)`
     - `"mcp_message"` → `IncomingControlRequest.McpMessage(...)`
   - **Note**: SDK-initiated subtypes (`initialize`, `interrupt`, etc.) are OUTGOING only — they should never appear in incoming messages. If received, log error and return `DecodeError`.

3. **For `type: "control_response"`** (CLI → SDK responses):
   - Extract `response.subtype` ("success" / "error") and `response.request_id`
   - Return `IncomingControlResponse.Success(...)` or `IncomingControlResponse.Error(...)`

4. **Response validation** (performed by GenServer after decode):
   - Look up `pending_requests[request_id]` to get `PendingRequestKind`
   - **Validation strategy: PERMISSIVE for most ops, STRICT only for initialize**

   | Operation | Validation | Rationale |
   |-----------|------------|-----------|
   | `InitializeOp` | **Soft-strict**: extract capabilities if present, degrade gracefully if missing | Avoid startup failure when CLI returns minimal payload |
   | `InterruptOp` | **Permissive**: any success response is OK | Just need ack; CLI may add fields over time |
   | `SetPermissionModeOp` | **Permissive**: any success response is OK | Just need ack |
   | `SetModelOp` | **Permissive**: any success response is OK | Just need ack |
   | `RewindFilesOp` | **Permissive**: any success response is OK | Just need ack |

   **Soft-strict validation for InitializeOp:**
   ```gleam
   fn decode_init_response(payload: Dynamic) -> InitResult {
     // Try to extract capabilities, but don't fail if missing
     let capabilities = case dynamic.field("capabilities", decode_capabilities)(payload) {
       Ok(caps) -> Some(caps)
       Error(_) -> None  // CLI may return empty/minimal response
     }
     let supported_commands = case dynamic.field("supported_commands", dynamic.list(dynamic.string))(payload) {
       Ok(cmds) -> cmds
       Error(_) -> []  // Assume empty if not provided
     }
     InitResult(
       capabilities: capabilities,
       supported_commands: supported_commands,
       raw_payload: Some(payload),  // Store for debugging
     )
   }
   ```

   **Why soft-strict for InitializeOp:**
   - Some CLI versions may return minimal/empty success payloads
   - If hooks are being invoked (implicit confirmation), the system is working
   - Strict decoding would convert a working session into a startup failure
   - SDK treats missing capabilities as `None` and disables dependent features
   - Logging warns when expected fields are missing for debugging

   **Permissive validation logic:**
   ```gleam
   case pending_request.kind {
     InitializeOp ->
       // Soft-strict: extract what we can, degrade gracefully
       let init_result = decode_init_response(response.payload)
       if init_result.capabilities == None {
         log.warn("Initialize response missing capabilities, features may be limited")
       }
       Ok(OperationResult.Init(init_result))
     InterruptOp | SetPermissionModeOp | SetModelOp | RewindFilesOp ->
       // Permissive: success subtype + matching request_id = OK
       // Store raw payload for debugging if caller wants it
       Ok(OperationResult.Success(raw_payload: Option(response.payload)))
   }
   ```

   **Why permissive for most ops:**
   - CLIs often add fields over time (forward compatibility)
   - Strict shape checks on ack-only ops create false failures
   - Operation succeeded if CLI returned `success`; payload contents are informational

   - If `request_id` not found in pending: log warning "Orphan response for {request_id}", drop

5. **Unknown fields**: Silently ignore (forward compatibility)
6. **Missing required fields**: Return `DecodeError` with field path

#### `request_id` Location in control_response (Spec Clarification)

**Source**: `plans/CLAUDE_AGENT_SDK_BIDIR_ADDITIONS.md` section 6.2

**Spec-defined location** (lines 46-63):
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

The spec clearly shows `request_id` is nested at `response.request_id`, NOT at the top level.

**Tolerant decoding rules** (for robustness against CLI variations):

| Location | Priority | Action |
|----------|----------|--------|
| `response.request_id` | 1 (preferred) | Use this value |
| Top-level `request_id` | 2 (fallback) | Use if `response.request_id` is missing |
| Neither present | — | Return `DecodeError("missing request_id")` |

**Decoding algorithm:**
```gleam
fn extract_request_id(json: Dynamic) -> Result(String, DecodeError) {
  // Try spec-compliant location first
  case dynamic.field("response", dynamic.field("request_id", dynamic.string))(json) {
    Ok(id) -> Ok(id)
    Error(_) ->
      // Fallback to top-level for forward compatibility
      case dynamic.field("request_id", dynamic.string)(json) {
        Ok(id) -> Ok(id)
        Error(_) -> Error(DecodeError("request_id not found in response.request_id or top-level"))
      }
  }
}
```

**Summary of ID locations by message type:**

| Message Type | `request_id` Location | Notes |
|--------------|----------------------|-------|
| `control_request` (SDK→CLI) | Top-level `request_id` | We generate this |
| `control_request` (CLI→SDK) | Top-level `request_id` | CLI generates this |
| `control_response` (CLI→SDK) | `response.request_id` | Echoes our request |
| `control_response` (SDK→CLI) | `response.request_id` | Echoes CLI's request |

#### Wire Schema Source of Truth

**All field names and structures are taken verbatim from `plans/CLAUDE_AGENT_SDK_BIDIR_ADDITIONS.md`** except where noted. This section explicitly marks the source of each field:

| Message Type | Field | Source | Notes |
|--------------|-------|--------|-------|
| Initialize request | `hooks` | **SPEC** | Object keyed by event name, each containing `[{matcher, hookCallbackIds}]` |
| Initialize request | `mcp_servers` | **SPEC** | Array of server name strings |
| Initialize request | `enable_file_checkpointing` | **SPEC** | Boolean |
| Hook callback | `callback_id` | **SPEC** | String matching registered callback ID |
| Hook callback | `input` | **SPEC** | Object with `hook_event_name`, `session_id`, tool-specific fields |
| Hook callback | `tool_use_id` | **SPEC** | String identifying the tool use |
| Hook response | `continue` | **SPEC** | Boolean |
| Hook response | `stopReason` | **SPEC** | String (not `reason`) — used when `continue: false` |
| Hook response | `hookSpecificOutput` | **SPEC** | Object with `hookEventName` and event-specific output |
| Permission request | `permission_suggestions` | **SPEC** | Array of suggested behaviors (may vary by CLI version) |
| Permission request | `blocked_path` | **INFERRED** | Not in spec; may not be present in all CLI versions |
| Permission response | `behavior` | **SPEC** | `"allow"` or `"deny"` |
| Permission response | `message` | **SPEC** | Optional denial reason |

**Decoder behavior for unknown/missing fields:**
- **Unknown fields**: Silently ignored (forward compatibility)
- **Missing optional fields**: Use default values or `Option.None`
- **Missing required fields**: Return `DecodeError` with field path
- **`blocked_path`**: Treat as optional; decode as `Option(String)` with `None` default

**CLI version variability:**
- `permission_suggestions` schema may evolve; decode as `List(String)` and handle unknown values gracefully
- New fields may appear in future CLI versions; decoders must not fail on unknown keys

#### Exact Enum/String Values

**Hook event names** (used as keys in `hooks` object during initialize):

| Gleam Enum | Wire String | Notes |
|------------|-------------|-------|
| `PreToolUse` | `"PreToolUse"` | PascalCase per spec |
| `PostToolUse` | `"PostToolUse"` | PascalCase per spec |
| `UserPromptSubmit` | `"UserPromptSubmit"` | PascalCase per spec |
| `Stop` | `"Stop"` | PascalCase per spec |
| `SubagentStop` | `"SubagentStop"` | PascalCase per spec |
| `PreCompact` | `"PreCompact"` | PascalCase per spec |

#### Initialize `hooks` Schema (Spec Excerpt)

**Source**: `plans/CLAUDE_AGENT_SDK_BIDIR_ADDITIONS.md` section 6.3

**Spec example (verbatim from spec lines 93-109):**
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

**Spec field definitions (lines 133-139):**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `hooks` | object | **no** | Map of hook event name → list of matchers |
| `hooks.<event>[]` | object | yes | Hook matcher configuration |
| `hooks.<event>[].matcher` | string \| null | **no** | Tool matcher pattern |
| `hooks.<event>[].hookCallbackIds` | string[] | yes | Callback IDs assigned by SDK |
| `hooks.<event>[].timeout` | integer | no | Timeout in seconds (default 60) |

**Key observations:**
- Keys are PascalCase event names (`PreToolUse`, NOT `pre_tool_use`)
- Values are **arrays** of matcher objects (allows multiple matchers per event)
- `matcher` can be: string pattern (`"Write|Edit"`), explicit `null` (match all), or **omitted** (spec says required: no)
- `timeout` is optional, defaults to 60 seconds

**MVP Encoding Strategy (Single "Always Works" Approach):**

The SDK uses a **deterministic, conservative encoding** to maximize compatibility:

```json
{
  "hooks": {
    "PreToolUse": [{"matcher": null, "hookCallbackIds": ["hook_0"]}],
    "PostToolUse": [{"matcher": null, "hookCallbackIds": ["hook_1"]}]
  }
}
```

| Rule | MVP Behavior | Rationale |
|------|--------------|-----------|
| Include event key? | **Yes, always** when user registered a callback for that event | CLI may ignore events without a key |
| `matcher` field | **Always `null` (explicit)** | Explicit null is unambiguous; omitting may vary by CLI version |
| `hookCallbackIds` | **Always include** (required field) | Spec requires this |
| `timeout` field | **Omit** (use CLI default 60s) | Avoid unit conversion issues in MVP; add later if needed |
| Empty hooks object | **Omit `hooks` entirely** if no hooks registered | Cleaner; spec allows omission |

**Why explicit `matcher: null` vs omitting:**
- Spec says `matcher` is optional (required: no)
- However, CLI version behavior may differ: some may treat omitted as "no match" vs `null` as "match all"
- MVP uses explicit `null` to ensure consistent "match all" semantics across CLI versions

**Encoding function (MVP):**

```gleam
fn encode_hooks(registered_hooks: List(#(HookEvent, CallbackId))) -> Option(Json) {
  case registered_hooks {
    [] -> None  // Omit hooks field entirely
    hooks -> {
      // Group by event, always use matcher: null
      let grouped = list.group(hooks, fn(h) { h.0 })
      let encoded = map.map(grouped, fn(event, callbacks) {
        [json.object([
          #("matcher", json.null()),  // Always explicit null
          #("hookCallbackIds", json.array(callbacks, fn(c) { json.string(c.1) })),
          // timeout: omitted, use CLI default
        ])]
      })
      Some(json.object(map.to_list(encoded)))
    }
  }
}
```

**Timeout unit conversion (internal milliseconds → wire seconds):**

The SDK internal API uses `timeout_ms: Int` (milliseconds) for consistency with Erlang/OTP timer APIs. The wire protocol uses `timeout: Int` (seconds) per the spec.

| SDK API | Wire Field | Conversion |
|---------|------------|------------|
| `with_hook_timeout(event, 30_000)` | `"timeout": 30` | `timeout_ms / 1000` (truncate) |
| `with_timeout(90_000)` | `"timeout": 90` | `timeout_ms / 1000` (truncate) |

**MVP behavior**: The SDK **omits** the `timeout` field in initialize requests, relying on CLI default (60s). If/when user-configurable hook timeouts are added:

```gleam
/// Convert internal milliseconds to wire seconds
fn ms_to_wire_seconds(timeout_ms: Int) -> Int {
  // Truncate toward zero (e.g., 59_999ms → 59s, 60_001ms → 60s)
  timeout_ms / 1000
}
```

**Rounding behavior**: Truncation (floor division) is used, NOT rounding. This ensures:
- `59_999ms` becomes `59s` (conservative, timeout fires earlier on SDK side)
- `60_001ms` becomes `60s` (not rounded up to 61s)
- Callers who want exact second boundaries should pass multiples of 1000

**Decoder tolerance rules (for robustness):**
- Accept `hooks` field present as `{}` OR field omitted entirely
- Accept `matcher` field as: string, explicit `null`, OR omitted (treat omitted as `null`)
- Ignore unknown fields in hook registration objects (forward compatibility)

**Omit vs null rules:**

| Field | Behavior |
|-------|----------|
| `hooks` (initialize) | Omit if no hooks registered; include with empty object `{}` is also valid |
| `matcher` (hook registration) | `null` = match all; string = pattern match; omit = treat as `null` |
| `mcp_servers` (initialize) | Omit if empty, or include as `[]` — both valid |
| `enable_file_checkpointing` | Include always; `false` if not enabled |
| `blocked_path` (permission req) | Optional field from CLI; may be absent |
| `tool_use_id` (hook callback) | Optional field from CLI; may be absent for some hooks |

**Permission mode values:**

| Gleam Enum | Wire String |
|------------|-------------|
| `Default` | `"default"` |
| `AcceptEdits` | `"acceptEdits"` |
| `BypassPermissions` | `"bypassPermissions"` |

**Permission response behavior values:**

| Gleam Enum | Wire String |
|------------|-------------|
| `Allow` | `"allow"` |
| `Deny` | `"deny"` |

#### Malformed CLI Request Handling

For incoming NDJSON lines that fail to parse:

1. **Invalid JSON** (syntax error):
   - Log error: "Malformed JSON on line: {truncated_content}"
   - Drop the line (cannot extract request_id, cannot respond)

2. **Valid JSON but missing required fields** (e.g., no `type`, no `request_id`, no `request.subtype`):
   - If `request_id` is extractable: send spec-compliant error response:
     ```json
     {"type":"control_response","response":{"subtype":"error","request_id":"<id>","error":"Missing required field: <field>"}}
     ```
   - If `request_id` not extractable: log error and drop

3. **Valid JSON with unknown `request.subtype`**:
   - Send error response: `"error":"Unknown subtype: <subtype>"`

### Initialization State Machine

The SDK uses a strict state machine for session lifecycle:

```
                    ┌─────────────────────────────────────────────────────────┐
                    │                                                         │
   start_session()  │                                                         ▼
        │           │                                              ┌──────────────────┐
        ▼           │                                              │     FAILED       │
┌──────────────┐    │  timeout/error                               │ (terminal state) │
│   STARTING   │────┼──────────────────────────────────────────────┤                  │
│              │    │                                              └──────────────────┘
│ (spawn CLI)  │    │
└──────────────┘    │
        │           │
        │ CLI spawned, send initialize
        ▼           │
┌──────────────┐    │
│  INIT_SENT   │────┘
│              │
│ (awaiting    │
│  response)   │
└──────────────┘
        │
        │ explicit success OR first hook_callback/can_use_tool (NOT regular messages)
        ▼
┌──────────────┐         stop()/port closed
│   RUNNING    │────────────────────────────────────────────────────► STOPPED
│              │
│ (normal ops) │
└──────────────┘
```

#### State Transitions

| From | Event | To | Action |
|------|-------|-----|--------|
| STARTING | CLI spawned | INIT_SENT | Send initialize control_request |
| INIT_SENT | Explicit success response (`response.request_id` matches init) | RUNNING | Store capabilities, flush queued ops |
| INIT_SENT | First `hook_callback` or `can_use_tool` request | RUNNING | Implicit confirmation (hooks/perms active) |
| INIT_SENT | Error response (`subtype: "error"`) | FAILED | Return `InitializationError(message)` |
| INIT_SENT | 10s timeout | FAILED | Return `InitializationTimeout` |
| INIT_SENT | Port closed | FAILED | Return `CliExitedDuringInit(exit_code)` |
| RUNNING | stop() called | STOPPED | Close port, notify subscribers |
| RUNNING | Port closed | STOPPED | Notify subscribers with exit code |

**Implicit confirmation rules (tightened):**

**Subtypes that trigger implicit RUNNING confirmation:**

| Incoming Subtype | Triggers RUNNING? | Rationale |
|------------------|-------------------|-----------|
| `hook_callback` | **Yes** | CLI is invoking a registered hook — proves CLI accepted init |
| `can_use_tool` | **Yes** | CLI is requesting permission — proves CLI accepted init |
| `mcp_message` | **Yes** | CLI is routing MCP message — proves CLI accepted init |
| Regular messages (system/assistant/user/result) | **No** | CLI may emit output while ignoring init |

**Handling CLI-initiated control_request during INIT_SENT:**

1. Any `control_request` with subtype in `{hook_callback, can_use_tool, mcp_message}` triggers RUNNING
2. Process the request normally (dispatch to handler, send response)
3. If `callback_id` is unknown (not in our registered hooks):
   - Still transition to RUNNING (CLI clearly accepted our init)
   - Log warning: "Unknown callback_id '{id}' during implicit confirmation"
   - Send fail-open response (`continue: true`) for hooks, fail-deny (`behavior: deny`) for permissions
4. Continue processing subsequent messages in RUNNING state

**Why regular messages don't count:**
- CLI may emit `system` or `assistant` messages before processing our initialize request
- Treating them as confirmation would cause false-positive RUNNING when CLI doesn't support hooks
- Only CLI-initiated control_request proves the CLI understood and accepted our bidirectional handshake

#### Control Operations During Initialization

- **Queue size limit**: Max 16 queued operations during INIT_SENT state
- **Queue overflow**: Returns `InitQueueOverflow` error immediately
- **On RUNNING**: Flush queue in order (see flush semantics below)
- **On FAILED**: All queued ops receive `SessionNotInitialized` error

**Queue flush semantics (on transition to RUNNING):**

The GenServer flushes all queued operations **immediately without blocking**:

```
for each queued_op in queue (in order):
  1. Generate request_id for the op
  2. Create PendingRequest with timer (same as normal operation)
  3. Add to pending_requests map
  4. Send control_request to CLI via Runner.write()
  // Do NOT wait for response — continue to next op
```

This approach:
- Sends all queued ops in rapid succession (non-blocking writes)
- Each op gets its own PendingRequest with independent timeout
- The GenServer remains responsive to incoming hook requests during flush
- Callers block on their individual `actor.call` until their specific response arrives (or timeout)
- `pending_requests` limit (64) accommodates burst: 16 queued + init + normal ops

**Example timeline:**
```
t0: RUNNING transition → flush begins
t1: Op A sent, PendingRequest A created (caller A blocking)
t2: Op B sent, PendingRequest B created (caller B blocking)
t3: Op C sent, PendingRequest C created (caller C blocking)
t4: flush complete, GenServer processes next message (could be hook request)
t5: CLI response for B arrives → caller B unblocks
t6: CLI response for A arrives → caller A unblocks
t7: CLI response for C arrives → caller C unblocks
```

Note: Response order from CLI may differ from send order; correlation handles this correctly.

#### Backpressure Limits (Prevent Unbounded Growth)

| Map | Max Size | On Overflow |
|-----|----------|-------------|
| `queued_ops` | 16 | Return `InitQueueOverflow` to caller |
| `pending_requests` | 64 | Return `TooManyPendingRequests` to caller |
| `pending_hooks` | 32 | Send immediate fail-open/deny response, don't spawn task |

**Rationale for default values:**

| Limit | Value | Rationale |
|-------|-------|-----------|
| `queued_ops` (16) | During INIT_SENT, unlikely to have >16 pending control calls. If exceeded, likely indicates init is stuck or misconfigured. |
| `pending_requests` (64) | Accounts for: init (1) + flush of 16 queued ops + normal concurrent ops. Real usage unlikely to exceed 20-30 concurrent. |
| `pending_hooks` (32) | CLI sends hooks sequentially per tool use. 32 allows burst during rapid tool sequences. If exceeded, CLI is overwhelming SDK. |

**Why fixed defaults (not configurable in MVP):**
- Exceeding these limits indicates a problem (stuck CLI, runaway loop, misconfiguration)
- Making them configurable hides bugs rather than surfacing them
- MVP focuses on correctness; tuning can be added if real workloads require

**Error messages and mitigations:**

| Error | Message | Mitigation |
|-------|---------|------------|
| `InitQueueOverflow` | "Too many control operations queued during initialization (max 16). Is the CLI responding?" | Check CLI version, reduce concurrent calls during startup |
| `TooManyPendingRequests` | "Too many pending control requests (max 64). Check for stuck operations or excessive concurrency." | Ensure previous ops complete before starting new ones; check hook timeout settings |
| Hooks at limit | (Logged, not returned) "Hook queue at capacity (32), sending fail-open/deny for request {id}" | Reduce hook callback latency; increase timeout if hooks are legitimately slow |

**Future configurability (post-MVP):**

If real workloads require different limits, add to `options.gleam`:

```gleam
/// Configure backpressure limits (default values are usually sufficient)
pub fn with_backpressure_limits(
  options: QueryOptions,
  queued_ops_max: Int,
  pending_requests_max: Int,
  pending_hooks_max: Int,
) -> QueryOptions
```

This is intentionally verbose to discourage casual tuning without understanding the implications.

- When limits are hit, log at WARN level with current count
- Hook timeouts ensure `pending_hooks` entries are eventually cleaned up

### CLI Version Detection Mechanism

The SDK detects CLI compatibility using a **strict version check with runtime fallback**:

#### Version Check Policy

| Scenario | Behavior | Rationale |
|----------|----------|-----------|
| Version parseable AND >= MIN | Proceed | Known compatible |
| Version parseable AND < MIN | **Fail fast**: `UnsupportedCliVersion(version)` | Known incompatible — no point trying |
| Version unparseable (format changed) | Log warning, proceed | May be newer CLI with different format |
| Version check timeout (2s) | Log warning, proceed | CLI may be slow to start |
| `claude --version` spawn fails | Proceed | Rely on runtime detection |

This policy ensures:
- Clear error when CLI is **known incompatible** (parseable version < MIN)
- Graceful degradation when version is **unknown** (unparseable/timeout/spawn fail)
- Runtime detection catches any edge cases

#### Legacy vs Bidir CLI Flags

**Key clarification**: The existing `query()` function does NOT use `--input-format stream-json`.

| API | CLI Flags | Protocol |
|-----|-----------|----------|
| `query()` (legacy) | `--output-format stream-json` only | Unidirectional: CLI → SDK NDJSON output; SDK → CLI via prompt argument only |
| `start_session()` (bidir) | `--output-format stream-json` **AND** `--input-format stream-json` | Bidirectional: NDJSON both directions; control messages + initialization handshake |

**What `--input-format stream-json` enables:**
- SDK can write NDJSON to CLI's stdin (not just pass initial prompt)
- Control protocol: initialize handshake, interrupt, set_model, etc.
- Hook callbacks: CLI sends control_request, SDK responds
- Permission callbacks: can_use_tool flow
- MCP server message routing

**What legacy mode lacks** (why bidir features require `start_session()`):
- No stdin streaming — prompt is passed as command-line argument
- No control messages — cannot interrupt, change model, etc.
- No hooks — CLI doesn't send callback requests
- No initialize handshake — no capability negotiation

#### Bidir Startup Write Sequence

**How prompt is delivered in bidir mode:**

The CLI in bidir mode expects the prompt to be sent as a **user message NDJSON** after the initialize handshake, NOT via command-line argument.

**Exact startup sequence:**

```
1. Spawn CLI:
   claude --output-format stream-json --input-format stream-json --verbose
   (NO prompt argument - prompt comes via stdin)

2. Send initialize control_request:
   {"type":"control_request","request_id":"req_0","request":{"subtype":"initialize","hooks":{...}}}

3. Wait for initialization confirmation:
   - Explicit: {"type":"control_response","response":{"subtype":"success","request_id":"req_0",...}}
   - Or implicit: First hook_callback/can_use_tool from CLI

4. Send user prompt as user message NDJSON:
   {"type":"user","content":"<prompt from start_session()>"}

5. CLI processes prompt and streams responses (system/assistant/result messages)
   Interleaved with control_requests for hooks, permissions, etc.
```

**Key points:**
- Prompt is NOT passed via `-- -- "prompt"` in bidir mode
- Prompt is sent AFTER initialize handshake completes
- User message format: `{"type":"user","content":"<text>"}`
- Subsequent user prompts (if API supports) use same user message format

**What if CLI expects prompt via argv?**

The spec (`plans/CLAUDE_AGENT_SDK_BIDIR_ADDITIONS.md` section 3.5) states:
> "Regular prompt strings are not passed via `-- -- "prompt"`; instead the SDK streams input messages and control requests."

This confirms: bidir mode uses stdin for prompts, not command-line arguments.

**Multi-turn conversations:**

If the SDK later supports multi-turn, additional user prompts are sent the same way:
```json
{"type":"user","content":"Follow-up question"}
```

The current MVP scope is single-prompt sessions, but the protocol supports multi-turn.

#### Decision Tree for Bidirectional Mode

```
start_session() with bidir features (hooks/can_use_tool/mcp_servers):
│
├─► Attempt `claude --version` (timeout 2s)
│   ├─► Parse succeeded, version >= MIN_BIDIR_CLI_VERSION
│   │   └─► Proceed to spawn with --input-format stream-json
│   │
│   ├─► Parse succeeded, version < MIN_BIDIR_CLI_VERSION
│   │   └─► FAIL FAST: Return UnsupportedCliVersion(parsed_version)
│   │
│   ├─► Parse failed (unparseable format, timeout)
│   │   └─► Log warning: "Version check inconclusive, attempting bidir mode"
│   │       └─► Proceed to spawn (rely on runtime detection below)
│   │
│   └─► spawn_executable failed for --version check
│       └─► Log warning: "Version check failed, attempting bidir mode"
│           └─► Proceed to spawn (rely on runtime detection below)
│
├─► CLI spawned with stream-json flags (with `2>&1` stderr redirect)
│   ├─► CLI exits immediately (exit code != 0)
│   │   └─► Check captured output for "unknown flag" / "invalid option"
│   │       └─► Return UnsupportedCliVersion(output_snippet)
│   │
│   ├─► CLI running, send initialize request
│   │   ├─► Receive control_response with subtype: "success"
│   │   │   └─► RUNNING (confirmed)
│   │   │
│   │   ├─► Receive control_response with subtype: "error"
│   │   │   └─► Return InitializationError(message)
│   │   │
│   │   ├─► Receive hook_callback or can_use_tool (implicit success)
│   │   │   └─► RUNNING (confirmed)
│   │   │
│   │   └─► 10s timeout with no confirmation signal
│   │       └─► Return InitializationTimeout
```

**Precedence:**
1. Runtime behavior (init response / hook request) takes precedence over version parse
2. Version parse is advisory — failure logs warning but doesn't block
3. If CLI doesn't support bidir, it will fail fast at spawn or init

**Version constant**: `const MIN_BIDIR_CLI_VERSION = "1.0.33"` (to be verified in prerequisites)

#### Startup Error Prioritization

When startup fails, the SDK must return actionable errors. This section defines error priority:

| Priority | Condition | Error | Payload | User Guidance |
|----------|-----------|-------|---------|---------------|
| 1 | CLI spawn exits immediately + output contains "unknown flag"/"invalid option" | `UnsupportedCliVersion` | `output_snippet: String` | "CLI version does not support bidirectional mode. Update to {MIN_VERSION} or later." |
| 2 | Version check parsed successfully + version < MIN | `UnsupportedCliVersion` | `parsed_version: String` | "CLI version {version} is below minimum {MIN_VERSION} for bidirectional mode." |
| 3 | CLI spawn fails (ENOENT, permission denied, etc.) | `CliNotFound` / `SpawnError` | `os_error: String` | "Could not start Claude CLI: {os_error}" |
| 4 | Initialize timeout (10s) with version check inconclusive | `InitializationTimeout` | `captured_output: Option(String)` | Include any captured stdout/stderr to aid debugging |
| 5 | Initialize returns error response | `InitializationError` | `message: String` | CLI-provided error message |
| 6 | Port closed before init complete | `CliExitedDuringInit` | `exit_code: Int, output: Option(String)` | Include exit code and any captured output |

**Error type definition:**

```gleam
pub type StartError {
  /// CLI doesn't support bidir (detected via version or "unknown flag")
  UnsupportedCliVersion(
    reason: String,      // "unknown flag in output" or "version X.Y.Z < min"
    output: Option(String),
  )
  /// CLI executable not found or spawn failed
  SpawnError(os_error: String)
  /// Initialization handshake timed out
  InitializationTimeout(
    timeout_ms: Int,
    captured_output: Option(String),  // Include for debugging
  )
  /// CLI returned error during initialization
  InitializationError(message: String)
  /// CLI exited before initialization completed
  CliExitedDuringInit(exit_code: Int, output: Option(String))
}
```

**Key UX principle**: When init times out after an inconclusive version check, include any captured output in the error so users can diagnose whether the CLI is incompatible, misconfigured, or simply slow.

**Stderr handling in port spawn:**
- The spawn command includes `2>&1` to redirect stderr to stdout
- This ensures CLI error messages (like "unknown flag") are captured via the port
- Port options: `open_port({spawn, "sh -c 'claude ... 2>&1'"}, [binary, {line, 65536}, exit_status])`
- Alternative: Use `open_port({spawn_executable, ...}, [{stderr_to_stdout, true}, ...])`
- The `stderr_to_stdout` option is preferred when available (Erlang/OTP 25+), falling back to shell redirect otherwise

### Message Framing and Long Line Handling

NDJSON framing requires reliable line detection. This section specifies how the SDK handles partial reads and long lines.

#### Port Configuration

**Recommended approach: Binary mode with manual line buffering**

```erlang
% Do NOT use {line, N} — it truncates long lines silently
open_port({spawn, "sh -c 'claude ... 2>&1'"}, [
  binary,
  {packet, 0},     % Raw binary mode
  exit_status,
  use_stdio
])
```

**Why not `{line, N}`:**
- `{line, 65536}` truncates lines longer than 65536 bytes without indication
- Control messages with large tool inputs (e.g., file contents) can exceed this
- Truncated JSON becomes invalid and is silently dropped

#### SDK-Side Line Buffering

The SDK implements its own newline buffering in the stream parser:

```gleam
/// Stream parser state
pub type StreamState {
  StreamState(
    buffer: BitArray,       // Accumulated bytes
    max_line_size: Int,     // Safety limit (default 16 MB)
  )
}

/// Process incoming port data
pub fn handle_port_data(state: StreamState, data: BitArray) -> #(StreamState, List(String)) {
  let combined = bit_array.append(state.buffer, data)
  let #(lines, remaining) = extract_complete_lines(combined)

  // Safety check: reject lines exceeding max size
  let valid_lines = list.filter(lines, fn(line) {
    string.byte_size(line) <= state.max_line_size
  })

  #(StreamState(..state, buffer: remaining), valid_lines)
}
```

#### Line Size Limits

| Limit | Default | Rationale |
|-------|---------|-----------|
| Max line size | 16 MB | Safety limit; real messages should be much smaller |
| Buffer growth warning | 1 MB | Log warning if buffer exceeds this between newlines |

**Handling oversized lines:**
1. If a single line exceeds `max_line_size`:
   - Log error: "Message exceeds max line size ({size} bytes), dropping"
   - Discard the buffer up to and including the next newline
   - Resume normal parsing
   - This is a fail-safe; real CLI messages should not exceed this limit

#### Partial Line Handling (Robustness Test)

The mock runner tests MUST verify partial line delivery:

```gleam
// test/stream_parser_test.gleam

pub fn partial_line_test() {
  // Simulate partial reads (common with large messages)
  let state = stream.new_state()

  // First chunk: partial JSON
  let #(state, lines1) = stream.handle_port_data(state, <<"{"type":"con">>)
  should.equal(lines1, [])  // No complete line yet

  // Second chunk: rest of line
  let #(state, lines2) = stream.handle_port_data(state, <<"trol_response"}\n">>)
  should.equal(lines2, ["{\"type\":\"control_response\"}"])

  // Multiple lines in one chunk
  let #(_, lines3) = stream.handle_port_data(state, <<"{\"a\":1}\n{\"b\":2}\n">>)
  should.equal(lines3, ["{\"a\":1}", "{\"b\":2}"])
}
```

### Port Message Protocol (Source of Truth)

This section is the **canonical specification** for port IO. All other sections must align with this.

#### Erlang Port Configuration

**Primary spawn mechanism** (deterministic, cross-platform):

```erlang
%% claude_agent_sdk_ffi.erl

%% Use spawn_executable for predictable behavior across platforms
open_port({spawn_executable, ClaudePath}, [
  {args, ["--output-format", "stream-json", "--input-format", "stream-json" | ExtraArgs]},
  binary,           % Receive data as binary, not list
  {packet, 0},      % Raw binary mode, no framing
  exit_status,      % Receive {Port, {exit_status, Code}} on process exit
  use_stdio,        % Communicate via stdin/stdout
  stderr_to_stdout  % Merge stderr into stdout
]).
```

**Compatibility constraints:**

| Requirement | Constraint |
|-------------|------------|
| Minimum OTP version | **OTP 25+** (for `stderr_to_stdout` option) |
| Supported OSes | Linux, macOS (Windows support deferred) |
| `claude` executable | Must be in PATH or absolute path provided |

**Why `spawn_executable` over `spawn`:**
- `{spawn, "sh -c '...'"}` introduces shell quoting/escaping issues
- `spawn_executable` passes args directly to the process, no shell involved
- Works consistently across Unix platforms
- Avoids shell-specific behaviors that differ by OS

**Fallback for OTP < 25 (if needed):**

```erlang
%% Only if stderr_to_stdout unavailable
case erlang:system_info(otp_release) >= "25" of
  true ->
    open_port({spawn_executable, ClaudePath}, [..., stderr_to_stdout]);
  false ->
    %% Fallback: spawn shell with 2>&1
    %% Note: Less reliable, Windows unsupported
    Command = lists:flatten(io_lib:format("~s ~s 2>&1", [ClaudePath, ArgsStr])),
    open_port({spawn, Command}, [binary, {packet, 0}, exit_status, use_stdio])
end.
```

**Init timeout failure mode:**
- If CLI outputs "unknown flag" to stderr and exits, `stderr_to_stdout` ensures it's captured
- Without stderr capture, user sees `InitializationTimeout` with no diagnostic info
- The `StartError.InitializationTimeout` includes `captured_output` to surface any available error text

#### Native Port Message Shapes

Erlang ports send messages with these exact shapes (native Erlang, NOT wrapped):

| Message | Shape | When |
|---------|-------|------|
| Data received | `{Port, {data, Binary}}` | CLI writes to stdout |
| Process exited | `{Port, {exit_status, ExitCode}}` | CLI process terminates |
| Port closed | `{'EXIT', Port, Reason}` | Port closed (if linked) |

**Where `Port` is the port reference returned by `open_port/2`.**

#### SDK Message Handling Strategy

The SDK uses **native port messages directly** in the GenServer's `handle_info`. No normalization layer in FFI:

```erlang
%% claude_agent_sdk_ffi.erl exports:
%%   open_port/1 -> Port
%%   port_write/2 -> ok | {error, Reason}
%%   close_port/1 -> ok
%%
%% Does NOT wrap/translate port messages - GenServer matches native tuples
```

```gleam
// internal/bidir.gleam handle_info patterns:

fn handle_info(msg: Dynamic, state: State) -> actor.Next(State) {
  case decode_port_message(msg, state.port) {
    Ok(PortData(binary)) ->
      // Buffer and parse NDJSON lines
      let #(new_stream_state, lines) = stream.handle_port_data(state.stream, binary)
      // ... process lines
    Ok(PortExitStatus(exit_code)) ->
      // CLI process terminated
      // ... handle shutdown
    Error(_) ->
      // Unknown message, ignore (e.g., hook_done, hook_timeout, DOWN)
      // ... handle internal messages
  }
}

// Decode native Erlang port tuples
fn decode_port_message(msg: Dynamic, port: Port) -> Result(PortMessage, Nil) {
  // Match {Port, {data, Binary}}
  case dynamic.tuple2(dynamic.dynamic, dynamic.dynamic)(msg) {
    Ok(#(msg_port, payload)) if msg_port == port ->
      case dynamic.tuple2(atom, dynamic.bit_array)(payload) {
        Ok(#(data_atom, binary)) if data_atom == atom.create("data") ->
          Ok(PortData(binary))
        Ok(#(exit_status_atom, code)) if exit_status_atom == atom.create("exit_status") ->
          Ok(PortExitStatus(dynamic.int(code)))
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}
```

#### Port Ownership Invariant

**CRITICAL**: The port MUST be owned by the GenServer process.

```
✓ CORRECT: BidirRunner.start() called inside actor.init callback
           → open_port() runs in GenServer process
           → Port owned by GenServer
           → Port messages delivered to GenServer mailbox

✗ WRONG:  BidirRunner.start() called before actor.start()
          → open_port() runs in caller process
          → Port owned by caller
          → Port messages never reach GenServer
```

The SDK ensures correct ownership by:
1. `start_session()` passes `BidirRunnerFactory` (not a runner instance) to actor
2. `actor.init` callback calls the factory
3. Factory invokes `open_port` → Port owned by the actor process

**Task spawning safety**: Hook tasks are spawned with `spawn_link + monitor`. They do NOT inherit or interact with the port. Only the GenServer process reads from/writes to the port.

#### Protocol Test for Port Message Shapes

```gleam
// test/port_message_test.gleam

pub fn port_message_decode_test() {
  // Simulate exact message shapes from Erlang port
  let mock_port = create_mock_port()

  // Test data message
  let data_msg = #(mock_port, #(atom.create("data"), <<"hello\n">>))
  let result = decode_port_message(dynamic.from(data_msg), mock_port)
  should.equal(result, Ok(PortData(<<"hello\n">>)))

  // Test exit_status message
  let exit_msg = #(mock_port, #(atom.create("exit_status"), 0))
  let result2 = decode_port_message(dynamic.from(exit_msg), mock_port)
  should.equal(result2, Ok(PortExitStatus(0)))

  // Test non-matching port (shouldn't decode)
  let other_port = create_mock_port()
  let wrong_port_msg = #(other_port, #(atom.create("data"), <<"x">>))
  let result3 = decode_port_message(dynamic.from(wrong_port_msg), mock_port)
  should.equal(result3, Error(Nil))
}
```

### Hook Task Lifecycle and Supervision

Hook callbacks execute in spawned OTP tasks with explicit lifecycle management:

#### Task Spawn and Communication

```
GenServer (bidir.gleam)                          Spawned Task
        │                                              │
        │  spawn_link + monitor                        │
        ├─────────────────────────────────────────────►│
        │                                              │
        │  set timer: {hook_timeout, request_id, ref}  │
        ├──────────┐                                   │
        │          │                                   │ execute callback
        │          │                                   │
        │          │                         ┌─────────┤
        │          │                         │ (callback completes)
        │          ▼                         ▼
        │  ┌─────────────┐           {hook_done, request_id, result}
        │  │ Timer fires │◄──────────────────┼─────────┤
        │  │ (timeout)   │     OR            │         │
        │  └─────────────┘                   │         │
        │          │                         │         │
        ▼          ▼                         ▼         ▼
   ┌─────────────────────────────────────────────────────────┐
   │              handle_info receives FIRST event           │
   │  - If hook_done: cancel timer, send response, cleanup   │
   │  - If hook_timeout: send fail-open, kill task, cleanup  │
   │  - If DOWN (crash): send fail-open/deny, cleanup        │
   └─────────────────────────────────────────────────────────┘
```

#### First-Event-Wins Protocol

1. **Task completes normally**: GenServer receives `{hook_done, request_id, result}`
   - Cancel pending timer
   - Send control_response via Runner
   - Remove from `pending_hooks` map
   - Task exits naturally

2. **Timer fires first (timeout)**: GenServer receives `{hook_timeout, request_id, timer_ref}`
   - Check `pending_hooks` — if request_id not present, ignore (already handled)
   - Send fail-open response (hooks) or fail-deny response (permissions)
   - Kill task with `Process.kill(task_pid, Kill)`
   - Remove from `pending_hooks` map
   - Log warning: "Hook callback timed out after {timeout}ms for {callback_id}"

3. **Task crashes**: GenServer receives `{DOWN, monitor_ref, process, task_pid, reason}`
   - Look up request_id from monitor_ref mapping
   - Cancel pending timer
   - Send fail-open response (hooks) or fail-deny response (permissions)
   - Remove from `pending_hooks` map
   - Log error: "Hook callback crashed: {reason}"

#### State Cleanup Invariants

- `pending_hooks: Map(RequestId, PendingHook)` with TaskPid, MonitorRef, TimerRef
- Every entry has exactly one task, one monitor, one timer
- Cleanup always removes all three atomically
- Double-response impossible: first event removes entry, subsequent events find nothing

### Session Termination Cleanup

When the session transitions to STOPPED or FAILED state, explicit cleanup is required:

#### Termination Handler (GenServer `terminate/2` or explicit stop handler)

```
1. Cancel all pending hook timers:
   for each (request_id, pending_hook) in pending_hooks:
     erlang:cancel_timer(pending_hook.timer_ref)
     Process.demonitor(pending_hook.monitor_ref, [:flush])
     Process.kill(pending_hook.task_pid, :kill)

2. Resolve all pending SDK-initiated requests:
   for each (request_id, pending_request) in pending_requests:
     Subject.send(pending_request.reply_to, SessionStopped)

3. Clear state maps:
   pending_hooks = Map.new()
   pending_requests = Map.new()
   queued_ops = []

4. Close the runner:
   Runner.close(runner)

5. Notify via events() Subject:
   Subject.send(events_subject, SessionStopped)  // or SessionCompleted/SessionFailed

6. Transition lifecycle to STOPPED
```

#### Post-Termination Message Handling

After lifecycle = STOPPED, all incoming messages are dropped:
- `{runner_data, _}` → ignore (runner should be closed, but drain mailbox)
- `{hook_done, _, _}` → ignore (task already killed)
- `{hook_timeout, _}` → ignore (timer already canceled)
- `{DOWN, _, _, _, _}` → ignore (already demonitored with flush)

### SDK-Initiated Operation Timeout Strategy

SDK-initiated operations (initialize, interrupt, set_permission_mode, set_model, rewind_files) follow a timeout strategy similar to hooks:

#### Per-Request Timeout Management

```
SDK initiates operation (e.g., interrupt(session))
        │
        ▼
┌─────────────────────────────────────────────────────────────┐
│  1. Generate request_id: "req_<counter>"                    │
│  2. Create PendingRequest with:                             │
│     - kind: InterruptOp                                     │
│     - reply_to: caller's Subject                            │
│     - deadline: monotonic_time() + timeout_ms               │
│     - timer_ref: send_after(timeout_ms, {op_timeout, id})   │
│  3. Store in pending_requests[request_id]                   │
│  4. Send control_request to CLI                             │
└─────────────────────────────────────────────────────────────┘
        │
        ▼ (one of three outcomes)
┌─────────────────────────────────────────────────────────────┐
│  A. Response arrives:                                       │
│     - Look up pending_requests[request_id]                  │
│     - Cancel timer_ref                                      │
│     - Send result to reply_to Subject                       │
│     - Remove from pending_requests                          │
├─────────────────────────────────────────────────────────────┤
│  B. Timer fires (timeout):                                  │
│     - Look up pending_requests[request_id]                  │
│     - If not found: ignore (already handled)                │
│     - Send ControlError.Timeout(kind, request_id) to reply_to│
│     - Remove from pending_requests                          │
│     - Late responses will find no entry → log + drop        │
├─────────────────────────────────────────────────────────────┤
│  C. Session stops/port closes:                              │
│     - Iterate all pending_requests                          │
│     - Cancel all timers                                     │
│     - Send SessionStopped to all reply_to Subjects          │
│     - Clear pending_requests map                            │
└─────────────────────────────────────────────────────────────┘
```

#### Timeout Values

| Operation | Default Timeout | Rationale |
|-----------|-----------------|-----------|
| Initialize | 10,000 ms | CLI startup should be fast; longer may indicate version incompatibility |
| Interrupt | 5,000 ms | Should complete quickly |
| SetPermissionMode | 5,000 ms | Configuration change |
| SetModel | 5,000 ms | Configuration change |
| RewindFiles | 30,000 ms | May involve disk I/O for checkpoint restoration |

#### Late Response Handling (First-Event-Wins)

When a response arrives for a request_id not in `pending_requests`:
1. Log at DEBUG level: "Late response for {request_id}, discarding"
2. Drop the response (caller already received timeout error)
3. Do NOT crash or raise error

This ensures the system remains stable even with network delays or CLI processing spikes.

#### Error Types for SDK-Initiated Operations

```gleam
pub type ControlError {
  /// Operation timed out waiting for CLI response
  Timeout(kind: PendingRequestKind, request_id: String)
  /// CLI returned an error response
  CliError(kind: PendingRequestKind, message: String)
  /// Session was stopped before operation completed
  SessionStopped
  /// Too many concurrent operations (backpressure limit hit)
  TooManyPendingRequests
}
```

### Control vs Normal Message Routing

Control messages are **internal protocol machinery** and are NOT exposed to the public message stream:

| Message Type | Routing | User Visible? |
|--------------|---------|---------------|
| Regular messages (system, assistant, user, result) | Forward to subscriber via `messages()` Subject | Yes |
| `control_request` (CLI→SDK: hook/permission/MCP) | Dispatch to internal handler | No |
| `control_response` (CLI→SDK: operation response) | Correlate with pending request, signal caller | No |

**Rationale**: Users subscribe to `messages()` to get conversation content. Control protocol is implementation detail. Adding a `Message.Control` variant would break existing pattern matches and require filtering.

**Legacy `query()` behavior**: Returns `Iterator(Message)` containing only regular messages. No change from current behavior.

### Session Termination Notification

**How users detect session termination** without changing the `Message` type:

#### Mechanism: `events()` Subject (separate from `messages()`)

```gleam
/// Session events (separate from conversation messages)
pub type SessionEvent {
  /// Session completed normally (CLI exited with code 0)
  SessionCompleted
  /// Session stopped by user via stop(session)
  SessionStopped
  /// Session failed (CLI exited with non-zero code, crash, etc.)
  SessionFailed(reason: String, exit_code: Option(Int))
}

/// Get the session event stream (completion, failure, etc.)
pub fn events(session: Session) -> Subject(SessionEvent)

/// Or: combined messages + events via single Subject
pub fn subscribe(session: Session) -> Subject(SessionUpdate)

pub type SessionUpdate {
  Msg(Message)           // Regular conversation message
  Event(SessionEvent)    // Session lifecycle event
}
```

**Design choice**: The plan proposes **Option A** (separate `events()` Subject):
- `messages()` returns `Subject(Message)` — unchanged public type
- `events()` returns `Subject(SessionEvent)` — new, for lifecycle events
- Users who don't care about events can ignore `events()`
- No breaking changes to `Message` type

**Alternative (Option B)**: Combined `subscribe()` returning `Subject(SessionUpdate)`:
- Single subscription point
- Requires users to pattern match on `Msg` vs `Event`
- Slightly more complex but avoids managing two subscriptions

**Recommended**: Option A for simplicity and non-breaking change.

#### Termination Detection Without `events()`

For users who only use `messages()`:
- The `Subject` stops receiving messages when session ends
- Callers can use `process.selecting` with timeout to detect "no more messages"
- The `stop()` function returns immediately; completion is eventually consistent

This design:
- Keeps `Message` type unchanged
- Provides explicit shutdown notification via optional `events()` subscription
- Works with existing OTP selective receive patterns

### File Checkpointing Semantics

File checkpointing enables `rewind_files` to restore file state:

#### Checkpoint Creation

Checkpoints are created **by the CLI**, not the SDK. The SDK's role is to:
1. Enable checkpointing via `enable_file_checkpointing: true` in initialize request
2. Send `rewind_files` control request when user calls `rewind_files(session)`

**CLI behavior assumptions** (out of SDK scope, may vary by CLI version):

| Assumption | Source | Validation |
|------------|--------|------------|
| CLI creates checkpoints before file-modifying tools | Spec section 4.3 | Verified via integration test |
| Checkpoint covers Write, Edit, MultiEdit tools | Current CLI behavior | Best-effort; may change |
| `user_message_id` format is stable | Observed behavior | Validated in integration test |

**Note**: These assumptions are based on current CLI behavior and spec documentation. The SDK does not control or guarantee CLI checkpoint behavior. Integration tests verify these assumptions hold for supported CLI versions.

#### `rewind_files` Invariant

- **Requires**: `with_file_checkpointing()` was called on options
- **Behavior**: Sends `rewind_files` control request; actual file restoration is CLI responsibility
- **Parameter**: `user_message_id` — the message ID to rewind to (obtained from `UserMessage.uuid`)
- **Error**: If checkpointing not enabled, returns `CheckpointingNotEnabled` error
- **Error**: If `user_message_id` is invalid/not found, CLI returns error response

### PreToolUse Modification Semantics

Only `PreToolUse` hooks may modify tool input. Other hooks' return values are interpreted as follows:

| Hook Event | `ModifyInput(input)` Handling |
|------------|-------------------------------|
| `PreToolUse` | Valid — sends `hookSpecificOutput.updatedInput` |
| `PostToolUse` | Ignored — treated as `Continue` |
| `UserPromptSubmit` | Ignored — treated as `Continue` |
| `Stop` | Ignored — treated as `Continue` |
| `SubagentStop` | Ignored — treated as `Continue` |
| `PreCompact` | Ignored — treated as `Continue` |

**Encoding errors**: If `new_input` cannot be JSON-encoded:
- Log warning: "PreToolUse modified_input failed to encode: {error}"
- Fall back to `Continue` (fail-open)
- Do not send `updatedInput` field

### API/Interface Design

#### Public API (`claude_agent_sdk.gleam`)

**Blocking behavior note**: All control operations (`interrupt`, `set_permission_mode`, `set_model`, `rewind_files`) are **synchronous/blocking** — they send a control request to the CLI and wait for a response (or timeout). The caller's process blocks until one of: CLI response received, timeout expires, or session stops.

```gleam
/// Start a bidirectional session with Claude CLI.
/// Blocks up to 10s waiting for CLI initialization; returns Timeout on expiry.
pub fn start_session(
  prompt: String,
  options: QueryOptions,
) -> Result(Session, StartError)

/// Get the message stream from an active session.
/// Non-blocking; returns immediately with Subject for receiving messages.
pub fn messages(session: Session) -> Subject(Message)

/// Send interrupt signal to stop current operation.
/// Blocks up to 5s waiting for CLI acknowledgment; returns Timeout on expiry.
pub fn interrupt(session: Session) -> Result(Nil, ControlError)

/// Change permission mode during session.
/// Blocks up to 5s waiting for CLI acknowledgment; returns Timeout on expiry.
pub fn set_permission_mode(
  session: Session,
  mode: PermissionMode,
) -> Result(Nil, ControlError)

/// Change model during session.
/// Blocks up to 5s waiting for CLI acknowledgment; returns Timeout on expiry.
pub fn set_model(session: Session, model: String) -> Result(Nil, ControlError)

/// Rewind files to checkpoint at specified message.
/// Requires enable_file_checkpointing option.
/// Blocks up to 30s waiting for CLI acknowledgment; returns Timeout on expiry.
pub fn rewind_files(session: Session, user_message_id: String) -> Result(Nil, ControlError)

/// Gracefully stop the session.
/// Non-blocking; initiates shutdown and returns immediately.
/// Subscribe to events() Subject to receive SessionStopped notification.
pub fn stop(session: Session) -> Result(Nil, StopError)

/// Get session lifecycle events (completion, stop, failure).
/// Separate from messages() to avoid changing the Message type.
pub fn events(session: Session) -> Subject(SessionEvent)

/// Backwards-compatible query function (legacy unidirectional mode ONLY).
/// NOTE: Does NOT support bidir features (hooks, can_use_tool, mcp_servers).
/// Use start_session() for bidirectional features.
pub fn query(prompt: String, options: QueryOptions) -> Result(QueryStream, QueryError)
```

**Timeout behavior summary:**

| Function | Blocks? | Timeout | On Timeout |
|----------|---------|---------|------------|
| `start_session` | Yes | 10s | `StartError.Timeout` |
| `messages` | No | — | — |
| `events` | No | — | — |
| `interrupt` | Yes | 5s | `ControlError.Timeout` |
| `set_permission_mode` | Yes | 5s | `ControlError.Timeout` |
| `set_model` | Yes | 5s | `ControlError.Timeout` |
| `rewind_files` | Yes | 30s | `ControlError.Timeout` |
| `stop` | No | — | — |

#### query() vs start_session() Compatibility

| Feature | query() | start_session() |
|---------|---------|-----------------|
| Prompt execution | Yes | Yes |
| Message streaming | Pull-based Iterator | Push-based Subject |
| Hooks (PreToolUse, etc.) | **No** — ignored with warning | Yes |
| Permission callback | **No** — ignored with warning | Yes |
| MCP servers | **No** — ignored with warning | Yes |
| Control operations | Not available | interrupt, set_model, etc. |
| File checkpointing | **No** | Yes |

**query() behavior when bidir features are set:**
- Log warning: "query() ignores hooks/can_use_tool/mcp_servers. Use start_session() for bidirectional features."
- Proceed with legacy unidirectional mode (no `--input-format stream-json`)
- Return existing `QueryStream` iterator as before

#### query() Implementation with New Runner Model

The bidirectional mode requires a push-based runner, but we keep the public `Runner` API unchanged.

**Option chosen: Keep public Runner intact, add internal BidirRunner**

```gleam
// runner.gleam — PUBLIC API (UNCHANGED from existing)
// This file is NOT modified; existing Runner type preserved

/// Existing public Runner type — pull-based for query()
pub type Runner {
  Runner(
    read: fn() -> Result(String, ReadError),  // Blocking read
    close: fn() -> Nil,
  )
}
```

```gleam
// internal/bidir_runner.gleam — NEW INTERNAL type

/// Push-based runner for start_session() (bidir mode only)
/// This is INTERNAL — not exported from the public API
pub type BidirRunner {
  BidirRunner(
    port: Port,  // Exposed for GenServer to match on native messages
    write: fn(String) -> Result(Nil, WriteError),
    close: fn() -> Nil,
    // GenServer matches native: {Port, {data, Binary}}, {Port, {exit_status, Code}}
  )
}

/// Create a BidirRunner that wraps an Erlang port
/// Called inside GenServer init; port owned by the GenServer process
pub fn start(cli_args: List(String)) -> Result(BidirRunner, SpawnError)

/// For testing: create a mock BidirRunner
pub fn mock(
  on_write: fn(String) -> Result(Nil, WriteError),
  on_close: fn() -> Nil,
) -> BidirRunner
```

**query() path (unchanged):**
1. `query()` creates a `Runner` (existing implementation, unchanged)
2. `QueryStream` uses `Runner.read()` as before
3. **No breaking change** — public `Runner` API identical to before

**start_session() path (new):**
1. `start_session()` spawns GenServer actor
2. GenServer init creates `BidirRunner` internally (not exposed)
3. Actor receives port messages via handle_info
4. Users consume messages via `Subject(Message)`

**Rationale**: By keeping `Runner` unchanged and adding an internal `BidirRunner`, we:
- Avoid breaking existing test mocks that use `Runner`
- Keep the public API stable (no union type change)
- Allow internal implementation to evolve without affecting users
- Provide a testing seam via `with_bidir_runner_factory()` option for bidir tests

**Testing seam for start_session():**
```gleam
// options.gleam — additive change, does NOT affect query() users

/// For testing bidir mode: provide a factory for mock BidirRunner
/// Only used by start_session(); ignored by query()
pub fn with_bidir_runner_factory(
  options: QueryOptions,
  factory: fn() -> BidirRunner,
) -> QueryOptions
```

#### Options Builder Extensions (`options.gleam`)

```gleam
/// Register a PreToolUse hook callback
pub fn with_pre_tool_use(
  options: QueryOptions,
  callback: fn(PreToolUseContext) -> HookResult,
) -> QueryOptions

/// Register a PostToolUse hook callback
pub fn with_post_tool_use(
  options: QueryOptions,
  callback: fn(PostToolUseContext) -> HookResult,
) -> QueryOptions

/// Register a UserPromptSubmit hook callback
pub fn with_user_prompt_submit(
  options: QueryOptions,
  callback: fn(UserPromptSubmitContext) -> HookResult,
) -> QueryOptions

/// Register a Stop hook callback
pub fn with_stop(
  options: QueryOptions,
  callback: fn(StopContext) -> HookResult,
) -> QueryOptions

/// Register a SubagentStop hook callback
pub fn with_subagent_stop(
  options: QueryOptions,
  callback: fn(SubagentStopContext) -> HookResult,
) -> QueryOptions

/// Register a PreCompact hook callback
pub fn with_pre_compact(
  options: QueryOptions,
  callback: fn(PreCompactContext) -> HookResult,
) -> QueryOptions

/// Register permission callback for tool authorization
/// Uses CanUseToolContext which includes permission_suggestions and blocked_path
pub fn with_can_use_tool(
  options: QueryOptions,
  callback: fn(CanUseToolContext) -> PermissionResult,
) -> QueryOptions

/// Register an MCP server handler
pub fn with_mcp_server(
  options: QueryOptions,
  name: String,
  handler: fn(Dynamic) -> Dynamic,
) -> QueryOptions

/// Enable file checkpointing for rewind_files support
pub fn with_file_checkpointing(options: QueryOptions) -> QueryOptions

/// Set global timeout for all hooks (default 60s)
pub fn with_timeout(options: QueryOptions, timeout_ms: Int) -> QueryOptions

/// Set timeout for specific hook type (overrides global)
pub fn with_hook_timeout(
  options: QueryOptions,
  event: HookEvent,
  timeout_ms: Int,
) -> QueryOptions
```

### File Impact Summary

| Path | Status | Description |
|------|--------|-------------|
| `gleam.toml` | Exists | Add `gleam_otp` dependency (gleam_erlang already present) |
| `src/claude_agent_sdk.gleam` | Exists | Add start_session, interrupt, set_model, rewind_files; re-export new types |
| `src/claude_agent_sdk/options.gleam` | Exists | Add hook callbacks, can_use_tool, mcp_servers, file_checkpointing, timeout builders, with_bidir_runner_factory |
| `src/claude_agent_sdk/runner.gleam` | Exists | **NO CHANGES** — public Runner API preserved unchanged |
| `src/claude_agent_sdk/internal/bidir_runner.gleam` | **New** | Internal push-based BidirRunner for bidir mode (not public) |
| `src/claude_agent_sdk/internal/stream.gleam` | Exists | Add control message detection and routing |
| `src/claude_agent_sdk/internal/port_ffi.gleam` | Exists | Add port_write external function binding |
| `src/claude_agent_sdk_ffi.erl` | Exists | Add port_write/2 using port_command |
| `src/claude_agent_sdk/message.gleam` | Exists | No changes needed (control messages are internal, not exposed) |
| `src/claude_agent_sdk/internal/decoder.gleam` | Exists | Add control message type detection |
| `src/claude_agent_sdk/internal/cli.gleam` | Exists | Add --input-format stream-json flag |
| `src/claude_agent_sdk/control.gleam` | **New** | Control message types, request/response builders, JSON encoders |
| `src/claude_agent_sdk/hook.gleam` | **New** | Hook event types, context types, result types, HookConfig |
| `src/claude_agent_sdk/internal/bidir.gleam` | **New** | GenServer session: init, handle_call, handle_info, state management |
| `src/claude_agent_sdk/internal/control_decoder.gleam` | **New** | JSON decoders for control_request/control_response messages |
| `src/claude_agent_sdk/internal/request_tracker.gleam` | **New** | Request ID generation, pending request map, correlation logic |
| `test/bidir_test.gleam` | **New** | Unit tests for bidirectional session |
| `test/control_test.gleam` | **New** | Unit tests for control message encoding/decoding |
| `test/hook_test.gleam` | **New** | Unit tests for hook dispatch and timeout behavior |
| `test/support/mock_runner.gleam` | **New** | Mock implementation of bidirectional runner |

## Risks, Edge Cases & Breaking Changes

### Edge Cases & Failure Modes

- **Hook callback throws exception**: Catch exception, log error, return fail-open response (`continue: true` for hooks). For permission callbacks, return fail-deny (`behavior: deny`) with logged error.
- **Hook callback times out (>60s default)**: Return fail-open response after timeout. Log warning with hook type and duration.
- **CLI sends malformed control_request**: Log parse error, send error response with request_id if extractable, otherwise ignore.
- **Request ID mismatch on response (protocol desync)**: Log warning, do not crash. Orphaned responses are dropped.
- **Port closes unexpectedly**: GenServer terminates, subscriber receives session_closed message.
- **CLI version incompatibility**: Detect during initialize handshake; if response indicates unsupported, fail fast with `UnsupportedCliVersion` error.
- **MCP handler throws exception**: Catch, return JSON-RPC error response to CLI.
- **rewind_files called without checkpointing enabled**: Return error `CheckpointingNotEnabled`.
- **Multiple concurrent control operations**: GenServer serializes via handle_call; safe but operations queue.
- **Malformed JSON on read**: Buffer until newline; if parse fails, log and skip line (robustness).

### Backwards Compatibility

- Existing `query()` API preserved with **unchanged implementation** (uses `PullRunner`, NOT `start_session`)
- `query()` and `start_session()` use **separate code paths**: query() → PullRunner (blocking reads), start_session() → PushRunner (message-based)
- All new types are additive (new option builders, new `control.gleam` and `hook.gleam` modules)
- Existing code using `query()` continues to work unchanged
- New options are all optional with sensible defaults
- `Message` type unchanged (control messages are internal, not exposed to public API)

### Breaking Changes

- **Potential Breaking Changes**:
  - `QueryOptions` internal structure changes (fields added) — **not breaking** (opaque type, builder pattern)
  - New public API `start_session()` returning a Session handle — **additive, not breaking**

- **NO BREAKING CHANGES to existing APIs**:
  - `Runner` type is **NOT modified** — existing test mocks continue to work
  - `query()` function behavior unchanged
  - `Message` type unchanged (control messages are internal)

- **API Visibility Summary**:
  - **PUBLIC/stable**: `claude_agent_sdk.gleam`, `options.gleam`, `runner.gleam`, `message.gleam`, `control.gleam`, `hook.gleam`
  - **INTERNAL/unstable**: All `internal/*` modules (including new `internal/bidir_runner.gleam`), `claude_agent_sdk_ffi.erl`

- **Additive Changes Only**:
  - New `start_session()` function
  - New option builders (`with_pre_tool_use()`, `with_can_use_tool()`, etc.)
  - New internal `BidirRunner` type (not exposed in public API)
  - New `with_bidir_runner_factory()` option for testing bidir mode

- **Versioning**:
  - This release is a **patch or minor bump** (no breaking changes)
  - Version bump: 0.x.y → 0.x.(y+1) for patch, or 0.x.y → 0.(x+1).0 for minor (new features)

#### Migration Notes

**For existing users**: No migration required. All existing code continues to work unchanged.

**For users who want to test `start_session()` (bidir mode)**:

```gleam
// Use with_bidir_runner_factory to inject a mock BidirRunner
import claude_agent_sdk/internal/bidir_runner.{BidirRunner}

let mock_runner = bidir_runner.mock(
  on_write: fn(data) {
    // Capture written data for test assertions
    process.send(test_collector, Written(data))
    Ok(Nil)
  },
  on_close: fn() { process.send(test_collector, Closed) },
)

let options = default_options()
  |> with_bidir_runner_factory(fn() { mock_runner })
  |> with_pre_tool_use(my_hook)

let session = start_session("prompt", options)

// Simulate CLI responses by sending directly to the session actor:
// (test helper function provided)
test_helpers.inject_cli_message(session, "{\"type\":\"system\"}")
```

**For users pattern-matching on Message**:

No changes required. `Message` type remains: `System | Assistant | User | Result`. Control messages are handled internally and never exposed to the public stream.

## Testing & Validation Strategy

### Unit Tests
- `control_test.gleam`: Control message encoding/decoding round-trips for all variants
- `hook_test.gleam`: Hook dispatch to correct callback, timeout handling (fail-open), exception catching
- `request_tracker_test.gleam`: Request ID generation uniqueness, correlation accuracy
- `control_decoder_test.gleam`: Verify all control message types parse correctly

### Integration Tests (Mock Runner)
- `bidir_test.gleam`: GenServer state transitions, message routing with mock runner
- **Handshake**: Verify SDK sends `initialize` with correct hooks and mcp_servers
- **Hook Flow**: Simulate `hook` request from CLI → Verify user callback invoked → Verify `hook_response` sent back
- **Permission Flow**: Simulate `permission` request → Verify `can_use_tool` → Verify response
- **MCP Flow**: Simulate MCP message → Verify routing to handler → Verify response
- **Concurrency**: Send multiple interleaved messages to verify GenServer state handling

### End-to-End Tests (Real CLI)
- `integration_test.gleam` (opt-in when CLI available):
  - Full session lifecycle with real Claude CLI
  - Initialize handshake succeeds with hook registration
  - Hook callbacks receive correct context and responses reach CLI
  - Control operations (interrupt, set_model) execute successfully
  - MCP message routing to handler and response back

### Regression Tests
- Existing `query()` API continues to work (wraps new session API)
- All existing message types parse correctly
- Existing option builders compose with new builders

### Manual Verification
- Run sample application with all hook types registered
- Verify interrupt stops long-running tool execution
- Test permission callback denying specific tools
- Verify MCP server message handling end-to-end

### Monitoring / Observability
- Log hook invocations with timing (debug level)
- Log hook timeouts and exceptions (warning level)
- Log control operation failures (error level)
- Consider adding optional telemetry callback for production monitoring

### Acceptance Criteria Coverage

| Spec AC | Covered By |
|---------|------------|
| AC #1: Bidirectional mode via `--input-format stream-json` | CLI argument builder in cli.gleam, integration tests |
| AC #2: NDJSON framing both directions | Stream layer extension, control encoder, port_ffi tests |
| AC #3: Request ID correlation | request_tracker module, unit + integration tests |
| AC #4: Initialize handshake registers hooks | bidir.gleam init, integration test verifies hooks active |
| AC #5: Hook callbacks invoke user functions | Hook dispatch in GenServer, hook_test.gleam |
| AC #6: Permission callbacks consult can_use_tool | CanUseTool handler, permission test cases |
| AC #7: SDK MCP servers route to handlers | MCP routing in GenServer, MCP integration test |
| AC #8: Control operations send control_request | Public API functions (interrupt, etc.), integration tests |
| AC #9: Timeouts default 60s, configurable per-hook | HookConfig with defaults, timeout test |
| AC #10: Hook errors fail-open | Exception handler returns continue:true, test verifies |
| AC #11: Permission errors fail-deny | Exception handler returns deny, test verifies |

## Documentation Updates

This release adds significant new functionality. The following documentation must be created/updated:

### README Updates

Add a new section "Bidirectional Mode (Hooks, Permissions, MCP)" with:

```markdown
## Bidirectional Mode

For advanced use cases requiring hooks, permissions, or SDK-hosted MCP servers, use `start_session()`:

```gleam
import claude_agent_sdk
import claude_agent_sdk/options
import claude_agent_sdk/hook

// Register a PreToolUse hook to intercept tool calls
let opts = options.default()
  |> options.with_pre_tool_use(fn(ctx) {
    case ctx.tool_name {
      "Bash" -> hook.Block("Bash commands require approval")
      _ -> hook.Continue
    }
  })
  |> options.with_can_use_tool(fn(ctx) {
    // Programmatic permission control
    permission.Allow
  })

// Start a bidirectional session
let assert Ok(session) = claude_agent_sdk.start_session("Hello", opts)

// Subscribe to messages
let messages = claude_agent_sdk.messages(session)

// Control operations available
claude_agent_sdk.interrupt(session)
claude_agent_sdk.set_model(session, "sonnet")
```

**Note:** The existing `query()` function remains available for simple use cases that don't need hooks or permissions. It uses legacy unidirectional mode and ignores any bidir-specific options.

### Key Differences: query() vs start_session()

| Feature | `query()` | `start_session()` |
|---------|-----------|-------------------|
| Message consumption | Pull-based Iterator | Push-based Subject |
| Hooks | Not supported | Supported |
| Permission callbacks | Not supported | Supported |
| MCP servers | Not supported | Supported |
| Control operations | Not available | interrupt, set_model, etc. |

For most simple prompts, `query()` is sufficient. Use `start_session()` when you need fine-grained control over Claude's behavior.
```

### CHANGELOG Entry

```markdown
## [0.x.0] - YYYY-MM-DD

### Added
- **Bidirectional mode**: New `start_session()` function for hooks, permissions, and MCP
- Hook callbacks: `with_pre_tool_use`, `with_post_tool_use`, `with_user_prompt_submit`, `with_stop`, `with_subagent_stop`, `with_pre_compact`
- Permission callback: `with_can_use_tool` for programmatic tool authorization
- MCP server support: `with_mcp_server` for SDK-hosted MCP servers
- Control operations: `interrupt()`, `set_permission_mode()`, `set_model()`, `rewind_files()`
- File checkpointing: `with_file_checkpointing()` option

### Unchanged
- `query()` function continues to work as before (legacy unidirectional mode)
- Public `Runner` type unchanged — existing test mocks continue to work
```

## Open Questions

1. **Minimum CLI version**: What is the earliest Claude CLI version supporting `--input-format stream-json`? Need to document this as a requirement and add version detection. — Affects prerequisites and error handling.

*Resolved questions (addressed in this plan):*
- ~~Initialize response format~~: Defined in "Initialization Handshake Semantics" section (explicit ack, implicit via hook/message, or 10s timeout).
- ~~MCP error response format~~: Defined in "Control Message Wire Format" section (JSON-RPC error with code -32603).
- ~~Request ID format~~: Simplified to `req_<counter>` — unique within session by definition.

## Next Steps

After this plan is approved, run `/create-tasks` to generate:
- `--beads` → Beads issues with dependencies for multi-agent execution
- (default) → TODO.md checklist for simpler tracking
