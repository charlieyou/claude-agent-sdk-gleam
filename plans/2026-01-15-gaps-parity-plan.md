# Gaps Parity Plan (Gleam SDK vs Python SDK)

Date: 2026-01-15
Owner: TBD
Status: Draft

## Context & Goals

Bring `claude-agent-sdk-gleam` to feature parity with `claude-agent-sdk-python` for
bidirectional sessions, control operations, MCP in-process servers, and key
configuration options while maintaining Gleam idioms and safety contracts.

### Gap Inventory

**Bidirectional public API (critical)**
- `start_session_new`, `start_session` are placeholders returning `SpawnFailed`.
- Control ops are placeholders returning `ControlNotImplemented`.
- `stop` returns `StopNotImplemented`.

**MCP SDK servers (critical)**
- `McpMessage` control request is ignored in both `Running` and `InitSent` paths.

**Options parity (high)**
Python-only options currently missing or not surfaced in Gleam:
- CLI path override and bundled CLI handling parity
- `fallback_model`, `betas`, `permission_prompt_tool_name`
- `settings`, `add_dirs`, `env`, `extra_args`
- `include_partial_messages`, `fork_session`, `agents`, `setting_sources`
- `sandbox`, `plugins`, `max_thinking_tokens`, `output_format`
- `enable_file_checkpointing` is present in Gleam `BidirOptions` but public API
  does not reach bidir start yet

## Scope

**In scope:**
- Public bidirectional session API implementation and wiring to internal actor
- Control operations (interrupt, set_permission_mode, set_model, rewind_files, stop)
- MCP SDK server message handling for in-process servers
- Option surface parity for CLI/config features supported in Python
- Documentation and examples for new features
- Tests and E2E validation for new capabilities

**Non-Goals:**
- Re-architecture of internal actor beyond what parity requires
- New CLI features not present in the Python SDK
- Breaking changes to existing Gleam query/stream APIs

## Assumptions & Constraints

### CLI Protocol Compatibility

**Supported CLI versions:** Minimum Claude CLI version that supports bidirectional
sessions and the control operations defined in the Python SDK (version TBD based on
Python SDK requirements).

**Capability detection:** The Gleam SDK assumes the CLI supports all bidir features
when `--output-format stream-json` is accepted. No feature-by-feature capability
negotiation is performed.

**Downgrade behavior:**
- If CLI lacks required flags: hard error at session start with descriptive message
- Unrecognized options in older CLI: passed through (CLI ignores unknown flags)
- Missing control op support: runtime error on control op call, not session start

### MCP Protocol

- MCP handler functions are user-provided callbacks conforming to a defined interface
- The SDK does not implement MCP protocol parsing; it routes raw JSON messages
- Handler timeouts and errors are the responsibility of the handler implementation

## Prerequisites

- Access to Python SDK source for behavior reference
- Working internal bidir actor (`internal/bidir_runner`)
- Test infrastructure for spawning mock CLI processes

## High-Level Approach

### Phase 1: Public bidirectional API wiring
1. Implement `start_session_new` and `start_session` to call internal bidir actor
2. Implement control ops with defined request/response contract (see Technical Design)
3. Map errors to `error.gleam` types with documentation
4. Add API docs for process ownership, timeouts, and lifecycles

### Phase 2: MCP SDK server handling
1. Implement `McpMessage` routing with defined lifecycle (see Technical Design)
2. Define request/response envelope matching Python semantics
3. Add test fixtures for MCP message flows

### Phase 3: Option surface parity
1. Extend `CliOptions` with missing CLI flags
2. Extend `BidirOptions` with advanced session config
3. Implement option precedence rules (see Technical Design)
4. Update docs and tests

### Phase 4: Documentation and examples
1. Add bidirectional usage guide
2. Add MCP in-process server example
3. Update options docs with precedence rules

### Phase 5: Tests and E2E
1. Unit tests for control ops and option serialization
2. Integration tests for bidir session start/stop flows
3. MCP handler tests (success/error/timeouts)
4. E2E tests for bidir session, hooks, permissions, and MCP servers

## Technical Design

### Control Plane Contract

Control operations use a request/response pattern with the following semantics:

**Request Correlation:**
- Each control request is assigned a monotonically increasing `request_id: Int`
- The bidir actor maintains a `pending_requests: Map(Int, Subject(Result))` for routing
- Responses are matched by `request_id` extracted from CLI JSON output

**Timeouts:**
- Default timeout: 30 seconds per control operation
- Configurable via `ControlOptions { timeout_ms: Option(Int) }`
- On timeout: return `Error(ControlTimeout(op_name, request_id))`

**Cancellation Semantics:**
- Control ops are fire-and-forget at CLI level (no CLI-side cancellation)
- Caller cancellation: remove from pending map, let CLI response be discarded
- Session stop implicitly cancels all pending ops (they receive `SessionStopped` error)

**Error Mapping:**
| Condition | Error Type |
|-----------|------------|
| Timeout waiting for response | `ControlTimeout(op, request_id)` |
| Transport failure (pipe broken) | `TransportError(reason)` |
| CLI rejection (invalid op) | `ControlRejected(op, cli_message)` |
| Session stopped during op | `SessionStopped` |

**Stop Guarantees:**
- `stop` sends SIGTERM to CLI process and waits up to 5 seconds for exit
- If CLI does not exit, SIGKILL is sent
- `stop` returns after process termination confirmed (best-effort then forced)
- All pending control ops receive `SessionStopped` error after stop completes

**Idempotency:**
- `stop` is idempotent: multiple calls return `Ok(())` if already stopped
- Other control ops are not idempotent; duplicate calls may have effects

### MCP Message Lifecycle

**State Machine Rules:**
```
InitSent:
  - McpMessage arrives → queue in bounded buffer (max 100 messages)
  - Buffer overflow → drop oldest, log warning
  - Init completes → flush queued messages to Running state

Running:
  - McpMessage arrives → route immediately to handler
  - Handler not registered for server → return error response to CLI

Stopped:
  - McpMessage arrives → discard silently (session ending)
```

**Request/Response Correlation:**
- MCP messages include `id` field per JSON-RPC spec
- SDK extracts `id` from request, passes to handler, expects `id` in response
- Handler returns `McpResponse { id: String, result: Option(Json), error: Option(McpError) }`
- SDK serializes response and writes to CLI stdin

**Backpressure Policy:**
- Queue size: 100 messages during `InitSent` phase
- Overflow behavior: drop oldest message, emit warning via `on_warning` callback
- Running phase: no queuing, synchronous dispatch to handler
- Handler blocking: SDK does not enforce handler timeout; handler is responsible

**Error/Timeout Handling:**
- Handler crash: return JSON-RPC error response with code `-32603` (internal error)
- Handler returns error: forward as JSON-RPC error response
- No timeout enforcement by SDK; handlers should implement their own timeouts

### Option Precedence Rules

Options are resolved with the following precedence (highest to lowest):

| Priority | Source | Example |
|----------|--------|---------|
| 1 (highest) | Explicit function arguments | `start_session(model: Some("opus"))` |
| 2 | Environment variables | `CLAUDE_MODEL=opus` |
| 3 | Config file settings | `settings.model` in config |
| 4 (lowest) | SDK defaults | Defined in `CliOptions.default()` |

**Merge Rules for Composite Options:**

| Option | Merge Behavior |
|--------|----------------|
| `env: Dict(String, String)` | Deep merge; explicit args override env vars override config |
| `settings: Dict(String, Json)` | Deep merge; same precedence as `env` |
| `add_dirs: List(String)` | Append; all sources concatenated (no deduplication) |
| `extra_args: List(String)` | Append; all sources concatenated |
| `betas: List(String)` | Replace; highest priority source wins entirely |
| `agents: List(AgentConfig)` | Replace; highest priority source wins entirely |

**Alignment Note:** These rules match Python SDK behavior. Any divergence discovered
during implementation should be raised as an issue.

### File Impact Summary

**Modified files:**
- `src/claude_agent_sdk.gleam` - Public API functions for bidir sessions
- `src/internal/bidir_runner.gleam` - Control op routing, MCP message handling
- `src/internal/bidir_types.gleam` - New control request/response types
- `src/error.gleam` - New error variants for control ops
- `src/options.gleam` - Extended CliOptions and BidirOptions

**New files:**
- `src/internal/mcp_router.gleam` - MCP message dispatch and correlation
- `src/internal/control_ops.gleam` - Control operation implementations
- `test/bidir_control_test.gleam` - Unit tests for control ops
- `test/mcp_router_test.gleam` - Unit tests for MCP routing

## Risks, Edge Cases & Breaking Changes

**Risks:**
- CLI protocol compatibility requirements for new options
- Semantics of MCP server handler responses in Gleam vs Python (may need adjustment)
- Handling of partial messages in Gleam stream model

**Edge Cases:**
- Control op timeout during session shutdown race
- MCP message arrives after session stop initiated but before process exits
- Handler panics or never returns

**Breaking Changes:**
- None expected; all new APIs are additive

## Testing & Validation

| Risk Area | Test Strategy |
|-----------|---------------|
| Control op correlation | Unit test with mock responses, verify correct routing by request_id |
| Control op timeouts | Unit test with delayed/missing responses, verify timeout error |
| MCP queue overflow | Unit test InitSent with >100 messages, verify drop + warning |
| MCP handler errors | Unit test handler crash/error, verify JSON-RPC error response |
| Option precedence | Unit test each precedence level, verify merge behavior |
| Session stop semantics | Integration test stop during pending ops, verify error delivery |

## Acceptance Criteria

### Session Start/Events
- **Given** valid options and available CLI
- **When** `start_session_new` is called
- **Then** session starts, emits events via callback, and transitions to Running state

### Control Operations
- **Given** a Running session
- **When** `interrupt` is called
- **Then** CLI receives interrupt request and SDK receives acknowledgment within timeout
- **When** `set_model("opus")` is called
- **Then** subsequent messages use the new model

### Control Operation Timeout
- **Given** a Running session
- **When** control op is called and CLI does not respond within 30s
- **Then** SDK returns `ControlTimeout` error and op is removed from pending

### Control Operation During Stop
- **Given** a Running session with pending control op
- **When** `stop` is called
- **Then** pending op receives `SessionStopped` error, session terminates

### MCP Request/Response
- **Given** a Running session with registered MCP handler for server "test"
- **When** CLI sends MCP request for server "test" with id "123"
- **Then** handler receives request, SDK sends response with id "123" to CLI

### MCP Handler Error
- **Given** a Running session with MCP handler that returns error
- **When** CLI sends MCP request
- **Then** SDK sends JSON-RPC error response to CLI

### Option Precedence
- **Given** model set in config file as "sonnet" and function arg as "opus"
- **When** session starts
- **Then** CLI receives `--model opus` (function arg wins)

### Option Merging (env)
- **Given** env var `FOO=1` in config and `BAR=2` in function arg
- **When** session starts
- **Then** CLI environment contains both `FOO=1` and `BAR=2`

## Open Questions

- Exact minimum CLI version required for full bidir support
- Whether MCP handler timeout should be enforced by SDK (current: no)
- Whether `add_dirs` should deduplicate paths (current: no, matches Python)

## Suggested Sequencing

1. Phase 1 (public bidir API + control plane contract)
2. Phase 2 (MCP handler + lifecycle)
3. Phase 3 (options parity + precedence)
4. Phase 4 (docs/examples)
5. Phase 5 (tests/E2E)
