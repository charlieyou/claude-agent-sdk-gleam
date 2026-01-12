# E2E SDK Test Plan

Date: 2026-01-12

## Context & Goals

Replace CLI-focused E2E tests with comprehensive SDK API tests. The current tests validate raw `claude` CLI behavior by shelling out directly—we need to test the **Gleam SDK wrapper** itself to verify that our abstractions work correctly end-to-end.

**Goals:**
1. Validate SDK stream consumption APIs (`query()`, `next()`, `close()`)
2. Verify message type decoding from real Claude CLI output
3. Test all `QueryOptions` builder functions
4. Test bidirectional session features (hooks, control messages) via `bidir.start_with_hooks()`
5. Confirm graceful error handling for expected failure modes

---

## Scope & Non-Goals

### In Scope
- SDK public API correctness (`query()`, stream iteration, options builders)
- Message parsing from real CLI streams (SystemMessage, AssistantMessage, ResultMessage, etc.)
- Bidirectional protocol via `bidir.start_with_hooks()` for hook callbacks
- MCP server configuration passthrough
- Error handling for CLI-not-found, auth failures, timeouts, malformed streams
- Protocol invariant assertions (message ordering, required fields, stream termination)

### Non-Goals (Explicitly Excluded)
- **Semantic content quality**: We do not assert on the meaning/style of Claude's responses
- **Model-specific token costs**: We do not validate specific cost values
- **CLI internal behavior**: We test what the SDK sees, not how the CLI works internally
- **Performance benchmarking**: Focus on correctness, not latency/throughput metrics
- **Comprehensive MCP server testing**: We verify passthrough, not MCP protocol details

---

## Assumptions & Constraints

### Assumptions
1. Claude CLI is installed and accessible in PATH for E2E tests
2. `ANTHROPIC_API_KEY` environment variable or authenticated CLI session is available
3. The SDK spawns the CLI as a subprocess—there is no direct HTTP API path
4. Hook callbacks require bidirectional sessions (`bidir.start_with_hooks()`), not `query()`
5. MCP servers are configured via `--mcp-config` pointing to a JSON file

### Constraints
- E2E tests incur real API costs; must be gated by `E2E_SDK_TEST=1`
- Live model responses are non-deterministic; assertions must use protocol invariants
- CI environment may have restricted file/network access
- Tests that modify global environment (PATH, env vars) must run serially or use isolation

---

## Prerequisites

- `ANTHROPIC_API_KEY` set or authenticated CLI session (via `claude auth`)
- Claude CLI installed (SDK spawns it as subprocess)
- `E2E_SDK_TEST=1` environment variable to enable (avoids accidental API costs)
- For MCP tests: `test/fixtures/mcp-echo-server.json` pointing to the echo MCP server

---

## Execution Model

The Gleam SDK does **not** make HTTP API calls directly. It spawns the `claude` CLI as a subprocess and communicates via:
- **stdout**: JSON-line messages (streaming responses)
- **stdin**: Control messages for bidirectional protocol (hooks, permission responses, interrupts)

### Implications for Testing

1. **Cannot fully remove CLI dependency**: E2E tests inherently exercise the CLI subprocess path
2. **Error injection requires test seams**: Malformed JSON, timeouts, and stream drops require either:
   - Mock runner injection via `with_test_mode(runner)` for offline/deterministic tests
   - Acceptance that some error paths are only testable in unit tests
3. **CLI-not-found and auth-failure tests**: Require per-test environment isolation or serial execution

### Test Seam Design

For fault injection tests (SDK-62, SDK-63, SDK-64), we use the SDK's `test_runner` abstraction:
```gleam
let mock_runner = claude_agent_sdk.test_runner(
  on_spawn: fn(_, _, _) { Ok(handle) },
  on_read: fn(_) { runner.Data("{\"malformed json") },  // Simulate error
  on_close: fn(_) { Nil },
)
let opts = default_options() |> with_test_mode(mock_runner)
```

This enables offline, deterministic testing of error handling without depending on real CLI behavior.

---

## High-Level Approach

### Test Categories

| Category | API | Description |
|----------|-----|-------------|
| 1. Stream API | `query()` | Core streaming query lifecycle |
| 2. Message Parsing | `query()` | All message types decode correctly |
| 3. Query Options | `query()` | All `QueryOptions` builders work |
| 4. Hooks | `bidir.start_with_hooks()` | Hook registration and dispatch |
| 5. Bidirectional Protocol | `bidir.*` | Control messages, interrupts |
| 6. MCP Integration | `query()` | MCP server config passthrough |
| 7. Error Handling | Both | Graceful failures |

### Migration Strategy

1. **Implement new SDK tests first** (SDK-01 through SDK-64)
2. **Verify new tests pass** in CI with `E2E_SDK_TEST=1`
3. **Delete old CLI tests** (E2E-01 through E2E-08) after new coverage confirmed
4. **Remove CLI shelling helpers** (`run_command`, etc.)

---

## Technical Design

### API Usage Patterns

#### Simple Streaming Query
```gleam
import claude_agent_sdk

let opts = claude_agent_sdk.default_options()
  |> claude_agent_sdk.with_max_turns(1)

case claude_agent_sdk.query("Say hello", opts) {
  Ok(stream) -> {
    // Iterate with next() until stream ends
    case claude_agent_sdk.next(stream) {
      Ok(item) -> // process item
      Error(err) -> // handle stream error
    }
    claude_agent_sdk.close(stream)
  }
  Error(err) -> // handle query error
}
```

#### Bidirectional Session with Hooks
```gleam
import claude_agent_sdk/internal/bidir
import gleam/dict

// Define hook handlers
let hook_config = bidir.HookConfig(
  handlers: dict.from_list([
    #("PreToolUse", fn(input) {
      // Return HookExecutionResult: Continue, Block(reason), or ModifyInput(new)
      dynamic.from(dict.from_list([#("continue", True)]))
    }),
  ]),
  permission_handlers: dict.new(),
)

// Start bidirectional session
let subscriber = process.new_subject()
let config = bidir.default_config(subscriber)
case bidir.start_with_hooks(runner, config, hook_config) {
  Ok(session) -> {
    // Hooks are dispatched automatically when CLI sends hook_callback
    // Session messages arrive on subscriber subject
  }
  Error(err) -> // handle start error
}
```

### Hook Response Types

The SDK uses these result types (from `claude_agent_sdk/hook.gleam`):

```gleam
// For hook callbacks (PreToolUse, PostToolUse, etc.)
pub type HookExecutionResult {
  Continue                    // Allow operation to proceed
  Block(reason: String)       // Block with reason
  ModifyInput(new_input: Dynamic)  // Modify input before proceeding
}

// For permission checks (can_use_tool)
pub type PermissionCheckResult {
  Allow                       // Allow the operation
  Deny(reason: String)        // Deny with reason
}
```

### File Impact Summary

| File | Change |
|------|--------|
| `test/e2e/run_e2e.gleam` | Replace all scenarios with SDK-based tests |
| `test/e2e/helpers.gleam` | Remove `run_command`, add stream consumption helpers |
| `test/fixtures/mcp-echo-server.json` | New: MCP config for SDK-50/51 |

---

## Scenario Definitions

### Category 1: Stream API (using `query()`)

#### SDK-01: Basic Query
- **What**: Call `query(prompt, options)` and consume stream to completion
- **How**:
  ```gleam
  let opts = claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_max_turns(1)
  case claude_agent_sdk.query("Say hello", opts) {
    Ok(stream) -> // consume with next() until Done
    Error(_) -> // fail test
  }
  ```
- **Assert** (protocol invariants):
  - Returns `Ok(QueryStream)`
  - Stream yields at least one item before termination
  - Stream eventually terminates (not hung)
  - `close()` succeeds without error

#### SDK-02: Stream Iteration Pattern
- **What**: Verify `next()` iteration pattern works correctly
- **How**: Loop calling `next()` and collecting items until terminal error
- **Assert**:
  - `next()` returns `Ok(StreamItem)` for each message
  - Stream ends with terminal `StreamError` (e.g., `StreamClosed`)
  - Items received in protocol order (SystemMessage first if present)

#### SDK-03: Multi-turn Conversation
- **What**: Allow multiple turns and verify conversation progresses
- **How**: Set `with_max_turns(3)`, send prompt that encourages tool use
- **Assert**:
  - Multiple `AssistantMessage` items possible (if model uses tools)
  - Stream eventually terminates within max_turns

#### SDK-04: Session Resume via SDK
- **What**: Extract `session_id` from first query, resume with SDK API
- **How**:
  ```gleam
  let opts1 = claude_agent_sdk.default_options()
  // first query, extract session_id from SystemMessage
  let opts2 = claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_resume(session_id)
  // second query continues context
  ```
- **Assert**:
  - Second query starts successfully
  - Session ID in second SystemMessage matches (or is valid)

---

### Category 2: Message Parsing (using `query()`)

#### SDK-10: SystemMessage Parsing
- **What**: Verify `SystemMessage` fields decode correctly
- **Assert**:
  - `session_id` present and non-empty string
  - `tools` list present (possibly empty in restricted mode)
  - `mcp_servers` list present (possibly empty)

#### SDK-11: AssistantMessage Content Blocks
- **What**: Verify content block types parse without error
- **Assert**:
  - `TextBlock` can be decoded when present
  - `ToolUseBlock` decodes with id, name, input when tool use occurs
  - No decoder crashes on valid content

#### SDK-12: ToolResultBlock Round-trip
- **What**: Prompt that triggers tool use, verify result block parsing
- **Assert**:
  - If `ToolUseBlock` received, subsequent message may contain `ToolResultBlock`
  - Tool result content decodes without error

#### SDK-13: UsageData Accuracy
- **What**: Verify token counts in `ResultMessage`
- **Assert**:
  - `input_tokens >= 0`
  - `output_tokens >= 0`
  - Fields parse as integers without error

#### SDK-14: ErrorMessage Handling
- **What**: Trigger an error condition, verify SDK surfaces it
- **How**: Use mock runner with error response, or invalid auth
- **Assert**:
  - Error is returned (not a crash)
  - Error contains diagnostic information

---

### Category 3: Query Options (using `query()`)

#### SDK-20: Model Selection
- **What**: `with_model("claude-sonnet-4-20250514")` is passed to CLI
- **Assert**: Query executes (doesn't fail with "invalid model" unless model actually invalid)

#### SDK-21: Max Turns Enforcement
- **What**: `with_max_turns(1)` limits conversation
- **Assert**: Stream terminates (may be after tool use cycle, but eventually stops)

#### SDK-22: Max Budget Enforcement
- **What**: `with_max_budget(0.01)` stops when budget exceeded
- **Assert**: Query terminates; if budget exceeded, result indicates budget stop

#### SDK-23: System Prompt
- **What**: `with_system_prompt("...")` is passed to CLI
- **Assert**: Query executes without error (no semantic check on response)

#### SDK-24: Allowed Tools Filter
- **What**: `with_allowed_tools(["Read"])` restricts tools
- **Assert**: `SystemMessage.tools` only contains allowed tools (when tools listed)

#### SDK-25: Disallowed Tools Filter
- **What**: `with_disallowed_tools(["Bash"])` excludes tools
- **Assert**: `SystemMessage.tools` does not contain disallowed tools (when tools listed)

#### SDK-26: Permission Mode - Default
- **What**: Default permission mode behavior
- **Assert**: Query executes (permission prompts may occur server-side)

#### SDK-27: Permission Mode - AcceptEdits
- **What**: `with_permission_mode(AcceptEdits)` option works
- **Assert**: Query executes without error

#### SDK-28: Permission Mode - BypassPermissions
- **What**: `with_permission_mode(BypassPermissions)` option works
- **Assert**: Query executes without error

---

### Category 4: Hooks (using `bidir.start_with_hooks()`)

**Note**: Hooks require bidirectional sessions, not simple `query()`.

#### SDK-30: PreToolUse Hook Registration
- **What**: Register `PreToolUse` hook via `HookConfig`, verify it fires
- **How**:
  ```gleam
  let hook_config = bidir.HookConfig(
    handlers: dict.from_list([
      #("PreToolUse", fn(input) {
        // Record invocation, return Continue
        dynamic.from(dict.from_list([#("continue", True)]))
      }),
    ]),
    permission_handlers: dict.new(),
  )
  let session = bidir.start_with_hooks(runner, config, hook_config)
  ```
- **Assert**:
  - Hook handler function is invoked when CLI sends `hook_callback`
  - Input contains `tool_name` and `tool_input`

#### SDK-31: PreToolUse Hook Block
- **What**: Hook returns `Block(reason)`, tool execution blocked
- **Assert**:
  - Tool does not execute (based on stream content)
  - Session continues without crash

#### SDK-32: PreToolUse Hook ModifyInput
- **What**: Hook returns `ModifyInput(new_input)`
- **Assert**:
  - Hook response sent to CLI
  - No SDK error

#### SDK-33: PostToolUse Hook
- **What**: Register `PostToolUse` hook, verify it fires after tool completes
- **Assert**:
  - Hook receives tool output
  - Can process and return

#### SDK-34: Permission Handler (can_use_tool)
- **What**: Register permission handler in `permission_handlers`
- **Assert**:
  - Handler invoked for `can_use_tool` requests
  - Can return `Allow` or `Deny(reason)` via `PermissionCheckResult`

#### SDK-35: Stop Hook
- **What**: Register `Stop` hook
- **Assert**:
  - Hook fires when session stops
  - Can return result

#### SDK-36: Multiple Hooks
- **What**: Register multiple hooks of same type
- **Assert**:
  - All handlers registered without error
  - Handlers invoked when events occur

---

### Category 5: Bidirectional Protocol (using `bidir.*`)

#### SDK-40: SetPermissionMode Control Message
- **What**: Call `bidir.set_permission_mode(session, AcceptEdits)`
- **Assert**:
  - Returns `Ok(Nil)` on success
  - Or appropriate error type

#### SDK-41: Interrupt Signal
- **What**: Call `bidir.interrupt(session)` during operation
- **Assert**:
  - Returns result (success or error)
  - Session remains usable or cleanly stopped

#### SDK-42: Hook Response Timeout
- **What**: Hook handler takes too long to respond
- **Assert**:
  - SDK handles timeout gracefully (fail-open for hooks, fail-deny for permissions)
  - Session continues or stops cleanly

#### SDK-43: Malformed Control Response (Offline Test)
- **What**: Use mock runner to inject malformed JSON
- **How**: `with_test_mode(mock_runner)` with malformed data
- **Assert**:
  - SDK logs/handles error
  - No crash

---

### Category 6: MCP Integration

#### SDK-50: MCP Server Configuration
- **What**: Configure MCP server via `with_mcp_config("test/fixtures/mcp-echo-server.json")`
- **Fixture**: Create `test/fixtures/mcp-echo-server.json`:
  ```json
  {
    "mcpServers": {
      "echo": {
        "command": "npx",
        "args": ["-y", "@anthropic/mcp-test-server"]
      }
    }
  }
  ```
- **Assert**:
  - Query executes without MCP config error
  - `SystemMessage.mcp_servers` contains configured server (if available)

#### SDK-51: MCP Tool Availability
- **What**: MCP server provides tools visible in `SystemMessage`
- **Assert**:
  - If MCP server connected, tools may appear in tools list
  - No crash if MCP tools present

#### SDK-52: MCP Server Failure
- **What**: MCP server unavailable or errors
- **How**: Point to non-existent MCP config or unreachable server
- **Assert**:
  - Query continues (MCP failure doesn't block query)
  - Error surfaced in `McpServerStatus` if available

---

### Category 7: Error Handling

#### SDK-60: CLI Not Found (Serial Test)
- **What**: Claude CLI not in PATH
- **Isolation**: Run serially with modified PATH, or use mock runner
- **How**:
  - Option A: Temporarily modify PATH to exclude `claude`
  - Option B: Use `with_test_mode(runner)` where spawn fails
- **Assert**:
  - `query()` returns `Error(CliNotFoundError(...))`
  - No crash or panic

#### SDK-61: Authentication Failure (Serial Test)
- **What**: Invalid or missing credentials
- **Isolation**: Run serially with cleared `ANTHROPIC_API_KEY`
- **Assert**:
  - Stream yields error or query fails
  - SDK surfaces error cleanly

#### SDK-62: Timeout and Delayed Exit Handling (Offline Tests)

Two tests cover SDK-62:

**SDK-62a: Delayed Exit (`sdk_62_delayed_exit_test`)**
- **What**: Process delays then exits with non-zero status
- **How**: Mock runner sleeps before returning `ExitStatus(1)`
- **Assert**:
  - ProcessError surfaced with correct exit code
  - SDK handles slow processes gracefully

**SDK-62b: Stream Timeout (`sdk_62_timeout_test`)**
- **What**: CLI hangs indefinitely during streaming
- **How**: Mock runner returns `runner.Timeout` variant
- **Assert**:
  - Timeout handled as end-of-stream or error
  - SDK does not hang indefinitely

#### SDK-63: Stream Interruption (Offline Test)
- **What**: Connection drops mid-stream
- **How**: Mock runner that sends partial data then closes
- **Assert**:
  - Partial data may be preserved
  - Error indicates interruption

#### SDK-64: Invalid JSON in Stream (Offline Test)
- **What**: Malformed JSON line in CLI output
- **How**: Mock runner emits `{"valid": true}\n{malformed\n{"valid": true}`
- **Assert**:
  - SDK logs warning or surfaces error
  - Valid lines before/after still processed (graceful degradation)

---

## CI Configuration

### Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `E2E_SDK_TEST` | Gate for E2E tests | `0` (skip) |
| `ANTHROPIC_API_KEY` | API authentication | Required when enabled |
| `E2E_FULL` | Run expensive multi-turn tests | `0` (skip) |

### CI Job Configuration

```yaml
# .github/workflows/test.yml (example)
jobs:
  unit-tests:
    # Always run - no API required
    steps:
      - run: gleam test

  e2e-tests:
    # Only on main branch or manual trigger
    if: github.ref == 'refs/heads/main' || github.event_name == 'workflow_dispatch'
    env:
      E2E_SDK_TEST: "1"
      ANTHROPIC_API_KEY: ${{ secrets.ANTHROPIC_API_KEY }}
    steps:
      - run: gleam test -- --only e2e
```

### Skip Behavior

- **When `E2E_SDK_TEST != 1`**: All SDK E2E tests skip (not fail)
- **When credentials missing**: Tests skip with warning message
- **On PR builds**: E2E tests skip by default (maintainer can enable via label)
- **On main/nightly**: E2E tests run with spend caps

### Local Development

Developers can run E2E tests locally:
```bash
export ANTHROPIC_API_KEY="sk-..."
export E2E_SDK_TEST=1
gleam test -- --only e2e
```

---

## Cost Management

E2E tests hit real API. Mitigations:

| Control | Implementation |
|---------|----------------|
| `max_turns(1)` default | Most tests limit to single turn |
| `max_budget(0.05)` ceiling | Per-test budget cap |
| Skip expensive tests | `E2E_FULL=1` required for multi-turn |
| Nightly runs only | Full suite runs on schedule |

---

## Test Isolation

### Parallel-Safe Tests (Default)
- Use unique prompts per test
- No shared mutable state
- Per-scenario log files

### Serial Tests (Marked Explicitly)
Tests requiring environment modification run serially:
- **SDK-60 (CLI not found)**: Modifies PATH
- **SDK-61 (Auth failure)**: Clears ANTHROPIC_API_KEY

Implementation: Use Gleam's test framework serial execution markers or run as separate test suite.

---

## Risks, Edge Cases & Breaking Changes

### Risks
| Risk | Mitigation |
|------|------------|
| API cost overrun | Budget caps, nightly-only full runs |
| Flaky tests from non-determinism | Protocol invariant assertions only |
| CI secret exposure | Standard GitHub secrets handling |
| Breaking changes in CLI | Version check, permissive mode option |

### Edge Cases Covered
- Empty tool list
- Zero-cost queries
- Immediate stream termination
- Hook timeout scenarios
- MCP server unavailable

### Breaking Changes
None—this replaces internal test implementation, not public API.

---

## Testing & Validation

### Test Matrix

| Category | Count | API | Offline? |
|----------|-------|-----|----------|
| Stream API | 4 | `query()` | No |
| Message Parsing | 5 | `query()` | No |
| Query Options | 9 | `query()` | No |
| Hooks | 7 | `bidir.*` | Mostly no |
| Bidir Protocol | 4 | `bidir.*` | Mixed |
| MCP | 3 | `query()` | No |
| Error Handling | 5 | Both | 4 offline, 1 serial |

### Coverage Mapping (Old → New)

| Old Test | Coverage | New Test(s) |
|----------|----------|-------------|
| E2E-01 (preflight) | CLI discovery | SDK-01 (implicit), SDK-60 |
| E2E-02 (version) | Version check | SDK-01 (implicit via SDK version check) |
| E2E-03 (auth) | Authentication | SDK-61 |
| E2E-04 (basic query) | Basic streaming | SDK-01, SDK-02 |
| E2E-05 (options) | Options passthrough | SDK-20 through SDK-28 |
| E2E-06 (multi-turn) | Conversation flow | SDK-03 |
| E2E-07 (resume) | Session resume | SDK-04 |
| E2E-08a (delayed-exit) | Delayed exit handling | SDK-62 (`sdk_62_delayed_exit_test`) |
| E2E-08b (timeout) | Stream timeout handling | SDK-62 (`sdk_62_timeout_test`) |

---

## Open Questions

1. **MCP test server**: Should we use `@anthropic/mcp-test-server` or create a simpler echo server fixture?
2. **Hook timeout values**: What are appropriate timeout defaults for hook callbacks in tests?
3. **Session caching**: Should resume tests create fresh sessions or can they share session IDs within a test run? (Current approach: per-test sessions for isolation)

---

## Migration Checklist

### Phase 1: Implement New Tests
- [ ] Add SDK import and stream consumption helpers to `test/e2e/helpers.gleam`
- [ ] Implement SDK-01 through SDK-04 (Stream API)
- [ ] Implement SDK-10 through SDK-14 (Message Parsing)
- [ ] Implement SDK-20 through SDK-28 (Options)
- [ ] Implement SDK-30 through SDK-36 (Hooks) using `bidir.start_with_hooks()`
- [ ] Implement SDK-40 through SDK-43 (Bidir Protocol)
- [ ] Implement SDK-50 through SDK-52 (MCP)
- [ ] Implement SDK-60 through SDK-64 (Error Handling)
- [ ] Create `test/fixtures/mcp-echo-server.json`
- [ ] Update CI to set `E2E_SDK_TEST=1` with API credentials on main

### Phase 2: Verify and Clean Up
- [ ] Verify all new tests pass in CI
- [ ] Confirm coverage mapping is complete
- [ ] Delete `run_e2e_01_preflight` through `run_e2e_08_timeout`
- [ ] Delete CLI discovery/version parsing code (if unused elsewhere)
- [ ] Delete `run_command` shell execution helper
- [ ] Add cost monitoring/alerting for E2E runs
