<!-- review-type: architecture-review -->

# Architecture Review: claude-agent-sdk-gleam

## Method

- **Entry points reviewed**: `src/claude_agent_sdk.gleam` (public API: `query()`, `start_session()`, control ops), `src/claude_agent_sdk/internal/bidir.gleam` (bidirectional protocol actor), `src/claude_agent_sdk/internal/stream.gleam` (unidirectional streaming)
- **Key files analyzed**: 21 source files across `src/claude_agent_sdk/` including internal modules, options, error types, decoder, and CLI argument handling
- **Hotspot inventory (LOC)**: `internal/bidir.gleam` (~2818), `internal/stream.gleam` (~1232), `internal/decoder.gleam` (~842), `claude_agent_sdk.gleam` (~830), `options.gleam` (~755) - all exceed 500-line threshold
- **Tools used**: Read (source inspection), Grep (function discovery), wc -l (LOC counting), targeted line ranges for complexity analysis
- **Assumptions**: SDK wraps Claude CLI with two modes (unidirectional streaming via `query()`, bidirectional sessions via `start_session()`); bidir mode is implemented internally but public API returns stubs

---

## Issues

### [High] God-file: `internal/bidir.gleam` combines 7+ distinct concerns in ~2800 lines

**Primary files**:
- `src/claude_agent_sdk/internal/bidir.gleam:1-2818`
- Key functions: `handle_message` (line 1052), `start_internal` (line 747), `transition` (line 180), `dispatch_hook_callback` (line 1624)

**Category**: Cohesion

**Type**: task

**Confidence**: High

**Source**: multiple (codex, gemini, claude)

**Context**: This file is critically oversized and mixes disparate concerns: (1) lifecycle state machine with explicit transitions, (2) OTP actor message handling with a large `ActorMessage` variant set in `handle_message`, (3) hook dispatch logic with capacity checks and timeout scheduling, (4) permission callback dispatch with fail-deny semantics, (5) request/response correlation via `pending_requests` and `pending_hooks`, (6) port message routing, (7) 7 inline error type definitions (SessionError, InvalidTransition, InterruptError, SetPermissionModeError, SetModelError, RewindFilesError, StartError), (8) backpressure constants and timer management. The `handle_message()` function at line 1052 is a mega-switch that touches all concerns. Changes to hook dispatch require understanding actor lifecycle; changes to timeout logic touch request correlation. The file is nearly impossible to test in isolation.

**Fix**: Decompose into focused modules following functional core / imperative shell pattern:
- `bidir/state.gleam`: Pure state types and `transition()` function (testable without actor)
- `bidir/routing.gleam`: `route_incoming()` and message classification (pure)
- `bidir/hooks.gleam`: Hook/permission dispatch, timeout policy, fail-open vs fail-deny semantics
- `bidir/correlation.gleam`: Pending request/response tracking
- `bidir/actor.gleam`: Thin orchestration shell that composes pure functions

**Acceptance Criteria**:
- No single module exceeds 600 lines
- Lifecycle transitions (`transition`) unit-testable without spawning processes
- Hook dispatch logic testable without starting full actor
- Error types importable without pulling in actor internals

**Test Plan**:
- Unit tests for `transition()` covering all valid/invalid state changes
- Unit tests for `route_incoming()` with mock message payloads
- Integration test showing actor correctly composes extracted modules

---

### [High] `QueryOptions` is a kitchen-sink type mixing 27 fields across 5 concern groups

**Primary files**:
- `src/claude_agent_sdk/options.gleam:79-124` (type definition with 27 fields)
- `src/claude_agent_sdk.gleam:396` (query usage)
- `src/claude_agent_sdk.gleam:538` (emits BidirOptionIgnored warning when bidir features used with query)

**Category**: Cohesion

**Type**: task

**Confidence**: High

**Source**: multiple (codex, claude)

**Context**: `QueryOptions` includes: CLI options (model, budget, turns, prompts), SDK toggles (test_mode, version checks), bidir hook callbacks (6 hooks), MCP server handlers, file checkpointing, timeout configuration, and `bidir_runner_factory` test seam. This "one struct to rule them all" couples unrelated concerns. The code at `src/claude_agent_sdk.gleam:532-541` already emits `BidirOptionIgnored` warnings because `query()` ignores bidir features. Every new capability widens the same type, increasing churn. Tests must populate unrelated fields to construct minimal configs.

**Fix**: Split configuration into layered records:
- `CliOptions`: model, max_turns, max_budget, prompts, allowed/disallowed tools, mcp_config, permission_mode
- `SdkOptions`: test_mode, skip_version_check, permissive_version_check, cwd
- `BidirOptions`: hook callbacks, timeout configuration, file checkpointing, mcp_servers
- `TestOptions`: test_runner, bidir_runner_factory

Then `query()` accepts `{CliOptions, SdkOptions}` while bidir accepts `{CliOptions, SdkOptions, BidirOptions}`.

**Acceptance Criteria**:
- `query()` signature does not mention or carry bidir-only concepts
- No "ignored option" warnings needed in query path
- Tests construct minimal configs without populating unrelated fields
- No single record type exceeds 12 fields

**Test Plan**:
- Verify `query()` compiles and runs with only `CliOptions` + `SdkOptions`
- Verify bidir path correctly uses all three option groups
- Builder ergonomics preserved via composition helpers

---

### [Medium] Mixed responsibility in `internal/stream.gleam`: pull iteration and push buffering in 1232 lines

**Primary files**:
- `src/claude_agent_sdk/internal/stream.gleam:1-1232`
- `LineBuffer` type: line 55
- `handle_port_data`: line 70
- Iterator logic: lines 551-927

**Category**: Cohesion

**Type**: task

**Confidence**: High

**Source**: multiple (codex, gemini, claude)

**Context**: This module combines three distinct layers: (1) raw byte buffering with CRLF normalization (`LineBuffer`, `append_to_buffer`, `read_line`), (2) JSON parsing/decoding glue (`process_line`), (3) stream lifecycle state machine (`StreamState`, `Streaming`, `Closed`) plus collection utilities. The pull-based `QueryStream` iterator and push-based `LineBuffer` for bidir actor are different orchestration models serving different execution paths. Coupling them increases cognitive load and makes transport changes (e.g., WebSocket) affect iterator code.

**Fix**: Extract a pure "line framing" submodule:
- `internal/line_framing.gleam`: Buffer operations, CRLF normalization, line extraction (pure, shared)
- Keep `internal/stream.gleam` for iterator/state machine (imports line_framing)
- Bidir actor imports line_framing directly without stream iterator concerns

**Acceptance Criteria**:
- Shared line framing functionality in a module under 200 lines
- Iterator code has no bidir-specific imports
- Line framing unit tests cover CRLF and overflow behaviors independently
- Public stream API unchanged

**Test Plan**:
- Extract existing line buffer tests to target new module
- Verify bidir actor uses line_framing directly
- No regressions in stream_test.gleam

---

### [Medium] Error type definitions: `StartError` duplicated, others fragmented across modules

**Primary files**:
- `src/claude_agent_sdk/error.gleam:308` (StartError with Timeout | SpawnFailed | NotImplemented)
- `src/claude_agent_sdk/internal/bidir.gleam:606` (StartError with ActorStartFailed | RunnerStartFailed)
- `src/claude_agent_sdk/internal/bidir_runner.gleam:27` (StartError - third definition)
- `src/claude_agent_sdk/internal/bidir.gleam:98-281` (SessionError, StopReason, InvalidTransition, InterruptError, SetPermissionModeError, SetModelError, RewindFilesError - fragmented, not duplicated)

**Category**: Duplication

**Type**: task

**Confidence**: High

**Source**: multiple (codex, claude)

**Context**: `StartError` is **duplicated** in three places with different variants: `error.gleam:308` (Timeout | SpawnFailed | NotImplemented), `bidir.gleam:606` (ActorStartFailed | RunnerStartFailed), and `bidir_runner.gleam:27`. The bidir module also defines 6 operation-specific error types (lines 98-281) that are **fragmented** (defined only in bidir, not duplicated elsewhere). Query-mode uses `claude_agent_sdk/error` diagnostics while bidir has its own `SessionError` hierarchy. This creates confusion about which types to use and prevents uniform error handling.

**Fix**: Consolidate all error types in `claude_agent_sdk/error.gleam`:
- Move all bidir error types to error module (or `error/session.gleam` sub-module)
- Define common vocabulary: `SdkError { kind, context, recoverability }`
- Both query and bidir map to consistent "timeout", "session closed", "CLI not found" kinds
- Keep rich diagnostics as structured context

**Acceptance Criteria**:
- Single definition of each error type across codebase
- All error types importable from `claude_agent_sdk/error` or sub-modules
- No inline error type definitions in implementation modules
- Public API docs show one coherent error story

**Test Plan**:
- Verify all error types compile from single module
- Update existing error handling to use canonical types
- Add test showing consistent error serialization format

---

### [Medium] Duplicated hook vs permission dispatch logic in bidir actor

**Primary files**:
- `src/claude_agent_sdk/internal/bidir.gleam:1624` (dispatch_hook_callback - async path)
- `src/claude_agent_sdk/internal/bidir.gleam:1710` (dispatch_permission_hook_callback - sync path)
- `src/claude_agent_sdk/internal/bidir.gleam:1778` (dispatch_permission_callback - async permission path)

**Category**: Duplication

**Type**: task

**Confidence**: Medium

**Source**: codex

**Context**: Hook callbacks and permission callbacks use nearly identical async machinery: capacity checks against `max_pending_hooks`, spawning tasks, scheduling timeouts with verify refs, sending immediate "async" responses, handling done/error/timeout events. The only difference is fail-open (hooks) vs fail-deny (permissions) policy. Note: there is also a separate sync path via `dispatch_permission_hook_callback` (line 1710) that does not share the async machinery. The async parallel structure makes policy changes risky and invites divergence. Adding a new async callback type would require copying ~100 lines.

**Fix**: Introduce unified callback abstraction:
```gleam
run_callback(
  callback_type: CallbackType,  // Hook | Permission
  handler: fn(Dynamic) -> Dynamic,
  request_id: String,
  input: Dynamic,
  timeout_ms: Int,
  fail_policy: FailPolicy,  // FailOpen | FailDeny
) -> #(SessionState, ImmediateResponse)
```

**Acceptance Criteria**:
- Single function schedules callback timeouts and manages `PendingHook` lifecycle
- Fail-open vs fail-deny selected by enum, not duplicated branches
- Capacity/backpressure errors consistent across callback types

**Test Plan**:
- Unit test for unified callback abstraction with both policies
- Verify hook and permission dispatch use same code path
- No behavior change in existing hook/permission tests

---

### [Medium] Testability: bidir protocol logic buried in actor callbacks

**Primary files**:
- `src/claude_agent_sdk/internal/bidir.gleam:1052` (handle_message entry point)
- `src/claude_agent_sdk/internal/bidir.gleam:747-910` (start_internal with ~160 lines)

**Category**: Testability

**Type**: task

**Confidence**: High

**Source**: multiple (gemini, claude)

**Context**: Core protocol logic (how `HookCallback` transitions state, how requests are queued, how timeouts affect pending operations) is embedded in `handle_message` actor callback. Tests must spawn the full actor to verify internal logic, making them slow and prone to race conditions. The `start_internal()` function at ~160 lines builds state, selector with 7 pattern matchers, port connection, and handshake in one block - impossible to test individual components.

**Fix**: Adopt "Functional Core, Imperative Shell" pattern:
- Extract `handle_event(state, event) -> #(new_state, List(Action))` where `Action` describes side effects
- Actor shell interprets `Action` list (SendResponse, ScheduleTimeout, WritePort)
- Extract `build_selector()` and `initial_state()` as separate testable functions
- Reduce `start_internal()` to thin composition

**Acceptance Criteria**:
- Complex protocol edge cases covered by fast, pure unit tests
- Actor implementation is thin translation layer (<50 lines)
- Selector construction independently testable
- No `process.sleep()` needed in protocol logic tests

**Test Plan**:
- Add unit tests for `handle_event()` pure function
- Verify extracted `build_selector()` produces correct pattern matches
- Existing actor integration tests continue to pass

---

### [Low] Oversized `SessionState` record with ~20 flat fields

**Primary files**:
- `src/claude_agent_sdk/internal/bidir.gleam:495` (SessionState definition)

**Category**: Abstraction

**Type**: chore

**Confidence**: Medium

**Source**: gemini

**Context**: `SessionState` is a flat record mixing static configuration (`hooks`, `default_timeout_ms`), volatile runtime state (`pending_requests`, `pending_hooks`, `lifecycle`), and IO buffers (`line_buffer`). Updates are verbose (`SessionState(..state, field: val)`) and it's unclear which fields change together.

**Fix**: Group related fields into nested records:
- `SessionConfig`: Static configuration (hooks, timeouts, capabilities)
- `RuntimeState`: Lifecycle, runner handle, subscriber
- `PendingOps`: pending_requests, pending_hooks, queued_ops

**Acceptance Criteria**:
- Top-level `SessionState` fields reduced to <8
- Helper functions receive relevant sub-records, not full state
- State updates target specific groups

**Test Plan**:
- Refactor state structure
- Verify all existing tests pass
- Measure if update verbosity decreases

---

### [Low] Port ownership and FFI spread across multiple modules

**Primary files**:
- `src/claude_agent_sdk/internal/bidir_runner.gleam:65` (port spawning)
- `src/claude_agent_sdk/internal/bidir.gleam:899` (port connection)
- `src/claude_agent_sdk/internal/cli.gleam:57` (version detection port)

**Category**: Boundaries

**Type**: chore

**Confidence**: Medium

**Source**: codex

**Context**: Port spawning and management appear in multiple modules: `cli` spawns port for `--version`, `bidir_runner` spawns bidir ports, `bidir` connects ports to actor. This spreads effectful IO across the codebase, making it harder to test core logic or swap port strategies.

**Fix**: Centralize port IO behind minimal interface injected into higher-level logic:
- Create `internal/port_io.gleam` with unified spawn/write/close interface
- Pure modules (transition, routing, decoding) have no `port_ffi` imports
- Tests provide fakes without building complex configs

**Acceptance Criteria**:
- Pure modules have zero `port_ffi` imports
- IO modules are small and composable
- Version detection and session spawning use same IO abstraction

**Test Plan**:
- Create port_io abstraction
- Update cli.gleam and bidir_runner.gleam to use it
- Add tests with mock port implementation

---

### [Low] Repetitive decoder pattern in `internal/decoder.gleam`

**Primary files**:
- `src/claude_agent_sdk/internal/decoder.gleam:107-367` (message decoders)

**Category**: Duplication

**Type**: chore

**Confidence**: Medium

**Source**: claude

**Context**: Four message decoders (`decode_system_message`, `decode_assistant_message`, `decode_user_message`, `decode_result_message`) follow similar patterns: build decoder with `decode.optional_field` chains, run decoder, wrap result or format errors. Sizes vary (decode_assistant_message: 33 lines, decode_user_message: 39 lines, decode_system_message: 81 lines, decode_result_message: 98 lines) but the structural pattern is repetitive.

**Fix**: Create `decode_with_error_wrap(decoder, error_context) -> Result` helper. Reduce each message decoder to ~10 lines. Consider whether repetition is acceptable given Gleam's compile-time safety.

**Acceptance Criteria**:
- Each message type decoder is <25 lines
- Error formatting logic appears once
- New message types addable with minimal boilerplate

**Test Plan**:
- Extract helper function
- Verify all decoder tests pass
- Add decoder for hypothetical new message type to verify ergonomics

---

## Summary

The codebase has clean public/internal boundaries and good separation in the unidirectional streaming path. The primary architectural debt is concentrated in **`internal/bidir.gleam`** (flagged by all three reviewers), which should be decomposed before adding more bidirectional features. Secondary concerns include the **kitchen-sink `QueryOptions`** type and **fragmented error handling** across modes. The recommendations follow a functional patterns bias: extract pure reducers, push IO to edges, and enable isolated unit testing of protocol logic.

**Priority order for remediation**:
1. Decompose `bidir.gleam` (unblocks safe changes to bidir mode)
2. Split `QueryOptions` (prevents query/bidir concern leakage)
3. Consolidate error types (improves API consistency)
4. Extract line framing from stream (reduces coupling)
5. Remaining low-priority cleanups as convenient
