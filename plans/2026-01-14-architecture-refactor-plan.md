# Implementation Plan: Architecture Refactor for claude-agent-sdk-gleam

## Context & Goals

- **Spec**: `plans/2026-01-14-architecture-review.md`
- Address architectural debt identified in multi-model architecture review
- Primary focus: Decompose `internal/bidir.gleam` god-file (~2800 lines, 7+ concerns)
- Secondary focus: Split `QueryOptions` kitchen-sink type (28 fields -> focused records)
- Enable isolated unit testing of protocol logic via Functional Core / Imperative Shell pattern

## Scope & Non-Goals

### In Scope (High Priority Only)
- **Decompose `bidir.gleam`** into 5 focused modules (<600 lines each)
- **Split `QueryOptions`** into concern-specific records (CliOptions, SdkOptions, BidirOptions) with clean break (no wrapper)
- **Consolidate error types** to `error.gleam` (single source of truth)
- Add unit tests for extracted pure functions

### Out of Scope (Non-Goals)
- Medium priority: Extract `line_framing.gleam` from `stream.gleam` (defer to follow-up)
- Medium priority: Duplicated hook vs permission dispatch unification (defer)
- Low priority: SessionState grouping, port IO centralization, decoder patterns
- Feature additions or API changes beyond refactoring
- Performance optimizations (unless blocking)
- Backwards compatibility wrapper for QueryOptions (clean break preferred)

## Assumptions & Constraints

### Implementation Constraints
- **Breaking Change**: This refactor changes public API signatures (QueryOptions split) - requires major version bump
- All existing tests must continue to pass after updating to new API (`gleam test` before PR)
- Single large PR - no broken intermediate states
- No external consumers of internal modules (safe to refactor imports)
- Pure modules (`state`, `routing`, `hooks`, `correlation`) must not import `port_ffi`, `gleam_otp/actor`, or `error.gleam` types containing OTP references
- Module imports must be acyclic: state → routing/hooks/correlation → actor (one-way)

### Testing Constraints
- Run `gleam test` continuously during refactor
- Pure functions (`transition`, `route_incoming`) must be unit-testable without spawning processes
- Existing 20+ bidir test files must continue to work with updated imports
- No specific coverage threshold - maintain existing coverage

## Integration Analysis

### Existing Mechanisms Considered

| Existing Mechanism | Could Serve Feature? | Decision | Rationale |
|--------------------|---------------------|----------|-----------|
| `error.gleam` error types | Yes | Extend | Already has public error vocabulary; consolidate session errors here |
| `internal/stream.gleam` LineBuffer | Yes | Reuse | Bidir actor already uses `stream.LineBuffer`; keep as-is for now |
| `options.gleam` builder pattern | Yes | Replicate | Apply same `with_*` builder pattern to split record types |
| OTP actor pattern in bidir | Yes | Thin shell | Keep minimal actor shell, extract pure logic to testable modules |

### Integration Approach
Adopt Functional Core / Imperative Shell pattern:
- Extract pure state machine logic into `bidir/state.gleam`
- Extract pure message routing into `bidir/routing.gleam`
- Extract hook dispatch policy into `bidir/hooks.gleam`
- Extract pending operation tracking into `bidir/correlation.gleam`
- Keep thin actor shell in `bidir/actor.gleam` that composes pure functions

**Data flow:**
1. `bidir/actor.gleam` receives `ActorMessage` via selector
2. Actor calls `bidir/routing.route_incoming()` to classify message
3. Based on route, calls `bidir/hooks.dispatch_hook()` or `bidir/correlation.resolve_pending()`
4. State transitions via `bidir/state.transition()`
5. Actions returned to actor shell for execution (port writes, timer scheduling)

## Prerequisites

- [ ] Ensure all existing tests pass (baseline: `gleam test`)
- [ ] Create feature branch for the refactor

## High-Level Approach

**Delivery Strategy:** Single large PR containing all High priority changes.

**Work Order** (logical dependencies within single PR):
1. Create `bidir/` directory structure and empty module files with type signatures
2. Move error types from `bidir.gleam` and `bidir_runner.gleam` to `error.gleam`
3. Extract pure state machine logic to `bidir/state.gleam`
4. Extract message routing logic to `bidir/routing.gleam`
5. Extract hook dispatch logic to `bidir/hooks.gleam`
6. Extract pending operation tracking to `bidir/correlation.gleam`
7. Reduce `bidir.gleam` to thin actor shell (rename to `bidir/actor.gleam`)
8. Split `QueryOptions` into `CliOptions`, `SdkOptions`, `BidirOptions`
9. Update `claude_agent_sdk.gleam` to use split option types
10. Update all test imports and fixtures

**Rationale for single PR:**
- Avoids intermediate broken states
- Error type consolidation is intertwined with bidir decomposition
- QueryOptions split touches same files as bidir changes
- Easier to review holistically than in pieces

## Technical Design

### Architecture

#### Bidir Module Structure

**Target Directory Layout:**
```
src/claude_agent_sdk/internal/
├── bidir/                    (proposed - new directory)
│   ├── state.gleam           (pure state types + transition function)
│   ├── routing.gleam         (route_incoming + message classification)
│   ├── hooks.gleam           (hook/permission dispatch + timeout policy)
│   ├── correlation.gleam     (pending request/response tracking)
│   └── actor.gleam           (thin orchestration shell)
│
├── bidir.gleam               (facade: re-exports from bidir/* for compatibility)
└── bidir_runner.gleam        (updated: remove StartError, import from error)
```

#### Module Responsibilities

**bidir/state.gleam** (~250 lines)
- **Types**: `SessionLifecycle`, `LifecycleEvent`, `InvalidTransition`, `SessionState`, `StartConfig`, `StopReason`
- **Functions**: `transition()`, `initial_state()`, `is_terminal()`, `default_config()`
- **Responsibility**: Pure state machine for session lifecycle and configuration
- **Dependencies**: None (zero imports from actor machinery or OTP)
- **Key extractions from bidir.gleam**:
  - `SessionLifecycle` type (line ~120)
  - `transition` function (line ~180)
  - `SessionState` record type
  - `StartConfig` type (line ~614) - moved here to avoid circular dependency with actor
  - `StopReason` type

**bidir/routing.gleam** (~150 lines)
- **Types**: `MessageRoute` (RouteHookCallback, RoutePermission, RouteMcp, RouteResponse, RouteSubscriber)
- **Functions**: `route_incoming()`, `route_control_request()`, `route_control_response()`
- **Responsibility**: Classify incoming messages and determine handling path
- **Dependencies**: `internal/control` types
- **Input**: `IncomingMessage` from control module
- **Output**: `MessageRoute` enum indicating handler

**bidir/hooks.gleam** (~250 lines)
- **Types**: `HookConfig`, `CallbackType` (HookType, PermissionType), `PendingHook`, `CleanupSource`
- **Functions**: `empty_hook_config()`, `build_hook_registrations()`, `is_hook_event_name()`, `dispatch_hook_callback()`, `dispatch_permission_callback()`
- **Responsibility**: Hook registration, dispatch, and fail-open vs fail-deny policy
- **Dependencies**: `bidir/state` for session state
- **Key extractions from bidir.gleam**:
  - `dispatch_hook_callback` function (line ~1624)
  - Hook registration logic

**bidir/correlation.gleam** (~150 lines)
- **Types**: `PendingRequest`, `QueuedOperation`, `RequestResult`
- **Functions**: `resolve_pending()`, `cancel_pending()`, `timeout_pending()`, `add_pending()`
- **Responsibility**: Track request/response correlation for async operations
- **Dependencies**: `bidir/state` for pending state
- **Key extractions from bidir.gleam**:
  - Pending request tracking logic
  - Response correlation

**bidir/actor.gleam** (~500 lines)
- **Types**: `ActorMessage`, `Response`, `SubscriberMessage`
- **Functions** (public API surface):
  - Lifecycle: `start()`, `start_for_testing()`, `start_with_hooks()`, `shutdown()`
  - Queries: `get_lifecycle()`, `get_pid()`, `get_capabilities()`, `ping()`
  - Control: `send_control_request()`, `send_user_message()`, `cancel_pending_request()`
  - Operations: `interrupt()`, `set_permission_mode()`, `set_model()`, `rewind_files()`
  - Testing: `inject_message()`, `inject_port_closed()`
  - Internal: `handle_message()`, `build_selector()`, `flush_queued_ops()`, `queue_operation()`, `add_pending_request()`, `add_pending_hook()`
- **Responsibility**: Thin orchestration shell that composes pure functions; contains all side effects (port writes, timer scheduling, process spawning)
- **Dependencies**: All other bidir/* modules, `port_ffi`, `gleam_otp/actor`, `error.gleam`
- **Key extractions from bidir.gleam**:
  - `start_internal` function (line ~747)
  - `handle_message` function (line ~1052)
  - Actor loop and selector setup
  - All public API functions that interact with the actor

#### Error Type Consolidation

**Move to `error.gleam`:**

```gleam
// Session-level errors (from bidir.gleam lines 98-113)
pub type SessionError {
  InitializationTimeout
  InitializationError(message: String)
  CliExitedDuringInit
  CliExitedDuringStartup
  RuntimeError(reason: String)
  InitQueueOverflow(message: String)
  TooManyPendingRequests(message: String)
}

// Operation-specific errors (from bidir.gleam lines 243-290)
pub type InterruptError {
  CliError(message: String)
  InterruptTimeout
  SessionStopped
}

pub type SetPermissionModeError {
  SetPermissionModeCliError(message: String)
  SetPermissionModeTimeout
  SetPermissionModeSessionStopped
}

pub type SetModelError {
  SetModelCliError(message: String)
  SetModelTimeout
  SetModelSessionStopped
}

pub type RewindFilesError {
  CheckpointingNotEnabled
  RewindFilesCliError(message: String)
  RewindFilesTimeout
  RewindFilesSessionStopped
}

// Consolidated StartError (merge 3 definitions)
// Note: ActorStartFailed wraps gleam_otp/actor.StartError - only bidir/actor.gleam should use this variant
pub type StartError {
  Timeout
  SpawnFailed(reason: String)
  ActorStartFailed(actor.StartError)  // OTP dependency - only used by bidir/actor.gleam
  RunnerStartFailed(String)
}
// Note: Remove `NotImplemented` variant (implementation complete)
// Pure modules (state, routing, hooks, correlation) must NOT import StartError due to OTP dependency
```

**Remove from source files:**
- `bidir.gleam`: Delete all error type definitions
- `bidir_runner.gleam`: Delete `StartError` definition (line ~27)

#### QueryOptions Split

**Decision:** Clean break - no unified wrapper. `query()` takes `CliOptions + SdkOptions`, `start_session()` takes all three types.

**CliOptions** (12 fields) - passed to Claude CLI:
```gleam
pub type CliOptions {
  CliOptions(
    model: Option(String),
    max_turns: Option(Int),
    max_budget_usd: Option(Float),
    system_prompt: Option(String),
    append_system_prompt: Option(String),
    allowed_tools: Option(List(String)),
    disallowed_tools: Option(List(String)),
    mcp_config_path: Option(String),
    permission_mode: Option(PermissionMode),
    resume_session_id: Option(String),
    continue_session: Bool,
    cwd: Option(String),
  )
}
```

**SdkOptions** (4 fields) - SDK behavior control:
```gleam
pub type SdkOptions {
  SdkOptions(
    test_mode: Bool,
    test_runner: Option(Runner),
    skip_version_check: Bool,
    permissive_version_check: Bool,
  )
}
```

**BidirOptions** (12 fields) - bidirectional session configuration:
```gleam
pub type BidirOptions {
  BidirOptions(
    on_pre_tool_use: Option(fn(PreToolUseContext) -> HookExecutionResult),
    on_post_tool_use: Option(fn(PostToolUseContext) -> HookExecutionResult),
    on_user_prompt_submit: Option(fn(UserPromptSubmitContext) -> HookExecutionResult),
    on_stop: Option(fn(StopContext) -> HookExecutionResult),
    on_subagent_stop: Option(fn(SubagentStopContext) -> HookExecutionResult),
    on_pre_compact: Option(fn(PreCompactContext) -> HookExecutionResult),
    on_can_use_tool: Option(fn(CanUseToolContext) -> PermissionCheckResult),
    mcp_servers: List(#(String, fn(Dynamic) -> Dynamic)),
    file_checkpointing_enabled: Bool,
    timeout_ms: Option(Int),
    hook_timeouts: Dict(HookEvent, Int),
    bidir_runner_factory: Option(fn() -> BidirRunner),
  )
}
```

**API Changes:**
- `query(prompt, cli_options, sdk_options)` - accepts 2 option types
- `start_session(cli_options, sdk_options, bidir_options)` - accepts 3 option types
- Remove `BidirOptionIgnored` warning (no longer needed with type separation)

**Builder Functions:** Each type gets its own builder functions following existing pattern:
- `cli_options()` -> default CliOptions
- `with_model(options, model)`, `with_max_turns(options, turns)`, etc.
- `sdk_options()` -> default SdkOptions
- `with_test_mode(options, enabled)`, etc.
- `bidir_options()` -> default BidirOptions
- `with_on_pre_tool_use(options, callback)`, etc.

### Data Model

No persistent data model changes - this is a pure refactoring.

### API/Interface Design

**Public API: Breaking Change (Major Version Bump Required)**

This refactor intentionally changes the public API:
- `query()` signature changes from `query(prompt, options)` to `query(prompt, cli_options, sdk_options)`
- `start_session()` signature changes to accept 3 option types
- All existing callers must update to new API

**Internal Interfaces (new):**

```gleam
// bidir/state.gleam
pub fn transition(
  from: SessionLifecycle,
  event: LifecycleEvent,
) -> Result(SessionLifecycle, InvalidTransition)

pub fn initial_state(config: StartConfig) -> SessionState

// bidir/routing.gleam
pub fn route_incoming(message: IncomingMessage) -> MessageRoute

// bidir/correlation.gleam
pub fn resolve_pending(
  pending: PendingRequest,
  response: IncomingControlResponse,
) -> Nil

pub fn cancel_pending(pending: PendingRequest, reason: String) -> Nil

// bidir/hooks.gleam
pub fn dispatch_hook_callback(
  config: HookConfig,
  callback_id: String,
  input: Dynamic,
) -> Result(Dynamic, String)

pub fn build_hook_registrations(
  bidir_options: BidirOptions,
) -> List(HookRegistration)
```

### File Impact Summary

| Path | Status | Description |
|------|--------|-------------|
| `src/claude_agent_sdk/internal/bidir.gleam` | Exists | Decompose -> thin facade re-exporting from bidir/* |
| `src/claude_agent_sdk/internal/bidir/state.gleam` | **New** | Pure state machine (SessionLifecycle, transition) |
| `src/claude_agent_sdk/internal/bidir/routing.gleam` | **New** | Pure message routing (MessageRoute, route_incoming) |
| `src/claude_agent_sdk/internal/bidir/hooks.gleam` | **New** | Pure hook dispatch (HookConfig, dispatch_*) |
| `src/claude_agent_sdk/internal/bidir/correlation.gleam` | **New** | Pure request tracking (PendingRequest, resolve_pending) |
| `src/claude_agent_sdk/internal/bidir/actor.gleam` | **New** | Thin actor shell (handle_message, selector) |
| `src/claude_agent_sdk/options.gleam` | Exists | Split into CliOptions, SdkOptions, BidirOptions |
| `src/claude_agent_sdk/error.gleam` | Exists | Add SessionError, InterruptError, SetPermissionModeError, SetModelError, RewindFilesError; consolidate StartError |
| `src/claude_agent_sdk/internal/bidir_runner.gleam` | Exists | Remove StartError (import from error.gleam) |
| `src/claude_agent_sdk.gleam` | Exists | Update to use split option types |
| `src/claude_agent_sdk/internal/cli.gleam` | Exists | Update option type usage |
| `test/**/*.gleam` | Exists | Update imports for new module paths and option types |

## Risks, Edge Cases & Breaking Changes

### Edge Cases & Failure Modes

- **Circular imports**: Module extraction must respect dependency order. State depends on nothing, routing depends on control types, hooks depends on state, correlation depends on state, actor depends on all.
- **State Drift**: Ensure `state.gleam` transitions are strictly pure and cover all status changes (e.g., `Suspended` -> `Running`).
- **Option Mismatch**: Users (tests) might try to pass wrong option type to functions. Compiler catches this.
- **Test fixture updates**: 20+ test files import bidir directly. All must be updated to new import paths.

### Breaking Changes & Compatibility

**Internal breaking changes** (acceptable per scope):
- All files importing `bidir.gleam` types must update imports
- `QueryOptions` replaced by 3 separate types
- Error types move from bidir to error module

**Public API breaking changes** (requires major version bump):
- `query()` and `start_session()` signatures change to accept split option types
- Error types returned from public functions use canonical `error.gleam` types

**Facade Strategy (for this PR):**
- `bidir.gleam` becomes a thin facade re-exporting public API from `bidir/actor.gleam`
- All 20+ test files updated to import from new module paths (`bidir/state`, `bidir/actor`, etc.)
- Facade retained for any non-test internal callers; can be removed in follow-up

**Mitigations:**
- Single PR ensures atomic transition
- No intermediate broken state
- `gleam test` validates all changes
- No external consumers to coordinate with

**Verification Gates for Single Large PR:**
1. `gleam build` produces no warnings (import errors, unused vars)
2. `gleam test` passes all tests
3. Dependency graph check: `grep -r "import.*bidir/actor" src/claude_agent_sdk/internal/bidir/{state,routing,hooks,correlation}.gleam` returns empty
4. Facade exports match: compare `bidir.gleam` re-exports against original public symbols

### Risks

- **Risk: Test Fragility** - Large refactor may expose hidden coupling in tests
  - Mitigation: Run tests continuously during refactor, fix as discovered
- **Risk: Incomplete Extraction** - May miss some coupling during decomposition
  - Mitigation: Use grep to find all references before moving code
- **Risk: Single large PR complexity** - Review burden is high
  - Mitigation: Clear commit structure, well-documented changes
- **Risk: Re-export vs direct import decision** - Facade may add unnecessary indirection
  - Mitigation: Prefer direct imports to new module paths; facade only for gradual migration if needed

## Testing & Validation Strategy

### Unit Tests (New)

**test/internal/bidir/state_test.gleam:**
- Test all valid state transitions (Starting -> InitSent -> Running -> Stopped)
- Test all invalid transitions return `Error(InvalidTransition)`
- Test terminal states (Stopped, Failed) reject all events

**test/internal/bidir/routing_test.gleam:**
- Test `route_incoming` for each `IncomingMessage` variant
- Test control request routing (HookCallback -> RouteHookCallback, CanUseTool -> RoutePermission, etc.)
- Test control response routing (Success/Error -> RouteResponse)
- Test regular messages route to subscriber

**test/internal/bidir/hooks_test.gleam:**
- Test hook dispatch with registered handler
- Test hook dispatch with missing handler (fail-open behavior)
- Test permission dispatch (fail-deny behavior)
- Test `build_hook_registrations` generates correct callback IDs

**test/internal/bidir/correlation_test.gleam:**
- Test `resolve_pending` sends `RequestSuccess` for Success response
- Test `resolve_pending` sends `RequestError` for Error response
- Test `cancel_pending` sends appropriate error
- Test `timeout_pending` behavior

### Integration Tests

- Existing actor tests validate composition works correctly
- No new integration tests required (existing coverage sufficient)
- `e2e/sdk_bidir_test.gleam`: Verifies the re-assembled bidir actor works correctly

### Regression Tests

- All tests in `test/` must pass unchanged (only setup/import adaptation)
- Verify `BidirOptionIgnored` warning no longer emitted (code path removed)

### Manual Verification

- `gleam build` succeeds with no warnings
- `gleam test` passes all tests
- `wc -l src/claude_agent_sdk/internal/bidir/*.gleam` shows no module >600 lines

### Acceptance Criteria Coverage

**Primary Behavioral Invariants (must pass):**

| Invariant | Verification |
|-----------|--------------|
| Pure modules have no side-effect imports | `grep -l "port_ffi\|gleam_otp/actor" bidir/{state,routing,hooks,correlation}.gleam` returns empty |
| One-way dependency graph | No module in `bidir/*` imports from `bidir/actor` (verify via grep) |
| `transition()` and `route_incoming()` are pure | Unit tests pass without `process.sleep()` or actor spawning |
| Actor shell performs all side effects | All `port_ffi` and timer calls only in `bidir/actor.gleam` |

**Secondary Metrics (sanity checks):**

| Spec AC | Covered By |
|---------|------------|
| No single module exceeds 600 lines | File size verification via `wc -l` |
| Lifecycle transitions unit-testable without spawning processes | `bidir/state_test.gleam` pure unit tests |
| Hook dispatch testable without starting full actor | `bidir/hooks_test.gleam` pure unit tests |
| Error types importable without pulling in actor internals | Session errors in `error.gleam`; pure modules don't import StartError |
| `query()` signature does not mention bidir concepts | `CliOptions` + `SdkOptions` only for query path |
| No "ignored option" warnings needed in query path | BidirOptions not accepted by query() |
| Tests construct minimal configs | Separate builder functions per type |
| No single record type exceeds 12 fields | CliOptions: 12, SdkOptions: 4, BidirOptions: 12 |

## Open Questions

None - all critical decisions resolved:
1. ~~Scope~~ -> High priority only
2. ~~Error type location~~ -> error.gleam
3. ~~QueryOptions backwards compat~~ -> Clean break (no wrapper)
4. ~~Phasing~~ -> Single large PR
5. ~~Extraction strategy~~ -> Top-down (interfaces first)
6. ~~CI/CD gates~~ -> gleam test before PR
7. ~~External consumers~~ -> None (safe to refactor)

## Next Steps

After this plan is approved, run `/create-tasks` to generate:
- `--beads` -> Beads issues with dependencies for multi-agent execution
- (default) -> TODO.md checklist for simpler tracking
