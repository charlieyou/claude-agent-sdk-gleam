# Architecture Remediation Plan (2026-01-15)

## Scope
This plan addresses remaining issues from `plans/2026-01-14-architecture-review.md` without modifying code yet. Focus areas:
- Extract line framing from `internal/stream.gleam`
- Unify hook/permission callback dispatch
- Reduce `SessionState` size via sub-records
- Centralize port I/O behind `port_io`
- Reduce decoder repetition in `internal/decoder.gleam`
- Continue decomposing `internal/bidir/actor.gleam` (already split but still oversized)

## Current State (Quick Check)
- `internal/bidir.gleam` is a thin facade, but `internal/bidir/actor.gleam` remains ~2800 LOC.
- Split options exist (`CliOptions`, `SdkOptions`, `BidirOptions`) and `query_new` uses them; legacy `QueryOptions` still in use and warns about ignored bidir options.
- Error types consolidated under `error.gleam`.
- `stream.gleam` still contains both line framing and iterator logic.
- Hook/permission async dispatch paths remain duplicated.
- `SessionState` remains a flat record.
- `port_ffi` is used directly in multiple modules.
- Decoder repetition persists.

## Plan (No Code Changes Yet)

### 1) Extract line framing
**Goal**: Separate line buffering/CRLF normalization from stream iteration.

- Target new module: `src/claude_agent_sdk/internal/line_framing.gleam`
- Move:
  - `LineBuffer` type
  - `append_to_buffer`, `read_line`, and helper types/results
  - `handle_port_data` (or split into pure + wrapper)
- Update imports:
  - `internal/stream.gleam` uses `line_framing` for buffer ops
  - `internal/bidir/actor.gleam` uses `line_framing.LineBuffer`
- Tests:
  - Extract or create unit tests for line framing (CRLF, empty input, overflow behavior)

### 2) Unify async callback dispatch
**Goal**: Remove duplication between hook and permission async paths.

- Introduce a shared async callback runner in `internal/bidir/actor.gleam` or a new `internal/bidir/callbacks.gleam` module.
- Normalize behavior:
  - Single function handles capacity checks, spawn, timer scheduling, pending bookkeeping
  - Policy switch for fail-open vs fail-deny
- Use in:
  - `dispatch_hook_callback`
  - `dispatch_permission_callback`
- Keep sync permission hook path (`dispatch_permission_hook_callback`) if required, but reduce overlap
- Tests:
  - Unit test for unified callback runner with both policies
  - Ensure existing hook/permission tests remain unchanged

### 3) Group `SessionState`
**Goal**: Reduce flat record size and clarify mutation boundaries.

- Define sub-records in `internal/bidir/actor.gleam`:
  - `SessionConfig`: static config (hooks, timeouts, file_checkpointing, mcp_handlers)
  - `RuntimeState`: lifecycle, runner, subscriber, self_subject, capabilities
  - `PendingOps`: pending_requests, pending_hooks, queued_ops
  - `Timers`: init_timeout_ms, init_timer_ref
  - `Buffers`: line_buffer
- Replace `SessionState` fields with grouped records
- Update all field references to use sub-records
- Tests: ensure no behavior change (existing tests should pass)

### 4) Centralize port I/O
**Goal**: Restrict direct `port_ffi` usage to a single interface.

- Introduce `internal/port_io.gleam` with a minimal interface:
  - `open_port`, `close_port`, `connect_port`, `receive_timeout`, `receive_blocking`, `write`, `monotonic_time_ms`
- Update:
  - `internal/cli.gleam`
  - `internal/bidir_runner.gleam`
  - `internal/stream.gleam`
  - `internal/bidir/actor.gleam`
- Keep `port_ffi` as the concrete implementation
- Tests: provide fake port_io for unit tests where needed

### 5) Decoder repetition cleanup
**Goal**: Reduce boilerplate in `internal/decoder.gleam`.

- Add helper: `decode_with_error_wrap(decoder, context) -> Result`
- Use in each message decoder to centralize error formatting
- Keep decoder logic intact; only refactor structure
- Tests: existing decoder tests should pass

### 6) Continue actor decomposition
**Goal**: Move pure logic out of `internal/bidir/actor.gleam` to reduce size and improve testability.

- Candidate extractions:
  - `handle_event(state, event)` reducer (pure)
  - `build_selector` construction for port/select handlers
  - Pending cleanup logic and queue flush logic
- Use extracted pure functions in actor shell
- Add unit tests for reducers without OTP

## Execution Order (Recommended)
1. Line framing extraction (low risk, contained)
2. Decoder helper (low risk, isolated)
3. Callback unification (medium risk, targeted)
4. SessionState grouping (medium risk, wide edits)
5. Port IO abstraction (medium-high risk, cross-module)
6. Actor decomposition (high risk, multi-module behavior changes)

## Open Questions
- Should `QueryOptions` be deprecated further or removed in next major version?
- Is `dispatch_permission_hook_callback` intended to remain sync, or should it share async path with policy control?
- Is there an existing test harness for port IO that can be reused?

## Deliverables
- Refactor plan only (this document)
- No code changes until approved
