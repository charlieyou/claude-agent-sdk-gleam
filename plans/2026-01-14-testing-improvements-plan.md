# Implementation Plan: Testing Improvements

## Context & Goals
- **Spec**: `plans/testing-improvements.md`
- **Critical Blocker**: Fix CI E2E job to actually run real CLI tests (e2e job → "Run E2E tests" step runs `gleam test` without `-- --e2e` — tests always skip)
- Strengthen integration/E2E coverage for real-world breakage risk
- Add crash handling, concurrency, backpressure, permission, hook timeout, and network failure test coverage
- Establish contract testing to detect CLI output drift and ensure SDK resilience

## Scope & Non-Goals

### In Scope
- Fix CI to run `gleam test -- --e2e` (spec #1) — **Phase 1 blocker**
- Add crash/interruption handling test (spec #2)
- Add parallel session/concurrency test (spec #3)
- Add backpressure + large payload test (spec #4)
- Add CLI output drift contract tests (spec #5)
- Add permission/denied flow test (spec #6)
- Add hook timeout test (spec #7)
- Add opt-in soak test (spec #9)
- Add OS coverage for Linux + macOS (spec #10)
- Add network/credential failure test (spec #11)

### Out of Scope (Non-Goals)
- **CLI version matrix** (spec #8): Deferred — start with `@latest` only
- **Windows support**: Skipped entirely per user decision
- Unit test coverage improvements (not in spec)
- Performance optimization of tests themselves
- Changes to core SDK implementation

## Assumptions & Constraints

### Assumptions
- Claude CLI v2.1.4+ behavior matches current SDK decoders
- `anthropic` CLI is installed and available in the test environment (local and CI)
- `ANTHROPIC_API_KEY` is available in CI secrets (already configured)
- macOS runners available in GitHub Actions

### Implementation Constraints
- **Extend existing infrastructure**: All new tests use existing `helpers.gleam` utilities and `e2e_helpers_ffi.erl` — no new test frameworks
- **ETS lock bypass**: Use `E2E_ALLOW_CONCURRENT=1` env var to skip lock acquisition for concurrency tests only
- **Crash handling**: Primary approach is port-based (`port_close/1`). If SDK doesn't expose port, fallback to OS-level PID kill via `os:cmd("kill -9 " ++ Pid)` using the existing `kill_pid/1` FFI helper
- **Golden transcript maintenance**: Derive required fields from existing decoder functions in `src/claude_agent_sdk/message.gleam`
- All new tests must follow existing patterns in `test/e2e/`
- Tests must use structured logging to `artifacts/e2e/`

### Testing Constraints
- E2E tests require `--e2e` flag and CLI installed
- Soak tests require `--soak` flag (separate opt-in)
- API costs should be minimized (short prompts, `max_turns=1` where possible)
- Contract tests should run offline against golden files (no API calls)
- **Reliability**: E2E tests are prone to flakiness — new tests (especially concurrency and network) must be robust (retries where appropriate, distinct resources)

### Flag Plumbing (How --e2e and --soak work)
Flags are passed after `--` to Gleam test and read via Erlang init args:
- `gleam test -- --e2e` → Erlang `init:get_plain_arguments()` returns `["--e2e"]`
- Existing `e2e_helpers_ffi:get_plain_args/0` wraps this and returns the list
- `has_e2e_flag()` in `helpers.gleam` checks if `"--e2e"` is in the args list
- New `has_soak_flag/0` will follow the same pattern for `"--soak"`

**Example commands:**
```bash
# Local E2E (skips soak)
gleam test -- --e2e

# Local E2E + soak
gleam test -- --e2e --soak

# CI (in test.yml e2e job)
gleam test -- --e2e
```

## Integration Analysis

### Existing Mechanisms Considered

| Existing Mechanism | Could Serve Feature? | Decision | Rationale |
|--------------------|---------------------|----------|-----------|
| `test/e2e/helpers.gleam` | Yes | **Extend** | Add `skip_if_no_soak()`, `get_stream_port()`, `with_concurrent_mode()`, contract helpers |
| `test/e2e_helpers_ffi.erl` | Yes | **Extend** | Add `port_close/1` wrapper, concurrent-mode bypass in `acquire_lock/0`, `has_soak_flag/0` |
| `.github/workflows/test.yml` | Yes | **Modify** | Fix `--e2e` flag (e2e job, "Run E2E tests" step), add OS matrix |
| `test/fixtures/` | Yes | **Extend** | Add `golden/` subdirectory for contract transcripts |
| `test/support/full_mock_runner.gleam` | Partial | Reuse patterns | For offline contract validation logic |
| `src/claude_agent_sdk/message.gleam` | Reference | Read-only | Derive required fields from decoder functions |

### Integration Approach
- All new E2E tests extend existing `helpers.gleam` utilities — no new test frameworks
- Lock bypass implemented via env var check in existing `acquire_lock/0` function
- Port tracking added to existing stream spawn mechanism
- Contract tests reuse existing fixture infrastructure pattern
- CI changes are additive (matrix jobs) except for the critical `--e2e` fix

## Prerequisites
- [x] Claude CLI v2.1.4+ installed for E2E (handled by CI npm install step)
- [x] `ANTHROPIC_API_KEY` configured in CI secrets (already present)
- [ ] macOS runner available in GitHub Actions (for OS matrix)
- [ ] Verify port reference is accessible from `QueryStream` state, OR confirm existing `kill_pid/1` FFI can be used as fallback (no SDK change needed if PID fallback works)

## High-Level Approach

The implementation follows the spec's 11 items, prioritized by impact:

**Phase 1: CI Fix (Critical Blocker)**
- Fix `.github/workflows/test.yml` e2e job → "Run E2E tests" step:
  - **Before:** `run: gleam test`
  - **After:** `run: gleam test -- --e2e`
- This unblocks all E2E testing in CI — currently tests always skip

**Phase 2: Helper Extensions**
- Add FFI for port tracking and concurrent-mode bypass
- Add Gleam helper functions for crash test, concurrency test, and soak test gating

**Phase 3: Core E2E Tests (High Value)**
- Crash handling (#2) — kill CLI mid-stream, verify recovery
- Concurrency (#3) — bypass lock, run parallel queries
- Backpressure (#4) — slow consumer test
- Permission denied (#6) — Bash tool trigger with deny handler
- Hook timeout (#7) — slow hook with timeout verification
- Network failure (#11) — invalid API key test

**Phase 4: Contract Infrastructure**
- Capture golden transcripts from real CLI
- Implement contract validation against decoder requirements
- Add contract test module (#5)

**Phase 5: CI Matrix & Soak**
- Add OS matrix: Linux + macOS (Windows skipped)
- Add opt-in soak test (#9) with `--soak` flag

## Technical Design

### Architecture

**Test Organization:**
```
test/e2e/
├── helpers.gleam              # Extended with crash/concurrency/soak helpers
├── sdk_crash_test.gleam       # New: crash/interruption tests
├── sdk_concurrency_test.gleam # New: parallel session tests
├── sdk_backpressure_test.gleam # New: slow consumer tests
├── sdk_contract_test.gleam    # New: golden transcript validation
├── sdk_permission_test.gleam  # New: permission denied flows
├── sdk_hook_timeout_test.gleam # New: hook timeout behavior
├── sdk_soak_test.gleam        # New: long-running stability test
├── sdk_network_test.gleam     # New: credential/network failure
└── ... (existing tests)

test/fixtures/
└── golden/                    # New: golden transcripts for contracts
    ├── basic_query.ndjson
    └── tool_use.ndjson
```

**CI Matrix:**
```yaml
e2e:
  strategy:
    matrix:
      os: [ubuntu-latest, macos-latest]  # Windows skipped
```

### Data Model
N/A — no persistent data changes

### API/Interface Design

**New `helpers.gleam` functions:**
```gleam
/// Check for --soak flag (opt-in for soak tests)
pub fn skip_if_no_soak() -> Result(Nil, String)

/// Get port reference from stream (for crash test)
/// Port stored in stream state during spawn
pub fn get_stream_port(stream: QueryStream) -> Option(Port)

/// Force-close the CLI port (for crash test)
pub fn force_close_port(port: Port) -> Result(Nil, String)

/// Run function with concurrent mode enabled (bypass ETS lock)
pub fn with_concurrent_mode(f: fn() -> a) -> a

/// Validate message list against golden transcript
pub fn validate_contract(
  messages: List(MessageEnvelope),
  golden_path: String
) -> Result(Nil, ContractError)
```

**New `e2e_helpers_ffi.erl` exports:**
```erlang
%% Check if E2E_ALLOW_CONCURRENT env var is set
is_concurrent_mode() -> boolean().

%% Close an Erlang port immediately
port_close_safe(Port) -> ok | {error, Reason}.

%% Check for --soak in plain arguments
has_soak_flag() -> boolean().
```

### File Impact Summary

| Path | Status | Description |
|------|--------|-------------|
| `.github/workflows/test.yml` | Exists | e2e job → "Run E2E tests" step: `gleam test` → `gleam test -- --e2e`; add OS matrix |
| `test/e2e/helpers.gleam` | Exists | Add `skip_if_no_soak()`, port helpers, concurrent mode, contract validation |
| `test/e2e_helpers_ffi.erl` | Exists | Add `is_concurrent_mode/0`, `port_close_safe/1`, `has_soak_flag/0`; modify `acquire_lock/0` |
| `test/e2e/sdk_crash_test.gleam` | **New** | Crash/interruption test |
| `test/e2e/sdk_concurrency_test.gleam` | **New** | Parallel session test |
| `test/e2e/sdk_backpressure_test.gleam` | **New** | Slow consumer test |
| `test/e2e/sdk_contract_test.gleam` | **New** | Golden transcript validation |
| `test/e2e/sdk_permission_test.gleam` | **New** | Permission denied flows |
| `test/e2e/sdk_hook_timeout_test.gleam` | **New** | Hook timeout behavior |
| `test/e2e/sdk_soak_test.gleam` | **New** | Long-running stability test |
| `test/e2e/sdk_network_test.gleam` | **New** | Credential/network failure |
| `test/fixtures/golden/` | **New** | Golden transcript directory |
| `test/fixtures/golden/basic_query.ndjson` | **New** | Golden transcript for basic query |

## Detailed Test Designs

### #1: CI E2E Fix
**File:** `.github/workflows/test.yml`
**Location:** Job `e2e` → Step `name: Run E2E tests`
**Change:**
```yaml
# Before
- name: Run E2E tests
  if: steps.prereq.outputs.skip != 'true'
  run: gleam test

# After
- name: Run E2E tests
  if: steps.prereq.outputs.skip != 'true'
  run: gleam test -- --e2e
```
**AC:** E2E tests no longer skip in CI when triggered (verify by checking test output for actual test runs, not skip messages)

### #2: Crash/Interruption Handling
**File:** `test/e2e/sdk_crash_test.gleam`
**Design:**
1. Start streaming query with `query()`
2. After first message, terminate CLI process:
   - **Primary:** If port accessible via `get_stream_port()`, use `port_close/1`
   - **Fallback:** If port not exposed, use existing `kill_pid/1` FFI with OS-level PID lookup
3. Assert: stream terminates with terminal error within 5s (not hang)
4. Assert: `close()` returns cleanly (no crash)
5. Assert: follow-up query succeeds (recovery)

**Approach:** Primary is port-based termination. If `QueryStream` doesn't expose port (and SDK changes are out of scope), fallback to finding the CLI child process via `os:cmd("pgrep -P " ++ ParentPid)` or storing PID at spawn time. The existing `kill_pid/1` FFI already handles process termination.

**AC:** Test fails if stream hangs >5s or if subsequent query fails after forced crash

### #3: Parallel Session/Concurrency
**File:** `test/e2e/sdk_concurrency_test.gleam`
**Design:**
1. Set `E2E_ALLOW_CONCURRENT=1` env var (or use `with_concurrent_mode()`)
2. Spawn 3-5 concurrent queries via `process.spawn_unlinked`
3. Track `session_id` from each stream's SystemMessage
4. Assert: no cross-session contamination (all session_ids distinct)
5. Assert: all streams terminate normally within timeout

**Containment strategy (prevent false failures from shared state):**
- Each test run uses unique artifact subdirectory: `artifacts/e2e/concurrency-<timestamp>/`
- Use deterministic, distinct prompts per session: "Say 'session-1'", "Say 'session-2'", etc.
- CLI doesn't share on-disk auth state during queries (auth happens at startup)
- If rate-limit errors occur, they're logged but test passes if isolation is maintained

**Approach:** Check `E2E_ALLOW_CONCURRENT=1` env var in `acquire_lock()` — if set, skip lock acquisition entirely.

**AC:** Fails on cross-session contamination (session_id leaks) or non-termination (>30s)

### #4: Backpressure + Large Payload
**File:** `test/e2e/sdk_backpressure_test.gleam`
**Design:**
1. Request large output (prompt: "Generate a 1000-word essay")
2. Consume slowly (`process.sleep(100)` between `next()` calls)
3. Assert: no crash, no hang
4. Assert: stream completes OR terminates with acceptable error

**Acceptable outcomes (allowlist):**
- ✅ Normal completion (Result message received)
- ✅ `BufferOverflow` or `BackpressureError` with stream closed cleanly
- ✅ `MaxOutputExceeded` error (CLI-side limit)
- ❌ Generic timeout (test fails)
- ❌ Unclassified/unknown error (test fails)
- ❌ Stream hangs >60s (test fails)

**AC:** Test passes only if outcome is in the allowlist above. Test must complete within 60s.

### #5: CLI Output Drift Contract Tests
**File:** `test/e2e/sdk_contract_test.gleam`
**Design:**
1. Load golden transcripts from `test/fixtures/golden/`
2. Parse each NDJSON line
3. For each message type, validate required fields exist
4. Run offline (no CLI needed)

**Contract field derivation rule:**
A field is **required** if removing it from the JSON causes the corresponding `decode_*` function to return `Error`. Concretely:
- `type` field: Always required (dispatch key)
- For `SystemMessage`: `session_id` optional (has `Option` type)
- For `AssistantMessage`: `content` required (decoder fails without it)
- For `ResultMessage`: `result` required
- Implementer: Create a small test helper that attempts decode of each message type with fields removed to generate the authoritative required-field list.

**Approach:** Build required-field list by testing which fields cause decoder failure. Store as a simple map in the contract test module:
```gleam
const required_fields = [
  #("system", ["type"]),
  #("assistant", ["type", "content"]),
  #("result", ["type", "result"]),
  ...
]
```

**AC:** CI fails if any required field is missing from golden transcripts or live CLI output

### #6: Permission Denied Flows
**File:** `test/e2e/sdk_permission_test.gleam`
**Design:**
1. Configure permission handler that returns `Deny` for tool use
2. Send prompt: "Run `echo hello`" (triggers Bash tool)
3. Assert: Bash tool does not execute
4. Assert: SDK surfaces denial in message stream

**Approach:** Use Bash tool trigger — prompt like "Run `echo hello`" reliably triggers Bash tool permission request.

**AC:** Fails if tool executes despite deny

### #7: Hook Timeout Coverage
**File:** `test/e2e/sdk_hook_timeout_test.gleam`
**Design:**
1. **Discovery phase (implementation prerequisite):**
   - Check SDK source for hook timeout configuration
   - If found, document value in this plan and test against it
   - If not found, add a short note to SDK docs and test defines the contract
2. Register slow PreToolUse hook (sleeps 2x expected timeout)
3. Trigger tool use
4. Assert: behavior matches documented policy (fail-open or fail-closed)
5. Log timeout reason in structured format

**Timeout behavior anchoring:**
- Before implementing, grep SDK for `timeout`, `hook`, `deadline` to find existing behavior
- If SDK has configurable timeout: test with default value
- If SDK has hardcoded timeout: test asserts that exact behavior
- If no timeout mechanism exists: test documents that hooks can block indefinitely (and consider adding timeout as follow-up)

**AC:** Test documents and asserts actual SDK timeout behavior. If behavior is truly undefined, test is gated behind `E2E_ASSERT_HOOK_TIMEOUT=1` until behavior is confirmed.

### #8: CLI Version Matrix
**File:** `.github/workflows/test.yml`
**Decision:** Deferred — start with `@latest` only.
- If breakage incidents occur, add specific version pinning later
- Simpler CI, lower maintenance burden initially

### #9: Opt-in Soak Test
**File:** `test/e2e/sdk_soak_test.gleam`
**Design:**
1. Skip unless `--soak` flag provided (via `skip_if_no_soak()`)
2. Run 20-50 sequential queries
3. After each query, log: process count, ETS table count, port count
4. Assert: no resource leaks (counts don't grow unbounded)
5. Assert: all queries complete successfully

**Opt-in:** Requires `--soak` command-line flag. Not run in normal E2E suite.

**AC:** Passes without resource exhaustion; log resource metrics per step

### #10: OS Coverage
**File:** `.github/workflows/test.yml`
**Design:**
```yaml
strategy:
  matrix:
    os: [ubuntu-latest, macos-latest]
```
- Validates path handling, port behavior, encoding across Linux/macOS
- Windows skipped per user decision

**AC:** Scheduled E2E runs complete on Linux and macOS

### #11: Network/Credential Failure
**File:** `test/e2e/sdk_network_test.gleam`
**Design:**
1. Spawn CLI in subprocess with isolated env (NOT mutating global env)
2. Pass invalid API key via explicit env map to CLI spawn
3. Attempt query
4. Assert: authentication error surfaced with clear type
5. Assert: error maps to SDK error enum (e.g., `AuthenticationError`)

**Isolation strategy (prevent leaking invalid key to other tests):**
```gleam
// In helpers or test: spawn CLI with explicit env override
let env = [#("ANTHROPIC_API_KEY", "invalid-test-key-12345")]
let stream = query_with_env(prompt, options, env)  // New helper variant
```
- Do NOT use `os:putenv` which affects the entire BEAM process
- Either add `query_with_env()` helper, or run this test in a separate OS subprocess
- Simpler alternative: run this specific test last in the suite and restore env after

**AC:** Fails if error is ambiguous or not mapped to expected SDK error. Other tests in suite must not be affected by invalid key.

## Risks, Edge Cases & Breaking Changes

### Edge Cases & Failure Modes
- **Partial output on crash:** CLI killed between messages — test must handle incomplete transcript
- **Hook timeout at boundary:** Exact timeout millisecond edge case — test accepts documented behavior
- **Network failure mid-stream vs before:** Test covers auth error; mid-stream network drop is separate concern
- **Concurrent query race:** Lock bypass may expose unexpected port interactions — test validates isolation
- **CI Failures:** Fixing the CI flag might reveal that existing E2E tests are already broken (since they weren't running). **Mitigation**: Fix existing tests immediately if they fail upon enabling.
- **Concurrency Flakiness:** Parallel tests might compete for rate limits or system resources. **Mitigation**: Use distinct prompts/tools if possible, or robust retries.
- **Environment Differences:** macOS vs Linux CLI behavior might differ slightly. **Mitigation**: Conditional logic in tests or OS-agnostic assertions.

### Risks
- **Golden transcript maintenance:** CLI updates may change output format — contract tests will fail until updated
- **API cost:** Soak test runs many queries — keep opt-in only

### Breaking Changes & Compatibility
- **No breaking changes** — all changes are additive to test infrastructure
- Existing tests unaffected
- CI behavior unchanged except E2E now actually runs when triggered

## Testing & Validation Strategy

### Unit Tests
- Contract validation logic can have unit tests against fixture files (no CLI needed)

### Integration / End-to-End Tests
- All new tests are E2E by nature (test real SDK to CLI interaction)

### Manual Validation
- [ ] Run full E2E suite locally with `gleam test -- --e2e`
- [ ] Verify CI E2E job no longer shows skipped tests
- [ ] Verify golden transcripts match current CLI output format
- [ ] Run soak test locally with `gleam test -- --e2e --soak`

### Monitoring / Observability
- All tests use existing structured logging to `artifacts/e2e/<test-id>.log`
- Resource metrics logged during soak test

### Acceptance Criteria Coverage

| Spec AC | Covered By |
|---------|------------|
| #1: CI runs real E2E | CI fix (e2e job → "Run E2E tests" step), manual verification |
| #2: Stream terminates with error (not hang) on crash, recovery works | `sdk_crash_test.gleam` |
| #3: No cross-session contamination | `sdk_concurrency_test.gleam` with session_id tracking |
| #4: No crash, buffer handled | `sdk_backpressure_test.gleam` |
| #5: CI fails if required fields missing | `sdk_contract_test.gleam` with golden transcripts |
| #6: Tool does not execute on deny | `sdk_permission_test.gleam` |
| #7: Timeout behavior matches expected policy | `sdk_hook_timeout_test.gleam` |
| #8: Version matrix passes | Deferred — start with @latest only |
| #9: No resource leaks over 20-50 queries | `sdk_soak_test.gleam` |
| #10: E2E completes on Linux + macOS | CI matrix, upload artifacts |
| #11: Auth error surfaced clearly | `sdk_network_test.gleam` |

## Open Questions

**To be discovered during implementation:**
- **Hook timeout default value:** Search SDK for existing timeout mechanism; if found, document and test against it; if not, test gates behind env flag until behavior confirmed
- **Port reference access:** Verify `QueryStream` state exposes port; if not, use PID fallback via existing `kill_pid/1` FFI (no SDK change needed)

**Deferred decisions (from Codex interview):**
- **Golden file update workflow:** Should golden files be stable API (fail on diff) or support "record mode" for intentional updates? — Start with fail-on-diff, add record mode if maintenance burden high
- **Concurrency level:** 3-5 sessions validated; higher levels (10+) deferred unless needed
- **CI trigger changes:** Keep current gating (label/schedule/dispatch) vs run E2E on every PR — Start with current gating, evaluate after fixing the `--e2e` flag
- **macOS runner frequency:** Run on every allowed trigger vs only schedule/dispatch — Start with every allowed trigger
- **Artifact upload behavior:** Upload on failure only vs also on success — Start with failure-only, add success if debugging needs arise

## Next Steps

After this plan is approved, run `/create-tasks` to generate:
- `--beads` → Beads issues with dependencies for multi-agent execution
- (default) → TODO.md checklist for simpler tracking
