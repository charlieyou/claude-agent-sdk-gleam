# Non-Mock Testing Strategy for Stream/Runner Paths

Date: 2026-01-11

## Goal
Replace mock/fake test_runner usage with real, deterministic system interactions while keeping tests reliable and cross-platform.

## Constraints
- No test_runner or ETS-backed fake stream state in unit tests.
- Must not require the real Claude CLI for unit/phase0 tests.
- Must remain cross-platform (Linux/macOS/Windows).

## Strategy Overview
Use a deterministic local helper command for stream semantics and port behavior tests. Prefer OS-provided echo commands for minimal dependencies, or introduce a tiny helper binary/script if OS echo is insufficient.

### Option A: OS Echo Command (Preferred minimal change)
- Linux/macOS: `/bin/echo` with known payloads
- Windows: `cmd.exe /c echo <payload>`
- Pros: zero new binaries, already used in phase0 runtime tests
- Cons: limited control over timing and multi-line NDJSON streams

### Option B: Minimal Helper Binary (Recommended for full stream semantics)
- A small helper executable that emits controlled NDJSON lines with delays and exit statuses.
- Can be committed to repo or built on the fly in CI.
- Pros: deterministic, can simulate edge cases (invalid JSON, partial lines, exit before result)
- Cons: additional build step and platform support

## Recommended Approach
- Keep phase0 runtime tests on OS echo (no change).
- Introduce a tiny helper for stream semantics tests that need:
  - Multiple NDJSON lines
  - Partial line splits
  - Controlled exit statuses
  - Delays/timeouts
- Make the helper optional and skip those tests with clear messages if not present.

## Test Mapping (Current -> Proposed)
- `test/stream_test.gleam`:
  - Replace test_runner-based stream semantics with helper-driven real port reads.
  - Keep pure buffer/normalize functions as unit tests.
- `test/resource_test.gleam`:
  - Replace close-tracking via ETS with helper-based assertions (e.g., confirm process terminates and close idempotency).

## Reliability Considerations
- Use timeouts with clear diagnostics.
- Keep payloads small and deterministic.
- Isolate tests that rely on the helper behind an env flag if necessary (e.g., `STREAM_HELPER=1`).

## Acceptance Criteria
- No usage of `test_runner()` in test modules.
- All stream/runner behaviors are validated against real port_ffi execution.
- Test suite remains deterministic on CI across platforms.
