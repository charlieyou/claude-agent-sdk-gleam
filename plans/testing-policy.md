# Testing Policy: No Mocks/Fakes + Coverage Criteria

Date: 2026-01-11
Scope: claude_agent_sdk test strategy

## Goals
- Provide high confidence in correctness without relying on mock or fake implementations of the CLI/runner/ports.
- Make it explicit which behaviors are validated by unit, integration, phase0 runtime, and E2E tests.
- Define what "full unit test coverage" means for this repository.

## Definitions
- Mock/Fake/Stub: Any test double that simulates an external boundary or internal subsystem instead of using the real implementation.
  - Examples: test_runner() that fakes CLI output, ETS-backed fake stream state, stubbed port reads.
- Fixture: Static data captured from real outputs or spec examples (e.g., JSON payloads) used for deterministic parsing tests.
  - Fixtures are allowed if they represent real or spec-defined data, not simulated behavior of an entire subsystem.
- External boundary: Claude CLI, OS process/port subsystem, filesystem, environment, network/auth.

## Policy: "No Mocks/Fakes" (Strict)
Disallowed:
- test_runner-based simulation of the CLI stream
- ETS-backed fake stream state used to mimic port behavior
- Any stubbed port read/close or spawn path that bypasses port_ffi

Allowed:
- Pure-function unit tests (parsers, builders, option resolution, type-level checks)
- Real port_ffi interactions against a deterministic local helper (e.g., /bin/echo or a tiny helper binary)
- Real Claude CLI integration tests (env-gated)
- Fixtures representing real or spec-defined JSON payloads (used to test decoder behavior)
- Environment variable reads using real FFI access

## Test Categories and Boundaries
- Unit tests (no external boundaries):
  - Pure parsing and validation logic
  - Option builders and precedence
  - Type-level construction/matching
  - Error formatting/diagnostics

- Phase 0 runtime tests (real port_ffi):
  - Spawn/read/exit/close behavior using a real OS command
  - Validates FFI wiring and runtime primitives

- Integration tests (real CLI):
  - PATH lookup
  - CLI version detection
  - Auth availability
  - Real query and stream behavior

- E2E integration tests (scripted workflows):
  - Full, user-like flow with real CLI
  - Multi-step scenarios with detailed logging and artifact capture

## "Full Unit Test Coverage" Definition
"Full unit coverage" means:
- Every public API function has at least one unit test covering its primary behavior.
- Every pure function and internal helper reachable from public API has tests for:
  - Success path
  - Error path(s) and edge cases
- Coverage threshold targets (per-module unless excluded):
  - Line coverage: >= 95%
  - Branch coverage: >= 90%
- Explicitly excluded from unit coverage (must be covered by phase0/integration/E2E):
  - Any function that spawns processes or uses port_ffi
  - CLI version detection that calls the OS
  - PATH lookup or auth checks

## Gating and Environment Variables
- Unit tests: default `gleam test`
- Phase 0 runtime tests: run with `gleam test`
- Integration tests: `CLAUDE_INTEGRATION_TEST=1`
- Decoder tests: should run by default once decoders are implemented; avoid permanent skip gates

## Acceptance Criteria (Policy)
- No test module uses test_runner() or ETS-based stream mocks once policy is adopted.
- Any behavior requiring the CLI or OS ports is covered by phase0/integration/E2E tests.
- Coverage reporting exists and can verify unit coverage thresholds for eligible modules.
- README/docs updated to reflect new policy and how to run each test category.
