# Testing

This document covers the SDK's testing strategy, including the no-mock policy,
coverage thresholds, and how to run and interpret each test category.

## No-Mock Policy

This SDK uses a **strict no-mock policy** for production code paths tested via
E2E tests with the real Claude CLI. Mock runners (`test_runner`,
`mock_bidir_runner`) are used only in integration-style tests outside the E2E
suite to validate protocol logic in isolation without requiring CLI access.

### Disallowed in E2E Tests

- Test runners that simulate CLI output
- ETS-backed fake stream state
- Stubbed port read/close operations
- Any test double that simulates external boundaries

### Allowed

- Pure-function unit tests (parsers, builders, option resolution)
- Real port_ffi against deterministic helpers (`/bin/echo`)
- Real CLI E2E tests (gated by `--e2e` flag)
- Fixtures representing real or spec-defined JSON payloads
- Offline protocol tests using mock runners (for deterministic testing)

See [plans/testing-policy.md](../plans/testing-policy.md) for full policy details.

## Test Categories

### Unit Tests (Pure Logic)

Run with `gleam test`:

- Pure parsing and validation logic
- Option builders and precedence
- Type-level construction/matching
- Error formatting/diagnostics
- Decoder functions with fixtures

These tests have no external dependencies and run fast.

### Phase 0 Runtime Tests

Tests that use real port_ffi against deterministic OS commands:

- Spawn/read/exit/close behavior using `/bin/echo` or similar
- Validates FFI wiring and runtime primitives

These run by default with `gleam test`.

### Offline Protocol Tests (Integration)

Mock-based protocol tests live in `test/` (not `test/e2e/`) and run as
integration-style tests for deterministic behavior without real CLI access.

### Integration Tests

Located in `test/query_integration_test.gleam`:

- PATH lookup and CLI discovery
- CLI version detection via `claude --version`
- Auth status checks via real CLI
- Preflight validation

**Note:** These tests interact with the real Claude CLI when available. Tests
are guarded to skip gracefully if the CLI is not in PATH or auth is unavailable.
In CI environments, these preflight checks may be skipped or mocked via fixtures.

### E2E Tests (Real CLI)

Full scripted workflows with real Claude CLI. Require `--e2e` flag:

- Multi-step scenarios with detailed logging
- Artifact capture for debugging
- Real API calls (incurs costs)

```bash
gleam test -- --e2e
```

See [E2E_TESTING.md](E2E_TESTING.md) for detailed E2E documentation.

## Coverage Reporting

### Quick Start

```bash
# Run coverage report
./scripts/coverage.sh

# JSON output for CI
./scripts/coverage.sh --json

# Enforce threshold (95% line coverage)
./scripts/coverage.sh --threshold 95.0
```

### Coverage Thresholds

For **eligible modules** (pure logic, no OS boundary code):

- Line coverage: >= 95%

Run `./scripts/coverage.sh --threshold 95.0` to enforce.

Note: Erlang's `cover` module measures line coverage. Branch coverage is not
currently measured or enforced.

## Module Exclusions

The following modules are **excluded from coverage thresholds** because they
contain OS/process boundary code that cannot be unit tested without mocks:

| Module | Reason |
|--------|--------|
| `claude_agent_sdk/internal/port_ffi` | Erlang port operations (spawn, read, close) |
| `claude_agent_sdk/internal/cli` | CLI version detection uses port_ffi |
| `claude_agent_sdk_ffi` | Erlang FFI layer |
| `bidir_ffi` | Bidirectional protocol FFI |

The `cli` module is excluded because `detect_cli_version/2` spawns a real CLI
process via port_ffi. Other functions in the module are pure but the module
is excluded as a whole for simplicity.

These modules are covered by:

- Phase 0 runtime tests (port_ffi behavior)
- Integration tests (CLI discovery/version checks)
- E2E tests (`gleam test -- --e2e`)

## Running Tests

### Quick Reference

```bash
# Unit + offline + integration tests (default)
gleam test

# E2E tests with real CLI (requires CLI + auth, incurs API costs)
gleam test -- --e2e

# Coverage with threshold enforcement
./scripts/coverage.sh --threshold 95.0

# CI-friendly JSON output
./scripts/coverage.sh --json --threshold 95.0
```

### Test Gating Summary

| Category | How to Run | Requirements |
|----------|-----------|--------------|
| Unit | `gleam test` | None |
| Phase 0 | `gleam test` | None |
| Offline Protocol | `gleam test` | None |
| Integration | `gleam test` | CLI in PATH for full checks (gracefully skips if unavailable) |
| E2E (Real CLI) | `gleam test -- --e2e` | Claude CLI installed + authenticated |

### Prerequisites for E2E Tests

1. Claude CLI installed (`claude --version` works)
2. Authentication via `claude auth login` (interactive)

## Interpreting Results

### Unit Test Failures

- Check the specific assertion that failed
- Review the fixture or input data
- Ensure decoders match current spec

### Integration Test Failures

- Verify CLI is in PATH: `which claude`
- Verify authentication: `claude auth status`
- Check CLI version compatibility: `claude --version`

### E2E Test Failures

- Check artifact logs in `artifacts/e2e/<test-id>.log`
- Review structured event data for timeline
- See [E2E_TESTING.md](E2E_TESTING.md) for troubleshooting guide

### Coverage Failures

If coverage drops below threshold:

1. Run `./scripts/coverage.sh` to see current coverage
2. Identify uncovered lines in the report
3. Add tests for uncovered paths
4. Re-run with threshold enforcement
