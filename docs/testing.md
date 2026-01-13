# Testing

This document covers the SDK's testing strategy, including coverage thresholds
and module exclusions.

## Test Categories

### Unit Tests (Pure Logic)

Run with `gleam test`:
- Pure parsing and validation logic
- Option builders and precedence
- Type-level construction/matching
- Error formatting/diagnostics
- Decoder functions with fixtures

### Phase 0 Runtime Tests

Tests that use real port_ffi against deterministic OS commands:
- Spawn/read/exit/close behavior using `/bin/echo` or similar
- Validates FFI wiring and runtime primitives

### Integration Tests

Require `CLAUDE_INTEGRATION_TEST=1`:
- PATH lookup and CLI discovery
- CLI version detection
- Auth availability checks
- Real query and stream behavior

### E2E Tests

Full scripted workflows with real Claude CLI:
- Multi-step scenarios with detailed logging
- Artifact capture for debugging

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
- Line coverage: ≥95%
- Branch coverage: ≥90% (where measurable)

Run `./scripts/coverage.sh --threshold 95.0` to enforce.

## Module Exclusions

The following modules are **excluded from coverage thresholds** because they
contain OS/process boundary code that cannot be unit tested without mocks:

| Module | Reason |
|--------|--------|
| `claude_agent_sdk/internal/port_ffi` | Erlang port operations (spawn, read, close) |
| `claude_agent_sdk_ffi` | Erlang FFI layer |
| `bidir_ffi` | Bidirectional protocol FFI |

### Function-Level Exclusions

Within otherwise-eligible modules, these functions involve OS calls:

| Module | Function | Reason |
|--------|----------|--------|
| `cli` | `detect_cli_version/2` | Spawns CLI process |
| `cli` | `find_cli_path/0` | PATH environment lookup |
| `cli` | `check_cli_availability/0` | OS process execution |

These functions are covered by integration tests (`CLAUDE_INTEGRATION_TEST=1`),
not unit coverage thresholds.

## No-Mock Policy

This SDK uses a **strict no-mock policy**:

**Disallowed:**
- Test runners that simulate CLI output
- ETS-backed fake stream state
- Stubbed port read/close operations

**Allowed:**
- Pure-function unit tests
- Real port_ffi against deterministic helpers (`/bin/echo`)
- Real CLI integration tests (env-gated)
- Fixtures representing real JSON payloads

See [plans/testing-policy.md](../plans/testing-policy.md) for full policy details.

## Running Tests

```bash
# Unit tests only (default)
gleam test

# Include integration tests
CLAUDE_INTEGRATION_TEST=1 gleam test

# Coverage with threshold enforcement
./scripts/coverage.sh --threshold 95.0

# CI-friendly JSON output
./scripts/coverage.sh --json --threshold 95.0
```
