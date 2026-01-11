# Coverage Reporting for claude-agent-sdk-gleam

This document describes how to generate and interpret code coverage reports.

## Quick Start

```bash
# Build and run coverage
gleam build --target=erlang
./scripts/coverage.sh
```

## How It Works

The coverage tooling uses Erlang's built-in `cover` module:

1. **Compile for instrumentation**: Before tests run, source modules are recompiled with coverage instrumentation via `cover:compile_beam/1`
2. **Run tests**: All `*_test` modules are discovered and executed
3. **Collect metrics**: `cover:analyse/3` gathers per-module line coverage
4. **Report**: Results are formatted as text or JSON

### What Gets Measured

Only production source modules are instrumented:
- `claude_agent_sdk` and all submodules (`claude_agent_sdk/*`)
- `claude_agent_sdk_ffi` (Erlang FFI)

Excluded from coverage:
- Test modules (`*_test`)
- Support/helper modules (`support@*`, `phase1_*`)
- Main entry points (`*@@main`)

## Command Options

```bash
./scripts/coverage.sh                    # Text report
./scripts/coverage.sh --json             # JSON output for CI parsing
./scripts/coverage.sh --threshold 70.0   # Exit 1 if total < 70%
```

## Understanding the Report

### Text Output

```
MODULE                                               COVERAGE           LINES
---------------------------------------------------------------------------
claude_agent_sdk/internal/stream                       78.3% 180/230
```

- **MODULE**: Source module path (@ replaced with /)
- **COVERAGE**: Percentage of executable lines covered
- **LINES**: covered/total executable lines

### JSON Output

```json
{
  "total_covered": 502,
  "total_lines": 797,
  "total_percent": 63.0,
  "modules": [
    {"module": "claude_agent_sdk/internal/stream", "covered": 180, "total": 230, "percent": 78.3}
  ]
}
```

## Baseline Snapshot (2026-01-11)

Current coverage baseline before gap analysis:

| Module | Coverage | Lines |
|--------|----------|-------|
| claude_agent_sdk | 22.9% | 11/48 |
| claude_agent_sdk/content | 0.0% | 0/0 |
| claude_agent_sdk/error | 53.6% | 15/28 |
| claude_agent_sdk/internal/cli | 71.2% | 84/118 |
| claude_agent_sdk/internal/constants | 0.0% | 0/0 |
| claude_agent_sdk/internal/decoder | 56.1% | 134/239 |
| claude_agent_sdk/internal/port_ffi | 53.2% | 25/47 |
| claude_agent_sdk/internal/stream | 78.3% | 180/230 |
| claude_agent_sdk/message | 0.0% | 0/0 |
| claude_agent_sdk/options | 81.3% | 13/16 |
| claude_agent_sdk/runner | 75.0% | 9/12 |
| claude_agent_sdk/stream | 0.0% | 0/13 |
| claude_agent_sdk_ffi | 67.4% | 31/46 |
| **TOTAL** | **63.0%** | **502/797** |

### Notes on 0% Modules

Some modules show 0% or 0/0 lines:
- `content`, `message`, `constants`: Pure type definitions with no executable code
- `stream` (public): Thin re-export wrapper; logic lives in `internal/stream`

## Coverage Policy

Per the testing policy (casg-xjv.1):
- Coverage thresholds apply only to **pure modules** (no port/CLI boundary code)
- FFI and port modules are covered by integration tests, not unit coverage
- Target: 100% coverage on eligible pure logic modules

## Troubleshooting

### "File not found" errors
Run `gleam build --target=erlang` first to compile beam files.

### Low coverage on internal modules
Some code paths only execute in integration tests with real CLI. The `cover` module only tracks in-process execution.

### Tests not running
Ensure test modules export functions ending in `_test` with arity 0.
