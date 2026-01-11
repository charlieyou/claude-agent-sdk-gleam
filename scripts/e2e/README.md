# E2E Test Runner

End-to-end integration test runner for Claude Agent SDK (Gleam).

## Quick Start

```bash
# Enable E2E tests (required gate)
export CLAUDE_INTEGRATION_TEST=1

# Run all scenarios
python scripts/e2e/run_e2e.py

# Run specific scenario
python scripts/e2e/run_e2e.py --scenario E2E-02

# List available scenarios
python scripts/e2e/run_e2e.py --list
```

## Prerequisites

- Python 3.10+
- Claude CLI installed and in PATH (`claude --version`)
- Authentication: `ANTHROPIC_API_KEY` env var or `claude login`

## Environment Variables

| Variable | Required | Description |
|----------|----------|-------------|
| `CLAUDE_INTEGRATION_TEST` | Yes | Set to `1` to enable E2E suite |
| `ANTHROPIC_API_KEY` | No* | API key for authentication |
| `CLAUDE_INTEGRATION_ALLOW_NONJSON` | No | Set to `1` to tolerate non-JSON CLI output |

*Either `ANTHROPIC_API_KEY` or `claude login` session required.

## Scenarios

| ID | Description |
|----|-------------|
| E2E-01 | Preflight: CLI discovery, version, auth |
| E2E-02 | Simple query happy path |
| E2E-03 | NDJSON purity validation |
| E2E-04 | Session resume flow |
| E2E-05 | Version parsing edge cases |
| E2E-06 | Auth missing (expected skip) |
| E2E-07 | CLI missing (expected skip) |
| E2E-08 | Timeout handling |

## Output Artifacts

Each run creates `artifacts/e2e/<run_id>/` containing:

| File | Description |
|------|-------------|
| `events.jsonl` | Structured event log (machine-parseable) |
| `summary.txt` | Human-readable results summary |
| `metadata.json` | Run metadata (versions, timing, system info) |
| `stdout.txt` | Redacted CLI stdout capture |
| `stderr.txt` | Redacted CLI stderr capture |

## JSONL Event Schema

Each line in `events.jsonl` contains:

```json
{
  "ts": "2026-01-11T12:00:00.000Z",
  "level": "INFO",
  "event": "scenario_start",
  "run_id": "20260111-120000-abc12345",
  "scenario_id": "E2E-02",
  "step": "simple_query",
  "elapsed_ms": 150
}
```

Event types: `run_start`, `run_end`, `scenario_start`, `scenario_end`, `step_start`, `step_end`, `command_start`, `command_end`, `skip`, `warning`, `error`, `artifact_written`

## Security

- API keys and tokens are redacted from all logs
- Environment variables are filtered to an allowlist
- Long lines are truncated (4KB max per line)
- Secrets matching known patterns replaced with `REDACTED`

## Example Output

```
$ CLAUDE_INTEGRATION_TEST=1 python scripts/e2e/run_e2e.py

E2E Run: 20260111-143052-a1b2c3d4
Output:  artifacts/e2e/20260111-143052-a1b2c3d4
CLI:     2.1.4 (Claude Code)

Running E2E-01... ✓ pass (234ms)
Running E2E-02... ✓ pass (1523ms)
Running E2E-03... ✓ pass (892ms)
Running E2E-04... ✓ pass (2105ms)
Running E2E-05... ✓ pass (12ms)
Running E2E-06... ○ skip (5ms)
Running E2E-07... ○ skip (3ms)
Running E2E-08... ✓ pass (5002ms)

Results: 6 passed, 0 failed, 2 skipped
Artifacts: artifacts/e2e/20260111-143052-a1b2c3d4
```
