# E2E Logging Design (Structured + Human Summary)

Date: 2026-01-11

## Goals
- Produce machine-parseable logs (JSONL) for automated triage.
- Provide a concise human-readable summary.
- Capture enough context to diagnose CLI/stream failures.
- Avoid leaking secrets or sensitive data.

## Log Files
- `artifacts/e2e/<run_id>/events.jsonl` (primary structured log)
- `artifacts/e2e/<run_id>/summary.txt` (human summary)
- `artifacts/e2e/<run_id>/stdout.txt` (redacted CLI stdout)
- `artifacts/e2e/<run_id>/stderr.txt` (redacted CLI stderr)
- `artifacts/e2e/<run_id>/metadata.json` (run metadata)

## JSONL Event Schema (v1)
Each line is a JSON object with at least:
- `ts`: ISO8601 timestamp
- `level`: `DEBUG` | `INFO` | `WARN` | `ERROR`
- `event`: Short event identifier
- `run_id`: Unique run identifier
- `scenario_id`: E2E scenario ID (e.g., `E2E-02`)
- `step`: Human-readable step label
- `elapsed_ms`: Duration since run start (int)

Optional fields:
- `command`: Executed command string
- `cwd`: Working directory
- `env`: Redacted environment snapshot (allowlist)
- `exit_code`: Process exit code
- `stdout_bytes`: Byte count (post-redaction)
- `stderr_bytes`: Byte count (post-redaction)
- `result`: `pass` | `fail` | `skip`
- `error`: { `kind`, `message`, `details` }

## Event Types
- `run_start`, `run_end`
- `scenario_start`, `scenario_end`
- `step_start`, `step_end`
- `command_start`, `command_end`
- `skip`, `warning`, `error`
- `artifact_written`

## Redaction Rules
- Never log full `ANTHROPIC_API_KEY` or auth tokens.
- Allowlist environment variables for logs (e.g., `CLAUDE_INTEGRATION_TEST`, `CLAUDE_INTEGRATION_ALLOW_NONJSON`, `PATH`).
- Truncate stdout/stderr lines after a max length (e.g., 4KB per line).
- Replace detected token-like strings with `REDACTED`.

## Summary Output (summary.txt)
- Run header: run_id, date, CLI version, system info.
- Scenario results table:
  - ID, status (pass/fail/skip), duration, key notes.
- Failure section:
  - For each failed scenario: error kind, message, link to event segment.
- Skip section:
  - For each skipped scenario: reason + remediation instructions.

## Metadata (metadata.json)
- `run_id`, `start_ts`, `end_ts`, `duration_ms`
- `cli_version` (raw + parsed)
- `os`, `arch`
- `gleam_version`
- `repo_sha` (if available)

## Example Events (Conceptual)
- `run_start` with metadata
- `scenario_start` for E2E-02
- `command_start` for `claude` query
- `command_end` with exit_code and stdout_bytes
- `scenario_end` with result=pass

## Failure Diagnostics Expectations
- If CLI missing: `skip` event with install instructions.
- If auth missing: `skip` event with login/API key guidance.
- If NDJSON impurity: `error` event with offending line count and sample (redacted).

## Retention and Artifacts
- Keep last N runs (default 20) or last 30 days.
- Upload artifacts to CI for failed runs; optionally store for all runs.
- Ensure artifacts are namespaced by run_id and timestamp.
