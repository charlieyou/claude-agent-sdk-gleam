# E2E Integration Scenarios (Design)

Date: 2026-01-11

## Purpose
Define end-to-end scenarios that validate real Claude CLI behavior, CLI discovery, authentication, stream semantics, and failure modes. These scenarios are intended for scripted execution with detailed logging and artifact capture.

## Prerequisites
- Claude CLI installed and available in PATH.
- Authentication via an authenticated CLI session or `ANTHROPIC_API_KEY`.
- Environment variables:
  - `CLAUDE_INTEGRATION_TEST=1` to enable E2E suite
  - Optional: `CLAUDE_INTEGRATION_ALLOW_NONJSON=1` for legacy CLI outputs

## Scenario List

### E2E-01: Preflight Environment Validation
- Steps:
  1) Resolve CLI executable (`claude --version`).
  2) Parse and validate version (>= minimum).
  3) Check auth availability (token or login).
- Success criteria:
  - Explicit, actionable output on any failure.
  - If all pass, report readiness for E2E scenarios.

### E2E-02: Simple Query (Happy Path)
- Steps:
  1) Run a minimal query with `max_turns=1` and a short prompt.
  2) Read stream until `Result` or end-of-stream.
- Success criteria:
  - Stream yields valid JSON messages.
  - A `Result` message appears before clean exit.
  - Exit status indicates success.

### E2E-03: NDJSON Purity Validation
- Steps:
  1) Run a query expecting clean NDJSON output.
  2) Detect any non-JSON lines in stdout.
- Success criteria:
  - No non-JSON lines unless `CLAUDE_INTEGRATION_ALLOW_NONJSON=1` is set.
  - If non-JSON appears, produce a clear diagnostic and mark failure.

### E2E-04: Session Resume Flow
- Steps:
  1) Create a session with an initial query and capture session ID.
  2) Resume the session with a follow-up prompt.
- Success criteria:
  - Resume produces a valid assistant response or result.
  - Session continuity confirmed via CLI output or SDK stream results.

### E2E-05: Version Edge Cases (Skip / Warn)
- Steps:
  1) Detect version parsing for unexpected version strings.
  2) Verify clear warnings but not hard failure when version is unparseable.
- Success criteria:
  - Graceful handling with informative warnings.

### E2E-06: Auth Missing (Expected Skip)
- Steps:
  1) Run with no auth configured.
- Success criteria:
  - Test is skipped with explicit instructions to authenticate.

### E2E-07: CLI Missing (Expected Skip)
- Steps:
  1) Simulate missing CLI (PATH lookup failure).
- Success criteria:
  - Test is skipped with explicit install guidance.

### E2E-08: Timeout / Slow Response (Optional)
- Steps:
  1) Run a query with a strict timeout threshold.
- Success criteria:
  - Timeout is reported clearly, with guidance to retry.

## Data Handling
- Never log raw API keys or tokens.
- Redact environment variables that include secrets.
- Store raw CLI stdout/stderr as artifacts with redaction and size limits.

## Output Expectations
- JSONL logs for every step (see logging design document).
- A human-readable summary showing pass/fail/skip for each scenario.
- Artifact bundle: logs + minimal metadata + CLI version and environment snapshot.
