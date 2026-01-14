# Testing Improvements Spec

Goal: strengthen integration/E2E coverage for real-world breakage risk (not unit coverage for coverage sake).

## Summary of Proposed Additions

1) Make CI E2E actually run real CLI tests
2) Add real-CLI crash/interruption handling test
3) Add parallel session/concurrency coverage
4) Add backpressure + large payload coverage
5) Add CLI output drift contract tests
6) Add real-CLI negative permission/denied flows
7) Add real-CLI hook timeout coverage
8) Add CLI version matrix for E2E
9) Add opt-in soak test
10) Add OS coverage for E2E
11) Add network/credential failure E2E

## 1) CI E2E actually runs real CLI tests

Problem:
- Current CI `e2e` job runs `gleam test` without `--e2e`, so real-CLI tests always skip.

Spec:
- Change CI to run `gleam test -- --e2e` in the `e2e` job.
- Keep existing gating (label/manual/schedule).
- Continue uploading `artifacts/e2e/`.

Acceptance:
- A scheduled or labeled run executes real-CLI E2E (not skipped for missing flag).

## 2) Real-CLI crash/interruption handling

Spec:
- Add E2E test that starts a streaming query, then kills the CLI process mid-stream.
- Assert:
  - Stream terminates with a terminal error, not a hang.
  - `close()` returns cleanly.
  - A follow-up query succeeds, proving recovery.

Notes:
- Use a dedicated helper to locate and kill the port process safely.

Acceptance:
- Test fails if stream hangs or if subsequent query fails after a forced crash.

## 3) Parallel session/concurrency coverage

Spec:
- Add an E2E test that runs N concurrent queries (3–5).
- Verify each stream’s messages stay isolated (no cross-session mixups).
- Validate all streams terminate normally.

Notes:
- The existing global lock in helpers serializes queries. Add a separate path
  (env flag) to allow concurrency for this test only.

Acceptance:
- Fails on cross-session contamination or non-termination.

## 4) Backpressure + large payload

Spec:
- Trigger long output (large response) from CLI.
- Consume stream slowly (sleep between `next()` calls).
- Validate behavior:
  - No crash.
  - Buffer overflow is handled as documented.
  - Stream closes cleanly on terminal error or result.

Acceptance:
- Passes with either normal completion or a defined overflow error.

## 5) CLI output drift contract tests

Spec:
- Capture representative NDJSON transcripts for current CLI versions.
- Add contract checks that enforce required fields for each message type.
- Run contract tests against recorded transcripts in CI.

Notes:
- Keep a small set of golden transcripts to reduce maintenance cost.

Acceptance:
- CI fails if required fields are missing or message shapes drift.

## 6) Real-CLI negative permission/denied flows

Spec:
- Exercise permission handlers against a prompt that triggers tool usage.
- Deny permission and assert:
  - Tool does not execute.
  - SDK surfaces deny response clearly.

Acceptance:
- Fails if tool executes despite deny.

## 7) Real-CLI hook timeout coverage

Spec:
- Register a hook that exceeds configured timeout.
- Assert configured fail-open/fail-close behavior is honored.
- Record structured logs for timeout reason.

Acceptance:
- Fails if timeout behavior does not match expected policy.

## 8) CLI version matrix for E2E

Spec:
- Run E2E against latest and previous minor CLI versions.
- Compare outputs against contract expectations.

Notes:
- Use matrix in GitHub Actions or scheduled job with CLI install args.

Acceptance:
- E2E passes across the matrix or reports version-specific failures.

## 9) Opt-in soak test

Spec:
- Add a long-running E2E test (opt-in) that runs 20–50 sequential queries.
- Validate:
  - No memory/resource leaks (process/ETS/ports).
  - No cumulative degradation.

Acceptance:
- Passes without resource exhaustion; log resource metrics per step.

## 10) OS coverage for E2E

Spec:
- Add scheduled E2E runs on macOS and Windows.
- Validate path/port/encoding issues across OSes.

Acceptance:
- Scheduled E2E runs complete on all OSes.

## 11) Network/credential failure E2E

Spec:
- Run E2E with an intentionally invalid API key.
- Assert authentication error is surfaced with clear error type.

Acceptance:
- Fails if error is ambiguous or not mapped to expected SDK error.
