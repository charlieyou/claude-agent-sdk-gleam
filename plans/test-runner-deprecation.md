# test_runner Usage Evaluation and Deprecation Plan

Date: 2026-01-11

## Context
The current test suite uses `test_runner()` in `test/stream_test.gleam` and `test/resource_test.gleam` to simulate CLI/port behavior. The new policy disallows mocks/fakes for stream/runner paths.

## Decision
- Do not use `test_runner()` in unit tests.
- Keep `test_runner()` available in the public API only if required for external consumers; otherwise consider deprecating in future major version.

## Proposed Actions
1. Remove `test_runner()` usage from internal tests; replace with real port_ffi helper strategy.
2. Update docs to clarify that `test_runner()` is not used by this project’s tests and is legacy/testing-only API.
3. If removing `test_runner()` from public API is desired, defer to a breaking-change release.

## Risks
- Removing `test_runner()` from public API may break downstream consumers.
- Real port-based tests may be slower or more platform-sensitive.

## Acceptance Criteria
- Internal tests do not depend on `test_runner()`.
- README and docs clarify intended testing path.
- Any public API change to `test_runner()` is explicitly versioned.
