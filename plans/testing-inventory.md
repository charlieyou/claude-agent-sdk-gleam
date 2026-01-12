# Test Inventory and Mock Usage Matrix

Date: 2026-01-11

## Summary
- Unit tests cover types, parsing, option builders, error formatting, and API surface.
- Integration tests are opt-in and exercise real CLI interaction.
- Phase 0 runtime tests are opt-in and exercise real port_ffi operations.
- Mock/fake usage exists today in resource and stream tests via test_runner + ETS.
- Decoder tests are gated behind DECODER_IMPLEMENTED and do not run by default.

## Test Modules

| Test file | Category | External deps | Env gating | Mock/fake usage | Notes |
|---|---|---|---|---|---|
| `test/api_surface_test.gleam` | Unit | None | None | No | Verifies public types and constructors importable. |
| `test/cli_args_test.gleam` | Unit | None | None | No | CLI args builder behavior. |
| `test/claude_agent_sdk_test.gleam` | Harness | None | None | No | Test runner entrypoint for gleeunit. |
| `test/error_test.gleam` | Unit | None | None | No | Error terminal/non-terminal and diagnostics. |
| `test/ets_helpers_test.gleam` | Unit | ETS (real) | None | No | Tests ETS helper functions; uses real ETS. |
| `test/json_decode_test.gleam` | Unit | Fixtures | `DECODER_IMPLEMENTED` | No | Decoder tests are skipped unless env var set. Uses JSON fixtures. |
| `test/message_test.gleam` | Unit | None | None | No | Type-level construction and matching. |
| `test/phase0_compile_check_test.gleam` | Unit (compile-time) | None | None | No | Ensures APIs exist and compile. |
| `test/phase0_runtime_test.gleam` | Phase0 runtime | OS port/echo | None | No | Uses real port_ffi to spawn /bin/echo or cmd.exe. |
| `test/phase1_spec_verification.gleam` | Docs (comment only) | None | None | No | Spec verification notes, no tests. |
| `test/query_integration_test.gleam` | Integration | Claude CLI + auth | `CLAUDE_INTEGRATION_TEST` | No | Real CLI checks + preflight + session resume. |
| `test/resource_test.gleam` | Unit | None | None | Yes | Uses test_runner + ETS to simulate stream and close behavior. |
| `test/stream_test.gleam` | Unit + Integration-ish | OS port/echo | None | Mixed | Many tests open real ports; later section uses test_runner + ETS for stream semantics. |
| `test/version_test.gleam` | Unit | None | None | No | Version parsing and comparisons. |

## Support Modules (Used by Tests)
- `test/support/env_helpers.gleam`: FFI helper to read environment variables.
- `test/support/ets_helpers.gleam`: ETS helper functions used for test state.
- `test/support/integration_helpers.gleam`: Preflight checks and integration gating.

## Known Mock/Fake Usage (Current)
- `test/resource_test.gleam`: test_runner + ETS fake state and close tracking.
- `test/stream_test.gleam`: test_runner + ETS for stream semantics section; real port_ffi elsewhere.

## Gated Tests (Current)
- `DECODER_IMPLEMENTED`: disables decoder tests by default.
- `CLAUDE_INTEGRATION_TEST`: disables real CLI integration tests by default.

## Gaps Against "No Mocks/Fakes"
- test_runner usage conflicts with strict no-mock policy.
- Some "unit" tests actually rely on real OS ports (stream tests), blurring categories.
- Decoder coverage is optional and not guaranteed without env gating.
