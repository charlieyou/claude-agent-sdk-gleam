# Testing Gaps Analysis

Date: 2026-01-11
Based on: Coverage report + testing-inventory.md + testing-policy.md

## Coverage Summary

| Module | Coverage | Lines | Status |
|--------|----------|-------|--------|
| claude_agent_sdk | 22.9% | 11/48 | GAP |
| claude_agent_sdk/error | 53.6% | 15/28 | GAP |
| claude_agent_sdk/internal/cli | 71.2% | 84/118 | GAP |
| claude_agent_sdk/internal/decoder | 56.1% | 134/239 | GAP |
| claude_agent_sdk/internal/port_ffi | 53.2% | 25/47 | EXCLUDED (FFI/port) |
| claude_agent_sdk/internal/stream | 78.3% | 180/230 | GAP |
| claude_agent_sdk/options | 81.3% | 13/16 | OK |
| claude_agent_sdk/runner | 75.0% | 9/12 | OK |
| claude_agent_sdk/stream | 0.0% | 0/13 | GAP |
| claude_agent_sdk_ffi | 67.4% | 31/46 | EXCLUDED (FFI) |
| claude_agent_sdk/content | 0.0% | 0/0 | N/A (type-only) |
| claude_agent_sdk/message | 0.0% | 0/0 | N/A (type-only) |
| claude_agent_sdk/internal/constants | 0.0% | 0/0 | N/A (constants) |
| **TOTAL** | **63.1%** | **503/797** | |

Policy targets: Line >= 95%, Branch >= 90%

## Critical Path Analysis

### 1. Query/Stream Lifecycle (CRITICAL)

**Entry point: `claude_agent_sdk.query()`** (src/claude_agent_sdk.gleam:323-470)
- Coverage: 25.0% (12/48 lines)
- Missing coverage:
  - `query()` function (lines 323-349): test_mode vs production path
  - `query_with_cli_path()` (lines 352-383): version check flow
  - `check_version_and_spawn()` (lines 386-419): version validation branches
  - `format_version()` (lines 422-427): version formatting
  - `spawn_query()` (lines 430-470): spawn logic with test_mode/production paths

**Why gap exists**: The main `query()` entry point requires either:
- Real CLI (production path) - covered by integration tests
- test_runner mock (test path) - conflicts with no-mock policy

**Proposed fix**: These functions either:
1. Call port_ffi (excluded from unit coverage per policy)
2. Are thin wrappers over tested internal functions

**Action**: Mark as EXCLUDED (port/CLI boundary). Covered by integration/E2E tests.

### 2. Public Stream API (GAP)

**Module: `claude_agent_sdk/stream`** (0% coverage, 13 lines)
- `next()` (lines 229-251): Maps internal stream results to public types
- `close()` (line 302-304): Delegates to internal_stream.close
- `is_closed()` (lines 318-320): Delegates to internal_stream.is_closed

**Why gap exists**: These are thin wrappers that delegate to `internal/stream.gleam` (which has 78.3% coverage).

**Proposed fix**: Add unit tests for the mapping logic in `next()` since it transforms internal types to public types.

**Action**: Add tests for `stream.next()` type mapping (P1).

### 3. Decoder Module (CRITICAL GAP)

**Module: `internal/decoder`** (56.1% coverage, 134/239 lines)
- Current test file: `json_decode_test.gleam` (17 tests)
- Fixtures: 13 JSON files in test/fixtures/

**Missing coverage**:
1. **decode_content_blocks()** (lines 560-588): List content block decoder
2. **decode_tool_result_block()** (lines 695-718): Tool result block parsing
3. **decode_usage()** (lines 732-766): Usage stats parsing
4. **decode_mcp_server_status()** (lines 768-782): MCP server status
5. **decode_permission_denial()** (lines 783+): Permission denial parsing
6. Error paths in existing decoders (missing field scenarios)

**Existing fixtures**: system_message, assistant_message, user_message, result_success, result_error, unknown_message_type, unknown_content_block, assistant_unknown_block, text_block_missing_text, tool_use_block_missing_id

**Missing fixtures**:
- tool_result_block.json (success case)
- usage_stats.json (standalone)
- mcp_server_status.json
- permission_denial.json
- Nested content blocks (multiple blocks in one message)

**Action**: Add missing decoder tests and fixtures (P1). See casg-xjv.9.

### 4. Internal Stream Module (GAP)

**Module: `internal/stream`** (78.3% coverage, 180/230 lines)
- Extensive test coverage in `stream_test.gleam` (75+ tests)

**Missing coverage areas**:
1. `new_from_runner()` (lines 146-161): test_runner path
2. Edge cases in `handle_exit_status()` for specific exit code combinations
3. Some branches in `next()` for runner-based streams vs port-based streams

**Why gap exists**: test_runner code paths conflict with no-mock policy.

**Action**:
- Port-based paths: Increase coverage via real port tests (phase0_runtime)
- Runner-based paths: Mark as EXCLUDED (test infrastructure only)

### 5. CLI Module (GAP)

**Module: `internal/cli`** (71.2% coverage, 84/118 lines)
- `detect_cli_version()` (lines 56-122): Version detection via subprocess
- `parse_version_string()` (lines 124-218): Version string parsing
- `version_meets_minimum()` (lines 220-230): Version comparison
- `build_cli_args()` (lines 254+): CLI argument building

**Missing coverage**:
1. `detect_cli_version()` - requires spawning real process (excluded)
2. Some edge cases in `parse_version_string()` (partially tested in version_test.gleam)
3. All option combinations in `build_cli_args()`

**Existing tests**: cli_args_test.gleam covers build_cli_args extensively

**Action**:
- `detect_cli_version()`: EXCLUDED (subprocess/port boundary)
- `parse_version_string()`: Add edge case tests (P2)
- `build_cli_args()`: Current coverage sufficient

### 6. Error Module (GAP)

**Module: `error`** (53.6% coverage, 15/28 lines)
- `is_terminal()` (lines 117-122): Tested in error_test.gleam
- `diagnose_exit_code()` (lines 130-174): Tested in error_test.gleam
- `error_to_string()` (lines 279-295): NOT tested
- `stream_error_to_string()` (lines 300-315): NOT tested

**Action**: Add tests for `error_to_string()` and `stream_error_to_string()` (P2).

## Gap Priority List

### P2 - Important (Quality)

> **Note:** Test coverage gaps for working code are P2 (quality), not P1. P1 is reserved
> for issues that would block release (e.g., broken functionality, security issues).
> The decoder and stream functions below are fully implemented and functional—they
> just need additional test coverage to meet the 95% target.

| Gap | File | Function/Area | Test Type | Effort |
|-----|------|---------------|-----------|--------|
| 1 | stream.gleam | `next()` type mapping | Unit | Low |
| 2 | decoder.gleam | decode_tool_result_block | Unit + fixture | Low |
| 3 | decoder.gleam | decode_usage | Unit + fixture | Low |
| 4 | decoder.gleam | decode_mcp_server_status | Unit + fixture | Low |
| 5 | decoder.gleam | decode_permission_denial | Unit + fixture | Low |
| 6 | decoder.gleam | decode_content_blocks (list) | Unit + fixture | Medium |
| 7 | error.gleam | error_to_string | Unit | Low |
| 8 | error.gleam | stream_error_to_string | Unit | Low |
| 9 | cli.gleam | parse_version_string edge cases | Unit | Low |

### Excluded from Unit Coverage (Per Policy)

These are covered by phase0_runtime, integration, or E2E tests:

| Module | Reason |
|--------|--------|
| claude_agent_sdk.query() | Port/CLI spawn boundary |
| internal/port_ffi | FFI/port operations |
| claude_agent_sdk_ffi | Erlang FFI bindings |
| internal/cli.detect_cli_version() | Subprocess spawn |
| internal/stream.new_from_runner() | Test infrastructure only |

## Recommended Test Additions

### For casg-xjv.9 (Decoder completeness)

1. Add fixtures:
   - `test/fixtures/tool_result_block.json`
   - `test/fixtures/usage_stats.json`
   - `test/fixtures/mcp_server_status.json`
   - `test/fixtures/permission_denial.json`
   - `test/fixtures/nested_content_blocks.json`

2. Add tests in `json_decode_test.gleam`:
   - `decode_tool_result_block_test()`
   - `decode_usage_test()`
   - `decode_mcp_server_status_test()`
   - `decode_permission_denial_test()`
   - `decode_content_blocks_multiple_test()`

### For P2 gaps

1. Add in `error_test.gleam`:
   - `error_to_string_cli_not_found_test()`
   - `error_to_string_unsupported_version_test()`
   - `error_to_string_unknown_version_test()`
   - `error_to_string_version_detection_test()`
   - `error_to_string_spawn_test()`
   - `error_to_string_test_mode_test()`
   - `stream_error_to_string_process_error_test()`
   - `stream_error_to_string_buffer_overflow_test()`
   - `stream_error_to_string_too_many_decode_errors_test()`
   - `stream_error_to_string_json_decode_error_test()`
   - `stream_error_to_string_unexpected_message_test()`

2. Add in `stream_test.gleam` or new `public_stream_test.gleam`:
   - Tests for `claude_agent_sdk/stream.next()` result mapping

## Mock/Fake Usage Status

Per testing-inventory.md and testing-policy.md:

| File | Current Usage | Policy Compliance |
|------|--------------|-------------------|
| resource_test.gleam | test_runner + ETS | VIOLATES policy |
| stream_test.gleam | Mixed (real port + test_runner) | PARTIAL violation |

**Remediation path**:
- Resource tests need refactoring to use real port (phase0 approach)
- Stream tests already have significant real-port coverage; test_runner sections can be deprecated

## Summary

- Current overall coverage: 63.1%
- Target: 95% line coverage for eligible modules
- Key gaps: Decoder (missing functions), public stream API (wrapper tests), error formatting
- Excluded from unit coverage: FFI, port operations, CLI subprocess calls
- Mock violations: resource_test.gleam, parts of stream_test.gleam (to be addressed separately)
