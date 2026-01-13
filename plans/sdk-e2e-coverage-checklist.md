# SDK E2E Test Coverage Checklist

Generated: 2026-01-13
Issue: casg-3wy.9

## Summary

All SDK-01 through SDK-64 scenarios from `plans/e2e-sdk-test-plan.md` are implemented.
Tests are gated behind `--e2e` flag to prevent accidental API costs.

## Coverage Matrix

### Category 1: Stream API (SDK-01 to SDK-04)

| ID | Test Function | File | Status |
|----|---------------|------|--------|
| SDK-01 | `sdk_01_basic_query_test` | `test/e2e/sdk_stream_test.gleam:34` | Implemented |
| SDK-02 | `sdk_02_stream_iteration_test` | `test/e2e/sdk_stream_test.gleam:105` | Implemented |
| SDK-03 | `sdk_03_multi_turn_test` | `test/e2e/sdk_stream_test.gleam:154` | Implemented |
| SDK-04 | `sdk_04_session_resume_test` | `test/e2e/sdk_stream_test.gleam:230` | Implemented |

### Category 2: Message Parsing (SDK-10 to SDK-14)

| ID | Test Function | File | Status |
|----|---------------|------|--------|
| SDK-10 | `sdk_10_system_message_test` | `test/e2e/sdk_message_test.gleam:33` | Implemented |
| SDK-11 | `sdk_11_content_blocks_test` | `test/e2e/sdk_message_test.gleam:152` | Implemented |
| SDK-12 | `sdk_12_tool_result_test` | `test/e2e/sdk_message_test.gleam:233` | Implemented |
| SDK-13 | `sdk_13_usage_data_test` | `test/e2e/sdk_message_test.gleam:368` | Implemented |
| SDK-14 | `sdk_14_error_message_test` | `test/e2e/sdk_message_test.gleam:490` | Implemented |

### Category 3: Query Options (SDK-20 to SDK-28)

| ID | Test Function | File | Status |
|----|---------------|------|--------|
| SDK-20 | `sdk_20_model_selection_test` | `test/e2e/sdk_options_test.gleam:41` | Implemented |
| SDK-21 | `sdk_21_max_turns_test` | `test/e2e/sdk_options_test.gleam:86` | Implemented |
| SDK-22 | `sdk_22_max_budget_test` | `test/e2e/sdk_options_test.gleam:129` | Implemented |
| SDK-23 | `sdk_23_system_prompt_test` | `test/e2e/sdk_options_test.gleam:175` | Implemented |
| SDK-24 | `sdk_24_allowed_tools_test` | `test/e2e/sdk_options_test.gleam:222` | Implemented |
| SDK-25 | `sdk_25_disallowed_tools_test` | `test/e2e/sdk_options_test.gleam:300` | Implemented |
| SDK-26 | `sdk_26_permission_default_test` | `test/e2e/sdk_options_test.gleam:378` | Implemented |
| SDK-27 | `sdk_27_permission_accept_edits_test` | `test/e2e/sdk_options_test.gleam:422` | Implemented |
| SDK-28 | `sdk_28_permission_bypass_test` | `test/e2e/sdk_options_test.gleam:466` | Implemented |

### Category 4: Hooks (SDK-30 to SDK-36)

| ID | Test Function | File | Status |
|----|---------------|------|--------|
| SDK-30 | `sdk_30_pre_tool_use_hook_test` | `test/e2e/sdk_hooks_test.gleam:228` | Implemented |
| SDK-31 | `sdk_31_pre_tool_use_block_test` | `test/e2e/sdk_hooks_test.gleam:316` | Implemented |
| SDK-32 | `sdk_32_pre_tool_use_modify_input_test` | `test/e2e/sdk_hooks_test.gleam:428` | Implemented |
| SDK-33 | `sdk_33_post_tool_use_hook_test` | `test/e2e/sdk_hooks_test.gleam:515` | Implemented |
| SDK-34 | `sdk_34_can_use_tool_test` | `test/e2e/sdk_hooks_test.gleam:617` | Implemented |
| SDK-35 | `sdk_35_stop_hook_test` | `test/e2e/sdk_hooks_test.gleam:735` | Implemented |
| SDK-36 | `sdk_36_multiple_hooks_test` | `test/e2e/sdk_hooks_test.gleam:825` | Implemented |

### Category 5: Bidirectional Protocol (SDK-40 to SDK-43)

| ID | Test Function | File | Status |
|----|---------------|------|--------|
| SDK-40 | `sdk_40_set_permission_mode_test` | `test/e2e/sdk_bidir_test.gleam:56` | Implemented |
| SDK-40b | `sdk_40b_set_permission_mode_error_test` | `test/e2e/sdk_bidir_test.gleam:123` | Implemented |
| SDK-41 | `sdk_41_interrupt_test` | `test/e2e/sdk_bidir_test.gleam:182` | Implemented |
| SDK-41b | `sdk_41b_interrupt_no_operation_test` | `test/e2e/sdk_bidir_test.gleam:234` | Implemented |
| SDK-42 | `sdk_42_hook_timeout_test` | `test/e2e/sdk_bidir_test.gleam:289` | Implemented |
| SDK-43 | `sdk_43_malformed_response_test` | `test/e2e/sdk_bidir_test.gleam:374` | Implemented |
| SDK-43b | `sdk_43b_malformed_mixed_with_valid_test` | `test/e2e/sdk_bidir_test.gleam:432` | Implemented |
| SDK-43c | `sdk_43c_shutdown_after_malformed_test` | `test/e2e/sdk_bidir_test.gleam:491` | Implemented |
| SDK-43d | `sdk_43d_truncated_json_test` | `test/e2e/sdk_bidir_test.gleam:530` | Implemented |

### Category 6: MCP Integration (SDK-50 to SDK-52)

| ID | Test Function | File | Status |
|----|---------------|------|--------|
| SDK-50 | `sdk_50_mcp_config_test` | `test/e2e/sdk_mcp_test.gleam:31` | Implemented |
| SDK-51 | `sdk_51_mcp_tools_test` | `test/e2e/sdk_mcp_test.gleam:80` | Implemented |
| SDK-52 | `sdk_52_mcp_failure_test` | `test/e2e/sdk_mcp_test.gleam:126` | Implemented |

### Category 7: Error Handling (SDK-60 to SDK-64)

| ID | Test Function | File | Status |
|----|---------------|------|--------|
| SDK-60 | `sdk_60_cli_not_found_test` | `test/e2e/sdk_error_serial_test.gleam:77` | Implemented |
| SDK-61 | `sdk_61_auth_failure_test` | `test/e2e/sdk_error_serial_test.gleam:161` | Implemented |
| SDK-62a | `sdk_62_delayed_exit_test` | `test/e2e/sdk_error_offline_test.gleam:54` | Implemented |
| SDK-62b | `sdk_62_timeout_test` | `test/e2e/sdk_error_offline_test.gleam:137` | Implemented |
| SDK-63 | `sdk_63_stream_interruption_test` | `test/e2e/sdk_error_offline_test.gleam:212` | Implemented |
| SDK-64 | `sdk_64_invalid_json_handling_test` | `test/e2e/sdk_error_offline_test.gleam:368` | Implemented |
| SDK-64b | `sdk_64b_valid_messages_preserved_test` | `test/e2e/sdk_error_offline_test.gleam:521` | Implemented |

## Additional E2E Coverage

The following tests in `test/e2e/bidir_e2e_test.gleam` provide additional real CLI coverage:

- `real_session_with_hook_test`: Real CLI hook firing
- `real_control_operations_test`: Real CLI control messages
- `real_permission_callback_test`: Real CLI permission handling
- `real_interrupt_test`: Real CLI interrupt handling

## Test Infrastructure

| File | Purpose |
|------|---------|
| `test/e2e/helpers.gleam` | Shared test utilities, E2E gating, structured logging |
| `test/fixtures/mcp-echo-server.json` | MCP server fixture for SDK-50/51 tests |

## Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `--e2e` | Enable E2E tests (passed as CLI arg) | Not set (tests skip) |
| `ANTHROPIC_API_KEY` | API authentication | Required when `--e2e` |

## Protocol Invariant Assertions

All tests assert on protocol structure, not semantic content:
- Stream yields at least one item
- Stream terminates (not hung)
- SystemMessage contains session_id
- Result message appears before stream ends
- Token counts are non-negative integers

## Waived Scenarios

None. All SDK-01 through SDK-64 scenarios are implemented.
