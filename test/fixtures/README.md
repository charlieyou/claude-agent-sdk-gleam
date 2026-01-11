# Test Fixtures

## Status: SYNTHETIC

These fixtures are **synthetic** (spec-derived minimal JSON), NOT captured from real CLI output.

## Fixture Policy

- Unit tests must run without a real CLI installed
- These fixtures are derived from the Claude Agent SDK specification
- Real CLI fixtures will be captured and committed in issue casg-k92.11

## Fixtures

| File | Description |
|------|-------------|
| `system_message.json` | Full system message with all observed fields |
| `system_message_minimal.json` | System message with only `type` field (core required) |
| `assistant_message.json` | Assistant message with text and tool_use blocks |
| `assistant_unknown_block.json` | Assistant message with unknown content block type |
| `user_message.json` | User message with tool_result |
| `result_success.json` | Successful result message |
| `result_error.json` | Error result message |
| `unknown_message_type.json` | Message with unknown type (forward-compat test) |
| `unknown_content_block.json` | Assistant with unknown content block type |

## Field Verification Status

Per plan section "Schema Source and Forward Compatibility":

| Field | Status | Notes |
|-------|--------|-------|
| `type` (all messages) | **Required** | Needed for message dispatch |
| All other fields | **Optional** | Per default policy: "When in doubt, make it Optional" |

## Spec Reference

- Spec: `plans/CLAUDE_AGENT_SDK_SPEC.md` sections 4.1-4.6
- Plan: `plans/2026-01-10-gleam-agent-sdk-plan.md` lines 3131-3224
