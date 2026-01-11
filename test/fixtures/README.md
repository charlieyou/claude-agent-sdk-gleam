# Test Fixtures

## Status: REAL CLI OUTPUT

These fixtures are captured from **real Claude CLI output** (version 2.1.4).

**Note**: `result_error.json` is intentionally synthetic because error scenarios are difficult to capture naturally from the CLI. It matches the real CLI schema structure.

## Capture Information

- **CLI version**: 2.1.4 (Claude Code)
- **Capture date**: 2026-01-11
- **Capture commands**:
  ```bash
  claude --print --output-format stream-json --verbose -- "List the files in the current directory using ls"
  claude --help
  ```

## Fixtures

| File | Description |
|------|-------------|
| `system_message.json` | System init message with tools, model, and session config |
| `assistant_message.json` | Assistant message with tool_use content block |
| `user_message.json` | User message with tool_result content block |
| `result_success.json` | Successful result message with usage/cost data |
| `result_error.json` | Error result message (synthetic - see note above) |
| `cli_help.txt` | Full `claude --help` output for flag validation |

## Compatibility Test Fixtures

These fixtures test forward compatibility and edge cases:

| File | Description |
|------|-------------|
| `system_message_minimal.json` | System message with only `type` field |
| `assistant_unknown_block.json` | Assistant message with unknown content block type |
| `unknown_message_type.json` | Message with unknown type (forward-compat test) |
| `unknown_content_block.json` | Content block with unknown type |

## Field Verification Status

Per plan section "Schema Source and Forward Compatibility":

| Field | Status | Notes |
|-------|--------|-------|
| `type` (all messages) | **Required** | Needed for message dispatch |
| All other fields | **Optional** | Per default policy: "When in doubt, make it Optional" |

## Required CLI Flags

The following flags are required for SDK operation (verified in `cli_help.txt`):

- `--print` / `-p`: Non-interactive output mode
- `--output-format`: Supports `stream-json` for structured output
- `--verbose`: Enables verbose output

## Spec Reference

- Spec: `plans/CLAUDE_AGENT_SDK_SPEC.md` sections 4.1-4.6
- Plan: `plans/2026-01-10-gleam-agent-sdk-plan.md` lines 3131-3224
