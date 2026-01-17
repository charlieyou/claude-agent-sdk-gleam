# Gaps Parity Implementation Plan

Date: 2026-01-17
Owner: SDK agent
Status: Draft (pending interview + review)
Spec: `plans/2026-01-15-gaps-parity-plan.md`

---

## Context & Goals

Bring `claude-agent-sdk-gleam` to feature parity with `claude-agent-sdk-python` for:
- Bidirectional public API (send_user_message, hooks wiring)
- CLI argument parity for BidirOptions fields
- Content type parity (ThinkingBlock, partial messages)
- Documentation updates

### Current State

The Gleam SDK has:
- ✅ Public `start_session_new()` with control operations
- ✅ `CliOptions` and `BidirOptions` types with parity fields
- ✅ Internal hooks/MCP infrastructure
- ❌ BidirOptions fields not wired to CLI args
- ❌ No public `send_user_message()`
- ❌ Hooks not wired from `start_session_new()`
- ❌ No ThinkingBlock content type
- ❌ No partial message support

---

## Scope & Non-Goals

### In Scope

From spec (plans/2026-01-15-gaps-parity-plan.md):
- Public bidirectional session API wiring to internal actor
- Control operations (already implemented, verify working)
- MCP SDK server message handling for in-process servers
- Option surface parity for CLI/config features supported in Python
- Documentation and examples for new features
- Tests and E2E validation for new capabilities

### Out of Scope (Non-Goals)

From spec:
- Re-architecture of internal actor beyond what parity requires
- New CLI features not present in the Python SDK
- Breaking changes to existing Gleam query/stream APIs

---

## Assumptions & Constraints

From spec:
- **Supported CLI versions:** Minimum Claude CLI version that supports bidirectional sessions and the control operations (version TBD based on Python SDK requirements)
- **Capability detection:** The Gleam SDK assumes the CLI supports all bidir features when `--output-format stream-json` is accepted
- **MCP handler functions** are user-provided callbacks conforming to a defined interface
- The SDK does not implement MCP protocol parsing; it routes raw JSON messages

### Implementation Constraints

- Assume CLI supports all BidirOptions flags when bidir mode is available (version 1.1.0+)
- **Agents serialization**: JSON string inline for ≤3 agents; @file for >3 agents (matches Python SDK)
- **Sandbox merge**: Merge sandbox config into settings JSON before passing to --settings

### Testing Constraints

- All new features must have unit tests
- E2E tests required for public API changes
- **Coverage**: Full test pyramid - Unit + Integration + E2E for all new public APIs

---

## Integration Analysis

### Existing Mechanisms Considered

| Existing Mechanism | Could Serve Feature? | Decision | Rationale |
|--------------------|---------------------|----------|-----------|
| `cli.build_bidir_cli_args_new()` | Yes | Extend | Already handles CliOptions, add BidirOptions |
| `options.BidirOptions` | Yes | Already Extended | Parity fields exist in types |
| `content.ContentBlock` | Yes | Extend | Add ThinkingBlock variant |
| `decoder.gleam` | Yes | Extend | Add partial message parsing |
| `claude_agent_sdk.gleam` | Yes | Extend | Add public send_user_message |
| `start_session_new()` | Yes | Extend | Wire HookConfig from BidirOptions |

### Integration Approach

Extend existing infrastructure rather than creating new systems:
1. CLI arg builder: Add BidirOptions fields to `build_bidir_cli_args_new()`
2. Public API: Expose existing internal `send_user_message()`
3. Hooks wiring: Build HookConfig from BidirOptions in `start_session_new()`
4. Content types: Add ThinkingBlock to existing ContentBlock enum

---

## Prerequisites

- [x] Access to Python SDK source for behavior reference
- [x] Working internal bidir actor (`internal/bidir_runner`)
- [x] Test infrastructure for spawning mock CLI processes
- [ ] Confirm Claude CLI supports all target flags: `--include-partial-messages`, `--fork-session`, `--agents`, `--setting-sources`, `--plugin`, `--max-thinking-tokens`, `--output-format`
- [ ] Verify internal `bidir.send_user_message()` error types for public error mapping decisions

---

## High-Level Approach

### Phase 1: Public API Completion

1. Add public `send_user_message()` to `claude_agent_sdk.gleam`
2. Wire hooks from BidirOptions to HookConfig in `start_session_new()`
3. Verify MCP server handling works through public API

### Phase 2: CLI Argument Parity

1. Extend `build_bidir_cli_args_new()` for BidirOptions fields:
   - `include_partial_messages` → `--include-partial-messages`
   - `fork_session` → `--fork-session`
   - `agents` → `--agents <JSON>` (inline for ≤3) or `--agents @/tmp/agents.json` (file for >3)
   - `setting_sources` → `--setting-sources`
   - `plugins` → `--plugin` (repeated flag)
   - `max_thinking_tokens` → `--max-thinking-tokens`
   - `output_format` → `--output-format` (when not stream-json, e.g., json-schema mode)

### Phase 3: Content Type Parity

1. Add `ThinkingBlock` to `ContentBlock` enum
2. Update decoder for thinking blocks
3. **Partial message streaming**: Match Python SDK semantics - emit partial content via existing message stream with partial indicator flag

### Phase 4: Documentation

1. Update bidir usage docs
2. Add MCP examples
3. Document new options

---

## Technical Design

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Public API Layer                          │
│  claude_agent_sdk.gleam                                      │
│  ├── start_session_new() [existing, add hooks wiring]       │
│  └── send_user_message() [NEW]                              │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    Internal Layer                            │
│  ├── bidir/actor.gleam (HookConfig from BidirOptions)       │
│  ├── cli.gleam (build_bidir_cli_args_new + new flags)       │
│  └── decoder.gleam (ThinkingBlock, is_partial)              │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    Types Layer                               │
│  ├── content.gleam (ContentBlock + ThinkingBlock)           │
│  ├── message.gleam (AssistantMessage + is_partial)          │
│  └── options.gleam (BidirOptions - fields already exist)    │
└─────────────────────────────────────────────────────────────┘
```

No architectural changes required. This is primarily a "plumbing" update to expose internal capabilities and data fields to the public surface and CLI.

### Public send_user_message

```gleam
pub fn send_user_message(sess: Session, prompt: String) -> Result(Nil, ControlError)
```

Maps to existing internal `bidir.send_user_message()` with error mapping to public types.

### Hooks Wiring (Automatic in start_session_new)

In `start_session_new()`, automatically build `HookConfig` from `BidirOptions`:

| BidirOptions Field | HookConfig Field | Notes |
|--------------------|------------------|-------|
| `on_pre_tool_use` | `pre_tool_use` | Direct mapping |
| `on_post_tool_use` | `post_tool_use` | Direct mapping |
| `on_user_prompt_submit` | `user_prompt_submit` | Direct mapping |
| `on_stop` | `stop` | Direct mapping |
| `on_subagent_stop` | `subagent_stop` | Direct mapping |
| `on_pre_compact` | `pre_compact` | Direct mapping |
| `on_can_use_tool` | `permission_handler` | Separate permission config |
| `hook_timeouts` | `hook_timeouts` | Per-hook timeout overrides |

**Decision**: Hooks are wired automatically - no separate `start_with_hooks()` call needed from public API.

### CLI Argument Extensions

In `cli.build_bidir_cli_args_new()`, add handling for BidirOptions fields:

| BidirOptions Field | CLI Flag | Format |
|--------------------|----------|--------|
| `include_partial_messages` | `--include-partial-messages` | Boolean flag (no value) |
| `fork_session` | `--fork-session` | Session ID string |
| `agents` | `--agents` | JSON string (≤3 agents) or `@/tmp/file.json` (>3 agents) |
| `setting_sources` | `--setting-sources` | Comma-separated list |
| `plugins` | `--plugin` | Repeated flag (one per plugin) |
| `max_thinking_tokens` | `--max-thinking-tokens` | Integer value |
| `output_format` | `--output-format` | Format string (overrides stream-json when set) |
| `sandbox` | (merged) | Merge into settings JSON before `--settings` |

### ThinkingBlock Content Type

```gleam
pub type ContentBlock {
  TextBlock(text: String)
  ToolUseBlock(id: String, name: String, input: Dynamic)
  ThinkingBlock(thinking: String, signature: Option(String))  // NEW
  UnknownBlock(raw: Dynamic)
}
```

**Notes:**
- `signature` is Optional; some thinking blocks may not include a cryptographic signature
- Decoder handles missing signature field gracefully (maps to `None`)

### Partial Message Streaming

**Approach**: Match Python SDK semantics.

When `include_partial_messages` is enabled:
1. CLI emits partial content updates as they stream
2. SDK surfaces these via the existing message stream
3. Message includes `is_partial: Bool` field to indicate incomplete content
4. Final message has `is_partial: False`

**Implementation**:
- Extend `Message` or `AssistantMessage` with optional `is_partial` field
- Decoder handles partial message JSON format from CLI
- No new public types needed - just field addition

### File Impact Summary

| File | Status | Description |
|------|--------|-------------|
| `src/claude_agent_sdk.gleam` | Exists | Add send_user_message, wire hooks in start_session_new |
| `src/claude_agent_sdk/internal/cli.gleam` | Exists | Extend for BidirOptions CLI args |
| `src/claude_agent_sdk/content.gleam` | Exists | Add ThinkingBlock variant to ContentBlock |
| `src/claude_agent_sdk/message.gleam` | Exists | Add is_partial field to AssistantMessage |
| `src/claude_agent_sdk/internal/decoder.gleam` | Exists | Add thinking block and is_partial decoding |
| `src/claude_agent_sdk/internal/bidir/actor.gleam` | Exists | May need HookConfig builder helper |
| `test/bidir_session_test.gleam` | Exists | Add send_user_message and hooks wiring tests |
| `test/cli_args_test.gleam` | Exists | Add BidirOptions flag tests |
| `test/decoder_test.gleam` | Exists | Add thinking block and is_partial decode tests |
| `test/e2e/sdk_hooks_test.gleam` | Exists | Verify hooks through public API |
| `test/e2e/sdk_mcp_test.gleam` | Exists | Verify MCP handling with new content types |
| `docs/building-an-app.md` | Exists | Document send_user_message usage |
| `docs/options.md` | Exists | Document BidirOptions CLI arg mappings |
| `docs/tools-and-permissions.md` | Exists | Document hooks wiring |

---

## Risks, Edge Cases & Breaking Changes

From spec:
- CLI protocol compatibility requirements for new options
- Semantics of MCP server handler responses in Gleam vs Python
- Handling of partial messages in Gleam stream model

### Edge Cases & Failure Modes

**Session Lifecycle:**
- Control op timeout during session shutdown race
- MCP message arrives after session stop initiated
- `send_user_message` called on closed/closing session: Return appropriate error variant

**Hook Handling:**
- Handler panics or throws exception: Catch and log, don't crash session
- Hook callback takes too long: Proceed/fail per existing HookConfig timeout logic
- Missing hook callback: No-op (omit from HookConfig)

**CLI Arguments:**
- Empty agents list: Emit no `--agents` flag (not `--agents '[]'`)
- Agents threshold: Exactly 3 agents uses inline JSON; >3 uses temp file
- Temp file location: Write to system temp directory (e.g., `/tmp/agents-<session-id>.json`)
- Temp file cleanup: Delete temp file after CLI spawn completes (in finally/after block)
- Agents file write failure (>3 agents): Fail session start gracefully with clear error

**Content Types:**
- ThinkingBlock without signature: Handle as `None` (signature field is Optional)
- Partial message with empty content: Valid state, content can be empty string
- Partial message ordering: Messages arrive in order; rely on stream ordering (no explicit sequence numbers needed)

### Breaking Changes & Compatibility

- **Potential Breaking Changes:**
  - Adding `is_partial` field to `AssistantMessage` changes record shape (affects users constructing manually in tests)
  - Adding `ThinkingBlock` variant changes `ContentBlock` exhaustiveness (pattern match updates required)

- **Mitigations:**
  - `is_partial` defaults to `False` (existing code unaffected if not pattern matching on field)
  - `AssistantMessage` is primarily read-only for users (received from SDK)
  - Document migration for users doing exhaustive pattern matching on `ContentBlock`
  - All changes are additive; no removed or renamed APIs

---

## Testing & Validation Strategy

### Unit Tests

| Area | Test File | Test Cases |
|------|-----------|------------|
| CLI arg building | `cli_args_test.gleam` | Each BidirOptions field emits correct flag; agents split logic (≤3 inline, >3 @file); empty lists omit flags |
| ThinkingBlock decoding | `decoder_test.gleam` | Thinking block JSON with/without signature; malformed JSON handling |
| Partial message decoding | `decoder_test.gleam` | is_partial=true/false/missing; empty content handling |
| send_user_message | `bidir_session_test.gleam` | Mock actor receives call; error mapping to public types |
| AssistantMessage | `decoder_test.gleam` | is_partial field correctly decoded; defaults to False when missing |

### Integration Tests

| Area | Test File | Test Cases |
|------|-----------|------------|
| Hooks wiring | `bidir_session_test.gleam` | start_session_new builds HookConfig from BidirOptions callbacks |
| Permission callback | `bidir_session_test.gleam` | can_use_tool callback controls permission decisions |
| MCP routing | `e2e/sdk_mcp_test.gleam` | Handlers receive expected MCP messages |
| Combined CLI args | `cli_args_test.gleam` | Multiple options produce correct combined invocation |

### E2E Tests

**Full E2E with real CLI** (if CLI available):
- Public `send_user_message()` complete flow
- Partial messages with `--include-partial-messages` flag
- Hooks invocation through public API
- ThinkingBlock content in responses

**Mock-based E2E** (always run):
- `e2e/sdk_hooks_test.gleam`: End-to-end hooks receive expected callbacks
- `e2e/sdk_mcp_test.gleam`: MCP messages with new content types decoded correctly
- CLI arg building verification
- Message decoding for all content types
- Error handling paths

### Regression Tests

- All existing tests must continue passing
- Existing `start_session_new` usage without hooks still works
- Existing BidirOptions with only basic fields still works
- No behavioral changes to existing public APIs

### Manual Verification

- Run sample app with all new options enabled
- Verify thinking blocks appear correctly in output
- Verify partial messages stream in expected order
- Verify hooks fire at expected lifecycle points
- Test with various agents configurations (0, 3, 4+ agents)

### Acceptance Criteria Coverage

| Spec AC | Covered By |
|---------|------------|
| send_user_message available as public API | Technical Design, `bidir_session_test.gleam` |
| Hooks wired automatically from BidirOptions | Architecture section, hooks wiring tests |
| All BidirOptions fields emit correct CLI args | CLI arg table, `cli_args_test.gleam` |
| ThinkingBlock in ContentBlock enum | Data Model section, `decoder_test.gleam` |
| is_partial field on AssistantMessage | Data Model section, `decoder_test.gleam` |
| Control operations work | Phase 1 verification, existing E2E tests |
| MCP message handling | Phase 1, `e2e/sdk_mcp_test.gleam` |
| Documentation updated | File Impact Summary docs entries |

---

## Spec/Legacy Fidelity

This plan aligns with the spec at `plans/2026-01-15-gaps-parity-plan.md`.

### Deviation Log

| Source | Deviation | Rationale | Approved? |
|--------|-----------|-----------|-----------|
| None | — | — | — |

---

## Open Questions

### Resolved (from interview)

1. ~~CLI flag for `agents`~~ → JSON string for ≤3 agents, @file for >3
2. ~~Partial message streaming~~ → Match Python SDK (is_partial field on messages)
3. ~~Hooks wiring~~ → Automatic in `start_session_new()` from BidirOptions
4. ~~Testing coverage~~ → Full pyramid: Unit + Integration + E2E

### Remaining

1. **CLI `output_format` values**: What are the valid format strings besides `stream-json`? (Need to verify against CLI docs)
2. **Minimum CLI version**: Exact version that supports all BidirOptions flags (assumed 1.1.0+ for now)
3. ~~Temp file location for agents~~ → Use system temp dir (e.g., `/tmp/agents-<session-id>.json`); not configurable
4. **Error type granularity for send_user_message**: Single `SendMessageError` type or multiple specific variants? (Affects public API design)
5. **Hook exception handling**: Should hook exceptions be silently logged or propagated? (Affects `actor.gleam` behavior)

---

## Next Steps

After this plan is approved, run `/cerberus:create-tasks` to generate:
- `--beads` → Beads issues with dependencies for multi-agent execution
- (default) → TODO.md checklist for simpler tracking
