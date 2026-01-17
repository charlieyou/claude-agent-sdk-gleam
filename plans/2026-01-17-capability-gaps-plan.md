# Capability Gaps Implementation Plan (Gleam SDK vs Python SDK)

Date: 2026-01-17
Owner: SDK Team
Status: Draft (pending interview + review)
Spec Reference: Derived from Python SDK capability analysis

---

## Context & Goals

- **Spec**: N/A — derived from Codex capability gaps analysis and Python SDK parity review
- Close functional capability gaps between the Gleam SDK and Python SDK
- Prioritize user-facing capabilities affecting integration breadth and ergonomics
- Maintain backwards compatibility with existing Gleam SDK APIs

### Current State

The Gleam SDK already has:
- ✅ `cli_path` option in CliOptions (but no bundled CLI or fallback chain)
- ✅ Single hook callbacks per event in BidirOptions
- ✅ Basic `PermissionCheckResult` (Allow/Deny) in `hook.gleam`
- ✅ Rich `PermissionResult` variants (AllowOnce/AllowAll/Edit) in `control.gleam`
- ✅ MCP server handlers as list of `(name, handler)` tuples
- ✅ `json_schema` as String in CliOptions
- ✅ `settings` as Dict in CliOptions with sandbox merge support
- ❌ No CLI discovery fallback chain
- ❌ No streaming input for `query()`
- ❌ No hook matchers or multiple hooks per event
- ❌ Rich permission variants not exposed to public hook API
- ❌ No typed MCP tool helpers
- ❌ No `--tools` preset support
- ❌ No stderr capture/callback
- ❌ No buffer size configuration

---

## Scope & Non-Goals

### In Scope

1. **CLI discovery chain**: Fallback path resolution (explicit → known locations → PATH)
2. **Streaming input for query()**: [TBD: Decide if needed or defer]
3. **Hook matchers + multiple hooks**: Support lists of hooks with optional tool name matchers
4. **Rich permission responses**: Expose AllowOnce/AllowAll/Edit to public hook API
5. **MCP tool helpers**: [TBD: Typed tool registration or leave as Dynamic handlers]
6. **Tools preset flag**: Add `--tools` CLI flag support
7. **Stderr capture**: [TBD: Callback option vs error field enrichment]
8. **Buffer size config**: [TBD: Add max_buffer_size option]

### Out of Scope (Non-Goals)

- Bundling CLI binaries in the Gleam package (complex packaging issue)
- Changes to Claude CLI behavior
- Changes to Gleam runtime/OTP semantics
- Breaking changes to existing public APIs (use deprecations if needed)
- `output_format` type change (already have `json_schema` as String, which matches Python's string usage)

---

## Assumptions & Constraints

### Implementation Constraints

- CLI discovery searches: explicit `cli_path` > `~/.claude/local/claude` > `claude` on PATH
- Hook lists execute in order; first Block/ModifyInput wins
- Rich permission variants already exist in `control.gleam`; wire to public types
- Backwards compatibility: existing single-callback API continues to work
- [TBD: Minimum CLI version for new flags like `--tools`]

### Testing Constraints

- All new features must have unit tests
- E2E tests required for public API changes (gated by --e2e flag)
- Coverage: Unit + Integration for CLI args, hook execution

---

## Integration Analysis

### Existing Mechanisms Considered

| Existing Mechanism | Could Serve Feature? | Decision | Rationale |
|--------------------|---------------------|----------|-----------|
| `options.CliOptions.cli_path` | Partial | Extend | Has path override; add fallback resolution |
| `options.BidirOptions.on_*` hooks | Partial | Extend | Single callback; add list + matcher support |
| `hook.PermissionCheckResult` | Partial | Extend | Add AllowOnce/AllowAll/Edit variants |
| `control.PermissionResult` | Yes | Wire | Already has rich variants; expose to public |
| `options.BidirOptions.mcp_servers` | Yes | Extend | Add typed tool helper constructors |
| `cli.build_args_*` | Yes | Extend | Add `--tools` flag |
| `internal/port_io.gleam` | Partial | Extend | Add stderr capture |
| `internal/stream.gleam` | Yes | Extend | Add buffer size option |

### Integration Approach

Extend existing infrastructure rather than creating new systems:
1. **CLI discovery**: Add fallback chain in `cli.gleam` before spawning
2. **Hook lists**: New type `HookCallback(List(HookEntry))` wrapping existing callbacks
3. **Rich permissions**: Extend `hook.PermissionCheckResult` to match `control.PermissionResult`
4. **MCP helpers**: Add builder functions returning existing `(String, fn(Dynamic) -> Dynamic)` tuples
5. **Tools preset**: Simple flag addition to CLI arg builder
6. **Stderr**: [TBD: Extend port_io or add callback option]
7. **Buffer size**: Thread through stream reader

---

## Prerequisites

- [x] Access to Python SDK source for behavior reference
- [x] Working internal bidir infrastructure
- [ ] Confirm CLI supports `--tools` flag
- [ ] [TBD: Verify stderr is accessible from Erlang ports or need workaround]

---

## High-Level Approach

[TBD: 1-2 paragraphs describing the phased approach after interview]

### Phase 1: Hook System Enhancement

[TBD: Hook lists + matchers + rich permissions]

### Phase 2: CLI Discovery & Tools Preset

[TBD: Fallback chain + --tools flag]

### Phase 3: MCP Tool Helpers

[TBD: Typed tool registration helpers]

### Phase 4: Diagnostics & Configuration

[TBD: Stderr capture + buffer size]

### Phase 5: Optional - Streaming Input

[TBD: Depends on user demand - may defer]

---

## Technical Design

### Architecture

[TBD: Component diagram after interview clarifies priorities]

### Data Model

#### Hook Enhancement Types

```gleam
// [TBD: Exact API shape]
pub type HookEntry {
  HookEntry(
    callback: fn(PreToolUseContext) -> HookExecutionResult,
    matcher: Option(HookMatcher),  // [TBD: Tool name only or regex?]
  )
}

pub type HookMatcher {
  ToolNameMatcher(tool_name: String)
  // [TBD: RegexMatcher?]
}
```

#### Enhanced PermissionCheckResult

```gleam
// Current (hook.gleam):
pub type PermissionCheckResult {
  Allow
  Deny(reason: String)
}

// Proposed extension:
pub type PermissionCheckResult {
  Allow
  AllowOnce    // [TBD: Keep separate from Allow?]
  AllowAll     // Allow all future uses of this tool
  Deny(reason: String)
  Edit(new_input: Dynamic)  // Modify input and proceed
}
```

#### CLI Discovery Chain

```gleam
// [TBD: In cli.gleam or new module?]
pub fn resolve_cli_path(explicit: Option(String)) -> Result(String, CliNotFoundError) {
  // 1. Use explicit path if provided
  // 2. Check ~/.claude/local/claude
  // 3. Search PATH for "claude"
  // [TBD: Additional known locations?]
}
```

### API/Interface Design

[TBD: Public API changes after interview]

### File Impact Summary

| File | Status | Description |
|------|--------|-------------|
| `src/claude_agent_sdk/options.gleam` | Exists | Add hook list types, max_buffer_size |
| `src/claude_agent_sdk/hook.gleam` | Exists | Add rich PermissionCheckResult variants, HookEntry type |
| `src/claude_agent_sdk/internal/cli.gleam` | Exists | Add CLI discovery chain, --tools flag |
| `src/claude_agent_sdk/internal/bidir/hooks.gleam` | Exists | Handle hook list execution |
| `src/claude_agent_sdk/internal/mcp_router.gleam` | Exists | [TBD: Add tool helpers?] |
| `src/claude_agent_sdk/internal/port_io.gleam` | Exists | [TBD: Stderr capture] |
| `src/claude_agent_sdk/internal/stream.gleam` | Exists | [TBD: Buffer size threading] |
| `src/claude_agent_sdk/error.gleam` | Exists | Add CliNotFoundError |
| `test/hook_list_test.gleam` | New | Hook list + matcher tests |
| `test/cli_discovery_test.gleam` | New | CLI path resolution tests |
| `docs/options.md` | Exists | Document new options |
| `docs/tools-and-permissions.md` | Exists | Document hook matchers, rich permissions |

---

## Risks, Edge Cases & Breaking Changes

### Potential Breaking Changes

1. **PermissionCheckResult extension**: Adding new variants (AllowOnce, AllowAll, Edit) changes exhaustive pattern matching
   - Mitigation: [TBD: Use @deprecated on old type and introduce new type?]

2. **Hook callback signature**: If changing from single callback to list
   - Mitigation: Keep existing `on_pre_tool_use` builder, add new `with_pre_tool_use_hooks` for lists

### Edge Cases

- CLI discovery when multiple versions installed: [TBD: First match or version check?]
- Hook matcher with no matching tools: Execute anyway (no-op matcher = always match)
- Empty hook list: Same as no hook (no-op)
- Multiple hooks returning Block: First Block wins, subsequent hooks skipped
- Stderr mixed with stdout: [TBD: How does CLI handle stderr?]

### Backwards Compatibility

- All existing `with_pre_tool_use()` single-callback usage continues to work
- `cli_path` override continues to skip discovery
- [TBD: Deprecation timeline for old types if breaking]

---

## Testing & Validation Strategy

### Unit Tests

| Area | Test File | Test Cases |
|------|-----------|------------|
| Hook lists | `hook_list_test.gleam` | List execution order; first Block wins; matcher filtering |
| CLI discovery | `cli_discovery_test.gleam` | Explicit path used; fallback to known locations; PATH search |
| Rich permissions | `hook_test.gleam` | AllowOnce/AllowAll/Edit encoding; control protocol mapping |
| Tools flag | `cli_args_test.gleam` | --tools emitted correctly |
| [TBD: Stderr] | [TBD] | [TBD] |

### Integration Tests

| Area | Test File | Test Cases |
|------|-----------|------------|
| Hook execution | `bidir_session_test.gleam` | Multiple hooks fire in order; matchers filter correctly |
| Permission flow | `bidir_session_test.gleam` | Rich permission results handled by CLI |

### E2E Tests

- [TBD: Tests requiring real CLI]

### Acceptance Criteria Coverage

| Gap | Approach | Test Coverage |
|-----|----------|---------------|
| CLI discovery | Fallback chain in cli.gleam | `cli_discovery_test.gleam` |
| Hook matchers | HookEntry with optional matcher | `hook_list_test.gleam` |
| Rich permissions | Extend PermissionCheckResult | `hook_test.gleam` |
| MCP helpers | [TBD] | [TBD] |
| Tools preset | --tools flag | `cli_args_test.gleam` |
| Stderr capture | [TBD] | [TBD] |
| Buffer size | [TBD] | [TBD] |

---

## Open Questions

### Critical (P0) - Must answer before implementation

1. **Streaming input for query()**: Is this needed? Python supports `AsyncIterable` prompts, but bidir mode may cover the use case.
   - Options: [A] Implement streaming query(), [B] Defer - bidir mode sufficient, [C] Skip entirely

2. **Hook matcher scope**: What should matchers support?
   - Options: [A] Tool name exact match only, [B] Tool name + glob patterns, [C] Full regex

3. **Breaking change strategy for PermissionCheckResult**: How to add new variants?
   - Options: [A] Extend existing type (breaking), [B] New type + deprecation, [C] Union type approach

### Important (P1) - Should answer before implementation

4. **MCP tool helpers**: What level of typing is needed?
   - Options: [A] Keep Dynamic handlers, add schema validation helpers, [B] Fully typed tool definitions, [C] Skip - current API sufficient

5. **Stderr capture mechanism**: How to expose stderr?
   - Options: [A] Callback option, [B] Field in error types, [C] Both

6. **Buffer size**: Where should this be configured?
   - Options: [A] SdkOptions, [B] CliOptions, [C] Separate StreamOptions type

### Lower Priority (P2)

7. **CLI discovery additional locations**: What paths beyond `~/.claude/local/`?
8. **Multiple CLIs installed**: Version preference when multiple found?

---

## Next Steps

After this plan is approved, run `/cerberus:create-tasks` to generate:
- `--beads` → Beads issues with dependencies for multi-agent execution
- (default) → TODO.md checklist for simpler tracking
