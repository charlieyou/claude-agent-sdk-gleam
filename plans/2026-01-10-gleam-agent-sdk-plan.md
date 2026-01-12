# Implementation Plan: Claude Agent SDK for Gleam

## Context & Goals
- **Spec**: `plans/CLAUDE_AGENT_SDK_SPEC.md` (authoritative specification document)
- Implement a complete Claude Agent SDK in the Gleam programming language
- Provide programmatic access to Claude Code's agentic capabilities via CLI subprocess
- Support both message-level and token-level streaming (message-level in v1, token-level in v2)
- **Target**: Gleam 1.0+, OTP 27+

**Spec Citation Policy**:
- All spec references use the format: `SPEC: <heading>` (e.g., `SPEC: Message Schema`)
- Do NOT use section numbers (SPEC: Message Schema) as these may shift between spec versions
- The authoritative spec file is `plans/CLAUDE_AGENT_SDK_SPEC.md`; no other files (e.g., `SDK_REQUIREMENTS.md`) are referenced
- If a spec heading is cited, verify it exists in the spec file before implementation

**Spec-Citation Verification Rules (AUTHORITATIVE)**:

| Rule | Definition |
|------|------------|
| **Required** | Spec uses MUST/REQUIRED language under an EXACT heading match |
| **Optional** | Spec uses SHOULD/MAY, or field not mentioned, or heading not found |
| **Heading mismatch** | Update cited heading string + default field to Optional until resolved |

**Verification procedure**:
1. Search spec for exact heading cited in plan
2. If found: check for MUST/REQUIRED language for field
3. If not found: search for alternative headings, update plan, default to Optional
4. Record result in test file comment (≤25 words per field)

**Required artifact** (test/phase1_spec_verification.gleam comment block):
```gleam
// Spec Verification Results - Phase 1
// Date: YYYY-MM-DD
// Spec version: plans/CLAUDE_AGENT_SDK_SPEC.md (commit hash)
//
// Field: type (all messages)
// Heading: SPEC: Message Schema
// Verified: YES - "type field MUST be present"
//
// Field: session_id (SystemMessage)
// Heading: SPEC: System Message (NOT FOUND - used "System Init")
// Verified: NO - SHOULD language only, defaulted to Optional
```

## Naming & Namespace (Canonical - Single Source of Truth)

**All naming decisions are defined here. Other sections MUST reference this section rather than restating.**

| Item | Value | Notes |
|------|-------|-------|
| **Hex package name** | `claude_agent_sdk_gleam` | Published to Hex under this name |
| **Gleam root module namespace** | `claude_agent_sdk` | All modules use this prefix |
| **Erlang FFI module name** | `claude_agent_sdk_ffi` | Matches Gleam namespace (not `_gleam` suffix) |
| **FFI file path** | `src/claude_agent_sdk_ffi.erl` | Module name matches filename |
| **Main module path** | `src/claude_agent_sdk.gleam` | Primary entry point |
| **Submodule path pattern** | `src/claude_agent_sdk/*.gleam` | e.g., `src/claude_agent_sdk/message.gleam` |
| **Internal module path** | `src/claude_agent_sdk/internal/*.gleam` | Implementation details |

**FFI naming under package rename**:
The Erlang FFI module and file are renamed in lockstep with the Gleam namespace. The FFI module is `claude_agent_sdk_ffi` (not `claude_agent_sdk_gleam_ffi`). This ensures:
1. Consistent naming across all SDK components
2. No import breakage due to mismatched FFI module names
3. Clear relationship between Gleam modules and their FFI backing

**Hex package vs Gleam namespace distinction**:
In this SDK, the Hex package name (`claude_agent_sdk_gleam`) differs from the Gleam module namespace (`claude_agent_sdk`). The package name appears in `gleam.toml` under `name`, while the namespace is determined by module file paths under `src/`.

## Key Gating Invariants (Read First)

**This plan is detailed. Use this summary to navigate to critical sections.**

| Invariant | Section | Why It Gates |
|-----------|---------|--------------|
| Phase 0 must pass before other work | [Phase 0 deliverable](#phase-0-deliverable-before-any-other-code) | Port config failure = nothing works |
| v1 uses port_ffi directly (not Runner) | [v1 Production Path](#v1-production-path-canonical) | Wrong abstraction = test mismatch |
| Version policy is single source of truth | [Canonical Version Policy](#canonical-version-policy-single-source-of-truth) | Duplicate logic = drift bugs |
| Port lifecycle: drain-then-close | [Port Lifecycle Contract](#port-lifecycle-contract-single-source-of-truth) | Immediate close = lost messages |
| QueryStream is single-process | [Process Ownership Contract](#process-ownership-contract-critical---single-source-of-truth) | Cross-process = undefined behavior |
| Mailbox isolation: filter on Port | [close() behavior](#close-behavior-with-mailbox-cleanup) | Generic receive = cross-contamination |
| Result is authoritative over exit_status | [Result vs exit_status precedence](#result-vs-exit_status-precedence-rule) | Wrong order = spurious ProcessError |
| raw_json = exact bytes, not re-encoded | [raw_json preservation contract](#raw_json-preservation-contract) | Re-encoding loses precision |
| Result can be success or failure | [Result message semantics](#result-message-semantics-clarification) | Assuming Result=success = bugs |

**Implementation tip**: The detailed matrices and pseudocode provide exhaustive coverage but can be overwhelming. Focus on the gating invariants above first, then drill into detail sections as needed.

**Code Snippet Disclaimer**: All Gleam code snippets in this plan are **pseudocode pending Phase 0 API confirmation**. Specific module names (e.g., `gleam_erlang/os`), function signatures, and patterns are illustrative and may require adjustment based on actual API discovery in Phase 0. The FFI functions specified in [Authoritative FFI Interface](#authoritative-ffi-interface-single-source-of-truth) are commitments; everything else is aspirational.

**Drift prevention rule**: Each invariant has ONE canonical section that defines it. Other sections MUST reference (not restate) these definitions. If you find conflicting statements, the canonical section in the table above is authoritative.

## Scope & Non-Goals

### In Scope (v1)
- Core query execution with message-level streaming
- Full message type system (system, assistant, user, result)
- Configuration options (model, max_turns, budget, permissions, etc.)
- Tool filtering (whitelist/blacklist)
- Session management (new, resume, continue)
- MCP server configuration support (path pass-through)
- Specific error types for different failure modes
- Unit tests (mocked CLI) + integration tests (real CLI)
- **Runner abstraction** for future JavaScript target support
- **Minimal resource-safety helpers** (`with_stream`) - exception to "no helpers" for safe cleanup

### In Scope (v2 - Future)
- Token-level streaming (`--include-partial-messages`)
- JavaScript/Node.js compilation target
- `StreamEvent` message variant
- Stderr capture via wrapper mechanism

### Out of Scope (Non-Goals)
- Browser target (cannot spawn subprocesses)
- Custom tool implementation within SDK (users implement MCP servers separately)
- GUI or interactive terminal features
- Direct API calls to Anthropic (relies on Claude Code CLI)
- MCP config builders (just path pass-through)
- **Domain-specific convenience functions** — functions that interpret or transform message content:
  - `get_final_text()` — extracts text from Result message
  - `count_tokens()` — sums token usage across messages
  - `extract_tool_calls()` — filters to ToolUseBlock content
  - `was_successful()` — interprets ResultSubtype
  - Users pattern-match on messages directly for these operations

### v1 Helper Classification

**Rubric for what's in v1**:
- ✅ **Resource safety**: Ensures port cleanup (prevents resource leaks)
- ✅ **Options builders**: Standard pattern for constructing QueryOptions
- ✅ **Iterator interop**: Enables use with `gleam/iterator` (with documented caveats)
- ❌ **Domain-specific**: Interprets message content or makes semantic decisions

| Helper | Category | In v1? | Rationale |
|--------|----------|--------|-----------|
| `with_stream()` | Resource safety | ✅ | Ensures port cleanup on early return/panic |
| `collect_items()` | Resource safety | ✅ | Guaranteed cleanup, common iteration pattern |
| `collect_messages()` | Resource safety | ✅ | Same as collect_items, filters to Messages |
| `fold_stream()` | Resource safety | ✅ | Guaranteed cleanup with custom accumulation |
| `to_iterator()` | Iterator interop | ✅ | Enables `gleam/iterator`; documented leak risk |
| `with_model()`, etc. | Options builders | ✅ | Standard Gleam builder pattern |
| `get_final_text()` | Domain-specific | ❌ | Interprets Result content |
| `count_tokens()` | Domain-specific | ❌ | Aggregates usage across messages |
| `was_successful()` | Domain-specific | ❌ | Interprets ResultSubtype semantics |

## Assumptions & Constraints

### Implementation Constraints
- **Platform v1**: Erlang VM only (OTP 27+ for gleam_erlang 1.3.0)
- **Platform v2**: Add Node.js via runner abstraction
- **CLI dependency**: Requires `claude` CLI installed and in PATH
- **JSON protocol**: All communication uses NDJSON (newline-delimited JSON)
- **Gleam version**: 1.0+ (stable API, compatible with gleam_stdlib >= 0.44.0)

### Spec Guarantees vs Observed Behavior (Single Source of Truth)

This table distinguishes between what the SDK specification guarantees vs what is observed in current CLI versions. **Reference this table rather than restating assumptions elsewhere.**

| Item | Spec Guarantee | Observed Behavior | SDK Approach |
|------|----------------|-------------------|--------------|
| `--verbose` flag | Required for stream-json (SPEC: CLI Invocation) | Present in CLI 1.0.x | Include unconditionally; fail if rejected |
| `--output-format stream-json` | Documented in spec | Produces NDJSON stdout | Core assumption; SDK built on this |
| Result is last message | NOT guaranteed by spec | Observed in CLI 1.0.x | Treat as assumption; drain after Result |
| Message field presence | Only `type` required | Many fields present | Minimal core fields; rest optional |
| Exit status ordering | NOT guaranteed | Usually after data | State machine tolerates any order |
| EOF message | NOT guaranteed | Platform-dependent | Treat as optional; use exit_status |
| `use_stdio` port option | NOT in spec (OTP) | Required on OTP 27+ | Always include; Phase 0 validates it works |
| Version string format | NOT specified | "claude v1.2.3" | Regex extraction; fail-fast on parse error |
| Stderr behavior | NOT specified | Goes to terminal | Not captured in v1; documented |

**How to use this table**: When documenting behavior elsewhere in this plan, reference this table rather than restating the guarantee/assumption. For example: "See Spec Guarantees table: Result is last message is an assumption, not a guarantee."

### Architecture Constraints
- Use **FFI-only port operations** for subprocess communication (see [Authoritative FFI Interface](#authoritative-ffi-interface-single-source-of-truth)) — `gleam_erlang/port` lacks required options (`spawn_executable`, `use_stdio`)
- Simple blocking iteration for streaming (not OTP actors)
- Functional `query()` API (not client-object pattern)
- Runner abstraction via opaque type with function records (Gleam doesn't have traits)

**Clarification: "not OTP actors" scope**:

The SDK does NOT introduce supervised processes or GenServers. However, it DOES use Erlang process primitives:

| Primitive | Used in SDK | Rationale |
|-----------|-------------|-----------|
| `receive` with timeout | ✅ Version detection, port reads | Blocking wait for port messages |
| Process mailbox | ✅ Port messages arrive here | Standard port communication |
| Process dictionary | ✅ Test mocks (optional) | State management in tests |
| GenServer | ❌ Not used | Adds supervision complexity |
| Supervisor | ❌ Not used | SDK is a library, not an application |
| Process spawning | ❌ Not used by SDK | User may wrap SDK in their own processes |

The distinction: "blocking `receive` in the calling process" is a synchronous primitive (like reading from a file), while "OTP actors" implies concurrent GenServers with supervision. The SDK uses the former, not the latter.

### Testing Constraints
- Unit tests: Mock CLI via `test_runner()` function record for stream semantics testing
- Integration tests: Real CLI, **opt-in via `CLAUDE_INTEGRATION_TEST=1`** environment variable
- Integration tests skip with message when CLI unavailable or env var not set
- Critical paths: All message type parsing, error handling, option building

**Unit Test Isolation Contract (Canonical - Single Source of Truth)**:

Unit tests MUST NOT require a real `claude` binary or system PATH lookup. The contract:

| Component | Unit-Testable? | How |
|-----------|----------------|-----|
| `parse_version_string()` | ✅ Yes | Pure function, no I/O |
| `version_meets_minimum()` | ✅ Yes | Pure function, no I/O |
| Stream semantics | ✅ Yes | Via `test_runner()` + `skip_version_check: True` |
| CLI arg building | ✅ Yes | Pure function, no I/O |
| JSON decoders | ✅ Yes | Pure functions, no I/O |
| `find_executable()` | ❌ **No** | Real FFI, integration tests only |
| Version detection (spawn) | ❌ **No** | Real FFI, integration tests only |
| `CliNotFoundError` behavior | ❌ **No** | Requires `find_executable()` FFI |

**Note on CliNotFoundError**: The actual "CLI not in PATH" scenario requires calling `find_executable()` which is OS FFI. Unit tests verify the error type exists and is constructable; integration tests verify the actual behavior when `claude` is missing from PATH.

**Mandatory unit test setup**:
1. **Always set `test_mode: True`** in options
2. **Always set `skip_version_check: True`**
3. **Provide mock via `test_runner()`** for stream semantics tests

```gleam
// PSEUDOCODE - pending Phase 0 API confirmation
// Standard unit test setup - NO real CLI needed
pub fn stream_semantics_test() {
  // test_runner provides mock spawn/read/close that doesn't touch port_ffi
  let mock_runner = test_runner(
    on_spawn: fn(_, _, _) { Ok(dynamic.from(mock_state)) },
    on_read: mock_read_fn,
    on_close: fn(_) { Nil },
  )

  let options = default_options()
    |> with_test_mode(True, mock_runner)  // REQUIRED: enables test_runner path
    |> with_skip_version_check(True)       // REQUIRED: skips version detection

  // query() uses test_runner instead of port_ffi when test_mode=True
  let assert Ok(stream) = query("test prompt", options)
  // ... test stream behavior via mock_read_fn responses ...
}

// Version parsing unit tests (isolated, pure functions - no mocking needed)
pub fn parse_version_string_test() {
  let assert Ok(CliVersion(1, 2, 3, _)) = parse_version_string("claude v1.2.3\n")
}
```

**What `test_mode: True` enables**:
- Production code path: `query()` → `port_ffi.ffi_open_port()` → `port_ffi.receive_blocking()`
- Test code path: `query()` → `test_runner.spawn()` → `test_runner.read_next()`

**What `skip_version_check: True` bypasses**:
1. `find_executable("claude")` call
2. Spawning `claude --version`
3. Version parsing and comparison

**Integration tests verify**: PATH lookup, version detection, real CLI interaction via `port_ffi`.

**Why this matters**: Unit tests should run in any environment (CI, containers, dev machines) without requiring `claude` to be installed. Only integration tests (opt-in) need the real CLI.

**Test dependency policy**:

| Dependency | Unit Tests | Integration Tests | SDK Runtime | gleam.toml Section |
|------------|------------|-------------------|-------------|-------------------|
| `gleam_stdlib` | ✅ Yes | ✅ Yes | ✅ Yes | `[dependencies]` |
| `gleam_erlang` | ✅ Yes | ✅ Yes | ✅ Yes | `[dependencies]` |
| `gleam_json` | ✅ Yes | ✅ Yes | ✅ Yes | `[dependencies]` |
| `gleeunit` | ✅ Yes | ✅ Yes | ❌ No | `[dev-dependencies]` |
| `gleam_otp` | ❌ **No** | ❌ **No** | ❌ **No** | N/A (not used) |
| `envoy` | ❌ **No** | ❌ **No** | ❌ **No** | Use FFI instead (see below) |

**No envoy dependency**: Environment variable access in tests uses a tiny FFI helper instead of adding `envoy` as a dependency. This reduces supply chain surface for minimal benefit.

```gleam
// In test/support/env_helpers.gleam
@external(erlang, "os", "getenv")
fn ffi_getenv(name: String) -> Dynamic

pub fn get_env(name: String) -> Result(String, Nil) {
  case dynamic.string(ffi_getenv(name)) {
    Ok(value) if value != "" -> Ok(value)
    _ -> Error(Nil)  // Not set or empty
  }
}
```

```toml
# gleam.toml snippet
[dev-dependencies]
gleeunit = "= 1.2.0"
# No envoy - using FFI helper instead
```

**Why no `gleam_otp` in tests**:
- SDK's "not OTP actors" constraint extends to tests to avoid scope creep
- Unit tests use process dictionary or ETS via `gleam_erlang` FFI (pure BEAM primitives)
- The cancellation recipe in docs is documentation-only (users add `gleam_otp` themselves)
- Keeps test dependencies minimal and SDK self-contained

### CLI Compatibility

**Minimum supported CLI version**: Claude Code CLI 1.0.0+ with `--output-format stream-json` support

#### Canonical Version Policy (Single Source of Truth)

All version-related behavior is defined here. Other sections reference this policy rather than restating it.

**Version flow**:
1. Resolve `claude` in PATH using `os:find_executable/1`
2. If not found → return `CliNotFoundError` immediately (no spawn attempt)
3. If found → run `claude --version` and parse output
4. Apply version policy (see table below)

**Version policy table**:

| Scenario | Default Behavior | With `permissive_version_check: True` |
|----------|------------------|--------------------------------------|
| Version parses, ≥1.0.0 | ✅ Proceed | ✅ Proceed |
| Version parses, <1.0.0 | ❌ `UnsupportedCliVersionError` | ❌ `UnsupportedCliVersionError` |
| Version parse fails | ❌ `UnknownVersionError` | ⚠️ Proceed with warning |
| Version detection fails (timeout/error) | ❌ `VersionDetectionError` | ⚠️ Proceed with warning |

**Fail-fast rationale**: Unknown version + fixed flags = likely `ProcessError(2)` with confusing diagnostics. Fail-fast is easier to debug than decode errors deep in the stream. Users with known-compatible enterprise/pre-release CLIs opt into permissive mode explicitly.

**Version parsing rules** (defined once, referenced elsewhere):
- Extract first `MAJOR.MINOR.PATCH` pattern via regex `(\d+)\.(\d+)\.(\d+)`
- Tolerate prefixes: `"claude "`, `"v"`, whitespace
- Ignore prerelease suffixes (`-beta.1`, `-rc.2`) for comparison
- Examples: `"claude v1.2.3"` → `CliVersion(1, 2, 3, "1.2.3")`

**Type definitions**:
```gleam
pub type CliVersion {
  CliVersion(major: Int, minor: Int, patch: Int, raw: String)
  UnknownVersion(raw_output: String)  // Parse failed but CLI exists
}
```

**Version comparison** (single implementation):
```gleam
/// Single source of truth for version comparison. Returns True if `version >= minimum`.
pub fn version_meets_minimum(version: CliVersion, minimum: CliVersion) -> Bool
```

**Version detection implementation** (reuses proven port configuration):

**Version detection uses raw FFI, not Runner (explicit design decision)**:

Version detection uses raw FFI port operations instead of the `Runner` abstraction. This is intentional:

| Aspect | Runner | Raw FFI (chosen for version detection) |
|--------|--------|----------------------------------------|
| Abstraction | Platform-agnostic `Handle` | BEAM-specific `Port` |
| Timeout support | Not exposed (blocks until data) | `receive ... after` for timeout |
| Use case | Main streaming (needs abstraction) | One-time check (BEAM-only ok) |
| Testing | Via `test_runner()` | Integration tests only |

**Why not Runner for version detection**:
1. **Timeout needed**: Version detection must timeout (5s) to avoid hanging. Runner's `read_next` blocks indefinitely.
2. **Simpler**: Version check is spawn → read all → close, not streaming iteration.
3. **No abstraction benefit**: Version detection is internal; it doesn't need to be mockable separately.
4. **Same FFI**: Uses same `ffi_open_port` as Runner internals, ensuring consistent port configuration.

**v2 impact**: When adding JS target, version detection would need JS-specific implementation (same as Runner internals). This is acceptable since it's internal code.

```gleam
// In src/claude_agent_sdk/internal/cli.gleam

/// Internal: Run `claude --version` and parse output.
/// Uses RAW FFI (not Runner) because version detection needs timeout support.
/// See [Architecture Boundary](#architecture-boundary-runner-vs-internal-port-operations-critical).
fn detect_cli_version(cli_path: String) -> Result(CliVersion, QueryError) {
  // 1. Spawn port via FFI (BEAM-only, not via Runner)
  let port = ffi_open_port(cli_path, ["--version"], ".")

  // 2. Read all output until exit (version string is short)
  let output = drain_port_until_exit(port, default_version_timeout_ms())

  // 3. Port auto-closes on exit_status message

  // 4. Parse version string
  case parse_version_string(output) {
    Ok(version) -> Ok(version)
    Error(_) -> Ok(UnknownVersion(output))  // Caller decides: fail-fast or permissive
  }
}

/// Default timeout for version check (injectable for testing)
fn default_version_timeout_ms() -> Int { 5000 }

/// Drain port until exit_status received. Accumulates all data.
/// Timeout to avoid hanging on misbehaving CLI.
///
/// IMPLEMENTATION NOTE: Uses Erlang's `receive ... after` construct.
/// This is NOT an OTP actor/GenServer - it's a simple blocking receive
/// with timeout in the calling process's mailbox.
fn drain_port_until_exit(port: Port, timeout_ms: Int) -> Result(String, VersionCheckError) {
  // Erlang implementation using receive with timeout (stream mode):
  // ```erlang
  // drain_loop(Port, Acc, Timeout) ->
  //     receive
  //         {Port, {data, Bytes}} ->
  //             drain_loop(Port, <<Acc/binary, Bytes/binary>>, Timeout);
  //         {Port, {exit_status, _Code}} ->
  //             {ok, Acc}
  //     after Timeout ->
  //         port_close(Port),
  //         {error, timeout}
  //     end.
  // ```
  //
  // On timeout: port is closed, returns error
  // On success: returns accumulated binary as string
}

/// Parse version string from CLI output.
/// See [Canonical Version Policy](#canonical-version-policy-single-source-of-truth) for parsing rules.
fn parse_version_string(output: String) -> Result(CliVersion, Nil) {
  // 1. Trim whitespace
  // 2. Use regex to find first \d+\.\d+\.\d+ match
  // 3. Parse each component as Int
  // 4. Return CliVersion(major, minor, patch, "major.minor.patch")
}
```

**Port configuration consistency**:
The version detection port uses the **exact same options** as the query port. Both use the FFI `open_port/3` function defined in [Authoritative FFI Interface](#authoritative-ffi-interface-single-source-of-truth).

**GATING DELIVERABLE: Port API Validation (Phase 0 - Must Complete First)**:

The port options above are based on Erlang `open_port/2` semantics. **This validation is a blocking prerequisite** - no further implementation proceeds until port options are confirmed working.

**API Reality Check (Required APIs and Fallback Strategy)**:

| API Need | Implementation | Status |
|----------|----------------|--------|
| Find executable | FFI: `@external(erlang, "os", "find_executable")` | Use FFI directly |
| OS type detection | FFI: `@external(erlang, "os", "type")` | Use FFI directly |
| Open port | FFI: direct `erlang:open_port/2` | Use FFI directly |
| Port options | FFI: build option list in Erlang | Use FFI directly |
| Receive with timeout | FFI: `@external` wrapper | Use FFI directly |

**v1 API Strategy**: Do NOT rely on `gleam_erlang/os` or other potentially-nonexistent modules. All OS/port operations use pure Erlang FFI from day one. This avoids Phase 0 compile failures from missing Gleam modules.

**FFI-only implementation (authoritative decision)**:

The SDK uses FFI exclusively for port operations. `gleam_erlang/port` does NOT expose the required options (`spawn_executable`, `use_stdio`, etc.), so there is no conditional — FFI is the ONLY implementation.

See [Authoritative FFI Interface](#authoritative-ffi-interface-single-source-of-truth) for the complete FFI module specification.

**Phase 0 API spike**: Before any other code, create a minimal spike that validates ALL port_ffi operations work (v1 production path):

```gleam
// test/phase0_port_ffi_validation_test.gleam - RUN FIRST
// Phase 0 validates the EXACT port_ffi interface that v1 production uses
pub fn phase0_port_ffi_validation_test() {
  // Get platform-specific test command (see cross-platform strategy below)
  let #(exe, args) = phase0_test_command()

  // 1. Spawn via port_ffi.ffi_open_port (same call v1 production uses)
  let port = port_ffi.ffi_open_port(exe, args, ".")

  // 2. Read via port_ffi.receive_blocking (validates message shape decoding)
  let assert Ok(Data(bytes)) = port_ffi.receive_blocking(port)
  let assert True = bit_array.byte_size(bytes) > 0

  // 3. Read exit status via port_ffi.receive_blocking
  let assert Ok(ExitStatus(0)) = port_ffi.receive_blocking(port)

  // 4. Close via port_ffi.ffi_close_port
  port_ffi.ffi_close_port(port)
}

// Separate test for timed receive (used by version detection and drains)
pub fn phase0_timed_receive_test() {
  let #(exe, args) = phase0_test_command()
  let port = port_ffi.ffi_open_port(exe, args, ".")

  // Timed receive should return data (not timeout) for immediate response
  let assert Ok(Data(_)) = port_ffi.receive_timeout(port, 5000)

  // After exit, timed receive with short timeout should return Timeout
  let _ = port_ffi.receive_timeout(port, 100)  // drain exit_status
  let assert Ok(Timeout) = port_ffi.receive_timeout(port, 50)

  port_ffi.ffi_close_port(port)
}
```

**Cross-platform Phase 0 test command strategy**:

Use a single deterministic command per platform that produces predictable output:

| Platform | Executable | Args | Expected Output |
|----------|-----------|------|-----------------|
| Linux/macOS | `/bin/echo` | `["test"]` | `test\n` (5 bytes) |
| Windows | `cmd.exe` | `["/c", "echo test"]` | `test\r\n` (6 bytes) |

```gleam
/// OS type detection: returns Dynamic, decode manually
/// Erlang os:type/0 returns {Family, Name} where Family is atom `unix` or `win32`
@external(erlang, "os", "type")
fn ffi_os_type() -> Dynamic

/// Decode OS type from Erlang atoms
fn decode_os_family(raw: Dynamic) -> Result(OsFamily, Nil) {
  case dynamic.tuple2(dynamic.dynamic, dynamic.dynamic)(raw) {
    Ok(#(family_dyn, _name_dyn)) ->
      // Erlang atoms decode as strings in Gleam dynamic
      case dynamic.string(family_dyn) {
        Ok("unix") -> Ok(Unix)
        Ok("win32") -> Ok(Win32)
        _ -> Error(Nil)  // Unknown OS family
      }
    Error(_) -> Error(Nil)
  }
}

type OsFamily {
  Unix
  Win32
}

fn phase0_test_command() -> #(String, List(String)) {
  case decode_os_family(ffi_os_type()) {
    Ok(Unix) -> #("/bin/echo", ["test"])
    Ok(Win32) -> #("cmd.exe", ["/c", "echo test"])
    Error(_) -> #("/bin/echo", ["test"])  // Default to Unix if unknown
  }
}
```

**FFI Type Representation (CANONICAL)**:

| Erlang Type | Gleam Representation | Decode Strategy |
|-------------|---------------------|-----------------|
| Port term | `opaque type Port { Port(Dynamic) }` | Wrap raw Dynamic from FFI |
| Atoms | `Dynamic` | Decode as string in Gleam |
| Tuples | `Dynamic` | Use `dynamic.tuple2/tuple3` etc. |
| `{Family, Name}` from os:type | `Dynamic` | Decode atoms to `OsFamily` variant |

**Why opaque types with Dynamic**: Gleam cannot directly represent Erlang's port terms or atoms at the type level. Using opaque types with Dynamic internals provides type safety at the API boundary while allowing FFI to pass raw terms.

**Why `/bin/echo` not `/bin/sh -c "echo ..."`**:
- `/bin/echo` is a simple external binary with predictable behavior
- No shell escaping or interpretation issues
- Faster (no shell startup)
- Behavior consistent across Unix variants

**v1 Production Path (Canonical)**:

v1 uses `port_ffi` directly for ALL production operations. The `Runner` abstraction exists for:
- **Unit test mocks**: `test_runner()` provides mock spawn/read/close for testing stream semantics
- **Future v2 multi-platform**: When adding Node.js support, Runner becomes the real abstraction

| v1 Operation | Implementation | Module |
|--------------|----------------|--------|
| Production spawn | `port_ffi.ffi_open_port()` directly | `internal/port_ffi.gleam` |
| Production read | `port_ffi.receive_blocking()` directly | `internal/port_ffi.gleam` |
| Production close | `port_ffi.ffi_close_port()` directly | `internal/port_ffi.gleam` |
| Unit test mock | `test_runner()` function record | `runner.gleam` |

**v1 Canonical Call Graph (AUTHORITATIVE - prevents implementation drift)**:

```
v1 PRODUCTION (test_mode=False):                 v1 TESTS (test_mode=True):
─────────────────────────────────────           ─────────────────────────────────
query()                                          query()
  └─→ find_executable("claude")                    └─→ [skipped - skip_version_check]
  └─→ detect_cli_version()                         └─→ test_runner.spawn()
        └─→ port_ffi.ffi_open_port()                     └─→ [mock, no OS process]
        └─→ port_ffi.receive_timeout()
  └─→ port_ffi.ffi_open_port()
        └─→ returns Port (Erlang term)

next()                                           next()
  └─→ port_ffi.receive_blocking(port)              └─→ test_runner.read_next(handle)
        └─→ returns Result(PortMessage, String)          └─→ [mock data from on_read fn]

close()                                          close()
  └─→ port_ffi.ffi_close_port(port)                └─→ test_runner.close(handle)
        └─→ port_close + mailbox drain                   └─→ [mock cleanup]
```

**Key rules**:
1. **v1 production NEVER calls Runner functions** — `query()/next()/close()` use `port_ffi.*` directly
2. **v1 tests NEVER call port_ffi functions** — `test_mode=True` routes through `test_runner()`
3. **`erlang_runner()` exists but is UNUSED in v1** — it's a v2 artifact for multi-platform support
4. **Runner type is NOT in the v1 public API** — only `test_runner()` is exported (for tests)

**Why erlang_runner() exists if unused**: It provides a consistent interface for v2 where `query()` will take a `Runner` parameter to support both BEAM and Node.js. In v1, we hardcode the `port_ffi` path for simplicity.

**v1 Public API Surface for Runner (AUTHORITATIVE)**:

| Item | v1 Status | Notes |
|------|-----------|-------|
| `Runner` type | ❌ NOT public | Internal only; not in public exports |
| `erlang_runner()` | ❌ NOT public | Internal; unused in v1 production |
| `test_runner()` | ✅ **Public** | For unit tests only |
| `with_runner()` | ❌ DOES NOT EXIST in v1 | Use `with_test_mode()` instead |
| `QueryOptions.runner` | ❌ DOES NOT EXIST in v1 | Use `test_mode` + `test_runner` fields |
| `ReadResult` type | ❌ NOT public | Internal to test_runner |

**v1 QueryOptions for testing**:
```gleam
// v1: No runner field. Test mode uses separate fields.
pub type QueryOptions {
  QueryOptions(
    // ... CLI options ...
    test_mode: Bool,                  // Enable test path (default: False)
    test_runner: Option(TestRunner),  // Mock for tests (only used if test_mode=True)
    skip_version_check: Bool,         // Skip version detection
  )
}
```

**Runner.spawn signature** (for test_runner and future v2): `spawn(executable_path: String, args: List(String), cwd: String) -> Result(Handle, SpawnError)`

**Path Resolution and Port Operations (clarification)**:

The `executable_path` is resolved ONCE via `find_executable("claude")` at `query()` entry. This resolved path is then used for:
1. **Version detection** — uses `port_ffi.receive_timeout` (5s max to avoid hanging)
2. **Query streaming** — uses `port_ffi.receive_blocking` (blocks until next message)

```gleam
pub fn query(prompt, options) -> Result(QueryStream, QueryError) {
  // 1. Resolve path ONCE
  let cli_path = case find_executable("claude") {
    Ok(path) -> path
    Error(_) -> return Error(CliNotFoundError(...))
  }

  // 2. Version detection - uses receive_timeout (5s max)
  case detect_cli_version(cli_path) { ... }  // Uses port_ffi.receive_timeout

  // 3. Query spawn - uses port_ffi directly (v1 production path)
  let port = port_ffi.ffi_open_port(cli_path, args, cwd)
  // QueryStream uses port_ffi.receive_blocking for reads
}
```

**Why two receive mechanisms with same port**:
- **Path resolved once**: Guarantees version check and query target the exact same binary
- **Different receive timeouts**: Version detection uses `receive_timeout` (5s max); query uses `receive_blocking`
- **Same FFI**: Both use `port_ffi` functions - the difference is blocking vs timed receive

**Phase 0 deliverable** (before any other code):
1. **API Confirmation Step** (FIRST - see table below)
2. Implement `port_ffi.gleam` with open_port/receive_blocking/receive_timeout/close_port (v1 production path)
3. Implement `test_runner()` for unit test mocks (optional - can defer to Phase 1 if not needed immediately)
4. Validate port_ffi operations via Phase 0 tests (spawn → read → exit_status → close)
5. Document the Erlang-to-Gleam message mapping
6. Pin dependency versions in `gleam.toml` (see Prerequisites)
7. Only then proceed to Phase 1

**v1 Port Operations Boundary (Canonical - Single Source of Truth)**:

v1 uses `port_ffi` directly for all production operations. The distinction is between blocking and timed receive.

| Operation | Module | Port Access | Timeout |
|-----------|--------|-------------|---------|
| `query()` streaming | `internal/stream.gleam` | `port_ffi.receive_blocking(port)` | ❌ Blocks |
| `detect_cli_version()` | `internal/cli.gleam` | `port_ffi.receive_timeout(port, ms)` | ✅ 5s max |
| Post-Result drain | `internal/stream.gleam` | `port_ffi.receive_timeout(port, ms)` | ✅ 100ms |
| Post-exit drain | `internal/stream.gleam` | `port_ffi.receive_timeout(port, ms)` | ✅ 50ms |
| Unit test mocks | `test/*.gleam` | `test_runner()` function record | N/A (mocked) |

**Why two receive variants**:
1. **Blocking**: For main streaming loop - must wait for CLI output
2. **Timed**: For version detection (5s max) and drains (short timeouts) - must not hang indefinitely

**test_runner() role**: Provides mock spawn/read/close for unit tests without touching port_ffi. Production code never uses Runner in v1.

**Phase 0 Two-Suite Structure (AUTHORITATIVE)**:

Phase 0 is explicitly divided into two suites with different requirements:

**Suite A: Compile Checks (ALWAYS REQUIRED)**
| Check | Required | What It Validates |
|-------|----------|-------------------|
| `gleam build` succeeds | ✅ REQUIRED | Project structure, dependencies |
| `gleam test test/phase0_compile_check_test.gleam` | ✅ REQUIRED | FFI bindings exist and typecheck |

**Suite B: Runtime Spawn Checks**
| Check | Status | Skip Mechanism |
|-------|--------|----------------|
| port_ffi spawn/read/close | ✅ REQUIRED | `[SKIP:PHASE0]` stdout line |
| Timed receive validation | ✅ REQUIRED | `[SKIP:PHASE0]` stdout line |

**Execution rules**:
1. Suite A MUST pass in ALL environments (compile-only, no subprocess spawning)
2. Suite B runs with the standard test suite
3. Suite B NEVER writes to filesystem (no `file.write()` calls)
4. Suite B skips emit `[SKIP:PHASE0] reason` to stdout only

**Phase 0 is satisfied when**:
1. Suite A passes (compile + typecheck)
2. Suite B EITHER passes OR is skipped with stdout message

**Phase 0 Skip Mechanism (stdout-only, no file writes)**:

Suite B skips use stdout ONLY. No filesystem writes.

**Skip output format**:
```
[SKIP:PHASE0] port_spawn_and_receive_test: No echo command in container
[SKIP:PHASE0] timed_receive_test: Port receive not available
```

**Implementation**:
```gleam
fn record_phase0_skip(test_name: String, reason: String) -> Nil {
  let line = "[SKIP:PHASE0] " <> test_name <> ": " <> reason
  io.println(line)  // Stdout only - NO file.write()
}

pub fn phase0_runtime_spawn_test() {
  run_spawn_test()
}
```

**If runtime validation skipped**:
- **Before release**: At least ONE known-good environment (Linux, macOS, or Windows) MUST run full runtime validation
- CI may skip runtime tests in minimal containers; release gates must not

**Confirmation artifact** (created by implementer):
```markdown
# Phase 0 Confirmation
Date: YYYY-MM-DD
Environment: Linux/macOS/Windows, OTP version, Gleam version

## Compile Check: PASS/FAIL
## Runtime Port Test: PASS/SKIP (reason)
## Notes: ...
```

**Phase 0 API Confirmation (Required First Step)**:

Before writing implementation code:
1. **Pin dependency versions** in `gleam.toml` (no floating ranges)
2. **Verify required APIs exist** at those exact versions
3. **Record confirmed imports** in `src/claude_agent_sdk/internal/confirmed_imports.gleam`

**Dependency Version Policy (v1)**:

v1 uses **exact version pins** for reproducibility. Ranges are NOT used.

```toml
# gleam.toml - Phase 0 must set exact versions
[dependencies]
gleam_stdlib = "= 0.44.0"   # Pin after Phase 0 verification
gleam_erlang = "= 1.3.0"    # Pin after Phase 0 verification
gleam_json = "= 2.0.0"      # Pin after Phase 0 verification

[dev-dependencies]
gleeunit = "= 1.2.0"        # Pin after Phase 0 verification
# No envoy - using test/support/env_helpers.gleam FFI instead
```

**Lockfile policy**: Commit `manifest.toml` (Gleam's lockfile) to version control. CI uses the lockfile to ensure reproducible builds.

**Why exact pins, not ranges**: Phase 0 verifies specific APIs exist. Floating ranges could introduce breaking changes in CI or user builds. The confirmed_imports.gleam file is only valid for the pinned versions.

**Confirmed imports file** (created in Phase 0, referenced by all other modules):

```gleam
// src/claude_agent_sdk/internal/confirmed_imports.gleam
// Phase 0 verified these imports exist at pinned versions.
// All modules should import from here to ensure consistency.

// Standard library (verified at gleam_stdlib = 0.44.0)
import gleam/bit_array  // byte_size, from_string, to_string
import gleam/dynamic    // tuple2, string, int, bit_array
import gleam/regex      // from_string, scan
import gleam/result     // map, try, unwrap
import gleam/list       // map, filter, fold
import gleam/option     // Some, None, unwrap

// Erlang interop - ALL via FFI (no gleam_erlang/os dependency)
// OS operations use @external(erlang, "os", ...) directly
// Port operations use @external in claude_agent_sdk_ffi.erl

// JSON (verified at gleam_json = 2.0.0)
import gleam/json       // decode, object, array, string, int
```

| API Need | Check Method | Confirmed Import | If Missing |
|----------|--------------|------------------|------------|
| `os:find_executable/1` | Check gleam_erlang source | TBD in Phase 0 | FFI: `@external(erlang, "os", "find_executable")` |
| `os:type/0` | Check gleam_erlang source | TBD in Phase 0 | FFI: `@external(erlang, "os", "type")` |
| `bit_array.byte_size/1` | Compile check | `gleam/bit_array` | Should exist |
| `dynamic.tuple2` | Compile check | `gleam/dynamic` | Should exist |
| `regex.from_string` | Compile check | `gleam/regex` | Should exist |
| `json.decode` | Compile check | `gleam/json` | Should exist (v2.0.0+) |
| `json.object` (for tests) | Compile check | `gleam/json` | Build strings manually if missing |
| `json.to_string` (for tests) | Compile check | `gleam/json` | Build strings manually if missing |

**Note on JSON APIs for tests**: Large-payload fixture generation may use `json.object`/`json.to_string`. If these APIs differ across gleam_json versions, build fixture strings manually to reduce encoder surface dependency.

**Confirmation test** (must pass before proceeding):

```gleam
// test/phase0_api_confirmation_test.gleam - RUN FIRST
import gleam/bit_array
import gleam/dynamic
import gleam/regex

pub fn api_confirmation_test() {
  // 1. Verify bit_array functions exist and work
  let bytes = bit_array.from_string("test\n")
  let assert True = bit_array.byte_size(bytes) == 5
  let assert Ok("test\n") = bit_array.to_string(bytes)

  // 2. Verify dynamic decoding works for tagged tuples
  let test_tuple = #("data", <<1, 2, 3>>)
  let dyn = dynamic.from(test_tuple)
  let decoder = dynamic.tuple2(dynamic.string, dynamic.bit_array)
  let assert Ok(#("data", <<1, 2, 3>>)) = decoder(dyn)

  // 3. Verify regex works for version parsing
  let assert Ok(re) = regex.from_string("(\\d+)\\.(\\d+)\\.(\\d+)")
  let assert [match, ..] = regex.scan(re, "claude v1.2.3")
  let assert True = match.content == "1.2.3"
}
```

**If any API is missing**: Document the gap, implement FFI binding, and update this section with the actual solution before proceeding.

**Verification in Restricted Environments (CI/Containers)**:

The "Check `gleam_stdlib` source" method assumes you can fetch dependencies. In restricted environments:

| Environment | Constraint | Verification Method |
|-------------|-----------|---------------------|
| Normal dev | Network available | `gleam add` + source inspection |
| Offline CI | Deps pre-vendored | `gleam.toml` version pins + compile test |
| Air-gapped | No network ever | Version pins + minimal "compiles/imports" tests |

**Alternative verification (no source inspection)**:

```gleam
// test/phase0_compile_check_test.gleam
// If this compiles, required APIs exist at pinned versions

import gleam/bit_array
import gleam/dynamic
import gleam/regex

/// This test verifies that required APIs exist by attempting to use them.
/// If compilation succeeds, the APIs exist in the pinned dependency versions.
/// No runtime behavior tested - just existence.
pub fn api_exists_compile_check_test() {
  // These calls will fail compilation if APIs don't exist
  let _ = bit_array.from_string
  let _ = bit_array.byte_size
  let _ = bit_array.to_string
  let _ = dynamic.tuple2
  let _ = dynamic.string
  let _ = dynamic.int
  let _ = dynamic.bit_array
  let _ = regex.from_string
  let _ = regex.scan
  Nil
}
```

**Recommended `gleam.toml` pins** (v1 baseline):

```toml
[dependencies]
gleam_stdlib = "= 0.44.0"
gleam_erlang = "= 1.3.0"
gleam_json = "= 2.0.0"
```

**Workflow for restricted environments**:
1. **Lock phase**: Developer with network runs `gleam deps download` and commits lockfile
2. **CI phase**: CI uses lockfile, runs `gleam build && gleam test test/phase0_compile_check_test.gleam`
3. **If compile succeeds**: APIs exist at locked versions, proceed with Phase 1

This avoids needing to inspect dependency sources at plan-execution time.

**Erlang FFI file placement and build verification**:

Gleam projects compile Erlang sources placed in `src/`. Verify the following in Phase 0:

| Item | Expected |
|------|----------|
| FFI file location | `src/claude_agent_sdk_ffi.erl` |
| Module name | `-module(claude_agent_sdk_ffi).` (matches filename) |
| Exports | `-export([open_port/3, receive_port_msg_blocking/1, receive_port_msg_timeout/2, close_port/1]).` |
| Build test | `gleam build` compiles `.erl` to `.beam` without errors |
| Test import | `gleam test` can call `@external` functions |

**If FFI compilation fails**: Check that:
- Module name matches filename exactly (case-sensitive)
- No syntax errors in Erlang code
- `gleam.toml` doesn't have unusual source exclusions
- Erlang is installed (OTP 27+)

**Authoritative FFI Interface (Single Source of Truth)**:

The SDK uses FFI-only for port operations. `gleam_erlang/port` is NOT used because it doesn't expose the required options (`spawn_executable`, `use_stdio`, etc.).

**Erlang FFI module** (`src/claude_agent_sdk_ffi.erl`):

```erlang
-module(claude_agent_sdk_ffi).
-export([open_port/3, receive_port_msg_blocking/1, receive_port_msg_timeout/2, close_port/1]).

%% Spawn port with all required options
open_port(Path, Args, Cwd) ->
    erlang:open_port({spawn_executable, Path}, [
        {args, Args}, {cd, Cwd}, stream, binary, exit_status, use_stdio
    ]).

%% BLOCKING receive - used by stream.gleam for v1 production reads
%% Returns STRING tags as 2-tuples for uniform Gleam decoding
receive_port_msg_blocking(Port) ->
    receive
        {Port, {data, Bytes}} -> {"data", Bytes};
        {Port, {exit_status, Code}} -> {"exit_status", Code};
        {Port, eof} -> {"eof", nil}                        %% 2-tuple with nil
    end.

%% TIMED receive - used ONLY for version detection + drains (internal)
%% Returns STRING tags as 2-tuples for uniform Gleam decoding
receive_port_msg_timeout(Port, TimeoutMs) ->
    receive
        {Port, {data, Bytes}} -> {"data", Bytes};
        {Port, {exit_status, Code}} -> {"exit_status", Code};
        {Port, eof} -> {"eof", nil}
    after TimeoutMs -> {"timeout", nil}                    %% 2-tuple with nil
    end.

%% Close port and drain mailbox (bounded)
close_port(Port) ->
    erlang:port_close(Port),
    drain_port_mailbox(Port, 0).

%% Bounded drain: max 100 messages OR 50ms timeout per message
%% This prevents infinite loops from misbehaving subprocesses
%% Parameters:
%%   - Max iterations: 100 (hard cap)
%%   - Timeout per message: 50ms (detects end of pending messages)
%%
%% Port Cleanup Contract (documented alongside timeout in canonical section):
%%   | Parameter | Value | Rationale |
%%   |-----------|-------|-----------|
%%   | Max messages | 100 | Prevents unbounded drain from buggy ports |
%%   | Timeout | 50ms | Short window for scheduler timing |
drain_port_mailbox(Port, Count) when Count >= 100 ->
    ok;  % Hard cap reached, stop draining
drain_port_mailbox(Port, Count) ->
    receive {Port, _} -> drain_port_mailbox(Port, Count + 1)
    after 50 -> ok
    end.
```

**Gleam bindings** (`src/claude_agent_sdk/internal/port_ffi.gleam`):

```gleam
import gleam/dynamic.{type Dynamic}

/// Port reference - opaque wrapper around Erlang port term
/// Represented as Dynamic internally; type safety comes from the opaque API
pub opaque type Port {
  Port(inner: Dynamic)
}

/// Internal message type (NOT public API)
type PortMessage {
  Data(BitArray)
  ExitStatus(Int)
  Eof
  Timeout  // Only returned by timed receive
}

// FFI bindings - exact function names match Erlang exports
// Returns Dynamic which we wrap in opaque Port type
@external(erlang, "claude_agent_sdk_ffi", "open_port")
fn ffi_open_port_raw(path: String, args: List(String), cwd: String) -> Dynamic

/// Open port with type-safe wrapper
pub fn ffi_open_port(path: String, args: List(String), cwd: String) -> Port {
  Port(ffi_open_port_raw(path, args, cwd))
}

@external(erlang, "claude_agent_sdk_ffi", "receive_port_msg_blocking")
fn ffi_receive_blocking_raw(port: Dynamic) -> Dynamic

/// Extract inner Dynamic for FFI calls
fn port_to_dynamic(port: Port) -> Dynamic {
  let Port(inner) = port
  inner
}

@external(erlang, "claude_agent_sdk_ffi", "receive_port_msg_timeout")
fn ffi_receive_timeout_raw(port: Dynamic, timeout_ms: Int) -> Dynamic

@external(erlang, "claude_agent_sdk_ffi", "close_port")
fn ffi_close_port_raw(port: Dynamic) -> Nil

pub fn ffi_close_port(port: Port) -> Nil {
  ffi_close_port_raw(port_to_dynamic(port))
}

/// Blocking receive - returns Result to propagate decode errors
/// Used by stream.gleam for main iteration
pub fn receive_blocking(port: Port) -> Result(PortMessage, String) {
  ffi_receive_blocking_raw(port_to_dynamic(port)) |> decode_port_message
}

/// Timed receive - INTERNAL ONLY for version detection + drains
/// Returns Result to propagate decode errors
pub fn receive_timeout(port: Port, timeout_ms: Int) -> Result(PortMessage, String) {
  ffi_receive_timeout_raw(port_to_dynamic(port), timeout_ms) |> decode_port_message
}

/// Canonical decoder for port messages returned by Erlang FFI.
///
/// **Design decision**: Erlang FFI returns STRING tags (not atoms) to simplify Gleam decoding.
/// Atom decoding in Gleam requires special handling that varies across stdlib versions.
/// Using strings ("data", "exit_status", "eof", "timeout") is more robust.
///
/// **FFI contract (CANONICAL - 2-tuples ONLY)**:
/// Erlang FFI ALWAYS returns 2-tuples with string first element:
/// - `{"data", Bytes}` for port data
/// - `{"exit_status", Code}` for exit status
/// - `{"eof", nil}` for end of file (2-tuple with nil)
/// - `{"timeout", nil}` for timeout (2-tuple with nil)
///
/// **NO 1-tuples**. This simplifies the Gleam decoder to use only `tuple2`.
///
/// **Return type**: `Result(PortMessage, String)` - errors are explicit, not variants.
fn decode_port_message(raw: Dynamic) -> Result(PortMessage, String) {
  // ALL messages are 2-tuples - no 1-tuple handling needed
  case dynamic.tuple2(dynamic.string, dynamic.any)(raw) {
    Ok(#("data", payload)) ->
      case dynamic.bit_array(payload) {
        Ok(bytes) -> Ok(Data(bytes))
        Error(_) -> Error("Invalid data payload: expected BitArray")
      }
    Ok(#("exit_status", payload)) ->
      case dynamic.int(payload) {
        Ok(code) -> Ok(ExitStatus(code))
        Error(_) -> Error("Invalid exit_status: expected Int")
      }
    Ok(#("eof", _)) -> Ok(Eof)
    Ok(#("timeout", _)) -> Ok(Timeout)
    _ -> Error("Unknown FFI message format")
  }
}

/// PortMessage type (INTERNAL ONLY - never exposed beyond port_ffi.gleam)
/// This type contains NO error variants. Decode errors return Result.Error.
type PortMessage {
  Data(BitArray)       // Raw bytes from port
  ExitStatus(Int)      // Process exit code
  Eof                  // End of file marker
  Timeout              // Receive timed out (only from timeout variant)
}
```

**Error Mapping (FFI → Runner → Stream) - AUTHORITATIVE**:

| FFI Layer | Runner Layer | Stream Layer |
|-----------|--------------|--------------|
| Raw Dynamic from Erlang | — | — |
| Decode success → `PortMessage` | Maps to `ReadResult` | Used by stream state machine |
| Decode failure → `Result.Error` | `ReadResult.ReadError(String)` | Fatal decode = terminal error |

**Canonical rule**: FFI decode failures are converted to `StreamError.FfiDecodeError(String)` at the port_ffi-to-stream boundary in v1. `PortMessage` only represents successful decodes. This keeps error handling centralized.

```gleam
// In port_ffi.gleam - internal only
fn decode_port_message(raw: Dynamic) -> Result(PortMessage, String) {
  // FFI returns consistent format - decode or fail
  decode_ffi_message(raw)
}

// In runner.gleam - public boundary
pub fn erlang_read(handle: Handle) -> ReadResult {
  case handle {
    ErlangHandle(port) ->
      case port_ffi.receive_blocking(port) {
        Ok(Data(bytes)) -> ReadResult.Data(bytes)
        Ok(ExitStatus(code)) -> ReadResult.ExitStatus(code)
        Ok(Eof) -> ReadResult.Eof
        Ok(Timeout) -> ReadResult.Eof  // Timeout treated as Eof at runner level
        Error(msg) -> ReadResult.ReadError(msg)  // Decode failure mapped here
      }
    // ...
  }
}
```

**FFI returns consistent 2-tuple format (simplified)**:

To simplify decoding, FFI always returns 2-tuples. Eof and Timeout use a dummy second element:

```erlang
%% CANONICAL: All returns are 2-tuples for uniform decoding
receive_port_msg_blocking(Port) ->
    receive
        {Port, {data, Bytes}} -> {"data", Bytes};
        {Port, {exit_status, Code}} -> {"exit_status", Code};
        {Port, eof} -> {"eof", nil}           %% 2-tuple with nil
    end.

receive_port_msg_timeout(Port, TimeoutMs) ->
    receive
        {Port, {data, Bytes}} -> {"data", Bytes};
        {Port, {exit_status, Code}} -> {"exit_status", Code};
        {Port, eof} -> {"eof", nil}
    after TimeoutMs -> {"timeout", nil}       %% 2-tuple with nil
    end.
```

**Why 2-tuples uniformly**:
- Gleam's `dynamic.tuple2` works for all cases
- No need for special 1-tuple handling (`dynamic.tuple1` or `dynamic.element`)
- Simpler, more testable decoder

**Gleam decoder (final)**:
```gleam
fn decode_ffi_message(raw: Dynamic) -> Result(PortMessage, String) {
  case dynamic.tuple2(dynamic.string, dynamic.any)(raw) {
    Ok(#("data", payload)) ->
      case dynamic.bit_array(payload) {
        Ok(bytes) -> Ok(Data(bytes))
        Error(_) -> Error("Invalid data payload")
      }
    Ok(#("exit_status", payload)) ->
      case dynamic.int(payload) {
        Ok(code) -> Ok(ExitStatus(code))
        Error(_) -> Error("Invalid exit_status payload")
      }
    Ok(#("eof", _)) -> Ok(Eof)
    Ok(#("timeout", _)) -> Ok(Timeout)
    _ -> Error("Unknown FFI message format")
  }
}
```

**Phase 0 must verify decoder works**:
```gleam
// In phase0_api_confirmation_test.gleam
pub fn decode_port_message_test() {
  // All FFI messages are 2-tuples
  let test_data = dynamic.from(#("data", <<1, 2, 3>>))
  let assert Ok(Data(<<1, 2, 3>>)) = decode_ffi_message(test_data)

  let test_exit = dynamic.from(#("exit_status", 42))
  let assert Ok(ExitStatus(42)) = decode_ffi_message(test_exit)

  let test_eof = dynamic.from(#("eof", Nil))
  let assert Ok(Eof) = decode_ffi_message(test_eof)

  let test_timeout = dynamic.from(#("timeout", Nil))
  let assert Ok(Timeout) = decode_ffi_message(test_timeout)
}
```

**Call site mapping**:

See [All timeout values](#all-timeout-values-single-source-of-truth) for the canonical timeout table. Summary: version detection 5s, post-Result drain 100ms, post-exit drain 50ms, close mailbox drain 50ms.

**Critical invariant**: Phase 0 test must validate port_ffi operations. If Phase 0 passes with a different API shape, later phases will fail.

**Port options** (all handled by FFI — see [Authoritative FFI Interface](#authoritative-ffi-interface-single-source-of-truth)):

| Option | Purpose |
|--------|---------|
| `spawn_executable` | Spawn by executable path |
| `stream` | Raw byte stream mode |
| `binary` | Binary data (not list) |
| `exit_status` | Receive exit code |
| `use_stdio` | Connect stdin/stdout |

All term construction is done in Erlang FFI. The authoritative implementation is defined above in the "Authoritative FFI Interface" section. Duplicate code removed to prevent drift.

```erlang
%% See Authoritative FFI Interface for complete module
-module(claude_agent_sdk_ffi).
-export([open_port/3, receive_port_msg/2]).

%% See Authoritative FFI Interface for canonical implementation.
%% This is a summary only - DO NOT copy this directly.
open_port(Path, Args, Cwd) ->
    erlang:open_port({spawn_executable, Path}, [...]).

%% AUTHORITATIVE FFI uses uniform 2-tuples with string tags:
%%   {"data", Bytes}, {"exit_status", Code}, {"eof", nil}, {"timeout", nil}
%% See [Authoritative FFI Interface](#authoritative-ffi-interface-single-source-of-truth)
```

**Phase 0 validates term shapes**: If `open_port` fails with `badarg`, term construction is wrong.

~~**Alternative (NOT RECOMMENDED)**: Gleam-side Dynamic construction~~

```gleam
// AVOID: Error-prone Dynamic term construction
pub fn open_executable_port(path: String, args: List(String), cwd: String) -> Port {
  let options = [
    dynamic.from(#("args", args)),  // May not produce correct {args, List} tuple
    dynamic.from(#("cd", cwd)),
    dynamic.from(atom.create_from_string("stream")),  // atom API may differ
    dynamic.from(atom.create_from_string("use_stdio")),
  ]
  erlang_open_port(spawn_executable(path), options)
}
```

**Erlang helper** (if needed for tuple construction):
```erlang
%% src/claude_agent_sdk_port_ffi.erl
-module(claude_agent_sdk_port_ffi).
-export([spawn_executable/1]).

spawn_executable(Path) -> {spawn_executable, Path}.
```

**Platform-specific validation** (Phase 0 tests):

| Platform | Test Command | Expected Behavior | Notes |
|----------|--------------|-------------------|-------|
| Linux/macOS | `/bin/echo "test"` | Receive `<<"test\n">>` | Standard |
| Windows | `cmd.exe /c echo test` | Receive `<<"test\r\n">>` | CRLF line endings |

**Phase 0 Runtime Validation Hierarchy (for restricted environments)**:

Try commands in order until one works. If all fail, skip runtime validation and use compile-only check.

| Priority | Unix Command | Windows Command | Notes |
|----------|-------------|-----------------|-------|
| 1 | `/bin/echo test` | `cmd.exe /c echo test` | Standard (most distros) |
| 2 | `/bin/busybox echo test` | — | Alpine Linux |
| 3 | `/usr/bin/env echo test` | — | Works in more containers |
| 4 | `/bin/sh -c "echo test"` | — | Ultimate fallback (POSIX) |
| 5 | — | — | Skip runtime; compile-only |

**Offline/restricted execution strategy**:

For environments without network access (air-gapped CI, offline dev):

| Requirement | Solution |
|-------------|----------|
| Dependencies | Commit `manifest.toml` (Gleam lockfile) to repo |
| Package cache | Run `gleam deps download` with network, commit `build/packages/` if allowed by policy |
| CI caching | Configure CI to cache `~/.cache/gleam` between runs |
| Verification | Compile-only check (`gleam build`) proves APIs exist at locked versions |

**CI configuration example** (GitHub Actions):
```yaml
- name: Cache Gleam dependencies
  uses: actions/cache@v4
  with:
    path: |
      ~/.cache/gleam
      build/packages
    key: gleam-deps-${{ hashFiles('manifest.toml') }}
```

```gleam
// test/port_validation_test.gleam - RUN FIRST
import claude_agent_sdk/internal/port_ffi

// OS type detection via FFI - returns Dynamic, decode to OsFamily
// See "FFI Type Representation" section for decode strategy
@external(erlang, "os", "type")
fn ffi_os_type() -> Dynamic

pub fn port_spawn_and_receive_test() {
  // Platform-appropriate echo command with fallbacks
  // Uses FFI for OS detection with Dynamic decode (see phase0_test_command)
  let commands = case decode_os_family(ffi_os_type()) {
    Ok(Win32) -> [#("cmd.exe", ["/c", "echo", "test"])]
    _ -> [
      #("/bin/echo", ["test"]),           // Primary
      #("/bin/busybox", ["echo", "test"]), // Alpine
      #("/usr/bin/env", ["echo", "test"]), // Container fallback
      #("/bin/sh", ["-c", "echo test"]),  // Ultimate fallback
    ]
  }

  // Try commands until one works
  case try_first_working_command(commands) {
    Ok(#(cmd, args)) -> {
      // 1. Spawn via FFI
      let port = port_ffi.ffi_open_port(cmd, args, ".")

      // 2. Verify data received (receive_timeout returns Result)
      let assert Ok(Data(bytes)) = port_ffi.receive_timeout(port, 5000)
      let assert True = bit_array.byte_size(bytes) > 0

      // 3. Verify exit_status received
      let assert Ok(ExitStatus(0)) = port_ffi.receive_timeout(port, 5000)

      // Test PASSES
    }
    Error(_) -> {
      // All commands failed - skip runtime validation
      // Log: "SKIP: No working echo command found. Using compile-only validation."
      // This is OK for minimal containers; Phase 0 compile check still gates.
    }
  }
}

/// Try commands until one exists in PATH
fn try_first_working_command(commands) {
  list.find(commands, fn(#(cmd, _)) {
    case find_executable(cmd) {
      Ok(_) -> True
      Error(_) -> False
    }
  })
}

pub fn port_exit_status_nonzero_test() {
  // Verify non-zero exit codes are captured
  let #(cmd, args) = case ffi_os_type() {
    #(Win32, _) -> #("cmd.exe", ["/c", "exit", "42"])
    _ -> #("/bin/sh", ["-c", "exit 42"])
  }
  let port = port_ffi.ffi_open_port(cmd, args, ".")
  let assert Ok(ExitStatus(42)) = port_ffi.receive_timeout(port, 5000)
}
```

**Phase 0 Success Criteria (Precise)**:

**Target OS/OTP validation matrix** (must validate before proceeding):

| OS | OTP Version | Must Validate | Notes |
|----|-------------|---------------|-------|
| Linux (Ubuntu 22.04+) | OTP 27.0+ | ✅ Primary target | CI environment |
| macOS (13+) | OTP 27.0+ | ✅ Primary target | Dev environment |
| Windows 10+ | OTP 27.0+ | ⚠️ Best effort | Document limitations |

**Windows Support Guidance (Explicit)**:

Windows is "best effort" in v1. This section documents what works, what may fail, and how to handle differences.

| Category | Linux/macOS | Windows | Handling |
|----------|-------------|---------|----------|
| Line endings | LF (`\n`) | CRLF (`\r\n`) | Normalize in buffer before split |
| PATH lookup | `which claude` | `where claude` | Use `os:find_executable/1` (cross-platform) |
| Echo command | `/bin/echo` | `cmd /c echo` | Platform switch in tests |
| EOF message | Rare | May not arrive | Don't rely on EOF; use exit_status |
| Port behavior | Well-tested | Less common | May have edge cases |

**What's expected to work on Windows**:
- Basic port spawn with `spawn_executable`
- Receiving `{data, Bytes}` messages
- Receiving `{exit_status, Code}` on process exit
- Line-based NDJSON parsing (after CRLF normalization)

**What may differ or fail**:
- EOF message delivery (don't rely on it)
- Subprocess cleanup timing (Windows process lifecycle differs)
- Error messages/exit codes for missing executables

**CI/Testing on Windows**:

| Test Category | Run on Windows CI? | Notes |
|---------------|-------------------|-------|
| Unit tests (mocked runner) | ✅ Yes | Platform-independent |
| Phase 0 port validation | ⚠️ Optional | May skip if no Windows CI |
| Integration tests (real CLI) | ⚠️ Optional | Requires Claude CLI on Windows |

**Reporting Windows failures**:

If tests fail only on Windows:
1. Document the specific failure in the test file
2. Add `@target(erlang)` skip annotation if Gleam supports it
3. File GitHub issue with "windows" label
4. Continue with Linux/macOS as primary targets

**CRLF Normalization (required for Windows)**:

```gleam
/// Normalize CRLF to LF before line splitting
fn normalize_line_endings(bytes: BitArray) -> BitArray {
  // Replace \r\n with \n throughout
  // This runs BEFORE splitting on \n
  bytes
  |> bit_array.to_string
  |> result.map(string.replace(_, "\r\n", "\n"))
  |> result.map(bit_array.from_string)
  |> result.unwrap(bytes)  // If not valid UTF-8, use original
}
```

This ensures the same newline-split logic works cross-platform.

**Test cases per platform**:

| Test | Required Messages | Pass Condition |
|------|-------------------|----------------|
| Echo test | `{Port, {data, Bytes}}` then `{Port, {exit_status, 0}}` | Data contains "test", exit=0 |
| Non-zero exit | `{Port, {exit_status, 42}}` | Exit code = 42 |
| Empty stdout | `{Port, {exit_status, 0}}` only (no data) | Exit received, no data = valid |

**Message shape requirements per OS**:
| OS | Data Message | Line Ending | EOF Behavior |
|----|--------------|-------------|--------------|
| Linux | `{Port, {data, <<"test\n">>}}` | LF (`\n`) | Optional |
| macOS | `{Port, {data, <<"test\n">>}}` | LF (`\n`) | Optional |
| Windows | `{Port, {data, <<"test\r\n">>}}` | CRLF (`\r\n`) | Optional |

**Contract: exit_status arrival with buffer state (crisp specification)**:

| Buffer State at exit_status | Exit Code | Behavior |
|----------------------------|-----------|----------|
| Empty (no data ever) | 0 | Warning(EmptyResponse), EndOfStream |
| Empty (no data ever) | ≠0 | ProcessError with stdout_was_empty=True |
| Complete lines only | any | Process lines, then handle exit |
| **Incomplete line (no trailing \n)** | 0 | Warning(IncompleteLastLine), EndOfStream |
| **Incomplete line (no trailing \n)** | ≠0 | ProcessError (incomplete line discarded) |

**Explicit rule for incomplete buffer at exit_status**:
```gleam
// On ExitStatus(code):
case #(buffer_ends_with_newline(buffer), code) {
  #(True, _) ->
    // Buffer has complete lines - process them first, then handle exit
    process_remaining_lines(buffer)
  #(False, 0) if buffer_is_empty(buffer) ->
    // No data at all - empty response
    yield Warning(EmptyResponse), EndOfStream
  #(False, 0) ->
    // Incomplete last line, clean exit - warn and discard
    yield Warning(IncompleteLastLine(buffer)), EndOfStream
  #(False, code) ->
    // Incomplete last line, error exit - include in diagnostic
    yield ProcessError(code, ErrorDiagnostic(last_non_json_line: Some(buffer), ...))
}
```

**Post-exit late-drain behavior (unified rule)**:

After observing `exit_status`, perform a bounded "late drain" loop to handle scheduler timing where `{data, Bytes}` arrives after `exit_status`.

**For timeout values and bounds**, see [All timeout values](#all-timeout-values-single-source-of-truth). Summary: 50ms timeout, 10 iterations max, 1KB data cap.

```gleam
// Unified late-drain after exit_status
fn drain_late_data(buffer: BitArray) -> BitArray {
  loop(buffer, iterations: 0, bytes_collected: 0) {
    case iterations >= 10 || bytes_collected >= 1024 {
      True -> buffer  // Bounds exceeded, stop draining
      False ->
        case receive_port_msg_timeout(50) {
          Data(bytes) ->
            let new_buffer = <<buffer:bits, bytes:bits>>
            loop(new_buffer, iterations + 1, bytes_collected + byte_size(bytes))
          _ -> buffer  // Timeout, EOF, or another exit_status: done
        }
    }
  }
}
```

**Relationship to post-Result drain**: See [Port Lifecycle Contract](#port-lifecycle-contract-single-source-of-truth) for the canonical drain sequencing. Summary:
- **Post-exit late-drain**: Before exit processing, appends to buffer (50ms × 10 iters, 1KB cap)
- **Post-Result drain**: After Result yielded, yields warnings (100ms × 10 msgs)

**Unit test requirement**: `stream_test.gleam` must include:
- Mock: `Data("partial")` then `ExitStatus(0)` → `Warning(IncompleteLastLine)`
- Mock: `Data("complete\n")` then `ExitStatus(0)` → message processed normally

**EOF is NOT required for pass**: The SDK treats `eof` as optional because:
- Some OTP versions/platforms don't send `eof` with `exit_status` option
- The SDK uses `exit_status` as the authoritative termination signal
- Missing `eof` is harmless (stream terminates on `exit_status`)

**Phase 0 port option validation**:

The SDK unconditionally uses this fixed option set (see [Authoritative FFI Interface](#authoritative-ffi-interface-single-source-of-truth)):

```erlang
[{args, Args}, {cd, Cwd}, stream, binary, exit_status, use_stdio]
```

Phase 0 validates this option set works on the target platform. No option-set selection mechanism is implemented — `use_stdio` is always included.

| Option | Purpose | Required |
|--------|---------|----------|
| `stream` | Raw byte stream mode | ✅ Yes |
| `binary` | Binary data (not list) | ✅ Yes |
| `exit_status` | Receive exit code | ✅ Yes |
| `use_stdio` | Connect stdin/stdout | ✅ Yes (OTP 27+) |

**Why `stderr_to_stdout` is NOT included**: The SDK's v1 I/O contract requires pure NDJSON on stdout. Merging stderr into stdout via `stderr_to_stdout` would violate this contract:
- Stderr content would appear as non-JSON lines interleaved with NDJSON
- The NDJSON purity preflight test would fail
- The consecutive decode error threshold would trigger false positives

**Phase 0 pass criteria**: If the fixed option set fails to produce stdout data from `/bin/echo test`, Phase 0 fails. There is no fallback option set — the FFI implementation is the only implementation.

**Why `use_stdio` is required**: In OTP 27+ configurations with `spawn_executable`, omitting `use_stdio` may cause stdout to NOT be piped to the port. Including `use_stdio` explicitly guarantees:
- stdout is connected via pipe → delivered as `{Port, {data, Bytes}}` messages
- stdin is connected (SDK doesn't write to it, but it's harmless)

**Message ordering (best effort, not guaranteed)**:

The SDK design assumes `{data, Bytes}` messages arrive before `{exit_status, Code}`, but this is **best effort, not a hard guarantee**. Scheduler timing and buffering can invert observations.

**State machine tolerates out-of-order delivery**:
- If `exit_status` arrives while buffer contains incomplete data: process buffer first, then handle exit
- If `exit_status` arrives before expected data: treat as early termination
- The stream state machine does NOT assume strict ordering

This ensures:
- stdout is delivered as `{Port, {data, Bytes}}` messages in both cases
- We have one proven port configuration, not two divergent ones
- If version detection works, query streaming will work

**Version check in query() flow**:

The version check uses `version_meets_minimum` as the **single source of truth** for version comparison. This ensures consistent behavior for future minimum version bumps (e.g., `>=1.2.0`).

```gleam
/// Minimum CLI version required by SDK
const minimum_cli_version = CliVersion(1, 0, 0, "1.0.0")

pub fn query(prompt: String, options: QueryOptions) -> Result(QueryStream, QueryError) {
  // 1. Find CLI in PATH
  let cli_path = case find_cli_in_path() {
    Ok(path) -> path
    Error(_) -> return Error(CliNotFoundError("claude not found in PATH"))
  }

  // 2. Check version (unless skipped for testing)
  case options.skip_version_check {
    True -> Nil
    False -> {
      case detect_cli_version(cli_path) {
        Ok(version) -> case version {
          CliVersion(_, _, _, _) as v ->
            // Use version_meets_minimum for ALL version comparisons
            case version_meets_minimum(v, minimum_cli_version) {
              True -> Nil
              False -> return Error(UnsupportedCliVersionError(
                detected_version: v.raw,
                minimum_required: minimum_cli_version.raw,
                suggestion: "Run: npm update -g @anthropic-ai/claude-code"
              ))
            }
          UnknownVersion(raw) ->
            // POLICY: Fail-fast by default, opt-in permissive mode
            case options.permissive_version_check {
              True -> queue_warning(UnparseableCliVersion, raw)
              False -> return Error(UnknownVersionError(raw, "..."))
            }
        }
        Error(e) ->
          // Version detection failed (timeout, port error, etc.)
          case options.permissive_version_check {
            True -> queue_warning(VersionDetectionFailed, e)  // Permissive: proceed
            False -> return Error(VersionDetectionError(reason: e))  // Fail-fast
          }
      }
    }
  }

  // 3. Build args and spawn via port_ffi directly (v1 production path)
  // Note: In test_mode, uses test_runner instead - see with_test_mode()
  let args = build_cli_args(prompt, options)
  let port = case options.test_mode {
    False -> port_ffi.ffi_open_port(cli_path, args, cwd)  // v1 production
    True -> test_runner_spawn(options.test_runner, cli_path, args, cwd)  // test mock
  }
  // ... rest of query implementation (QueryStream wraps port)
}

/// Single source of truth for version comparison.
/// Returns True if `version >= minimum`.
/// NOTE: Only accepts CliVersion, not UnknownVersion (handled separately in query()).
pub fn version_meets_minimum(version: CliVersion, minimum: CliVersion) -> Bool {
  let CliVersion(maj, min, patch, _) = version
  let CliVersion(min_maj, min_min, min_patch, _) = minimum
  maj > min_maj ||
  (maj == min_maj && min > min_min) ||
  (maj == min_maj && min == min_min && patch >= min_patch)
}
```

**Version policy**: See [Canonical Version Policy](#canonical-version-policy-single-source-of-truth) for the authoritative version handling rules and fail-fast rationale.

**QueryOptions field**:
```gleam
pub type QueryOptions {
  QueryOptions(
    // ... existing options ...
    /// When True, proceed with warning if version cannot be determined.
    /// Default: False (fail fast on unknown version).
    /// Use for: known-compatible enterprise builds, testing, or wrappers.
    permissive_version_check: Bool,
  )
}
```

**Version detection test strategy**:

Version detection involves real OS processes, so testing is split:

| Test Type | What's Tested | How |
|-----------|---------------|-----|
| **Unit tests** | `parse_version_string()` parsing logic | Pure function, no OS |
| **Unit tests** | Version comparison logic | Pure function, no OS |
| **Integration tests** (opt-in) | Full `detect_cli_version()` with real CLI | Real `claude --version` |
| **Integration tests** (opt-in) | Port preflight validation | Real OS command |

**Unit-testable components** (no process spawning):
```gleam
// These are pure functions, fully unit-testable
pub fn parse_version_string(output: String) -> Result(CliVersion, Nil)
pub fn version_meets_minimum(version: CliVersion, min: CliVersion) -> Bool
pub fn format_version_error(detected: CliVersion, required: CliVersion) -> String
```

**Integration-only components** (require real processes):
```gleam
// These spawn real OS processes, tested in integration tests only
fn detect_cli_version(cli_path: String) -> Result(CliVersion, QueryError)
fn drain_port_until_exit(port: Port, timeout_ms: Int) -> Result(String, VersionCheckError)
```

**Timeout testing**: The 5-second default is via `default_version_timeout_ms()`. For integration tests that need to verify timeout behavior, use a small test script that sleeps, with a short timeout override (e.g., 100ms).

**Version detection acceptance criteria**:

| Criterion | Test Type | Verification |
|-----------|-----------|--------------|
| `parse_version_string("claude v1.2.3\n")` returns `Ok(CliVersion(1,2,3,_))` | Unit test | Pure function |
| `parse_version_string("garbage")` returns `Error` | Unit test | Pure function |
| `version_meets_minimum(v1.0.0, v1.0.0)` returns `True` | Unit test | Pure function |
| `version_meets_minimum(v0.9.0, v1.0.0)` returns `False` | Unit test | Pure function |
| Real `claude --version` returns valid version | Integration test | Opt-in, real CLI |
| Port preflight receives data from `echo` | Integration test | Opt-in, validates port config |
| **Outcome-based**: `query()` produces `System` message | Integration test | Verifies full stack works, not just flags exist |

**Why separate from Runner abstraction**:
- Version detection is a **startup check**, not a streaming operation
- It doesn't need line buffering, stream state, or error classification
- Keeping it separate avoids polluting the `Runner` interface with version-check logic
- Tests that use `test_runner()` can skip version checks via `QueryOptions.skip_version_check: Bool` (default: False)

**Flag requirements** (from SPEC: CLI Invocation, verified against `claude --help`):
| Flag | Purpose | Spec Reference |
|------|---------|----------------|
| `--print` / `-p` | Non-interactive mode | SPEC: CLI Invocation |
| `--output-format stream-json` | NDJSON streaming output | SPEC: CLI Invocation |
| `--verbose` | **Required** for `stream-json` format | SPEC: CLI Invocation |
| `--` | Separator between options and prompt | SPEC: CLI Invocation |

**Note on `--verbose`**: The SDK specification states `--verbose` is "Required for `stream-json` format". The SDK includes this flag unconditionally.

**`--verbose` rejection handling (explicit policy)**:

If the CLI rejects `--verbose` (exit code 2, "invalid argument"), the SDK does NOT implement a fallback retry in v1. Instead:

1. **Behavior**: The query fails with `ProcessError(2, diagnostic)` where diagnostic includes:
   - `exit_code_hint: "Invalid arguments (check SDK version compatibility)"`
   - `troubleshooting: "The CLI rejected --verbose. Your CLI version may be incompatible with this SDK version. Update CLI: npm update -g @anthropic-ai/claude-code"`

2. **Rationale for no fallback**:
   - `--verbose` is documented as required for `stream-json` in SDK spec
   - Silently dropping it could produce invalid/incomplete output
   - Better to fail explicitly than produce unexpected behavior
   - Users can downgrade SDK or upgrade CLI to resolve

3. **v2 consideration**: If CLI versions diverge significantly, add `QueryOptions.cli_compatibility_mode` to adjust flag sets per CLI version.

**Verification source of truth**: `test/fixtures/README.md` documents:
- Exact CLI version used for verification (e.g., "Claude Code CLI v1.0.3")
- Whether `--verbose` requirement was confirmed via `claude --help` output
- Date of verification

**Important**: The SDK does NOT make assumptions about what `--verbose` enables beyond "it is required for stream-json to work". The exact effect on message schema/content may vary by CLI version. Decoders are designed to be resilient:
- Optional fields use `Option` types
- Unknown fields are silently ignored
- Unknown message types yield `UnexpectedMessageError` with raw JSON preserved

**Stdout I/O Contract (NDJSON Purity)**:

The SDK makes the following explicit assertion about stdout:

> **Stdout may include non-JSON lines.** The SDK treats them as non-terminal decode errors up to the `TooManyDecodeErrors` threshold (default: 5 consecutive).

**Expected sources of non-JSON stdout lines** (v1 defensive tolerance):

| Source | Example | Expected? | Handling |
|--------|---------|-----------|----------|
| `--verbose` debug output | Startup info lines | Possible | `JsonDecodeError`, continue |
| CLI warning messages | Deprecation notices | Possible | `JsonDecodeError`, continue |
| ANSI escape sequences | Color codes | Unlikely (stream-json disables) | `JsonDecodeError`, continue |
| Binary tool output | Raw bytes in tool result | Not expected | `JsonDecodeError`, continue |
| Stderr leakage | Error messages | NOT expected (separate stream) | Would trigger threshold |

**Key distinction**: Non-JSON lines are NOT from stderr (SDK does not merge stderr via `stderr_to_stdout`). They would come from stdout itself, typically from CLI versions that emit metadata alongside JSON.

**How non-JSON lines are recorded**:
- Each non-JSON line yields `JsonDecodeError(line, error)` containing the raw line content
- The `line` field preserves the non-JSON content for debugging
- `ErrorDiagnostic.last_non_json_line` is populated with the most recent non-JSON line when `ProcessError` occurs
- `TooManyDecodeErrors` includes `last_error` with the parse error from the final bad line

**Rationale**: While `--output-format stream-json` *should* produce pure NDJSON, we cannot guarantee this across all CLI versions or edge cases. The SDK is defensive:

1. **Primary expectation**: Stdout is pure NDJSON (one JSON object per line)
2. **Fallback handling**: Non-JSON lines yield `JsonDecodeError` (non-terminal)
3. **Safety threshold**: 5+ consecutive decode errors → `TooManyDecodeErrors` (terminal)
4. **Recovery**: Any successful JSON parse resets the error counter to 0

**Integration preflight for NDJSON purity** (opt-in):

```gleam
/// Integration test: Verify CLI produces pure NDJSON for trivial prompt.
/// This catches `--verbose` behavior changes early.
pub fn ndjson_purity_preflight_test() {
  case get_env("CLAUDE_INTEGRATION_TEST") {
    Ok("1") -> {
      // Run: claude --print --output-format stream-json --verbose -- "Say hello"
      // Assert: Every line in stdout parses as valid JSON
      // If any non-JSON line: FAIL with actionable message:
      //   "CLI produced non-JSON output. Check --verbose behavior in CLI version X.Y.Z"
    }
    _ -> should.be_true(True)
  }
}
```

**Preflight vs runtime tolerance (intentional asymmetry)**:

| Context | Non-JSON tolerance | Rationale |
|---------|-------------------|-----------|
| Preflight test | **ZERO** (fail on first non-JSON) | Catch CLI drift immediately |
| Runtime streaming | **5 consecutive** (then terminal error) | Resilient to transient issues |

The preflight is **intentionally stricter** than runtime because:
1. A clean CLI should produce 100% JSON with `--output-format stream-json`
2. Any non-JSON in the preflight indicates a CLI behavior change or misconfiguration
3. Failing fast in preflight gives actionable feedback before users hit issues
4. Runtime tolerance is for edge cases (corrupted output, race conditions), not expected behavior

**Why strict preflight is still valuable**: Even if it causes skips, it signals CLI drift that should be investigated. This is preferable to silently running with noisy output.

**Optional: Allow non-JSON in integration tests** (for noisy-but-functional CLIs):

If `--verbose` legitimately outputs non-JSON banner lines in some CLI versions, use:

```bash
CLAUDE_INTEGRATION_TEST=1 CLAUDE_INTEGRATION_ALLOW_NONJSON=1 gleam test
```

| Env Var | Preflight Behavior |
|---------|-------------------|
| Not set | Strict: fail on first non-JSON, skip tests |
| `CLAUDE_INTEGRATION_ALLOW_NONJSON=1` | Tolerant: use runtime 5-error threshold |

**Implementation**:
```gleam
fn preflight_ndjson_check() -> Bool {
  let allow_nonjson = case get_env("CLAUDE_INTEGRATION_ALLOW_NONJSON") {
    Ok("1") -> True
    _ -> False
  }
  case allow_nonjson {
    True -> True  // Skip preflight, use runtime tolerance
    False -> run_strict_ndjson_check()  // Fail on first non-JSON
  }
}
```

**Default is strict** — the drift signal is preserved unless explicitly disabled.

**Fixture strategy for `--verbose` resilience**:
- All fixtures captured WITH `--verbose` (matching production usage)
- Decoders treat most fields as optional (resilient to field presence changes)
- If CLI ever permits `stream-json` without `--verbose`, capture comparison fixtures
- Integration test validates `--verbose` is accepted by current CLI

**Flag validation and versioning**:

The CLI argument mapping table below was verified against:
- SDK specification (SPEC: CLI Invocation, SPEC: Query Options)
- Claude Code CLI v1.0.x `--help` output

**Verification artifact**: `test/fixtures/README.md` documents the exact CLI version used for verification. Additionally:
```
test/fixtures/cli_help.txt  # Critical flags from claude --help
```

**Fixture handling policy (canonical)**:

| Fixture Type | Created When | Source | Storage | Test Behavior |
|--------------|--------------|--------|---------|---------------|
| **Synthetic/minimal fixtures** | Phase 1 (initial) | Spec-derived, hand-crafted | Committed | Unit tests (always work) |
| **Real CLI fixtures** | Follow-up capture | Real `claude` output | Committed | Unit tests (validates spec accuracy) |
| CLI help excerpt | Developer capture | Real `claude --help` | Committed | Reference/integration tests |

**Synthetic vs Real Fixture Strategy (explicit)**:

| Phase | Fixture Source | Purpose | Blocks On |
|-------|---------------|---------|-----------|
| **Phase 1 (Initial)** | Synthetic/spec-derived | Prove decoders work with minimal valid JSON | Nothing (works in any environment) |
| **Follow-up Capture** | Real `claude` CLI output | Validate spec matches reality, catch drift | `claude` CLI available |

**Why synthetic fixtures first**:
- Unit tests must run in CI/containers without `claude` installed
- Spec-derived fixtures prove decoder correctness against documented schema
- Blocks initial implementation only on spec reading, not CLI access
- Real fixtures are captured AFTER initial implementation works

**Synthetic fixture requirements**:
- Contains ONLY spec-guaranteed required fields (see "Canonical decoder policy table")
- Valid JSON syntax
- Documents "synthetic" source in `test/fixtures/README.md`
- Example: `system_message_synthetic.json` with just `type` and `session_id`

**Real fixture capture** (follow-up task):
1. Run `claude --print --output-format stream-json --verbose -- "hello"` locally
2. Save output lines to `test/fixtures/` with descriptive names
3. Update `test/fixtures/README.md` with:
   - CLI version used
   - Capture date
   - Capture command
4. Compare real vs synthetic fixtures to detect spec drift

**Tests NEVER write fixtures**: All fixtures are captured once by developers and committed to the repository. Tests only READ fixtures. This ensures:
- Tests work in read-only filesystems
- No network required for unit tests
- Reproducible test results

**Fixture capture process** (developer task, not automated):
1. Run `claude --help` / `claude --version` locally
2. Capture relevant output to `test/fixtures/`
3. Document CLI version in `test/fixtures/README.md`
4. Commit to repository

**Fixture Maintenance Policy (CI/Schema Drift)**:

When CI fails due to fixture/schema drift:

| Failure Type | Cause | Resolution |
|--------------|-------|------------|
| New field in CLI output | CLI added optional field | Add field as `Option(T)` in decoder; update fixture |
| Missing required field | Spec incorrectly assumed required | Downgrade to optional; re-evaluate spec citation |
| Changed field type | CLI behavior change | Update decoder; document breaking change |
| Unknown message type | New message type added | `UnknownBlock`/`UnexpectedMessageError` handles it |

**Maintenance rules**:
1. **Never hard-require observed fields** — only fields cited in SPEC: Message Schema as "required" should cause decode failure
2. **Update fixture README** with new CLI version and capture date when refreshing fixtures
3. **Treat drift as expected** — CLI will evolve; decoders must be resilient
4. **Spec citations must have artifacts** — if plan cites "SPEC: Message Schema requires session_id", the spec doc must exist and be verifiable
5. **Schema drift detection** — compare new fixtures against old to identify added/removed/changed fields; document changes in CHANGELOG

**Example fixture README.md**:
```markdown
# Test Fixtures

## Capture Information
- CLI Version: Claude Code CLI v1.0.5
- Capture Date: 2024-03-15
- Capture Command: `claude --print --output-format stream-json --verbose -- "hello"`

## Fixtures
- system_message.json - Real output, CLI v1.0.5
- system_message_synthetic.json - Spec-derived minimal (type + session_id only)
- assistant_message.json - Real output, CLI v1.0.5
...
```

**Integration test for flag validity**:
```gleam
/// Verify critical flags exist in current CLI (integration test)
pub fn critical_flags_exist_test() {
  // Run: claude --help
  // Assert output contains: "--print", "--output-format", "--verbose"
  // This catches flag renames/removals early
}
```

If the CLI rejects a flag, users receive `ProcessError` with:
- Exit code from the CLI
- Diagnostic context (see Error Diagnostics section below)

## Prerequisites

**Runtime dependencies** (in `[dependencies]`):
- [ ] Add gleam_json (v2.0.0) for JSON encoding/decoding
- [ ] Add gleam_erlang (v1.3.0) for ports and process management

**Dev-only dependencies** (in `[dev-dependencies]` — not shipped to SDK users):
- [ ] Add gleeunit (v1.2.0) for unit testing
- [ ] Create `test/support/env_helpers.gleam` for environment variable access (FFI, no external dep)

**See [Dependency Version Policy](#dependency-version-policy-v1) for exact pin requirements.**

**External requirements**:
- [ ] Claude CLI must be installed (`claude --version` succeeds)
- [ ] Claude CLI must be authenticated (CLI session available)

## High-Level Approach

The SDK wraps the Claude Code CLI as a subprocess, communicating via stdout using NDJSON. The implementation uses Gleam's discriminated unions (custom types) to model the message protocol with full type safety.

**Key architectural decisions:**
1. **v1 uses port_ffi directly; Runner is test-only abstraction**: v1 production code calls `port_ffi.ffi_open_port()`, `receive_blocking()`, etc. directly for simplicity. The `Runner` type (function record with `spawn`, `read_next`, `close`) exists for: (a) `test_runner()` enabling unit tests without OS processes, and (b) future v2 multi-platform support where `erlang_runner()` and `js_runner()` would be alternatives.
2. **Opaque QueryStream type**: Returns a dedicated `QueryStream` opaque type (not a plain `Iterator`) that encapsulates both iteration and cleanup. This ensures `close()` has access to the underlying port handle.
3. **Functional API**: Single `query(prompt, options)` function. No client objects needed since each query spawns a fresh CLI process. Sessions are tracked via `session_id` from the system message.
4. **Specific error types**: Separate error variants for CLI not found, process exit, JSON parse errors, etc. With explicit terminal vs non-terminal error classification.

## Technical Design

### Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                      User Application                            │
├─────────────────────────────────────────────────────────────────┤
│  ┌───────────────┐  ┌───────────────┐  ┌─────────────────────┐  │
│  │    query()    │  │ QueryOptions  │  │   Message Types     │  │
│  │   function    │  │   builders    │  │ (discriminated      │  │
│  │               │  │               │  │  unions)            │  │
│  └───────┬───────┘  └───────┬───────┘  └──────────┬──────────┘  │
│          │                  │                     │             │
│          └──────────────────┼─────────────────────┘             │
│                             ▼                                   │
│                    ┌─────────────────┐                          │
│                    │ Runner (opaque  │ ◄── Function record type │
│                    │  fn record)     │                          │
│                    └────────┬────────┘                          │
│                             │                                   │
│       ┌─────────────────────┼─────────────────────┐             │
│       ▼                     ▼                     ▼             │
│  ┌──────────┐        ┌──────────┐          ┌──────────┐         │
│  │ erlang_  │ (v1)   │  test_   │ (test)   │  js_     │ (v2)    │
│  │ runner() │        │ runner() │          │ runner() │         │
│  └──────────┘        └──────────┘          └──────────┘         │
└─────────────────────────────────────────────────────────────────┘
```

**Runner Abstraction (Gleam Implementation)**:

The key design challenge is that Gleam's opaque types cannot have their constructors called from outside the defining module. To enable unit testing with mock runners, we use an **internal sum type** for handles that is never exposed publicly.

```gleam
// In src/claude_agent_sdk/runner.gleam

/// Runner is an opaque type containing platform-specific process management functions.
/// This is Gleam's idiomatic approach to polymorphism (no traits in Gleam).
pub opaque type Runner {
  Runner(
    /// Canonical spawn signature: executable path, args, working directory
    spawn: fn(String, List(String), String) -> Result(Handle, SpawnError),
    read_next: fn(Handle) -> ReadResult,
    close: fn(Handle) -> Nil,
  )
}

pub type SpawnError {
  ExecutableNotFound(path: String)
  SpawnFailed(reason: String)
}

/// Handle is an INTERNAL sum type (not pub) - only this module can construct handles.
/// Tests use test_runner() which creates TestHandle internally; callers never see Handle.
type Handle {
  ErlangHandle(port: Port)
  TestHandle(state: Dynamic)  // Opaque state for test mocks
}

/// **v1 Architecture: BEAM-only, Direct Port Access (CANONICAL)**
///
/// v1 is BEAM-only. `QueryStream` uses `port_ffi` directly for ALL operations.
/// The `Runner` abstraction is a **future-facing placeholder** for v2 multi-platform support.
///
/// **v1 Canonical Model**:
/// ```gleam
/// // v1: QueryStream uses port_ffi directly
/// type QueryStreamInternal {
///   QueryStreamInternal(
///     port: Port,           // Direct BEAM port
///     state: StreamState,
///     buffer: BitArray,
///   )
/// }
/// ```
///
/// **What this means for v1**:
/// - `query()` spawns via `port_ffi.ffi_open_port()` directly
/// - `next()` reads via `port_ffi.receive_blocking()` directly (returns Result)
/// - `close()` closes via `port_ffi.ffi_close_port()` directly
/// - All drain operations use `port_ffi.receive_timeout()` directly (returns Result)
/// - **Runner is NOT used on the production path in v1**
///
/// **Runner's role in v1 (test-only)**:
/// - `test_runner()`: Provides mock spawn/read/close for unit tests
/// - Unit tests use `test_runner()` to avoid real port operations
/// - `erlang_runner()` exists but is NOT used by QueryStream in v1
///
/// **Unit test strategy**:
/// ```gleam
/// // Production: uses port_ffi directly
/// pub fn query(prompt, options) {
///   case options.test_mode {
///     False -> query_via_port_ffi(prompt, options)  // v1 production: port_ffi directly
///     True -> query_via_test_runner(prompt, options.test_runner)  // test path only
///   }
/// }
/// ```
///
/// **v2 Impact (JS target)**:
/// When adding Node.js support, `Runner` becomes the real abstraction.
/// QueryStream would use Runner for all operations, with platform-specific
/// implementations (erlang_runner, node_runner).
///
/// **Location mapping (v1)**:
/// | Operation | Location | Uses |
/// |-----------|----------|------|
/// | `query()` spawn | `src/claude_agent_sdk.gleam` | `port_ffi.ffi_open_port()` |
/// | `next()` read | `src/claude_agent_sdk/internal/stream.gleam` | `port_ffi.receive_blocking()` → `Result(PortMessage, String)` |
/// | Timed drain | `src/claude_agent_sdk/internal/stream.gleam` | `port_ffi.receive_timeout()` → `Result(PortMessage, String)` |
/// | `close()` | `src/claude_agent_sdk/internal/stream.gleam` | `port_ffi.ffi_close_port()` |
/// | Unit tests | `test/*.gleam` | `test_runner()` mocks |

/// Result of reading next chunk from the underlying port/process.
///
/// ## Exit Status Observation Contract
///
/// The v1 port_ffi implementation observes exit status as follows:
///
/// **Message types from BEAM port** (with `exit_status` option):
/// - `{Port, {data, Bytes}}` → `Data(Bytes)`
/// - `{Port, {exit_status, Code}}` → `ExitStatus(Code)`
/// - `{Port, eof}` → `Eof`
///
/// **Ordering behavior (best effort)**:
/// 1. `{data, Bytes}` messages typically arrive BEFORE `{exit_status, Code}`
/// 2. However, scheduler timing can cause `exit_status` to be observed first
/// 3. `eof` may or may not arrive (depends on port mode and OS)
/// 4. The SDK state machine tolerates any ordering (see below)
///
/// **QueryStream prioritization**:
/// The QueryStream layer (using port_ffi directly in v1) maintains a line buffer.
/// When `read_next()` returns `ExitStatus(code)`:
/// - Any buffered incomplete line is processed first (may yield JsonDecodeError)
/// - THEN the exit status is processed (yielding ProcessError or EndOfStream)
///
/// **close() behavior (with mailbox cleanup)**:
/// - `close()` MUST be called from the SAME process that called `query()`
/// - `close()` calls `port_close(Port)` which terminates the subprocess
/// - THEN performs bounded drain (50ms timeout, filter on THIS Port only)
/// - This prevents leftover messages from polluting subsequent queries
/// - After `close()`, subsequent `read_next()` returns `Eof`
///
/// **Process Ownership Contract (CRITICAL - Single Source of Truth)**:
///
/// `QueryStream` is single-process. Behavior is UNDEFINED if:
/// - `next()` is called from a different process than `query()`
/// - `next()` is called from multiple processes concurrently
/// - `close()` is called from a different process than `query()`
///
/// **Rationale**: Erlang ports deliver messages to the spawning process's mailbox.
/// Calling `close()` from another process may close the underlying port, but the
/// blocked `receive` in the owning process will NOT be unblocked, potentially
/// causing deadlock or resource leak. This is a fundamental Erlang port constraint.
///
/// **Cross-process close() behavior (explicit)**:
/// - `close()` from another process: Closes the port, but the blocked `next()` in
///   the owning process remains blocked indefinitely (no unblock mechanism exists).
///   The SDK does NOT detect or warn about cross-process close() calls.
/// - `close()` while `next()` is blocked in same process: Not possible - Gleam is
///   single-threaded per process; a blocked `next()` prevents any other code from
///   executing in that process.
/// - For cancellation: Users must wrap SDK in an OTP process (see Cancellation Recipe).
///
/// **API documentation requirement**: README must state:
/// - "`close()` is only effective when called by the process that spawned the query"
/// - "Because `next()` blocks, cross-process cancellation requires an OTP wrapper"
/// - "The SDK does not attempt to detect cross-process use; violating this contract
///   results in undefined behavior (typically deadlock)"
///
/// **If cross-process use is needed**: Wrap in an OTP GenServer (outside SDK scope)
///
/// **Mailbox isolation invariant**:
/// Every `receive` MUST filter on the specific `Port` reference:
/// ```erlang
/// receive {Port, Msg} -> ... end  % CORRECT: filters on Port
/// receive Msg -> ... end          % WRONG: could receive other port's messages
/// ```
/// This ensures sequential queries in the same process never cross-contaminate.
///
pub type ReadResult {
  Data(BitArray)      // Raw bytes from port
  Eof                 // Stream ended (port closed or eof received)
  ExitStatus(Int)     // Process exited with code (distinct from Eof)
  ReadError(String)   // I/O error
}

/// Construct BEAM runner using FFI port operations (v1 - public)
/// See [Authoritative FFI Interface](#authoritative-ffi-interface-single-source-of-truth) for implementation details.
pub fn erlang_runner() -> Runner {
  Runner(
    spawn: erlang_spawn,    // Returns Ok(ErlangHandle(port))
    read_next: erlang_read, // Pattern matches on ErlangHandle, uses receive
    close: erlang_close,
  )
}

/// CRITICAL: Port Read Mechanism and Process Ownership
///
/// The erlang_read() function uses Erlang's `receive` to read RAW PORT MESSAGES:
/// ```erlang
/// %% RAW PORT MESSAGE FORMAT (what BEAM delivers):
/// %%   {Port, {data, Bytes}} | {Port, {exit_status, Code}} | {Port, eof}
/// %%
/// %% FFI CONVERTS to uniform 2-tuples with string tags for Gleam:
/// %%   {"data", Bytes} | {"exit_status", Code} | {"eof", nil}
/// %% See Authoritative FFI Interface for canonical implementation.
/// ```
///
/// IMPORTANT CONSTRAINT: Port messages are delivered to the process that
/// spawned the port. Therefore:
///
/// 1. `query()` spawns the port in the calling process
/// 2. `next()` MUST be called from the SAME process that called `query()`
/// 3. Passing QueryStream to another process will NOT work (messages go to original process)
///
/// This is documented in the API:
/// - "QueryStream is not process-safe; use from the process that created it"
/// - If cross-process use is needed, wrap in an OTP process (outside SDK scope)
///
/// The "no OTP actors" constraint means the SDK itself doesn't introduce
/// GenServers or supervision trees. Users can wrap the SDK in their own
/// OTP processes if needed.

**Cancellation Pattern Recipe (Documentation Only - See README)**:

*Full example moved to `README.md` during implementation. Summary below:*

The SDK's blocking `next()` cannot be cancelled from another process. For users who need cancellation (timeouts, user abort, etc.), here is the recommended pattern:

```gleam
// Example: Cancellable query with timeout using gleam_otp

import gleam/erlang/process.{Subject}
import gleam/otp/task

/// Run a query with cancellation support.
/// Returns stream items via the result_subject.
/// Send Cancel message to cancel_subject to abort.
pub fn query_with_cancellation(
  prompt: String,
  options: QueryOptions,
  result_subject: Subject(Result(StreamItem, StreamError)),
  cancel_subject: Subject(Cancel),
) -> task.Task(Nil) {
  task.async(fn() {
    // This process owns the stream
    case query(prompt, options) {
      Error(e) -> {
        process.send(result_subject, Error(QueryStartError(e)))
      }
      Ok(stream) -> {
        consume_with_cancellation(stream, result_subject, cancel_subject)
      }
    }
  })
}

fn consume_with_cancellation(
  stream: QueryStream,
  result_subject: Subject(Result(StreamItem, StreamError)),
  cancel_subject: Subject(Cancel),
) {
  // Check for cancellation before each blocking read
  case process.receive(cancel_subject, 0) {
    Ok(Cancel) -> {
      // Cancellation requested - close and exit
      close(stream)
      process.send(result_subject, Ok(EndOfStream))
    }
    Error(Nil) -> {
      // No cancellation - proceed with blocking read
      case next(stream) {
        #(Ok(EndOfStream), _) -> {
          process.send(result_subject, Ok(EndOfStream))
        }
        #(Ok(item), new_stream) -> {
          process.send(result_subject, Ok(item))
          consume_with_cancellation(new_stream, result_subject, cancel_subject)
        }
        #(Error(e), new_stream) if !is_terminal(e) -> {
          process.send(result_subject, Error(e))
          consume_with_cancellation(new_stream, result_subject, cancel_subject)
        }
        #(Error(e), _) -> {
          process.send(result_subject, Error(e))
        }
      }
    }
  }
}

type Cancel { Cancel }

// Usage:
let result_subject = process.new_subject()
let cancel_subject = process.new_subject()

let task = query_with_cancellation("prompt", options, result_subject, cancel_subject)

// To cancel:
process.send(cancel_subject, Cancel)

// To receive results:
case process.receive(result_subject, 30_000) {
  Ok(Ok(item)) -> handle_item(item)
  Ok(Error(e)) -> handle_error(e)
  Error(Nil) -> handle_timeout()
}
```

**Key points**:
1. The consuming process owns the stream (spawned via `task.async`)
2. Parent process communicates via subjects (messages)
3. Cancellation is checked between blocking `next()` calls
4. `close()` is always called when cancelling (releases port resources)
5. This pattern adds `gleam_otp` dependency but keeps SDK non-actor

**Alternative: Timeout per `next()` call** (not currently supported):
A future SDK version could add `next_with_timeout(stream, ms)` that uses Erlang's `receive ... after N` pattern. This would allow per-read timeouts without spawning a separate process.

/// Construct test runner for unit tests.
/// The mock functions receive/return Dynamic values that are wrapped internally.
/// This avoids exposing Handle to test code while enabling full stream semantics testing.
///
/// Uses SAME signature as erlang_runner: spawn(executable, args, cwd)
pub fn test_runner(
  on_spawn: fn(String, List(String), String) -> Result(Dynamic, String),
  on_read: fn(Dynamic) -> ReadResult,
  on_close: fn(Dynamic) -> Nil,
) -> Runner {
  Runner(
    spawn: fn(executable, args, cwd) {
      case on_spawn(executable, args, cwd) {
        Ok(state) -> Ok(TestHandle(state))
        Error(e) -> Error(SpawnFailed(e))
      }
    },
    read_next: fn(handle) {
      case handle {
        TestHandle(state) -> on_read(state)
        _ -> ReadError("Wrong handle type")
      }
    },
    close: fn(handle) {
      case handle {
        TestHandle(state) -> on_close(state)
        _ -> Nil
      }
    },
  )
}
```

**Why this works**:
- `Handle` is not `pub`, so test code cannot construct it directly
- `test_runner()` accepts `Dynamic` values and wraps them in `TestHandle` internally
- Tests provide mock functions that work with their own state types (cast to/from Dynamic)
- The `Runner` functions close over the handle, so `query()` and `QueryStream` work identically for both runners

**State Evolution in test_runner()**:

The `test_runner()` needs to model stateful iteration. Since `on_read` is called repeatedly but returns only `ReadResult` (not updated state), state must be managed externally.

**Canonical Strategy: ETS with unique ref key**

ETS is the canonical approach for test_runner state. Process dictionary is NOT recommended (flaky under concurrent test execution).

**ETS Example (Canonical)**:
```gleam
// State in ETS, keyed by unique ref for concurrency safety
pub fn mock_stream_test() {
  let table = ets.new("mock_state", [set, public])

  let runner = test_runner(
    on_spawn: fn(_, _, _) {
      let ref = make_ref()
      ets.insert(table, {ref, ["line1\n", "line2\n"]})
      Ok(dynamic.from(ref))
    },
    on_read: fn(state) {
      let ref = dynamic.unsafe_coerce(state)
      case ets.lookup(table, ref) {
        [{_, [line, ..rest]}] -> {
          ets.insert(table, {ref, rest})
          Data(bit_array.from_string(line))
        }
        _ -> Eof
      }
    },
    on_close: fn(state) {
      let ref = dynamic.unsafe_coerce(state)
      ets.delete(table, ref)
    },
  )
}
```

**Why ETS, not process dictionary**: Process dictionary is flaky under concurrent test execution. ETS with unique ref keys ensures tests can run in parallel without interference.

**ETS FFI Requirements (TEST-ONLY)**:

**IMPORTANT**: ETS FFI helpers MUST be located under `test/` directory ONLY, NOT in `src/`. This ensures ETS is never used by runtime modules.

| File | Purpose |
|------|---------|
| `test/support/ets_helpers.gleam` | ETS FFI bindings for test_runner state management |
| `test/support/test_utils.gleam` | General test utilities (no ETS) |

**Invariant**: No `src/**/*.gleam` file may import from `test/**`. This is enforced by Gleam's module system (test modules are not visible to src).

| Feature | Location | FFI Binding |
|---------|----------|-------------|
| `ets:new/2` | `test/support/ets_helpers.gleam` | `@external(erlang, "ets", "new")` |
| `ets:insert/2` | `test/support/ets_helpers.gleam` | `@external(erlang, "ets", "insert")` |
| `ets:lookup/2` | `test/support/ets_helpers.gleam` | `@external(erlang, "ets", "lookup")` |
| `ets:delete/2` | `test/support/ets_helpers.gleam` | `@external(erlang, "ets", "delete")` |
| `make_ref/0` | `test/support/ets_helpers.gleam` | `@external(erlang, "erlang", "make_ref")` |

**v1 test_runner state strategy: ETS only**

Use ETS with unique ref key for all stateful test_runner tests. This is the ONLY approach described in the plan.

**Why ETS only**:
- Safe with concurrent test execution (unique keys per handle)
- No `gleam_otp` dependency (violates test dep policy)
- Clear, single pattern for implementers

**Alternative strategies** (process dictionary, Subject-based) are intentionally NOT documented in this plan. If needed for edge cases, create `docs/TESTING.md` during implementation with caveats.

**Public vs Internal API**:
- **Public (exported)**: `test_runner()` (for unit tests), `query()`, `QueryStream`, all message types
- **Internal (not exported)**: `port_ffi` module (all functions), `Runner` type (v1 test-only), `Handle` type, port configuration

**v1 Note**: `erlang_runner()` exists internally but is NOT on the v1 production path and NOT in public API. It may be made public in v2 for multi-platform support.

### Port/Process Management (v1 port_ffi)

The ErlangRunner uses **FFI-only port operations** (see [Authoritative FFI Interface](#authoritative-ffi-interface-single-source-of-truth)) with **explicit stdout piping** (OTP 27+):

```erlang
%% Port options for OTP 27+ (gleam_erlang 1.3.0+)
%% CRITICAL: Must explicitly pipe stdout to receive data as port messages
%%
%% v1 uses STREAM mode exclusively (not {line, N}) for correctness:
%% - Handles arbitrary-length NDJSON lines (no truncation)
%% - Manual line reassembly with buffer overflow protection
%% - Consistent behavior regardless of message size
Port = open_port({spawn_executable, ClaudePath}, [
    {args, Args},
    {cd, Cwd},
    stream,           % Raw byte stream - SDK handles line reassembly
    binary,           % Binary data mode
    exit_status,      % Receive exit status on close
    use_stdio         % REQUIRED: connect to program's stdio for stdout piping
    %% NOTE: stderr_to_stdout is NOT included - stderr goes to parent
])
```

**v1 Mode Choice: `stream` Only**

The SDK uses `stream` mode exclusively in v1 (not `{line, N}`):

| Consideration | `stream` mode | `{line, N}` mode |
|---------------|---------------|------------------|
| Arbitrary line length | ✅ Handles any size | ❌ Truncates at N bytes |
| Buffer overflow protection | ✅ SDK controls limit | ❌ Relies on port behavior |
| Implementation complexity | Medium (line reassembly) | Low |
| v2 optimization candidate | N/A | ✅ For known-small messages |

**Why not `{line, N}`**: NDJSON messages from Claude can be arbitrarily large (e.g., large code blocks, tool results). Using `{line, 65536}` would silently truncate lines > 64KB, causing JSON parse failures. Stream mode with SDK-controlled buffering is safer.

**OTP 27 Port Behavior (Verified)**:

With `{spawn_executable, Path}` and `use_stdio`:
1. **Stdout**: Connected via pipe → SDK receives `{Port, {data, Bytes}}` messages
2. **Stderr**: Inherited by child process → goes to parent's stderr (NOT captured by SDK)
3. **Stdin**: Connected but SDK doesn't write to it

**Port options**: See [Authoritative FFI Interface](#authoritative-ffi-interface-single-source-of-truth) for the exact port options used. The `use_stdio` option is included unconditionally to ensure stdout piping works across OTP versions.

**Stderr Behavior**:
- Stderr is inherited from parent process (visible in terminal)
- NOT captured by SDK (no `stderr_to_stdout`)
- No deadlock risk (no unread stderr pipe)
- No NDJSON corruption (stderr never mixed into port data)

**Integration test preflight** (validates port configuration):
```gleam
/// Preflight check: ensure port actually delivers stdout data
/// Run before any real queries to fail fast if port config is wrong
pub fn validate_port_config() -> Result(Nil, String) {
  // Spawn: echo "test" (or equivalent cross-platform)
  // Expect: receive {Port, {data, <<"test\n">>}} within 1 second
  // If no data received: port config is broken, return Error
}
```

**v2 consideration**: Capture stderr via wrapper script (preferred) that writes stderr to a temp file or named pipe. Using `stderr_to_stdout` is NOT recommended as it violates the NDJSON purity contract and complicates parsing.

**I/O Contract (Critical Design)**:

**Stdout**: Pure NDJSON messages. The `--output-format stream-json` flag ensures all structured data is JSON.

**Stderr**: NOT captured in v1 — flows to parent's stderr (visible in terminal where app runs). This avoids:
- NDJSON corruption from interleaved stderr content
- Deadlock risk from stderr pipe filling (since there's no stderr pipe)
- Complexity of multi-pipe reading on BEAM ports

**NDJSON Policy: Supported vs Tolerated (Canonical)**:

| Scenario | Classification | SDK Behavior | Preflight Result |
|----------|---------------|--------------|------------------|
| All stdout lines are valid JSON | **Supported** | Normal operation | PASS |
| 1-4 consecutive non-JSON lines | **Tolerated** | `JsonDecodeError` (non-terminal), continues | N/A (runtime only) |
| 5+ consecutive non-JSON lines | **Not Supported** | `TooManyDecodeErrors` (terminal) | N/A (runtime only) |
| Non-JSON mixed with valid JSON | **Tolerated** | Errors for bad lines, processes good lines | N/A (runtime only) |

**Integration Test Behavior When Preflight Fails (CANONICAL RULE)**:

**Single rule**: Preflight failure = integration tests **SKIP** with `[SKIP:NDJSON]` message.

| Preflight Result | Integration Tests | Message |
|------------------|-------------------|---------|
| PASS | Run normally | — |
| FAIL | **SKIP** all integration tests | `[SKIP:NDJSON] CLI not producing pure NDJSON` |
| TIMEOUT | **SKIP** all integration tests | `[SKIP:TIMEOUT] Preflight check timed out` |

**Preflight Timeout Bounds (MANDATORY)**:

All preflight checks MUST use the same 5-second timeout as version detection:

| Preflight Check | Timeout | Rationale |
|-----------------|---------|-----------|
| Version detection (`claude --version`) | 5s | Matches runtime version detection |
| NDJSON purity check | 5s | Prevents hang on misbehaving CLI |
| Auth status check (if CLI supports) | 5s | Prevents hang on network issues |

**Skip Mechanism (AUTHORITATIVE)**:

To skip a test without failing gleeunit:

```gleam
fn skip_test(reason: String) -> Nil {
  io.println("[SKIP] " <> reason)
  should.be_true(True)  // Pass the test (no assertion failure)
}

// CORRECT: Skip with message, test passes
pub fn integration__ndjson_purity_test() {
  case preflight_ndjson_check() {
    Ok(True) -> run_actual_test()
    Ok(False) -> skip_test("[SKIP:NDJSON] CLI not producing pure NDJSON")
    Error(Timeout) -> skip_test("[SKIP:TIMEOUT] Preflight check timed out")
  }
}
```

**Do NOT**:
- Return early without `should.be_true(True)` — gleeunit may mark as failed
- Print skip message without passing — confuses test output
- Run preflight in unit tests — preflight is integration-only

**Rationale**: Integration tests assume supported CLI behavior. If the CLI isn't producing pure NDJSON, test results would be unreliable (pass/fail based on timing of non-JSON lines). Skipping with a clear message is better than flaky tests.

**Runtime tolerance is separate**: The 5-consecutive-error threshold is for production resilience, NOT for test tolerance. Tests should not rely on tolerating decode errors.

**Line Reassembly for Large Messages**:

Using `stream` mode, data arrives as arbitrary byte chunks. The runner maintains a buffer and reassembles lines:

```gleam
/// Internal reader state
type ReaderState {
  ReaderState(
    port: Port,
    buffer: BitArray,        // Accumulated bytes awaiting newline
    closed: Bool,
  )
}

/// Maximum buffer size before yielding BufferOverflow error (safety guard)
const max_buffer_bytes = 10_000_000  // 10MB

/// Read next complete line, handling arbitrary-length messages
fn read_line(state: ReaderState) -> #(ReadLineResult, ReaderState) {
  // 1. Check buffer for complete line (ends with \n)
  // 2. If found: extract line, return it, keep remainder in buffer
  // 3. If not found and buffer < max_buffer_bytes: read more bytes, repeat
  // 4. If buffer >= max_buffer_bytes: return BufferOverflow error
}

type ReadLineResult {
  CompleteLine(String)
  PortClosed
  ExitReceived(Int)
  BufferOverflow      // Safety: line exceeded 10MB
  Error(String)
}
```

**Line Classification and JSON Parsing**:

```gleam
/// Attempt to parse a line as NDJSON message
fn parse_line(line: String) -> LineParseResult {
  // With stdout-only and --verbose, all lines should be JSON
  // Attempt decode; failure means malformed output (rare)
  case json.decode(line, message_decoder()) {
    Ok(msg) -> JsonMessage(msg, line)
    Error(e) -> MalformedLine(line, e)
  }
}
```

**Port Lifecycle Contract (Single Source of Truth)**:

All drain and close behavior is defined here. Other sections MUST reference this section.

**For timeout values and bounds**, see [All timeout values](#all-timeout-values-single-source-of-truth). This section defines WHEN and HOW drains occur; the timeout section defines the numeric values.

**Q1: Who closes the port and when is drain performed?**

| Scenario | Pre-Close Drain | Mechanism |
|----------|-----------------|-----------|
| Normal completion (after Result) | Post-Result drain (100ms × 10 msgs) | `ffi_close_port()` after drain completes |
| exit_status before Result | Post-exit late-drain (50ms × 10 iters) | `ffi_close_port()` after drain completes |
| User calls `close()` | None before close | `ffi_close_port()` (has internal mailbox drain) |
| Terminal internal error | None before close | `ffi_close_port()` (has internal mailbox drain) |
| Process crash | None | Erlang runtime auto-cleanup |

**Key distinction**: `ffi_close_port()` itself performs a bounded mailbox drain (50ms × 100 msgs) to prevent mailbox pollution. The "pre-close drain" column refers to SDK-level draining that yields warnings or collects data. Terminal errors skip SDK-level draining but still get the FFI-level mailbox cleanup.

**Q2: What does each drain yield?**

| Drain Type | When | Bounds | Yields |
|------------|------|--------|--------|
| **Post-Result drain** | After `Result` yielded | 100ms × 10 msgs | Late messages → `WarningEvent`, then `EndOfStream` |
| **Post-exit late-drain** | On `exit_status` received | 50ms × 10 iters, 1KB cap | Appends to buffer (internal), no direct yield |
| **FFI mailbox drain** | Inside `ffi_close_port()` | 50ms × 100 msgs | Nothing (silent discard, cleanup only) |

**Drain Timing (CANONICAL RULE)**:

Drain is performed **only on subsequent `next()` calls**, NOT inside the call that yields Result.

```
next() call #1: Yields Ok(Message(Result(_))) → state = ResultReceived
next() call #2: State is ResultReceived, performs drain, yields Ok(WarningEvent) or Ok(EndOfStream)
next() call #3: (if needed) Yields remaining drain items or Ok(EndOfStream)
```

**The call that yields Result does NOT drain.** This preserves single-yield-per-call invariant.

**Q3: What happens on `exit_status` with partial buffer data?**

See [Explicit rule for incomplete buffer at exit_status](#explicit-rule-for-incomplete-buffer-at-exit_status). Summary:

| Buffer State | Exit Code | Behavior |
|--------------|-----------|----------|
| Complete lines (ends with `\n`) | Any | Process lines first, then handle exit |
| Incomplete line | 0 (clean) | `Warning(IncompleteLastLine)`, then `EndOfStream` |
| Incomplete line | ≠0 (error) | `ProcessError` with diagnostic including partial buffer |
| Empty | 0 | `Warning(EmptyResponse)`, then `EndOfStream` |

**Canonical Terminal Path Sequences (AUTHORITATIVE)**:

Each terminal path is documented with explicit port status and when `ffi_close_port()` is called.

**Path 1: Normal completion (Result received, then clean exit)**
```
1. next() in STREAMING: receive Data(Result JSON)
   → Yield Ok(Message(Result(_)))
   → State = ResultReceived, Port = OPEN

2. next() in ResultReceived: receive ExitStatus(0)
   → SDK-level drain complete (no warnings to yield)
   → Call ffi_close_port() [FFI mailbox drain runs inside this call]
   → Yield Ok(EndOfStream)
   → State = Closed, Port = CLOSED
```

**Path 2: Normal completion with non-zero exit after Result**
```
1. next() in STREAMING: receive Data(Result JSON)
   → Yield Ok(Message(Result(_)))
   → State = ResultReceived, Port = OPEN

2. next() in ResultReceived: receive ExitStatus(non-zero)
   → Yield Ok(WarningEvent(NonZeroExitAfterResult(code)))
   → State = PendingEndOfStream, Port = STILL OPEN (waiting to yield EndOfStream)

3. next() in PendingEndOfStream:
   → Call ffi_close_port() [FFI mailbox drain runs inside this call]
   → Yield Ok(EndOfStream)
   → State = Closed, Port = CLOSED
```

**Path 3: Early exit (exit_status before Result, code=0)**
```
1. next() in STREAMING: receive ExitStatus(0) [no Result seen]
   → Post-exit late-drain: 50ms × 10 iters, collect late data
   → Process any buffered data
   → Yield Ok(WarningEvent(CleanExitNoResult))
   → State = PendingEndOfStream, Port = STILL OPEN

2. next() in PendingEndOfStream:
   → Call ffi_close_port() [FFI mailbox drain runs inside this call]
   → Yield Ok(EndOfStream)
   → State = Closed, Port = CLOSED
```

**Path 4: Process error (exit_status before Result, code≠0)**
```
1. next() in STREAMING: receive ExitStatus(non-zero) [no Result seen]
   → Post-exit late-drain: 50ms × 10 iters, collect late data
   → Process any buffered data (may contribute to diagnostic)
   → Call ffi_close_port() [FFI mailbox drain runs inside this call]
   → Yield Error(ProcessError(code, diagnostic))
   → State = Closed, Port = CLOSED
```

**Path 5: User calls close() mid-stream**
```
1. User calls close() at any point (any state)
   → Call ffi_close_port() [terminates subprocess + FFI mailbox drain]
   → State = Closed, Port = CLOSED

2. Subsequent next() calls:
   → Yield Ok(EndOfStream)
   → No further port operations
```

**Drain Interaction Rule (CANONICAL)**:
- **SDK-level drain** (post-Result or post-exit): Consumes messages from mailbox and yields warnings. Runs BEFORE `ffi_close_port()`.
- **FFI-level drain** (inside `ffi_close_port()`): Silent discard of any remaining messages. Runs AFTER `port_close()` to prevent mailbox pollution.
- **Order**: SDK drain first (if applicable) → `ffi_close_port()` → FFI drain → port closed
- **No double-drain**: SDK drain uses `receive_timeout` to read messages; FFI drain only catches stragglers that arrive after SDK drain completes.

**Invariant**: Every `ffi_close_port()` call includes internal mailbox drain. SDK-level draining (post-Result, post-exit) is performed for normal/exit_status paths to capture warnings; terminal errors skip SDK-level draining but still get FFI-level mailbox cleanup.

**SDK vs FFI Cleanup Contract (AUTHORITATIVE)**:

The cleanup responsibilities are divided as follows:

| Layer | Responsibility | When |
|-------|---------------|------|
| **SDK (stream.gleam)** | Semantic drains (yield warnings, collect late data) | Before transitioning to Closed |
| **SDK (stream.gleam)** | Tracks `closed: Bool` in QueryStream state | Always |
| **SDK (stream.gleam)** | Calls `ffi_close_port()` exactly once | On transition TO Closed state |
| **FFI (port_ffi)** | `port_close()` + silent mailbox drain | When SDK calls `ffi_close_port()` |

**Key rules**:

1. **SDK owns semantic drains**: Post-Result drain (100ms × 10) and post-exit late-drain (50ms × 10) run BEFORE calling `ffi_close_port()`. These drains yield warnings via the stream. FFI drain does NOT yield anything.

2. **SDK tracks closed state**: `QueryStream` contains `closed: Bool`. Before calling `ffi_close_port()`, SDK sets `closed = True`. All subsequent operations check this flag first.

3. **FFI close is non-semantic**: `ffi_close_port()` calls `port_close()` then silently discards remaining mailbox messages. It does NOT return values or yield warnings.

4. **Idempotency guarantee**:
   - SDK level: `close()` checks `closed` flag; if True, returns immediately (no-op)
   - FFI level: Calling `port_close()` on already-closed port is safe in Erlang (silently ignored)
   - Double-call is thus safe at both layers

5. **Mailbox isolation**: Both SDK drains and FFI drain filter on the specific Port term:
   ```erlang
   receive {Port, Msg} -> ...  % CORRECT: filters on Port
   ```
   This ensures sequential queries never cross-contaminate.

**Why FFI drain exists**: It catches any messages that arrive after SDK drain completes but before/during port_close. This is a "last line of defense" for scheduler timing edge cases, not a semantic feature.

**Port Close + Drain Interaction Contract (CRISP INVARIANTS)**:

| Invariant | Requirement |
|-----------|-------------|
| `ffi_close_port()` order | MUST call `port_close()` FIRST, then bounded mailbox drain |
| SDK semantic drains | MUST happen ONLY while port is still open |
| Semantic drain objective | Yield warnings / append small late data, NOT exhaustively consume mailbox |
| Post-close messages | All `{Port, _}` after `ffi_close_port()` are discardable cleanup |
| No hang risk | FFI drain uses bounded timeout (50ms × 100); never blocks indefinitely |

**What prevents double-consume or missing messages**:
1. SDK drains are bounded (100ms × 10 or 50ms × 10) - they stop after timeout, not exhaustive
2. SDK drain happens BEFORE port_close - port is still receiving
3. `port_close()` terminates subprocess - no more new data after this point
4. FFI drain runs AFTER port_close - only catches scheduler-delayed messages
5. FFI drain is silent discard - no semantic value, just cleanup

**close() implementation**:
```gleam
pub fn close(stream: QueryStream) -> QueryStream {
  case stream.closed {
    True -> stream  // Already closed, no-op
    False -> {
      // SDK does NOT do semantic drain on explicit close() - user chose to abort
      ffi_close_port(stream.port)
      QueryStream(..stream, closed: True, state: Closed)
    }
  }
}
```

**Stream Termination State Machine**:

The stream reader maintains state to handle the race between `Result` message and `exit_status`:

```
                     ┌─────────────────────────────────────┐
                     │           STREAMING                 │
                     │  (reading NDJSON lines from port)   │
                     │  result_seen = false                │
                     └──────────────┬──────────────────────┘
                                    │
            ┌───────────────────────┼───────────────────────┐
            │                       │                       │
            ▼                       ▼                       ▼
   ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
   │ Result message  │    │ exit_status     │    │ Eof (port       │
   │ received        │    │ received        │    │ closed)         │
   │                 │    │                 │    │                 │
   │ SET result_seen │    │ CHECK           │    │                 │
   │ = true          │    │ result_seen     │    │                 │
   └────────┬────────┘    └────────┬────────┘    └────────┬────────┘
            │                      │                      │
            ▼                      │                      ▼
   ┌─────────────────┐    ┌────────┴────────┐    ┌─────────────────┐
   │ Yield Result,   │    │                 │    │ EndOfStream       │
   │ enter DRAINING  │    │                 │    │ (warn if no     │
   │ state           │    │                 │    │  Result seen)   │
   └────────┬────────┘    │                 │    └─────────────────┘
            │       ┌─────┴─────┐     ┌─────┴─────┐
            │       │result_seen│     │result_seen│
            │       │= FALSE    │     │= TRUE     │
            │       ▼           │     ▼           │
            │  ┌─────────┐      │  ┌─────────┐    │
            │  │code=0   │      │  │ANY code │    │
            │  │→EndOfSt │      │  │→Warning │    │
            │  │code≠0   │      │  │+EndOfSt │    │
            │  │→ProcErr │      │  └─────────┘    │
            │  └─────────┘                        │
            │                                     │
            ▼                                     │
   ┌─────────────────┐                            │
   │ DRAINING:       │◄───────────────────────────┘
   │ Wait for        │
   │ exit_status     │
   │ (100ms timeout) │
   │                 │
   │ ANY exit_status │
   │ → Warning only  │
   │ (Result is      │
   │ authoritative)  │
   └─────────────────┘
```

**Result vs exit_status precedence rule (CRITICAL)**:

| Scenario | result_seen | exit_status | Outcome |
|----------|-------------|-------------|---------|
| exit_status before Result | FALSE | 0 | EndOfStream (warn: no Result) |
| exit_status before Result | FALSE | ≠0 | **ProcessError (terminal)** |
| exit_status after Result | TRUE | 0 | EndOfStream (normal) |
| exit_status after Result | TRUE | ≠0 | **Warning only** + EndOfStream |

**Crisp invariant**: If `result_seen == TRUE`, no subsequent `exit_status` can produce `ProcessError`. Only warnings + EndOfStream.

**Termination rules (explicit)**:

The key principle: **Result determines stream termination semantics; exit_status after Result is converted into a warning (if non-zero) but does NOT flip the outcome into ProcessError.**

**IMPORTANT: Semantic trade-off for downstream users**

This rule can hide real process failures. For example:
- CLI emits a `Result` message
- CLI crashes before flushing final stdout
- Exit code is non-zero, but SDK demotes it to a warning

Users who rely on stream completion (e.g., for data pipelines) may incorrectly treat partial/incorrect output as success.

**Stricter semantics option (DEFERRED TO v2)**:

> **NOT IN v1 SCOPE** — Design sketched below for future reference. v1 uses "Result is authoritative" semantics only.

The option `strict_exit_status: Bool` would allow users to treat non-zero exit codes as terminal errors even after Result. This is deferred to v2 because:
1. **Adds complexity**: Changes core stream semantics and acceptance tests
2. **Rare need**: Most users want Result to be authoritative
3. **Workaround exists**: Users can check `warnings` in `CollectResult` and fail manually

```gleam
// v2 design sketch (NOT implemented in v1)
pub type QueryOptions {
  QueryOptions(
    // ... existing fields ...
    /// v2: When True, non-zero exit_status after Result becomes ProcessError.
    strict_exit_status: Bool,  // DEFERRED TO v2
  )
}
```

**v1 behavior** (fixed, no option):

| Scenario | Behavior |
|----------|----------|
| Non-zero exit after Result | Warning + EndOfStream (Result is authoritative) |

Users who need stricter semantics in v1 can inspect `CollectResult.warnings` and fail if `NonZeroExitAfterResult` is present.

**README Recommendation for Automation/CI (MUST INCLUDE)**:

The README must include explicit guidance for users in automation contexts:

```markdown
## Success Semantics in Automation

**Important:** By default, the SDK treats `Result` as authoritative for success/failure.
A non-zero exit code AFTER a `Result` message produces only a *warning*, not an error.

**For CI pipelines, automation, or data reliability contexts**, we recommend treating
any warnings as potential failures:

```gleam
let result = collect_messages(stream)
case list.is_empty(result.warnings) {
  True -> // Clean success
  False -> // Has warnings - fail in strict contexts
    case list.any(result.warnings, is_exit_warning) {
      True -> panic as "Non-zero exit after Result - possible data integrity issue"
      False -> // Other warnings may be acceptable
    }
}

fn is_exit_warning(w: Warning) -> Bool {
  case w.code {
    NonZeroExitAfterResult(_) -> True
    _ -> False
  }
}
```

This ensures pipeline-grade success semantics without waiting for v2's `strict_exit_status` option.
```

**collect_* helpers return type (canonical definition)**:

The `collect_items()` and `collect_messages()` helpers use `CollectResult` to capture all stream output categories:

```gleam
/// Canonical return type for collect_* helpers.
/// Explicitly separates items, warnings, non-terminal errors, and terminal error.
pub type CollectResult(a) {
  CollectResult(
    /// Successfully parsed items (messages for collect_messages, all StreamItem for collect_items)
    items: List(a),
    /// Non-fatal warnings yielded during stream (e.g., NonZeroExitAfterResult, UnexpectedMessageAfterResult)
    warnings: List(Warning),
    /// Non-terminal errors encountered (e.g., JsonDecodeError, UnexpectedMessageError)
    /// These did NOT stop collection; stream continued past them.
    non_terminal_errors: List(StreamError),
    /// Terminal error that stopped collection (e.g., TooManyDecodeErrors, ProcessError)
    /// None if stream ended normally via EndOfStream.
    terminal_error: Option(StreamError),
  )
}
```

**Why explicit error categories**:
- `non_terminal_errors` allows users to see decode issues without them being silently swallowed
- `terminal_error` clearly indicates whether collection was interrupted
- `warnings` captures protocol-level warnings (exit status after Result, etc.)
- Users can choose to fail on any non-empty error list or proceed with partial data

**How `next()` results map to CollectResult**:

| `next()` returns | CollectResult field |
|-----------------|---------------------|
| `Ok(Message(m))` | `items` (appended) |
| `Ok(WarningEvent(w))` | `warnings` (appended) |
| `Ok(EndOfStream)` | Collection stops, `terminal_error = None` |
| `Error(e)` where `is_terminal(e)` | Collection stops, `terminal_error = Some(e)` |
| `Error(e)` where `!is_terminal(e)` | `non_terminal_errors` (appended), continue |

This ensures the unified stream contract (`next()` returns `Result(StreamItem, StreamError)`) maps cleanly to collection helpers without losing information.

**Result message semantics (clarification)**:

A `Result` message is the terminal completion of the *stream protocol*; it may represent task success or task failure:

| Field | Value | Meaning |
|-------|-------|---------|
| `is_error: False` | Combined with `subtype: Success` | Task completed successfully |
| `is_error: True` | Combined with error subtypes | Task failed (see below) |
| `subtype: ErrorMaxTurns` | — | Task hit turn limit (failure) |
| `subtype: ErrorMaxBudget` | — | Task hit budget limit (failure) |
| `subtype: ErrorDuringExecution` | — | Task encountered error (failure) |

**SDK responsibility vs user responsibility**:
- **SDK**: Treats `Result` as stream completion. Yields the `Result` message, then `EndOfStream`. Does NOT interpret `is_error` or `subtype` to determine stream-level success.
- **User**: Interprets `is_error` and `subtype` to determine application-level success. Pattern-matches on `ResultSubtype` to handle different outcomes.

**Why the SDK doesn't interpret Result**:
- Domain-specific interpretation is a non-goal (see Scope)
- Users may have different success criteria (e.g., ErrorMaxTurns might be acceptable for some use cases)
- Keeps the SDK focused on transport, not business logic

**Examples and tests should NOT imply Result==success**. Use neutral terminology like "stream completed" or "Result received" rather than "query succeeded".

**CLI behavior assumption (NOT a hard invariant)**:

> **Result is expected to be the last message** in current CLI versions (1.0.x).

This is an *assumption* based on observed behavior, NOT a guaranteed invariant. The SDK's authoritative contract is the **drain-after-Result** behavior, which handles cases where the assumption doesn't hold:

**Drain-after-Result behavior (public-facing sequencing)**:

The drain phase happens via SUBSEQUENT `next()` calls, not eagerly inside the call that returns Result:

1. **First call yields Result**: `next()` returns `Ok(Message(Result(_)))`. Stream state becomes `ResultReceived`.
2. **Subsequent calls do drain**: Caller continues calling `next()`. Each call uses 100ms timeout:
   - If more data arrives: yield `Ok(WarningEvent(UnexpectedMessageAfterResult(_)))`
   - If exit_status arrives: yield `Ok(EndOfStream)` (with warning if non-zero)
   - If timeout/EOF: yield `Ok(EndOfStream)`
3. **Drain cap**: After 10 drain iterations, force `EndOfStream` (discard remaining)

**Why drain on subsequent calls (not eager)**:
- Keeps `next()` semantics simple: one call = one item yielded
- Users who stop iterating after Result don't pay drain cost
- Resource cleanup happens when user requests next item OR calls `close()`

**Interaction with `with_stream` and `collect_*`**:
- These helpers iterate until `EndOfStream`, triggering automatic cleanup
- See [SDK vs FFI Cleanup Contract](#sdk-vs-ffi-cleanup-contract-authoritative) for drain sequencing
- Users calling `next()` manually should iterate until `EndOfStream` for full cleanup

**If user stops after Result without iterating to EndOfStream**:
- Port remains open until `close()` is called (or process exits)
- `close()` performs its own bounded drain (see Port Cleanup Contract)
- No resource leak, but exit_status may not be observed

**v1 Timeout Policy (Explicit)**:

See [Authoritative FFI Interface - Call site mapping](#call-site-mapping) for the exact timeout values per call site.

**next() timeout behavior is STATE-DEPENDENT (Canonical Timeout Policy)**:

| Stream State | next() Timeout | Rationale |
|--------------|----------------|-----------|
| `Streaming` (before Result) | ❌ Blocks indefinitely | User controls via `max_turns`/`max_budget` |
| `ResultReceived` (after Result) | ✅ **100ms per call** | Cleanup phase; short window for late messages |
| `Closed` | ❌ Returns immediately | No I/O needed |

**Public guarantee**: "Before Result, `next()` blocks; after Result, `next()` may return `EndOfStream` after up to 100ms even without receiving more data."

This is intentional state-dependent behavior, NOT an exception to a "no timeout" rule. Users should understand:
- Iteration before Result may block as long as CLI runs
- Iteration after Result completes quickly (≤100ms per call, ≤10 calls max)

**All timeout values (Single Source of Truth)**:

See also [Port Lifecycle Contract](#port-lifecycle-contract-single-source-of-truth) for when each drain operation occurs.

| Operation | Timeout | Max Iterations | Worst Case | Used By |
|-----------|---------|----------------|------------|---------|
| Post-Result drain | **100ms** | 10 | **~1s** | `next()` in ResultReceived state |
| Version detection | 5s | 1 | 5s | `detect_cli_version()` |
| Post-exit late-drain | 50ms | 10, 1KB cap | ~500ms | `drain_late_data()` |
| close() mailbox drain | 50ms | 100 | ~5s | `ffi_close_port()` |

**Constants Module** (`src/claude_agent_sdk/internal/constants.gleam`):

All tunable limits are centralized here. v1 treats these as internal; v2 may expose configurability.

```gleam
// src/claude_agent_sdk/internal/constants.gleam
// Single source of truth for all SDK limits and thresholds

// Buffer limits
pub const max_line_size_bytes = 10_485_760  // 10MB - single NDJSON line limit
pub const initial_buffer_size = 65_536      // 64KB - initial buffer allocation

// Error thresholds
pub const max_consecutive_decode_errors = 5  // Terminal after this many

// Drain bounds
pub const post_result_drain_timeout_ms = 100
pub const post_result_drain_max_iterations = 10
pub const post_exit_drain_timeout_ms = 50
pub const post_exit_drain_max_iterations = 10
pub const post_exit_drain_max_bytes = 1024   // 1KB
pub const close_mailbox_drain_timeout_ms = 50
pub const close_mailbox_drain_max_messages = 100

// Version detection
pub const version_detection_timeout_ms = 5000
```

**Rationale for v1 non-configurability**: These defaults cover expected CLI behavior. Making them configurable adds API surface and complexity. If users encounter edge cases (e.g., very large tool outputs), they can report and we'll adjust defaults or add configurability in v2.

**Why 100ms for post-Result drain** (changed from 500ms):
- CLI behavior: Late messages typically arrive within milliseconds of Result
- User impact: 500ms × 100 iterations = 50s worst case is too long
- New bounds: 100ms × 10 iterations = ~1s worst case is acceptable cleanup time
- If CLI is still producing output after Result, that's unexpected behavior

**Implementation of drain timeout**:
```gleam
// Internal only - not exposed in public API
fn drain_after_result(port: Port) -> List(Warning) {
  // Uses receive_port_msg with 100ms timeout (via FFI)
  // Max 10 iterations, collects warnings until exit_status/timeout/cap
}
```

This aligns with the state machine: once Result is received, the stream is logically complete, and we're just cleaning up.

```gleam
type StreamState {
  Streaming(buffer: BitArray, consecutive_decode_errors: Int)
  ResultReceived(result: ResultMessage, drain_count: Int)  // Continue draining
  Exited(code: Int)
  Closed
}

// After Result (internal drain phase with timeout - see v1 Timeout Policy):
ResultReceived(result, drain_count) ->
  case receive_port_msg_timeout(100) {  // 100ms drain timeout per call (state-dependent behavior)
    Data(bytes) if drain_count < 10 ->
      // Unexpected message after Result - yield as warning
      yield Warning(UnexpectedMessageAfterResult(bytes))
      ResultReceived(result, drain_count + 1)
    ExitStatus(code) ->
      // Normal drain completion
      case code {
        0 -> yield EndOfStream
        _ -> yield Warning(NonZeroExitAfterResult(code)), yield EndOfStream
      }
    Eof | Timeout ->
      // Drain complete (timeout or EOF)
      yield EndOfStream
  }
```

| Scenario | Behavior | Outcome |
|----------|----------|---------|
| Result → (drain) → exit_status=0 | Yield Result, drain, EndOfStream | Normal success |
| Result → more messages → exit_status=0 | Yield Result, yield warnings for each, EndOfStream | Success with warnings |
| Result → exit_status≠0 | Yield Result, drain, Warning(NonZeroExitAfterResult), EndOfStream | Success with warning |
| Result → timeout/EOF | Yield Result, EndOfStream | Normal success |
| exit_status≠0 → no Result | Yield ProcessError (terminal) | Failure |
| exit_status=0 → no Result | Yield Warning(CleanExitNoResult), EndOfStream | Abnormal success |

**Why drain instead of immediate close**:
- CLI may emit metadata/cleanup messages after Result in future versions
- Truncating risks data loss even if current CLI doesn't emit post-Result messages
- Short drain timeout prevents indefinite blocking (see [All timeout values](#all-timeout-values-single-source-of-truth))
- Drain cap prevents memory exhaustion from buggy CLI

**Authoritative Post-Result Contract (Public API)**:

Users MAY observe the following after receiving a `Result` message:

| Event | Type | Meaning |
|-------|------|---------|
| `Warning(UnexpectedMessageAfterResult)` | Warning | CLI emitted data after Result (unexpected but handled) |
| `Warning(NonZeroExitAfterResult(code))` | Warning | CLI exited non-zero after Result (task succeeded but cleanup failed) |
| `EndOfStream` | Terminal | Normal stream termination |

**Contract guarantees**:
1. `Result` is yielded BEFORE any post-Result warnings
2. No errors (only warnings) can occur after `Result`
3. `EndOfStream` always follows `Result` eventually
4. Drain timeout per [All timeout values](#all-timeout-values-single-source-of-truth) (~1s worst case)
5. If drain cap exceeded, remaining messages are discarded (warning logged)

**Clarification on "authoritative"**:
- Result determines *success/failure semantics* (task completed vs task failed)
- exit_status after Result is *surfaced as warning* (not ignored, not error)
- This is NOT "ignoring" exit_status; it's *demoting* non-zero exit_status from error to warning when Result already confirmed task completion

**Rationale for warning on non-zero exit after Result**: A non-zero exit code after emitting a Result could indicate:
- CLI crashed during cleanup after completing the task
- Partial failure that the CLI reported as success
- Resource cleanup issues

By emitting a warning rather than ignoring, we preserve the "Result-first" semantics (task completed) while surfacing the anomaly for debugging.

**Implementation state tracking**: See [StreamState type](#stream-termination-state-machine) for the canonical definition. The stream tracks `consecutive_decode_errors` in the `Streaming` state.

### CLI Command Contract

**Exact command shape used by SDK** (from SPEC: CLI Invocation):

```bash
claude --print --output-format stream-json --verbose [OPTIONS...] -- "PROMPT"
```

**Argument ordering**:
1. Fixed flags: `--print`, `--output-format`, `stream-json`, `--verbose`
2. Optional flags from QueryOptions (see mapping table)
3. Separator: `--`
4. Prompt as final positional argument (passed directly to spawn, no shell escaping)

**Prompt handling**: The prompt is passed as a single argument to `spawn_executable`. Gleam/Erlang handles argument passing directly to the OS. No shell escaping needed since we don't use a shell.

**Example generated command**:
```bash
# QueryOptions: model="sonnet", max_turns=5, allowed_tools=["Read", "Write"]
claude --print --output-format stream-json --verbose --model sonnet --max-turns 5 --allowed-tools Read,Write -- "List all .gleam files"
```

### Data Model

#### Schema Source and Forward Compatibility

**Schema source**: Message types are based on the Claude Code SDK specification (SPEC: Message Schema, SPEC: Message Types) and verified against real CLI output from Claude Code CLI v1.0.x. Test fixtures are captured from:
- `claude --print --output-format stream-json --verbose -- "hello"` (real CLI output)
- Documented in `test/fixtures/README.md` with CLI version used

**Schema versioning policy (Minimum Spec-Guaranteed)**:

"Core" fields are ONLY those explicitly guaranteed by the SDK specification. Observed-but-unspecified fields are treated as optional even if present in every current fixture.

| Category | Decode Behavior | Rationale |
|----------|-----------------|-----------|
| Core fields (spec-required) | Fail if missing | Spec guarantees presence |
| Observed-but-not-specified | Optional (`Option` type) | May disappear in future CLI versions |
| Unknown JSON fields | Silently ignored | Forward compatibility |
| Unknown message `type` | `UnexpectedMessageError` | Forward compatibility |
| Unknown content block `type` | `UnknownBlock(raw)` | Forward compatibility |

**Core field criteria** (must satisfy ALL):
1. Explicitly documented as required in `plans/CLAUDE_AGENT_SDK_SPEC.md` (SPEC: Message Schema)
2. Semantically essential for SDK to function
3. Present in ALL known CLI versions (1.0.0+)

**Canonical decoder policy table (PROVISIONAL - requires Phase 1 verification)**:

This table is PROVISIONAL until Phase 1 verification confirms each field's required status in the spec.

| Message Type | Field | Expected Spec Heading | Provisional Status | Phase 1 Verified? |
|--------------|-------|----------------------|-------------------|-------------------|
| All messages | `type` | SPEC: Message Schema | Required | ☐ Pending |
| SystemMessage | `session_id` | SPEC: System Message | **Optional** (pending verification) | ☐ Pending |
| AssistantMessage | `message.content` | SPEC: Assistant Message | **Optional** (pending verification) | ☐ Pending |
| UserMessage | `message.content` | SPEC: User Message | **Optional** (pending verification) | ☐ Pending |
| ResultMessage | `subtype` | SPEC: Result Message | **Optional** (pending verification) | ☐ Pending |

**Default policy**: Until Phase 1 verifies a field as required, treat it as **Optional**. Only `type` is assumed required (needed for message dispatch).

**Decoder implementation rule**: Only fields verified as required in Phase 1 should cause decode failure if missing. All other fields MUST be `Option` types that default to `None` if absent.

**When in doubt, make it Optional**. Decode failure from missing required field is harder to recover from than handling `None`.

**Decode Failure Semantics (clarification)**:

How decode failures map to error types:

| Failure | Error Type | Message Produced? |
|---------|-----------|-------------------|
| Invalid JSON syntax | `JsonDecodeError` | No `MessageEnvelope` |
| Valid JSON, unknown `type` field | `UnexpectedMessageError` | No `MessageEnvelope` |
| Valid JSON, known `type`, missing required field | `JsonDecodeError` | No `MessageEnvelope` |
| Valid JSON, known `type`, all required fields present | None | `MessageEnvelope` produced |

**Key rule**: `MessageEnvelope.message` is ONLY populated when all required fields are present. Partial messages are never produced; any missing required field yields a decode error.

**Phase 1 GATING STEP: Spec Verification (REQUIRED before locking fixtures)**:

Before implementing decoders or creating fixtures, verify the "required field" claims against the actual spec:

| Step | Action | If Discrepancy Found |
|------|--------|---------------------|
| 1 | Open `plans/CLAUDE_AGENT_SDK_SPEC.md` | — |
| 2 | Search for each field in the "Required Fields" column above | — |
| 3 | Verify spec heading explicitly says the field is REQUIRED | If not explicit → downgrade to `Option(T)` |
| 4 | Document verification result in `test/fixtures/README.md` | Record spec section, version, date |

**Verification checklist** (implementer fills in during Phase 1):

| Field | Search for Heading | Found Heading | Verified Required? | Notes |
|-------|-------------------|---------------|-------------------|-------|
| `type` (all messages) | SPEC: Message Schema | ☐ TBD | ☐ | |
| `session_id` (System) | SPEC: System Message | ☐ TBD | ☐ | |
| `message.content` (Assistant) | SPEC: Assistant Message | ☐ TBD | ☐ | |
| `message.content` (User) | SPEC: User Message | ☐ TBD | ☐ | |
| `subtype` (Result) | SPEC: Result Message | ☐ TBD | ☐ | |

**If heading not found**: Search for alternative headings, document what was found, and update the Expected Spec Heading in the canonical table.

**If spec does not guarantee a field as required**:
1. Downgrade to `Option(T)` in decoder
2. Update decoder policy table above
3. Update synthetic fixtures to NOT include the field
4. Add minimal fixture test proving decoder works without the field

**Why this gate matters**: If decoders are too strict, integration tests will fail when CLI output doesn't match assumptions. Better to discover spec gaps early than debug decoder failures later.

**Fixture validation strategy**:
- Unit tests verify decoders against captured fixtures
- Each fixture file documents the CLI version it was captured from
- `unknown_message_type.json` fixture validates forward-compat handling
- `unknown_content_block.json` fixture validates `UnknownBlock` handling
- **Minimal fixtures**: Fixtures with ONLY core fields (no extras) prove decoders work without optional data
- **Negative fixtures**: Fixtures with missing core fields verify appropriate decode failure
- **Schema drift detection**: Compare fixture fields against spec to catch new fields that should be optional

#### Line Buffer Contract (SINGLE SOURCE OF TRUTH)

All line buffering, splitting, and handling is defined here. Other sections MUST reference this section.

**Byte-level processing pipeline**:

```
Port Data → Buffer → CRLF Normalize → Newline Split → UTF-8 Decode → JSON Parse → Message
```

| Stage | Operation | Spec |
|-------|-----------|------|
| 1. Buffer | Accumulate bytes | `BitArray`, grows until newline found |
| 2. Normalize | Convert CRLF to LF | Replace `\r\n` with `\n` (Windows compat) |
| 3. Split | Find newline byte | Split on `0x0A` (LF) at byte level |
| 4. UTF-8 | Decode line to string | `bit_array.to_string`, yield error if invalid |
| 5. JSON | Parse JSON | `gleam_json` decoder, yield error if invalid |
| 6. Message | Type dispatch | Route to appropriate message decoder |

**Buffer limits**:

| Limit | Value | Behavior on Exceed |
|-------|-------|-------------------|
| Max line size | 10 MB | `Error(BufferOverflow)` (terminal) |
| Max buffer memory | 10 MB | Same as max line (single line buffer) |

**Incomplete line handling** (when `exit_status` arrives):

| Buffer State | Exit Code | Result |
|--------------|-----------|--------|
| Empty | 0 | `WarningEvent(CleanExitNoResult)` |
| Empty | ≠0 | `ProcessError(code, ...)` |
| Has incomplete line | 0 | Process as final line (may be JsonDecodeError) |
| Has incomplete line | ≠0 | `ProcessError` (incomplete data discarded) |

**When this applies**:

| Phase | Uses Line Buffer | Notes |
|-------|-----------------|-------|
| Streaming (before Result) | ✅ Yes | Normal operation |
| Post-Result drain | ✅ Yes | Late messages buffered |
| Post-exit late-drain | ✅ Yes | Scheduler timing edge cases |
| Version detection | ❌ No | Reads until exit, no line parsing |

**CRLF normalization (cross-platform)**:

```gleam
fn normalize_crlf(buffer: BitArray) -> BitArray {
  // Run BEFORE splitting on newline
  buffer
  |> bit_array.to_string
  |> result.map(string.replace(_, "\r\n", "\n"))
  |> result.map(bit_array.from_string)
  |> result.unwrap(buffer)
}
```

#### Message Types

```gleam
/// Top-level message type - discriminated union of all message kinds
pub type Message {
  System(SystemMessage)
  Assistant(AssistantMessage)
  User(UserMessage)
  Result(ResultMessage)
}

/// Every message preserves raw JSON for forward compatibility
pub type MessageEnvelope {
  MessageEnvelope(
    message: Message,
    raw_json: String,       // Original JSON for forward compat / debugging (valid UTF-8)
    raw_bytes: BitArray,    // Original bytes (always present; enables invalid UTF-8 debugging)
  )
}

/// **raw_json preservation contract**:
///
/// The `raw_json` field contains the EXACT UTF-8 bytes for the line, NOT re-encoded JSON.
/// The `raw_bytes` field contains the original `BitArray` (always preserved).
///
/// **Byte-level processing (canonical)**:
/// 1. Buffer incoming data as `BitArray` (raw bytes)
/// 2. Split on newline byte (`0x0A`) at byte level (NOT String.split)
/// 3. For each complete line (bytes before newline):
///    a. **Always preserve `raw_bytes: BitArray`** (exact bytes, unconditional)
///    b. Attempt UTF-8 decode: `bit_array.to_string(line_bytes)`
///    c. If UTF-8 decode fails: yield `Utf8DecodeError` with `raw_bytes` (non-terminal)
///    d. If UTF-8 succeeds: store as `raw_json: String`
/// 4. Attempt JSON decode on `raw_json`
/// 5. Return MessageEnvelope(decoded_message, raw_json, raw_bytes)
///
/// **UTF-8 failure handling (explicit)**:
///
/// | Scenario | `raw_json` | `raw_bytes` | Error Yielded |
/// |----------|------------|-------------|---------------|
/// | Valid UTF-8 | Exact UTF-8 string | Exact bytes | None |
/// | Invalid UTF-8 | N/A (not set) | Exact bytes | `Utf8DecodeError(raw_bytes)` |
///
/// - Invalid UTF-8 is treated as non-terminal decode error (like JsonDecodeError)
/// - Contributes to consecutive decode error count (TooManyDecodeErrors threshold)
/// - **The `raw_bytes` field is ALWAYS populated** for debugging invalid UTF-8
/// - Stream continues to next line
///
/// **IMPORTANT: `raw_json: String` is only available for valid UTF-8**.
/// If the CLI ever emits non-UTF-8 bytes (unlikely but possible for binary tool output),
/// users must use `raw_bytes` for accurate debugging. The SDK does NOT perform lossy
/// UTF-8 conversion for `raw_json` — it either has the exact string or the line is
/// treated as a decode error with `raw_bytes` preserved.
///
/// **Why byte-level splitting**: Chunk boundaries from the port may split UTF-8 multi-byte
/// sequences. Buffering as bytes and splitting on `\n` (single byte) ensures we never
/// split a codepoint. UTF-8 decode happens per complete line, not per chunk.
///
/// **Why this matters**: Re-encoding would lose formatting, key order, and numeric precision.
/// The raw_json allows users to re-parse with different decoders or access fields the SDK
/// doesn't model.
///
/// **Acceptance criterion**: `raw_json` equals the fixture line byte-for-byte (for valid UTF-8).
/// `raw_bytes` equals the fixture line byte-for-byte (always).
```

**Message Field Classification**:

Fields are classified as **core** (stable, always present) or **extra** (may vary by CLI version):

```gleam
/// System initialization message (emitted once at session start)
pub type SystemMessage {
  SystemMessage(
    // --- Optional until SPEC verification (see Canonical Decoder Policy Table) ---
    session_id: Option(String),  // Pending verification: SPEC: Message Schema.1
    // --- Fields that SHOULD be present but spec doesn't guarantee ---
    // Marked Optional to avoid decode failures if CLI changes
    uuid: Option(String),
    cwd: Option(String),
    model: Option(String),
    tools: Option(List(String)),
    claude_code_version: Option(String),
    // --- Extra fields (observed but not in spec) ---
    mcp_servers: Option(List(McpServerStatus)),
    permission_mode: Option(String),
    api_key_source: Option(String),
    slash_commands: Option(List(String)),
    agents: Option(List(String)),
    output_style: Option(String),
    skills: Option(List(String)),
    plugins: Option(List(Dynamic)),
  )
}

// NOTE: Per Canonical Decoder Policy Table, only `type` is assumed required.
// All other fields (including session_id) are Optional until Phase 1 SPEC verification.
// This prevents decode failures if CLI output changes between versions.

/// Claude's response message
/// Per Canonical Decoder Policy Table: message.content is Optional (pending verification)
pub type AssistantMessage {
  AssistantMessage(
    // --- Optional until SPEC verification (see Canonical Decoder Policy Table) ---
    message: Option(AssistantMessageContent),  // Pending verification: SPEC: Message Schema.2
    // --- Optional per "when in doubt, Optional" rule ---
    uuid: Option(String),
    session_id: Option(String),
    parent_tool_use_id: Option(String),
  )
}

pub type AssistantMessageContent {
  AssistantMessageContent(
    // --- Optional until SPEC verification ---
    content: Option(List(ContentBlock)),  // Pending verification: SPEC: Message Schema.2
    // --- Optional: observed but not spec-guaranteed ---
    id: Option(String),
    role: Option(String),              // Usually "assistant"
    model: Option(String),
    stop_reason: Option(String),
    usage: Option(Usage),
  )
}

/// Content block types
pub type ContentBlock {
  TextBlock(text: String)
  ToolUseBlock(id: String, name: String, input: Dynamic)
  UnknownBlock(raw: Dynamic)  // Forward compat for new block types
}

/// Tool result message
/// Per Canonical Decoder Policy Table: message.content is Optional (pending verification)
pub type UserMessage {
  UserMessage(
    // --- Optional until SPEC verification (see Canonical Decoder Policy Table) ---
    message: Option(UserMessageContent),  // Pending verification: SPEC: Message Schema.3
    // --- Optional per "when in doubt, Optional" rule ---
    uuid: Option(String),
    session_id: Option(String),
    parent_tool_use_id: Option(String),
    tool_use_result: Option(Dynamic),
  )
}

pub type UserMessageContent {
  UserMessageContent(
    // --- Optional until SPEC verification ---
    content: Option(List(ToolResultBlock)),  // Pending verification: SPEC: Message Schema.3
    // --- Optional ---
    role: Option(String),              // Usually "user"
  )
}

pub type ToolResultBlock {
  ToolResultBlock(
    tool_use_id: String,
    content: String,
  )
}

/// Final result message
/// Per Canonical Decoder Policy Table: subtype is Optional (pending verification)
pub type ResultMessage {
  ResultMessage(
    // --- Optional until SPEC verification (see Canonical Decoder Policy Table) ---
    subtype: Option(ResultSubtype),    // Pending verification: SPEC: Message Schema.4
    // --- Optional per "when in doubt, Optional" rule ---
    uuid: Option(String),
    session_id: Option(String),
    is_error: Option(Bool),            // Defaults to False if absent
    result: Option(String),            // May be empty string
    duration_ms: Option(Int),
    duration_api_ms: Option(Int),
    num_turns: Option(Int),
    total_cost_usd: Option(Float),
    usage: Option(Usage),
    model_usage: Option(Dynamic),
    permission_denials: Option(List(PermissionDenial)),
    errors: Option(List(String)),
  )
}

pub type ResultSubtype {
  Success
  ErrorMaxTurns
  ErrorDuringExecution
  ErrorMaxBudget
  UnknownSubtype(String)
}

pub type Usage {
  Usage(
    input_tokens: Option(Int),
    output_tokens: Option(Int),
    cache_creation_input_tokens: Option(Int),
    cache_read_input_tokens: Option(Int),
  )
}

pub type McpServerStatus {
  McpServerStatus(name: String, status: String)
}

pub type PermissionDenial {
  PermissionDenial(
    tool_name: String,
    tool_use_id: String,
    tool_input: Dynamic,
  )
}
```

**JSON Decoding Strategy:**
- Use `gleam/dynamic` decoders with `optional_field` for extra fields
- Unknown fields in JSON objects are silently ignored
- Missing optional fields default to `None`
- Core fields that are missing cause `JsonDecodeError`
- Unknown message `type` values yield `UnexpectedMessageError` with `raw_json`
- Unknown content block types become `UnknownBlock(raw)`

### API/Interface Design

#### Primary Query Function and QueryStream

```gleam
/// Opaque stream type that encapsulates iteration and cleanup.
pub opaque type QueryStream

/// Execute a query against Claude Code CLI and stream messages.
///
/// **IMPORTANT**: The `next()` function blocks the calling process while
/// waiting for CLI output. For interactive applications, consider running
/// the stream consumption in a spawned OTP process.
///
/// Returns a QueryStream that yields MessageEnvelope values (message + raw JSON).
pub fn query(
  prompt: String,
  options: QueryOptions,
) -> Result(QueryStream, QueryError)
```

#### Stream Operations and Semantics

```gleam
/// Get next item from stream. Returns updated stream for continued iteration.
///
/// **Blocking**: This function blocks until data is available from the CLI.
///
/// ## CRITICAL: Always Use the Returned Stream
///
/// **You MUST always use the returned `QueryStream`, even when `next()` returns
/// an `Error`.** The returned stream contains updated internal state (buffers,
/// counters, etc.). Discarding it and reusing the old stream will cause
/// incorrect behavior.
///
/// ```gleam
/// // CORRECT: Always use the returned stream
/// let #(result, stream) = next(stream)
/// case result {
///   Error(JsonDecodeError(_)) -> next(stream)  // Use new stream
///   ...
/// }
///
/// // WRONG: Reusing old stream after error
/// let #(result, _new_stream) = next(stream)
/// case result {
///   Error(_) -> next(stream)  // BUG: should use _new_stream
///   ...
/// }
/// ```
///
/// ## QueryStream State Semantics (CANONICAL)
///
/// **Shared state (via port reference)**:
/// - Port handle (Erlang term) - same port across all copies
/// - Port closed status - when `port_close()` is called, it affects all copies
///
/// **Per-copy state (in QueryStream record)**:
/// - `buffer: BitArray` - accumulated bytes not yet forming a complete line
/// - `consecutive_decode_errors: Int` - counter for TooManyDecodeErrors threshold
/// - `state: StreamState` - current state machine state (Streaming/ResultReceived/etc.)
/// - `drain_count: Int` - iterations in post-Result drain
/// - `closed: Bool` - SDK-level closed flag
///
/// **Why "must use returned stream" matters**:
/// Using an old copy after calling `next()` is **undefined/unsupported** because:
/// - Old copy has stale buffer (may miss data or re-process bytes)
/// - Old copy has wrong decode error counter (may incorrectly trigger TooManyDecodeErrors)
/// - Old copy has wrong drain counter (may yield duplicate warnings or miss EndOfStream)
///
/// | Scenario | Behavior | Risk |
/// |----------|----------|------|
/// | `next(old_stream)` after `next(stream)` returned `new_stream` | Undefined | May skip/duplicate items |
/// | `close(any_copy)` | Closes port for ALL copies | Correct (port is shared) |
/// | `next(any_copy)` after `close()` | Returns `Ok(EndOfStream)` | Correct |
/// | Multiple concurrent `next()` on copies | Undefined | Data corruption |
///
/// **Unit test to demonstrate**:
/// ```gleam
/// pub fn stream_handle_sharing_test() {
///   let assert Ok(stream) = query("test", default_options())
///   let #(_, stream2) = next(stream)  // stream2 is "current"
///
///   // close() on any copy closes the port
///   close(stream)  // Closes underlying port
///
///   // All copies now return EndOfStream
///   let assert #(Ok(EndOfStream), _) = next(stream2)
/// }
/// ```
///
/// **Why this design**: Without linear types, preventing copy is impossible.
/// The safest behavior is for close() to be effective regardless of which
/// copy is used, ensuring no port leaks.
///
/// ## Return Value Classification
///
/// **Ok variants** (what the stream yields):
/// - `Ok(Message(envelope))` → Message received, call `next()` again
/// - `Ok(WarningEvent(warning))` → Non-fatal warning, call `next()` again
/// - `Ok(EndOfStream)` → Normal completion, stop iteration
///
/// **Error variants**:
/// - `Error(JsonDecodeError(_))` → Non-terminal, call `next()` again
/// - `Error(UnexpectedMessageError(_))` → Non-terminal, call `next()` again
/// - `Error(ProcessError(_))` → Terminal, stop iteration
/// - `Error(BufferOverflow)` → Terminal, stop iteration
/// - `Error(TooManyDecodeErrors(_))` → Terminal, stop iteration
///
/// ## Error Counter Behavior
///
/// The stream tracks `consecutive_decode_errors` internally:
/// - `JsonDecodeError` increments the counter
/// - Any successful `Ok(Message(_))` **resets the counter to 0**
/// - When counter reaches 5 → `TooManyDecodeErrors` (terminal)
///
/// This prevents cascading failures while allowing recovery from transient issues.
///
/// ## Blocking and Timeout Behavior (State-Dependent)
///
/// `next()` has **different timeout behavior depending on stream state**:
///
/// | Stream State | Timeout Behavior | Public Guarantee |
/// |--------------|------------------|------------------|
/// | `Streaming` (before Result) | Blocks indefinitely | User controls via `max_turns`/`max_budget` |
/// | `ResultReceived` (after Result) | **100ms timeout per call** | May return EndOfStream after timeout |
/// | `Closed` | Returns immediately | No I/O needed |
///
/// **Before Result**: `next()` blocks indefinitely until:
/// - Data arrives from the CLI
/// - The CLI exits (yielding EndOfStream or ProcessError)
/// - The port closes unexpectedly
///
/// **After Result**: `next()` uses 100ms timeout internally for drain. This means:
/// - If no more data arrives within 100ms, `next()` returns `Ok(EndOfStream)`
/// - This is intentional cleanup behavior, NOT a user-facing timeout
/// - Users iterating past Result may see EndOfStream after short delay
///
/// **If the CLI hangs** (before Result), `next()` will block forever. Mitigation:
/// 1. Use `max_turns` or `max_budget_usd` in QueryOptions to limit CLI execution
/// 2. Run stream consumption in a separate OTP process with monitoring
/// 3. Use `close()` from the SAME process to force termination (see below)
///
/// **Cancellation**: `close()` can be called from the same process that owns
/// the stream to force termination. However, due to port message ownership,
/// calling `close()` from a DIFFERENT process will NOT unblock `next()`.
///
/// **v2 consideration**: Add optional `read_timeout_ms` to QueryOptions that
/// wraps the receive loop with `after` clause.
///
/// The returned `QueryStream` is always safe to call `next()` on. After any
/// terminal condition, subsequent calls return `Ok(EndOfStream)`.
pub fn next(stream: QueryStream) -> #(Result(StreamItem, StreamError), QueryStream)

/// Force cleanup of stream's underlying port/process.
///
/// **Idempotent**: Safe to call multiple times; subsequent calls are no-ops.
/// **Bounded-blocking**: Closes port then drains mailbox (up to 50ms) to prevent cross-query pollution.
/// **Auto-called**: Called automatically on terminal errors and EndOfStream.
/// **Same-process**: MUST be called from the same process that called `query()`.
///
/// After `close()`, `next()` returns `Ok(EndOfStream)`.
pub fn close(stream: QueryStream) -> Nil

/// Execute a function with a stream, ensuring cleanup on completion or error.
/// This is the recommended way to safely consume streams.
///
/// **PROCESS OWNERSHIP**: Must consume stream in the same process that called `query()`.
/// Port messages are delivered to the spawning process only.
///
/// ```gleam
/// // CORRECT: Consume immediately in same process
/// with_stream(query("hello", opts), fn(stream) {
///   // Process stream... Port automatically closed when function returns
/// })
///
/// // WRONG: Do not store and pass to another process
/// let result = query("hello", opts)  // Result contains stream
/// spawn(fn() { with_stream(result, ...) })  // WRONG: different process!
/// ```
pub fn with_stream(
  result: Result(QueryStream, QueryError),
  f: fn(QueryStream) -> a,
) -> Result(a, QueryError)

/// Consume entire stream, collecting all items. Guarantees cleanup.
/// Returns CollectResult containing items, warnings, non-terminal errors, and optional terminal error.
///
/// **PROCESS OWNERSHIP**: Must call from same process that called `query()`.
///
/// **How errors are handled**:
/// - Non-terminal errors (JsonDecodeError, UnexpectedMessageError): Accumulated in `non_terminal_errors`
/// - Terminal errors (TooManyDecodeErrors, ProcessError): Stored in `terminal_error`, stops collection
/// - Warnings (WarningEvent): Accumulated in `warnings`
/// - Messages: Accumulated in `items`
///
/// See [Unified Stream Contract](#unified-stream-contract) for error classification.
pub fn collect_items(stream: QueryStream) -> CollectResult(StreamItem)

/// Collect only messages, ignoring warnings. Guarantees cleanup.
///
/// **PROCESS OWNERSHIP**: Must call from same process that called `query()`.
/// Filters StreamItem to just Message variants; warnings/non-terminal errors still tracked.
pub fn collect_messages(stream: QueryStream) -> CollectResult(MessageEnvelope)

/// Fold over stream with guaranteed cleanup. Preferred for custom processing.
/// The fold function receives each Result(StreamItem, StreamError) and accumulates.
/// Iteration stops on EndOfStream or terminal error.
///
/// **PROCESS OWNERSHIP**: Must call from same process that called `query()`.
pub fn fold_stream(
  stream: QueryStream,
  initial: acc,
  f: fn(acc, Result(StreamItem, StreamError)) -> acc,
) -> acc

/// Convert stream to standard iterator (for use with gleam/iterator).
///
/// ## PROCESS OWNERSHIP WARNING
///
/// **Must be consumed in the same process that called `query()`.** Port messages
/// are delivered to the spawning process only. Storing the iterator and consuming
/// from another process will result in hangs or silent no-op reads.
///
/// ## Resource Safety Warning
///
/// This function returns an iterator that does NOT automatically close the
/// underlying port. You MUST either:
/// 1. Consume the iterator fully (until it returns `Done`), OR
/// 2. Use `with_stream` to ensure cleanup on early termination
///
/// **Recommended alternatives**:
/// - `collect_messages()` — collects all messages, guaranteed cleanup
/// - `fold_stream()` — process messages with accumulator, guaranteed cleanup
/// - `with_stream()` — wrap any iteration pattern with guaranteed cleanup
///
/// This function is provided for interop with `gleam/iterator` combinators
/// but should be used carefully. Consider using the safer alternatives above.
pub fn to_iterator(stream: QueryStream) -> Iterator(Result(StreamItem, StreamError))
```

#### Unified Stream Contract

The stream uses a **single unified contract** where:
- `Ok(StreamItem)` represents all successful yields: messages, warnings, and end-of-stream
- `Error(StreamError)` represents all errors (terminal and non-terminal)

This avoids ambiguity about how end-of-stream and warnings are represented.

```gleam
/// What the stream can yield on success
pub type StreamItem {
  /// A parsed message from the CLI
  Message(MessageEnvelope)
  /// A non-fatal warning (stream continues)
  WarningEvent(Warning)
  /// Normal end of stream (terminal - no more items)
  EndOfStream
}

/// next() always returns Result(StreamItem, StreamError)
/// - Ok(Message(_)) → message received, call next() again
/// - Ok(WarningEvent(_)) → non-fatal warning, call next() again
/// - Ok(EndOfStream) → stream finished normally, stop iteration
/// - Error(e) where is_terminal(e) → stream finished with error, stop iteration
/// - Error(e) where !is_terminal(e) → non-fatal error, call next() again
```

**Single Yield Per Call Invariant (Critical)**:

Each `next()` call yields **exactly one result** (one `Ok(...)` or one `Error(...)`). The implementation MUST NOT:
- Yield multiple items in a single call (e.g., Result then Warning)
- Perform multi-step operations that yield intermediate results
- Buffer multiple items and return them in one call

This means:
1. When Result is received, `next()` returns `Ok(Message(Result(_)))` and transitions state to `ResultReceived`
2. **On the next `next()` call**, drain begins with bounded timeout
3. Each drain iteration yields one item (Warning, Error, or EndOfStream)

**Why this matters**: Single-yield semantics ensure:
- Predictable iteration patterns (one `next()` = one item)
- State machine transitions are explicit and testable
- No hidden batching that could mask ordering issues

**Authoritative State Transition Table (SINGLE SOURCE OF TRUTH)**:

All other sections referencing state transitions MUST defer to this table. Each row defines exactly ONE yield per `next()` call.

| Current State | Port Event | Yields THIS Call | Next State |
|---------------|-----------|------------------|------------|
| `Streaming` | Data (valid JSON message) | `Ok(Message(envelope))` | `Streaming` |
| `Streaming` | Data (Result message) | `Ok(Message(Result(_)))` | `ResultReceived` |
| `Streaming` | Data (invalid JSON) | `Error(JsonDecodeError(line, err))` | `Streaming` |
| `Streaming` | Data (valid JSON, unknown type) | `Error(UnexpectedMessageError(raw))` | `Streaming` |
| `Streaming` | ExitStatus, code≠0 | `Error(ProcessError(code, diag))` | `Closed` |
| `Streaming` | ExitStatus, code=0 | `Ok(WarningEvent(CleanExitNoResult))` | `PendingEndOfStream` |
| `Streaming` | Eof (unexpected) | `Ok(WarningEvent(CleanExitNoResult))` | `PendingEndOfStream` |
| `ResultReceived` | Data (any) | `Ok(WarningEvent(UnexpectedMessageAfterResult))` | `ResultReceived` |
| `ResultReceived` | ExitStatus, code=0 | `Ok(EndOfStream)` | `Closed` |
| `ResultReceived` | ExitStatus, code≠0 | `Ok(WarningEvent(NonZeroExitAfterResult(code)))` | `PendingEndOfStream` |
| `ResultReceived` | Drain timeout (100ms) | `Ok(EndOfStream)` | `Closed` |
| `ResultReceived` | Drain cap (10 msgs) | `Ok(EndOfStream)` | `Closed` |
| `PendingEndOfStream` | (next call) | `Ok(EndOfStream)` | `Closed` |
| `Closed` | Any | `Ok(EndOfStream)` | `Closed` |

**State definitions** (with explicit port status):
- `Streaming`: Normal operation, reading NDJSON lines. **Port: OPEN**
- `ResultReceived`: Result message yielded, now draining for exit_status. **Port: OPEN**
- `PendingEndOfStream`: Warning yielded, EndOfStream will be yielded on next call. **Port: OPEN** (NOT closed yet!)
- `Closed`: Terminal state, all calls return EndOfStream. **Port: CLOSED**

**When exactly does the port close?**
- Port closes **on the `next()` call that transitions TO `Closed`**, not before
- Example: Streaming → PendingEndOfStream yields Warning (port still open), then PendingEndOfStream → Closed yields EndOfStream (port closes NOW)
- The `ffi_close_port()` call happens inside `next()` BEFORE returning EndOfStream/ProcessError

**Port Close Side Effects (CRITICAL - ties state machine to resource cleanup)**:

| Transition TO | Side Effect | When Executed |
|--------------|-------------|---------------|
| `Closed` (from any state) | **`ffi_close_port()` called** | BEFORE returning EndOfStream/ProcessError |
| Any other transition | No side effect | N/A |

**Auto-close behavior (EXPLICIT)**:

When `next()` returns `Ok(EndOfStream)` or `Error(ProcessError(...))`, the implementation **MUST call `ffi_close_port()` before returning**. This is NOT optional and does NOT depend on helpers like `with_stream`.

```gleam
// Inside next() implementation - pseudocode
fn next(stream: QueryStream) -> #(Result(StreamItem, StreamError), QueryStream) {
  case stream.state {
    Closed -> #(Ok(EndOfStream), stream)
    _ -> {
      let #(result, new_state) = process_port_event(stream)
      case new_state {
        Closed -> {
          // MUST close port before returning
          ffi_close_port(stream.port)
          #(result, QueryStream(..stream, state: Closed))
        }
        other -> #(result, QueryStream(..stream, state: other))
      }
    }
  }
}
```

**Canonical rule**: `ffi_close_port()` is called **exactly once**, on the transition TO `Closed` state. This happens:
- On `ProcessError` (terminal error → Closed)
- On any `EndOfStream` return (drain timeout, drain cap, clean exit, etc.)
- On explicit `close()` call

**Resource safety guarantee**: If user iterates until `EndOfStream` or receives `ProcessError`, the port IS closed. No leak possible. Helpers like `with_stream` provide additional safety for early-return scenarios.

**close() semantics**:
- `close()` forces immediate transition to `Closed` (regardless of current state)
- If already `Closed`, no-op (idempotent)
- `close()` performs its own bounded mailbox drain before closing port
- After `close()`, all `next()` calls return `Ok(EndOfStream)`

**Key invariant**: `PendingEndOfStream` ensures single-yield semantics. When we need to yield Warning then EndOfStream, we yield Warning and transition to `PendingEndOfStream`. The NEXT `next()` call yields EndOfStream **and closes the port**.

#### StreamError Type

```gleam
/// Errors during stream iteration (does NOT include normal end-of-stream)
pub type StreamError {
  // --- Terminal errors (port closed, stream done) ---
  /// CLI process exited with non-zero code
  ProcessError(
    exit_code: Int,
    diagnostic: ErrorDiagnostic,  // Additional context for debugging
  )
  /// Single line exceeded 10MB buffer limit
  BufferOverflow
  /// Too many consecutive JSON decode failures (default: 5)
  TooManyDecodeErrors(count: Int, last_error: String)

  // --- Non-terminal errors (stream continues) ---
  /// Line was not valid JSON (continues until TooManyDecodeErrors threshold)
  JsonDecodeError(line: String, error: String)
  /// Valid JSON but unknown message type
  UnexpectedMessageError(raw_json: String)
}

/// Diagnostic context for ProcessError (since stderr is not captured in v1)
pub type ErrorDiagnostic {
  ErrorDiagnostic(
    /// Last non-JSON line seen on stdout (if any) — may contain error hints
    last_non_json_line: Option(String),
    /// True if stdout was completely empty (common for auth failures)
    stdout_was_empty: Bool,
    /// Common exit code interpretations
    exit_code_hint: String,
    /// Guidance for users
    troubleshooting: String,
  )
}

/// Generate diagnostic context based on exit code and stdout state.
///
/// **Empty stdout handling (P1 concern)**:
/// Many CLI failures (auth, network, config) emit ONLY stderr and exit non-zero,
/// producing no stdout lines. This is a common first-run failure mode.
/// When stdout is empty, we provide enhanced guidance.
fn diagnose_exit_code(exit_code: Int, stdout_was_empty: Bool) -> ErrorDiagnostic {
  let #(hint, troubleshooting) = case #(exit_code, stdout_was_empty) {
    // First-run auth failure: exit 1, no stdout (stderr has auth error)
    #(1, True) -> #(
      "Authentication required",
      "Authenticate the Claude CLI. The CLI produced no output, which typically means authentication failed. If running non-interactively (CI, daemon, container), ensure ANTHROPIC_API_KEY is set or redirect stderr for diagnostics: your_app 2>/tmp/stderr.log"
    )
    // Exit 1 with some output: more specific error in stdout
    #(1, False) -> #(
      "General error (check if CLI is authenticated)",
      "Run 'claude --version' to verify CLI. Stderr output (if any) appears in parent terminal."
    )
    // Invalid args: usually has stdout explaining which arg
    #(2, _) -> #(
      "Invalid arguments (check SDK version compatibility)",
      "The CLI rejected command arguments. Check that your SDK version matches your CLI version. Run 'claude --help' to verify supported flags."
    )
    #(126, _) -> #(
      "Permission denied (check file permissions)",
      "Check that the claude binary is executable and accessible."
    )
    #(127, _) -> #(
      "Command not found (CLI may have moved)",
      "The CLI was not found. Reinstall with: npm install -g @anthropic-ai/claude-code"
    )
    // Unknown error with empty stdout: likely stderr-only
    #(_, True) -> #(
      "CLI error (no output)",
      "The CLI exited with an error but produced no stdout. Stderr may contain details. For non-interactive environments, redirect stderr: your_app 2>/tmp/stderr.log"
    )
    #(_, False) -> #(
      "Unknown error",
      "Run 'claude --version' to verify CLI. Stderr output (if any) appears in parent terminal."
    )
  }
  ErrorDiagnostic(
    last_non_json_line: None,  // Populated during stream processing
    stdout_was_empty: stdout_was_empty,
    exit_code_hint: hint,
    troubleshooting: troubleshooting,
  )
}

/// Check if an error is terminal (stream closed, no more items)
pub fn is_terminal(error: StreamError) -> Bool {
  case error {
    ProcessError(_, _) | BufferOverflow | TooManyDecodeErrors(_, _) -> True
    JsonDecodeError(_, _) | UnexpectedMessageError(_) -> False
  }
}
```

#### Error Diagnostics (v1 Limitations)

**Stderr handling in v1**: Stderr is NOT captured by the SDK — it flows directly to the parent process's stderr (visible in terminal). This is intentional to:
- Keep NDJSON stdout stream clean
- Avoid pipe deadlock complexity
- Let CLI auth errors, warnings appear naturally

**Actionable error information**:
1. **Exit codes**: `ProcessError` includes `ErrorDiagnostic` with common exit code interpretations
2. **Last stdout line**: If any non-JSON text appears on stdout before failure, it's captured in `last_non_json_line`
3. **Troubleshooting guidance**: Built-in suggestions for common issues (auth, version, permissions)
4. **Terminal visibility**: Users see stderr output directly in their terminal

**Common failure scenarios and diagnostics**:
| Scenario | Exit Code | Stdout | Diagnostic Hint | User Action |
|----------|-----------|--------|-----------------|-------------|
| Not authenticated (first run) | 1 | Empty | "Authentication required" | Authenticate the CLI or set `ANTHROPIC_API_KEY` |
| Auth error with details | 1 | Has content | "check if CLI is authenticated" | Authenticate the CLI |
| Invalid flags | 2 | Has content | "check SDK version compatibility" | Update SDK or CLI |
| CLI moved/removed | 127 | Empty | "CLI may have moved" | Reinstall CLI |
| Permission denied | 126 | Empty | "check file permissions" | Check path/permissions |
| Unknown stderr-only error | * | Empty | "CLI error (no output)" | Redirect stderr for diagnostics |

**README documentation requirement**:

The SDK README must prominently document how to capture stderr when running non-interactively:

```markdown
## Troubleshooting

### Empty stdout errors in CI/containers

When running in non-interactive environments, stderr may not be visible.
To diagnose errors, redirect stderr to a file:

```bash
your_gleam_app 2>/tmp/claude_stderr.log
```

Common causes of empty stdout with non-zero exit:
- **Not authenticated**: Authenticate the CLI or set `ANTHROPIC_API_KEY`
- **Network issues**: Check connectivity to Anthropic API
- **Config errors**: Check `~/.claude/` configuration
```

**Stderr visibility across environments**:

The SDK relies on stderr being visible in the user's terminal for detailed CLI error messages. However, stderr may be unavailable in certain deployment contexts:

| Environment | Stderr Behavior | Troubleshooting |
|-------------|----------------|-----------------|
| Interactive terminal | Visible directly | Default case, no action needed |
| Daemon/service | May be lost or redirected to syslog | Redirect stderr to file: `my_app 2>/var/log/my_app.stderr` |
| CI/CD pipelines | Usually captured in job logs | Check job output for CLI messages |
| GUI applications | May be discarded | Redirect stderr before launching |
| Containers | Depends on logging driver | Check container logs |

**Troubleshooting note for ProcessError**:
The `ErrorDiagnostic.troubleshooting` field should include:
> "If running without an attached terminal (daemon, service, container), stderr output may not be visible. To capture stderr for debugging, rerun with stderr redirected to a file: `your_app 2>/tmp/stderr.log`"

**v2 enhancement**: Stderr capture via wrapper script for full diagnostic capture.

#### Warning Type

Warnings are delivered via `Ok(WarningEvent(warning))` in the stream (see Unified Stream Contract above).

```gleam
/// Non-fatal warning that can be yielded from the stream
pub type Warning {
  Warning(code: WarningCode, message: String, context: Option(String))
}

pub type WarningCode {
  UnparseableCliVersion              // CLI version string didn't match expected format
  CleanExitNoResult                  // CLI exited 0 but no Result message received
  NonZeroExitAfterResult(Int)        // Non-zero exit code after Result message (includes exit code)
  UnexpectedMessageAfterResult       // Data received after Result message
  DeprecatedOption                   // Reserved for future use
}
```

**Canonical Naming (Single Source of Truth)**:

All sections MUST use these exact names. No ad-hoc naming in diagrams/pseudocode.

| Category | Canonical Name | Description |
|----------|---------------|-------------|
| **StreamItem Variants** | | |
| Message | `Message(MessageEnvelope)` | Parsed message from CLI |
| Warning event | `WarningEvent(Warning)` | Non-fatal warning, stream continues |
| End of stream | `EndOfStream` | Stream completed (Result received or clean exit) |
| **WarningCode Variants** | | |
| No Result on clean exit | `CleanExitNoResult` | CLI exited 0 but no Result message |
| Non-zero after Result | `NonZeroExitAfterResult(Int)` | Non-zero exit AFTER Result (warning, not error) |
| Late message | `UnexpectedMessageAfterResult` | Data received after Result |
| Version parse fail | `UnparseableCliVersion` | CLI version string malformed |
| **StreamError Variants** | | |
| Process failure | `ProcessError(exit_code, diagnostic)` | CLI exited non-zero BEFORE Result |
| JSON parse error | `JsonDecodeError(line, error)` | Invalid JSON (non-terminal) |
| Unknown message | `UnexpectedMessageError(raw_json)` | Valid JSON, unknown type (non-terminal) |
| Buffer overflow | `BufferOverflow` | Single line > 10MB (terminal) |
| Decode cascade | `TooManyDecodeErrors(count, last_error)` | 5+ consecutive decode failures (terminal) |

**Name Normalization Rules**:
- Use `EndOfStream`, never `Eof`, `Done`, or `StreamEnd`
- Use `NonZeroExitAfterResult`, never `ExitCodeError` or `NonZeroExit`
- Use `CleanExitNoResult`, never just `NoResult` without "CleanExit" prefix

**Warning behavior**:
- Warnings are **non-terminal** — the stream continues after yielding a warning
- Callers can ignore warnings (pattern match only on `Message`)
- No external logging dependency required

**Example stream consumption**:
```gleam
case next(stream) {
  #(Ok(Message(envelope)), stream) -> handle_message(envelope, stream)
  #(Ok(WarningEvent(warning)), stream) -> {
    io.println_error("Warning: " <> warning.message)
    continue_stream(stream)  // Warnings are non-fatal
  }
  #(Ok(EndOfStream), _) -> done()
  #(Error(e), stream) if !is_terminal(e) -> continue_stream(stream)
  #(Error(e), _) -> handle_terminal_error(e)
}
```

#### Error Taxonomy (QueryError vs StreamError)

**When each error type occurs (CANONICAL)**:

| Error | Type | When | Notes |
|-------|------|------|-------|
| CLI not in PATH | `QueryError.CliNotFoundError` | In `query()`, before stream | Startup |
| Version too old | `QueryError.UnsupportedCliVersionError` | In `query()`, before stream | Startup |
| Version parse failed | `QueryError.UnknownVersionError` | In `query()`, before stream | Startup |
| Version detection timeout | `QueryError.VersionDetectionError` | In `query()`, before stream | Startup |
| Spawn failed | `QueryError.SpawnError` | In `query()`, before stream | Startup |
| Non-zero exit (before Result) | `StreamError.ProcessError` | In `next()`, after stream exists | Runtime |
| Invalid JSON line | `StreamError.JsonDecodeError` | In `next()`, after stream exists | Runtime |
| Unknown message type | `StreamError.UnexpectedMessageError` | In `next()`, after stream exists | Runtime |
| Buffer overflow | `StreamError.BufferOverflow` | In `next()`, after stream exists | Runtime |
| Too many decode errors | `StreamError.TooManyDecodeErrors` | In `next()`, after stream exists | Runtime |

**Rule**: If error occurs before `query()` returns `Ok(stream)`, it's a `QueryError`. If error occurs during `next()` calls on an existing stream, it's a `StreamError`.

**CLI invalid args (exit code 2)**: This manifests as `StreamError.ProcessError` during `next()`, NOT as a startup error, because the spawn succeeds and the error is detected when the CLI writes to stdout/exits.

#### QueryError Type (Startup Errors)

```gleam
/// Errors that prevent query from starting
pub type QueryError {
  /// Claude CLI not found in PATH
  CliNotFoundError(message: String)
  /// CLI version too old or incompatible
  UnsupportedCliVersionError(
    detected_version: String,  // e.g., "0.9.0"
    minimum_required: String,  // e.g., "1.0.0"
    suggestion: String,        // e.g., "Run: npm update -g @anthropic-ai/claude-code"
  )
  /// CLI version could not be parsed (fail-fast default behavior)
  UnknownVersionError(
    raw_output: String,        // Raw output from `claude --version`
    suggestion: String,        // "set permissive_version_check: True if known-compatible"
  )
  /// Version detection failed (timeout, spawn error, etc.)
  VersionDetectionError(
    reason: String,            // "timeout" or spawn error message
  )
  /// Failed to spawn process
  SpawnError(reason: String)
}

pub fn error_to_string(error: QueryError) -> String
pub fn stream_error_to_string(error: StreamError) -> String
```

#### Options Type

```gleam
pub type QueryOptions {
  QueryOptions(
    // --- CLI options ---
    model: Option(String),
    max_turns: Option(Int),
    max_budget_usd: Option(Float),
    system_prompt: Option(String),
    append_system_prompt: Option(String),
    allowed_tools: Option(List(String)),
    disallowed_tools: Option(List(String)),
    mcp_config_path: Option(String),
    permission_mode: Option(PermissionMode),
    resume_session_id: Option(String),
    continue_session: Bool,
    cwd: Option(String),  // See cwd handling rules below
    // --- SDK options (not passed to CLI) ---
    test_mode: Bool,                  // Enable test_runner path (default: False)
    test_runner: Option(TestRunner),  // Mock runner for unit tests (only used if test_mode=True)
    skip_version_check: Bool,         // Skip CLI version check entirely (default: False)
    permissive_version_check: Bool,   // Allow unknown versions with warning (default: False)
  )
}

/// **cwd handling rules (canonical)**:
///
/// | cwd value | Port option | Behavior |
/// |-----------|-------------|----------|
/// | `None` | Omit `{cd, ...}` | Inherit current working directory |
/// | `Some("")` | Treated as `None` | Empty string = inherit cwd |
/// | `Some(path)` | `{cd, path}` | Change to specified directory |
///
/// **Validation**:
/// - The SDK does NOT validate cwd existence before spawning
/// - Invalid/non-existent directories cause spawn to fail with `SpawnError`
/// - The Erlang port will return `{error, enoent}` or similar
///
/// **Why no pre-validation**:
/// - Race condition: directory could be deleted between check and spawn
/// - Cross-platform path validation is complex and error-prone
/// - Erlang already provides clear error messages
/// - Keeps SDK simple and delegates validation to OS
///
/// **Error surfacing**:
/// - `SpawnError(SpawnFailed(reason))` where `reason` contains OS error message

pub type PermissionMode {
  Default
  AcceptEdits
  BypassPermissions
  Plan
}

pub fn default_options() -> QueryOptions
pub fn with_model(options: QueryOptions, model: String) -> QueryOptions
pub fn with_max_turns(options: QueryOptions, n: Int) -> QueryOptions
pub fn with_max_budget(options: QueryOptions, usd: Float) -> QueryOptions
pub fn with_system_prompt(options: QueryOptions, prompt: String) -> QueryOptions
pub fn with_append_system_prompt(options: QueryOptions, prompt: String) -> QueryOptions
pub fn with_allowed_tools(options: QueryOptions, tools: List(String)) -> QueryOptions
pub fn with_disallowed_tools(options: QueryOptions, tools: List(String)) -> QueryOptions
pub fn with_mcp_config(options: QueryOptions, path: String) -> QueryOptions
pub fn with_permission_mode(options: QueryOptions, mode: PermissionMode) -> QueryOptions
pub fn with_resume(options: QueryOptions, session_id: String) -> QueryOptions
pub fn with_continue(options: QueryOptions) -> QueryOptions
pub fn with_cwd(options: QueryOptions, path: String) -> QueryOptions

// --- SDK options builders (v1) ---
// NOTE: with_runner() does NOT exist in v1 - use test_mode instead
pub fn with_test_mode(options: QueryOptions, runner: TestRunner) -> QueryOptions
pub fn with_skip_version_check(options: QueryOptions) -> QueryOptions
```

#### Session Management Lifecycle

Sessions enable multi-turn conversations across separate `query()` calls:

1. **New session**: Call `query()` without `resume_session_id` or `continue_session`. Extract `session_id` from `SystemMessage`.
2. **Resume by ID**: Use `with_resume(options, session_id)` to continue a specific session.
3. **Continue most recent**: Use `with_continue(options)` to resume the most recent session.

**If `session_id` is absent from SystemMessage**:
- Session resume is **best-effort** - the SDK cannot fabricate a session ID
- `SystemMessage.session_id` is `Option(String)` per the decoder policy (pending spec verification)
- If `None`, users cannot use that session ID for future `--resume` calls
- The query still succeeds; only the resume capability is unavailable
- **Public docs must state**: "Session resume requires `session_id` from `SystemMessage`. If the CLI omits this field, session resume is not possible for that query."

#### CLI Argument Mapping

Verified against SPEC: CLI Invocation and SPEC: Query Options. Source of truth: `claude --help` output.

| QueryOptions Field | CLI Argument(s) | Notes |
|-------------------|-----------------|-------|
| `model` | `--model <value>` | Model alias or full name |
| `max_turns` | `--max-turns <value>` | Integer |
| `max_budget_usd` | `--max-budget-usd <value>` | Float as string |
| `system_prompt` | `--system-prompt <value>` | Replaces default; takes precedence |
| `append_system_prompt` | `--append-system-prompt <value>` | Ignored if `system_prompt` set |
| `allowed_tools` | `--allowed-tools <tool1>,<tool2>` | Comma-separated, no spaces |
| `disallowed_tools` | `--disallowed-tools <tool1>,<tool2>` | Ignored if `allowed_tools` set |
| `mcp_config_path` | `--mcp-config <path>` | Path to JSON config file |
| `permission_mode = Default` | (no flag) | Default behavior |
| `permission_mode = AcceptEdits` | `--permission-mode acceptEdits` | camelCase per spec |
| `permission_mode = BypassPermissions` | `--dangerously-skip-permissions` | Different flag name |
| `permission_mode = Plan` | `--permission-mode plan` | lowercase |
| `resume_session_id` | `--resume <uuid>` | Takes precedence over `continue_session` |
| `continue_session = True` | `--continue` | Ignored if `resume_session_id` set |
| `cwd` | (port spawn `{cd, Path}` option) | Not a CLI flag |

**Always included:** `--print`, `--output-format stream-json`, `--verbose`, `--`, `<prompt>`

#### Option Precedence Rules (Consolidated)

When conflicting options are set, the SDK applies these precedence rules during CLI argument building:

| Conflict | Winner | Loser (ignored) | Behavior |
|----------|--------|-----------------|----------|
| `system_prompt` vs `append_system_prompt` | `system_prompt` | `append_system_prompt` | Only `--system-prompt` emitted |
| `allowed_tools` vs `disallowed_tools` | `allowed_tools` | `disallowed_tools` | Only `--allowed-tools` emitted |
| `resume_session_id` vs `continue_session` | `resume_session_id` | `continue_session` | Only `--resume` emitted |

**Implementation**: The `build_cli_args()` function checks these precedence rules explicitly:

```gleam
fn build_cli_args(options: QueryOptions, prompt: String) -> List(String) {
  let args = ["--print", "--output-format", "stream-json", "--verbose"]

  // Precedence: system_prompt > append_system_prompt
  let args = case options.system_prompt {
    Some(p) -> list.append(args, ["--system-prompt", p])
    None -> case options.append_system_prompt {
      Some(p) -> list.append(args, ["--append-system-prompt", p])
      None -> args
    }
  }

  // Precedence: allowed_tools > disallowed_tools
  let args = case options.allowed_tools {
    Some(tools) -> list.append(args, ["--allowed-tools", string.join(tools, ",")])
    None -> case options.disallowed_tools {
      Some(tools) -> list.append(args, ["--disallowed-tools", string.join(tools, ",")])
      None -> args
    }
  }

  // Precedence: resume_session_id > continue_session
  let args = case options.resume_session_id {
    Some(id) -> list.append(args, ["--resume", id])
    None -> case options.continue_session {
      True -> list.append(args, ["--continue"])
      False -> args
    }
  }

  // ... remaining options ...
  list.append(args, ["--", prompt])
}
```

**Acceptance criteria for precedence** (see Testing section):
- Given `system_prompt = Some("x")` and `append_system_prompt = Some("y")`, args contain `--system-prompt x` and NOT `--append-system-prompt`
- Given `allowed_tools = Some(["Read"])` and `disallowed_tools = Some(["Write"])`, args contain `--allowed-tools Read` and NOT `--disallowed-tools`
- Given `resume_session_id = Some("abc")` and `continue_session = True`, args contain `--resume abc` and NOT `--continue`

### Public Module Layout and Re-export Policy

**Public API surface** (stable, documented, users may import directly):

| Module | Public Exports | Description |
|--------|---------------|-------------|
| `claude_agent_sdk` | All re-exports | **Primary entry point** - import this for most usage |
| `claude_agent_sdk/message` | All message types | Import when type annotations are needed |
| `claude_agent_sdk/error` | Error types, `is_terminal()` | Import for error handling utilities |

**Internal modules** (unstable, may change without notice):

| Module | Why Internal |
|--------|--------------|
| `claude_agent_sdk/internal/*` | Implementation details (including port_ffi) |
| `claude_agent_sdk/json_decode` | Decoders are internal; users get decoded types |
| `claude_agent_sdk/runner` | `test_runner()` re-exported for unit tests; `erlang_runner()` internal in v1 |

**Re-export policy in main module**:

```gleam
// src/claude_agent_sdk.gleam

// Core query API
pub { query, QueryStream, next, close, with_stream, collect_items, collect_messages, fold_stream, to_iterator }

// Options
pub { QueryOptions, default_options, with_model, with_max_turns, with_max_budget, ... }
pub { PermissionMode }

// Message types (re-exported from message.gleam)
pub { Message, MessageEnvelope, SystemMessage, AssistantMessage, UserMessage, ResultMessage }
pub { ContentBlock, TextBlock, ToolUseBlock, UnknownBlock }
pub { ResultSubtype, Usage, McpServerStatus, PermissionDenial }

// Error types (re-exported from error.gleam)
pub { QueryError, StreamError, ErrorDiagnostic, is_terminal }
pub { StreamItem, Warning, WarningCode }

// Runner (only public constructors, not Handle type)
pub { test_runner }  // v1: only test_runner is public

// Version types (for advanced users)
pub { CliVersion }
```

**v1 Helper Functions - Stability Promise (AUTHORITATIVE)**:

| Helper | Stability | Category | Notes |
|--------|-----------|----------|-------|
| `with_stream()` | ✅ **Stable** | Resource safety | Guaranteed cleanup; primary recommended pattern |
| `collect_items()` | ✅ **Stable** | Resource safety | Returns `CollectResult`; iterates until EndOfStream |
| `collect_messages()` | ✅ **Stable** | Resource safety | Filters to messages; same semantics as collect_items |
| `fold_stream()` | ✅ **Stable** | Resource safety | Custom accumulation with guaranteed cleanup |
| `to_iterator()` | ⚠️ **Stable but Advanced** | Iterator interop | Documented leak risk; prefer alternatives above |

**Why these helpers are NOT "domain-specific"**: They provide resource safety (port cleanup) and iterator compatibility, not message content interpretation. They do NOT:
- Parse or interpret message content
- Make semantic decisions about success/failure
- Transform or aggregate message data

**Semver impact**: All helper functions above are part of the public API contract. Breaking changes require major version bump per standard semver.

**`to_iterator()` special note**: While stable, its documentation explicitly warns about leak risk and recommends alternatives. It exists for `gleam/iterator` combinator interop but should be used with care.

**Import recommendations** (for README/docs):

```gleam
// Standard usage - import main module only
import claude_agent_sdk.{query, default_options, with_model, Message}

// When you need message type annotations
import claude_agent_sdk/message.{SystemMessage, ResultMessage}

// When you need error handling utilities
import claude_agent_sdk/error.{is_terminal, ProcessError}
```

**Breaking change policy**:
- Changes to exports from `claude_agent_sdk` (main) → semver major bump
- Changes to exports from `claude_agent_sdk/message` → semver major bump
- Changes to exports from `claude_agent_sdk/error` → semver major bump
- Changes to `internal/*` modules → semver minor bump (no compatibility guarantee)

### File Impact Summary

| Path | Status | Visibility | Purpose |
|------|--------|------------|---------|
| `src/claude_agent_sdk.gleam` | Modify | **Public** | Main module, re-exports public API |
| `src/claude_agent_sdk/query.gleam` | **New** | **Public** via re-export | `query()`, `QueryStream`, `next()`, `close()`, `with_stream` |
| `src/claude_agent_sdk/options.gleam` | **New** | **Public** via re-export | `QueryOptions` type and builders |
| `src/claude_agent_sdk/message.gleam` | **New** | **Public** | All message types |
| `src/claude_agent_sdk/content.gleam` | **New** | **Public** via re-export | Content block types |
| `src/claude_agent_sdk/error.gleam` | **New** | **Public** | `QueryError`, `StreamError` types, `is_terminal()` |
| `src/claude_agent_sdk/json_decode.gleam` | **New** | Internal | JSON decoders for all types |
| `src/claude_agent_sdk/runner.gleam` | **New** | `test_runner()` only | `TestRunner` type for unit tests; `erlang_runner()` internal (v2 future) |
| `src/claude_agent_sdk/internal/cli.gleam` | **New** | Internal | CLI argument building, PATH lookup, version detection |
| `src/claude_agent_sdk/internal/constants.gleam` | **New** | Internal | Tunable limits (buffer size, thresholds) - single source of truth |
| `src/claude_agent_sdk/internal/confirmed_imports.gleam` | **New** | Internal | Phase 0 verified imports |
| `gleam.toml` | Modify | N/A | Add gleam_json, gleam_erlang deps (pinned versions) |
| `test/message_test.gleam` | **New** | Test | Message type parsing tests |
| `test/options_test.gleam` | **New** | Test | Options builder tests |
| `test/json_decode_test.gleam` | **New** | Test | JSON decoder tests |
| `test/cli_args_test.gleam` | **New** | Test | CLI argument building tests |
| `test/stream_test.gleam` | **New** | Test | Stream semantics using `test_runner()` |
| `test/query_integration_test.gleam` | **New** | Test | Integration tests (opt-in) |
| `test/fixtures/` | **New** | Test | JSON fixtures for schema tests (small); README with CLI version |
| `test/fixtures/cli_help.txt` | **New** | Test | Critical flags from `claude --help` for verification |

## Risks, Edge Cases & Breaking Changes

### Risks

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| CLI output format changes | Medium | High | Optional fields, `raw_json` preservation, `UnknownBlock` |
| Port/process resource leaks | Low | Medium | Opaque `QueryStream` with `with_stream` helper |
| Blocking on large outputs | Low | Low | Document blocking; suggest spawned OTP process |
| OTP 27 requirement limits adoption | Low | Medium | Document requirement clearly |
| Stderr not captured | Low | Low | `ErrorDiagnostic` provides exit code hints + troubleshooting; stderr visible in terminal; v2 can add capture |
| CLI flag mismatch | Low | Medium | CLI version detection with `UnsupportedCliVersionError`; fail-fast for unknown versions (opt-in permissive mode) |
| Older CLI versions incompatible | Medium | High | Minimum version check (1.0.0+) before spawn; actionable error message with upgrade instructions |

### Edge Cases

| Edge Case | Handling |
|-----------|----------|
| Empty response from Claude | Valid - may have only tool use, no text |
| Very long running queries | No timeout; user uses `max_turns` or `max_budget` |
| CLI not found | Return `Error(CliNotFoundError)` before spawn attempt |
| CLI version too old | Return `Error(UnsupportedCliVersionError)` with detected version and upgrade instructions |
| CLI version unparseable | Fail with `UnknownVersionError` by default; proceed with warning if `permissive_version_check: True` |
| CLI not authenticated | `ProcessError` with `ErrorDiagnostic` (exit code hint + troubleshooting); stderr visible in terminal |
| NDJSON line > 10MB | `BufferOverflow` terminal error, port closed |
| Malformed JSON line | `JsonDecodeError` non-terminal, stream continues |
| 5+ consecutive malformed lines | `TooManyDecodeErrors` terminal error (prevents cascading failures) |
| Unknown message type | `UnexpectedMessageError` non-terminal, stream continues |
| Unknown content block type | Decode as `UnknownBlock(raw)`, continue |
| Caller doesn't call close() | Port leaks until Gleam process exits; use `with_stream` |
| Gleam process crashes | Port auto-closes (linked) |
| next() after terminal error | Returns `EndOfStream` (safe, idempotent) |
| exit_status before Result message | `ProcessError` if non-zero; `EndOfStream` with warning if zero |
| exit_status after Result message | Warning only (see [Result vs exit_status precedence](#result-vs-exit_status-precedence-rule-critical)) |
| Eof without Result or exit_status | `EndOfStream` (unusual but not an error) |

### Backwards Compatibility
- N/A (greenfield project)
- Future: JS target may require Handle type changes (noted in code)
- Forward compatibility: Optional fields, `raw_json`, `UnknownBlock`, `UnknownSubtype`

## Testing & Validation Strategy

### Test Runner for Unit Tests

The `test_runner()` function enables testing stream semantics without OS processes:

```gleam
// Example: test stream behavior on malformed input
let mock_runner = test_runner(
  on_spawn: fn(_, _, _) { Ok(mock_handle) },
  on_read: fn(_) { Data(<<"{\"type\":\"system\"...}\n\"bad json\n">>) },
  on_close: fn(_) { Nil },
)
// Use mock_runner to test JsonDecodeError handling
```

### Integration Test Gating

Integration tests require real CLI and are **opt-in**:

**Gleam test framework**: The SDK uses `gleeunit` (standard Gleam test runner). Tests are discovered by:
- Files matching `test/*_test.gleam`
- Functions named `*_test()` that are public (`pub fn`)

**Integration test pattern** (skip when env var not set):

**Naming convention**: All integration tests use the prefix `integration__` to clearly identify them as opt-in tests that require external dependencies.

```gleam
// In test/query_integration_test.gleam
import support/env_helpers.{get_env}
import gleam/io
import gleeunit/should

/// Helper: Check if integration tests are enabled and log skip if not.
/// Returns True if tests should run, False if skipped.
fn integration_enabled(test_name: String) -> Bool {
  case get_env("CLAUDE_INTEGRATION_TEST") {
    Ok("1") -> True
    _ -> {
      io.println("[SKIP] " <> test_name <> " (set CLAUDE_INTEGRATION_TEST=1 to run)")
      False
    }
  }
}

/// PREFLIGHT CHECKS: Run before any actual integration tests.
/// Ensures environment can support tests; skips with actionable message if not.
pub fn integration__preflight_check_test() {
  case integration_enabled("integration__preflight_check_test") {
    True -> {
      // 1. CLI exists and version is parseable
      case find_executable("claude") {
        Error(_) -> {
          io.println("[SKIP:ENV] claude not in PATH - install CLI first")
          should.be_true(True)  // Skip, don't fail
          return
        }
        Ok(path) -> {
          // 2. Version check (with 5s timeout)
          case detect_cli_version_with_timeout(path, 5000) {
            Error(_) -> {
              io.println("[SKIP:ENV] claude --version timed out or failed")
              should.be_true(True)
              return
            }
            Ok(UnknownVersion(_)) -> {
              io.println("[WARN] Unparseable version - tests may fail")
            }
            Ok(CliVersion(maj, _, _, _)) if maj < 1 -> {
              io.println("[SKIP:ENV] CLI version < 1.0.0 - upgrade required")
              should.be_true(True)
              return
            }
            Ok(_) -> Nil  // Version OK
          }
        }
      }

      // 3. Required flags exist (non-network check)
      // Run: claude --help and verify --output-format, --print, --verbose present
      // If missing: skip with "[SKIP:ENV] Required flags missing"

      // 4. All preflights passed
      io.println("[PREFLIGHT] All checks passed - integration tests will run")
    }
    False -> should.be_true(True)
  }
}
```

**Integration test environment guidance**:

| Environment | Recommendation |
|-------------|----------------|
| Local dev (authenticated) | Set `CLAUDE_INTEGRATION_TEST=1` and run normally |
| CI (no auth) | Leave env var unset; tests skip cleanly |
| CI (with auth secret) | Set env var + `ANTHROPIC_API_KEY` secret |
| Read-only filesystem | Tests should still work (no file writes) |
| No network | Preflight will skip with actionable message |

**Skip hierarchy (recommended)**:

Integration tests should default to skipping unless ALL conditions are met:

1. `CLAUDE_INTEGRATION_TEST=1` environment variable is set
2. `claude` is found in PATH (non-network check)
3. `claude --version` succeeds with 5s timeout (non-network check)
4. For query tests ONLY: auth is available (API key or authenticated CLI session)

**Auth detection for query tests**:
```gleam
fn is_authenticated() -> Bool {
  // Check 1: ANTHROPIC_API_KEY in environment (preferred - no subprocess)
  case get_env("ANTHROPIC_API_KEY") {
    Ok(_) -> True
    _ ->
      // Check 2: `claude auth status` with 5s timeout (if CLI supports it)
      // Uses same timeout machinery as version detection
      case run_with_timeout("claude", ["auth", "status"], 5000) {
        Ok(output) if string.contains(output, "authenticated") -> True
        _ -> False  // Timeout, unsupported command, or not authenticated
      }
  }
}

pub fn integration__real_cli_query_test() {
  case integration_enabled(...) && is_authenticated() {
    True -> run_query_test()
    False -> skip_test("[SKIP:AUTH] Auth not available - set ANTHROPIC_API_KEY or authenticate the CLI")
  }
}
```

**Note**: If `claude auth status` is not supported by the CLI version, the command will fail and `is_authenticated()` returns False (graceful degradation). Users can always set `ANTHROPIC_API_KEY` to bypass CLI auth detection.

**Non-query preflights must be non-network**:
- `claude --version`: Reads local binary, no network
- `claude --help`: Reads local binary, no network
- These can run even without auth

**Timeout protection**: All integration tests use bounded timeouts internally (not exposed to SDK API):
- Preflight version check: 5s
- Preflight help check: 5s
- Query tests: 30s max (use `max_turns=1` for fast tests)
- No test can hang indefinitely

```gleam

/// Integration test: real CLI query (opt-in via CLAUDE_INTEGRATION_TEST=1)
pub fn integration__real_cli_query_test() {
  case integration_enabled("integration__real_cli_query_test") {
    True -> {
      // Actual integration test logic
      let assert Ok(stream) = query("Hello", default_options())
      // ... consume stream, verify messages
    }
    False -> should.be_true(True)  // Skipped (message already printed)
  }
}

/// Integration test: session resume (opt-in)
pub fn integration__session_resume_test() {
  case integration_enabled("integration__session_resume_test") {
    True -> {
      // Test session resume logic
    }
    False -> should.be_true(True)  // Skipped
  }
}

/// Integration test: NDJSON purity check
pub fn integration__ndjson_purity_test() {
  case integration_enabled("integration__ndjson_purity_test") {
    True -> {
      // Verify CLI produces pure NDJSON (see NDJSON Purity section above)
    }
    False -> should.be_true(True)
  }
}
```

**Test output visibility**:
- When `CLAUDE_INTEGRATION_TEST` is NOT set, each skipped test prints: `[SKIP] integration__<name> (set CLAUDE_INTEGRATION_TEST=1 to run)`
- This makes it clear how many integration tests were skipped and how to enable them
- The `integration__` prefix makes skipped tests easy to grep in output

**Running tests**:
```bash
# Unit tests only (integration tests print SKIP messages)
gleam test
# Output includes: [SKIP] integration__real_cli_query_test (set CLAUDE_INTEGRATION_TEST=1 to run)

# Include integration tests
CLAUDE_INTEGRATION_TEST=1 gleam test
```

**CI behavior**: Integration tests do NOT run in normal CI. Run manually or in dedicated integration CI job with authenticated CLI. The skip pattern ensures `gleam test` passes without the env var while making skipped tests visible in output.

### Acceptance Criteria (Behavioral)

| Criterion | Verification |
|-----------|--------------|
| **Message Decoding** | |
| Given fixture `system_message.json`, decoder yields `Ok(System(s))` with `s.session_id` matching | Unit test |
| Given fixture with unknown `type`, decoder yields `UnexpectedMessageError` with raw JSON | Unit test |
| Given fixture with unknown content block type, decoder yields `UnknownBlock(raw)` | Unit test |
| **CLI Argument Building** | |
| Given options with `model = Some("sonnet")`, CLI args include `--model sonnet` | Unit test |
| Given `system_prompt` AND `append_system_prompt` set, only `--system-prompt` emitted | Unit test |
| Given `allowed_tools` AND `disallowed_tools` set, only `--allowed-tools` emitted | Unit test |
| Given `resume_session_id` AND `continue_session=True`, only `--resume` emitted | Unit test |
| **Version Detection (Unit - pure functions)** | |
| `parse_version_string("claude v1.2.3\n")` returns `Ok(CliVersion(1,2,3,"1.2.3"))` | Unit test |
| `parse_version_string("1.2.3")` returns `Ok(CliVersion(1,2,3,"1.2.3"))` | Unit test |
| `parse_version_string("v1.2.3")` returns `Ok(CliVersion(1,2,3,"1.2.3"))` | Unit test |
| `parse_version_string("Claude Code CLI 1.2.3-beta.1")` returns `Ok(CliVersion(1,2,3,"1.2.3"))` | Unit test |
| `parse_version_string("  1.2.3  \n")` returns `Ok(CliVersion(1,2,3,"1.2.3"))` (whitespace tolerant) | Unit test |
| `parse_version_string("garbage")` returns `Error` | Unit test |
| `parse_version_string("")` returns `Error` | Unit test |
| `version_meets_minimum(v0.9.0, v1.0.0)` returns `False` | Unit test |
| `version_meets_minimum(v1.0.0, v1.0.0)` returns `True` | Unit test |
| `version_meets_minimum(v1.0.0-beta.1, v1.0.0)` returns `True` (prerelease passes) | Unit test |
| **Version Detection (Integration - process spawning)** | |
| Real `claude --version` returns parseable version | Integration test (opt-in) |
| Port preflight receives data from echo command | Integration test (opt-in) |
| Given CLI version < 1.0.0, `query()` returns `UnsupportedCliVersionError` | Integration test (opt-in) |
| **End-to-End Query (Integration - outcome-based)** | |
| `query()` with required flags produces at least one `System` message | Integration test (opt-in) |
| `query()` with unauthenticated CLI produces `ProcessError` with actionable diagnostic | Integration test (opt-in) |
| NDJSON purity: all stdout lines from `query()` parse as valid JSON | Integration test (opt-in) |
| **Startup Errors** | |
| Given `claude` not in PATH, `query()` returns `CliNotFoundError` | Integration test (requires PATH lookup) |
| `CliNotFoundError` variant exists and is constructable | Unit test (type-level, no FFI) |
| **Stream Behavior** | |
| Given mock runner yielding malformed line, `next()` returns `Error(JsonDecodeError(_))` and stream continues | Unit test |
| Given mock runner yielding 5+ consecutive malformed lines, `next()` returns `Error(TooManyDecodeErrors(_))` (terminal) | Unit test |
| Given mock runner yielding 15MB line, `next()` returns `Error(BufferOverflow)` (terminal) | Unit test |
| Given `close()` called, subsequent `next()` returns `Ok(EndOfStream)` | Unit test |
| Given mock runner yielding exit_status before Result, `next()` returns `Error(ProcessError(_))` with diagnostic | Unit test |
| Given mock runner yielding Result then exit_status=0, stream yields `Ok(Message(Result(_)))` then `Ok(EndOfStream)` | Unit test |
| Given mock runner yielding Result then exit_status!=0, stream yields Result, then `Ok(WarningEvent(NonZeroExitAfterResult(_)))`, then `Ok(EndOfStream)` | Unit test |
| Given mock runner yielding exit_status=0 before Result, stream yields `Ok(WarningEvent(CleanExitNoResult))` then `Ok(EndOfStream)` | Unit test |
| Given mock runner yielding warning, `next()` returns `Ok(WarningEvent(_))` and stream continues | Unit test |
| **Mailbox Isolation** | |
| Two sequential queries in the same process do not cross-contaminate stream events (even if first is closed early) | Integration test |
| **Resource Safety** | |
| Given `with_stream`, port is closed even on early return/panic | Unit test |
| Given `collect_items`, all items collected and port closed | Unit test |
| **Primary Integration (REQUIRED - behavioral)** | |
| `port_ffi.ffi_open_port()` spawns `/bin/echo test` and `receive_blocking()` receives `Data` then `ExitStatus(0)` | Phase 0 runtime test |
| `receive_port_msg_timeout()` returns `Timeout` after specified ms with no data | Phase 0 runtime test |
| `query()` with valid CLI produces at least `System` then `Result` messages | Integration test (opt-in) |
| **Secondary Hygiene (code-level invariants)** | |
| FFI module naming is verified at compile time | Unit test imports `claude_agent_sdk_ffi` functions (compile fails if wrong name) |
| No `_gleam` suffix in module paths | Static check + unit test verifying all @external use `claude_agent_sdk_ffi` |

**Code-level naming invariant test** (added to `test/ffi_naming_test.gleam`):

```gleam
// test/ffi_naming_test.gleam
// This test exists to catch FFI module naming errors at compile time.
// If the FFI module is named incorrectly, this file won't compile.

import claude_agent_sdk/internal/port_ffi

pub fn ffi_module_naming_invariant_test() {
  // These imports verify at compile time that:
  // 1. port_ffi.gleam exists at the correct path
  // 2. @external references use "claude_agent_sdk_ffi" (Erlang module name)
  // If any naming is wrong, this test file fails to compile.

  // Just reference the functions to ensure they exist
  let _open = port_ffi.ffi_open_port
  let _close = port_ffi.ffi_close_port

  // Test passes if we get here (compilation succeeded)
  True
}
```

**Developer checklist** (shell commands for quick verification):
- `find src -name "*_gleam*" -type f` → empty
- `grep -r "claude_agent_sdk_gleam" src/ test/` → empty

### Test Structure

```
test/
├── message_test.gleam          # Message type invariants
├── options_test.gleam          # Options builder behavior
├── json_decode_test.gleam      # JSON parsing with fixtures
├── cli_args_test.gleam         # CLI argument building
├── stream_test.gleam           # Stream semantics via test_runner()
├── query_integration_test.gleam # Real CLI tests (opt-in)
└── fixtures/
    ├── README.md                 # Documents CLI version used to capture fixtures
    ├── system_message.json
    ├── system_message_minimal.json
    ├── assistant_message.json
    ├── assistant_unknown_block.json  # Has unknown content block type
    ├── result_success.json
    ├── result_error.json
    ├── unknown_message_type.json # For forward-compat testing
    └── unknown_content_block.json # For UnknownBlock testing
```

**Large payload testing** (generated at runtime, NOT stored as fixtures):

To avoid bloating the repository, large payloads for buffer testing are generated at test runtime:

```gleam
// In test/stream_test.gleam

/// Generate a valid NDJSON message with specified content size
fn generate_large_message(size_bytes: Int) -> String {
  // Create valid JSON: {"type":"assistant","message":{"content":[{"type":"text","text":"AAA..."}]}}
  let padding = string.repeat("A", size_bytes - 100)  // Account for JSON structure
  json.object([
    #("type", json.string("assistant")),
    #("message", json.object([
      #("content", json.array([
        json.object([
          #("type", json.string("text")),
          #("text", json.string(padding))
        ])
      ]))
    ]))
  ])
  |> json.to_string
}

pub fn buffer_handles_1mb_message_test() {
  let large_json = generate_large_message(1_000_000)
  // Test with mock runner yielding this payload
}

pub fn buffer_overflow_at_10mb_test() {
  let huge_json = generate_large_message(11_000_000)
  // Test that BufferOverflow is returned
}
```

This keeps fixtures small (for schema/forward-compat testing only) while still testing large payload handling.

## Decisions Log

| Decision | Rationale |
|----------|-----------|
| Gleam 1.0+ / OTP 27+ | Stable API, required for gleam_erlang 1.3.0 |
| Opaque `Runner` with function records | Gleam has no traits; enables testing via `test_runner()` |
| Opaque `QueryStream` type | Ensures cleanup handle is accessible; prevents leaks |
| `--verbose` included unconditionally | Required (SPEC: CLI Invocation) for `stream-json`; no assumptions about its effect on schema |
| Stdout-only, no stderr merge | Keeps NDJSON clean; avoids corruption and deadlock |
| `stream` mode (not `{line, N}`) | Supports arbitrary-length NDJSON messages |
| 10MB buffer limit | Safety guard against unbounded memory; configurable if needed |
| Terminal vs non-terminal errors | Clear contract for stream behavior after errors |
| `with_stream` helper | Resource safety exception to "no helpers" non-goal |
| `test_runner()` for unit tests | Tests stream semantics without OS process mocking |
| Integration tests opt-in | Avoids CI failures when CLI unavailable |
| PATH lookup before spawn | Deterministic `CliNotFoundError` |
| `raw_json` in MessageEnvelope | Forward compat; users can access unparsed fields |
| `UnknownBlock` for new content types | Forward compat without decoder failures |
| CLI version detection | Proactive error with actionable message vs cryptic flag failures |
| Version check with opt-in permissive mode | Fail-fast by default; `permissive_version_check: True` proceeds with warning |
| `ErrorDiagnostic` in ProcessError | Actionable errors despite no stderr capture in v1 |
| `TooManyDecodeErrors` threshold (5) | Prevent cascading failures from masking real issues |
| Result message authoritative | Explicit state machine: Result seen → ignore subsequent exit_status |
| gleeunit with env-gated skips | Standard Gleam testing pattern; no custom harness needed |
| Handle as internal sum type | Enables test_runner() without exposing Handle to test code |
| test_runner() uses Dynamic | Tests pass their own state types; SDK wraps in TestHandle internally |
| Version detection separate from Runner | One-shot startup check doesn't need stream abstraction complexity |
| Structured Warning events in stream | No logging dependency; callers can programmatically handle warnings |
| collect_messages() and fold_stream() | Safe-by-default alternatives to to_iterator() with guaranteed cleanup |
| Include `use_stdio` in port options | OTP 27 requires explicit stdio connection for stdout piping |
| Unified StreamItem/StreamError contract | EndOfStream as Ok variant, errors only in StreamError; no ambiguity |
| collect_items/fold_stream in v1 scope | Resource safety helpers, not convenience functions; distinct rationale |
| Version detection reuses port config | Same options as query port ensures proven consistency |
| Consolidated precedence rules | Single source of truth for option conflicts; tested explicitly |
| Port read via `receive` (not actor) | Uses Erlang receive in calling process; "no actors" means no GenServers/supervision |
| Process ownership constraint | QueryStream must be used from process that called query(); documented limitation |
| Warning on non-zero exit after Result | Surfaces anomalies while preserving Result-first semantics |
| Stderr troubleshooting guidance | Explicit documentation for daemon/container environments where stderr may be lost |
| `use_stdio` required in port options | OTP 27: explicit piping required for stdout to arrive as port messages |
| Version detection unit vs integration split | Pure parsing functions unit-tested; process-spawning code integration-tested |
| Large fixtures generated at runtime | Avoids repository bloat; schema fixtures stay small |
| Error counter resets on success | Allows recovery from transient decode errors while preventing cascades |
| `stream` mode only in v1 | Handles arbitrary-length NDJSON; `{line,N}` truncates silently |
| No read timeout in v1 next() | Documented limitation with mitigation strategies; v2 can add optional timeout |
| test_runner() uses ETS with ref key | Concurrency-safe; process dictionary is flaky under parallel tests |

## Implementation Phases

### Phase 1: Foundation
- Define message types with core/extra classification
- Implement JSON decoders with forward compatibility
- Create test fixtures including large message test

### Phase 2: Core Query
- Implement `test_runner()` for unit test mocks (v1 test-only abstraction)
- Build QueryStream using port_ffi directly with line reassembly, buffer limit, terminal/non-terminal errors
- Wire up CLI argument building and PATH lookup

### Phase 3: Testing & Polish
- Unit tests via `test_runner()` for stream semantics
- Integration tests (opt-in) with real CLI
- Example code demonstrating `with_stream` usage

### Phase 4: Documentation
- Module documentation and README
- Document blocking behavior, integration test setup

## Open Questions (Resolved)

1. ~~**Hex package name**: `claude_agent_sdk` or `claude_agent_sdk_gleam`?~~
   **RESOLVED**: Package name is `claude_agent_sdk_gleam`. Module paths use `claude_agent_sdk/...` throughout. See [Naming & Namespace](#naming--namespace-canonical---single-source-of-truth) for the canonical naming specification.

2. **Example location**: `examples/` directory or inline in README?
   - Inline in README for v1 (simple usage)
   - Consider `examples/` directory if multiple examples needed later

## Next Steps

After this plan is approved, run `/create-tasks` to generate:
- `--beads` → Beads issues with dependencies for multi-agent execution
- (default) → TODO.md checklist for simpler tracking
