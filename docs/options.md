# Options and Configuration

`QueryOptions` configures how the SDK invokes the Claude Code CLI and how the
SDK behaves while streaming. Start from `default_options()` and chain builders.

```gleam
import claude_agent_sdk
import claude_agent_sdk/options

let opts = claude_agent_sdk.default_options()
  |> claude_agent_sdk.with_model("sonnet")
  |> claude_agent_sdk.with_max_turns(5)
  |> claude_agent_sdk.with_append_system_prompt("Be concise.")
  |> claude_agent_sdk.with_permission_mode(options.AcceptEdits)
```

## CLI options

These map to CLI flags:

- `with_model("model")` -> `--model`
- `with_max_turns(n)` -> `--max-turns`
- `with_max_budget(usd)` -> `--max-budget-usd`
- `with_system_prompt(text)` -> `--system-prompt`
- `with_append_system_prompt(text)` -> `--append-system-prompt`
- `with_allowed_tools([..])` -> `--allowed-tools`
- `with_disallowed_tools([..])` -> `--disallowed-tools`
- `with_mcp_config(path)` -> `--mcp-config`
- `with_permission_mode(mode)` -> `--permission-mode` or `--dangerously-skip-permissions`
- `with_resume(session_id)` -> `--resume`
- `with_continue()` -> `--continue`

### Precedence rules

Some options are mutually exclusive. The SDK follows these precedence rules:

- `system_prompt` overrides `append_system_prompt`
- `allowed_tools` overrides `disallowed_tools`
- `resume` overrides `continue`

## SDK behavior options

These options affect the SDK only:

- `with_test_mode(runner)` uses a mock runner instead of a real CLI process
- `with_skip_version_check()` disables the CLI version check
- `with_permissive_version_check()` allows unknown versions with a warning

## Permission modes

```gleam
import claude_agent_sdk/options

options.Default
options.AcceptEdits
options.BypassPermissions
options.Plan
```

- `Default`: no explicit flag; CLI prompts interactively
- `AcceptEdits`: `--permission-mode acceptEdits`
- `BypassPermissions`: `--dangerously-skip-permissions`
- `Plan`: `--permission-mode plan`

## Working directory

`with_cwd(path)` controls the CLI process working directory.

- `None` or `Some("")` -> inherit current working directory
- `Some(path)` -> run CLI with `{cd, path}`

## BidirOptions CLI mappings

`BidirOptions` fields translate to CLI flags for bidirectional sessions:

| Field | CLI Flag | Notes |
|-------|----------|-------|
| `agents` | `--agents <json>` or `--agents @<file>` | See agents serialization below |
| `plugins` | `--plugin <name>` (repeated) | One flag per plugin |
| `sandbox` | `--settings '{"sandbox":...}'` | Merged into settings JSON |
| `max_thinking_tokens` | `--max-thinking-tokens` | Integer token limit |
| `output_format` | `--output-format` | Output format string |
| `fork_session` | `--fork` | Fork from existing session ID |
| `setting_sources` | `--settings-sources` | Comma-separated source list |

### Agents serialization

The `agents` field serializes agent configs differently based on count:

- **≤3 agents**: Inline JSON in the `--agents` argument
- **>3 agents**: Written to a temp file, passed as `--agents @/tmp/agents-<id>.json`

The temp file uses cryptographically random naming and is cleaned up after spawn.

```gleam
import claude_agent_sdk/options

let agents = [
  options.AgentConfig(
    name: "reviewer",
    description: "Code review specialist",
    prompt: "You review code for best practices.",
  ),
]

let bidir_opts = claude_agent_sdk.bidir_options()
  |> options.bidir_with_agents(agents)
```

### Sandbox configuration

Sandbox config is merged into the CLI's `--settings` JSON under the `"sandbox"` key.
If other `--settings` flags exist (from `cli_options.settings` or `extra_args`),
they are merged with sandbox taking precedence on the `"sandbox"` key.

```gleam
import claude_agent_sdk/options
import gleam/dict

let sandbox = options.SandboxConfig(
  sandbox_type: "docker",
  config: dict.new(),
)

let bidir_opts = claude_agent_sdk.bidir_options()
  |> options.bidir_with_sandbox(sandbox)
```

### Plugins

Plugins are passed as repeated `--plugin` flags:

```gleam
let bidir_opts = claude_agent_sdk.bidir_options()
  |> options.bidir_with_plugins(["myplugin", "anotherplugin"])
// Produces: --plugin myplugin --plugin anotherplugin
```

## Extended CLI options

Additional `CliOptions` fields for advanced use:

| Field | CLI Flag | Notes |
|-------|----------|-------|
| `cli_path` | — | Custom path to claude executable |
| `fallback_model` | `--fallback-model` | Fallback model if primary unavailable |
| `betas` | `--beta` (repeated) | Beta feature flags |
| `permission_prompt_tool_name` | `--permission-prompt-tool-name` | Tool name for permission prompts |
| `settings` | `--settings` | Raw settings JSON dict |
| `add_dirs` | `--add-dir` (repeated) | Additional directories |
| `env` | — | Environment variables for CLI process |
| `extra_args` | — | Additional raw CLI arguments |

## Migration notes

### ThinkingBlock signature field

`ThinkingBlock` now includes an optional `signature` field:

```gleam
// Before (hypothetical)
content.ThinkingBlock(thinking)

// After
content.ThinkingBlock(thinking, signature)
// where signature: Option(String)
```

Users with exhaustive pattern matching on `ContentBlock` may need to update.

### is_partial field

`AssistantMessage` includes `is_partial: Bool` to indicate streaming state.
Defaults to `False` when not present in JSON. Use this to distinguish
partial streaming updates from complete messages.
