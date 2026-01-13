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
