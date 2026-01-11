//// CLI argument building from QueryOptions.
//// Internal module - converts QueryOptions to List(String) for CLI invocation.

import claude_agent_sdk/options.{
  type PermissionMode, type QueryOptions, AcceptEdits, BypassPermissions, Plan,
}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string

/// Build CLI arguments from QueryOptions and prompt.
/// Fixed arguments are always included: --print --output-format stream-json --verbose
/// Prompt is added at the end after -- separator.
pub fn build_cli_args(options: QueryOptions, prompt: String) -> List(String) {
  // Start with fixed arguments
  let args = ["--print", "--output-format", "stream-json", "--verbose"]

  // Add optional model
  let args = case options.model {
    Some(m) -> list.append(args, ["--model", m])
    None -> args
  }

  // Add optional max_turns
  let args = case options.max_turns {
    Some(n) -> list.append(args, ["--max-turns", int.to_string(n)])
    None -> args
  }

  // Add optional max_budget_usd
  let args = case options.max_budget_usd {
    Some(usd) -> list.append(args, ["--max-budget-usd", float.to_string(usd)])
    None -> args
  }

  // Precedence: system_prompt > append_system_prompt
  let args = case options.system_prompt {
    Some(p) -> list.append(args, ["--system-prompt", p])
    None ->
      case options.append_system_prompt {
        Some(p) -> list.append(args, ["--append-system-prompt", p])
        None -> args
      }
  }

  // Precedence: allowed_tools > disallowed_tools
  let args = case options.allowed_tools {
    Some(tools) ->
      list.append(args, ["--allowed-tools", string.join(tools, ",")])
    None ->
      case options.disallowed_tools {
        Some(tools) ->
          list.append(args, ["--disallowed-tools", string.join(tools, ",")])
        None -> args
      }
  }

  // Add optional mcp_config_path
  let args = case options.mcp_config_path {
    Some(path) -> list.append(args, ["--mcp-config", path])
    None -> args
  }

  // Add optional permission_mode
  let args = case options.permission_mode {
    Some(mode) -> add_permission_mode(args, mode)
    None -> args
  }

  // Precedence: resume_session_id > continue_session
  let args = case options.resume_session_id {
    Some(id) -> list.append(args, ["--resume", id])
    None ->
      case options.continue_session {
        True -> list.append(args, ["--continue"])
        False -> args
      }
  }

  // Add prompt separator and quoted prompt at the end
  list.append(args, ["--", "\"" <> prompt <> "\""])
}

/// Add permission mode flags to argument list.
fn add_permission_mode(args: List(String), mode: PermissionMode) -> List(String) {
  case mode {
    AcceptEdits -> list.append(args, ["--permission-mode", "acceptEdits"])
    BypassPermissions -> list.append(args, ["--dangerously-skip-permissions"])
    Plan -> list.append(args, ["--permission-mode", "plan"])
    // Default permission mode - don't add any flag
    options.Default -> args
  }
}
