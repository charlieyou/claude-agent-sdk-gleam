# End-to-End Testing with Real Claude CLI

This document explains how to run E2E tests that validate the SDK against the real Claude CLI.

## Prerequisites

1. **Claude CLI installed**: The `claude` command must be in your PATH
2. **Claude CLI authenticated**: Run `claude auth login` (or equivalent) so the CLI can respond non-interactively
3. **Opt-in enabled**: Pass the `--e2e` flag when running tests

## Running E2E Tests

```bash
# Run all E2E tests
gleam test -- --e2e

# Run specific test
gleam test -- --only real_session_with_hook_test -- --e2e
```

## Test Categories

### Real Session with Hook (`real_session_with_hook_test`)
Validates hook registration and invocation with real CLI:
- Starts a session with PreToolUse hook registered
- Sends a prompt that triggers tool use
- Verifies hook fires with tool context

### Real Control Operations (`real_control_operations_test`)
Validates control message handling:
- Sends `set_model` control request
- Verifies CLI responds (success or expected error)
- Confirms session continues normally

### Real Permission Callback (`real_permission_callback_test`)
Validates permission handling:
- Registers permission handler that denies Bash tool
- Sends prompt requesting bash execution
- Verifies tool is blocked, session continues

### Real Interrupt (`real_interrupt_test`)
Validates graceful interruption:
- Starts session with longer-running task
- Sends interrupt signal
- Verifies session stops gracefully

## How Skipping Works

Tests skip automatically when prerequisites are missing:

```
--e2e flag not set:
  [SKIP] --e2e flag not provided; skipping E2E test

--e2e provided but CLI not in PATH:
  [SKIP] Claude CLI not found in PATH
```

## CI Configuration

By default, CI should not run E2E tests. This is intentional since they require:
- Real API access (costs money, requires secrets)
- Claude CLI installation
- Network connectivity

For CI environments with Claude CLI available:

```yaml
# Example GitHub Actions
env:
  # Ensure CLI is authenticated in CI (e.g., via `claude auth login`)
  # Then run: gleam test -- --e2e
```

## Troubleshooting

### "Session failed to reach Running state"
- Check CLI version compatibility (`claude --version`)
- Verify API key is valid (`claude auth status`)
- Check network connectivity

### "Hook not invoked within timeout"
- The prompt may not trigger tool use
- CLI may have different tool use behavior
- Try with explicit tool request: `"Run echo hello"`

### "Permission handler not invoked"
- CLI may use different tool name casing
- Permission handlers registered for both `"Bash"` and `"bash"`
- Check CLI protocol version

### Timeout issues
- Tests use 30-second timeouts for hook invocation
- Real CLI operations may be slower than mocks
- Network latency affects response times

## Helper Functions

The `e2e/helpers` module provides:

```gleam
// Check if E2E tests should run
skip_if_no_e2e() -> Result(Nil, String)

// Check if CLI is available (env var + PATH check)
is_cli_available() -> Bool
```

## Example Test Pattern

```gleam
pub fn my_e2e_test() {
  case skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      case is_cli_available() {
        False -> {
          io.println("[SKIP] Claude CLI not found in PATH")
          Nil
        }
        True -> {
          // Run actual E2E test
          // ...
        }
      }
    }
  }
}
```
