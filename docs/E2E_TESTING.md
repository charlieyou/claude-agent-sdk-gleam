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
gleam test -- --only bidir_e2e_test -- --e2e
```

## Cost Controls

E2E tests incur real API costs. Use these strategies to manage expenses:

| Strategy | How |
|----------|-----|
| Skip by default | Tests only run with `--e2e` flag |
| Run specific tests | Use `--only <test_name>` to run single tests |
| CI gating | Only run on labeled PRs or scheduled jobs |
| Monitor artifacts | Check `artifacts/e2e/` for API call patterns |

## Artifacts and Logging

### Artifact Location

Each E2E test produces a structured log file:

```
artifacts/e2e/<test-id>.log
```

For example: `artifacts/e2e/sdk_01_basic_query.log`

### Log Format

Logs are JSONL (one JSON object per line) with these fields:

```json
{
  "ts": "2026-01-13T22:45:12.123Z",
  "level": "STEP",
  "test_id": "sdk_01_basic_query",
  "step": 3,
  "elapsed_ms": 1542,
  "event": "stream_opened"
}
```

### Log Levels

| Level | Description |
|-------|-------------|
| `INFO` | Test lifecycle events (start, complete) |
| `STEP` | Test steps with numbered sequence |
| `ERROR` | Failures with error details |
| `DEBUG` | Verbose data (stream transcripts, payloads) |

### Viewing Artifacts

```bash
# View a specific test's log
cat artifacts/e2e/sdk_01_basic_query.log

# Pretty-print JSON lines
cat artifacts/e2e/sdk_01_basic_query.log | jq .

# Find errors across all tests
grep '"level":"ERROR"' artifacts/e2e/*.log | jq .
```

### Stdout Output

During test runs, human-readable output is printed to stdout:

```
[STEP] sdk_01_basic_query:1 session_started
[STEP] sdk_01_basic_query:2 prompt_sent
[STEP] sdk_01_basic_query:3 stream_opened
[INFO] sdk_01_basic_query:3 test_complete
```

## E2E Test Suites (Real CLI)

These tests use the real Claude CLI and require the `--e2e` flag:

### Bidir E2E Tests (`test/e2e/bidir_e2e_test.gleam`)

Validates bidirectional protocol with real CLI:
- Real session with hook registration and firing
- Control operations (set_model, interrupt)
- Permission callback handling
- Graceful interrupt during operation

### SDK Hooks Tests (`test/e2e/sdk_hooks_test.gleam`)

Validates hook registration and invocation:
- PreToolUse / PostToolUse hooks
- Hook parameter passing
- Hook timeout behavior

### SDK Message Tests (`test/e2e/sdk_message_test.gleam`)

Validates message streaming:
- System, Assistant, User, Result message types
- Message envelope parsing
- Stream termination handling

### SDK Options Tests (`test/e2e/sdk_options_test.gleam`)

Validates query options:
- Model selection
- System prompt configuration
- Max tokens and other limits

### SDK Error Serial Tests (`test/e2e/sdk_error_serial_test.gleam`)

Validates error conditions with real CLI:
- Authentication failures
- Invalid request handling

### SDK Stream Tests (`test/e2e/sdk_stream_test.gleam`)

Validates stream lifecycle:
- Stream creation and iteration
- Early termination (close)
- Timeout handling

### SDK MCP Tests (`test/e2e/sdk_mcp_test.gleam`)

Validates MCP server configuration:
- Server registration
- Tool discovery via MCP

## Offline Protocol Tests (Mock-Based)

These tests use mock runners for deterministic testing and run without `--e2e`:

### SDK Bidir Tests (`test/e2e/sdk_bidir_test.gleam`)

Tests bidirectional protocol logic with `mock_bidir_runner`:
- Session creation and state transitions
- Control message round-trips
- Error handling for invalid operations

### SDK Error Offline Tests (`test/e2e/sdk_error_offline_test.gleam`)

Tests error handling with `test_runner`:
- Network error simulation
- Malformed response handling

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

### GitHub Actions Example

```yaml
jobs:
  e2e:
    runs-on: ubuntu-latest
    if: contains(github.event.pull_request.labels.*.name, 'run-e2e')
    steps:
      - uses: actions/checkout@v4
      - name: Install Claude CLI
        run: npm install -g @anthropic-ai/claude-code
      - name: Authenticate
        run: claude auth login --api-key ${{ secrets.ANTHROPIC_API_KEY }}
      - name: Run E2E tests
        run: gleam test -- --e2e
      - name: Upload artifacts
        uses: actions/upload-artifact@v4
        with:
          name: e2e-logs
          path: artifacts/e2e/
          retention-days: 7
```

### Triggers

| Trigger | How to Use |
|---------|-----------|
| **Label** | Add `run-e2e` label to a PR |
| **Manual** | Use "Run workflow" button in Actions tab |
| **Schedule** | Automatic weekly run (Sundays 02:00 UTC) |

## Troubleshooting

### "Session failed to reach Running state"
- Check CLI version compatibility (`claude --version`)
- Verify API key is valid (`claude auth status`)
- Check network connectivity
- Review artifact log for detailed error

### "Hook not invoked within timeout"
- The prompt may not trigger tool use
- CLI may have different tool use behavior
- Try with explicit tool request: `"Run echo hello"`
- Check `DEBUG` level entries in artifact log

### "Permission handler not invoked"
- CLI may use different tool name casing
- Permission handlers registered for both `"Bash"` and `"bash"`
- Check CLI protocol version

### Timeout issues
- Tests use 30-second timeouts for hook invocation
- Real CLI operations may be slower than expected
- Network latency affects response times
- Consider running serial tests individually

### Debugging with Artifacts

1. Find the failing test's log:
   ```bash
   ls -la artifacts/e2e/
   ```

2. View the timeline:
   ```bash
   cat artifacts/e2e/<test-id>.log | jq -r '.ts + " " + .event'
   ```

3. Find the error:
   ```bash
   grep '"level":"ERROR"' artifacts/e2e/<test-id>.log | jq .
   ```

4. Check elapsed time for timeouts:
   ```bash
   cat artifacts/e2e/<test-id>.log | jq 'select(.elapsed_ms > 5000)'
   ```

## Helper Functions

The `test/e2e/helpers.gleam` module provides:

```gleam
// Check if E2E tests should run
skip_if_no_e2e() -> Result(Nil, String)

// Check if CLI is available (env var + PATH check)
is_cli_available() -> Bool

// Create test context with logging
new_test_context(test_id: String) -> TestContext

// Log a test step (auto-increments step counter)
test_step(ctx: TestContext, description: String) -> TestContext

// Log an error
log_error(ctx: TestContext, event: String, error_msg: String) -> Nil

// Log test completion
log_test_complete(ctx: TestContext, success: Bool, notes: String) -> Nil

// Query with timeout protection
query_and_consume_with_timeout(
  prompt: String,
  options: QueryOptions,
  timeout_ms: Int,
) -> QueryOutcome
```

## Example Test Pattern

```gleam
import claude_agent_sdk.{default_options}
import claude_agent_sdk/error
import e2e/helpers.{
  QueryFailure, QuerySuccess, QueryTimedOut,
  is_cli_available, log_error, log_test_complete,
  new_test_context, query_and_consume_with_timeout,
  skip_if_no_e2e, test_step,
}
import gleam/io

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
          let ctx = new_test_context("my_test")
          let ctx = test_step(ctx, "starting_session")

          // Run actual E2E test
          case query_and_consume_with_timeout("Hello", default_options(), 30_000) {
            QuerySuccess(result) -> {
              let ctx = test_step(ctx, "query_complete")
              log_test_complete(ctx, True, "Success")
            }
            QueryFailure(err) -> {
              log_error(ctx, "query_failed", error.error_to_string(err))
              log_test_complete(ctx, False, "Query failed")
            }
            QueryTimedOut -> {
              log_error(ctx, "query_timeout", "30s timeout exceeded")
              log_test_complete(ctx, False, "Timeout")
            }
          }
        }
      }
    }
  }
}
```
