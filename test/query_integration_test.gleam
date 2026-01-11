import gleam/io
import support/env_helpers.{get_env}

/// Helper: Check if integration tests are enabled and log skip if not.
/// Returns True if tests should run, False if skipped.
fn integration_enabled(test_name: String) -> Bool {
  case get_env("CLAUDE_INTEGRATION_TEST") {
    Ok("1") -> True
    _ -> {
      io.println(
        "[SKIP] " <> test_name <> " (set CLAUDE_INTEGRATION_TEST=1 to run)",
      )
      False
    }
  }
}
