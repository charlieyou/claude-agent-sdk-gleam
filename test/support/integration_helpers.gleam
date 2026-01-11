import gleam/io
import support/env_helpers.{get_env}

/// Check if integration tests should run for the given test name.
/// Returns True when CLAUDE_INTEGRATION_TEST=1, otherwise prints skip message and returns False.
pub fn integration_enabled(test_name: String) -> Bool {
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
