/// E2E Serial Tests for Environment-Dependent Error Scenarios (SDK-60).
///
/// These tests MUST run serially because they modify global environment variables:
/// - PATH (for CLI not found)
///
/// ## Why Serial?
/// Modifying environment variables affects all concurrent processes.
/// Running these tests in parallel would cause race conditions and flaky tests.
///
/// ## Running Tests
/// Run these tests in isolation to avoid race conditions with other E2E tests:
/// ```bash
/// gleam test -- --e2e
/// gleam test -- --only sdk_error_serial
/// ```
import claude_agent_sdk
import claude_agent_sdk/error
import e2e/helpers
import gleam/io
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import gleeunit/should

// ============================================================================
// FFI Bindings for Environment Manipulation
// ============================================================================

/// Get environment variable value.
/// Returns Ok(value) if set, Error(Nil) if not set.
@external(erlang, "e2e_helpers_ffi", "get_env")
fn get_env(name: String) -> Result(String, Nil)

/// Set environment variable.
@external(erlang, "e2e_helpers_ffi", "set_env")
fn set_env(name: String, value: String) -> Nil

/// Unset environment variable.
@external(erlang, "e2e_helpers_ffi", "unset_env")
fn unset_env(name: String) -> Nil

// ============================================================================
// Test Helpers
// ============================================================================

/// Save the current value of an environment variable.
fn save_env(name: String) -> Option(String) {
  case get_env(name) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

/// Restore an environment variable to its previous state.
fn restore_env(name: String, saved: Option(String)) -> Nil {
  case saved {
    Some(value) -> set_env(name, value)
    None -> unset_env(name)
  }
}

// ============================================================================
// SDK-60: CLI Not Found
// ============================================================================

/// SDK-60: Verify CliNotFoundError when PATH excludes claude.
///
/// This test modifies PATH to point to a non-existent directory,
/// which should cause query() to return CliNotFoundError.
pub fn sdk_60_cli_not_found_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_60_cli_not_found")

      let ctx = helpers.test_step(ctx, "save_env")
      let original_path = save_env("PATH")

      let ctx = helpers.test_step(ctx, "modify_path")
      set_env("PATH", "/nonexistent/path/only")

      let ctx = helpers.test_step(ctx, "execute_query")
      let result =
        claude_agent_sdk.query("test", claude_agent_sdk.default_options())

      // Restore PATH immediately (before any assertions that might panic)
      restore_env("PATH", original_path)

      let ctx = helpers.test_step(ctx, "validate_error")
      case result {
        Error(error.CliNotFoundError(msg)) -> {
          // Verify message contains helpful installation instructions
          msg
          |> string.contains("not found")
          |> should.be_true

          msg
          |> string.contains("npm install")
          |> should.be_true

          helpers.log_info_with(ctx, "cli_not_found_error", [
            #("message", json.string(msg)),
          ])
          helpers.log_test_complete(
            ctx,
            True,
            "CliNotFoundError with helpful message",
          )
        }
        Error(other_error) -> {
          helpers.log_error(
            ctx,
            "unexpected_error",
            error.error_to_string(other_error),
          )
          helpers.log_test_complete(ctx, False, "Expected CliNotFoundError")
          should.fail()
        }
        Ok(_stream) -> {
          helpers.log_error(
            ctx,
            "unexpected_success",
            "Query succeeded when should have failed",
          )
          helpers.log_test_complete(
            ctx,
            False,
            "Expected CliNotFoundError but query succeeded",
          )
          should.fail()
        }
      }
    }
  }
}
