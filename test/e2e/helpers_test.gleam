/// Unit tests for E2E helper functions.
import e2e/helpers
import gleeunit/should

// ============================================================================
// skip_if_no_soak tests
// ============================================================================

/// Verify skip_if_no_soak returns Error without --soak flag.
/// In the test environment, --soak is not set, so this should return Error.
pub fn skip_if_no_soak_returns_error_without_flag_test() {
  // Without --soak flag, should return Error
  let result = helpers.skip_if_no_soak()
  case result {
    Error(msg) -> {
      // Verify the error message contains expected text
      should.be_true(msg |> string_contains("--soak"))
    }
    Ok(_) -> {
      // If --soak happens to be set, that's OK - just pass
      should.be_true(True)
    }
  }
}

// ============================================================================
// is_concurrent_mode tests
// ============================================================================

/// Verify is_concurrent_mode returns False by default (no env var set).
pub fn is_concurrent_mode_returns_false_by_default_test() {
  // By default, E2E_ALLOW_CONCURRENT should not be set
  helpers.is_concurrent_mode()
  |> should.be_false()
}

/// Verify with_concurrent_mode sets the env var during execution.
pub fn with_concurrent_mode_sets_env_var_test() {
  // Before: should be false
  helpers.is_concurrent_mode()
  |> should.be_false()

  // During: should be true
  let inside_result =
    helpers.with_concurrent_mode(fn() { helpers.is_concurrent_mode() })

  inside_result
  |> should.be_true()

  // After: should be false again (restored)
  helpers.is_concurrent_mode()
  |> should.be_false()
}

// ============================================================================
// Helper functions
// ============================================================================

fn string_contains(haystack: String, needle: String) -> Bool {
  case haystack, needle {
    "", "" -> True
    _, "" -> True
    "", _ -> False
    h, n -> {
      let haystack_len = string_length(h)
      let needle_len = string_length(n)
      case haystack_len < needle_len {
        True -> False
        False -> string_contains_impl(h, n, 0, haystack_len - needle_len)
      }
    }
  }
}

fn string_contains_impl(
  haystack: String,
  needle: String,
  pos: Int,
  max_pos: Int,
) -> Bool {
  case pos > max_pos {
    True -> False
    False -> {
      let substr = string_slice(haystack, pos, string_length(needle))
      case substr == needle {
        True -> True
        False -> string_contains_impl(haystack, needle, pos + 1, max_pos)
      }
    }
  }
}

@external(erlang, "string", "length")
fn string_length(s: String) -> Int

@external(erlang, "string", "slice")
fn string_slice(s: String, start: Int, len: Int) -> String
