/// Tests for timeout configuration options.
///
/// These tests verify that timeout_ms, hook_timeouts, and bidir_runner_factory
/// fields and builder functions work correctly.
import gleam/dict
import gleam/option
import gleeunit/should

import claude_agent_sdk.{
  default_options, with_bidir_runner_factory, with_hook_timeout, with_timeout,
}
import claude_agent_sdk/hook
import claude_agent_sdk/internal/bidir_runner

// =============================================================================
// Timeout Configuration Tests
// =============================================================================

/// Test with_timeout sets the global timeout_ms field.
pub fn with_timeout_sets_timeout_ms_test() {
  let options =
    default_options()
    |> with_timeout(30_000)

  should.equal(options.timeout_ms, option.Some(30_000))
}

/// Test default options have no timeout set.
pub fn default_options_no_timeout_test() {
  let options = default_options()

  should.equal(options.timeout_ms, option.None)
}

/// Test with_timeout can be chained with other options.
pub fn with_timeout_chainable_test() {
  let options =
    default_options()
    |> with_timeout(60_000)
    |> with_timeout(45_000)

  // Last one wins
  should.equal(options.timeout_ms, option.Some(45_000))
}

// =============================================================================
// Per-Hook Timeout Tests
// =============================================================================

/// Test with_hook_timeout adds an entry to hook_timeouts dict.
pub fn with_hook_timeout_sets_entry_test() {
  let options =
    default_options()
    |> with_hook_timeout(hook.PreToolUse, 5000)

  let expected = dict.from_list([#(hook.PreToolUse, 5000)])
  should.equal(options.hook_timeouts, expected)
}

/// Test multiple with_hook_timeout calls accumulate.
pub fn with_hook_timeout_accumulates_test() {
  let options =
    default_options()
    |> with_hook_timeout(hook.PreToolUse, 5000)
    |> with_hook_timeout(hook.PostToolUse, 10_000)
    |> with_hook_timeout(hook.Stop, 15_000)

  should.equal(dict.get(options.hook_timeouts, hook.PreToolUse), Ok(5000))
  should.equal(dict.get(options.hook_timeouts, hook.PostToolUse), Ok(10_000))
  should.equal(dict.get(options.hook_timeouts, hook.Stop), Ok(15_000))
}

/// Test with_hook_timeout overwrites previous value for same event.
pub fn with_hook_timeout_overwrites_same_event_test() {
  let options =
    default_options()
    |> with_hook_timeout(hook.PreToolUse, 5000)
    |> with_hook_timeout(hook.PreToolUse, 8000)

  should.equal(dict.get(options.hook_timeouts, hook.PreToolUse), Ok(8000))
  should.equal(dict.size(options.hook_timeouts), 1)
}

/// Test default options have empty hook_timeouts.
pub fn default_options_empty_hook_timeouts_test() {
  let options = default_options()

  should.equal(dict.size(options.hook_timeouts), 0)
}

/// Test all hook event types can be used with with_hook_timeout.
pub fn with_hook_timeout_all_event_types_test() {
  let options =
    default_options()
    |> with_hook_timeout(hook.PreToolUse, 1000)
    |> with_hook_timeout(hook.PostToolUse, 2000)
    |> with_hook_timeout(hook.UserPromptSubmit, 3000)
    |> with_hook_timeout(hook.Stop, 4000)
    |> with_hook_timeout(hook.SubagentStop, 5000)
    |> with_hook_timeout(hook.PreCompact, 6000)

  should.equal(dict.size(options.hook_timeouts), 6)
  should.equal(dict.get(options.hook_timeouts, hook.PreToolUse), Ok(1000))
  should.equal(dict.get(options.hook_timeouts, hook.PostToolUse), Ok(2000))
  should.equal(dict.get(options.hook_timeouts, hook.UserPromptSubmit), Ok(3000))
  should.equal(dict.get(options.hook_timeouts, hook.Stop), Ok(4000))
  should.equal(dict.get(options.hook_timeouts, hook.SubagentStop), Ok(5000))
  should.equal(dict.get(options.hook_timeouts, hook.PreCompact), Ok(6000))
}

// =============================================================================
// BidirRunner Factory Tests
// =============================================================================

/// Test with_bidir_runner_factory sets the factory function.
pub fn with_bidir_runner_factory_sets_factory_test() {
  // Create a mock factory
  let mock_factory = fn() {
    bidir_runner.mock(on_write: fn(_) { Ok(Nil) }, on_close: fn() { Nil })
  }

  let options =
    default_options()
    |> with_bidir_runner_factory(mock_factory)

  // Verify factory is set (Option is Some)
  case options.bidir_runner_factory {
    option.Some(factory) -> {
      // Call the factory and verify it returns a BidirRunner
      let _runner = factory()
      should.be_true(True)
    }
    option.None -> {
      should.fail()
    }
  }
}

/// Test default options have no bidir_runner_factory set.
pub fn default_options_no_bidir_runner_factory_test() {
  let options = default_options()

  should.equal(options.bidir_runner_factory, option.None)
}

/// Test that the factory can be invoked multiple times.
pub fn bidir_runner_factory_callable_test() {
  let mock_factory = fn() {
    bidir_runner.mock(on_write: fn(_) { Ok(Nil) }, on_close: fn() { Nil })
  }

  let options =
    default_options()
    |> with_bidir_runner_factory(mock_factory)

  case options.bidir_runner_factory {
    option.Some(factory) -> {
      // Call factory multiple times - each should succeed
      let _r1 = factory()
      let _r2 = factory()
      let _r3 = factory()
      should.be_true(True)
    }
    option.None -> {
      should.fail()
    }
  }
}

// =============================================================================
// Combined Options Tests
// =============================================================================

/// Test that timeout options can be combined with other query options.
pub fn timeout_options_combine_with_other_options_test() {
  let mock_factory = fn() {
    bidir_runner.mock(on_write: fn(_) { Ok(Nil) }, on_close: fn() { Nil })
  }

  let options =
    default_options()
    |> with_timeout(60_000)
    |> with_hook_timeout(hook.PreToolUse, 5000)
    |> with_bidir_runner_factory(mock_factory)

  // All three should be set
  should.equal(options.timeout_ms, option.Some(60_000))
  should.equal(dict.get(options.hook_timeouts, hook.PreToolUse), Ok(5000))

  case options.bidir_runner_factory {
    option.Some(_) -> should.be_true(True)
    option.None -> should.fail()
  }
}
