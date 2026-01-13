/// Tests for bidirectional mode CLI argument building.
import claude_agent_sdk/hook.{Continue}
import claude_agent_sdk/internal/cli
import claude_agent_sdk/options
import gleam/list
import gleeunit/should

// =============================================================================
// Helper functions
// =============================================================================

/// Assert that a list contains a specific element.
fn assert_contains(lst: List(String), elem: String) -> Nil {
  list.contains(lst, elem) |> should.be_true
}

/// Assert that a list does NOT contain a specific element.
fn assert_not_contains(lst: List(String), elem: String) -> Nil {
  list.contains(lst, elem) |> should.be_false
}

/// Find the index of an element in a list.
fn index_of(lst: List(String), elem: String) -> Result(Int, Nil) {
  find_index_helper(lst, elem, 0)
}

fn find_index_helper(
  lst: List(String),
  elem: String,
  idx: Int,
) -> Result(Int, Nil) {
  case lst {
    [] -> Error(Nil)
    [head, ..tail] ->
      case head == elem {
        True -> Ok(idx)
        False -> find_index_helper(tail, elem, idx + 1)
      }
  }
}

// =============================================================================
// Tests for minimum_bidir_cli_version constant
// =============================================================================

pub fn minimum_bidir_cli_version_exists_test() {
  // Verify the constant exists and has expected values
  case cli.minimum_bidir_cli_version {
    cli.CliVersion(major, minor, patch, raw) -> {
      major |> should.equal(1)
      minor |> should.equal(1)
      patch |> should.equal(0)
      raw |> should.equal("1.1.0")
    }
    _ -> should.fail()
  }
}

pub fn bidir_version_greater_than_minimum_test() {
  // Bidir version should be >= regular minimum
  cli.version_meets_minimum(
    cli.minimum_bidir_cli_version,
    cli.minimum_cli_version,
  )
  |> should.be_true
}

// =============================================================================
// Tests for has_bidir_features
// =============================================================================

pub fn has_bidir_features_default_options_test() {
  let opts = options.default_options()
  cli.has_bidir_features(opts) |> should.be_false
}

pub fn has_bidir_features_with_pre_tool_use_test() {
  let opts =
    options.QueryOptions(
      ..options.default_options(),
      on_pre_tool_use: option.Some(fn(_) { Continue }),
    )
  cli.has_bidir_features(opts) |> should.be_true
}

pub fn has_bidir_features_with_post_tool_use_test() {
  let opts =
    options.QueryOptions(
      ..options.default_options(),
      on_post_tool_use: option.Some(fn(_) { Continue }),
    )
  cli.has_bidir_features(opts) |> should.be_true
}

pub fn has_bidir_features_with_user_prompt_submit_test() {
  let opts =
    options.QueryOptions(
      ..options.default_options(),
      on_user_prompt_submit: option.Some(fn(_) { Continue }),
    )
  cli.has_bidir_features(opts) |> should.be_true
}

pub fn has_bidir_features_with_stop_test() {
  let opts =
    options.QueryOptions(
      ..options.default_options(),
      on_stop: option.Some(fn(_) { Continue }),
    )
  cli.has_bidir_features(opts) |> should.be_true
}

pub fn has_bidir_features_with_subagent_stop_test() {
  let opts =
    options.QueryOptions(
      ..options.default_options(),
      on_subagent_stop: option.Some(fn(_) { Continue }),
    )
  cli.has_bidir_features(opts) |> should.be_true
}

pub fn has_bidir_features_with_pre_compact_test() {
  let opts =
    options.QueryOptions(
      ..options.default_options(),
      on_pre_compact: option.Some(fn(_) { Continue }),
    )
  cli.has_bidir_features(opts) |> should.be_true
}

pub fn has_bidir_features_with_can_use_tool_test() {
  let opts =
    options.QueryOptions(
      ..options.default_options(),
      on_can_use_tool: option.Some(fn(_) { hook.Allow }),
    )
  cli.has_bidir_features(opts) |> should.be_true
}

pub fn has_bidir_features_with_model_only_test() {
  // Model is not a bidir feature
  let opts = options.default_options() |> options.with_model("opus")
  cli.has_bidir_features(opts) |> should.be_false
}

// =============================================================================
// Tests for build_bidir_cli_args
// =============================================================================

pub fn bidir_cli_args_includes_input_format_test() {
  let opts = options.default_options()
  let args = cli.build_bidir_cli_args(opts, "Hello Claude")

  // Verify --input-format stream-json is present
  assert_contains(args, "--input-format")
  assert_contains(args, "stream-json")

  // Verify correct order: --input-format followed by stream-json
  let input_idx = index_of(args, "--input-format")
  case input_idx {
    Ok(idx) -> {
      // Find second occurrence of stream-json (after --output-format)
      let stream_json_indices =
        list.index_map(args, fn(arg, i) { #(arg, i) })
        |> list.filter(fn(pair) { pair.0 == "stream-json" })
        |> list.map(fn(pair) { pair.1 })

      // Should have two stream-json values (output-format and input-format)
      list.length(stream_json_indices) |> should.equal(2)

      // --input-format should be immediately followed by stream-json
      list.contains(stream_json_indices, idx + 1) |> should.be_true
    }
    Error(Nil) -> should.fail()
  }
}

pub fn bidir_cli_args_includes_standard_args_test() {
  let opts = options.default_options()
  let args = cli.build_bidir_cli_args(opts, "test prompt")

  // Standard args should be present
  assert_contains(args, "--print")
  assert_contains(args, "--output-format")
  assert_contains(args, "--verbose")
  assert_contains(args, "--")
  assert_contains(args, "test prompt")
}

pub fn bidir_cli_args_with_model_test() {
  let opts = options.default_options() |> options.with_model("opus")
  let args = cli.build_bidir_cli_args(opts, "test prompt")

  assert_contains(args, "--model")
  assert_contains(args, "opus")
  // Bidir-specific flag should still be present
  assert_contains(args, "--input-format")
}

pub fn bidir_vs_standard_args_difference_test() {
  let opts = options.default_options()
  let standard_args = cli.build_cli_args(opts, "test prompt")
  let bidir_args = cli.build_bidir_cli_args(opts, "test prompt")

  // Standard args should NOT have --input-format
  assert_not_contains(standard_args, "--input-format")

  // Bidir args SHOULD have --input-format
  assert_contains(bidir_args, "--input-format")

  // Both should have --output-format
  assert_contains(standard_args, "--output-format")
  assert_contains(bidir_args, "--output-format")
}

// Need to import option for Some
import gleam/option
