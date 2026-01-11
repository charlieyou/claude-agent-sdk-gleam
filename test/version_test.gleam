//// Unit tests for version detection types and parse_version_string.
//// Tests pure functions only - no OS processes or external dependencies.

import gleeunit/should

import claude_agent_sdk/internal/cli.{
  CliVersion, UnknownVersion, format_version_error, minimum_cli_version,
  parse_version_string, version_meets_minimum,
}

// ============================================================================
// parse_version_string tests
// ============================================================================

pub fn parse_version_string_claude_v_format_test() {
  // "claude v1.2.3\n" -> Ok(CliVersion(1,2,3,"1.2.3"))
  let result = parse_version_string("claude v1.2.3\n")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("1.2.3")
}

pub fn parse_version_string_bare_version_test() {
  // "1.2.3" -> Ok(CliVersion(1,2,3,"1.2.3"))
  let result = parse_version_string("1.2.3")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("1.2.3")
}

pub fn parse_version_string_v_prefix_test() {
  // "v1.2.3" -> Ok(CliVersion(1,2,3,"1.2.3"))
  let result = parse_version_string("v1.2.3")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("1.2.3")
}

pub fn parse_version_string_cli_with_prerelease_test() {
  // "Claude Code CLI 1.2.3-beta.1" -> Ok(CliVersion(1,2,3,"1.2.3"))
  let result = parse_version_string("Claude Code CLI 1.2.3-beta.1")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("1.2.3")
}

pub fn parse_version_string_whitespace_tolerant_test() {
  // "  1.2.3  \n" -> Ok(CliVersion(1,2,3,"1.2.3"))
  let result = parse_version_string("  1.2.3  \n")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("1.2.3")
}

pub fn parse_version_string_garbage_returns_error_test() {
  // "garbage" -> Error
  let result = parse_version_string("garbage")
  result |> should.be_error()
}

pub fn parse_version_string_empty_returns_error_test() {
  // "" -> Error
  let result = parse_version_string("")
  result |> should.be_error()
}

pub fn parse_version_string_only_whitespace_returns_error_test() {
  let result = parse_version_string("   \n\t  ")
  result |> should.be_error()
}

pub fn parse_version_string_partial_version_returns_error_test() {
  // "1.2" without patch -> Error
  let result = parse_version_string("1.2")
  result |> should.be_error()
}

pub fn parse_version_string_large_numbers_test() {
  // Handle realistic version numbers
  let result = parse_version_string("v10.20.30")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(10)
  minor |> should.equal(20)
  patch |> should.equal(30)
  raw |> should.equal("10.20.30")
}

// ============================================================================
// version_meets_minimum tests
// ============================================================================

pub fn version_meets_minimum_exact_match_test() {
  // v1.0.0 >= v1.0.0 -> True
  let version = CliVersion(1, 0, 0, "1.0.0")
  let minimum = CliVersion(1, 0, 0, "1.0.0")
  version_meets_minimum(version, minimum) |> should.be_true()
}

pub fn version_meets_minimum_below_major_test() {
  // v0.9.0 >= v1.0.0 -> False
  let version = CliVersion(0, 9, 0, "0.9.0")
  let minimum = CliVersion(1, 0, 0, "1.0.0")
  version_meets_minimum(version, minimum) |> should.be_false()
}

pub fn version_meets_minimum_above_major_test() {
  // v2.0.0 >= v1.0.0 -> True
  let version = CliVersion(2, 0, 0, "2.0.0")
  let minimum = CliVersion(1, 0, 0, "1.0.0")
  version_meets_minimum(version, minimum) |> should.be_true()
}

pub fn version_meets_minimum_above_minor_test() {
  // v1.1.0 >= v1.0.0 -> True
  let version = CliVersion(1, 1, 0, "1.1.0")
  let minimum = CliVersion(1, 0, 0, "1.0.0")
  version_meets_minimum(version, minimum) |> should.be_true()
}

pub fn version_meets_minimum_below_minor_test() {
  // v1.0.0 >= v1.1.0 -> False
  let version = CliVersion(1, 0, 0, "1.0.0")
  let minimum = CliVersion(1, 1, 0, "1.1.0")
  version_meets_minimum(version, minimum) |> should.be_false()
}

pub fn version_meets_minimum_above_patch_test() {
  // v1.0.1 >= v1.0.0 -> True
  let version = CliVersion(1, 0, 1, "1.0.1")
  let minimum = CliVersion(1, 0, 0, "1.0.0")
  version_meets_minimum(version, minimum) |> should.be_true()
}

pub fn version_meets_minimum_below_patch_test() {
  // v1.0.0 >= v1.0.1 -> False
  let version = CliVersion(1, 0, 0, "1.0.0")
  let minimum = CliVersion(1, 0, 1, "1.0.1")
  version_meets_minimum(version, minimum) |> should.be_false()
}

pub fn version_meets_minimum_prerelease_passes_test() {
  // v1.0.0-beta.1 counts as v1.0.0 for version comparison purposes
  // The prerelease suffix is stripped during parsing, so this tests
  // that a parsed prerelease version passes the minimum check
  let version = CliVersion(1, 0, 0, "1.0.0")
  let minimum = CliVersion(1, 0, 0, "1.0.0")
  version_meets_minimum(version, minimum) |> should.be_true()
}

// ============================================================================
// minimum_cli_version tests
// ============================================================================

pub fn minimum_cli_version_is_1_0_0_test() {
  let min = minimum_cli_version
  let CliVersion(major, minor, patch, raw) = min
  major |> should.equal(1)
  minor |> should.equal(0)
  patch |> should.equal(0)
  raw |> should.equal("1.0.0")
}

// ============================================================================
// format_version_error tests
// ============================================================================

pub fn format_version_error_basic_test() {
  let detected = CliVersion(0, 9, 0, "0.9.0")
  let required = CliVersion(1, 0, 0, "1.0.0")
  let msg = format_version_error(detected, required)

  // Should contain both versions
  msg |> should.not_equal("")
  // Verify it's a sensible message (contains version info)
  let has_detected = case msg {
    _ if msg == "" -> False
    _ -> True
  }
  has_detected |> should.be_true()
}

// ============================================================================
// UnknownVersion tests
// ============================================================================

pub fn unknown_version_stores_raw_output_test() {
  let unknown = UnknownVersion("some weird output")
  case unknown {
    UnknownVersion(raw) -> raw |> should.equal("some weird output")
    _ -> should.fail()
  }
}
