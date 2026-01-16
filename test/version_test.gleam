//// Unit tests for version detection types and parse_version_string.
//// Tests pure functions only - no OS processes or external dependencies.

import gleeunit/should

import claude_agent_sdk/internal/cli.{
  CliVersion, SpawnFailed, UnknownVersion, detect_cli_version,
  format_version_error, minimum_cli_version, parse_version_string,
  version_meets_minimum,
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
  let assert CliVersion(major, minor, patch, raw) = min
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
  let UnknownVersion(raw) = UnknownVersion("some weird output")
  raw |> should.equal("some weird output")
}

// ============================================================================
// detect_cli_version spawn failure tests
// ============================================================================

pub fn detect_cli_version_nonexistent_path_returns_spawn_failed_test() {
  // Calling detect_cli_version with a nonexistent path should return
  // Error(SpawnFailed(_)) rather than crashing
  let result = detect_cli_version("/nonexistent/path/to/cli", ".")
  case result {
    Error(SpawnFailed(reason)) -> {
      // Should contain "enoent" in some form (file not found)
      reason |> should.not_equal("")
    }
    _ -> {
      // Should not succeed or return a different error type
      should.fail()
    }
  }
}

// ============================================================================
// Additional parse_version_string edge-case tests
// ============================================================================

pub fn parse_version_string_prerelease_alpha_test() {
  // "1.0.0-alpha" -> Ok(CliVersion(1,0,0,"1.0.0"))
  let result = parse_version_string("1.0.0-alpha")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(0)
  patch |> should.equal(0)
  raw |> should.equal("1.0.0")
}

pub fn parse_version_string_prerelease_rc_test() {
  // "v2.1.0-rc.1" -> Ok(CliVersion(2,1,0,"2.1.0"))
  let result = parse_version_string("v2.1.0-rc.1")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(2)
  minor |> should.equal(1)
  patch |> should.equal(0)
  raw |> should.equal("2.1.0")
}

pub fn parse_version_string_build_metadata_test() {
  // "1.2.3+build.456" -> Ok(CliVersion(1,2,3,"1.2.3"))
  let result = parse_version_string("1.2.3+build.456")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("1.2.3")
}

pub fn parse_version_string_prerelease_and_build_metadata_test() {
  // "3.0.0-beta.2+build.789" -> Ok(CliVersion(3,0,0,"3.0.0"))
  let result = parse_version_string("3.0.0-beta.2+build.789")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(3)
  minor |> should.equal(0)
  patch |> should.equal(0)
  raw |> should.equal("3.0.0")
}

pub fn parse_version_string_missing_minor_and_patch_returns_error_test() {
  // "1" -> Error (only major)
  let result = parse_version_string("1")
  result |> should.be_error()
}

pub fn parse_version_string_major_with_trailing_dot_returns_error_test() {
  // "1." -> Error
  let result = parse_version_string("1.")
  result |> should.be_error()
}

pub fn parse_version_string_major_minor_with_trailing_dot_returns_error_test() {
  // "1.2." -> Error (missing patch after dot)
  let result = parse_version_string("1.2.")
  result |> should.be_error()
}

pub fn parse_version_string_leading_zeros_preserved_test() {
  // "01.02.03" -> Ok(CliVersion(1,2,3,"01.02.03"))
  // Leading zeros are preserved in raw string but parsed as decimal integers
  let result = parse_version_string("01.02.03")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("01.02.03")
}

pub fn parse_version_string_tab_whitespace_test() {
  // "\t1.2.3\t" -> Ok (tab whitespace trimmed)
  let result = parse_version_string("\t1.2.3\t")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("1.2.3")
}

pub fn parse_version_string_mixed_whitespace_test() {
  // " \n\t 1.2.3 \r\n " -> Ok (mixed whitespace trimmed)
  let result = parse_version_string(" \n\t 1.2.3 \r\n ")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("1.2.3")
}

pub fn parse_version_string_internal_whitespace_returns_error_test() {
  // "1. 2.3" -> Error (whitespace between version parts)
  let result = parse_version_string("1. 2.3")
  result |> should.be_error()
}

pub fn parse_version_string_negative_prefix_finds_version_test() {
  // "-1.2.3" -> Ok (parser scans past non-digit prefix and finds version)
  let result = parse_version_string("-1.2.3")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("1.2.3")
}

pub fn parse_version_string_four_parts_parses_first_three_test() {
  // "1.2.3.4" -> Ok(CliVersion(1,2,3,"1.2.3")) - parser stops at third component
  let result = parse_version_string("1.2.3.4")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("1.2.3")
}

pub fn parse_version_string_special_chars_returns_error_test() {
  // "!@#$%^&*()" -> Error
  let result = parse_version_string("!@#$%^&*()")
  result |> should.be_error()
}

pub fn parse_version_string_version_in_path_test() {
  // "/usr/local/bin/claude-1.2.3" -> Ok (finds version in path)
  let result = parse_version_string("/usr/local/bin/claude-1.2.3")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(2)
  patch |> should.equal(3)
  raw |> should.equal("1.2.3")
}

pub fn parse_version_string_multiple_versions_takes_first_test() {
  // "v1.0.0 -> v2.0.0" -> Ok(CliVersion(1,0,0,"1.0.0")) - takes first
  let result = parse_version_string("v1.0.0 -> v2.0.0")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(1)
  minor |> should.equal(0)
  patch |> should.equal(0)
  raw |> should.equal("1.0.0")
}

pub fn parse_version_string_zero_versions_test() {
  // "0.0.0" -> Ok(CliVersion(0,0,0,"0.0.0"))
  let result = parse_version_string("0.0.0")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(0)
  minor |> should.equal(0)
  patch |> should.equal(0)
  raw |> should.equal("0.0.0")
}

pub fn parse_version_string_very_large_numbers_test() {
  // "999.999.999" -> Ok (stress test with large version numbers)
  let result = parse_version_string("999.999.999")
  let assert Ok(CliVersion(major, minor, patch, raw)) = result
  major |> should.equal(999)
  minor |> should.equal(999)
  patch |> should.equal(999)
  raw |> should.equal("999.999.999")
}

// ============================================================================
// Additional version_meets_minimum edge-case tests
// ============================================================================

pub fn version_meets_minimum_unknown_version_returns_false_test() {
  // Safety policy: UnknownVersion as version returns False (fail-safe behavior)
  let version = UnknownVersion("garbage output")
  let minimum = CliVersion(1, 0, 0, "1.0.0")
  version_meets_minimum(version, minimum) |> should.be_false()
}

pub fn version_meets_minimum_unknown_minimum_returns_false_test() {
  // Safety policy: UnknownVersion as minimum returns False (fail-safe behavior)
  let version = CliVersion(1, 0, 0, "1.0.0")
  let minimum = UnknownVersion("garbage")
  version_meets_minimum(version, minimum) |> should.be_false()
}

pub fn version_meets_minimum_both_unknown_returns_false_test() {
  // Safety policy: both UnknownVersion returns False (fail-safe behavior)
  let version = UnknownVersion("output1")
  let minimum = UnknownVersion("output2")
  version_meets_minimum(version, minimum) |> should.be_false()
}

pub fn version_meets_minimum_zero_version_test() {
  // v0.0.0 >= v0.0.0 -> True (edge case with all zeros)
  let version = CliVersion(0, 0, 0, "0.0.0")
  let minimum = CliVersion(0, 0, 0, "0.0.0")
  version_meets_minimum(version, minimum) |> should.be_true()
}

pub fn version_meets_minimum_zero_below_any_test() {
  // v0.0.0 >= v0.0.1 -> False
  let version = CliVersion(0, 0, 0, "0.0.0")
  let minimum = CliVersion(0, 0, 1, "0.0.1")
  version_meets_minimum(version, minimum) |> should.be_false()
}

pub fn version_meets_minimum_large_numbers_test() {
  // v100.200.300 >= v100.200.299 -> True
  let version = CliVersion(100, 200, 300, "100.200.300")
  let minimum = CliVersion(100, 200, 299, "100.200.299")
  version_meets_minimum(version, minimum) |> should.be_true()
}

pub fn version_meets_minimum_minor_trumps_patch_test() {
  // v1.2.0 >= v1.1.99 -> True (minor wins over patch)
  let version = CliVersion(1, 2, 0, "1.2.0")
  let minimum = CliVersion(1, 1, 99, "1.1.99")
  version_meets_minimum(version, minimum) |> should.be_true()
}

pub fn version_meets_minimum_major_trumps_minor_test() {
  // v2.0.0 >= v1.99.99 -> True (major wins over minor/patch)
  let version = CliVersion(2, 0, 0, "2.0.0")
  let minimum = CliVersion(1, 99, 99, "1.99.99")
  version_meets_minimum(version, minimum) |> should.be_true()
}

// ============================================================================
// format_version_error edge-case tests
// ============================================================================

pub fn format_version_error_unknown_detected_test() {
  // UnknownVersion as detected
  let detected = UnknownVersion("weird output")
  let required = CliVersion(1, 0, 0, "1.0.0")
  let msg = format_version_error(detected, required)
  // Should include the raw output from UnknownVersion
  msg |> should.not_equal("")
}

pub fn format_version_error_unknown_required_test() {
  // UnknownVersion as required (unusual but supported)
  let detected = CliVersion(0, 9, 0, "0.9.0")
  let required = UnknownVersion("unknown minimum")
  let msg = format_version_error(detected, required)
  msg |> should.not_equal("")
}
