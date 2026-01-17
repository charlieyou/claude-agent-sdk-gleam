//// Changelog hygiene tests
////
//// Tests to enforce changelog presence, format, and version ordering.
//// Ensures CHANGELOG.md follows project conventions.

import gleam/int
import gleam/list
import gleam/result
import gleam/string
import gleeunit/should
import simplifile

// ============================================================================
// Test constants
// ============================================================================

const changelog_path = "CHANGELOG.md"

// ============================================================================
// Helpers
// ============================================================================

/// Parse a version tuple from a version string like "1.2.3"
fn parse_version(version_str: String) -> Result(#(Int, Int, Int), Nil) {
  case string.split(version_str, ".") {
    [major_str, minor_str, patch_str] -> {
      use major <- result.try(int.parse(major_str))
      use minor <- result.try(int.parse(minor_str))
      use patch <- result.try(int.parse(patch_str))
      Ok(#(major, minor, patch))
    }
    _ -> Error(Nil)
  }
}

/// Compare two version tuples (True if first > second)
fn version_greater(a: #(Int, Int, Int), b: #(Int, Int, Int)) -> Bool {
  case a, b {
    #(ma, _, _), #(mb, _, _) if ma > mb -> True
    #(ma, _, _), #(mb, _, _) if ma < mb -> False
    #(_, mia, _), #(_, mib, _) if mia > mib -> True
    #(_, mia, _), #(_, mib, _) if mia < mib -> False
    #(_, _, pa), #(_, _, pb) -> pa > pb
  }
}

/// Extract version string from a line like "## 0.1.0" or "## 0.1.0 (2026-01-17)"
fn extract_version_from_line(line: String) -> Result(String, Nil) {
  case string.starts_with(line, "## ") {
    False -> Error(Nil)
    True -> {
      let rest = string.drop_start(line, 3)
      // Take just the version part (stop at space or end)
      let version_part = case string.split_once(rest, " ") {
        Ok(#(version, _)) -> version
        Error(_) -> rest
      }
      // Validate it looks like a version
      case parse_version(version_part) {
        Ok(_) -> Ok(version_part)
        Error(_) -> Error(Nil)
      }
    }
  }
}

/// Check if a line is a bullet point (starts with "- ")
fn is_bullet_point(line: String) -> Bool {
  string.starts_with(line, "- ")
}

/// Check if a line is an empty bullet point (just "- " or "-")
fn is_empty_bullet_point(line: String) -> Bool {
  let trimmed = string.trim(line)
  trimmed == "-" || trimmed == "- "
}

/// Check if a line looks like a version header but has malformed format.
/// Returns True for lines that:
/// - Start with "##" (with or without space) followed by something version-like
/// - But fail to parse as valid X.Y.Z semver
fn is_malformed_version_header(line: String) -> Bool {
  // Skip valid headers
  case extract_version_from_line(line) {
    Ok(_) -> False
    Error(_) -> {
      // Check if it looks like it's trying to be a version header
      let trimmed = string.trim(line)
      case string.starts_with(trimmed, "##") {
        False -> False
        True -> {
          // Get text after "##" (with or without space)
          let after_hashes =
            trimmed
            |> string.drop_start(2)
            |> string.trim_start()
          // Check if it looks version-like (starts with digit)
          case string.to_graphemes(after_hashes) {
            [] -> False
            [first, ..] -> {
              // If first char is a digit, this looks like a version header
              case int.parse(first) {
                Ok(_) -> True
                Error(_) -> False
              }
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// Tests
// ============================================================================

pub fn changelog_exists_test() {
  simplifile.is_file(changelog_path)
  |> should.equal(Ok(True))
}

pub fn changelog_starts_with_header_test() {
  let assert Ok(content) = simplifile.read(changelog_path)
  content
  |> string.starts_with("# Changelog")
  |> should.be_true()
}

pub fn changelog_has_valid_version_format_test() {
  let assert Ok(content) = simplifile.read(changelog_path)
  let lines = string.split(content, "\n")

  // Find all version lines
  let versions =
    lines
    |> list.filter_map(fn(line) { extract_version_from_line(line) })

  // Should have at least one version
  list.length(versions)
  |> should.not_equal(0)

  // All version lines should parse successfully (already validated by extract_version_from_line)
  Nil
}

pub fn changelog_has_bullet_points_test() {
  let assert Ok(content) = simplifile.read(changelog_path)
  let lines = string.split(content, "\n")

  // Check that each version section has at least one bullet point
  check_sections_have_bullets(lines, False, False)
}

fn check_sections_have_bullets(
  lines: List(String),
  in_section: Bool,
  has_bullets: Bool,
) -> Nil {
  case lines {
    [] -> {
      // End of file - if we were in a section, it should have bullets
      case in_section && !has_bullets {
        True -> should.fail()
        False -> Nil
      }
    }
    [line, ..rest] -> {
      case extract_version_from_line(line) {
        Ok(_) -> {
          // New version section
          // If we were in a section, check it had bullets
          case in_section && !has_bullets {
            True -> should.fail()
            False -> check_sections_have_bullets(rest, True, False)
          }
        }
        Error(_) -> {
          // Not a version line
          let new_has_bullets = has_bullets || is_bullet_point(line)
          check_sections_have_bullets(rest, in_section, new_has_bullets)
        }
      }
    }
  }
}

pub fn changelog_versions_in_descending_order_test() {
  let assert Ok(content) = simplifile.read(changelog_path)
  let lines = string.split(content, "\n")

  // Extract all versions as tuples
  let versions =
    lines
    |> list.filter_map(fn(line) {
      case extract_version_from_line(line) {
        Ok(v) -> parse_version(v)
        Error(_) -> Error(Nil)
      }
    })

  // Check descending order
  check_descending_order(versions)
}

fn check_descending_order(versions: List(#(Int, Int, Int))) -> Nil {
  case versions {
    [] | [_] -> Nil
    [first, second, ..rest] -> {
      // first should be > second
      version_greater(first, second)
      |> should.be_true()
      check_descending_order([second, ..rest])
    }
  }
}

pub fn changelog_no_empty_bullet_points_test() {
  let assert Ok(content) = simplifile.read(changelog_path)
  let lines = string.split(content, "\n")

  lines
  |> list.any(is_empty_bullet_point)
  |> should.be_false()
}

pub fn changelog_no_malformed_version_headers_test() {
  let assert Ok(content) = simplifile.read(changelog_path)
  let lines = string.split(content, "\n")

  // Find any malformed version headers (e.g., "## 0.1", "##0.1.0", "## 1.0.0-rc1")
  let malformed =
    lines
    |> list.filter(is_malformed_version_header)

  // Should have no malformed headers
  malformed
  |> should.equal([])
}
