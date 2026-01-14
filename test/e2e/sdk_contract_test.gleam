/// Contract validation tests for SDK decoders against golden transcripts.
///
/// These tests verify that SDK decoders can parse actual CLI output format,
/// detecting any drift in the output structure between CLI versions.
///
/// All tests run OFFLINE using pre-captured golden fixtures - no API calls.
import claude_agent_sdk/internal/decoder.{
  type DecodeError, JsonDecodeError, JsonSyntaxError, UnexpectedMessageType,
  decode_message,
}
import claude_agent_sdk/message.{Assistant, Result, System, User}
import e2e/helpers
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should

// ============================================================================
// Golden transcript contract validation
// ============================================================================

/// Contract test: Parse basic_query.ndjson golden fixture.
/// Validates that SDK decoders handle actual CLI output format.
pub fn contract_01_basic_query_golden_test() {
  let ctx = helpers.new_test_context("contract_01_basic_query")

  helpers.log_info(ctx, "Loading golden fixture")
  let path = "test/fixtures/golden/basic_query.ndjson"

  case read_golden_file(path) {
    Error(reason) -> {
      helpers.log_error(ctx, "Failed to read golden file", reason)
      should.fail()
    }
    Ok(lines) -> {
      helpers.log_info_with(ctx, "Parsing golden transcript", [
        #("lines", json.int(list.length(lines))),
      ])

      // Parse each line and validate
      let results = list.index_map(lines, fn(line, idx) { #(idx, line) })
      let errors = validate_golden_lines(results, [])

      case errors {
        [] -> {
          helpers.log_info(ctx, "Contract validation passed")
          helpers.log_test_complete(ctx, True, "All lines parsed successfully")
          should.be_true(True)
        }
        _ -> {
          let error_msg = format_validation_errors(errors)
          helpers.log_error(ctx, "Contract validation failed", error_msg)
          helpers.log_test_complete(ctx, False, error_msg)
          should.fail()
        }
      }
    }
  }
}

/// Contract test: Verify message types match expected sequence.
/// The basic_query fixture should have: system, assistant, result
pub fn contract_02_message_sequence_test() {
  let ctx = helpers.new_test_context("contract_02_message_sequence")

  helpers.log_info(ctx, "Validating message sequence")
  let path = "test/fixtures/golden/basic_query.ndjson"

  case read_and_parse_golden(path) {
    Error(reason) -> {
      helpers.log_error(ctx, "Failed to parse golden file", reason)
      should.fail()
    }
    Ok(messages) -> {
      let expected_types = ["system", "assistant", "result"]
      let actual_types = list.map(messages, message_type_string)

      helpers.log_info_with(ctx, "Checking sequence", [
        #("expected", json.string(string.join(expected_types, ","))),
        #("actual", json.string(string.join(actual_types, ","))),
      ])

      case expected_types == actual_types {
        True -> {
          helpers.log_info(ctx, "Message sequence matches")
          helpers.log_test_complete(ctx, True, "Sequence verified")
          should.be_true(True)
        }
        False -> {
          let msg =
            "Sequence mismatch: expected ["
            <> string.join(expected_types, ",")
            <> "] got ["
            <> string.join(actual_types, ",")
            <> "]"
          helpers.log_error(ctx, "Sequence mismatch", msg)
          helpers.log_test_complete(ctx, False, msg)
          should.fail()
        }
      }
    }
  }
}

/// Contract test: Verify required field presence.
/// The type field is always required for all message types.
pub fn contract_03_required_fields_test() {
  let ctx = helpers.new_test_context("contract_03_required_fields")

  helpers.log_info(ctx, "Validating required fields")
  let path = "test/fixtures/golden/basic_query.ndjson"

  case read_golden_file(path) {
    Error(reason) -> {
      helpers.log_error(ctx, "Failed to read golden file", reason)
      should.fail()
    }
    Ok(lines) -> {
      // All lines must have 'type' field for decoder to work
      let results =
        list.index_map(lines, fn(line, idx) {
          case decode_message(line) {
            Ok(msg) -> {
              // Verify message was actually parsed
              let type_str = message_type_string(msg)
              #(idx, Ok(type_str))
            }
            Error(e) -> #(idx, Error(format_decode_error(e)))
          }
        })

      let errors =
        list.filter_map(results, fn(r) {
          case r {
            #(idx, Error(msg)) ->
              Ok("Line " <> int.to_string(idx) <> ": " <> msg)
            #(_, Ok(_)) -> Error(Nil)
          }
        })

      case errors {
        [] -> {
          helpers.log_info(ctx, "All required fields present")
          helpers.log_test_complete(ctx, True, "Required fields verified")
          should.be_true(True)
        }
        _ -> {
          let error_msg = string.join(errors, "; ")
          helpers.log_error(ctx, "Missing required fields", error_msg)
          helpers.log_test_complete(ctx, False, error_msg)
          should.fail()
        }
      }
    }
  }
}

// ============================================================================
// Negative tests: Verify decoder rejects malformed input
// ============================================================================

/// Verify decoder rejects JSON without type field.
pub fn contract_04_missing_type_rejected_test() {
  let ctx = helpers.new_test_context("contract_04_missing_type")

  helpers.log_info(ctx, "Testing missing type field rejection")

  let malformed = "{\"session_id\": \"abc123\"}"
  case decode_message(malformed) {
    Error(JsonDecodeError(msg)) -> {
      helpers.log_info_with(ctx, "Correctly rejected", [
        #("error", json.string(msg)),
      ])
      should.be_true(string.contains(msg, "type"))
      helpers.log_test_complete(ctx, True, "Missing type correctly rejected")
    }
    Error(other) -> {
      helpers.log_info_with(ctx, "Rejected with different error", [
        #("error", json.string(format_decode_error(other))),
      ])
      // Still counts as rejected, which is correct behavior
      helpers.log_test_complete(ctx, True, "Rejected as expected")
      should.be_true(True)
    }
    Ok(_) -> {
      helpers.log_error(
        ctx,
        "Should have rejected",
        "Missing type was accepted",
      )
      helpers.log_test_complete(ctx, False, "Missing type accepted")
      should.fail()
    }
  }
}

/// Verify decoder rejects unknown message type.
pub fn contract_05_unknown_type_rejected_test() {
  let ctx = helpers.new_test_context("contract_05_unknown_type")

  helpers.log_info(ctx, "Testing unknown type rejection")

  let unknown = "{\"type\": \"foobar\"}"
  case decode_message(unknown) {
    Error(UnexpectedMessageType(t)) -> {
      helpers.log_info_with(ctx, "Correctly rejected unknown type", [
        #("type", json.string(t)),
      ])
      should.equal(t, "foobar")
      helpers.log_test_complete(ctx, True, "Unknown type correctly rejected")
    }
    Error(other) -> {
      helpers.log_info_with(ctx, "Rejected with different error", [
        #("error", json.string(format_decode_error(other))),
      ])
      helpers.log_test_complete(ctx, True, "Rejected as expected")
      should.be_true(True)
    }
    Ok(_) -> {
      helpers.log_error(
        ctx,
        "Should have rejected",
        "Unknown type was accepted",
      )
      helpers.log_test_complete(ctx, False, "Unknown type accepted")
      should.fail()
    }
  }
}

/// Verify decoder rejects invalid JSON syntax.
pub fn contract_06_invalid_json_rejected_test() {
  let ctx = helpers.new_test_context("contract_06_invalid_json")

  helpers.log_info(ctx, "Testing invalid JSON rejection")

  let invalid = "{type: system}"
  case decode_message(invalid) {
    Error(JsonSyntaxError(_)) -> {
      helpers.log_info(ctx, "Correctly rejected invalid JSON")
      helpers.log_test_complete(ctx, True, "Invalid JSON correctly rejected")
      should.be_true(True)
    }
    Error(other) -> {
      helpers.log_info_with(ctx, "Rejected with different error", [
        #("error", json.string(format_decode_error(other))),
      ])
      helpers.log_test_complete(ctx, True, "Rejected as expected")
      should.be_true(True)
    }
    Ok(_) -> {
      helpers.log_error(
        ctx,
        "Should have rejected",
        "Invalid JSON was accepted",
      )
      helpers.log_test_complete(ctx, False, "Invalid JSON accepted")
      should.fail()
    }
  }
}

// ============================================================================
// Helper functions
// ============================================================================

/// Read golden file lines.
@external(erlang, "e2e_helpers_ffi", "read_file_lines")
fn read_golden_file(path: String) -> Result(List(String), String)

/// Parse all lines in a golden file and return parsed messages.
fn read_and_parse_golden(path: String) -> Result(List(message.Message), String) {
  case read_golden_file(path) {
    Error(e) -> Error(e)
    Ok(lines) -> {
      let results = list.map(lines, decode_message)
      let errors =
        list.filter_map(results, fn(r) {
          case r {
            Error(e) -> Ok(format_decode_error(e))
            Ok(_) -> Error(Nil)
          }
        })
      case errors {
        [] ->
          Ok(
            list.filter_map(results, fn(r) {
              case r {
                Ok(msg) -> Ok(msg)
                Error(_) -> Error(Nil)
              }
            }),
          )
        _ -> Error(string.join(errors, "; "))
      }
    }
  }
}

/// Get message type as string from Message.
fn message_type_string(msg: message.Message) -> String {
  case msg {
    System(_) -> "system"
    Assistant(_) -> "assistant"
    User(_) -> "user"
    Result(_) -> "result"
  }
}

/// Validate a list of golden lines, collecting errors.
fn validate_golden_lines(
  lines: List(#(Int, String)),
  errors: List(String),
) -> List(String) {
  case lines {
    [] -> list.reverse(errors)
    [#(idx, line), ..rest] -> {
      case decode_message(line) {
        Ok(_) -> validate_golden_lines(rest, errors)
        Error(e) -> {
          let err =
            "Line " <> int.to_string(idx) <> ": " <> format_decode_error(e)
          validate_golden_lines(rest, [err, ..errors])
        }
      }
    }
  }
}

/// Format decode error for display.
fn format_decode_error(err: DecodeError) -> String {
  case err {
    JsonSyntaxError(msg) -> "JSON syntax error: " <> msg
    UnexpectedMessageType(t) -> "Unexpected message type: " <> t
    JsonDecodeError(msg) -> "Decode error: " <> msg
  }
}

/// Format validation errors for display.
fn format_validation_errors(errors: List(String)) -> String {
  case errors {
    [] -> "No errors"
    [single] -> single
    _ ->
      int.to_string(list.length(errors))
      <> " errors: "
      <> string.join(errors, "; ")
  }
}
