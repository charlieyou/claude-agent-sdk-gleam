/// Integration contract validation tests for SDK decoders against golden transcripts.
///
/// These tests verify that SDK decoders can parse actual CLI output format,
/// detecting any drift in the output structure between CLI versions.
///
/// All tests run OFFLINE using pre-captured golden fixtures - no API calls.
/// No artifacts are written; tests are pure read-only operations.
import claude_agent_sdk/internal/decoder.{
  type DecodeError, JsonDecodeError, JsonSyntaxError, UnexpectedMessageType,
  decode_message,
}
import claude_agent_sdk/message.{
  type AssistantMessage, type ResultMessage, type SystemMessage, Assistant,
  Result, System, User,
}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

import e2e/helpers

// ============================================================================
// Required fields contract per message type
// ============================================================================

// Required fields per message type for contract validation:
// - system: type (implicit in successful decode)
// - assistant: type, message (must be Some for valid response)
// - result: type, result (must be Some for valid completion)

// ============================================================================
// Golden transcript contract validation
// ============================================================================

/// Contract test: Parse basic_query.ndjson golden fixture.
/// Validates that SDK decoders handle actual CLI output format.
pub fn contract_01_basic_query_golden_test_() {
  use <- helpers.with_e2e_timeout()
  let path = "test/fixtures/golden/basic_query.ndjson"

  case read_golden_file(path) {
    Error(reason) -> {
      should.fail()
      panic as { "Failed to read golden file: " <> reason }
    }
    Ok(lines) -> {
      // Parse each line and validate
      let results = list.index_map(lines, fn(line, idx) { #(idx, line) })
      let errors = validate_golden_lines(results, [])

      case errors {
        [] -> should.be_true(True)
        _ -> {
          let error_msg = format_validation_errors(errors)
          should.fail()
          panic as { "Contract validation failed: " <> error_msg }
        }
      }
    }
  }
}

/// Contract test: Verify message types match expected sequence.
/// The basic_query fixture should have: system, assistant, result
pub fn contract_02_message_sequence_test_() {
  use <- helpers.with_e2e_timeout()
  let path = "test/fixtures/golden/basic_query.ndjson"

  case read_and_parse_golden(path) {
    Error(reason) -> {
      should.fail()
      panic as { "Failed to parse golden file: " <> reason }
    }
    Ok(messages) -> {
      let expected_types = ["system", "assistant", "result"]
      let actual_types = list.map(messages, message_type_string)

      case expected_types == actual_types {
        True -> should.be_true(True)
        False -> {
          let msg =
            "Sequence mismatch: expected ["
            <> string.join(expected_types, ",")
            <> "] got ["
            <> string.join(actual_types, ",")
            <> "]"
          should.fail()
          panic as msg
        }
      }
    }
  }
}

/// Contract test: Verify required field presence per message type.
/// Validates that golden fixtures contain all fields the SDK needs.
pub fn contract_03_required_fields_test_() {
  use <- helpers.with_e2e_timeout()
  let path = "test/fixtures/golden/basic_query.ndjson"

  case read_golden_file(path) {
    Error(reason) -> {
      should.fail()
      panic as { "Failed to read golden file: " <> reason }
    }
    Ok(lines) -> {
      // Parse each line and validate required fields per type
      let errors =
        list.index_map(lines, fn(line, idx) {
          validate_required_fields(line, idx)
        })
        |> list.filter_map(fn(r) {
          case r {
            Ok(_) -> Error(Nil)
            Error(msg) -> Ok(msg)
          }
        })

      case errors {
        [] -> should.be_true(True)
        _ -> {
          let error_msg = string.join(errors, "; ")
          should.fail()
          panic as { "Missing required fields: " <> error_msg }
        }
      }
    }
  }
}

// ============================================================================
// Negative tests: Verify decoder rejects malformed input
// ============================================================================

/// Verify decoder rejects JSON without type field.
pub fn contract_04_missing_type_rejected_test_() {
  use <- helpers.with_e2e_timeout()
  let malformed = "{\"session_id\": \"abc123\"}"
  case decode_message(malformed) {
    Error(JsonDecodeError(msg)) -> {
      should.be_true(string.contains(msg, "type"))
    }
    Error(_) -> {
      // Still counts as rejected, which is correct behavior
      should.be_true(True)
    }
    Ok(_) -> {
      should.fail()
      panic as "Missing type field was accepted"
    }
  }
}

/// Verify decoder rejects unknown message type.
pub fn contract_05_unknown_type_rejected_test_() {
  use <- helpers.with_e2e_timeout()
  let unknown = "{\"type\": \"foobar\"}"
  case decode_message(unknown) {
    Error(UnexpectedMessageType(t)) -> {
      should.equal(t, "foobar")
    }
    Error(_) -> {
      should.be_true(True)
    }
    Ok(_) -> {
      should.fail()
      panic as "Unknown message type was accepted"
    }
  }
}

/// Verify decoder rejects invalid JSON syntax.
pub fn contract_06_invalid_json_rejected_test_() {
  use <- helpers.with_e2e_timeout()
  let invalid = "{type: system}"
  case decode_message(invalid) {
    Error(JsonSyntaxError(_)) -> {
      should.be_true(True)
    }
    Error(_) -> {
      should.be_true(True)
    }
    Ok(_) -> {
      should.fail()
      panic as "Invalid JSON was accepted"
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

/// Validate required fields for a single JSON line.
/// Parses the message and checks type-specific required fields.
fn validate_required_fields(line: String, idx: Int) -> Result(Nil, String) {
  case decode_message(line) {
    Error(e) ->
      Error(
        "Line "
        <> int.to_string(idx)
        <> ": parse error - "
        <> format_decode_error(e),
      )
    Ok(msg) -> {
      case msg {
        System(sys) -> validate_system_fields(sys, idx)
        Assistant(asst) -> validate_assistant_fields(asst, idx)
        Result(res) -> validate_result_fields(res, idx)
        User(_) -> Ok(Nil)
      }
    }
  }
}

/// Validate system message required fields.
fn validate_system_fields(_msg: SystemMessage, _idx: Int) -> Result(Nil, String) {
  // System messages only require type (which is implicit in successful decode)
  // All other fields are optional per the decoder
  Ok(Nil)
}

/// Validate assistant message required fields.
/// AssistantMessage.message must be Some and its content field must also be Some.
fn validate_assistant_fields(
  msg: AssistantMessage,
  idx: Int,
) -> Result(Nil, String) {
  case msg.message {
    Some(inner) ->
      case inner.content {
        Some(_) -> Ok(Nil)
        None ->
          Error(
            "Line "
            <> int.to_string(idx)
            <> ": assistant message missing required field 'content'",
          )
      }
    None ->
      Error(
        "Line "
        <> int.to_string(idx)
        <> ": assistant message missing required field 'message'",
      )
  }
}

/// Validate result message required fields.
/// ResultMessage.result must be Some for a valid completion.
fn validate_result_fields(msg: ResultMessage, idx: Int) -> Result(Nil, String) {
  case msg.result {
    Some(_) -> Ok(Nil)
    None ->
      Error(
        "Line "
        <> int.to_string(idx)
        <> ": result message missing required field 'result'",
      )
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
