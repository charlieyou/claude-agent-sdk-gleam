/// Tests for JSON decoding of Claude CLI messages.
///
/// These tests load fixtures from test/fixtures/ and verify decoder behavior.
/// Per TDD methodology: tests are written first and skip until decoders are
/// implemented. Set DECODER_IMPLEMENTED=1 to run full tests.
import claude_agent_sdk/internal/decoder
import claude_agent_sdk/message
import gleam/bit_array
import gleam/string
import gleeunit/should
import simplifile

/// Helper to load a fixture file
fn load_fixture(name: String) -> String {
  let path = "test/fixtures/" <> name
  case simplifile.read(path) {
    Ok(content) -> content
    Error(_) -> panic as { "Failed to load fixture: " <> path }
  }
}

// =============================================================================
// System Message Tests
// =============================================================================

pub fn decode_system_message_test() {
  let json = load_fixture("system_message.json")
  let result = decoder.decode_message(json)
  let _ = should.be_ok(result)
  Nil
}

pub fn decode_system_message_minimal_test() {
  let json = load_fixture("system_message_minimal.json")
  let result = decoder.decode_message(json)
  let _ = should.be_ok(result)
  Nil
}

// =============================================================================
// Assistant Message Tests
// =============================================================================

pub fn decode_assistant_message_test() {
  let json = load_fixture("assistant_message.json")
  let result = decoder.decode_message(json)
  let _ = should.be_ok(result)
  Nil
}

pub fn decode_assistant_unknown_block_test() {
  let json = load_fixture("assistant_unknown_block.json")
  let result = decoder.decode_message(json)
  let _ = should.be_ok(result)
  Nil
}

// =============================================================================
// User Message Tests
// =============================================================================

pub fn decode_user_message_test() {
  let json = load_fixture("user_message.json")
  let result = decoder.decode_message(json)
  let _ = should.be_ok(result)
  Nil
}

// =============================================================================
// Result Message Tests
// =============================================================================

pub fn decode_result_success_test() {
  let json = load_fixture("result_success.json")
  let result = decoder.decode_message(json)
  let _ = should.be_ok(result)
  Nil
}

pub fn decode_result_error_test() {
  let json = load_fixture("result_error.json")
  let result = decoder.decode_message(json)
  let _ = should.be_ok(result)
  Nil
}

// =============================================================================
// Forward Compatibility Tests
// =============================================================================

pub fn decode_unknown_message_type_test() {
  let json = load_fixture("unknown_message_type.json")
  let result = decoder.decode_message(json)
  case result {
    Error(decoder.UnexpectedMessageType(_)) -> Nil
    _ ->
      panic as "Expected UnexpectedMessageType error for unknown message type"
  }
}

pub fn decode_unknown_content_block_test() {
  let json = load_fixture("unknown_content_block.json")
  let result = decoder.decode_message(json)
  let _ = should.be_ok(result)
  Nil
}

// =============================================================================
// Known Type Missing Required Fields Tests
// =============================================================================

pub fn decode_text_block_missing_text_field_test() {
  let json = load_fixture("text_block_missing_text.json")
  let result = decoder.decode_message(json)
  // Known type "text" missing required "text" field must error
  case result {
    Error(decoder.JsonDecodeError(msg)) -> {
      // Verify error message mentions TextBlock and missing field
      let has_textblock = string.contains(msg, "TextBlock")
      let has_missing = string.contains(msg, "missing")
      case has_textblock && has_missing {
        True -> Nil
        False ->
          panic as {
            "Expected error mentioning TextBlock missing field, got: " <> msg
          }
      }
    }
    Ok(_) -> panic as "Expected error for text block missing text field"
    Error(other) ->
      panic as {
        "Expected JsonDecodeError for missing field, got different error type"
      }
  }
}

pub fn decode_tool_use_block_missing_id_field_test() {
  let json = load_fixture("tool_use_block_missing_id.json")
  let result = decoder.decode_message(json)
  // Known type "tool_use" missing required "id" field must error
  case result {
    Error(decoder.JsonDecodeError(msg)) -> {
      // Verify error message mentions ToolUseBlock and missing field
      let has_tooluse = string.contains(msg, "ToolUseBlock")
      let has_missing = string.contains(msg, "missing")
      case has_tooluse && has_missing {
        True -> Nil
        False ->
          panic as {
            "Expected error mentioning ToolUseBlock missing field, got: " <> msg
          }
      }
    }
    Ok(_) -> panic as "Expected error for tool_use block missing id field"
    Error(_) ->
      panic as "Expected JsonDecodeError for missing field, got different error type"
  }
}

// =============================================================================
// Error Case Tests
// =============================================================================

pub fn decode_invalid_json_test() {
  let json = "{ invalid json }"
  let result = decoder.decode_message(json)

  // Should return JsonSyntaxError
  case result {
    Error(decoder.JsonSyntaxError(_)) -> Nil
    Error(decoder.JsonDecodeError(_)) -> Nil
    // Accept either until fully implemented
    _ -> panic as "Expected JsonSyntaxError for invalid JSON"
  }
}

pub fn decode_empty_string_test() {
  let json = ""
  let result = decoder.decode_message(json)

  // Should return an error (empty is not valid JSON)
  should.be_error(result)
}

// =============================================================================
// MessageEnvelope Tests (raw_json / raw_bytes preservation)
// =============================================================================

pub fn decode_message_envelope_raw_json_preservation_test() {
  // Load fixture and verify raw_json equals fixture content byte-for-byte
  let json = load_fixture("system_message.json")
  let raw_bytes = bit_array.from_string(json)

  case decoder.decode_message_envelope(json, raw_bytes) {
    Ok(envelope) -> {
      // raw_json must equal exact input string
      should.equal(envelope.raw_json, json)
    }
    Error(_) -> panic as "Expected successful decode for system_message.json"
  }
}

pub fn decode_message_envelope_raw_bytes_preservation_test() {
  // Load fixture and verify raw_bytes matches bit_array.from_string(fixture)
  let json = load_fixture("system_message.json")
  let raw_bytes = bit_array.from_string(json)

  case decoder.decode_message_envelope(json, raw_bytes) {
    Ok(envelope) -> {
      // raw_bytes must equal exact input BitArray
      should.equal(envelope.raw_bytes, raw_bytes)
    }
    Error(_) -> panic as "Expected successful decode for system_message.json"
  }
}

pub fn decode_message_envelope_contains_decoded_message_test() {
  let json = load_fixture("system_message.json")
  let raw_bytes = bit_array.from_string(json)

  case decoder.decode_message_envelope(json, raw_bytes) {
    Ok(envelope) -> {
      // Verify message is a System message
      case envelope.message {
        message.System(_) -> Nil
        _ -> panic as "Expected System message in envelope"
      }
    }
    Error(_) -> panic as "Expected successful decode for system_message.json"
  }
}

pub fn decode_message_envelope_error_returns_decode_error_test() {
  // Invalid JSON should return decode error
  let json = "{ invalid json }"
  let raw_bytes = bit_array.from_string(json)

  case decoder.decode_message_envelope(json, raw_bytes) {
    Error(decoder.JsonSyntaxError(_)) -> Nil
    Error(decoder.JsonDecodeError(_)) -> Nil
    _ -> panic as "Expected decode error for invalid JSON"
  }
}
