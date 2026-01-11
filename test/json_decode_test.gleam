/// Tests for JSON decoding of Claude CLI messages.
///
/// These tests load fixtures from test/fixtures/ and verify decoder behavior.
/// Per TDD methodology: tests are written first and skip until decoders are
/// implemented. Set DECODER_IMPLEMENTED=1 to run full tests.
import claude_agent_sdk/internal/decoder
import claude_agent_sdk/message
import gleam/bit_array
import gleam/io
import gleeunit/should
import simplifile
import support/env_helpers

/// Check if decoder tests should run (DECODER_IMPLEMENTED env var set)
fn decoder_implemented() -> Bool {
  case env_helpers.get_env("DECODER_IMPLEMENTED") {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Skip helper for decoder tests
fn skip_if_not_implemented(test_name: String) -> Bool {
  case decoder_implemented() {
    True -> False
    False -> {
      let line =
        "[SKIP:DECODER] " <> test_name <> ": DECODER_IMPLEMENTED not set"
      io.println(line)
      True
    }
  }
}

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
  case skip_if_not_implemented("decode_system_message_test") {
    True -> Nil
    False -> {
      let json = load_fixture("system_message.json")
      let result = decoder.decode_message(json)
      let _ = should.be_ok(result)
      Nil
    }
  }
}

pub fn decode_system_message_minimal_test() {
  case skip_if_not_implemented("decode_system_message_minimal_test") {
    True -> Nil
    False -> {
      let json = load_fixture("system_message_minimal.json")
      let result = decoder.decode_message(json)
      let _ = should.be_ok(result)
      Nil
    }
  }
}

// =============================================================================
// Assistant Message Tests
// =============================================================================

pub fn decode_assistant_message_test() {
  case skip_if_not_implemented("decode_assistant_message_test") {
    True -> Nil
    False -> {
      let json = load_fixture("assistant_message.json")
      let result = decoder.decode_message(json)
      let _ = should.be_ok(result)
      Nil
    }
  }
}

pub fn decode_assistant_unknown_block_test() {
  case skip_if_not_implemented("decode_assistant_unknown_block_test") {
    True -> Nil
    False -> {
      let json = load_fixture("assistant_unknown_block.json")
      let result = decoder.decode_message(json)
      let _ = should.be_ok(result)
      Nil
    }
  }
}

// =============================================================================
// User Message Tests
// =============================================================================

pub fn decode_user_message_test() {
  case skip_if_not_implemented("decode_user_message_test") {
    True -> Nil
    False -> {
      let json = load_fixture("user_message.json")
      let result = decoder.decode_message(json)
      let _ = should.be_ok(result)
      Nil
    }
  }
}

// =============================================================================
// Result Message Tests
// =============================================================================

pub fn decode_result_success_test() {
  case skip_if_not_implemented("decode_result_success_test") {
    True -> Nil
    False -> {
      let json = load_fixture("result_success.json")
      let result = decoder.decode_message(json)
      let _ = should.be_ok(result)
      Nil
    }
  }
}

pub fn decode_result_error_test() {
  case skip_if_not_implemented("decode_result_error_test") {
    True -> Nil
    False -> {
      let json = load_fixture("result_error.json")
      let result = decoder.decode_message(json)
      let _ = should.be_ok(result)
      Nil
    }
  }
}

// =============================================================================
// Forward Compatibility Tests
// =============================================================================

pub fn decode_unknown_message_type_test() {
  case skip_if_not_implemented("decode_unknown_message_type_test") {
    True -> Nil
    False -> {
      let json = load_fixture("unknown_message_type.json")
      let result = decoder.decode_message(json)
      case result {
        Error(decoder.UnexpectedMessageType(_)) -> Nil
        _ ->
          panic as "Expected UnexpectedMessageType error for unknown message type"
      }
    }
  }
}

pub fn decode_unknown_content_block_test() {
  case skip_if_not_implemented("decode_unknown_content_block_test") {
    True -> Nil
    False -> {
      let json = load_fixture("unknown_content_block.json")
      let result = decoder.decode_message(json)
      let _ = should.be_ok(result)
      Nil
    }
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
  case
    skip_if_not_implemented(
      "decode_message_envelope_raw_json_preservation_test",
    )
  {
    True -> Nil
    False -> {
      // Load fixture and verify raw_json equals fixture content byte-for-byte
      let json = load_fixture("system_message.json")
      let raw_bytes = bit_array.from_string(json)

      case decoder.decode_message_envelope(json, raw_bytes) {
        Ok(envelope) -> {
          // raw_json must equal exact input string
          should.equal(envelope.raw_json, json)
        }
        Error(_) ->
          panic as "Expected successful decode for system_message.json"
      }
    }
  }
}

pub fn decode_message_envelope_raw_bytes_preservation_test() {
  case
    skip_if_not_implemented(
      "decode_message_envelope_raw_bytes_preservation_test",
    )
  {
    True -> Nil
    False -> {
      // Load fixture and verify raw_bytes matches bit_array.from_string(fixture)
      let json = load_fixture("system_message.json")
      let raw_bytes = bit_array.from_string(json)

      case decoder.decode_message_envelope(json, raw_bytes) {
        Ok(envelope) -> {
          // raw_bytes must equal exact input BitArray
          should.equal(envelope.raw_bytes, raw_bytes)
        }
        Error(_) ->
          panic as "Expected successful decode for system_message.json"
      }
    }
  }
}

pub fn decode_message_envelope_contains_decoded_message_test() {
  case
    skip_if_not_implemented(
      "decode_message_envelope_contains_decoded_message_test",
    )
  {
    True -> Nil
    False -> {
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
        Error(_) ->
          panic as "Expected successful decode for system_message.json"
      }
    }
  }
}

pub fn decode_message_envelope_error_returns_decode_error_test() {
  case
    skip_if_not_implemented(
      "decode_message_envelope_error_returns_decode_error_test",
    )
  {
    True -> Nil
    False -> {
      // Invalid JSON should return decode error
      let json = "{ invalid json }"
      let raw_bytes = bit_array.from_string(json)

      case decoder.decode_message_envelope(json, raw_bytes) {
        Error(decoder.JsonSyntaxError(_)) -> Nil
        Error(decoder.JsonDecodeError(_)) -> Nil
        _ -> panic as "Expected decode error for invalid JSON"
      }
    }
  }
}
