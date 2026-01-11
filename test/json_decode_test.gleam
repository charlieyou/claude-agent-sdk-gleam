/// Tests for JSON decoding of Claude CLI messages.
///
/// These tests load fixtures from test/fixtures/ and verify decoder behavior.
/// Per TDD methodology: tests are written first and expected to FAIL until
/// decoders are implemented.
import claude_agent_sdk/internal/decoder
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

  // Should succeed once implemented
  should.be_ok(result)
}

pub fn decode_system_message_minimal_test() {
  let json = load_fixture("system_message_minimal.json")
  let result = decoder.decode_message(json)

  // Should succeed with only required `type` field
  should.be_ok(result)
}

// =============================================================================
// Assistant Message Tests
// =============================================================================

pub fn decode_assistant_message_test() {
  let json = load_fixture("assistant_message.json")
  let result = decoder.decode_message(json)

  // Should succeed once implemented
  should.be_ok(result)
}

pub fn decode_assistant_unknown_block_test() {
  let json = load_fixture("assistant_unknown_block.json")
  let result = decoder.decode_message(json)

  // Should succeed - unknown blocks become UnknownBlock(raw)
  should.be_ok(result)
}

// =============================================================================
// User Message Tests
// =============================================================================

pub fn decode_user_message_test() {
  let json = load_fixture("user_message.json")
  let result = decoder.decode_message(json)

  // Should succeed once implemented
  should.be_ok(result)
}

// =============================================================================
// Result Message Tests
// =============================================================================

pub fn decode_result_success_test() {
  let json = load_fixture("result_success.json")
  let result = decoder.decode_message(json)

  // Should succeed once implemented
  should.be_ok(result)
}

pub fn decode_result_error_test() {
  let json = load_fixture("result_error.json")
  let result = decoder.decode_message(json)

  // Should succeed once implemented
  should.be_ok(result)
}

// =============================================================================
// Forward Compatibility Tests
// =============================================================================

pub fn decode_unknown_message_type_test() {
  let json = load_fixture("unknown_message_type.json")
  let result = decoder.decode_message(json)

  // Should return UnexpectedMessageType error
  case result {
    Error(decoder.UnexpectedMessageType(_)) -> Nil
    _ ->
      panic as "Expected UnexpectedMessageType error for unknown message type"
  }
}

pub fn decode_unknown_content_block_test() {
  let json = load_fixture("unknown_content_block.json")
  let result = decoder.decode_message(json)

  // Should succeed - unknown content blocks are preserved as UnknownBlock
  should.be_ok(result)
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
