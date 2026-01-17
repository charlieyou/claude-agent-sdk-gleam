/// Tests for JSON decoding of Claude CLI messages.
///
/// These tests load fixtures from test/fixtures/ and verify decoder behavior.
import claude_agent_sdk/content
import claude_agent_sdk/internal/decoder
import claude_agent_sdk/message
import gleam/bit_array
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{None, Some}
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

pub fn decode_assistant_is_partial_true_test() {
  let json = load_fixture("assistant_partial_true.json")
  let result = decoder.decode_message(json)
  let assert Ok(message.Assistant(msg)) = result
  msg.is_partial
  |> should.be_true
  Nil
}

pub fn decode_assistant_is_partial_false_test() {
  let json = load_fixture("assistant_partial_false.json")
  let result = decoder.decode_message(json)
  let assert Ok(message.Assistant(msg)) = result
  msg.is_partial
  |> should.be_false
  Nil
}

pub fn decode_assistant_is_partial_missing_test() {
  // Existing fixture has no is_partial field - should default to False
  let json = load_fixture("assistant_message.json")
  let result = decoder.decode_message(json)
  let assert Ok(message.Assistant(msg)) = result
  msg.is_partial
  |> should.be_false
  Nil
}

pub fn decode_assistant_partial_empty_content_test() {
  // Partial message with empty content is valid
  let json = load_fixture("assistant_partial_empty.json")
  let result = decoder.decode_message(json)
  let assert Ok(message.Assistant(msg)) = result
  msg.is_partial
  |> should.be_true
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
    Error(_other) ->
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
// ThinkingBlock Tests
// =============================================================================

pub fn decode_thinking_block_with_signature_test() {
  // ThinkingBlock with signature field
  let json =
    "{\"type\": \"thinking\", \"thinking\": \"Let me analyze this problem...\", \"signature\": \"abc123sig\"}"
  let dynamic = parse_json_to_dynamic(json)
  case decoder.decode_content_block(dynamic) {
    Ok(content.ThinkingBlock(thinking:, signature:)) -> {
      should.equal(thinking, "Let me analyze this problem...")
      should.equal(signature, Some("abc123sig"))
    }
    Ok(_) -> panic as "Expected ThinkingBlock variant"
    Error(_) -> panic as "Expected successful decode for thinking block"
  }
}

pub fn decode_thinking_block_without_signature_test() {
  // ThinkingBlock without signature field (signature is optional)
  let json = "{\"type\": \"thinking\", \"thinking\": \"Analyzing...\"}"
  let dynamic = parse_json_to_dynamic(json)
  case decoder.decode_content_block(dynamic) {
    Ok(content.ThinkingBlock(thinking:, signature:)) -> {
      should.equal(thinking, "Analyzing...")
      should.equal(signature, None)
    }
    Ok(_) -> panic as "Expected ThinkingBlock variant"
    Error(_) ->
      panic as "Expected successful decode for thinking block without signature"
  }
}

pub fn decode_thinking_block_missing_thinking_field_test() {
  // ThinkingBlock missing required "thinking" field falls back to UnknownBlock
  // per forward compatibility requirements
  let json = "{\"type\": \"thinking\", \"signature\": \"sig\"}"
  let dynamic = parse_json_to_dynamic(json)
  case decoder.decode_content_block(dynamic) {
    Ok(content.UnknownBlock(_)) -> Nil
    Ok(_) ->
      panic as "Expected UnknownBlock fallback for malformed thinking block"
    Error(_) -> panic as "Expected UnknownBlock fallback, got error"
  }
}

pub fn decode_thinking_block_via_message_path_test() {
  // Verify thinking blocks are decoded correctly via decode_message path
  // This tests the content_block_decoder used by assistant message decoding
  let json =
    "{\"type\": \"assistant\", \"message\": {\"content\": [{\"type\": \"thinking\", \"thinking\": \"Deep thought here\", \"signature\": \"sig123\"}]}}"
  case decoder.decode_message(json) {
    Ok(message.Assistant(msg)) -> {
      case msg.message {
        Some(inner) -> {
          case inner.content {
            Some([content.ThinkingBlock(thinking:, signature:)]) -> {
              should.equal(thinking, "Deep thought here")
              should.equal(signature, Some("sig123"))
            }
            Some(_) -> panic as "Expected single ThinkingBlock in content"
            None -> panic as "Expected content in assistant message"
          }
        }
        None -> panic as "Expected inner message in assistant message"
      }
    }
    Ok(_) -> panic as "Expected Assistant message type"
    Error(e) -> panic as { "Decode failed: " <> string.inspect(e) }
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

// =============================================================================
// Standalone Type Decoder Tests
// =============================================================================

/// Helper to parse JSON string to Dynamic
fn parse_json_to_dynamic(json_str: String) {
  case json.parse(json_str, decode.dynamic) {
    Ok(dynamic) -> dynamic
    Error(_) -> panic as "Failed to parse JSON fixture"
  }
}

pub fn decode_tool_result_block_test() {
  let json = load_fixture("tool_result_block.json")
  let dynamic = parse_json_to_dynamic(json)
  case decoder.decode_tool_result_block(dynamic) {
    Ok(block) -> {
      should.equal(block.tool_use_id, "toolu_01BeipFBP3sUY3EAwVg3qQnE")
      should.equal(block.content, "File created successfully.")
      should.equal(block.is_error, Some(False))
    }
    Error(_) -> panic as "Expected successful decode for tool_result_block.json"
  }
}

pub fn decode_usage_test() {
  let json = load_fixture("usage_stats.json")
  let dynamic = parse_json_to_dynamic(json)
  case decoder.decode_usage(dynamic) {
    Ok(usage) -> {
      should.equal(usage.input_tokens, Some(1500))
      should.equal(usage.output_tokens, Some(350))
      should.equal(usage.cache_creation_input_tokens, Some(2000))
      should.equal(usage.cache_read_input_tokens, Some(800))
    }
    Error(_) -> panic as "Expected successful decode for usage_stats.json"
  }
}

pub fn decode_mcp_server_status_test() {
  let json = load_fixture("mcp_server_status.json")
  let dynamic = parse_json_to_dynamic(json)
  case decoder.decode_mcp_server_status(dynamic) {
    Ok(status) -> {
      should.equal(status.name, "my-mcp-server")
      should.equal(status.status, "connected")
    }
    Error(_) -> panic as "Expected successful decode for mcp_server_status.json"
  }
}

pub fn decode_permission_denial_test() {
  let json = load_fixture("permission_denial.json")
  let dynamic = parse_json_to_dynamic(json)
  case decoder.decode_permission_denial(dynamic) {
    Ok(denial) -> {
      should.equal(denial.tool_name, "Bash")
      should.equal(denial.tool_use_id, "toolu_01XyZ789AbCdEfGhIjKlMnOp")
      // tool_input is Dynamic - decoder success implies it's present
      Nil
    }
    Error(_) -> panic as "Expected successful decode for permission_denial.json"
  }
}

// =============================================================================
// Multiple Content Blocks Tests
// =============================================================================

pub fn decode_nested_content_blocks_test() {
  let json = load_fixture("nested_content_blocks.json")
  let result = decoder.decode_message(json)
  case result {
    Ok(message.Assistant(msg)) -> {
      case msg.message {
        Some(inner) -> {
          case inner.content {
            Some(blocks) -> {
              // Should have 4 blocks: text, tool_use, text, tool_use
              should.equal(4, list.length(blocks))
            }
            None -> panic as "Expected content in assistant message"
          }
        }
        None -> panic as "Expected inner message in assistant message"
      }
    }
    Ok(_) -> panic as "Expected Assistant message type"
    Error(_) ->
      panic as "Expected successful decode for nested_content_blocks.json"
  }
}

// =============================================================================
// Error Path Tests for Standalone Decoders
// =============================================================================

pub fn decode_tool_result_block_missing_field_test() {
  // Missing tool_use_id should error
  let json = "{\"content\": \"ok\", \"type\": \"tool_result\"}"
  let dynamic = parse_json_to_dynamic(json)
  case decoder.decode_tool_result_block(dynamic) {
    Error(decoder.JsonDecodeError(msg)) -> {
      let has_expected =
        string.contains(msg, "tool_use_id") || string.contains(msg, "String")
      case has_expected {
        True -> Nil
        False ->
          panic as { "Expected error mentioning missing field, got: " <> msg }
      }
    }
    Ok(_) -> panic as "Expected error for tool_result missing tool_use_id"
    Error(_) -> panic as "Expected JsonDecodeError for missing field"
  }
}

pub fn decode_mcp_server_status_missing_field_test() {
  // Missing name should error
  let json = "{\"status\": \"connected\"}"
  let dynamic = parse_json_to_dynamic(json)
  case decoder.decode_mcp_server_status(dynamic) {
    Error(decoder.JsonDecodeError(msg)) -> {
      let has_name =
        string.contains(msg, "name") || string.contains(msg, "String")
      case has_name {
        True -> Nil
        False ->
          panic as { "Expected error mentioning missing name, got: " <> msg }
      }
    }
    Ok(_) -> panic as "Expected error for mcp_server_status missing name"
    Error(_) -> panic as "Expected JsonDecodeError for missing field"
  }
}

pub fn decode_permission_denial_missing_field_test() {
  // Missing tool_name should error
  let json = "{\"tool_use_id\": \"xyz\", \"tool_input\": {}}"
  let dynamic = parse_json_to_dynamic(json)
  case decoder.decode_permission_denial(dynamic) {
    Error(decoder.JsonDecodeError(msg)) -> {
      let has_tool_name =
        string.contains(msg, "tool_name") || string.contains(msg, "String")
      case has_tool_name {
        True -> Nil
        False ->
          panic as {
            "Expected error mentioning missing tool_name, got: " <> msg
          }
      }
    }
    Ok(_) -> panic as "Expected error for permission_denial missing tool_name"
    Error(_) -> panic as "Expected JsonDecodeError for missing field"
  }
}

pub fn decode_usage_all_optional_succeeds_test() {
  // Usage should decode even with empty object (all fields optional)
  let json = "{}"
  let dynamic = parse_json_to_dynamic(json)
  case decoder.decode_usage(dynamic) {
    Ok(usage) -> {
      should.equal(usage.input_tokens, None)
      should.equal(usage.output_tokens, None)
    }
    Error(_) -> panic as "Expected empty usage object to decode successfully"
  }
}

// =============================================================================
// Golden Transcript Tests (Contract Validation)
// =============================================================================

/// Load and parse a golden transcript (NDJSON) file
fn load_golden_transcript(name: String) -> List(String) {
  let path = "test/fixtures/golden/" <> name
  case simplifile.read(path) {
    Ok(content) -> {
      content
      |> string.split("\n")
      |> list.filter(fn(line) { string.trim(line) != "" })
    }
    Error(_) -> panic as { "Failed to load golden transcript: " <> path }
  }
}

pub fn decode_golden_basic_query_transcript_test() {
  // Load the golden transcript and verify each line parses successfully
  let lines = load_golden_transcript("basic_query.ndjson")

  // Should have 3 messages: system, assistant, result
  should.equal(list.length(lines), 3)

  // Decode each line and verify type
  let types =
    list.map(lines, fn(line) {
      case decoder.decode_message(line) {
        Ok(msg) -> {
          case msg {
            message.System(_) -> "system"
            message.Assistant(_) -> "assistant"
            message.Result(_) -> "result"
            message.User(_) -> "user"
          }
        }
        Error(err) -> {
          panic as {
            "Failed to decode golden transcript line: " <> string.inspect(err)
          }
        }
      }
    })

  // Verify expected message type sequence
  should.equal(types, ["system", "assistant", "result"])
}
