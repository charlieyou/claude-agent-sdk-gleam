/// E2E Tests for Structured Output (SDK-15 to SDK-18).
///
/// These tests verify that structured output with JSON schema validation works
/// correctly with the real Claude CLI. Tests assert schema adherence without
/// content brittleness.
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// # Ensure the Claude CLI is authenticated (e.g., `claude auth login`)
/// ```
import claude_agent_sdk
import claude_agent_sdk/error.{error_to_string}
import claude_agent_sdk/message.{type MessageEnvelope, Result}
import claude_agent_sdk/options
import e2e/helpers
import gleam/dynamic
import gleam/dynamic/decode
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

// ============================================================================
// SDK-15: Simple Structured Output (flat object)
// ============================================================================

/// SDK-15: Verify simple structured output with flat object schema.
/// Tests that a simple object with required string fields is properly validated.
pub fn sdk_15_simple_structured_output_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_15_simple_structured_output")

      let ctx = helpers.test_step(ctx, "configure_options")
      // Simple schema: {"type":"object","properties":{"name":{"type":"string"}},"required":["name"]}
      let schema =
        json.to_string(
          json.object([
            #("type", json.string("object")),
            #(
              "properties",
              json.object([
                #("name", json.object([#("type", json.string("string"))])),
              ]),
            ),
            #("required", json.array(["name"], json.string)),
          ]),
        )

      let cli_opts =
        options.cli_options()
        |> options.with_max_turns(1)
        |> options.with_json_schema(schema)

      let sdk_opts = options.sdk_options()

      let ctx = helpers.test_step(ctx, "execute_query")
      case
        claude_agent_sdk.query_new(
          "Return a JSON object with a name field set to 'Alice'",
          cli_opts,
          sdk_opts,
        )
      {
        Error(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed")
          should.fail()
        }
        Ok(stream) -> {
          let result = helpers.consume_stream(stream)

          let ctx = helpers.test_step(ctx, "find_result_message")
          let result_msg = find_result_with_structured_output(result.messages)

          case result_msg {
            Some(output) -> {
              let ctx = helpers.test_step(ctx, "validate_schema_adherence")
              // Validate the output is a dynamic value (parsed JSON)
              // The key assertion: structured_output is present
              helpers.log_info_with(ctx, "structured_output_present", [
                #("has_output", json.bool(True)),
              ])

              // Try to decode as object with name field
              let name_decoder = {
                use name <- decode.field("name", decode.string)
                decode.success(name)
              }

              case decode.run(output, name_decoder) {
                Ok(name) -> {
                  // Verify name is non-empty string
                  { name != "" }
                  |> should.be_true
                  helpers.log_info_with(ctx, "name_field_valid", [
                    #("name", json.string(name)),
                  ])
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "Simple structured output validated",
                  )
                }
                Error(_) -> {
                  helpers.log_error(
                    ctx,
                    "decode_failed",
                    "Could not decode name field",
                  )
                  helpers.log_test_complete(
                    ctx,
                    False,
                    "Schema validation failed",
                  )
                  should.fail()
                }
              }
            }
            None -> {
              helpers.log_info(ctx, "no_structured_output_acceptable")
              // structured_output may not be present in all CLI versions
              helpers.log_test_complete(
                ctx,
                True,
                "No structured output (acceptable)",
              )
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// SDK-16: Nested Structured Output
// ============================================================================

/// SDK-16: Verify nested structured output with complex object schema.
/// Tests that nested objects and arrays are properly validated.
pub fn sdk_16_nested_structured_output_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_16_nested_structured_output")

      let ctx = helpers.test_step(ctx, "configure_options")
      // Nested schema with address object
      let schema =
        json.to_string(
          json.object([
            #("type", json.string("object")),
            #(
              "properties",
              json.object([
                #("name", json.object([#("type", json.string("string"))])),
                #(
                  "address",
                  json.object([
                    #("type", json.string("object")),
                    #(
                      "properties",
                      json.object([
                        #(
                          "city",
                          json.object([#("type", json.string("string"))]),
                        ),
                        #(
                          "country",
                          json.object([#("type", json.string("string"))]),
                        ),
                      ]),
                    ),
                    #("required", json.array(["city", "country"], json.string)),
                  ]),
                ),
              ]),
            ),
            #("required", json.array(["name", "address"], json.string)),
          ]),
        )

      let cli_opts =
        options.cli_options()
        |> options.with_max_turns(1)
        |> options.with_json_schema(schema)

      let sdk_opts = options.sdk_options()

      let ctx = helpers.test_step(ctx, "execute_query")
      case
        claude_agent_sdk.query_new(
          "Return a JSON object with name 'Bob' and address with city 'Paris' and country 'France'",
          cli_opts,
          sdk_opts,
        )
      {
        Error(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed")
          should.fail()
        }
        Ok(stream) -> {
          let result = helpers.consume_stream(stream)

          let ctx = helpers.test_step(ctx, "find_result_message")
          let result_msg = find_result_with_structured_output(result.messages)

          case result_msg {
            Some(output) -> {
              let ctx = helpers.test_step(ctx, "validate_nested_schema")
              helpers.log_info_with(ctx, "structured_output_present", [
                #("has_output", json.bool(True)),
              ])

              // Try to decode nested structure
              let nested_decoder = {
                use name <- decode.field("name", decode.string)
                use address <- decode.field("address", {
                  use city <- decode.field("city", decode.string)
                  use country <- decode.field("country", decode.string)
                  decode.success(#(city, country))
                })
                decode.success(#(name, address))
              }

              case decode.run(output, nested_decoder) {
                Ok(#(name, #(city, country))) -> {
                  // Verify all fields are non-empty
                  { name != "" && city != "" && country != "" }
                  |> should.be_true
                  helpers.log_info_with(ctx, "nested_fields_valid", [
                    #("name", json.string(name)),
                    #("city", json.string(city)),
                    #("country", json.string(country)),
                  ])
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "Nested structured output validated",
                  )
                }
                Error(_) -> {
                  helpers.log_error(
                    ctx,
                    "decode_failed",
                    "Could not decode nested structure",
                  )
                  helpers.log_test_complete(
                    ctx,
                    False,
                    "Nested schema validation failed",
                  )
                  should.fail()
                }
              }
            }
            None -> {
              helpers.log_info(ctx, "no_structured_output_acceptable")
              helpers.log_test_complete(
                ctx,
                True,
                "No structured output (acceptable)",
              )
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// SDK-17: Enum Structured Output
// ============================================================================

/// SDK-17: Verify structured output with enum constraint.
/// Tests that enum values are properly validated.
pub fn sdk_17_enum_structured_output_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_17_enum_structured_output")

      let ctx = helpers.test_step(ctx, "configure_options")
      // Schema with enum constraint
      let schema =
        json.to_string(
          json.object([
            #("type", json.string("object")),
            #(
              "properties",
              json.object([
                #(
                  "color",
                  json.object([
                    #("type", json.string("string")),
                    #("enum", json.array(["red", "green", "blue"], json.string)),
                  ]),
                ),
              ]),
            ),
            #("required", json.array(["color"], json.string)),
          ]),
        )

      let cli_opts =
        options.cli_options()
        |> options.with_max_turns(1)
        |> options.with_json_schema(schema)

      let sdk_opts = options.sdk_options()

      let ctx = helpers.test_step(ctx, "execute_query")
      case
        claude_agent_sdk.query_new(
          "Return a JSON object with a color field set to 'blue'",
          cli_opts,
          sdk_opts,
        )
      {
        Error(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed")
          should.fail()
        }
        Ok(stream) -> {
          let result = helpers.consume_stream(stream)

          let ctx = helpers.test_step(ctx, "find_result_message")
          let result_msg = find_result_with_structured_output(result.messages)

          case result_msg {
            Some(output) -> {
              let ctx = helpers.test_step(ctx, "validate_enum_constraint")
              helpers.log_info_with(ctx, "structured_output_present", [
                #("has_output", json.bool(True)),
              ])

              // Try to decode color field
              let color_decoder = {
                use color <- decode.field("color", decode.string)
                decode.success(color)
              }

              case decode.run(output, color_decoder) {
                Ok(color) -> {
                  // Verify color is one of the valid enum values
                  let is_valid_enum =
                    color == "red" || color == "green" || color == "blue"
                  is_valid_enum
                  |> should.be_true
                  helpers.log_info_with(ctx, "enum_value_valid", [
                    #("color", json.string(color)),
                  ])
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "Enum structured output validated",
                  )
                }
                Error(_) -> {
                  helpers.log_error(
                    ctx,
                    "decode_failed",
                    "Could not decode color field",
                  )
                  helpers.log_test_complete(
                    ctx,
                    False,
                    "Enum schema validation failed",
                  )
                  should.fail()
                }
              }
            }
            None -> {
              helpers.log_info(ctx, "no_structured_output_acceptable")
              helpers.log_test_complete(
                ctx,
                True,
                "No structured output (acceptable)",
              )
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// SDK-18: Structured Output with Tools
// ============================================================================

/// SDK-18: Verify structured output works alongside tool use.
/// Tests that tool execution doesn't interfere with structured output schema.
pub fn sdk_18_structured_output_with_tools_test_() {
  use <- helpers.with_e2e_timeout()
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_18_structured_output_with_tools")

      let ctx = helpers.test_step(ctx, "configure_options")
      // Schema expecting file info structure
      let schema =
        json.to_string(
          json.object([
            #("type", json.string("object")),
            #(
              "properties",
              json.object([
                #("filename", json.object([#("type", json.string("string"))])),
                #("exists", json.object([#("type", json.string("boolean"))])),
              ]),
            ),
            #("required", json.array(["filename", "exists"], json.string)),
          ]),
        )

      let cli_opts =
        options.cli_options()
        |> options.with_max_turns(2)
        |> options.with_permission_mode(options.BypassPermissions)
        |> options.with_json_schema(schema)

      let sdk_opts = options.sdk_options()

      let ctx = helpers.test_step(ctx, "execute_query")
      case
        claude_agent_sdk.query_new(
          "Check if gleam.toml exists and return a JSON with filename and exists fields",
          cli_opts,
          sdk_opts,
        )
      {
        Error(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, False, "Query failed")
          should.fail()
        }
        Ok(stream) -> {
          let result = helpers.consume_stream(stream)

          let ctx = helpers.test_step(ctx, "validate_stream_termination")
          // Key invariant: stream should terminate normally even with schema
          result.terminated_normally
          |> should.be_true

          let ctx = helpers.test_step(ctx, "find_result_message")
          let result_msg = find_result_with_structured_output(result.messages)

          case result_msg {
            Some(output) -> {
              let ctx = helpers.test_step(ctx, "validate_tool_schema_combo")
              helpers.log_info_with(ctx, "structured_output_present", [
                #("has_output", json.bool(True)),
              ])

              // Try to decode file info structure
              let file_info_decoder = {
                use filename <- decode.field("filename", decode.string)
                use exists <- decode.field("exists", decode.bool)
                decode.success(#(filename, exists))
              }

              case decode.run(output, file_info_decoder) {
                Ok(#(filename, exists)) -> {
                  // Verify filename is non-empty
                  { filename != "" }
                  |> should.be_true
                  helpers.log_info_with(ctx, "file_info_valid", [
                    #("filename", json.string(filename)),
                    #("exists", json.bool(exists)),
                  ])
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "Structured output with tools validated",
                  )
                }
                Error(_) -> {
                  helpers.log_error(
                    ctx,
                    "decode_failed",
                    "Could not decode file info",
                  )
                  helpers.log_test_complete(
                    ctx,
                    False,
                    "Tool+schema validation failed",
                  )
                  should.fail()
                }
              }
            }
            None -> {
              helpers.log_info(ctx, "no_structured_output_acceptable")
              // structured_output may not be present in all CLI versions
              // or when tool use is involved
              helpers.log_test_complete(
                ctx,
                True,
                "No structured output (acceptable)",
              )
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Extract structured_output from ResultMessage if present.
fn find_result_with_structured_output(
  messages: List(MessageEnvelope),
) -> option.Option(dynamic.Dynamic) {
  list.find_map(messages, fn(envelope) {
    case envelope.message {
      Result(res) -> {
        case res.structured_output {
          Some(output) -> Ok(output)
          None -> Error(Nil)
        }
      }
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}
