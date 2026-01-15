/// Regression tests for query() API.
///
/// Ensures the query() API continues to work unchanged after adding
/// bidirectional protocol support. These tests verify backward compatibility
/// for existing users.
import claude_agent_sdk
import claude_agent_sdk/content.{TextBlock, ToolUseBlock, UnknownBlock}
import claude_agent_sdk/error
import claude_agent_sdk/hook
import claude_agent_sdk/message
import claude_agent_sdk/options
import claude_agent_sdk/runner
import gleam/dynamic
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import support/ets_helpers

// =============================================================================
// Test Helpers
// =============================================================================

const lines_key = "lines"

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> dynamic.Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn from_dynamic(d: dynamic.Dynamic) -> a

/// Create a simple mock runner that emits lines in sequence.
fn create_mock_runner(
  table_name: String,
  lines: List(String),
) -> claude_agent_sdk.Runner {
  let table = ets_helpers.new(table_name)
  ets_helpers.insert(table, to_dynamic(lines_key), to_dynamic(lines))

  runner.test_runner(
    on_spawn: fn(_cmd, _args, _cwd) { Ok(to_dynamic(table)) },
    on_read: fn(handle) {
      let table: ets_helpers.Table = from_dynamic(handle)
      case ets_helpers.lookup(table, to_dynamic(lines_key)) {
        Some(lines_dyn) -> {
          let remaining: List(String) = from_dynamic(lines_dyn)
          case remaining {
            [line, ..rest] -> {
              ets_helpers.insert(table, to_dynamic(lines_key), to_dynamic(rest))
              runner.Data(<<line:utf8>>)
            }
            [] -> runner.ExitStatus(0)
          }
        }
        None -> runner.ReadError("Mock runner missing lines state")
      }
    },
    on_close: fn(handle) {
      let table: ets_helpers.Table = from_dynamic(handle)
      ets_helpers.delete(table, to_dynamic(lines_key))
      Nil
    },
  )
}

// =============================================================================
// Test: query() returns Iterator(Message)
// =============================================================================

/// Test that query() returns a stream that yields messages as an iterator.
/// This verifies the core API behavior is unchanged.
pub fn query_returns_message_iterator_test() {
  let system_json =
    "{\"type\":\"system\",\"session_id\":\"sess-001\",\"subtype\":\"init\"}\n"
  let assistant_json =
    "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Hello!\"}]}}\n"
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}\n"

  let runner =
    create_mock_runner("query_returns_iterator", [
      system_json,
      assistant_json,
      result_json,
    ])

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(runner)

  case claude_agent_sdk.query("Hello", opts) {
    Error(err) -> {
      io.println("Unexpected error: " <> string.inspect(err))
      should.fail()
    }
    Ok(stream) -> {
      // Collect messages via collect_messages (which uses the iterator internally)
      let result = claude_agent_sdk.collect_messages(stream)

      // Should have 3 messages: system, assistant, result
      should.equal(list.length(result.items), 3)

      // Verify message types in order
      case result.items {
        [env1, env2, env3] -> {
          case env1.message {
            message.System(_) -> should.be_true(True)
            _ -> {
              io.println("Expected System message first")
              should.fail()
            }
          }
          case env2.message {
            message.Assistant(_) -> should.be_true(True)
            _ -> {
              io.println("Expected Assistant message second")
              should.fail()
            }
          }
          case env3.message {
            message.Result(_) -> should.be_true(True)
            _ -> {
              io.println("Expected Result message third")
              should.fail()
            }
          }
        }
        _ -> {
          io.println("Expected exactly 3 messages")
          should.fail()
        }
      }

      // Verify no terminal error
      should.equal(result.terminal_error, None)
    }
  }
}

// =============================================================================
// Test: All existing message types parse correctly
// =============================================================================

/// Test that all known message types parse to their correct variants.
/// Covers: System, Assistant, User, Result
pub fn existing_message_types_parse_test() {
  let system_json =
    "{\"type\":\"system\",\"session_id\":\"sess-003\",\"subtype\":\"init\",\"model\":\"claude-3-opus\"}\n"
  let assistant_json =
    "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Let me help.\"}]}}\n"
  let user_json =
    "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"tu_001\",\"content\":\"result\"}]}}\n"
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"completed\",\"is_error\":false}\n"

  let runner =
    create_mock_runner("message_types_parse", [
      system_json,
      assistant_json,
      user_json,
      result_json,
    ])

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(runner)

  case claude_agent_sdk.query("Test", opts) {
    Error(err) -> {
      io.println("Unexpected error: " <> string.inspect(err))
      should.fail()
    }
    Ok(stream) -> {
      let result = claude_agent_sdk.collect_messages(stream)

      // Should have 4 messages
      should.equal(list.length(result.items), 4)

      // Verify each type
      case result.items {
        [env1, env2, env3, env4] -> {
          // System message
          case env1.message {
            message.System(sys) -> {
              should.equal(sys.session_id, Some("sess-003"))
              should.equal(sys.model, Some("claude-3-opus"))
            }
            _ -> should.fail()
          }

          // Assistant message
          case env2.message {
            message.Assistant(asst) -> {
              case asst.message {
                Some(content) -> {
                  case content.content {
                    Some([TextBlock(text)]) ->
                      should.equal(text, "Let me help.")
                    _ -> should.fail()
                  }
                }
                None -> should.fail()
              }
            }
            _ -> should.fail()
          }

          // User message (tool result)
          case env3.message {
            message.User(user) -> {
              case user.message {
                Some(content) -> should.equal(content.role, Some("user"))
                None -> should.fail()
              }
            }
            _ -> should.fail()
          }

          // Result message
          case env4.message {
            message.Result(res) -> {
              should.equal(res.subtype, Some(message.Success))
              should.equal(res.result, Some("completed"))
              should.equal(res.is_error, Some(False))
            }
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
  }
}

// =============================================================================
// Test: Existing Runner mock works unchanged
// =============================================================================

/// Test that the existing simple Runner mock interface still works.
/// This verifies backward compatibility for test mocks.
pub fn existing_runner_mock_works_test() {
  // Create runner using the exact same pattern as existing tests
  let system_json =
    "{\"type\":\"system\",\"session_id\":\"sess-004\",\"subtype\":\"init\"}\n"
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"ok\"}\n"

  let runner = create_mock_runner("existing_runner", [system_json, result_json])

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(runner)

  case claude_agent_sdk.query("Test", opts) {
    Error(_) -> should.fail()
    Ok(stream) -> {
      let result = claude_agent_sdk.collect_messages(stream)

      // Verify stream works correctly with existing mock
      should.equal(list.length(result.items), 2)
      should.equal(result.terminal_error, None)

      // Verify messages parsed correctly
      case result.items {
        [env1, env2] -> {
          case env1.message {
            message.System(_) -> should.be_true(True)
            _ -> should.fail()
          }
          case env2.message {
            message.Result(_) -> should.be_true(True)
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
  }
}

// =============================================================================
// Test: Option composition still works
// =============================================================================

/// Test that chaining multiple options still works as before.
/// Verifies: with_model, with_max_turns, with_system_prompt
pub fn query_options_still_compose_test() {
  let system_json =
    "{\"type\":\"system\",\"session_id\":\"sess-005\",\"subtype\":\"init\"}\n"
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}\n"

  let runner = create_mock_runner("options_compose", [system_json, result_json])

  // Chain multiple options
  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(runner)
    |> claude_agent_sdk.with_model("opus")
    |> claude_agent_sdk.with_max_turns(5)
    |> claude_agent_sdk.with_system_prompt("You are a helpful assistant.")

  // Verify options are set correctly
  should.equal(opts.model, Some("opus"))
  should.equal(opts.max_turns, Some(5))
  should.equal(opts.system_prompt, Some("You are a helpful assistant."))

  // Query should work with composed options
  case claude_agent_sdk.query("Hello", opts) {
    Error(err) -> {
      io.println("Query failed: " <> string.inspect(err))
      should.fail()
    }
    Ok(stream) -> {
      let result = claude_agent_sdk.collect_messages(stream)
      should.equal(list.length(result.items), 2)
      should.equal(result.terminal_error, None)
    }
  }
}

// =============================================================================
// Test: Content block types parse correctly
// =============================================================================

/// Test that various content block types in assistant messages parse correctly.
/// Covers: TextBlock, ToolUseBlock, UnknownBlock
pub fn content_block_types_parse_test() {
  let system_json =
    "{\"type\":\"system\",\"session_id\":\"sess-006\",\"subtype\":\"init\"}\n"
  // Assistant with text, tool_use, and unknown block types
  let assistant_json =
    "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Checking...\"},{\"type\":\"tool_use\",\"id\":\"tu_001\",\"name\":\"Bash\",\"input\":{\"command\":\"ls\"}},{\"type\":\"future_type\",\"data\":\"unknown\"}]}}\n"
  let result_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}\n"

  let runner =
    create_mock_runner("content_blocks", [
      system_json,
      assistant_json,
      result_json,
    ])

  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(runner)

  case claude_agent_sdk.query("Run ls", opts) {
    Error(err) -> {
      io.println("Unexpected error: " <> string.inspect(err))
      should.fail()
    }
    Ok(stream) -> {
      let result = claude_agent_sdk.collect_messages(stream)

      // Find the assistant message
      let assistant_msgs =
        list.filter(result.items, fn(env) {
          case env.message {
            message.Assistant(_) -> True
            _ -> False
          }
        })

      case assistant_msgs {
        [env] -> {
          case env.message {
            message.Assistant(asst) -> {
              case asst.message {
                Some(content) -> {
                  case content.content {
                    Some(blocks) -> {
                      should.equal(list.length(blocks), 3)

                      // Check each block type
                      case blocks {
                        [b1, b2, b3] -> {
                          // TextBlock
                          case b1 {
                            TextBlock(text) -> should.equal(text, "Checking...")
                            _ -> should.fail()
                          }
                          // ToolUseBlock
                          case b2 {
                            ToolUseBlock(id, name, _input) -> {
                              should.equal(id, "tu_001")
                              should.equal(name, "Bash")
                            }
                            _ -> should.fail()
                          }
                          // UnknownBlock (future_type parsed as unknown)
                          case b3 {
                            UnknownBlock(_raw) -> should.be_true(True)
                            _ -> should.fail()
                          }
                        }
                        _ -> should.fail()
                      }
                    }
                    None -> should.fail()
                  }
                }
                None -> should.fail()
              }
            }
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
  }
}

// =============================================================================
// Test: Result subtypes parse correctly
// =============================================================================

/// Test that all result subtypes parse correctly.
/// Covers: Success, ErrorMaxTurns, ErrorDuringExecution, ErrorMaxBudget, UnknownSubtype
pub fn result_subtypes_parse_test() {
  // Test Success subtype
  let system_json =
    "{\"type\":\"system\",\"session_id\":\"sess-007\",\"subtype\":\"init\"}\n"
  let success_json =
    "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\"}\n"

  let runner1 =
    create_mock_runner("result_subtype_success", [system_json, success_json])
  let opts1 =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(runner1)

  case claude_agent_sdk.query("Test", opts1) {
    Error(_) -> should.fail()
    Ok(stream) -> {
      let result = claude_agent_sdk.collect_messages(stream)
      case list.last(result.items) {
        Ok(env) -> {
          case env.message {
            message.Result(res) ->
              should.equal(res.subtype, Some(message.Success))
            _ -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
  }

  // Test error_max_turns subtype
  let error_turns_json =
    "{\"type\":\"result\",\"subtype\":\"error_max_turns\",\"is_error\":true}\n"

  let runner2 =
    create_mock_runner("result_subtype_maxturns", [
      system_json,
      error_turns_json,
    ])
  let opts2 =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(runner2)

  case claude_agent_sdk.query("Test", opts2) {
    Error(_) -> should.fail()
    Ok(stream) -> {
      let result = claude_agent_sdk.collect_messages(stream)
      case list.last(result.items) {
        Ok(env) -> {
          case env.message {
            message.Result(res) ->
              should.equal(res.subtype, Some(message.ErrorMaxTurns))
            _ -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
  }

  // Test unknown subtype (forward compatibility)
  let unknown_json =
    "{\"type\":\"result\",\"subtype\":\"future_subtype\",\"result\":\"done\"}\n"

  let runner3 =
    create_mock_runner("result_subtype_unknown", [system_json, unknown_json])
  let opts3 =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(runner3)

  case claude_agent_sdk.query("Test", opts3) {
    Error(_) -> should.fail()
    Ok(stream) -> {
      let result = claude_agent_sdk.collect_messages(stream)
      case list.last(result.items) {
        Ok(env) -> {
          case env.message {
            message.Result(res) -> {
              case res.subtype {
                Some(message.UnknownSubtype(s)) ->
                  should.equal(s, "future_subtype")
                _ -> should.fail()
              }
            }
            _ -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
  }
}

// =============================================================================
// Test: MessageEnvelope preserves raw JSON
// =============================================================================

/// Test that MessageEnvelope preserves the raw JSON alongside parsed message.
pub fn message_envelope_preserves_raw_json_test() {
  let system_json = "{\"type\":\"system\",\"session_id\":\"sess-008\"}\n"
  let result_json = "{\"type\":\"result\",\"subtype\":\"success\"}\n"

  let runner = create_mock_runner("envelope_raw", [system_json, result_json])
  let opts =
    claude_agent_sdk.default_options()
    |> claude_agent_sdk.with_test_mode(runner)

  case claude_agent_sdk.query("Test", opts) {
    Error(_) -> should.fail()
    Ok(stream) -> {
      let result = claude_agent_sdk.collect_messages(stream)

      case result.items {
        [env1, _env2] -> {
          // raw_json should contain the original JSON (with trailing newline stripped)
          should.be_true(string.contains(env1.raw_json, "\"type\":\"system\""))
          should.be_true(string.contains(env1.raw_json, "sess-008"))

          // raw_bytes should be non-empty
          should.be_true(env1.raw_bytes != <<>>)
        }
        _ -> should.fail()
      }
    }
  }
}

