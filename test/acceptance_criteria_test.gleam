/// Acceptance Criteria Verification Tests.
///
/// This module provides a traceable mapping from the spec's acceptance criteria (AC #1-11)
/// to their test implementations. Each test explicitly verifies one AC and documents
/// which test files provide additional coverage.
///
/// ## Acceptance Criteria Coverage Matrix
///
/// | AC  | Description                                    | Primary Test File(s)                      |
/// |-----|------------------------------------------------|-------------------------------------------|
/// | #1  | Bidirectional mode via --input-format stream-json | cli_bidir_test.gleam                     |
/// | #2  | NDJSON framing both directions                 | stream_test.gleam                         |
/// | #3  | Request ID correlation                         | request_tracker_test.gleam, control_integration_test.gleam |
/// | #4  | Initialize handshake registers hooks           | init_handshake_test.gleam                 |
/// | #5  | Hook callbacks invoke user functions           | hook_integration_test.gleam               |
/// | #6  | Permission callbacks consult can_use_tool      | hook_integration_test.gleam               |
/// | #7  | SDK MCP servers route to handlers              | permission_options_test.gleam, e2e/sdk_mcp_test.gleam |
/// | #8  | Control operations send control_request        | control_integration_test.gleam            |
/// | #9  | Timeouts default 60s, configurable per-hook    | timeout_options_test.gleam, hook_timeout_test.gleam |
/// | #10 | Hook errors fail-open                          | fail_open_test.gleam                      |
/// | #11 | Permission errors fail-deny                    | fail_deny_test.gleam                      |
///
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/option.{None}
import gleam/string
import gleeunit/should

import claude_agent_sdk/control.{Interrupt, SetModel}
import claude_agent_sdk/hook
import claude_agent_sdk/internal/bidir
import claude_agent_sdk/internal/bidir/actor.{
  type RequestResult, type SubscriberMessage, HookConfig, RequestSuccess,
  Running, StartConfig,
}
import claude_agent_sdk/internal/cli
import claude_agent_sdk/internal/line_framing.{
  CompleteLine, NeedMoreData, normalize_crlf, read_line,
}
import claude_agent_sdk/internal/request_tracker
import claude_agent_sdk/options
import support/full_mock_runner
import support/mock_bidir_runner

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

/// Receive messages from a subject until finding one containing target_id.
fn receive_until_match(
  subject: Subject(String),
  target_id: String,
  max_attempts: Int,
) -> Result(String, Nil) {
  case max_attempts <= 0 {
    True -> Error(Nil)
    False -> {
      case process.receive(subject, 500) {
        Ok(msg) -> {
          case string.contains(msg, target_id) {
            True -> Ok(msg)
            False -> receive_until_match(subject, target_id, max_attempts - 1)
          }
        }
        Error(Nil) -> Error(Nil)
      }
    }
  }
}

// =============================================================================
// AC #1: Bidirectional mode via --input-format stream-json
// =============================================================================

/// AC #1: Verify --input-format stream-json is passed to CLI for bidirectional mode.
///
/// Covered by: cli_bidir_test.gleam
/// - bidir_cli_args_includes_input_format_test
/// - bidir_vs_standard_args_difference_test
/// - has_bidir_features_with_* tests
pub fn test_ac1_bidir_mode_cli_flag_test() {
  // Verify that build_bidir_cli_args includes --input-format stream-json
  let opts = options.default_options()
  let args = cli.build_bidir_cli_args(opts, "test prompt")

  // Must contain --input-format
  should.be_true(args |> contains_arg("--input-format"))

  // Must contain stream-json (appears twice: once for --output-format, once for --input-format)
  let stream_json_count = count_occurrences(args, "stream-json")
  should.equal(stream_json_count, 2)

  // Standard args should NOT have --input-format
  let standard_args = cli.build_cli_args(opts, "test prompt")
  should.be_false(standard_args |> contains_arg("--input-format"))
}

// =============================================================================
// AC #2: NDJSON framing both directions
// =============================================================================

/// AC #2: Verify messages are framed as NDJSON (newline-delimited JSON).
///
/// Covered by: stream_test.gleam
/// - read_line_* tests
/// - chunk_reassembly_* tests
/// - normalize_crlf_* tests
pub fn test_ac2_ndjson_framing_test() {
  // Test 1: Verify read_line extracts complete lines ending with \n
  let #(result, remaining) = read_line(<<"hello\n":utf8>>)
  should.equal(result, CompleteLine("hello"))
  should.equal(remaining, <<>>)

  // Test 2: Verify partial lines wait for more data
  let #(result2, _remaining2) = read_line(<<"partial":utf8>>)
  should.equal(result2, NeedMoreData)

  // Test 3: Verify CRLF is normalized to LF (Windows compatibility)
  let normalized = normalize_crlf(<<"line\r\n":utf8>>)
  should.equal(normalized, <<"line\n":utf8>>)

  // Test 4: Verify multiple lines can be read sequentially
  let #(result3, remaining3) = read_line(<<"first\nsecond\n":utf8>>)
  should.equal(result3, CompleteLine("first"))
  let #(result4, _remaining4) = read_line(remaining3)
  should.equal(result4, CompleteLine("second"))
}

// =============================================================================
// AC #3: Request ID correlation
// =============================================================================

/// AC #3: Verify request IDs are generated and used for response correlation.
///
/// Covered by: request_tracker_test.gleam, control_integration_test.gleam
/// - generate_id_produces_unique_ids_test
/// - test_control_request_id_correlation_test
pub fn test_ac3_request_id_correlation_test() {
  // Test request_tracker generates unique IDs with req_ prefix
  let tracker = request_tracker.new()
  let #(tracker, id1) = request_tracker.generate_id(tracker)
  let #(tracker, id2) = request_tracker.generate_id(tracker)
  let #(_tracker, id3) = request_tracker.generate_id(tracker)

  // IDs must be non-empty and follow req_ prefix pattern
  should.be_true(string.starts_with(id1, "req_"))
  should.be_true(string.starts_with(id2, "req_"))
  should.be_true(string.starts_with(id3, "req_"))

  // IDs must be unique
  should.not_equal(id1, id2)
  should.not_equal(id2, id3)
  should.not_equal(id1, id3)

  // Test that responses correlate to correct requests via integration test
  // (Full correlation test is in control_integration_test.gleam)
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)
  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let adapter = full_mock_runner.set_session(adapter, session)

  let assert Ok(_init) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send request with specific ID
  let result_subject: Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("ac3_test_id"), result_subject)

  // Verify request contains our ID
  let assert Ok(request_json) = process.receive(adapter.captured_writes, 500)
  should.be_true(string.contains(request_json, "\"request_id\":\"ac3_test_id\""))

  // Send response with matching ID
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"ac3_test_id\"}}"
  bidir.inject_message(session, response_json)

  // Verify correlation worked
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> Nil
    _ -> should.fail()
  }

  bidir.shutdown(session)
}

// =============================================================================
// AC #4: Initialize handshake registers hooks
// =============================================================================

/// AC #4: Verify initialize handshake sends hook registrations.
///
/// Covered by: init_handshake_test.gleam
/// - init_request_sent_on_start_test
/// - success_response_transitions_to_running_test
/// - capabilities_stored_on_success_test
pub fn test_ac4_init_handshake_test() {
  let mock = mock_bidir_runner.new()

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start session - should send initialize request
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Verify init request was sent
  let assert Ok(init_msg) = process.receive(mock.writes, 500)
  should.be_true(string.contains(init_msg, "control_request"))
  should.be_true(string.contains(init_msg, "initialize"))
  // Verify request_id is present with req_ prefix (don't pin exact counter value)
  should.be_true(string.contains(init_msg, "\"request_id\":\"req_"))

  // Extract request_id from init message for response correlation
  // The init message contains a request_id we need to echo back
  let assert Ok(req_id) = extract_request_id(init_msg)

  // Send success response with matching request_id
  let success_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\""
    <> req_id
    <> "\",\"response\":{\"capabilities\":{\"hooks_supported\":true}}}}"
  bidir.inject_message(session, success_json)

  process.sleep(50)

  // Should transition to Running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  bidir.shutdown(session)
}

// =============================================================================
// AC #5: Hook callbacks invoke user functions
// =============================================================================

/// AC #5: Verify hook callbacks are invoked with context.
///
/// Covered by: hook_integration_test.gleam
/// - pre_tool_use_receives_context_test
/// - post_tool_use_receives_output_test
pub fn test_ac5_hook_callbacks_test() {
  // Track if hook was called
  let hook_called: Subject(Bool) = process.new_subject()

  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)

  // Register hook handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("TestHook", fn(_input: Dynamic) -> Dynamic {
          process.send(hook_called, True)
          to_dynamic(dict.from_list([#("continue", True)]))
        }),
      ]),
      permission_handlers: dict.new(),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) =
    bidir.start_with_hooks(adapter.bidir_runner, config, hooks)
  let adapter = full_mock_runner.set_session(adapter, session)

  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Trigger hook callback
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_hook_ac5\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"TestHook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Verify hook was called
  let assert Ok(True) = process.receive(hook_called, 500)

  bidir.shutdown(session)
}

// =============================================================================
// AC #6: Permission callbacks consult can_use_tool
// =============================================================================

/// AC #6: Verify permission callbacks are invoked and can allow/deny.
///
/// Covered by: hook_integration_test.gleam
/// - can_use_tool_permission_test
pub fn test_ac6_permission_callbacks_test() {
  let permission_called: Subject(Bool) = process.new_subject()

  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)

  // Register permission handler that allows the tool
  let hooks =
    HookConfig(
      handlers: dict.new(),
      permission_handlers: dict.from_list([
        #("ac6_tool", fn(_input: Dynamic) -> Dynamic {
          process.send(permission_called, True)
          to_dynamic(dict.from_list([#("behavior", "allow")]))
        }),
      ]),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) =
    bidir.start_with_hooks(adapter.bidir_runner, config, hooks)
  let adapter = full_mock_runner.set_session(adapter, session)

  let assert Ok(_) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send can_use_tool request
  let permission_json =
    "{\"type\":\"control_request\",\"request_id\":\"perm_ac6\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"ac6_tool\",\"input\":{},\"permission_suggestions\":[]}}"
  bidir.inject_message(session, permission_json)
  process.sleep(100)

  // Verify permission handler was called
  let assert Ok(True) = process.receive(permission_called, 500)

  // Verify response includes "allow"
  let assert Ok(response) =
    receive_until_match(adapter.captured_writes, "perm_ac6", 5)
  should.be_true(string.contains(response, "allow"))

  bidir.shutdown(session)
}

// =============================================================================
// AC #7: SDK MCP servers route to handlers
// =============================================================================

/// AC #7: Verify MCP servers can be registered and handlers are callable.
///
/// NOTE: This test verifies configuration/registration. Runtime routing of
/// McpMessage requests to SDK-registered handlers is marked as TODO in
/// bidir.gleam and covered by E2E tests when available.
///
/// Covered by: permission_options_test.gleam, e2e/sdk_mcp_test.gleam
/// - with_mcp_server_adds_to_list_test
/// - mcp_server_handler_is_callable_test
/// - sdk_50_mcp_config_test (E2E - tests CLI MCP config passthrough)
pub fn test_ac7_mcp_routing_test() {
  // Test that MCP servers can be registered via options
  let handler = fn(request: Dynamic) -> Dynamic {
    // Echo back the request
    request
  }

  let opts =
    options.bidir_options()
    |> options.with_mcp_server("ac7_server", handler)

  // Verify server was registered
  should.equal(1, case opts.mcp_servers {
    [#(name, _)] -> {
      should.equal(name, "ac7_server")
      1
    }
    _ -> 0
  })

  // Verify handler is callable (registration test)
  let assert [#(_, stored_handler)] = opts.mcp_servers
  let test_input = to_dynamic("test_request")
  let result = stored_handler(test_input)
  should.equal(result, test_input)
  // NOTE: Runtime routing test would require bidir.McpMessage handling
  // to be implemented (currently TODO in src/claude_agent_sdk/internal/bidir.gleam).
  // E2E tests in e2e/sdk_mcp_test.gleam verify CLI MCP config passthrough.
}

// =============================================================================
// AC #8: Control operations send control_request
// =============================================================================

/// AC #8: Verify control operations send properly formatted control_request.
///
/// Covered by: control_integration_test.gleam
/// - test_interrupt_flow_test
/// - test_set_permission_mode_flow_test
/// - test_set_model_flow_test
/// - test_rewind_files_with_checkpointing_test
pub fn test_ac8_control_requests_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)
  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let adapter = full_mock_runner.set_session(adapter, session)

  let assert Ok(_init) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Test: set_model sends control_request
  let result_subject: Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    SetModel("ac8_model_req", "opus"),
    result_subject,
  )

  let assert Ok(request_json) = process.receive(adapter.captured_writes, 500)

  // Verify wire format
  should.be_true(string.contains(request_json, "\"type\":\"control_request\""))
  should.be_true(string.contains(request_json, "\"subtype\":\"set_model\""))
  should.be_true(string.contains(request_json, "\"model\":\"opus\""))
  should.be_true(string.contains(
    request_json,
    "\"request_id\":\"ac8_model_req\"",
  ))

  // Send success response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"ac8_model_req\"}}"
  bidir.inject_message(session, response_json)

  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> Nil
    _ -> should.fail()
  }

  bidir.shutdown(session)
}

// =============================================================================
// AC #9: Timeouts default 60s, configurable per-hook
// =============================================================================

/// AC #9: Verify timeouts default to 60s and are configurable per-hook.
///
/// Covered by: timeout_options_test.gleam, hook_timeout_test.gleam
/// - with_timeout_sets_timeout_ms_test
/// - with_hook_timeout_sets_entry_test
/// - with_hook_timeout_accumulates_test
/// - slow_callback_times_out_test
pub fn test_ac9_hook_timeouts_test() {
  // Test 0: Verify default timeout is 60_000ms (60s) per AC #9
  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let default_config = bidir.default_config(subscriber)
  should.equal(default_config.default_timeout_ms, 60_000)

  // Test 1: Global timeout can be set
  let opts1 =
    options.bidir_options()
    |> options.with_timeout(30_000)
  case opts1.timeout_ms {
    option.Some(30_000) -> Nil
    _ -> should.fail()
  }

  // Test 2: Per-hook timeouts can be configured
  let opts2 =
    options.bidir_options()
    |> options.with_hook_timeout(hook.PreToolUse, 5000)
    |> options.with_hook_timeout(hook.PostToolUse, 10_000)

  should.equal(dict.get(opts2.hook_timeouts, hook.PreToolUse), Ok(5000))
  should.equal(dict.get(opts2.hook_timeouts, hook.PostToolUse), Ok(10_000))

  // Test 3: Per-hook overwrites
  let opts3 =
    options.bidir_options()
    |> options.with_hook_timeout(hook.PreToolUse, 5000)
    |> options.with_hook_timeout(hook.PreToolUse, 8000)

  should.equal(dict.get(opts3.hook_timeouts, hook.PreToolUse), Ok(8000))
}

// =============================================================================
// AC #10: Hook errors fail-open
// =============================================================================

/// AC #10: Verify hook errors (crashes/timeouts) return continue: true.
///
/// Covered by: fail_open_test.gleam
/// - crashing_callback_returns_continue_true_test
/// - timeout_returns_continue_true_test
/// - session_continues_after_crash_fail_open_test
pub fn test_ac10_hook_errors_fail_open_test() {
  let crash_tracker: Subject(Bool) = process.new_subject()

  let mock = mock_bidir_runner.new()

  // Hook that crashes
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("crash_hook", fn(_input: Dynamic) -> Dynamic {
          process.send(crash_tracker, True)
          panic as "AC10 test crash"
        }),
      ]),
      permission_handlers: dict.new(),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 5000,
      enable_file_checkpointing: False,
      mcp_servers: [],
      on_warning: None,
    )

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Capture init request and extract request_id for response correlation
  let assert Ok(init_msg) = process.receive(mock.writes, 500)
  let assert Ok(req_id) = extract_request_id(init_msg)

  // Transition to Running with matching request_id
  let init_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\""
    <> req_id
    <> "\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_json)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Trigger crashing hook
  let hook_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_ac10\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"crash_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_json)

  // Verify crash happened
  let assert Ok(True) = process.receive(crash_tracker, 500)
  process.sleep(100)

  // Verify fail-open response (continue: true)
  let assert Ok(response) = receive_until_match(mock.writes, "continue", 5)
  should.be_true(string.contains(response, "cli_ac10"))
  should.be_true(string.contains(response, "continue"))
  should.be_true(string.contains(response, "success"))
  should.be_false(string.contains(response, "deny"))

  // Session should still be running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  bidir.shutdown(session)
}

// =============================================================================
// AC #11: Permission errors fail-deny
// =============================================================================

/// AC #11: Verify permission errors (crashes/timeouts) return behavior: "deny".
///
/// Covered by: fail_deny_test.gleam
/// - permission_crash_returns_deny_test
/// - permission_timeout_returns_deny_test
pub fn test_ac11_permission_errors_fail_deny_test() {
  let crash_tracker: Subject(Bool) = process.new_subject()

  let mock = mock_bidir_runner.new()

  // Permission handler that crashes
  let hooks =
    HookConfig(
      handlers: dict.new(),
      permission_handlers: dict.from_list([
        #("crash_perm_tool", fn(_input: Dynamic) -> Dynamic {
          process.send(crash_tracker, True)
          panic as "AC11 permission crash"
        }),
      ]),
    )

  let subscriber: Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 5000,
      enable_file_checkpointing: False,
      mcp_servers: [],
      on_warning: None,
    )

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Capture init request and extract request_id for response correlation
  let assert Ok(init_msg) = process.receive(mock.writes, 500)
  let assert Ok(req_id) = extract_request_id(init_msg)

  // Transition to Running with matching request_id
  let init_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\""
    <> req_id
    <> "\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_json)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send can_use_tool for crashing permission handler
  let perm_json =
    "{\"type\":\"control_request\",\"request_id\":\"perm_ac11\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"crash_perm_tool\",\"input\":{},\"permission_suggestions\":[]}}"
  bidir.inject_message(session, perm_json)

  // Verify crash happened
  let assert Ok(True) = process.receive(crash_tracker, 500)
  process.sleep(100)

  // Verify fail-deny response (behavior: "deny")
  let assert Ok(response) = receive_until_match(mock.writes, "perm_ac11", 5)
  should.be_true(string.contains(response, "deny"))
  should.be_true(string.contains(response, "success"))
  should.be_false(string.contains(response, "allow"))

  // Session should still be running
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  bidir.shutdown(session)
}

// =============================================================================
// Helpers
// =============================================================================

/// Extract request_id from a JSON message string.
/// Looks for "request_id":"<id>" pattern.
fn extract_request_id(json: String) -> Result(String, Nil) {
  let marker = "\"request_id\":\""
  case string.split(json, marker) {
    [_, after, ..] -> {
      // Find the closing quote
      case string.split(after, "\"") {
        [id, ..] -> Ok(id)
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn contains_arg(args: List(String), target: String) -> Bool {
  case args {
    [] -> False
    [head, ..tail] ->
      case head == target {
        True -> True
        False -> contains_arg(tail, target)
      }
  }
}

fn count_occurrences(args: List(String), target: String) -> Int {
  count_occurrences_helper(args, target, 0)
}

fn count_occurrences_helper(args: List(String), target: String, acc: Int) -> Int {
  case args {
    [] -> acc
    [head, ..tail] ->
      case head == target {
        True -> count_occurrences_helper(tail, target, acc + 1)
        False -> count_occurrences_helper(tail, target, acc)
      }
  }
}
