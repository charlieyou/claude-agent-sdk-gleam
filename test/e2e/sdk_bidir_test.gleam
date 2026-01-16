/// Tests for Bidirectional Protocol (SDK-40 to SDK-43).
///
/// Tests control messages and advanced bidirectional protocol features
/// using mock runners (no real CLI required):
/// - SDK-40: SetPermissionMode control message
/// - SDK-41: Interrupt signal during operation
/// - SDK-42: Hook response timeout handling
/// - SDK-43: Malformed control response handling
///
/// ## Running Tests
/// ```bash
/// gleam test -- --only sdk_bidir
/// ```
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/json
import gleam/option.{None}
import gleam/string
import gleeunit/should

import claude_agent_sdk/control.{AcceptEdits, Interrupt}
import claude_agent_sdk/internal/bidir.{
  type RequestResult, type SubscriberMessage,
}
import claude_agent_sdk/internal/bidir/actor.{
  HookConfig, RequestError, RequestSuccess, StartConfig,
}
import e2e/helpers
import support/mock_bidir_runner

// ============================================================================
// Test Helpers
// ============================================================================

/// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

/// Complete initialization handshake for a bidir session
fn complete_init(mock: mock_bidir_runner.MockRunner, session) -> Nil {
  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)
  // Send success response with capabilities
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)
  process.sleep(50)
}

// ============================================================================
// SDK-40: SetPermissionMode
// ============================================================================

/// SDK-40: SetPermissionMode control message sends correct wire format.
///
/// Tests that set_permission_mode sends the expected JSON structure
/// and handles CLI success response appropriately.
pub fn sdk_40_set_permission_mode_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_40_set_permission_mode")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  // Use mock runner to capture wire format
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)

  // Complete init handshake
  let ctx = helpers.test_step(ctx, "complete_init_handshake")
  complete_init(mock, session)

  // Verify Running state
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Send set_permission_mode request using async API
  let ctx = helpers.test_step(ctx, "send_set_permission_mode")
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_perm", AcceptEdits),
    result_subject,
  )

  // Verify wire format
  let ctx = helpers.test_step(ctx, "verify_wire_format")
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(request_json, "\"type\":\"control_request\""))
  should.be_true(string.contains(
    request_json,
    "\"subtype\":\"set_permission_mode\"",
  ))
  should.be_true(string.contains(request_json, "\"mode\":\"acceptEdits\""))
  should.be_true(string.contains(request_json, "\"request_id\":\"req_perm\""))

  // Inject success response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_perm\"}}"
  bidir.inject_message(session, response_json)

  // Verify success response is received
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> {
      helpers.log_info(ctx, "permission_mode_set_success")
      helpers.log_test_complete(
        ctx,
        True,
        "set_permission_mode sent correct wire format and received success",
      )
    }
    _ -> {
      helpers.log_error(ctx, "unexpected_result", "Expected RequestSuccess")
      helpers.log_test_complete(ctx, False, "Expected RequestSuccess")
      should.fail()
    }
  }

  bidir.shutdown(session)
}

/// SDK-40b: SetPermissionMode error response handling.
///
/// Verifies that CLI error responses are properly surfaced.
pub fn sdk_40b_set_permission_mode_error_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_40b_permission_error")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)
  let ctx = helpers.test_step(ctx, "complete_init_handshake")
  complete_init(mock, session)
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Send request
  let ctx = helpers.test_step(ctx, "send_permission_mode_request")
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_err", AcceptEdits),
    result_subject,
  )

  // Consume request
  let assert Ok(_) = process.receive(mock.writes, 500)

  // Inject error response
  let ctx = helpers.test_step(ctx, "inject_error_response")
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req_err\",\"error\":\"Permission mode not supported\"}}"
  bidir.inject_message(session, response_json)

  // Verify error response is received
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    actor.RequestError(msg) -> {
      should.equal(msg, "Permission mode not supported")
      helpers.log_info_with(ctx, "error_response_received", [
        #("error_msg", json.string(msg)),
      ])
      helpers.log_test_complete(ctx, True, "Error response properly surfaced")
    }
    _ -> {
      helpers.log_error(ctx, "unexpected_result", "Expected RequestError")
      helpers.log_test_complete(ctx, False, "Expected RequestError")
      should.fail()
    }
  }

  bidir.shutdown(session)
}

// ============================================================================
// SDK-41: Interrupt Signal
// ============================================================================

/// SDK-41: Interrupt signal sends correct wire format.
///
/// Tests that interrupt() sends the expected JSON structure
/// and handles CLI success response appropriately.
pub fn sdk_41_interrupt_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_41_interrupt")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)
  let ctx = helpers.test_step(ctx, "complete_init_handshake")
  complete_init(mock, session)
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Send interrupt request using async API
  let ctx = helpers.test_step(ctx, "send_interrupt_request")
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_int"), result_subject)

  // Verify wire format
  let ctx = helpers.test_step(ctx, "verify_wire_format")
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(request_json, "\"type\":\"control_request\""))
  should.be_true(string.contains(request_json, "\"subtype\":\"interrupt\""))
  should.be_true(string.contains(request_json, "\"request_id\":\"req_int\""))

  // Inject success response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_int\"}}"
  bidir.inject_message(session, response_json)

  // Verify success response is received
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> {
      helpers.log_info(ctx, "interrupt_wire_format_correct")
      helpers.log_test_complete(ctx, True, "interrupt sent correct wire format")
    }
    _ -> {
      helpers.log_error(ctx, "unexpected_result", "Expected RequestSuccess")
      helpers.log_test_complete(ctx, False, "Expected RequestSuccess")
      should.fail()
    }
  }

  // Verify session can still be stopped
  bidir.shutdown(session)
}

/// SDK-41b: Interrupt when no operation is running.
///
/// Verifies that interrupt returns appropriate error when
/// there's nothing to interrupt.
pub fn sdk_41b_interrupt_no_operation_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_41b_interrupt_no_op")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)
  let ctx = helpers.test_step(ctx, "complete_init_handshake")
  complete_init(mock, session)
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Send interrupt request
  let ctx = helpers.test_step(ctx, "send_interrupt_request")
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_noop"), result_subject)

  // Consume request
  let assert Ok(_) = process.receive(mock.writes, 500)

  // Inject error response (no operation to interrupt)
  let ctx = helpers.test_step(ctx, "inject_error_response")
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req_noop\",\"error\":\"No operation to interrupt\"}}"
  bidir.inject_message(session, response_json)

  // Verify error response is received
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    actor.RequestError(msg) -> {
      should.equal(msg, "No operation to interrupt")
      helpers.log_info_with(ctx, "interrupt_error_received", [
        #("error_msg", json.string(msg)),
      ])
      helpers.log_test_complete(ctx, True, "Interrupt error properly surfaced")
    }
    _ -> {
      helpers.log_error(ctx, "unexpected_result", "Expected RequestError")
      helpers.log_test_complete(ctx, False, "Expected RequestError")
      should.fail()
    }
  }

  bidir.shutdown(session)
}

// ============================================================================
// SDK-42: Hook Response Timeout
// ============================================================================

/// SDK-42: Hook response timeout handling.
///
/// Tests that slow hook handlers are timed out and fail-open behavior
/// is applied (allowing the operation to continue).
pub fn sdk_42_hook_timeout_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_42_hook_timeout")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  let mock = mock_bidir_runner.new()

  // Create hook config with slow handler (sleeps longer than timeout)
  let ctx = helpers.test_step(ctx, "configure_slow_hook")
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("slow_hook", fn(_input: Dynamic) -> Dynamic {
          // Sleep 500ms - longer than 100ms timeout
          process.sleep(500)
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use short hook timeout (100ms) for faster test
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 100,
      enable_file_checkpointing: False,
      mcp_servers: [],
      on_warning: None,
    )

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)
  let ctx = helpers.test_step(ctx, "complete_init_handshake")
  complete_init(mock, session)
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Send hook_callback that will trigger the slow handler
  let ctx = helpers.test_step(ctx, "trigger_slow_hook")
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_slow_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"slow_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Wait for response - should be fail-open (continue: true) due to timeout
  process.sleep(200)
  // Response should come within timeout + processing time
  let ctx = helpers.test_step(ctx, "verify_timeout_response")
  let assert Ok(first_json) = process.receive(mock.writes, 500)
  let response_json = case
    string.contains(first_json, "\"reason\":\"timeout\"")
  {
    True -> first_json
    False -> {
      let assert Ok(second_json) = process.receive(mock.writes, 500)
      second_json
    }
  }

  // Verify fail-open response was sent with correct fields
  should.be_true(string.contains(response_json, "cli_slow_1"))
  should.be_true(string.contains(response_json, "success"))
  // Verify fail-open semantics: continue must be explicitly true
  should.be_true(string.contains(response_json, "\"continue\":true"))
  // Verify timeout was the reason (proves timeout handler fired, not slow hook completing)
  should.be_true(string.contains(response_json, "\"reason\":\"timeout\""))

  helpers.log_info(ctx, "hook_timeout_fail_open_verified")
  helpers.log_test_complete(
    ctx,
    True,
    "Hook timeout handled with fail-open behavior",
  )

  bidir.shutdown(session)
}

// ============================================================================
// SDK-43: Malformed Control Response (Offline Test)
// ============================================================================

/// SDK-43: Malformed control response is handled gracefully.
///
/// This test uses mock runner - no E2E gate needed.
/// Verifies that malformed JSON in control responses doesn't crash the session.
pub fn sdk_43_malformed_response_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_43_malformed_response")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  // Note: This test uses mock runner - no E2E gate needed
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)
  let ctx = helpers.test_step(ctx, "complete_init_handshake")
  complete_init(mock, session)
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Inject malformed JSON - should be logged but not crash
  let ctx = helpers.test_step(ctx, "inject_malformed_json")
  bidir.inject_message(session, "{malformed json\n")
  process.sleep(50)

  // Session should still be running
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Inject another malformed message
  bidir.inject_message(session, "not json at all")
  process.sleep(50)

  // Session should still be running (graceful degradation)
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  helpers.log_info(ctx, "malformed_json_handled")

  // Verify we can still send valid control messages
  let ctx = helpers.test_step(ctx, "verify_session_functional")
  let result_subject = process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_valid", AcceptEdits),
    result_subject,
  )

  // Should still be able to receive messages on the wire
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(request_json, "set_permission_mode"))

  helpers.log_info(ctx, "session_functional_after_malformed")
  helpers.log_test_complete(
    ctx,
    True,
    "Malformed JSON handled without crash, session functional",
  )

  bidir.shutdown(session)
}

/// SDK-43b: Malformed JSON mixed with valid messages.
///
/// Verifies that valid messages are still processed even when
/// interspersed with malformed JSON.
pub fn sdk_43b_malformed_mixed_with_valid_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_43b_malformed_mixed")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)
  let ctx = helpers.test_step(ctx, "complete_init_handshake")
  complete_init(mock, session)
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Send a valid control request
  let ctx = helpers.test_step(ctx, "send_valid_request")
  let result_subject = process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_test", AcceptEdits),
    result_subject,
  )

  // Consume the request
  let assert Ok(_) = process.receive(mock.writes, 500)

  // Inject malformed JSON (should be ignored)
  let ctx = helpers.test_step(ctx, "inject_malformed_then_valid")
  bidir.inject_message(session, "{broken")
  process.sleep(10)

  // Inject valid success response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_test\"}}"
  bidir.inject_message(session, response_json)

  // Should receive success despite malformed JSON in between
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> {
      helpers.log_info(ctx, "valid_response_received_despite_malformed")
      helpers.log_test_complete(
        ctx,
        True,
        "Valid response received despite interleaved malformed JSON",
      )
    }
    _ -> {
      helpers.log_error(ctx, "unexpected_result", "Expected RequestSuccess")
      helpers.log_test_complete(ctx, False, "Expected RequestSuccess")
      should.fail()
    }
  }

  bidir.shutdown(session)
}

/// SDK-43c: Session stops cleanly after receiving malformed data.
///
/// Verifies that shutdown works properly even after malformed data injection.
pub fn sdk_43c_shutdown_after_malformed_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_43c_shutdown_after_malformed")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)
  let ctx = helpers.test_step(ctx, "complete_init_handshake")
  complete_init(mock, session)

  // Inject several malformed messages
  let ctx = helpers.test_step(ctx, "inject_malformed_messages")
  bidir.inject_message(session, "garbage")
  bidir.inject_message(session, "{incomplete")
  bidir.inject_message(session, "123")
  process.sleep(50)

  // Session should still be running
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Shutdown should work cleanly
  let ctx = helpers.test_step(ctx, "shutdown_session")
  bidir.shutdown(session)

  // Verify shutdown completed by checking actor process is no longer alive
  let pid = bidir.get_pid(session)
  // Give the actor time to process shutdown and stop
  process.sleep(100)
  should.equal(process.is_alive(pid), False)

  helpers.log_info(ctx, "clean_shutdown_verified")
  helpers.log_test_complete(ctx, True, "Clean shutdown after malformed data")
}

/// SDK-43d: Truncated JSON line handling.
///
/// Verifies truncated JSON lines are handled gracefully.
pub fn sdk_43d_truncated_json_test_() {
  use <- helpers.with_e2e_timeout()
  let ctx = helpers.new_test_context("sdk_43d_truncated_json")
  let ctx = helpers.test_step(ctx, "setup_mock_runner")

  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(mock.runner, config)
  let ctx = helpers.test_step(ctx, "complete_init_handshake")
  complete_init(mock, session)

  // Inject truncated JSON that looks like it might be valid but isn't
  let ctx = helpers.test_step(ctx, "inject_truncated_json")
  bidir.inject_message(session, "{\"type\":\"control_response\",\"response\":{")
  process.sleep(50)

  // Session should still be running
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  helpers.log_info(ctx, "truncated_json_handled")
  helpers.log_test_complete(ctx, True, "Truncated JSON handled gracefully")

  bidir.shutdown(session)
}
