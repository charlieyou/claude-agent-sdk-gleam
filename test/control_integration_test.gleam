/// Control operations integration tests using FullMockRunner.
///
/// Tests all control operations (interrupt, set_permission_mode, set_model, rewind_files)
/// through the full mock runner to verify correct message encoding and response handling.
///
/// Unlike the unit tests in *_test.gleam files that use mock_bidir_runner,
/// these tests use full_mock_runner which provides complete protocol simulation.
///
/// Wire formats:
/// - interrupt: {"type":"control_request","request_id":"...","request":{"subtype":"interrupt"}}
/// - set_permission_mode: {"type":"control_request","request_id":"...","request":{"subtype":"set_permission_mode","mode":"..."}}
/// - set_model: {"type":"control_request","request_id":"...","request":{"subtype":"set_model","model":"..."}}
/// - rewind_files: {"type":"control_request","request_id":"...","request":{"subtype":"rewind_files","user_message_id":"..."}}
///
/// Response format:
/// {"type":"control_response","response":{"subtype":"success","request_id":"..."}}
/// {"type":"control_response","response":{"subtype":"error","request_id":"...","error":"..."}}
import gleam/dict
import gleam/erlang/process
import gleam/string
import gleeunit/should

import claude_agent_sdk/control.{
  AcceptEdits, BypassPermissions, Default, Interrupt, Plan, RewindFiles,
  SetModel, SetPermissionMode,
}
import claude_agent_sdk/internal/bidir.{
  type RequestResult, type SubscriberMessage, CheckpointingNotEnabled,
  RequestError, RequestSuccess, Running,
}
import support/full_mock_runner

// =============================================================================
// Test: Interrupt Flow
// =============================================================================

/// Test: interrupt sends correct control_request and receives ack via FullMockRunner
///
/// Verifies:
/// 1. interrupt control_request has operation="interrupt"
/// 2. Response is correctly correlated
pub fn test_interrupt_flow_test() {
  // Arrange: start session with full mock runner
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init handshake
  let assert Ok(_init) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Act: send interrupt using async API
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_int_1"), result_subject)

  // Assert: mock runner receives interrupt control_request
  let assert Ok(request_json) = process.receive(adapter.captured_writes, 500)
  should.be_true(string.contains(request_json, "\"type\":\"control_request\""))
  should.be_true(string.contains(request_json, "\"subtype\":\"interrupt\""))
  should.be_true(string.contains(request_json, "\"request_id\":\"req_int_1\""))

  // Send success response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_int_1\"}}"
  bidir.inject_message(session, response_json)

  // Assert: caller receives success
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> Nil
    _ -> should.fail()
  }

  bidir.shutdown(session)
}

// =============================================================================
// Test: Set Permission Mode Flow
// =============================================================================

/// Test: set_permission_mode encodes all modes correctly via FullMockRunner
///
/// Verifies each PermissionMode variant encodes to correct wire format
pub fn test_set_permission_mode_flow_test() {
  test_permission_mode(Default, "default")
  test_permission_mode(AcceptEdits, "acceptEdits")
  test_permission_mode(BypassPermissions, "bypassPermissions")
  test_permission_mode(Plan, "plan")
}

fn test_permission_mode(
  mode: control.PermissionMode,
  expected_mode_str: String,
) -> Nil {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init
  let assert Ok(_init) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Act: send set_permission_mode using async API
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    SetPermissionMode("req_perm_1", mode),
    result_subject,
  )

  // Assert: mock runner receives set_permission_mode control_request
  let assert Ok(request_json) = process.receive(adapter.captured_writes, 500)
  should.be_true(string.contains(request_json, "\"type\":\"control_request\""))
  should.be_true(string.contains(
    request_json,
    "\"subtype\":\"set_permission_mode\"",
  ))
  should.be_true(string.contains(
    request_json,
    "\"mode\":\"" <> expected_mode_str <> "\"",
  ))
  should.be_true(string.contains(request_json, "\"request_id\":\"req_perm_1\""))

  // Send success response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_perm_1\"}}"
  bidir.inject_message(session, response_json)

  // Assert: returns success
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> Nil
    _ -> should.fail()
  }

  bidir.shutdown(session)
}

// =============================================================================
// Test: Set Model Flow
// =============================================================================

/// Test: set_model encodes model correctly via FullMockRunner
///
/// Verifies model string is correctly encoded in wire format
pub fn test_set_model_flow_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init
  let assert Ok(_init) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Act: send set_model using async API
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    SetModel("req_model_1", "opus"),
    result_subject,
  )

  // Assert: mock runner receives set_model control_request
  let assert Ok(request_json) = process.receive(adapter.captured_writes, 500)
  should.be_true(string.contains(request_json, "\"type\":\"control_request\""))
  should.be_true(string.contains(request_json, "\"subtype\":\"set_model\""))
  should.be_true(string.contains(request_json, "\"model\":\"opus\""))
  should.be_true(string.contains(request_json, "\"request_id\":\"req_model_1\""))

  // Send success response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_model_1\"}}"
  bidir.inject_message(session, response_json)

  // Assert: returns success
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> Nil
    _ -> should.fail()
  }

  bidir.shutdown(session)
}

// =============================================================================
// Test: Rewind Files with Checkpointing
// =============================================================================

/// Test: rewind_files sends correct request when checkpointing is enabled
///
/// Verifies:
/// 1. With checkpointing enabled, rewind_files sends control_request
/// 2. Request includes user_message_id correctly
pub fn test_rewind_files_with_checkpointing_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()

  // Config WITH checkpointing enabled
  let config =
    bidir.StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: True,
      mcp_servers: [],
    )

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init
  let assert Ok(_init) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Act: send rewind_files using async API
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(
    session,
    RewindFiles("req_rewind_1", "msg_123"),
    result_subject,
  )

  // Assert: mock runner receives rewind_files control_request
  let assert Ok(request_json) = process.receive(adapter.captured_writes, 500)
  should.be_true(string.contains(request_json, "\"type\":\"control_request\""))
  should.be_true(string.contains(request_json, "\"subtype\":\"rewind_files\""))
  should.be_true(string.contains(
    request_json,
    "\"user_message_id\":\"msg_123\"",
  ))
  should.be_true(string.contains(
    request_json,
    "\"request_id\":\"req_rewind_1\"",
  ))

  // Send success response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_rewind_1\"}}"
  bidir.inject_message(session, response_json)

  // Assert: returns success
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> Nil
    _ -> should.fail()
  }

  bidir.shutdown(session)
}

// =============================================================================
// Test: Rewind Files Without Checkpointing Fails
// =============================================================================

/// Test: rewind_files fails immediately when checkpointing is not enabled
///
/// Verifies:
/// 1. Without checkpointing, rewind_files returns CheckpointingNotEnabled
/// 2. No request is sent to CLI
pub fn test_rewind_files_without_checkpointing_fails_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()

  // Config WITHOUT checkpointing (default)
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init
  let assert Ok(_init) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Act: call rewind_files (should fail immediately)
  let result = bidir.rewind_files(session, "msg_123", 5000)

  // Assert: returns CheckpointingNotEnabled
  should.equal(result, Error(CheckpointingNotEnabled))

  // Assert: no request was sent (only init request should be in the captured writes)
  case process.receive(adapter.captured_writes, 100) {
    Error(Nil) -> Nil
    // Expected: no additional writes
    Ok(_) -> should.fail()
    // Unexpected: a request was sent
  }

  bidir.shutdown(session)
}

// =============================================================================
// Test: Control Request ID Correlation
// =============================================================================

/// Test: Each control operation has unique request_id and responses correlate correctly
///
/// Verifies:
/// 1. Two control operations can be sent with different request IDs
/// 2. Responses are correctly correlated by request_id (even out of order)
pub fn test_control_request_id_correlation_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init
  let assert Ok(_init) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Act: send two control operations with different IDs
  let result1_subject: process.Subject(RequestResult) = process.new_subject()
  let result2_subject: process.Subject(RequestResult) = process.new_subject()

  bidir.send_control_request(session, Interrupt("req_corr_1"), result1_subject)
  bidir.send_control_request(
    session,
    SetModel("req_corr_2", "sonnet"),
    result2_subject,
  )

  // Capture both requests
  let assert Ok(request1_json) = process.receive(adapter.captured_writes, 500)
  let assert Ok(request2_json) = process.receive(adapter.captured_writes, 500)

  // Verify different request IDs
  should.be_true(string.contains(request1_json, "\"request_id\":\"req_corr_1\""))
  should.be_true(string.contains(request2_json, "\"request_id\":\"req_corr_2\""))

  // Send responses in REVERSE order to test correlation
  let response2_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_corr_2\"}}"
  bidir.inject_message(session, response2_json)

  let response1_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_corr_1\"}}"
  bidir.inject_message(session, response1_json)

  // Assert: both operations succeed (correlation worked)
  let assert Ok(result1) = process.receive(result1_subject, 500)
  let assert Ok(result2) = process.receive(result2_subject, 500)

  case result1 {
    RequestSuccess(_) -> Nil
    _ -> should.fail()
  }

  case result2 {
    RequestSuccess(_) -> Nil
    _ -> should.fail()
  }

  bidir.shutdown(session)
}

// =============================================================================
// Test: Control Operation Error Response
// =============================================================================

/// Test: Control operation handles CLI error response correctly
///
/// Verifies error response is correctly propagated to caller
pub fn test_control_error_response_test() {
  let mock =
    full_mock_runner.new()
    |> full_mock_runner.with_auto_init_ack()

  let adapter = full_mock_runner.start(mock)
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start(adapter.bidir_runner, config)
  let adapter = full_mock_runner.set_session(adapter, session)

  // Complete init
  let assert Ok(_init) = process.receive(adapter.captured_writes, 500)
  let _adapter = full_mock_runner.process_init(adapter)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Act: send interrupt request
  let result_subject: process.Subject(RequestResult) = process.new_subject()
  bidir.send_control_request(session, Interrupt("req_err_1"), result_subject)

  // Capture request
  let assert Ok(_request_json) = process.receive(adapter.captured_writes, 500)

  // Send error response
  let error_response =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req_err_1\",\"error\":\"Nothing to interrupt\"}}"
  bidir.inject_message(session, error_response)

  // Assert: returns Error with CLI message
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestError(msg) ->
      should.be_true(string.contains(msg, "Nothing to interrupt"))
    _ -> should.fail()
  }

  bidir.shutdown(session)
}
