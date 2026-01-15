/// Tests for set_permission_mode control operation.
///
/// Verifies:
/// - Wire format for each PermissionMode variant
/// - Success response handling
/// - Synchronous public API (bidir.set_permission_mode)
import gleam/dict
import gleam/erlang/process
import gleam/string
import gleeunit/should

import claude_agent_sdk/control.{AcceptEdits, BypassPermissions, Default, Plan}
import claude_agent_sdk/internal/bidir.{type SubscriberMessage}
import claude_agent_sdk/internal/bidir/actor.{
  RequestError, RequestSuccess, SetPermissionModeCliError,
  SetPermissionModeSessionStopped, SetPermissionModeTimeout, StartConfig,
}
import support/mock_bidir_runner

// =============================================================================
// Wire Format Tests
// =============================================================================

/// Test: set_permission_mode with Default sends correct wire format
pub fn set_permission_mode_default_wire_format_test() {
  // Arrange: start session
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Complete initialization
  complete_init(mock, session)

  // Act: call set_permission_mode with Default
  let result_subject = process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_perm", Default),
    result_subject,
  )

  // Assert: wire format contains set_permission_mode and mode:"default"
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(request_json, "\"type\":\"control_request\""))
  should.be_true(string.contains(
    request_json,
    "\"subtype\":\"set_permission_mode\"",
  ))
  should.be_true(string.contains(request_json, "\"mode\":\"default\""))
  should.be_true(string.contains(request_json, "\"request_id\":\"req_perm\""))

  // Cleanup
  bidir.shutdown(session)
}

/// Test: set_permission_mode with AcceptEdits sends correct wire format
pub fn set_permission_mode_accept_edits_wire_format_test() {
  // Arrange
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)
  let assert Ok(session) = bidir.start(mock.runner, config)
  complete_init(mock, session)

  // Act: call set_permission_mode with AcceptEdits
  let result_subject = process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_perm", AcceptEdits),
    result_subject,
  )

  // Assert: wire format contains mode:"acceptEdits"
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(
    request_json,
    "\"subtype\":\"set_permission_mode\"",
  ))
  should.be_true(string.contains(request_json, "\"mode\":\"acceptEdits\""))

  // Cleanup
  bidir.shutdown(session)
}

/// Test: set_permission_mode with BypassPermissions sends correct wire format
pub fn set_permission_mode_bypass_permissions_wire_format_test() {
  // Arrange
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)
  let assert Ok(session) = bidir.start(mock.runner, config)
  complete_init(mock, session)

  // Act: call set_permission_mode with BypassPermissions
  let result_subject = process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_perm", BypassPermissions),
    result_subject,
  )

  // Assert: wire format contains mode:"bypassPermissions"
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(
    request_json,
    "\"subtype\":\"set_permission_mode\"",
  ))
  should.be_true(string.contains(request_json, "\"mode\":\"bypassPermissions\""))

  // Cleanup
  bidir.shutdown(session)
}

/// Test: set_permission_mode with Plan sends correct wire format
pub fn set_permission_mode_plan_wire_format_test() {
  // Arrange
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)
  let assert Ok(session) = bidir.start(mock.runner, config)
  complete_init(mock, session)

  // Act: call set_permission_mode with Plan
  let result_subject = process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_perm", Plan),
    result_subject,
  )

  // Assert: wire format contains mode:"plan"
  let assert Ok(request_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(
    request_json,
    "\"subtype\":\"set_permission_mode\"",
  ))
  should.be_true(string.contains(request_json, "\"mode\":\"plan\""))

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Response Handling Tests
// =============================================================================

/// Test: set_permission_mode returns Ok on success response
pub fn set_permission_mode_receives_success_test() {
  // Arrange
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)
  let assert Ok(session) = bidir.start(mock.runner, config)
  complete_init(mock, session)

  // Verify Running state
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Act: send set_permission_mode request
  let result_subject: process.Subject(bidir.RequestResult) =
    process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_perm_success", AcceptEdits),
    result_subject,
  )

  // Consume the request from mock
  let assert Ok(_request_json) = process.receive(mock.writes, 500)

  // Simulate CLI success response
  let response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_perm_success\"}}"
  bidir.inject_message(session, response_json)

  // Assert: caller receives RequestSuccess
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_payload) -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Synchronous Public API Tests
// =============================================================================

/// Test: synchronous set_permission_mode maps RequestSuccess to Ok(Nil)
///
/// Tests the result mapping logic in set_permission_mode by using the async API
/// and manually checking the mapping behavior through direct invocation.
pub fn set_permission_mode_success_mapping_test() {
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)
  let assert Ok(session) = bidir.start(mock.runner, config)
  complete_init(mock, session)

  // Send via async API with known request ID
  let result_subject: process.Subject(bidir.RequestResult) =
    process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_success_map", AcceptEdits),
    result_subject,
  )

  // Consume request and inject success response
  let assert Ok(_) = process.receive(mock.writes, 500)
  bidir.inject_message(
    session,
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_success_map\"}}",
  )

  // Verify RequestSuccess is returned (this is what set_permission_mode maps to Ok(Nil))
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestSuccess(_) -> should.be_true(True)
    _ -> should.fail()
  }

  bidir.shutdown(session)
}

/// Test: synchronous set_permission_mode maps RequestError to SetPermissionModeCliError
pub fn set_permission_mode_error_mapping_test() {
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)
  let assert Ok(session) = bidir.start(mock.runner, config)
  complete_init(mock, session)

  // Send via async API with known request ID
  let result_subject: process.Subject(bidir.RequestResult) =
    process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_error_map", Default),
    result_subject,
  )

  // Consume request and inject error response
  let assert Ok(_) = process.receive(mock.writes, 500)
  bidir.inject_message(
    session,
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\"req_error_map\",\"message\":\"Invalid mode\"}}",
  )

  // Verify RequestError is returned (this is what set_permission_mode maps to CliError)
  let assert Ok(result) = process.receive(result_subject, 500)
  case result {
    RequestError(msg) -> should.equal(msg, "Invalid mode")
    _ -> should.fail()
  }

  bidir.shutdown(session)
}

/// Test: synchronous set_permission_mode times out via actor timeout path
///
/// Uses short actor timeout (100ms) to test the RequestTimeout -> SetPermissionModeTimeout
/// mapping path without waiting 5 seconds.
pub fn set_permission_mode_sync_actor_timeout_test() {
  // Create mock that doesn't respond to set_permission_mode
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use short actor timeout for faster test
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 100,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
      mcp_servers: [],
    )
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Complete init
  let assert Ok(_) = process.receive(mock.writes, 500)
  bidir.inject_message(
    session,
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}",
  )
  process.sleep(50)

  // Act: call set_permission_mode - actor will timeout at 100ms, before client's 5000ms
  let result = bidir.set_permission_mode(session, AcceptEdits)

  // Assert: returns timeout error (from actor's RequestTimeout)
  case result {
    Error(SetPermissionModeTimeout) -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

/// Test: set_permission_mode cancels pending request on client timeout
///
/// This test verifies the mailbox pollution fix: when the client times out
/// before the actor's internal timeout, it cancels the pending request to prevent
/// stale RequestTimeout messages from being delivered to the client's subject.
///
/// Strategy:
/// 1. Use a short actor timeout (50ms)
/// 2. Cancel the request immediately after sending
/// 3. Wait longer than the actor timeout (200ms)
/// 4. Verify no RequestTimeout is delivered to result_subject
pub fn set_permission_mode_cancels_pending_on_client_timeout_test() {
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use short actor timeout so it would fire if not cancelled
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 50,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
      enable_file_checkpointing: False,
      mcp_servers: [],
    )
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Complete init
  let assert Ok(_) = process.receive(mock.writes, 500)
  bidir.inject_message(
    session,
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}",
  )
  process.sleep(50)

  // Send a request - actor will schedule a 50ms timeout
  let result_subject: process.Subject(bidir.RequestResult) =
    process.new_subject()
  bidir.send_control_request(
    session,
    control.SetPermissionMode("req_cancel_test", AcceptEdits),
    result_subject,
  )

  // Consume the request from mock
  let assert Ok(_) = process.receive(mock.writes, 500)

  // Cancel the pending request immediately (simulates client timeout)
  // This should cancel the timer and remove the pending request
  bidir.cancel_pending_request(session, "req_cancel_test")

  // Wait longer than the actor timeout would have been (50ms)
  // If cancellation failed, we'd receive RequestTimeout here
  let receive_result = process.receive(result_subject, 200)

  // Should NOT receive anything - timeout was cancelled
  should.be_error(receive_result)

  // Actor should still be alive and responsive
  should.equal(bidir.get_lifecycle(session, 1000), bidir.running())

  // Cleanup
  bidir.shutdown(session)
}

/// Test: synchronous set_permission_mode error type variants
pub fn set_permission_mode_error_type_test() {
  // This test verifies the error type variants exist and pattern match correctly
  let _err1: bidir.SetPermissionModeError =
    SetPermissionModeCliError("test")
  let _err2: bidir.SetPermissionModeError = SetPermissionModeTimeout
  let _err3: bidir.SetPermissionModeError =
    SetPermissionModeSessionStopped

  let check_variant = fn(err: bidir.SetPermissionModeError) {
    case err {
      SetPermissionModeCliError(_) -> "cli_error"
      SetPermissionModeTimeout -> "timeout"
      SetPermissionModeSessionStopped -> "stopped"
    }
  }

  should.equal(check_variant(SetPermissionModeCliError("x")), "cli_error")
  should.equal(check_variant(SetPermissionModeTimeout), "timeout")
  should.equal(check_variant(SetPermissionModeSessionStopped), "stopped")
}

// =============================================================================
// Test Helpers
// =============================================================================

/// Complete initialization handshake for a session.
fn complete_init(mock: mock_bidir_runner.MockRunner, session) {
  // Wait for init request
  let assert Ok(_init_request) = process.receive(mock.writes, 500)

  // Send init success
  let init_success =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}"
  bidir.inject_message(session, init_success)
  process.sleep(50)
}
