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
import claude_agent_sdk/internal/bidir.{
  type ActorMessage, type SubscriberMessage, Running,
}
import claude_agent_sdk/internal/bidir_runner
import support/mock_bidir_runner

/// Type alias for session reference (Subject(ActorMessage)).
type SessionRef =
  process.Subject(ActorMessage)

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
  should.equal(bidir.get_lifecycle(session, 1000), Running)

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
    bidir.RequestSuccess(_payload) -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Synchronous Public API Tests
// =============================================================================

/// Test: synchronous set_permission_mode returns Ok(Nil) on success
///
/// Uses auto-responding mock to test the blocking set_permission_mode() function.
pub fn set_permission_mode_sync_returns_ok_on_success_test() {
  // Create auto-responding mock that responds to set_permission_mode requests
  let #(mock, session_ref) =
    create_auto_respond_mock(fn(json) {
      case string.contains(json, "set_permission_mode") {
        True -> {
          // Extract request_id and return success
          case extract_request_id(json) {
            Ok(req_id) ->
              Ok(
                "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\""
                <> req_id
                <> "\"}}",
              )
            Error(_) -> Error(Nil)
          }
        }
        False -> Error(Nil)
      }
    })

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Store session reference for auto-responder
  process.send(session_ref, session)

  // Complete init
  let assert Ok(_) = process.receive(mock.writes, 500)
  bidir.inject_message(
    session,
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}",
  )
  process.sleep(50)

  // Act: call the synchronous set_permission_mode function
  let result = bidir.set_permission_mode(session, AcceptEdits)

  // Assert: returns Ok(Nil)
  should.equal(result, Ok(Nil))

  // Cleanup
  bidir.shutdown(session)
}

/// Test: synchronous set_permission_mode returns error on CLI error response
pub fn set_permission_mode_sync_returns_error_on_cli_error_test() {
  // Create auto-responding mock that returns error
  let #(mock, session_ref) =
    create_auto_respond_mock(fn(json) {
      case string.contains(json, "set_permission_mode") {
        True -> {
          case extract_request_id(json) {
            Ok(req_id) ->
              Ok(
                "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_id\":\""
                <> req_id
                <> "\",\"message\":\"Invalid mode\"}}",
              )
            Error(_) -> Error(Nil)
          }
        }
        False -> Error(Nil)
      }
    })

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)
  let assert Ok(session) = bidir.start(mock.runner, config)

  process.send(session_ref, session)

  // Complete init
  let assert Ok(_) = process.receive(mock.writes, 500)
  bidir.inject_message(
    session,
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}",
  )
  process.sleep(50)

  // Act: call the synchronous set_permission_mode function
  let result = bidir.set_permission_mode(session, Default)

  // Assert: returns SetPermissionModeCliError
  case result {
    Error(bidir.SetPermissionModeCliError(msg)) ->
      should.equal(msg, "Invalid mode")
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

/// Test: synchronous set_permission_mode times out when no response
pub fn set_permission_mode_sync_timeout_test() {
  // Create mock that doesn't respond to set_permission_mode
  let mock = mock_bidir_runner.new()
  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use short timeout for faster test
  let config =
    bidir.StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 100,
      hook_timeouts: dict.new(),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30_000,
    )
  let assert Ok(session) = bidir.start(mock.runner, config)

  // Complete init
  let assert Ok(_) = process.receive(mock.writes, 500)
  bidir.inject_message(
    session,
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{}}}}",
  )
  process.sleep(50)

  // Act: call set_permission_mode but don't respond - will timeout at 5000ms
  // We can't easily test the full 5000ms timeout, so we verify the function exists
  // and the timeout mechanism works via the actor's default_timeout_ms (set to 100ms)
  let result = bidir.set_permission_mode(session, AcceptEdits)

  // Assert: returns timeout error (either from actor timeout or local timeout)
  case result {
    Error(bidir.SetPermissionModeTimeout) -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  bidir.shutdown(session)
}

/// Test: synchronous set_permission_mode error type variants
pub fn set_permission_mode_error_type_test() {
  // This test verifies the error type variants exist and pattern match correctly
  let _err1: bidir.SetPermissionModeError =
    bidir.SetPermissionModeCliError("test")
  let _err2: bidir.SetPermissionModeError = bidir.SetPermissionModeTimeout
  let _err3: bidir.SetPermissionModeError =
    bidir.SetPermissionModeSessionStopped

  let check_variant = fn(err: bidir.SetPermissionModeError) {
    case err {
      bidir.SetPermissionModeCliError(_) -> "cli_error"
      bidir.SetPermissionModeTimeout -> "timeout"
      bidir.SetPermissionModeSessionStopped -> "stopped"
    }
  }

  should.equal(check_variant(bidir.SetPermissionModeCliError("x")), "cli_error")
  should.equal(check_variant(bidir.SetPermissionModeTimeout), "timeout")
  should.equal(check_variant(bidir.SetPermissionModeSessionStopped), "stopped")
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

/// Create a mock runner that auto-responds to requests.
///
/// The respond_fn receives the written JSON and returns either:
/// - Ok(response_json) to inject a response
/// - Error(Nil) to not respond
///
/// Returns the mock and a subject to store the session reference.
fn create_auto_respond_mock(
  respond_fn: fn(String) -> Result(String, Nil),
) -> #(mock_bidir_runner.MockRunner, process.Subject(SessionRef)) {
  let writes = process.new_subject()
  let closed = process.new_subject()
  let session_holder: process.Subject(SessionRef) = process.new_subject()

  // Capture respond_fn and session_holder for the write callback
  let respond = respond_fn
  let holder = session_holder

  let runner =
    bidir_runner.mock(
      on_write: fn(data) {
        process.send(writes, data)
        // Try to auto-respond
        case respond(data) {
          Ok(response) -> {
            // Get session from holder (non-blocking)
            case process.receive(holder, 0) {
              Ok(session) -> {
                // Put it back for future uses
                process.send(holder, session)
                // Inject response
                bidir.inject_message(session, response)
              }
              Error(_) -> Nil
            }
          }
          Error(_) -> Nil
        }
        Ok(Nil)
      },
      on_close: fn() { process.send(closed, True) },
    )

  let mock =
    mock_bidir_runner.MockRunner(runner: runner, writes: writes, closed: closed)
  #(mock, session_holder)
}

/// Extract request_id from JSON string.
/// Looks for "request_id":"<id>" pattern.
fn extract_request_id(json: String) -> Result(String, Nil) {
  case string.split_once(json, "\"request_id\":\"") {
    Ok(#(_, after)) ->
      case string.split_once(after, "\"") {
        Ok(#(id, _)) -> Ok(id)
        Error(_) -> Error(Nil)
      }
    Error(_) -> Error(Nil)
  }
}
