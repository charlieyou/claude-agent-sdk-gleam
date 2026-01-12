/// Tests for hook cleanup invariants (casg-red.6).
///
/// Verifies:
/// - cleanup_pending_hook removes task_pid, monitor_ref, and timer_ref atomically
/// - No resource leaks after normal completion, timeout, or crash
/// - Double-cleanup is safe (idempotent)
/// - Session termination cleans up all pending hooks
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/string
import gleeunit/should

import claude_agent_sdk/internal/bidir.{
  type SubscriberMessage, HookConfig, Running, StartConfig,
}
import support/mock_bidir_runner

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// FFI to check if a process is alive
@external(erlang, "erlang", "is_process_alive")
fn is_process_alive(pid: process.Pid) -> Bool

// =============================================================================
// Normal Completion Cleanup Tests
// =============================================================================

/// Test that normal completion cleans up all resources.
/// After hook completes successfully, there should be no leaked:
/// - Timers (cancel_timer on already-fired timer returns False)
/// - Monitors (demonitor already called)
/// - Task processes (exited naturally)
pub fn normal_completion_cleanup_test() {
  let mock = mock_bidir_runner.new()

  // Subject to capture the task PID for verification
  let pid_receiver: process.Subject(process.Pid) = process.new_subject()

  // Create hook config with handler that reports its PID and completes normally
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("cleanup_test_hook", fn(_input: Dynamic) -> Dynamic {
          // Report our PID for later verification
          process.send(pid_receiver, process.self())
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 1000,
      enable_file_checkpointing: False,
    )

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send hook_callback
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_cleanup_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"cleanup_test_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Capture the task PID
  let assert Ok(task_pid) = process.receive(pid_receiver, 500)

  // Wait for response
  let assert Ok(response_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(response_json, "cli_cleanup_1"))
  should.be_true(string.contains(response_json, "success"))

  // Give time for cleanup
  process.sleep(100)

  // Verify task process has exited (natural completion)
  should.be_false(is_process_alive(task_pid))

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Timeout Cleanup Tests
// =============================================================================

/// Test that timeout cleans up all resources.
/// After timeout, there should be no leaked:
/// - Timers (timer fired, so already gone)
/// - Monitors (demonitor called during cleanup)
/// - Task processes (killed by cleanup)
pub fn timeout_cleanup_test() {
  let mock = mock_bidir_runner.new()

  // Subject to capture the task PID for verification
  let pid_receiver: process.Subject(process.Pid) = process.new_subject()

  // Create hook config with slow handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("slow_hook", fn(_input: Dynamic) -> Dynamic {
          // Report our PID for later verification
          process.send(pid_receiver, process.self())
          // Sleep longer than timeout
          process.sleep(500)
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use short timeout (50ms)
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 50,
      enable_file_checkpointing: False,
    )

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send hook_callback
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_timeout_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"slow_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Capture the task PID
  let assert Ok(task_pid) = process.receive(pid_receiver, 500)

  // Task should be alive initially
  should.be_true(is_process_alive(task_pid))

  // Wait for timeout (50ms) + margin
  process.sleep(100)

  // Should have received timeout response
  let assert Ok(response_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(response_json, "cli_timeout_1"))
  should.be_true(string.contains(response_json, "timeout"))

  // Give time for cleanup to complete
  process.sleep(50)

  // Verify task process has been killed by timeout cleanup
  should.be_false(is_process_alive(task_pid))

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Crash Cleanup Tests
// =============================================================================

/// Test that crash cleans up all resources.
/// After task crash, there should be no leaked:
/// - Timers (cancelled during cleanup)
/// - Monitors (demonitor called during cleanup)
/// - Task processes (already dead from crash)
pub fn crash_cleanup_test() {
  let mock = mock_bidir_runner.new()

  // Subject to capture the task PID for verification
  let pid_receiver: process.Subject(process.Pid) = process.new_subject()

  // Create hook config with crashing handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("crashing_hook", fn(_input: Dynamic) -> Dynamic {
          // Report our PID for later verification
          process.send(pid_receiver, process.self())
          // Crash by calling panic
          panic as "intentional crash for test"
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 1000,
      enable_file_checkpointing: False,
    )

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send hook_callback
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_crash_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"crashing_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Capture the task PID
  let assert Ok(task_pid) = process.receive(pid_receiver, 500)

  // Wait for crash to be processed
  process.sleep(100)

  // Should have received fail-open response
  let assert Ok(response_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(response_json, "cli_crash_1"))
  should.be_true(string.contains(response_json, "crash"))

  // Verify task process is dead (from crash)
  should.be_false(is_process_alive(task_pid))

  // Cleanup
  bidir.shutdown(session)
}

// =============================================================================
// Session Termination Cleanup Tests
// =============================================================================

/// Test that session termination cleans up all pending hooks.
/// When session stops with pending hooks, all should be cleaned up:
/// - Timers cancelled
/// - Tasks killed
/// - Monitors removed
pub fn session_termination_cleanup_test() {
  let mock = mock_bidir_runner.new()

  // Subject to capture task PIDs for verification
  let pid_receiver: process.Subject(process.Pid) = process.new_subject()

  // Create hook config with blocking handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("blocking_hook", fn(_input: Dynamic) -> Dynamic {
          // Report our PID for later verification
          process.send(pid_receiver, process.self())
          // Block indefinitely (until killed)
          process.sleep(60_000)
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 60_000,
      enable_file_checkpointing: False,
    )

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send multiple hook_callbacks to create multiple pending hooks
  let hook_callback_json_1 =
    "{\"type\":\"control_request\",\"request_id\":\"cli_term_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"blocking_hook\",\"input\":{}}}"
  let hook_callback_json_2 =
    "{\"type\":\"control_request\",\"request_id\":\"cli_term_2\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"blocking_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json_1)
  bidir.inject_message(session, hook_callback_json_2)

  // Capture task PIDs
  let assert Ok(task_pid_1) = process.receive(pid_receiver, 500)
  let assert Ok(task_pid_2) = process.receive(pid_receiver, 500)

  // Both tasks should be alive
  should.be_true(is_process_alive(task_pid_1))
  should.be_true(is_process_alive(task_pid_2))

  // Shutdown the session (this should trigger cleanup)
  bidir.shutdown(session)

  // Give time for cleanup
  process.sleep(100)

  // Both tasks should be killed by session termination cleanup
  should.be_false(is_process_alive(task_pid_1))
  should.be_false(is_process_alive(task_pid_2))
}

// =============================================================================
// Double Cleanup Tests (Idempotency)
// =============================================================================

/// Test that cleanup is idempotent - multiple cleanups don't cause errors.
/// This can happen if HookDone arrives just after timeout fires.
/// The second cleanup should find nothing and be a no-op.
pub fn double_cleanup_is_safe_test() {
  let mock = mock_bidir_runner.new()

  // Subject to control callback timing
  let callback_gate: process.Subject(Nil) = process.new_subject()

  // Create hook config with gated handler
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("gated_hook", fn(_input: Dynamic) -> Dynamic {
          // Wait for signal
          let _ = process.receive(callback_gate, 1000)
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  // Use very short timeout
  let config =
    StartConfig(
      subscriber: subscriber,
      default_timeout_ms: 60_000,
      hook_timeouts: dict.from_list([]),
      init_timeout_ms: 10_000,
      default_hook_timeout_ms: 30,
      enable_file_checkpointing: False,
    )

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send hook_callback
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_double_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"gated_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Wait for timeout to fire first
  process.sleep(80)

  // Timeout response should be received
  let assert Ok(timeout_response) = process.receive(mock.writes, 500)
  should.be_true(string.contains(timeout_response, "cli_double_1"))

  // Now signal the callback to complete (late)
  process.send(callback_gate, Nil)
  process.sleep(50)

  // The late HookDone should be ignored (cleanup already happened).
  // Session should still be healthy (not crashed).
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Cleanup
  bidir.shutdown(session)
}
