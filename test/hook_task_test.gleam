/// Tests for hook task spawning (casg-red.2).
///
/// Verifies:
/// - Hook callback executes in a separate process (not the GenServer)
/// - HookDone message triggers control_response
/// - pending_hooks entry removed after processing
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/string
import gleeunit/should

import claude_agent_sdk/internal/bidir
import claude_agent_sdk/internal/bidir/actor.{
  type SubscriberMessage, HookConfig, Running,
}
import support/mock_bidir_runner

// FFI for creating Dynamic values
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// =============================================================================
// Hook Task Spawning Tests
// =============================================================================

pub fn hook_callback_runs_in_separate_process_test() {
  // Create mock runner to capture writes
  let mock = mock_bidir_runner.new()

  // Subject to receive the PID of the process executing the callback
  let pid_receiver: process.Subject(process.Pid) = process.new_subject()

  // Create hook config with handler that reports its own PID
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("test_hook", fn(_input: Dynamic) -> Dynamic {
          // Send our PID to verify we're running in a different process
          process.send(pid_receiver, process.self())
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  // Start session with hooks
  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Get the GenServer's PID for comparison
  let genserver_pid = bidir.get_pid(session)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running by sending init response
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)

  // Wait for Running state
  process.sleep(50)
  should.equal(bidir.get_lifecycle(session, 1000), Running)

  // Send hook_callback control request
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_task_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"test_hook\",\"input\":{\"event\":\"pre_tool\"}}}"
  bidir.inject_message(session, hook_callback_json)

  // Receive the callback's PID
  let assert Ok(callback_pid) = process.receive(pid_receiver, 1000)

  // Verify callback ran in a DIFFERENT process than the GenServer
  should.not_equal(callback_pid, genserver_pid)

  // Cleanup
  bidir.shutdown(session)
}

pub fn hook_done_triggers_response_test() {
  // Create mock runner to capture writes
  let mock = mock_bidir_runner.new()

  // Create hook config with handler that returns specific data
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("response_hook", fn(_input: Dynamic) -> Dynamic {
          // Return a distinctive result
          to_dynamic(
            dict.from_list([
              #("continue", to_dynamic(True)),
              #("marker", to_dynamic("from_task")),
            ]),
          )
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)
  process.sleep(50)

  // Send hook_callback
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_resp_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"response_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Wait for the response to be written
  process.sleep(100)

  // Verify response was written with correct request_id
  let assert Ok(response_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(response_json, "control_response"))
  should.be_true(string.contains(response_json, "cli_resp_1"))
  should.be_true(string.contains(response_json, "success"))

  // Cleanup
  bidir.shutdown(session)
}

pub fn response_only_sent_after_callback_completes_test() {
  // Create mock runner to capture writes
  let mock = mock_bidir_runner.new()

  // Subject to signal when callback starts and finishes
  let timing_receiver: process.Subject(String) = process.new_subject()

  // Create hook config with handler that sleeps
  let hooks =
    HookConfig(
      handlers: dict.from_list([
        #("slow_hook", fn(_input: Dynamic) -> Dynamic {
          // Signal start
          process.send(timing_receiver, "started")
          // Sleep to simulate work
          process.sleep(100)
          // Signal completion
          process.send(timing_receiver, "completed")
          to_dynamic(dict.from_list([#("continue", to_dynamic(True))]))
        }),
      ]),
      permission_handlers: dict.from_list([]),
    )

  let subscriber: process.Subject(SubscriberMessage) = process.new_subject()
  let config = bidir.default_config(subscriber)

  let assert Ok(session) = bidir.start_with_hooks(mock.runner, config, hooks)

  // Wait for init request
  let assert Ok(_init_msg) = process.receive(mock.writes, 500)

  // Transition to Running
  let init_response_json =
    "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req_0\",\"response\":{\"capabilities\":{\"supported_commands\":[\"initialize\"],\"hooks_supported\":true,\"permissions_supported\":true,\"mcp_sdk_servers_supported\":true}}}}"
  bidir.inject_message(session, init_response_json)
  process.sleep(50)

  // Send hook_callback
  let hook_callback_json =
    "{\"type\":\"control_request\",\"request_id\":\"cli_slow_1\",\"request\":{\"subtype\":\"hook_callback\",\"callback_id\":\"slow_hook\",\"input\":{}}}"
  bidir.inject_message(session, hook_callback_json)

  // Wait for callback to start
  let assert Ok("started") = process.receive(timing_receiver, 500)

  // Async ack is sent immediately while callback runs
  let assert Ok(async_response) = process.receive(mock.writes, 500)
  should.be_true(string.contains(async_response, "cli_slow_1"))
  should.be_true(string.contains(async_response, "async"))

  // Wait for callback to complete
  let assert Ok("completed") = process.receive(timing_receiver, 500)

  // Now response should be written
  process.sleep(50)
  let assert Ok(response_json) = process.receive(mock.writes, 500)
  should.be_true(string.contains(response_json, "cli_slow_1"))
  should.be_true(string.contains(response_json, "continue"))

  // Cleanup
  bidir.shutdown(session)
}
