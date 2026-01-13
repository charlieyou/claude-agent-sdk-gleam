/// Shared helpers for E2E SDK tests.
///
/// This module provides common functions for:
/// - Test skipping based on CLI flags
/// - Stream consumption utilities
/// - Protocol invariant assertions
import claude_agent_sdk
import claude_agent_sdk/error.{EndOfStream, Message, WarningEvent}
import claude_agent_sdk/message.{
  type MessageEnvelope, Assistant, Result, System, User,
}
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}

/// Convert Result to Option (helper to avoid qualified module reference).
fn option_from_result(result: Result(a, b)) -> Option(a) {
  case result {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

/// Check if E2E SDK tests are enabled via --e2e flag.
/// Returns Ok(Nil) if present, Error with skip message otherwise.
pub fn skip_if_no_e2e() -> Result(Nil, String) {
  case has_e2e_flag() {
    True -> Ok(Nil)
    False -> Error("[SKIP] --e2e flag not provided; skipping E2E test")
  }
}

/// Check if --e2e is set (opt-in for real CLI tests).
/// Returns Ok(Nil) if enabled, Error with skip message otherwise.
pub fn skip_if_no_cli_e2e() -> Result(Nil, String) {
  skip_if_no_e2e()
}

/// Check if real Claude CLI is available for E2E testing.
/// Returns True if both:
/// - --e2e flag is set (opt-in)
/// - 'claude' executable exists in PATH
pub fn is_cli_available() -> Bool {
  has_e2e_flag() && cli_exists_in_path()
}

/// Check if 'claude' executable exists in PATH using os:find_executable.
fn cli_exists_in_path() -> Bool {
  let name_charlist = binary_to_list("claude")
  let result = os_find_executable(name_charlist)
  case dynamic.classify(result) {
    "Bool" -> False
    // Non-false means path was found (charlist)
    _ -> True
  }
}

/// Raw FFI to Erlang's os:find_executable/1.
/// Takes charlist, returns charlist (absolute path) or false.
@external(erlang, "os", "find_executable")
fn os_find_executable(name: dynamic.Dynamic) -> dynamic.Dynamic

/// Convert binary (Gleam string) to charlist for Erlang interop.
@external(erlang, "erlang", "binary_to_list")
fn binary_to_list(binary: String) -> dynamic.Dynamic

/// Return plain command-line arguments (after --).
@external(erlang, "e2e_helpers_ffi", "get_plain_args")
fn get_plain_args() -> dynamic.Dynamic

/// Kill a process immediately.
@external(erlang, "e2e_helpers_ffi", "kill_pid")
fn kill_pid(pid: process.Pid) -> Nil

/// Acquire global lock for serialized CLI queries.
@external(erlang, "e2e_helpers_ffi", "acquire_lock")
fn acquire_query_lock() -> Nil

/// Release global lock for serialized CLI queries.
@external(erlang, "e2e_helpers_ffi", "release_lock")
fn release_query_lock() -> Nil

fn has_e2e_flag() -> Bool {
  case decode.run(get_plain_args(), decode.list(decode.string)) {
    Ok(args) -> list.any(args, fn(arg) { arg == "--e2e" })
    Error(_) -> False
  }
}

/// Result of consuming a stream.
pub type ConsumeResult {
  ConsumeResult(
    /// All message envelopes collected (in order received).
    messages: List(MessageEnvelope),
    /// True if stream terminated normally (EndOfStream).
    terminated_normally: Bool,
  )
}

/// Result of running a query with a timeout guard.
pub type QueryOutcome {
  QuerySuccess(ConsumeResult)
  QueryFailure(claude_agent_sdk.QueryError)
  QueryTimedOut
}

/// Run query() + consume_stream() in a spawned process with a timeout.
/// Ensures tests don't hang if the CLI stops responding.
pub fn query_and_consume_with_timeout(
  prompt: String,
  options: claude_agent_sdk.QueryOptions,
  timeout_ms: Int,
) -> QueryOutcome {
  let _ = acquire_query_lock()
  let subject: process.Subject(QueryOutcome) = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let outcome = case claude_agent_sdk.query(prompt, options) {
        Ok(stream) -> QuerySuccess(consume_stream(stream))
        Error(err) -> QueryFailure(err)
      }
      process.send(subject, outcome)
    })

  let outcome = case process.receive(subject, timeout_ms) {
    Ok(outcome) -> outcome
    Error(Nil) -> {
      let _ = kill_pid(pid)
      QueryTimedOut
    }
  }
  let _ = release_query_lock()
  outcome
}

/// Consume stream until Result or EndOfStream and return all messages.
/// Closes the stream after consuming to avoid hangs if the CLI doesn't exit.
pub fn consume_stream(stream: claude_agent_sdk.QueryStream) -> ConsumeResult {
  consume_stream_loop(stream, [], False)
}

fn consume_stream_loop(
  stream: claude_agent_sdk.QueryStream,
  acc: List(MessageEnvelope),
  terminated: Bool,
) -> ConsumeResult {
  case terminated {
    True -> {
      let _ = claude_agent_sdk.close(stream)
      ConsumeResult(messages: list.reverse(acc), terminated_normally: True)
    }
    False -> {
      let #(result, updated_stream) = claude_agent_sdk.next(stream)
      case result {
        Ok(Message(envelope)) -> {
          let updated_acc = [envelope, ..acc]
          case envelope.message {
            Result(_) -> {
              let _ = claude_agent_sdk.close(updated_stream)
              ConsumeResult(
                messages: list.reverse(updated_acc),
                terminated_normally: True,
              )
            }
            _ -> consume_stream_loop(updated_stream, updated_acc, False)
          }
        }
        Ok(EndOfStream) -> consume_stream_loop(updated_stream, acc, True)
        Ok(WarningEvent(_)) ->
          // Skip warnings, continue iteration
          consume_stream_loop(updated_stream, acc, False)
        Error(err) -> {
          // Check if error is terminal (stream closed)
          case claude_agent_sdk.is_terminal(err) {
            True -> {
              // Terminal error - close and return what we have
              let _ = claude_agent_sdk.close(updated_stream)
              ConsumeResult(
                messages: list.reverse(acc),
                terminated_normally: False,
              )
            }
            False ->
              // Non-terminal error (JsonDecodeError, UnexpectedMessageError)
              // Continue iteration - stream may still yield more items
              consume_stream_loop(updated_stream, acc, False)
          }
        }
      }
    }
  }
}

/// Assert stream produces at least one message before termination.
/// Returns the consumed result for further inspection.
pub fn assert_stream_produces_items(
  stream: claude_agent_sdk.QueryStream,
) -> ConsumeResult {
  let result = consume_stream(stream)
  case result.messages != [] {
    True -> result
    False -> panic as "Stream produced no messages"
  }
}

/// Extract session_id from the first SystemMessage in a list of envelopes.
/// Returns None if no SystemMessage found or if session_id is not set.
pub fn extract_session_id(messages: List(MessageEnvelope)) -> Option(String) {
  list.find_map(messages, fn(envelope) {
    case envelope.message {
      System(sys_msg) ->
        case sys_msg.session_id {
          Some(id) -> Ok(id)
          None -> Error(Nil)
        }
      _ -> Error(Nil)
    }
  })
  |> option_from_result
}

/// Check if messages contain a Result message (success or error).
pub fn has_result_message(messages: List(MessageEnvelope)) -> Bool {
  list.any(messages, fn(envelope) {
    case envelope.message {
      Result(_) -> True
      _ -> False
    }
  })
}

/// Count messages by type.
pub type MessageCounts {
  MessageCounts(system: Int, assistant: Int, user: Int, result: Int)
}

/// Count messages by type for protocol validation.
pub fn count_message_types(messages: List(MessageEnvelope)) -> MessageCounts {
  list.fold(messages, MessageCounts(0, 0, 0, 0), fn(counts, envelope) {
    case envelope.message {
      System(_) -> MessageCounts(..counts, system: counts.system + 1)
      Assistant(_) -> MessageCounts(..counts, assistant: counts.assistant + 1)
      User(_) -> MessageCounts(..counts, user: counts.user + 1)
      Result(_) -> MessageCounts(..counts, result: counts.result + 1)
    }
  })
}
