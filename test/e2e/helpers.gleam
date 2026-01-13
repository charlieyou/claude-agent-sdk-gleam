/// Shared helpers for E2E SDK tests.
///
/// This module provides common functions for:
/// - Test skipping based on environment variables
/// - Stream consumption utilities
/// - Protocol invariant assertions
import claude_agent_sdk
import claude_agent_sdk/error.{EndOfStream, Message, WarningEvent}
import claude_agent_sdk/message.{
  type MessageEnvelope, Assistant, Result, System, User,
}
import gleam/dynamic
import gleam/list
import gleam/option.{type Option, None, Some}

/// Convert Result to Option (helper to avoid qualified module reference).
fn option_from_result(result: Result(a, b)) -> Option(a) {
  case result {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

/// Check if E2E SDK tests are enabled.
/// Returns Ok(Nil) if E2E_SDK_TEST=1, Error with skip message otherwise.
pub fn skip_if_no_e2e() -> Result(Nil, String) {
  case get_env("E2E_SDK_TEST") {
    Ok("1") -> Ok(Nil)
    Ok(_) -> Error("[SKIP] E2E_SDK_TEST is set but not '1'")
    Error(_) -> Error("[SKIP] E2E_SDK_TEST not set; skipping E2E test")
  }
}

/// Get environment variable value via Erlang's built-in os module.
/// Uses os:getenv/1 directly to avoid FFI module load issues.
/// Returns Ok(value) if set, Error(Nil) if not set.
fn get_env(name: String) -> Result(String, Nil) {
  // Convert Gleam string (binary) to charlist for os:getenv
  let charlist_name = binary_to_list(name)
  let result = os_getenv_charlist(charlist_name)
  // os:getenv returns charlist on success, false (Bool) if not set
  case dynamic.classify(result) {
    // false is classified as "Bool", meaning var not set
    "Bool" -> Error(Nil)
    // Otherwise it's a charlist (List) - convert to binary string
    _ -> Ok(list_to_binary(result))
  }
}

/// Convert binary (Gleam string) to charlist for Erlang interop.
@external(erlang, "erlang", "binary_to_list")
fn binary_to_list(binary: String) -> dynamic.Dynamic

/// Raw FFI to Erlang's os:getenv/1 - always available as built-in module.
/// Takes charlist, returns charlist or false.
@external(erlang, "os", "getenv")
fn os_getenv_charlist(name: dynamic.Dynamic) -> dynamic.Dynamic

/// Convert charlist to binary (Gleam string).
@external(erlang, "erlang", "list_to_binary")
fn list_to_binary(charlist: dynamic.Dynamic) -> String

/// Result of consuming a stream.
pub type ConsumeResult {
  ConsumeResult(
    /// All message envelopes collected (in order received).
    messages: List(MessageEnvelope),
    /// True if stream terminated normally (EndOfStream).
    terminated_normally: Bool,
  )
}

/// Consume entire stream and return all messages.
/// Closes the stream after consuming.
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
        Ok(Message(envelope)) ->
          consume_stream_loop(updated_stream, [envelope, ..acc], False)
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
