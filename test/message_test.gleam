/// Tests for message type invariants and constructability.
///
/// This module tests type-level construction and pattern matching for all
/// message-related types. No I/O or subprocess spawning.
import gleam/dynamic
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

import claude_agent_sdk/content.{
  type ContentBlock, type ToolResultBlock, TextBlock, ToolResultBlock,
  ToolUseBlock, UnknownBlock,
}
import claude_agent_sdk/error.{
  type StreamError, type StreamItem, type Warning, type WarningCode,
  BidirOptionIgnored, BufferOverflow, CleanExitNoResult, DeprecatedOption,
  EndOfStream, ErrorDiagnostic, JsonDecodeError, Message as StreamMessage,
  NonZeroExitAfterResult, ProcessError, TooManyDecodeErrors,
  UnexpectedMessageAfterResult, UnexpectedMessageError, UnparseableCliVersion,
  Warning, WarningEvent, is_terminal,
}
import claude_agent_sdk/message.{
  type AssistantMessage, type Message, type MessageEnvelope, type ResultMessage,
  type ResultSubtype, type SystemMessage, type UserMessage, Assistant,
  AssistantMessage, ErrorDuringExecution, ErrorMaxBudget, ErrorMaxTurns,
  McpServerStatus, MessageEnvelope, PermissionDenial, Result, ResultMessage,
  Success, System, SystemMessage, UnknownSubtype, Usage, User, UserMessage,
}

// ============================================================================
// StreamItem Variant Tests
// ============================================================================

pub fn stream_item_message_variant_test() {
  // Verify Message variant wraps MessageEnvelope
  let envelope = make_minimal_envelope()
  let item: StreamItem = StreamMessage(envelope)
  case item {
    StreamMessage(_) -> should.be_true(True)
  }
}

pub fn stream_item_warning_event_variant_test() {
  // Verify WarningEvent variant wraps Warning
  let warning = Warning(code: CleanExitNoResult, message: "test", context: None)
  let item: StreamItem = WarningEvent(warning)
  case item {
    WarningEvent(w) -> w.message |> should.equal("test")
  }
}

pub fn stream_item_end_of_stream_variant_test() {
  // Verify EndOfStream is constructable
  let item: StreamItem = EndOfStream
  case item {
    EndOfStream -> should.be_true(True)
  }
}

pub fn stream_item_all_variants_matchable_test() {
  // Verify exhaustive pattern matching compiles
  let items: List(StreamItem) = [
    StreamMessage(make_minimal_envelope()),
    WarningEvent(Warning(code: CleanExitNoResult, message: "", context: None)),
    EndOfStream,
  ]
  items
  |> list.length
  |> should.equal(3)
}

// ============================================================================
// Message Variant Tests
// ============================================================================

pub fn message_system_variant_test() {
  let msg: Message = System(make_minimal_system())
  case msg {
    System(_) -> should.be_true(True)
  }
}

pub fn message_assistant_variant_test() {
  let msg: Message = Assistant(make_minimal_assistant())
  case msg {
    Assistant(_) -> should.be_true(True)
  }
}

pub fn message_user_variant_test() {
  let msg: Message = User(make_minimal_user())
  case msg {
    User(_) -> should.be_true(True)
  }
}

pub fn message_result_variant_test() {
  let msg: Message = Result(make_minimal_result())
  case msg {
    Result(_) -> should.be_true(True)
  }
}

pub fn message_all_variants_matchable_test() {
  // Verify exhaustive pattern matching on Message
  let messages: List(Message) = [
    System(make_minimal_system()),
    Assistant(make_minimal_assistant()),
    User(make_minimal_user()),
    Result(make_minimal_result()),
  ]
  messages
  |> list.length
  |> should.equal(4)
}

// ============================================================================
// ContentBlock Variant Tests
// ============================================================================

pub fn content_block_text_variant_test() {
  let block = TextBlock(text: "Hello, world!")
  case block {
    TextBlock(text) -> text |> should.equal("Hello, world!")
  }
}

pub fn content_block_tool_use_variant_test() {
  let block =
    ToolUseBlock(id: "tu_123", name: "bash", input: dynamic.string("ls"))
  case block {
    ToolUseBlock(id, name, _input) -> {
      id |> should.equal("tu_123")
      name |> should.equal("bash")
    }
  }
}

pub fn content_block_unknown_variant_test() {
  // UnknownBlock for forward compatibility with new content types
  let raw = dynamic.string("new_type_data")
  let block = UnknownBlock(raw: raw)
  case block {
    UnknownBlock(_) -> should.be_true(True)
  }
}

pub fn content_block_all_variants_matchable_test() {
  // Verify exhaustive pattern matching on ContentBlock
  let blocks: List(ContentBlock) = [
    TextBlock(text: "test"),
    ToolUseBlock(id: "id", name: "name", input: dynamic.nil()),
    UnknownBlock(raw: dynamic.nil()),
  ]
  blocks
  |> list.length
  |> should.equal(3)
}

// ============================================================================
// ToolResultBlock Tests
// ============================================================================

pub fn tool_result_block_construction_test() {
  let result: ToolResultBlock =
    ToolResultBlock(tool_use_id: "tu_456", content: "output", is_error: None)
  result.tool_use_id |> should.equal("tu_456")
  result.content |> should.equal("output")
  result.is_error |> should.equal(None)
}

pub fn tool_result_block_with_error_test() {
  let result =
    ToolResultBlock(
      tool_use_id: "tu_789",
      content: "Error: command failed",
      is_error: Some(True),
    )
  result.is_error |> should.equal(Some(True))
}

// ============================================================================
// Warning Variant Tests
// ============================================================================

pub fn warning_unparseable_cli_version_test() {
  let code: WarningCode = UnparseableCliVersion
  let warning = Warning(code: code, message: "Could not parse", context: None)
  case warning.code {
    UnparseableCliVersion -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn warning_clean_exit_no_result_test() {
  let warning: Warning =
    Warning(
      code: CleanExitNoResult,
      message: "No result message",
      context: None,
    )
  case warning.code {
    CleanExitNoResult -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn warning_non_zero_exit_after_result_test() {
  let warning =
    Warning(
      code: NonZeroExitAfterResult(1),
      message: "Non-zero after result",
      context: None,
    )
  case warning.code {
    NonZeroExitAfterResult(code) -> code |> should.equal(1)
    _ -> should.fail()
  }
}

pub fn warning_unexpected_message_after_result_test() {
  let warning =
    Warning(
      code: UnexpectedMessageAfterResult,
      message: "Extra message",
      context: Some("raw json here"),
    )
  case warning.code {
    UnexpectedMessageAfterResult -> {
      warning.context |> should.equal(Some("raw json here"))
    }
    _ -> should.fail()
  }
}

pub fn warning_deprecated_option_test() {
  let warning =
    Warning(code: DeprecatedOption, message: "Option deprecated", context: None)
  case warning.code {
    DeprecatedOption -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn warning_bidir_option_ignored_test() {
  let warning =
    Warning(
      code: BidirOptionIgnored,
      message: "query() ignores hooks/can_use_tool/timeout_ms",
      context: None,
    )
  case warning.code {
    BidirOptionIgnored -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn warning_all_codes_matchable_test() {
  // Verify exhaustive pattern matching on WarningCode
  let codes: List(WarningCode) = [
    UnparseableCliVersion,
    CleanExitNoResult,
    NonZeroExitAfterResult(0),
    UnexpectedMessageAfterResult,
    DeprecatedOption,
    BidirOptionIgnored,
  ]
  codes
  |> list.length
  |> should.equal(6)
}

// ============================================================================
// Error Terminal Classification Tests
// ============================================================================

pub fn error_process_error_is_terminal_test() {
  let diagnostic = make_minimal_diagnostic()
  ProcessError(1, diagnostic)
  |> is_terminal
  |> should.be_true
}

pub fn error_buffer_overflow_is_terminal_test() {
  BufferOverflow
  |> is_terminal
  |> should.be_true
}

pub fn error_too_many_decode_errors_is_terminal_test() {
  TooManyDecodeErrors(5, "last error")
  |> is_terminal
  |> should.be_true
}

pub fn error_json_decode_error_is_not_terminal_test() {
  JsonDecodeError("bad json", "parse error")
  |> is_terminal
  |> should.be_false
}

pub fn error_unexpected_message_preserves_raw_json_test() {
  // UnexpectedMessageError preserves raw_json for forward compatibility
  let raw = "{\"type\":\"future_type\",\"data\":123}"
  let error: StreamError = UnexpectedMessageError(raw)
  case error {
    UnexpectedMessageError(json) -> json |> should.equal(raw)
  }
}

pub fn error_unexpected_message_is_not_terminal_test() {
  UnexpectedMessageError("{\"type\":\"unknown\"}")
  |> is_terminal
  |> should.be_false
}

// ============================================================================
// ResultSubtype Tests
// ============================================================================

pub fn result_subtype_success_test() {
  let subtype: ResultSubtype = Success
  case subtype {
    Success -> should.be_true(True)
  }
}

pub fn result_subtype_error_max_turns_test() {
  let subtype: ResultSubtype = ErrorMaxTurns
  case subtype {
    ErrorMaxTurns -> should.be_true(True)
  }
}

pub fn result_subtype_error_during_execution_test() {
  let subtype: ResultSubtype = ErrorDuringExecution
  case subtype {
    ErrorDuringExecution -> should.be_true(True)
  }
}

pub fn result_subtype_error_max_budget_test() {
  let subtype: ResultSubtype = ErrorMaxBudget
  case subtype {
    ErrorMaxBudget -> should.be_true(True)
  }
}

pub fn result_subtype_unknown_preserves_string_test() {
  // UnknownSubtype for forward compatibility
  let subtype: ResultSubtype = UnknownSubtype("new_subtype")
  case subtype {
    UnknownSubtype(s) -> s |> should.equal("new_subtype")
  }
}

pub fn result_subtype_all_variants_matchable_test() {
  let subtypes: List(ResultSubtype) = [
    Success,
    ErrorMaxTurns,
    ErrorDuringExecution,
    ErrorMaxBudget,
    UnknownSubtype("x"),
  ]
  subtypes
  |> list.length
  |> should.equal(5)
}

// ============================================================================
// Supporting Type Tests
// ============================================================================

pub fn usage_construction_test() {
  let usage =
    Usage(
      input_tokens: Some(100),
      output_tokens: Some(50),
      cache_creation_input_tokens: None,
      cache_read_input_tokens: Some(25),
    )
  usage.input_tokens |> should.equal(Some(100))
  usage.output_tokens |> should.equal(Some(50))
  usage.cache_creation_input_tokens |> should.equal(None)
  usage.cache_read_input_tokens |> should.equal(Some(25))
}

pub fn mcp_server_status_construction_test() {
  let status = McpServerStatus(name: "filesystem", status: "connected")
  status.name |> should.equal("filesystem")
  status.status |> should.equal("connected")
}

pub fn permission_denial_construction_test() {
  let denial =
    PermissionDenial(
      tool_name: "Bash",
      tool_use_id: "tu_abc",
      tool_input: dynamic.string("rm -rf /"),
    )
  denial.tool_name |> should.equal("Bash")
  denial.tool_use_id |> should.equal("tu_abc")
}

pub fn message_envelope_construction_test() {
  let envelope: MessageEnvelope =
    MessageEnvelope(
      message: System(make_minimal_system()),
      raw_json: "{\"type\":\"system\"}",
      raw_bytes: <<"{\"type\":\"system\"}":utf8>>,
    )
  envelope.raw_json |> should.equal("{\"type\":\"system\"}")
}

// ============================================================================
// Helper Functions
// ============================================================================

fn make_minimal_system() -> SystemMessage {
  SystemMessage(
    subtype: None,
    uuid: None,
    session_id: None,
    cwd: None,
    model: None,
    tools: None,
    mcp_servers: None,
    permission_mode: None,
    api_key_source: None,
    slash_commands: None,
    agents: None,
    claude_code_version: None,
  )
}

fn make_minimal_assistant() -> AssistantMessage {
  AssistantMessage(
    uuid: None,
    session_id: None,
    parent_tool_use_id: None,
    message: None,
    is_partial: False,
  )
}

fn make_minimal_user() -> UserMessage {
  UserMessage(
    uuid: None,
    session_id: None,
    parent_tool_use_id: None,
    message: None,
    tool_use_result: None,
  )
}

fn make_minimal_result() -> ResultMessage {
  ResultMessage(
    subtype: None,
    uuid: None,
    session_id: None,
    is_error: None,
    duration_ms: None,
    duration_api_ms: None,
    num_turns: None,
    result: None,
    total_cost_usd: None,
    usage: None,
    model_usage: None,
    permission_denials: None,
    structured_output: None,
    errors: None,
  )
}

fn make_minimal_diagnostic() -> error.ErrorDiagnostic {
  ErrorDiagnostic(
    last_non_json_line: None,
    stdout_was_empty: False,
    exit_code_hint: "test",
    troubleshooting: "test",
  )
}

fn make_minimal_envelope() -> MessageEnvelope {
  MessageEnvelope(
    message: System(make_minimal_system()),
    raw_json: "{}",
    raw_bytes: <<"{}":utf8>>,
  )
}
