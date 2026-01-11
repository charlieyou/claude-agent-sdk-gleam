/// API surface tests for Claude Agent SDK.
///
/// This test module verifies that all Epic 1 public types are accessible
/// via the main claude_agent_sdk module. Tests compile-time API surface.
import gleam/dynamic
import gleam/option.{None, Some}
import gleeunit/should

// Import all public types from main SDK module to verify they're exported
import claude_agent_sdk.{
  type AssistantMessage, type AssistantMessageContent, type ContentBlock,
  type ErrorDiagnostic, type Handle, type McpServerStatus, type Message,
  type MessageEnvelope, type PermissionDenial, type QueryError, type ReadResult,
  type ResultMessage, type ResultSubtype, type Runner, type StreamError,
  type StreamItem, type SystemMessage, type ToolResultBlock, type Usage,
  type UserMessage, type UserMessageContent, type Warning, type WarningCode,
  is_terminal, test_runner, version,
}

// Import message module constructors for creating test values
import claude_agent_sdk/message

// Import error module constructors
import claude_agent_sdk/error

// Import content module constructors
import claude_agent_sdk/content

// Import runner module constructors for ReadResult
import claude_agent_sdk/runner

// =============================================================================
// API Surface Compilation Tests
// =============================================================================
// These tests verify that types can be used after importing from main module.
// If any type is missing from re-exports, this file will fail to compile.

pub fn version_test() {
  version()
  |> should.equal("0.1.0")
}

pub fn message_types_importable_test() {
  // Verify Message discriminated union constructors work
  let sys_msg: SystemMessage =
    message.SystemMessage(
      subtype: Some("init"),
      uuid: Some("test-uuid"),
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

  let msg: Message = message.System(sys_msg)

  // Verify message matches expected variant
  let is_system = case msg {
    message.System(_) -> True
    _ -> False
  }
  is_system |> should.be_true()
}

pub fn message_envelope_importable_test() {
  // Verify MessageEnvelope can be created and used
  let sys_msg =
    message.SystemMessage(
      subtype: Some("init"),
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

  let envelope: MessageEnvelope =
    message.MessageEnvelope(
      message: message.System(sys_msg),
      raw_json: "{}",
      raw_bytes: <<>>,
    )

  envelope.raw_json
  |> should.equal("{}")
}

pub fn content_block_types_importable_test() {
  // Verify ContentBlock constructors work
  let text_block: ContentBlock = content.TextBlock("hello")

  let is_text = case text_block {
    content.TextBlock(text) -> text == "hello"
    _ -> False
  }
  is_text |> should.be_true()
}

pub fn unknown_block_importable_test() {
  // Verify UnknownBlock works with dynamic
  let raw = dynamic.string("unknown content")
  let unknown_block: ContentBlock = content.UnknownBlock(raw)

  let is_unknown = case unknown_block {
    content.UnknownBlock(_) -> True
    _ -> False
  }
  is_unknown |> should.be_true()
}

pub fn tool_result_block_importable_test() {
  let result: ToolResultBlock =
    content.ToolResultBlock(
      tool_use_id: "id-1",
      content: "result",
      is_error: None,
    )

  result.tool_use_id
  |> should.equal("id-1")
}

pub fn error_types_importable_test() {
  // Verify QueryError constructors work
  let err: QueryError = error.CliNotFoundError("claude not found")

  let is_cli_error = case err {
    error.CliNotFoundError(msg) -> msg == "claude not found"
    _ -> False
  }
  is_cli_error |> should.be_true()
}

pub fn stream_error_types_importable_test() {
  // Verify StreamError constructors work
  let err: StreamError = error.BufferOverflow

  // Test is_terminal function is re-exported
  is_terminal(err)
  |> should.be_true()
}

pub fn warning_types_importable_test() {
  // Verify Warning and WarningCode work
  let code: WarningCode = error.UnparseableCliVersion
  let warning: Warning =
    error.Warning(code: code, message: "version unknown", context: None)

  warning.message
  |> should.equal("version unknown")
}

pub fn stream_item_importable_test() {
  // Verify StreamItem constructors work
  let item: StreamItem = error.EndOfStream

  let is_end = case item {
    error.EndOfStream -> True
    _ -> False
  }
  is_end |> should.be_true()
}

pub fn result_subtype_importable_test() {
  // Verify ResultSubtype constructors work
  let subtype: ResultSubtype = message.Success

  let is_success = case subtype {
    message.Success -> True
    _ -> False
  }
  is_success |> should.be_true()
}

pub fn usage_importable_test() {
  // Verify Usage works
  let usage: Usage =
    message.Usage(
      input_tokens: Some(100),
      output_tokens: Some(50),
      cache_creation_input_tokens: None,
      cache_read_input_tokens: None,
    )

  usage.input_tokens
  |> should.equal(Some(100))
}

pub fn mcp_server_status_importable_test() {
  // Verify McpServerStatus works
  let status: McpServerStatus =
    message.McpServerStatus(name: "test-server", status: "connected")

  status.name
  |> should.equal("test-server")
}

pub fn error_diagnostic_importable_test() {
  // Verify ErrorDiagnostic works
  let diagnostic: ErrorDiagnostic =
    error.ErrorDiagnostic(
      last_non_json_line: None,
      stdout_was_empty: True,
      exit_code_hint: "auth required",
      troubleshooting: "run claude login",
    )

  diagnostic.stdout_was_empty
  |> should.be_true()
}

pub fn assistant_message_content_importable_test() {
  // Verify AssistantMessageContent works
  let asst_content: AssistantMessageContent =
    message.AssistantMessageContent(
      model: Some("claude-3-opus"),
      id: Some("msg-123"),
      message_type: Some("message"),
      role: Some("assistant"),
      content: None,
      stop_reason: Some("end_turn"),
      usage: None,
    )

  asst_content.model
  |> should.equal(Some("claude-3-opus"))
}

pub fn user_message_content_importable_test() {
  // Verify UserMessageContent works
  let user_content: UserMessageContent =
    message.UserMessageContent(role: Some("user"), content: None)

  user_content.role
  |> should.equal(Some("user"))
}

pub fn result_message_importable_test() {
  // Verify ResultMessage works
  let result: ResultMessage =
    message.ResultMessage(
      subtype: Some(message.Success),
      uuid: Some("result-uuid"),
      session_id: None,
      is_error: Some(False),
      duration_ms: Some(1000),
      duration_api_ms: Some(500),
      num_turns: Some(3),
      result: Some("Task completed"),
      total_cost_usd: Some(0.05),
      usage: None,
      model_usage: None,
      permission_denials: None,
      structured_output: None,
      errors: None,
    )

  result.is_error
  |> should.equal(Some(False))
}

pub fn permission_denial_importable_test() {
  // Verify PermissionDenial works
  let denial: PermissionDenial =
    message.PermissionDenial(
      tool_name: "bash",
      tool_use_id: "tu-123",
      tool_input: dynamic.string("rm -rf /"),
    )

  denial.tool_name
  |> should.equal("bash")
}

pub fn assistant_message_importable_test() {
  // Verify AssistantMessage works
  let msg: AssistantMessage =
    message.AssistantMessage(
      uuid: Some("asst-uuid"),
      session_id: Some("session-123"),
      parent_tool_use_id: None,
      message: None,
    )

  msg.uuid
  |> should.equal(Some("asst-uuid"))
}

pub fn user_message_importable_test() {
  // Verify UserMessage works
  let msg: UserMessage =
    message.UserMessage(
      uuid: Some("user-uuid"),
      session_id: Some("session-123"),
      parent_tool_use_id: Some("tool-use-456"),
      message: None,
      tool_use_result: None,
    )

  msg.parent_tool_use_id
  |> should.equal(Some("tool-use-456"))
}

// =============================================================================
// Runner Types API Surface Tests
// =============================================================================

pub fn runner_types_importable_test() {
  // Verify Runner, Handle, ReadResult types and test_runner function are exported
  // Create a test runner using the runner module directly (test_runner has labeled args)
  let _test_runner: Runner =
    runner.test_runner(
      on_spawn: fn(_cmd, _args, _cwd) { Ok(dynamic.nil()) },
      on_read: fn(_handle) { runner.Eof },
      on_close: fn(_handle) { Nil },
    )

  // Verify ReadResult constructors work
  let read_result: ReadResult = runner.Data(<<>>)
  let is_data = case read_result {
    runner.Data(_) -> True
    _ -> False
  }
  is_data |> should.be_true()
}
