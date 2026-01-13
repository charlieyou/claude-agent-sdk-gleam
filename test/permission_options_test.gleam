/// Tests for permission and MCP option builders.
///
/// Verifies that mcp_servers and file_checkpointing_enabled options
/// are correctly stored and that builders compose properly.
import claude_agent_sdk/options
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option
import gleeunit/should

// Helper to convert any value to Dynamic (identity function in Erlang)
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// =============================================================================
// MCP Server Builder Tests
// =============================================================================

pub fn with_mcp_server_adds_to_list_test() {
  let handler = fn(_request: Dynamic) -> Dynamic { to_dynamic(Nil) }

  let opts =
    options.default_options()
    |> options.with_mcp_server("my-server", handler)

  // Should have one server
  list.length(opts.mcp_servers)
  |> should.equal(1)

  // Check the name
  let assert [#(name, _)] = opts.mcp_servers
  name |> should.equal("my-server")
}

pub fn with_mcp_server_accumulates_test() {
  let handler1 = fn(_request: Dynamic) -> Dynamic { to_dynamic("response1") }
  let handler2 = fn(_request: Dynamic) -> Dynamic { to_dynamic("response2") }

  let opts =
    options.default_options()
    |> options.with_mcp_server("server-a", handler1)
    |> options.with_mcp_server("server-b", handler2)

  // Should have two servers
  list.length(opts.mcp_servers)
  |> should.equal(2)

  // Check names (oldest first so newest wins in dict.from_list)
  let names = list.map(opts.mcp_servers, fn(pair) { pair.0 })
  names |> should.equal(["server-a", "server-b"])
}

pub fn mcp_server_handler_is_callable_test() {
  let handler = fn(request: Dynamic) -> Dynamic {
    // Echo the request back
    request
  }

  let opts =
    options.default_options()
    |> options.with_mcp_server("echo-server", handler)

  let assert [#(_, stored_handler)] = opts.mcp_servers
  let input = to_dynamic("test-input")
  let result = stored_handler(input)

  // Result should be the same as input (echo behavior)
  result |> should.equal(input)
}

// =============================================================================
// File Checkpointing Builder Tests
// =============================================================================

pub fn with_file_checkpointing_enables_flag_test() {
  let opts =
    options.default_options()
    |> options.with_file_checkpointing()

  opts.file_checkpointing_enabled
  |> should.be_true
}

pub fn default_options_has_checkpointing_disabled_test() {
  let opts = options.default_options()

  opts.file_checkpointing_enabled
  |> should.be_false
}

pub fn file_checkpointing_idempotent_test() {
  // Calling multiple times should still be True
  let opts =
    options.default_options()
    |> options.with_file_checkpointing()
    |> options.with_file_checkpointing()

  opts.file_checkpointing_enabled
  |> should.be_true
}

// =============================================================================
// Default State Tests
// =============================================================================

pub fn default_options_has_empty_mcp_servers_test() {
  let opts = options.default_options()

  opts.mcp_servers
  |> should.equal([])
}

// =============================================================================
// Composition Tests
// =============================================================================

pub fn mcp_and_checkpointing_compose_test() {
  let handler = fn(_request: Dynamic) -> Dynamic { to_dynamic(Nil) }

  let opts =
    options.default_options()
    |> options.with_mcp_server("my-server", handler)
    |> options.with_file_checkpointing()

  // Both should be set
  list.length(opts.mcp_servers) |> should.equal(1)
  opts.file_checkpointing_enabled |> should.be_true
}

pub fn mcp_composes_with_other_options_test() {
  let handler = fn(_request: Dynamic) -> Dynamic { to_dynamic(Nil) }

  let opts =
    options.default_options()
    |> options.with_model("sonnet")
    |> options.with_mcp_server("tools", handler)
    |> options.with_max_turns(5)
    |> options.with_file_checkpointing()
    |> options.with_timeout(30_000)

  // All options should be set correctly
  opts.model |> should.equal(option.Some("sonnet"))
  opts.max_turns |> should.equal(option.Some(5))
  list.length(opts.mcp_servers) |> should.equal(1)
  opts.file_checkpointing_enabled |> should.be_true
  opts.timeout_ms |> should.equal(option.Some(30_000))
}

pub fn multiple_mcp_servers_with_other_options_test() {
  let handler1 = fn(_: Dynamic) -> Dynamic { to_dynamic(1) }
  let handler2 = fn(_: Dynamic) -> Dynamic { to_dynamic(2) }
  let handler3 = fn(_: Dynamic) -> Dynamic { to_dynamic(3) }

  let opts =
    options.default_options()
    |> options.with_mcp_server("server-1", handler1)
    |> options.with_permission_mode(options.BypassPermissions)
    |> options.with_mcp_server("server-2", handler2)
    |> options.with_file_checkpointing()
    |> options.with_mcp_server("server-3", handler3)

  // Should have 3 servers (oldest first so newest wins in dict.from_list)
  list.length(opts.mcp_servers) |> should.equal(3)
  let names = list.map(opts.mcp_servers, fn(pair) { pair.0 })
  names |> should.equal(["server-1", "server-2", "server-3"])

  // Other options should be set
  opts.permission_mode |> should.equal(option.Some(options.BypassPermissions))
  opts.file_checkpointing_enabled |> should.be_true
}
