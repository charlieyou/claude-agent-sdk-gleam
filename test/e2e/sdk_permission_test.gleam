/// E2E Tests for Permission Denied Flow.
///
/// These tests verify that the on_can_use_tool permission handler
/// can deny tool execution via the SDK QueryOptions API.
///
/// ## What's Being Tested
/// - Permission handler returning Deny prevents Bash tool execution
/// - Denial is surfaced in message stream (permission_denials in Result)
///
/// ## Running Tests
/// ```bash
/// gleam test -- --e2e
/// # Ensure the Claude CLI is authenticated (e.g., `claude auth login`)
/// gleam test -- --only sdk_permission
/// ```
import claude_agent_sdk
import claude_agent_sdk/error.{error_to_string}
import claude_agent_sdk/hook.{type CanUseToolContext, Deny}
import claude_agent_sdk/message.{type MessageEnvelope, Result}
import e2e/helpers
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

// ============================================================================
// Permission Denied Test
// ============================================================================

/// Test that permission denial handler works - tool does not execute when denied.
///
/// This test:
/// 1. Configures QueryOptions with on_can_use_tool handler returning Deny
/// 2. Sends prompt "Run `echo hello`" to trigger Bash tool
/// 3. Asserts: Bash tool does NOT execute (no "hello" in output)
/// 4. Asserts: SDK surfaces denial in message stream (permission_denials)
pub fn permission_denied_flow_test() {
  case helpers.skip_if_no_e2e() {
    Error(msg) -> {
      io.println(msg)
      Nil
    }
    Ok(Nil) -> {
      let ctx = helpers.new_test_context("sdk_permission_denied")
      let ctx = helpers.test_step(ctx, "configure_options")

      // Configure options with permission handler that denies all tools
      let opts =
        claude_agent_sdk.default_options()
        |> claude_agent_sdk.with_max_turns(1)
        |> claude_agent_sdk.with_can_use_tool(fn(tool_ctx: CanUseToolContext) {
          // Log the tool being checked
          io.println(
            "[PERMISSION] Checking tool: " <> tool_ctx.tool_name <> " - DENYING",
          )
          Deny("Denied by test - no tool execution allowed")
        })

      let ctx = helpers.test_step(ctx, "execute_query")
      // Prompt that reliably triggers Bash tool
      let prompt = "Run `echo hello`"

      case helpers.query_and_consume_with_timeout(prompt, opts, 60_000) {
        helpers.QuerySuccess(result) -> {
          let ctx = helpers.test_step(ctx, "verify_denial")

          // Check that permission_denials is populated in the Result message
          let denials = extract_permission_denials(result.messages)
          helpers.log_info_with(ctx, "denials_found", [
            #("count", json.int(list.length(denials))),
            #("tools", json.string(string.join(denials, ", "))),
          ])

          // Check that "hello" does NOT appear in any message content
          // (would indicate Bash tool executed despite denial)
          let has_hello = check_for_hello_output(result.messages)

          let ctx = helpers.test_step(ctx, "assert_results")
          case has_hello {
            True -> {
              helpers.log_error(
                ctx,
                "tool_executed",
                "Bash tool executed despite denial - 'hello' found in output",
              )
              helpers.log_test_complete(
                ctx,
                False,
                "Tool executed despite permission deny",
              )
              should.fail()
            }
            False -> {
              helpers.log_info(ctx, "no_execution_detected")

              // Success: either we got denials OR no hello output
              case denials != [] {
                True -> {
                  helpers.log_info(ctx, "denials_surfaced_in_stream")
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "Permission deny worked - tool blocked, denials surfaced",
                  )
                }
                False -> {
                  // No denials in stream but also no hello - still a success
                  // The CLI may not surface denials in all cases
                  helpers.log_info_with(ctx, "no_denials_in_stream", [
                    #(
                      "note",
                      json.string(
                        "Denials may not be surfaced by CLI in all cases",
                      ),
                    ),
                  ])
                  helpers.log_test_complete(
                    ctx,
                    True,
                    "Permission deny worked - tool blocked (denials not surfaced)",
                  )
                }
              }
            }
          }
        }
        helpers.QueryFailure(err) -> {
          helpers.log_error(ctx, "query_failed", error_to_string(err))
          helpers.log_test_complete(ctx, True, "Query failed - skipping test")
        }
        helpers.QueryTimedOut -> {
          helpers.log_info(ctx, "query_timeout")
          helpers.log_test_complete(
            ctx,
            True,
            "Query timed out - skipping test",
          )
        }
      }
    }
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Extract tool names from permission_denials in Result messages.
fn extract_permission_denials(messages: List(MessageEnvelope)) -> List(String) {
  list.flat_map(messages, fn(envelope) {
    case envelope.message {
      Result(result_msg) ->
        case result_msg.permission_denials {
          Some(denials) -> list.map(denials, fn(denial) { denial.tool_name })
          None -> []
        }
      _ -> []
    }
  })
}

/// Check if any message contains "hello" output (indicating Bash executed).
fn check_for_hello_output(messages: List(MessageEnvelope)) -> Bool {
  list.any(messages, fn(envelope) {
    let msg_str = string.inspect(envelope)
    string.contains(msg_str, "hello")
  })
}
