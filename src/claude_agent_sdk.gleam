//// Claude Agent SDK for Gleam
////
//// This module provides the main entry point for the Claude Agent SDK.
//// Re-exports all public types for convenient imports.
////
//// ## Pattern Matching on Variant Types
////
//// Due to Gleam's module system, variant constructors cannot be re-exported.
//// This means you can import types from this module and use factory functions
//// to create values, but pattern matching requires importing the source module:
////
//// ```gleam
//// import claude_agent_sdk.{type ContentBlock}
//// import claude_agent_sdk/content  // Required for pattern matching
////
//// // Creating values - use factory functions from main module
//// let block = claude_agent_sdk.text_block("Hello")
////
//// // Pattern matching - use constructors from source module
//// case block {
////   content.TextBlock(text) -> // handle text
////   content.ToolUseBlock(id, name, input) -> // handle tool use
////   content.UnknownBlock(raw) -> // handle unknown
//// }
//// ```
////
//// This applies to: ContentBlock, ToolResultBlock, Message, QueryError,
//// StreamError, and other variant types.

import gleam/dynamic
import gleam/option

// =============================================================================
// Message Types (from message.gleam)
// =============================================================================

import claude_agent_sdk/message

pub type Message =
  message.Message

pub type MessageEnvelope =
  message.MessageEnvelope

pub type SystemMessage =
  message.SystemMessage

pub type AssistantMessage =
  message.AssistantMessage

pub type AssistantMessageContent =
  message.AssistantMessageContent

pub type UserMessage =
  message.UserMessage

pub type UserMessageContent =
  message.UserMessageContent

pub type ResultMessage =
  message.ResultMessage

pub type ResultSubtype =
  message.ResultSubtype

pub type Usage =
  message.Usage

pub type McpServerStatus =
  message.McpServerStatus

pub type PermissionDenial =
  message.PermissionDenial

// =============================================================================
// Content Block Types (from content.gleam)
// =============================================================================

import claude_agent_sdk/content

pub type ContentBlock =
  content.ContentBlock

/// Creates a TextBlock content block
pub fn text_block(text: String) -> ContentBlock {
  content.TextBlock(text)
}

/// Creates a ToolUseBlock content block
pub fn tool_use_block(
  id: String,
  name: String,
  input: dynamic.Dynamic,
) -> ContentBlock {
  content.ToolUseBlock(id, name, input)
}

/// Creates an UnknownBlock content block for forward compatibility
pub fn unknown_block(raw: dynamic.Dynamic) -> ContentBlock {
  content.UnknownBlock(raw)
}

pub type ToolResultBlock =
  content.ToolResultBlock

/// Creates a ToolResultBlock
pub fn tool_result_block(
  tool_use_id: String,
  content_text: String,
  is_error: option.Option(Bool),
) -> ToolResultBlock {
  content.ToolResultBlock(tool_use_id, content_text, is_error)
}

// =============================================================================
// Error Types (from error.gleam)
// =============================================================================

import claude_agent_sdk/error

pub type QueryError =
  error.QueryError

pub type StreamError =
  error.StreamError

pub type ErrorDiagnostic =
  error.ErrorDiagnostic

pub type StreamItem =
  error.StreamItem

pub type Warning =
  error.Warning

pub type WarningCode =
  error.WarningCode

/// Check if a StreamError is terminal (stream closed, no more items)
pub const is_terminal = error.is_terminal

// =============================================================================
// SDK Version
// =============================================================================

pub fn version() -> String {
  "0.1.0"
}
