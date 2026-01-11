//// Claude Agent SDK for Gleam
////
//// This module provides the main entry point for the Claude Agent SDK.
//// Re-exports all public types for convenient imports.

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

pub type ToolResultBlock =
  content.ToolResultBlock

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
