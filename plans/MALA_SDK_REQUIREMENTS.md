# Claude Agent SDK (Gleam) — Requirements from mala-gleam

This document captures the **minimum SDK features** the `mala-gleam` orchestrator needs, so the SDK can be built independently and integrated later.

## 1) Session Lifecycle
- Create session with model + system prompt + initial user prompt
- Stream events over a stable API (SSE/WebSocket/HTTP chunking ok)
- Cancel/abort session cleanly
- Optional: resume session with resume token (if supported)

## 2) Event Stream Contract
The SDK must expose a **typed event stream** suitable for mapping to `AgentEvent`:
- `assistant_text` (text chunk)
- `tool_call` (name, id, json input)
- `tool_result` (id, status, output)
- `final` (summary + optional resolution)
- `usage` (token counts)
- `checkpoint` (resume token if available)

Each event should include:
- `session_id`
- timestamp (ms or ISO)
- stable ordering

## 3) Tool Call Support
- JSON-schema–driven tool definitions
- Tool call events include `tool_name`, `tool_id`, `input`
- The SDK must accept tool results back into the stream

## 4) Errors and Retries
- Distinguish transport errors vs model errors
- Surface retryable vs fatal errors
- Provide error metadata for logging

## 5) Configuration Surface
- API key input (env + explicit override)
- Base URL override (for local gateways)
- Model name + timeout configuration
- Max retries/backoff parameters

## 6) Concurrency + Backpressure
- Support multiple concurrent sessions (even if limited)
- Backpressure or buffering limits must be explicit

## 7) Logging / Observability Hooks
- Optional callback for raw event logging (for persistence/redaction)
- Structured error info

## 8) Determinism / Testing
- Provide a **mock backend** or fixture mode
- Deterministic replay for tests (optional but strongly preferred)

## 9) Security / Safety Notes
- No CLI hook dependence (SDK must be usable without Claude CLI)
- Avoid leaking secrets in error surfaces (redaction friendly)

## 10) Minimal Integration Contract (mala-gleam)
The `mala-gleam` adapter expects:
- `start_session(config) -> session_id`
- `stream_events(session_id, handler)`
- `send_tool_result(session_id, tool_id, output)`
- `cancel_session(session_id)`

If the SDK can provide these (or equivalents), integration is straightforward.
