/// Bidirectional session GenServer for Claude Agent SDK.
///
/// This module provides the OTP actor (GenServer) that manages bidirectional
/// protocol sessions with the Claude CLI. The actor owns the port and handles
/// all message routing, hook callbacks, and request/response correlation.
///
/// ## Architecture
///
/// The session actor:
/// - Owns the CLI port (critical for message routing)
/// - Maintains lifecycle state (Starting → InitSent → Running → Stopped/Failed)
/// - Correlates outgoing requests with incoming responses
/// - Dispatches hook callbacks to registered handlers
/// - Manages pending operations and queued messages
///
/// ## Current Status: Skeleton
///
/// This is the initial skeleton implementation with type definitions and stubs.
/// Port message handling and selector configuration will be added in subsequent
/// tasks (T2: Lifecycle state machine, T3: Port message routing).
///
/// ## Usage
///
/// ```gleam
/// // Start a session
/// let assert Ok(session) = bidir.start(runner, config)
///
/// // Send a control request
/// bidir.send_request(session, Initialize(...))
///
/// // Receive messages via subscriber Subject
/// ```
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None}
import gleam/otp/actor

import claude_agent_sdk/hook.{type HookEvent}
import claude_agent_sdk/internal/bidir_runner.{type BidirRunner}

// =============================================================================
// Session Lifecycle
// =============================================================================

/// Session lifecycle states.
///
/// The session progresses through these states:
/// - Starting: Actor started, port not yet spawned
/// - InitSent: Initialize request sent, awaiting response
/// - Running: Fully operational, processing messages
/// - Stopped: Clean shutdown completed
/// - Failed: Unrecoverable error occurred
pub type SessionLifecycle {
  /// Actor started, port not yet spawned.
  Starting
  /// Initialize request sent to CLI, awaiting response.
  InitSent
  /// Fully operational, processing messages.
  Running
  /// Clean shutdown completed.
  Stopped
  /// Unrecoverable error occurred.
  Failed(String)
}

// =============================================================================
// Pending Operations
// =============================================================================

/// A pending request awaiting a response from the CLI.
///
/// Tracks SDK-initiated control requests that need response correlation.
pub type PendingRequest {
  PendingRequest(
    /// The request ID for correlation.
    request_id: String,
    /// Subject to send the response to.
    reply_to: Subject(RequestResult),
    /// When the request was sent (monotonic ms).
    sent_at: Int,
  )
}

/// Result of a control request.
pub type RequestResult {
  /// Request completed successfully.
  RequestSuccess(Dynamic)
  /// Request failed with error message.
  RequestError(String)
  /// Request timed out.
  RequestTimeout
}

/// A pending hook callback awaiting SDK response.
///
/// Tracks CLI-initiated hook requests that need SDK handler execution.
pub type PendingHook {
  PendingHook(
    /// The callback ID from the CLI.
    callback_id: String,
    /// The request ID for the response.
    request_id: String,
    /// When the hook was received (monotonic ms).
    received_at: Int,
  )
}

/// An operation queued while session is not Running.
///
/// Operations may be queued during startup before the session is fully initialized.
pub type QueuedOperation {
  /// A control request to send once Running.
  QueuedRequest(request_id: String, payload: Dynamic)
}

// =============================================================================
// Hook Configuration
// =============================================================================

/// Configuration for registered hooks.
pub type HookConfig {
  HookConfig(
    /// Map of callback_id -> handler function.
    handlers: Dict(String, fn(Dynamic) -> Dynamic),
  )
}

/// Create an empty hook configuration.
pub fn empty_hook_config() -> HookConfig {
  HookConfig(handlers: dict.new())
}

// =============================================================================
// CLI Capabilities
// =============================================================================

/// Capabilities reported by CLI during initialization.
pub type CliCapabilities {
  CliCapabilities(
    /// Supported control commands.
    supported_commands: List(String),
    /// Whether hooks are supported.
    hooks_supported: Bool,
    /// Whether permissions are supported.
    permissions_supported: Bool,
    /// Whether SDK MCP servers are supported.
    mcp_sdk_servers_supported: Bool,
  )
}

// =============================================================================
// Subscriber Notifications
// =============================================================================

/// Messages sent to subscribers when CLI events occur.
///
/// This is the public notification type - subscribers receive these messages
/// to observe session events. Internal port handling uses a separate type.
pub type SubscriberMessage {
  /// A parsed message from the CLI.
  CliMessage(Dynamic)
  /// Session has stopped (clean or with error).
  SessionStopped(reason: Option(String))
}

// =============================================================================
// Session State
// =============================================================================

/// Complete state for a bidirectional session actor.
///
/// Contains all state needed to manage the session lifecycle, route messages,
/// and handle concurrent control operations.
pub type SessionState {
  SessionState(
    /// The runner managing the CLI port.
    runner: BidirRunner,
    /// Current lifecycle state.
    lifecycle: SessionLifecycle,
    /// Pending SDK-initiated requests awaiting CLI responses.
    pending_requests: Dict(String, PendingRequest),
    /// Pending CLI-initiated hooks awaiting SDK responses.
    pending_hooks: Dict(String, PendingHook),
    /// Operations queued while not Running.
    queued_ops: List(QueuedOperation),
    /// Hook configuration and handlers.
    hooks: HookConfig,
    /// MCP server handlers (server_name -> handler).
    mcp_handlers: Dict(String, fn(Dynamic) -> Dynamic),
    /// Counter for generating request IDs.
    next_request_id: Int,
    /// Counter for generating callback IDs.
    next_callback_id: Int,
    /// Subject for sending messages to subscriber.
    subscriber: Subject(SubscriberMessage),
    /// CLI capabilities (populated after init response).
    capabilities: Option(CliCapabilities),
    /// Default timeout for control requests (ms).
    default_timeout_ms: Int,
    /// Per-event hook timeouts (ms).
    hook_timeouts: Dict(HookEvent, Int),
  )
}

// =============================================================================
// Actor Messages
// =============================================================================

/// Messages sent to the actor for synchronous operations.
/// Each variant that needs a reply includes a reply_to Subject.
///
/// Note: Port messages will be handled via selector in future implementation.
/// This skeleton focuses on the call/cast interface.
pub type ActorMessage {
  /// Get current lifecycle state.
  GetLifecycle(reply_to: Subject(SessionLifecycle))
  /// Ping for health check.
  Ping(reply_to: Subject(Response))
  /// Shutdown the session gracefully.
  ShutdownActor
}

/// Responses from synchronous operations.
pub type Response {
  /// Pong response.
  Pong
}

// =============================================================================
// Actor Implementation
// =============================================================================

/// Error type for session start failures.
pub type StartError {
  /// Actor failed to start.
  ActorStartFailed(actor.StartError)
  /// Runner failed to start.
  RunnerStartFailed(String)
}

/// Start configuration for a session.
pub type StartConfig {
  StartConfig(
    /// Subject to receive subscriber messages.
    subscriber: Subject(SubscriberMessage),
    /// Default timeout for requests (ms).
    default_timeout_ms: Int,
    /// Hook timeouts per event.
    hook_timeouts: Dict(HookEvent, Int),
  )
}

/// Create a default start configuration.
pub fn default_config(subscriber: Subject(SubscriberMessage)) -> StartConfig {
  StartConfig(
    subscriber: subscriber,
    default_timeout_ms: 60_000,
    hook_timeouts: dict.new(),
  )
}

/// Start a bidirectional session actor with an existing runner.
///
/// The runner must already be started (port spawned). This function creates
/// the actor that will manage the session lifecycle.
///
/// ## Port Ownership Note
///
/// In the current skeleton, the runner is passed in for testing purposes.
/// The full implementation (T3) will use `new_with_initialiser` to spawn
/// the port inside the actor's init function, ensuring proper port ownership.
/// Mock runners work correctly because they don't use real port messages.
pub fn start(
  runner: BidirRunner,
  config: StartConfig,
) -> Result(Subject(ActorMessage), StartError) {
  let initial_state =
    SessionState(
      runner: runner,
      lifecycle: Starting,
      pending_requests: dict.new(),
      pending_hooks: dict.new(),
      queued_ops: [],
      hooks: empty_hook_config(),
      mcp_handlers: dict.new(),
      next_request_id: 0,
      next_callback_id: 0,
      subscriber: config.subscriber,
      capabilities: None,
      default_timeout_ms: config.default_timeout_ms,
      hook_timeouts: config.hook_timeouts,
    )

  // Use the Builder pattern for gleam_otp 1.x
  // Note: Port selector will be added in T3 using new_with_initialiser
  let builder =
    actor.new(initial_state)
    |> actor.on_message(handle_message)

  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(ActorStartFailed(err))
  }
}

/// Handle incoming messages to the actor.
///
/// This is the main message loop. Handles synchronous requests with replies.
fn handle_message(
  state: SessionState,
  message: ActorMessage,
) -> actor.Next(SessionState, ActorMessage) {
  case message {
    GetLifecycle(reply_to) -> {
      process.send(reply_to, state.lifecycle)
      actor.continue(state)
    }
    Ping(reply_to) -> {
      process.send(reply_to, Pong)
      actor.continue(state)
    }
    ShutdownActor -> {
      // Clean shutdown - close the runner
      // BidirRunner.close is a function field: fn() -> Nil
      let close_fn = state.runner.close
      close_fn()
      actor.stop()
    }
  }
}

/// Call the actor synchronously to get lifecycle state.
pub fn get_lifecycle(
  session: Subject(ActorMessage),
  timeout: Int,
) -> SessionLifecycle {
  actor.call(session, timeout, GetLifecycle)
}

/// Call the actor synchronously with a ping.
pub fn ping(session: Subject(ActorMessage), timeout: Int) -> Response {
  actor.call(session, timeout, Ping)
}

/// Shutdown the session gracefully.
pub fn shutdown(session: Subject(ActorMessage)) -> Nil {
  actor.send(session, ShutdownActor)
}
