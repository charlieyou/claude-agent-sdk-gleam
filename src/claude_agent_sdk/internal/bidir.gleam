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
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor

import claude_agent_sdk/control.{
  type IncomingControlRequest, type IncomingControlResponse,
  type IncomingMessage, type OutgoingControlRequest,
  type OutgoingControlResponse, type PermissionMode, CanUseTool, ControlRequest,
  ControlResponse, Error as ControlError, HookCallback, HookResponse,
  HookSuccess, Interrupt, McpMessage, RegularMessage, SetModel,
  SetPermissionMode, Success,
}
import claude_agent_sdk/hook.{type HookEvent}
import claude_agent_sdk/internal/bidir_runner.{type BidirRunner}
import claude_agent_sdk/internal/control_decoder
import claude_agent_sdk/internal/control_encoder
import claude_agent_sdk/message

// FFI: Convert any value to Dynamic (identity function at runtime)
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

// FFI: Compare two Dynamic values for exact equality (uses Erlang =:=)
@external(erlang, "bidir_ffi", "dynamic_equals")
fn dynamic_equals(a: Dynamic, b: Dynamic) -> Bool

// =============================================================================
// Session Lifecycle
// =============================================================================

/// Session lifecycle states.
///
/// The session progresses through these states:
/// - Starting: Actor started, port not yet spawned
/// Specific error variants for session failures.
///
/// These variants enable programmatic error handling rather than string parsing.
pub type SessionError {
  /// Initialization handshake timed out.
  InitializationTimeout
  /// CLI returned error during initialization.
  InitializationError(message: String)
  /// CLI exited before initialization completed.
  CliExitedDuringInit
  /// CLI exited during startup (before spawn completed).
  CliExitedDuringStartup
  /// A runtime error occurred with the given reason.
  RuntimeError(reason: String)
  /// Too many operations queued during initialization (max 16).
  InitQueueOverflow(message: String)
  /// Too many pending control requests (max 64).
  TooManyPendingRequests(message: String)
}

/// Reason why a session was stopped.
///
/// Used to communicate shutdown cause to subscribers and pending callers.
pub type StopReason {
  /// stop() was explicitly called by the SDK user.
  UserRequested
  /// CLI process exited with the given code.
  CliExited(code: Int)
  /// Session failed during initialization.
  InitFailed(SessionError)
}

/// Session lifecycle states.
///
/// - Starting: Actor started, CLI port not yet spawned
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
  Failed(SessionError)
}

/// Events that trigger lifecycle state transitions.
pub type LifecycleEvent {
  /// CLI process spawned successfully.
  CliSpawned
  /// Initialize request succeeded.
  InitSuccess
  /// Initialize request timed out.
  InitTimeout
  /// Stop was requested by the SDK user.
  StopRequested
  /// CLI port closed unexpectedly.
  PortClosed
  /// An error occurred with the given reason.
  ErrorOccurred(String)
}

/// Error returned when an invalid state transition is attempted.
pub type InvalidTransition {
  InvalidTransition(from: SessionLifecycle, event: LifecycleEvent)
}

/// Attempt a lifecycle state transition.
///
/// Returns Ok(new_state) for valid transitions, Error(InvalidTransition) otherwise.
///
/// Valid transitions:
/// - Starting + CliSpawned → InitSent
/// - Starting + ErrorOccurred/PortClosed → Failed
/// - InitSent + InitSuccess → Running
/// - InitSent + ErrorOccurred/InitTimeout/PortClosed → Failed
/// - Running + StopRequested/PortClosed → Stopped
/// - Running + ErrorOccurred → Failed
/// - Stopped/Failed + any → Error (terminal states)
pub fn transition(
  from: SessionLifecycle,
  event: LifecycleEvent,
) -> Result(SessionLifecycle, InvalidTransition) {
  case from, event {
    // Terminal states reject all transitions
    Stopped, _ -> Error(InvalidTransition(from, event))
    Failed(_), _ -> Error(InvalidTransition(from, event))

    // Starting state transitions
    Starting, CliSpawned -> Ok(InitSent)
    Starting, ErrorOccurred(reason) -> Ok(Failed(RuntimeError(reason)))
    Starting, PortClosed -> Ok(Failed(CliExitedDuringStartup))
    Starting, _ -> Error(InvalidTransition(from, event))

    // InitSent state transitions
    InitSent, InitSuccess -> Ok(Running)
    InitSent, ErrorOccurred(reason) -> Ok(Failed(InitializationError(reason)))
    InitSent, InitTimeout -> Ok(Failed(InitializationTimeout))
    InitSent, PortClosed -> Ok(Failed(CliExitedDuringInit))
    InitSent, _ -> Error(InvalidTransition(from, event))

    // Running state transitions
    Running, StopRequested -> Ok(Stopped)
    Running, PortClosed -> Ok(Stopped)
    Running, ErrorOccurred(reason) -> Ok(Failed(RuntimeError(reason)))
    Running, _ -> Error(InvalidTransition(from, event))
  }
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
    /// Timer reference for timeout cancellation.
    timer_ref: Option(Dynamic),
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
  /// Session was stopped before request completed.
  RequestSessionStopped
}

/// Error type for interrupt() operation.
///
/// Represents the possible failure modes when calling interrupt().
pub type InterruptError {
  /// CLI returned an error response.
  CliError(message: String)
  /// Request timed out (5000ms default).
  InterruptTimeout
  /// Session was stopped or not running.
  SessionStopped
}

/// Error type for set_permission_mode() operation.
///
/// Represents the possible failure modes when changing permission mode.
pub type SetPermissionModeError {
  /// CLI returned an error response.
  SetPermissionModeCliError(message: String)
  /// Request timed out (5000ms default).
  SetPermissionModeTimeout
  /// Session was stopped or not running.
  SetPermissionModeSessionStopped
}

/// Error type for set_model() operation.
///
/// Represents the possible failure modes when changing the model.
pub type SetModelError {
  /// CLI returned an error response.
  SetModelCliError(message: String)
  /// Request timed out (5000ms default).
  SetModelTimeout
  /// Session was stopped or not running.
  SetModelSessionStopped
}

/// Error type for rewind_files() operation.
///
/// Represents the possible failure modes when rewinding files.
pub type RewindFilesError {
  /// File checkpointing was not enabled during session initialization.
  CheckpointingNotEnabled
  /// CLI returned an error response.
  RewindFilesCliError(message: String)
  /// Request timed out (30000ms default for disk I/O).
  RewindFilesTimeout
  /// Session was stopped or not running.
  RewindFilesSessionStopped
}

/// A pending hook callback awaiting SDK response.
///
/// Tracks CLI-initiated hook requests that need SDK handler execution.
pub type PendingHook {
  PendingHook(
    /// The PID of the spawned task executing the callback.
    task_pid: process.Pid,
    /// Monitor reference for crash detection.
    monitor_ref: process.Monitor,
    /// Timer reference for cancellation (passed to cancel_timer).
    timer_ref: Dynamic,
    /// Verification reference for first-event-wins (compared against message).
    verify_ref: Dynamic,
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
  ///
  /// The payload is a pre-encoded JSON string (without trailing newline),
  /// created via control_encoder.encode_request. This ensures wire format
  /// consistency with directly-sent requests.
  QueuedRequest(
    request_id: String,
    /// Pre-encoded JSON string from control_encoder.encode_request.
    payload: String,
    reply_to: Subject(RequestResult),
  )
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
  /// Session has ended (clean shutdown or error).
  SessionEnded(StopReason)
}

// =============================================================================
// Message Routing
// =============================================================================

/// Routing destination for an incoming message.
///
/// Used by the dispatcher to determine which handler should process a message.
pub type MessageRoute {
  /// Route to hook callback handler with the callback_id.
  RouteHookCallback(callback_id: String)
  /// Route to permission handler with the tool_name.
  RoutePermission(tool_name: String)
  /// Route to MCP handler with the server_name.
  RouteMcp(server_name: String)
  /// Route to response correlator with the request_id.
  RouteResponse(request_id: String)
  /// Route to subscriber for regular messages.
  RouteSubscriber
}

/// Determine the routing destination for an incoming message.
///
/// This pure function inspects the message structure and returns the
/// appropriate route. The caller uses this to dispatch to the correct handler.
pub fn route_incoming(message: IncomingMessage) -> MessageRoute {
  case message {
    ControlRequest(request) -> route_control_request(request)
    ControlResponse(response) -> route_control_response(response)
    RegularMessage(_) -> RouteSubscriber
  }
}

/// Route a control request to its handler.
fn route_control_request(request: IncomingControlRequest) -> MessageRoute {
  case request {
    HookCallback(callback_id: cid, ..) -> RouteHookCallback(cid)
    CanUseTool(tool_name: name, ..) -> RoutePermission(name)
    McpMessage(server_name: name, ..) -> RouteMcp(name)
  }
}

/// Route a control response to its correlator.
fn route_control_response(response: IncomingControlResponse) -> MessageRoute {
  case response {
    Success(request_id: rid, ..) -> RouteResponse(rid)
    ControlError(request_id: rid, ..) -> RouteResponse(rid)
  }
}

/// Resolve a pending request by sending the appropriate result to its reply_to subject.
///
/// This function is called when a control_response is received that matches
/// a pending request. It sends either RequestSuccess or RequestError to the
/// caller's reply_to subject.
pub fn resolve_pending(
  pending: PendingRequest,
  response: IncomingControlResponse,
) -> Nil {
  case response {
    Success(_request_id, payload) -> {
      process.send(pending.reply_to, RequestSuccess(payload))
    }
    ControlError(_request_id, message) -> {
      process.send(pending.reply_to, RequestError(message))
    }
  }
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
    /// Default timeout for hook callbacks (ms).
    default_hook_timeout_ms: Int,
    /// Request ID of the init request (for correlation).
    init_request_id: Option(String),
    /// Subject for receiving injected messages (for testing).
    inject_subject: Option(Subject(String)),
    /// Timeout for initialization handshake (ms).
    init_timeout_ms: Int,
    /// Timer reference for init timeout (for cancellation on cleanup).
    init_timer_ref: Option(Dynamic),
    /// Whether file checkpointing is enabled (for rewind_files support).
    file_checkpointing_enabled: Bool,
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
  /// Get current capabilities.
  GetCapabilities(reply_to: Subject(Option(CliCapabilities)))
  /// Get whether file checkpointing is enabled.
  GetCheckpointingEnabled(reply_to: Subject(Bool))
  /// Ping for health check.
  Ping(reply_to: Subject(Response))
  /// Shutdown the session gracefully.
  ShutdownActor
  /// Injected JSON message from mock runner (for testing).
  InjectedMessage(json: String)
  /// Injected port closed event from mock runner (for testing).
  InjectedPortClosed
  /// Init timeout fired.
  InitTimeoutFired
  /// Send a control request to CLI.
  SendControlRequest(
    request: OutgoingControlRequest,
    reply_to: Subject(RequestResult),
  )
  /// Request timeout fired.
  RequestTimeoutFired(request_id: String)
  /// Hook task completed with result.
  HookDone(request_id: String, result: Dynamic)
  /// Hook task crashed with error.
  HookError(request_id: String, reason: Dynamic)
  /// Hook timeout fired (first-event-wins protocol).
  /// Includes verify_ref for verification against stale timeouts.
  HookTimeout(request_id: String, verify_ref: Dynamic)
  /// Cancel a pending request (for client-side timeout cleanup).
  CancelPendingRequest(request_id: String)
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
    /// Timeout for initialization handshake (ms). Default: 10000.
    init_timeout_ms: Int,
    /// Default timeout for hook callbacks (ms). Default: 30000.
    default_hook_timeout_ms: Int,
    /// Enable file checkpointing for rewind_files support. Default: False.
    enable_file_checkpointing: Bool,
  )
}

/// Create a default start configuration.
pub fn default_config(subscriber: Subject(SubscriberMessage)) -> StartConfig {
  StartConfig(
    subscriber: subscriber,
    default_timeout_ms: 60_000,
    hook_timeouts: dict.new(),
    init_timeout_ms: 10_000,
    default_hook_timeout_ms: 30_000,
    enable_file_checkpointing: False,
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
/// The full implementation will use `new_with_initialiser` to spawn
/// the port inside the actor's init function, ensuring proper port ownership.
/// Mock runners work correctly because they don't use real port messages.
pub fn start(
  runner: BidirRunner,
  config: StartConfig,
) -> Result(Subject(ActorMessage), StartError) {
  start_internal(runner, config, None, empty_hook_config())
}

/// Start a bidirectional session actor for testing.
///
/// This is the same as `start` but exposes the actor subject for
/// injecting messages during tests. Use `inject_message` and
/// `inject_port_closed` to simulate CLI responses.
pub fn start_for_testing(
  runner: BidirRunner,
  config: StartConfig,
) -> Result(Subject(ActorMessage), StartError) {
  start_internal(runner, config, None, empty_hook_config())
}

/// Start a bidirectional session actor with pre-configured hooks.
///
/// This allows tests to register hook handlers that will be invoked
/// when the CLI sends hook_callback control requests.
pub fn start_with_hooks(
  runner: BidirRunner,
  config: StartConfig,
  hooks: HookConfig,
) -> Result(Subject(ActorMessage), StartError) {
  start_internal(runner, config, None, hooks)
}

/// Inject a JSON message to the actor as if it came from the CLI.
///
/// This is for testing only - simulates a control_response or other
/// CLI message arriving on the port.
pub fn inject_message(session: Subject(ActorMessage), json: String) -> Nil {
  actor.send(session, InjectedMessage(json))
}

/// Internal start function.
fn start_internal(
  runner: BidirRunner,
  config: StartConfig,
  _inject_subject: Option(Subject(String)),
  hooks: HookConfig,
) -> Result(Subject(ActorMessage), StartError) {
  // Generate the init request ID
  let init_request_id = "req_0"

  // Use new_with_initialiser to perform init handshake during actor start
  let builder =
    actor.new_with_initialiser(5000, fn(self_subject) {
      // Build initial state
      let initial_state =
        SessionState(
          runner: runner,
          lifecycle: Starting,
          pending_requests: dict.new(),
          pending_hooks: dict.new(),
          queued_ops: [],
          hooks: hooks,
          mcp_handlers: dict.new(),
          next_request_id: 1,
          // Start at 1 since req_0 is used for init
          next_callback_id: 0,
          subscriber: config.subscriber,
          capabilities: None,
          default_timeout_ms: config.default_timeout_ms,
          hook_timeouts: config.hook_timeouts,
          default_hook_timeout_ms: config.default_hook_timeout_ms,
          init_request_id: Some(init_request_id),
          inject_subject: None,
          init_timeout_ms: config.init_timeout_ms,
          init_timer_ref: None,
          file_checkpointing_enabled: config.enable_file_checkpointing,
        )

      // Perform the initialization handshake
      let state_after_init = perform_init_handshake(initial_state)

      // Build a selector that handles:
      // 1. Messages on the actor's subject (via process.selecting)
      // 2. Init timeout message {init_timeout, nil} (via select_record)
      // 3. Request timeout message {request_timeout, request_id} (via select_record)
      // 4. Hook done message {hook_done, request_id, result} (via select_record)
      // 5. Hook error message {hook_error, request_id, reason} (via select_record)
      // 6. Hook timeout message {hook_timeout, request_id} (via select_record)
      let init_timeout_tag = atom.create("init_timeout")
      let request_timeout_tag = atom.create("request_timeout")
      let hook_done_tag = atom.create("hook_done")
      let hook_error_tag = atom.create("hook_error")
      let hook_timeout_tag = atom.create("hook_timeout")
      let selector =
        process.new_selector()
        |> process.select_map(self_subject, fn(msg) { msg })
        |> process.select_record(init_timeout_tag, 1, fn(_dyn) {
          InitTimeoutFired
        })
        |> process.select_record(
          request_timeout_tag,
          1,
          fn(msg_dyn: Dynamic) -> ActorMessage {
            // gleam_erlang_ffi:select/2 passes the full Erlang tuple to the handler.
            // msg_dyn = {request_timeout, RequestId} (2-element tuple)
            // decode.at([1], ...) extracts index 1 (0-indexed), i.e., RequestId
            let request_id = case
              decode.run(msg_dyn, decode.at([1], decode.string))
            {
              Ok(id) -> id
              Error(_) -> "unknown"
            }
            RequestTimeoutFired(request_id)
          },
        )
        |> process.select_record(
          hook_done_tag,
          2,
          fn(msg_dyn: Dynamic) -> ActorMessage {
            // msg_dyn = {hook_done, RequestId, Result} (3-element tuple)
            let request_id = case
              decode.run(msg_dyn, decode.at([1], decode.string))
            {
              Ok(id) -> id
              Error(_) -> "unknown"
            }
            let result = case
              decode.run(msg_dyn, decode.at([2], decode.dynamic))
            {
              Ok(r) -> r
              Error(_) -> dynamic.nil()
            }
            HookDone(request_id, result)
          },
        )
        |> process.select_record(
          hook_error_tag,
          2,
          fn(msg_dyn: Dynamic) -> ActorMessage {
            // msg_dyn = {hook_error, RequestId, Reason} (3-element tuple)
            let request_id = case
              decode.run(msg_dyn, decode.at([1], decode.string))
            {
              Ok(id) -> id
              Error(_) -> "unknown"
            }
            let reason = case
              decode.run(msg_dyn, decode.at([2], decode.dynamic))
            {
              Ok(r) -> r
              Error(_) -> dynamic.nil()
            }
            HookError(request_id, reason)
          },
        )
        |> process.select_record(
          hook_timeout_tag,
          2,
          fn(msg_dyn: Dynamic) -> ActorMessage {
            // msg_dyn = {hook_timeout, RequestId, VerifyRef} (3-element tuple)
            let request_id = case
              decode.run(msg_dyn, decode.at([1], decode.string))
            {
              Ok(id) -> id
              Error(_) -> "unknown"
            }
            let verify_ref = case
              decode.run(msg_dyn, decode.at([2], decode.dynamic))
            {
              Ok(r) -> r
              Error(_) -> dynamic.nil()
            }
            HookTimeout(request_id, verify_ref)
          },
        )

      // Return the actor's subject to the caller with custom selector
      Ok(
        actor.initialised(state_after_init)
        |> actor.selecting(selector)
        |> actor.returning(self_subject),
      )
    })
    |> actor.on_message(handle_message)

  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(ActorStartFailed(err))
  }
}

/// Perform initialization handshake.
///
/// Called during actor init:
/// 1. Transition to InitSent
/// 2. Send initialize control_request
/// 3. Schedule init timeout timer
fn perform_init_handshake(state: SessionState) -> SessionState {
  // Transition from Starting to InitSent
  let assert Ok(new_lifecycle) = transition(state.lifecycle, CliSpawned)

  // Build the initialize request
  let assert Some(request_id) = state.init_request_id
  let init_request =
    control.Initialize(
      request_id: request_id,
      hooks: [],
      // Hooks will be populated from config in future
      mcp_servers: [],
      enable_file_checkpointing: state.file_checkpointing_enabled,
    )

  // Encode and send the request
  let json_line = control_encoder.encode_request(init_request) <> "\n"
  let write_fn = state.runner.write
  let _result = write_fn(json_line)

  // Schedule init timeout (send message to self after delay)
  // Store the timer reference for later cancellation
  let timer_ref = schedule_init_timeout(state.init_timeout_ms)

  SessionState(
    ..state,
    lifecycle: new_lifecycle,
    init_timer_ref: Some(timer_ref),
  )
}

/// Schedule init timeout by sending InitTimeoutFired message after delay.
///
/// Uses erlang:send_after/3 to schedule the timeout message.
/// Returns the timer reference for later cancellation.
@external(erlang, "bidir_ffi", "schedule_init_timeout")
fn schedule_init_timeout(timeout_ms: Int) -> Dynamic

/// Schedule request timeout by sending RequestTimeoutFired message after delay.
///
/// Uses erlang:send_after/3 to schedule the timeout message.
/// The message includes request_id for correlation.
/// Returns the timer reference for later cancellation.
@external(erlang, "bidir_ffi", "schedule_request_timeout")
fn schedule_request_timeout(timeout_ms: Int, request_id: String) -> Dynamic

/// Cancel a timer by its reference.
///
/// Returns True if the timer was cancelled, False if it had already fired.
@external(erlang, "bidir_ffi", "cancel_timer")
fn cancel_timer(timer_ref: Dynamic) -> Bool

/// Schedule hook timeout by sending HookTimeout message after delay.
///
/// Uses erlang:send_after/3 to schedule the timeout message.
/// The message includes request_id and verify_ref for correlation.
/// Returns {TimerRef, VerifyRef} tuple for cancellation and verification.
@external(erlang, "bidir_ffi", "schedule_hook_timeout")
fn schedule_hook_timeout(
  timeout_ms: Int,
  request_id: String,
) -> #(Dynamic, Dynamic)

/// Kill a task process immediately.
///
/// Uses erlang:exit/2 with 'kill' reason for immediate termination.
/// Used when hook timeout fires before task completes.
@external(erlang, "bidir_ffi", "kill_task")
fn kill_task(pid: process.Pid) -> Nil

/// Spawn a hook task to execute callback in separate process.
///
/// The task executes the handler with input and sends {hook_done, request_id, result}
/// back to parent. Returns {Pid, MonitorRef} tuple.
@external(erlang, "bidir_ffi", "spawn_hook_task")
fn spawn_hook_task(
  parent: process.Pid,
  request_id: String,
  handler: fn(Dynamic) -> Dynamic,
  input: Dynamic,
) -> #(process.Pid, process.Monitor)

/// Demonitor a hook task (cleanup after completion).
@external(erlang, "bidir_ffi", "demonitor_hook")
fn demonitor_hook(monitor_ref: process.Monitor) -> Nil

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
    GetCapabilities(reply_to) -> {
      process.send(reply_to, state.capabilities)
      actor.continue(state)
    }
    GetCheckpointingEnabled(reply_to) -> {
      process.send(reply_to, state.file_checkpointing_enabled)
      actor.continue(state)
    }
    Ping(reply_to) -> {
      process.send(reply_to, Pong)
      actor.continue(state)
    }
    ShutdownActor -> {
      // Clean shutdown with full cleanup
      cleanup_session(state, UserRequested)
      actor.stop()
    }
    InjectedMessage(json) -> {
      handle_injected_message(state, json)
    }
    InjectedPortClosed -> {
      handle_port_closed(state)
    }
    InitTimeoutFired -> {
      handle_init_timeout(state)
    }
    SendControlRequest(request, reply_to) -> {
      handle_send_control_request(state, request, reply_to)
    }
    RequestTimeoutFired(request_id) -> {
      handle_request_timeout(state, request_id)
    }
    HookDone(request_id, result) -> {
      handle_hook_done(state, request_id, result)
    }
    HookError(request_id, _reason) -> {
      handle_hook_error(state, request_id)
    }
    HookTimeout(request_id, verify_ref) -> {
      handle_hook_timeout(state, request_id, verify_ref)
    }
    CancelPendingRequest(request_id) -> {
      handle_cancel_pending_request(state, request_id)
    }
  }
}

/// Handle hook task completion.
///
/// Called when a spawned hook task sends back its result. Looks up the
/// pending hook, demonitors the task, sends response to CLI, and removes
/// from pending_hooks.
fn handle_hook_done(
  state: SessionState,
  request_id: String,
  result: Dynamic,
) -> actor.Next(SessionState, ActorMessage) {
  case dict.get(state.pending_hooks, request_id) {
    Ok(pending) -> {
      // Cancel the timeout timer (we won the race)
      let _ = cancel_timer(pending.timer_ref)

      // Demonitor the task (cleanup)
      demonitor_hook(pending.monitor_ref)

      // Send success response to CLI
      let response = HookResponse(request_id, HookSuccess(result))
      send_control_response(state, response)

      // Remove from pending_hooks
      let new_pending = dict.delete(state.pending_hooks, request_id)
      actor.continue(SessionState(..state, pending_hooks: new_pending))
    }
    Error(Nil) -> {
      // Already handled (timeout won), ignore
      actor.continue(state)
    }
  }
}

/// Handle hook task crash/error.
///
/// Called when a spawned hook task crashes. Sends a fail-open response
/// to the CLI to prevent hanging, then cleans up the pending hook entry.
fn handle_hook_error(
  state: SessionState,
  request_id: String,
) -> actor.Next(SessionState, ActorMessage) {
  case dict.get(state.pending_hooks, request_id) {
    Ok(pending) -> {
      // Cancel the timeout timer (we won the race)
      let _ = cancel_timer(pending.timer_ref)

      // Demonitor the task (cleanup)
      demonitor_hook(pending.monitor_ref)

      // Send fail-open response to CLI (allow continuation despite error)
      let fail_open_result = to_dynamic(dict.from_list([#("continue", True)]))
      let response = HookResponse(request_id, HookSuccess(fail_open_result))
      send_control_response(state, response)

      // Remove from pending_hooks
      let new_pending = dict.delete(state.pending_hooks, request_id)
      actor.continue(SessionState(..state, pending_hooks: new_pending))
    }
    Error(Nil) -> {
      // Already handled (timeout won), ignore
      actor.continue(state)
    }
  }
}

/// Handle hook timeout (first-event-wins protocol).
///
/// Called when the timeout fires before hook task completes. Kills the task,
/// sends fail-open response, and removes from pending_hooks.
fn handle_hook_timeout(
  state: SessionState,
  request_id: String,
  msg_verify_ref: Dynamic,
) -> actor.Next(SessionState, ActorMessage) {
  case dict.get(state.pending_hooks, request_id) {
    Ok(pending) -> {
      // Verify ref matches to prevent stale timeouts from killing wrong task
      case dynamic_equals(pending.verify_ref, msg_verify_ref) {
        True -> {
          // Kill the task (timeout won the race)
          kill_task(pending.task_pid)

          // Demonitor with flush to clear any DOWN message
          demonitor_hook(pending.monitor_ref)

          // Send fail-open response to CLI (allow continuation despite timeout)
          let fail_open_result =
            to_dynamic(dict.from_list([#("continue", True)]))
          let response = HookResponse(request_id, HookSuccess(fail_open_result))
          send_control_response(state, response)

          // Remove from pending_hooks
          let new_pending = dict.delete(state.pending_hooks, request_id)
          actor.continue(SessionState(..state, pending_hooks: new_pending))
        }
        False -> {
          // Stale timeout (timer_ref mismatch), ignore
          actor.continue(state)
        }
      }
    }
    Error(Nil) -> {
      // Already handled (HookDone/HookError won), ignore stale timeout
      actor.continue(state)
    }
  }
}

/// Handle injected JSON message (simulated CLI response).
fn handle_injected_message(
  state: SessionState,
  json: String,
) -> actor.Next(SessionState, ActorMessage) {
  // Decode the JSON line
  case control_decoder.decode_line(json) {
    Ok(incoming) -> {
      // Use route_incoming to determine dispatch destination
      case route_incoming(incoming) {
        RouteResponse(_request_id) -> {
          // Control response - handle via response correlation
          case incoming {
            ControlResponse(response) ->
              handle_control_response(state, response)
            _ -> actor.continue(state)
          }
        }
        RouteHookCallback(_) | RoutePermission(_) | RouteMcp(_) -> {
          // Control request - may trigger implicit confirmation during InitSent
          case incoming {
            ControlRequest(request) -> handle_control_request(state, request)
            _ -> actor.continue(state)
          }
        }
        RouteSubscriber -> {
          // Regular message - forward to subscriber
          case incoming {
            RegularMessage(msg) -> {
              forward_to_subscriber(state, msg)
            }
            _ -> actor.continue(state)
          }
        }
      }
    }
    Error(_decode_error) -> {
      // Invalid JSON - log and drop (fail-safe)
      actor.continue(state)
    }
  }
}

/// Handle control response from CLI.
fn handle_control_response(
  state: SessionState,
  response: IncomingControlResponse,
) -> actor.Next(SessionState, ActorMessage) {
  case response {
    Success(request_id, payload) -> {
      handle_success_response(state, request_id, payload)
    }
    ControlError(request_id, message) -> {
      handle_error_response(state, request_id, message)
    }
  }
}

/// Handle successful control response.
fn handle_success_response(
  state: SessionState,
  request_id: String,
  payload: Dynamic,
) -> actor.Next(SessionState, ActorMessage) {
  // Check if this is the init response
  case state.init_request_id {
    Some(init_id) if init_id == request_id -> {
      // This is the init success response
      handle_init_success(state, payload)
    }
    _ -> {
      // Not init response - correlate with pending_requests
      case dict.get(state.pending_requests, request_id) {
        Ok(pending) -> {
          // Cancel timeout timer
          case pending.timer_ref {
            Some(timer_ref) -> {
              let _ = cancel_timer(timer_ref)
              Nil
            }
            None -> Nil
          }
          // Found matching request - resolve it
          resolve_pending(pending, Success(request_id, payload))
          // Remove from pending_requests
          let new_state =
            SessionState(
              ..state,
              pending_requests: dict.delete(state.pending_requests, request_id),
            )
          actor.continue(new_state)
        }
        Error(Nil) -> {
          // No matching pending request - ignore (stale/duplicate)
          actor.continue(state)
        }
      }
    }
  }
}

/// Handle init success - transition to Running and store capabilities.
fn handle_init_success(
  state: SessionState,
  payload: Dynamic,
) -> actor.Next(SessionState, ActorMessage) {
  // Only process if in InitSent state
  case state.lifecycle {
    InitSent -> {
      // Transition to Running
      let assert Ok(new_lifecycle) = transition(state.lifecycle, InitSuccess)

      // Parse capabilities from payload (basic extraction)
      let capabilities = parse_capabilities(payload)

      // Cancel init timeout timer and clear init state
      case state.init_timer_ref {
        Some(timer_ref) -> {
          let _ = cancel_timer(timer_ref)
          Nil
        }
        None -> Nil
      }
      let new_state =
        SessionState(
          ..state,
          lifecycle: new_lifecycle,
          capabilities: Some(capabilities),
          init_request_id: None,
          init_timer_ref: None,
        )

      // Flush queued operations (non-blocking)
      let flushed_state = flush_queued_ops(new_state)

      actor.continue(flushed_state)
    }
    _ -> {
      // Not in InitSent - ignore (might be duplicate response)
      actor.continue(state)
    }
  }
}

/// Parse CLI capabilities from init response payload.
fn parse_capabilities(_payload: Dynamic) -> CliCapabilities {
  // For now, return default capabilities
  // Full parsing will be added when capabilities format is finalized
  CliCapabilities(
    supported_commands: [],
    hooks_supported: True,
    permissions_supported: True,
    mcp_sdk_servers_supported: True,
  )
}

/// Handle error control response.
fn handle_error_response(
  state: SessionState,
  request_id: String,
  message: String,
) -> actor.Next(SessionState, ActorMessage) {
  // Check if this is the init response
  case state.init_request_id {
    Some(init_id) if init_id == request_id -> {
      // This is init error response
      handle_init_error(state, message)
    }
    _ -> {
      // Not init response - correlate with pending_requests
      case dict.get(state.pending_requests, request_id) {
        Ok(pending) -> {
          // Cancel timeout timer
          case pending.timer_ref {
            Some(timer_ref) -> {
              let _ = cancel_timer(timer_ref)
              Nil
            }
            None -> Nil
          }
          // Found matching request - resolve with error
          resolve_pending(pending, ControlError(request_id, message))
          // Remove from pending_requests
          let new_state =
            SessionState(
              ..state,
              pending_requests: dict.delete(state.pending_requests, request_id),
            )
          actor.continue(new_state)
        }
        Error(Nil) -> {
          // No matching pending request - ignore (stale/duplicate)
          actor.continue(state)
        }
      }
    }
  }
}

/// Handle init error - transition to Failed.
fn handle_init_error(
  state: SessionState,
  message: String,
) -> actor.Next(SessionState, ActorMessage) {
  // Only process if in InitSent state
  case state.lifecycle {
    InitSent -> {
      // Transition to Failed with error message
      let assert Ok(new_lifecycle) =
        transition(state.lifecycle, ErrorOccurred(message))

      // Cleanup and stop
      let new_state = SessionState(..state, lifecycle: new_lifecycle)
      cleanup_session(new_state, InitFailed(InitializationError(message)))
      actor.stop()
    }
    _ -> {
      // Not in InitSent - ignore
      actor.continue(state)
    }
  }
}

/// Handle incoming control request from CLI.
///
/// Control requests (hook_callback, can_use_tool, mcp_message) during InitSent
/// trigger implicit confirmation - proving CLI accepted our handshake.
fn handle_control_request(
  state: SessionState,
  request: IncomingControlRequest,
) -> actor.Next(SessionState, ActorMessage) {
  case state.lifecycle {
    InitSent -> {
      // Implicit confirmation! CLI is sending control requests,
      // proving it understood our initialization and registered our hooks.
      handle_implicit_confirmation(state, request)
    }
    Running -> {
      // Normal request processing in Running state
      case request {
        HookCallback(request_id, callback_id, input, _tool_use_id) -> {
          dispatch_hook_callback(state, request_id, callback_id, input)
        }
        CanUseTool(..) | McpMessage(..) -> {
          // TODO: Implement can_use_tool and mcp_message handlers
          actor.continue(state)
        }
      }
    }
    _ -> {
      // Not in a state to handle requests - ignore
      actor.continue(state)
    }
  }
}

/// Dispatch a hook callback to the registered handler.
///
/// Looks up the callback_id in HookConfig and invokes the handler.
/// Sends control_response back to CLI with the result.
/// Unknown callback_ids are logged and ignored (fail-open).
fn dispatch_hook_callback(
  state: SessionState,
  request_id: String,
  callback_id: String,
  input: Dynamic,
) -> actor.Next(SessionState, ActorMessage) {
  // Check backpressure limit before spawning task
  case dict.size(state.pending_hooks) >= max_pending_hooks {
    True -> {
      // At capacity - send fail-open response without spawning
      let fail_open_result = to_dynamic(dict.from_list([#("continue", True)]))
      let response = HookResponse(request_id, HookSuccess(fail_open_result))
      send_control_response(state, response)
      actor.continue(state)
    }
    False -> {
      case dict.get(state.hooks.handlers, callback_id) {
        Ok(handler) -> {
          // Spawn a task to execute the handler asynchronously
          // The task will send HookDone message back when complete
          let parent_pid = process.self()
          let #(task_pid, monitor_ref) =
            spawn_hook_task(parent_pid, request_id, handler, input)

          // Schedule timeout for first-event-wins protocol
          // Returns {TimerRef, VerifyRef} tuple
          let #(timer_ref, verify_ref) =
            schedule_hook_timeout(state.default_hook_timeout_ms, request_id)

          // Record the pending hook with timer and verify references
          let pending =
            PendingHook(
              task_pid: task_pid,
              monitor_ref: monitor_ref,
              timer_ref: timer_ref,
              verify_ref: verify_ref,
              callback_id: callback_id,
              request_id: request_id,
              received_at: 0,
            )
          let new_pending =
            dict.insert(state.pending_hooks, request_id, pending)
          actor.continue(SessionState(..state, pending_hooks: new_pending))
        }
        Error(Nil) -> {
          // Unknown callback_id - send fail-open success response
          // This allows the CLI to proceed rather than hang waiting for a response
          let fail_open_result =
            to_dynamic(dict.from_list([#("continue", True)]))
          let response = HookResponse(request_id, HookSuccess(fail_open_result))
          send_control_response(state, response)
          actor.continue(state)
        }
      }
    }
  }
}

/// Send a control response to the CLI via the runner.
fn send_control_response(
  state: SessionState,
  response: OutgoingControlResponse,
) -> Nil {
  let json_line = control_encoder.encode_response(response) <> "\n"
  let write_fn = state.runner.write
  let _result = write_fn(json_line)
  Nil
}

/// Handle implicit confirmation during InitSent.
///
/// When CLI sends hook_callback, can_use_tool, or mcp_message before
/// explicit init response, it proves CLI accepted the handshake.
///
/// Process:
/// 1. Transition to Running
/// 2. Clear init_request_id (init timeout will be ignored)
/// 3. Flush queued_ops
/// 4. Process the triggering request normally
fn handle_implicit_confirmation(
  state: SessionState,
  request: IncomingControlRequest,
) -> actor.Next(SessionState, ActorMessage) {
  // Transition to Running
  let assert Ok(new_lifecycle) = transition(state.lifecycle, InitSuccess)

  // Cancel init timeout timer
  case state.init_timer_ref {
    Some(timer_ref) -> {
      let _ = cancel_timer(timer_ref)
      Nil
    }
    None -> Nil
  }

  // Clear init state since init is implicitly confirmed
  // Use default capabilities since we didn't get explicit response
  let new_state =
    SessionState(
      ..state,
      lifecycle: new_lifecycle,
      capabilities: Some(CliCapabilities(
        supported_commands: [],
        hooks_supported: True,
        permissions_supported: True,
        mcp_sdk_servers_supported: True,
      )),
      init_request_id: None,
      init_timer_ref: None,
    )

  // Flush queued operations (non-blocking)
  let flushed_state = flush_queued_ops(new_state)

  // Now process the triggering request in Running state
  // Dispatch the request that triggered implicit confirmation
  case request {
    HookCallback(request_id, callback_id, input, _tool_use_id) -> {
      dispatch_hook_callback(flushed_state, request_id, callback_id, input)
    }
    CanUseTool(..) | McpMessage(..) -> {
      // TODO: Implement can_use_tool and mcp_message handlers
      actor.continue(flushed_state)
    }
  }
}

/// Handle port closed event.
fn handle_port_closed(
  state: SessionState,
) -> actor.Next(SessionState, ActorMessage) {
  case state.lifecycle {
    InitSent -> {
      // Port closed during init - transition to Failed
      let assert Ok(new_lifecycle) = transition(state.lifecycle, PortClosed)
      let new_state = SessionState(..state, lifecycle: new_lifecycle)
      cleanup_session(new_state, InitFailed(CliExitedDuringInit))
      actor.stop()
    }
    Running -> {
      // Port closed during running - transition to Stopped
      let assert Ok(new_lifecycle) = transition(state.lifecycle, PortClosed)
      let new_state = SessionState(..state, lifecycle: new_lifecycle)
      cleanup_session(new_state, CliExited(0))
      actor.stop()
    }
    _ -> {
      // Already stopped/failed - ignore
      actor.continue(state)
    }
  }
}

/// Handle init timeout.
fn handle_init_timeout(
  state: SessionState,
) -> actor.Next(SessionState, ActorMessage) {
  case state.lifecycle {
    InitSent -> {
      // Timeout during init - transition to Failed
      let assert Ok(new_lifecycle) = transition(state.lifecycle, InitTimeout)
      let new_state = SessionState(..state, lifecycle: new_lifecycle)
      cleanup_session(new_state, InitFailed(InitializationTimeout))
      actor.stop()
    }
    _ -> {
      // Not in InitSent - timeout is stale, ignore
      actor.continue(state)
    }
  }
}

/// Handle SDK-initiated control request.
///
/// If Running, encodes the request, sends to CLI, schedules timeout, and
/// registers pending request. If not Running, queues the request for later.
fn handle_send_control_request(
  state: SessionState,
  request: OutgoingControlRequest,
  reply_to: Subject(RequestResult),
) -> actor.Next(SessionState, ActorMessage) {
  // Extract request_id from the request
  let request_id = get_request_id(request)

  case state.lifecycle {
    Running -> {
      // Check backpressure limit
      case dict.size(state.pending_requests) >= max_pending_requests {
        True -> {
          // At capacity - reject immediately
          process.send(
            reply_to,
            RequestError("Too many pending control requests (max 64)"),
          )
          actor.continue(state)
        }
        False -> {
          // Encode and send request to CLI
          let json_payload = control_encoder.encode_request(request)
          let write_fn = state.runner.write
          let _result = write_fn(json_payload <> "\n")

          // Schedule timeout timer
          let timer_ref =
            schedule_request_timeout(state.default_timeout_ms, request_id)

          // Register pending request
          let pending_req =
            PendingRequest(
              request_id: request_id,
              reply_to: reply_to,
              sent_at: 0,
              timer_ref: Some(timer_ref),
            )
          let new_pending =
            dict.insert(state.pending_requests, request_id, pending_req)
          let new_state = SessionState(..state, pending_requests: new_pending)
          actor.continue(new_state)
        }
      }
    }
    InitSent -> {
      // Queue request for later
      let json_payload = control_encoder.encode_request(request)
      let queued_op = QueuedRequest(request_id, json_payload, reply_to)
      case queue_operation(state, queued_op) {
        Ok(new_state) -> actor.continue(new_state)
        Error(err) -> {
          // Queue overflow - reject request
          case err {
            InitQueueOverflow(msg) -> process.send(reply_to, RequestError(msg))
            _ -> process.send(reply_to, RequestError("Queue overflow"))
          }
          actor.continue(state)
        }
      }
    }
    _ -> {
      // Session not ready - reject immediately
      process.send(reply_to, RequestSessionStopped)
      actor.continue(state)
    }
  }
}

/// Extract request_id from an OutgoingControlRequest.
fn get_request_id(request: OutgoingControlRequest) -> String {
  case request {
    control.Initialize(id, _, _, _) -> id
    control.Interrupt(id) -> id
    control.SetPermissionMode(id, _) -> id
    control.SetModel(id, _) -> id
    control.RewindFiles(id, _) -> id
  }
}

/// Handle request timeout.
///
/// If the request is still pending, sends RequestTimeout to the caller
/// and removes from pending_requests. If not found, the response already
/// arrived and this timeout is stale.
fn handle_request_timeout(
  state: SessionState,
  request_id: String,
) -> actor.Next(SessionState, ActorMessage) {
  case dict.get(state.pending_requests, request_id) {
    Ok(pending) -> {
      // Request still pending - send timeout to caller
      process.send(pending.reply_to, RequestTimeout)
      // Remove from pending (timer already fired, no need to cancel)
      let new_pending = dict.delete(state.pending_requests, request_id)
      let new_state = SessionState(..state, pending_requests: new_pending)
      actor.continue(new_state)
    }
    Error(Nil) -> {
      // Request already handled - stale timeout, ignore
      actor.continue(state)
    }
  }
}

/// Handle cancel pending request (client-side timeout cleanup).
///
/// Called when a client times out locally and wants to prevent the actor
/// from sending a stale timeout message later. This cancels the timer and
/// removes the pending request without sending any response.
fn handle_cancel_pending_request(
  state: SessionState,
  request_id: String,
) -> actor.Next(SessionState, ActorMessage) {
  case dict.get(state.pending_requests, request_id) {
    Ok(pending) -> {
      // Cancel the actor's timer to prevent stale timeout message
      case pending.timer_ref {
        Some(ref) -> {
          let _ = cancel_timer(ref)
          Nil
        }
        None -> Nil
      }
      // Remove from pending (no response sent - client already timed out)
      let new_pending = dict.delete(state.pending_requests, request_id)
      let new_state = SessionState(..state, pending_requests: new_pending)
      actor.continue(new_state)
    }
    Error(Nil) -> {
      // Request already handled, nothing to cancel
      actor.continue(state)
    }
  }
}

/// Flush queued operations after successful init.
///
/// Sends all queued operations to CLI without blocking.
/// Each operation gets its own pending request entry.
///
/// Note: Overflow errors are sent as RequestError (not SessionError) since
/// reply_to subjects expect RequestResult, consistent with send_control_request.
@internal
pub fn flush_queued_ops(state: SessionState) -> SessionState {
  // Reverse to process in FIFO order (queue_operation prepends)
  let ops_in_order = list.reverse(state.queued_ops)

  // Process each queued operation and accumulate updated pending_requests
  let #(new_pending, _) =
    list.fold(
      ops_in_order,
      #(state.pending_requests, state.runner),
      fn(acc, op) {
        let #(pending, runner) = acc
        case op {
          QueuedRequest(request_id, json_payload, reply_to) -> {
            // Enforce max_pending_requests backpressure limit
            case dict.size(pending) >= max_pending_requests {
              True -> {
                // At capacity: send error to caller, don't insert pending entry
                process.send(
                  reply_to,
                  RequestError("Too many pending control requests (max 64)"),
                )
                #(pending, runner)
              }
              False -> {
                // Send pre-encoded JSON to CLI (payload is already a String)
                let write_fn = runner.write
                let _result = write_fn(json_payload <> "\n")

                // Schedule timeout timer for the flushed request
                let timer_ref =
                  schedule_request_timeout(state.default_timeout_ms, request_id)

                // Register in pending_requests for response correlation
                let pending_req =
                  PendingRequest(
                    request_id: request_id,
                    reply_to: reply_to,
                    sent_at: 0,
                    timer_ref: Some(timer_ref),
                  )
                let updated_pending =
                  dict.insert(pending, request_id, pending_req)
                #(updated_pending, runner)
              }
            }
          }
        }
      },
    )

  SessionState(..state, queued_ops: [], pending_requests: new_pending)
}

/// Forward a regular message to the subscriber.
///
/// Wraps the message in CliMessage envelope and sends to subscriber Subject.
fn forward_to_subscriber(
  state: SessionState,
  msg: message.Message,
) -> actor.Next(SessionState, ActorMessage) {
  // Convert Message to Dynamic for CliMessage envelope
  let payload = to_dynamic(msg)
  process.send(state.subscriber, CliMessage(payload))
  actor.continue(state)
}

/// Clean up session resources when stopping.
///
/// This function performs the full cleanup sequence:
/// 1. Cancel init timeout timer (if active)
/// 2. Resolve all pending_requests with RequestSessionStopped
/// 3. Resolve all queued_ops with RequestSessionStopped
/// 4. Close the runner (terminates CLI process)
/// 5. Notify subscriber with SessionEnded
///
/// State maps are not explicitly cleared since actor.stop() is called
/// immediately after cleanup, terminating the actor process.
fn cleanup_session(state: SessionState, reason: StopReason) -> Nil {
  // 1. Cancel init timeout timer if active
  case state.init_timer_ref {
    Some(timer_ref) -> {
      let _ = cancel_timer(timer_ref)
      Nil
    }
    None -> Nil
  }

  // 2. Cancel timers and resolve all pending SDK-initiated requests
  let _ =
    dict.each(state.pending_requests, fn(_request_id, pending) {
      // Cancel timeout timer
      case pending.timer_ref {
        Some(timer_ref) -> {
          let _ = cancel_timer(timer_ref)
          Nil
        }
        None -> Nil
      }
      // Send session stopped to caller
      process.send(pending.reply_to, RequestSessionStopped)
    })

  // 3. Cancel timers and kill tasks for pending hook callbacks
  let _ =
    dict.each(state.pending_hooks, fn(_request_id, pending) {
      // Cancel timeout timer
      let _ = cancel_timer(pending.timer_ref)
      // Kill the task
      kill_task(pending.task_pid)
      // Demonitor to avoid stray DOWN messages
      demonitor_hook(pending.monitor_ref)
      Nil
    })

  // 4. Resolve all queued operations with session stopped error
  list.each(state.queued_ops, fn(op) {
    case op {
      QueuedRequest(_request_id, _payload, reply_to) -> {
        process.send(reply_to, RequestSessionStopped)
      }
    }
  })

  // 5. Close the runner (terminates CLI process)
  let close_fn = state.runner.close
  close_fn()

  // 6. Notify subscriber that session has ended
  process.send(state.subscriber, SessionEnded(reason))

  Nil
}

/// Call the actor synchronously to get lifecycle state.
pub fn get_lifecycle(
  session: Subject(ActorMessage),
  timeout: Int,
) -> SessionLifecycle {
  actor.call(session, timeout, GetLifecycle)
}

/// Get the PID of the session actor.
pub fn get_pid(session: Subject(ActorMessage)) -> process.Pid {
  let assert Ok(pid) = process.subject_owner(session)
  pid
}

/// Call the actor synchronously with a ping.
pub fn ping(session: Subject(ActorMessage), timeout: Int) -> Response {
  actor.call(session, timeout, Ping)
}

/// Get CLI capabilities (populated after successful init).
pub fn get_capabilities(
  session: Subject(ActorMessage),
  timeout: Int,
) -> Option(CliCapabilities) {
  actor.call(session, timeout, GetCapabilities)
}

/// Shutdown the session gracefully.
pub fn shutdown(session: Subject(ActorMessage)) -> Nil {
  actor.send(session, ShutdownActor)
}

/// Send an InjectedPortClosed message to the actor (for testing).
pub fn inject_port_closed(session: Subject(ActorMessage)) -> Nil {
  actor.send(session, InjectedPortClosed)
}

/// Send a control request to the CLI.
///
/// The request will be encoded and sent immediately if session is Running.
/// If session is in InitSent state, the request is queued and will be sent
/// after initialization completes.
///
/// Results are delivered asynchronously to the reply_to Subject as RequestResult.
/// A timeout timer is automatically scheduled based on default_timeout_ms.
pub fn send_control_request(
  session: Subject(ActorMessage),
  request: OutgoingControlRequest,
  reply_to: Subject(RequestResult),
) -> Nil {
  actor.send(session, SendControlRequest(request, reply_to))
}

/// Cancel a pending request by ID.
///
/// Used for client-side timeout cleanup. When a client times out locally
/// before the actor's timer fires, this cancels the actor's timer and
/// removes the pending request to prevent stale messages.
///
/// This is safe to call even if the request was already handled.
pub fn cancel_pending_request(
  session: Subject(ActorMessage),
  request_id: String,
) -> Nil {
  actor.send(session, CancelPendingRequest(request_id))
}

// =============================================================================
// Public Control Operations
// =============================================================================

/// Default timeout for interrupt operation (5000ms).
const interrupt_timeout_ms: Int = 5000

/// Interrupt the current operation.
///
/// Signals the CLI to stop the current processing and return control.
/// This is a synchronous call that blocks until the CLI responds or times out.
///
/// ## Timeout
///
/// Uses a 5000ms timeout. Interrupt should complete quickly since it just
/// signals the CLI to stop.
///
/// ## Returns
///
/// - `Ok(Nil)` - Interrupt succeeded
/// - `Error(CliError(message))` - CLI returned an error (e.g., nothing to interrupt)
/// - `Error(InterruptTimeout)` - No response within 5000ms
/// - `Error(SessionStopped)` - Session is not running
///
/// ## Example
///
/// ```gleam
/// case bidir.interrupt(session) {
///   Ok(Nil) -> io.println("Interrupted successfully")
///   Error(CliError(msg)) -> io.println("Interrupt failed: " <> msg)
///   Error(InterruptTimeout) -> io.println("Interrupt timed out")
///   Error(SessionStopped) -> io.println("Session not running")
/// }
/// ```
pub fn interrupt(session: Subject(ActorMessage)) -> Result(Nil, InterruptError) {
  // Generate a request ID for this interrupt
  let request_id = generate_request_id()
  let request = Interrupt(request_id)

  // Create subject to receive the result
  let result_subject: Subject(RequestResult) = process.new_subject()

  // Send the request
  send_control_request(session, request, result_subject)

  // Wait for response with 5000ms timeout
  // If the actor is alive, it will respond (success, error, or timeout)
  // If the actor is stopped, we timeout here and return InterruptTimeout
  case process.receive(result_subject, interrupt_timeout_ms) {
    Ok(RequestSuccess(_)) -> Ok(Nil)
    Ok(RequestError(message)) -> Error(CliError(message))
    Ok(RequestTimeout) -> Error(InterruptTimeout)
    Ok(RequestSessionStopped) -> Error(SessionStopped)
    // Actor didn't respond in time (possibly stopped) - cancel pending request
    // to prevent stale timeout message from polluting caller's mailbox
    Error(Nil) -> {
      cancel_pending_request(session, request_id)
      Error(InterruptTimeout)
    }
  }
}

/// Default timeout for set_permission_mode operation (5000ms).
const set_permission_mode_timeout_ms: Int = 5000

/// Set the permission mode for the session.
///
/// Changes how the CLI handles permission requests. This is a synchronous call
/// that blocks until the CLI responds or times out.
///
/// ## Permission Modes
///
/// - `Default` - Normal permission prompting behavior
/// - `AcceptEdits` - Auto-accept file edits
/// - `BypassPermissions` - Bypass all permission checks
/// - `Plan` - Plan mode (read-only exploration)
///
/// ## Timeout
///
/// Uses a 5000ms timeout. Configuration changes should complete quickly.
///
/// ## Returns
///
/// - `Ok(Nil)` - Permission mode changed successfully
/// - `Error(SetPermissionModeCliError(message))` - CLI returned an error
/// - `Error(SetPermissionModeTimeout)` - No response within 5000ms
/// - `Error(SetPermissionModeSessionStopped)` - Session is not running
///
/// ## Example
///
/// ```gleam
/// import claude_agent_sdk/control.{AcceptEdits}
///
/// case bidir.set_permission_mode(session, AcceptEdits) {
///   Ok(Nil) -> io.println("Permission mode set to AcceptEdits")
///   Error(SetPermissionModeCliError(msg)) -> io.println("Failed: " <> msg)
///   Error(SetPermissionModeTimeout) -> io.println("Timed out")
///   Error(SetPermissionModeSessionStopped) -> io.println("Session not running")
/// }
/// ```
pub fn set_permission_mode(
  session: Subject(ActorMessage),
  mode: PermissionMode,
) -> Result(Nil, SetPermissionModeError) {
  // Generate a request ID for this operation
  let request_id = generate_request_id()
  let request = SetPermissionMode(request_id, mode)

  // Create subject to receive the result
  let result_subject: Subject(RequestResult) = process.new_subject()

  // Send the request
  send_control_request(session, request, result_subject)

  // Wait for response with 5000ms timeout
  case process.receive(result_subject, set_permission_mode_timeout_ms) {
    Ok(RequestSuccess(_)) -> Ok(Nil)
    Ok(RequestError(message)) -> Error(SetPermissionModeCliError(message))
    Ok(RequestTimeout) -> Error(SetPermissionModeTimeout)
    Ok(RequestSessionStopped) -> Error(SetPermissionModeSessionStopped)
    Error(Nil) -> {
      // Client timed out before actor response - cancel the pending request
      // to prevent stale timeout message from polluting caller's mailbox
      cancel_pending_request(session, request_id)
      Error(SetPermissionModeTimeout)
    }
  }
}

/// Default timeout for set_model operation (5000ms).
const set_model_timeout_ms: Int = 5000

/// Set the model for the session.
///
/// Changes the model used by the CLI for subsequent operations. This is a
/// synchronous call that blocks until the CLI responds or times out.
///
/// ## Model Values
///
/// Common values: "sonnet", "opus", "haiku", or full model IDs like
/// "claude-3-5-sonnet-20241022".
///
/// ## Timeout
///
/// Uses a 5000ms timeout. Configuration changes should complete quickly.
///
/// ## Returns
///
/// - `Ok(Nil)` - Model changed successfully
/// - `Error(SetModelCliError(message))` - CLI returned an error (e.g., invalid model)
/// - `Error(SetModelTimeout)` - No response within 5000ms
/// - `Error(SetModelSessionStopped)` - Session is not running
///
/// ## Example
///
/// ```gleam
/// case bidir.set_model(session, "sonnet") {
///   Ok(Nil) -> io.println("Model set to sonnet")
///   Error(SetModelCliError(msg)) -> io.println("Failed: " <> msg)
///   Error(SetModelTimeout) -> io.println("Timed out")
///   Error(SetModelSessionStopped) -> io.println("Session not running")
/// }
/// ```
pub fn set_model(
  session: Subject(ActorMessage),
  model: String,
) -> Result(Nil, SetModelError) {
  // Generate a request ID for this operation
  let request_id = generate_request_id()
  let request = SetModel(request_id, model)

  // Create subject to receive the result
  let result_subject: Subject(RequestResult) = process.new_subject()

  // Send the request
  send_control_request(session, request, result_subject)

  // Wait for response with 5000ms timeout
  case process.receive(result_subject, set_model_timeout_ms) {
    Ok(RequestSuccess(_)) -> Ok(Nil)
    Ok(RequestError(message)) -> Error(SetModelCliError(message))
    Ok(RequestTimeout) -> Error(SetModelTimeout)
    Ok(RequestSessionStopped) -> Error(SetModelSessionStopped)
    Error(Nil) -> Error(SetModelTimeout)
  }
}

/// Default timeout for rewind_files operation (30000ms).
/// Longer timeout due to potential disk I/O for checkpoint restoration.
const rewind_files_timeout_ms: Int = 30_000

/// Rewind files to the state at a given user message checkpoint.
///
/// Restores file state to a previously captured checkpoint. This requires
/// file checkpointing to be enabled when starting the session (via
/// `enable_file_checkpointing: True` in StartConfig).
///
/// This is a synchronous call that blocks until the CLI responds or times out.
///
/// ## Requirements
///
/// File checkpointing must be enabled during session initialization.
/// If not enabled, returns `Error(CheckpointingNotEnabled)` immediately
/// without sending a request to the CLI.
///
/// ## Timeout
///
/// Uses a 30000ms timeout (longer than other operations) because file
/// restoration may involve disk I/O.
///
/// ## Returns
///
/// - `Ok(Nil)` - Files rewound successfully
/// - `Error(CheckpointingNotEnabled)` - Checkpointing was not enabled at session start
/// - `Error(RewindFilesCliError(message))` - CLI returned an error (e.g., checkpoint not found)
/// - `Error(RewindFilesTimeout)` - No response within 30000ms
/// - `Error(RewindFilesSessionStopped)` - Session is not running
///
/// ## Example
///
/// ```gleam
/// // Start session with checkpointing enabled
/// let config = StartConfig(
///   ..bidir.default_config(subscriber),
///   enable_file_checkpointing: True,
/// )
/// let assert Ok(session) = bidir.start(runner, config)
///
/// // Later, rewind to a checkpoint
/// case bidir.rewind_files(session, "msg_123", 30_000) {
///   Ok(Nil) -> io.println("Files rewound successfully")
///   Error(CheckpointingNotEnabled) -> io.println("Checkpointing not enabled")
///   Error(RewindFilesCliError(msg)) -> io.println("Failed: " <> msg)
///   Error(RewindFilesTimeout) -> io.println("Timed out")
///   Error(RewindFilesSessionStopped) -> io.println("Session not running")
/// }
/// ```
pub fn rewind_files(
  session: Subject(ActorMessage),
  user_message_id: String,
  timeout_ms: Int,
) -> Result(Nil, RewindFilesError) {
  // First check if checkpointing is enabled
  let checkpointing_subject: Subject(Bool) = process.new_subject()
  actor.send(session, GetCheckpointingEnabled(checkpointing_subject))

  // Wait briefly for the checkpointing state (should be fast, just memory read)
  case process.receive(checkpointing_subject, 1000) {
    Ok(True) -> {
      // Checkpointing enabled - proceed with the request
      rewind_files_internal(session, user_message_id, timeout_ms)
    }
    Ok(False) -> {
      // Checkpointing not enabled - return error immediately
      Error(CheckpointingNotEnabled)
    }
    Error(Nil) -> {
      // Actor didn't respond - session likely stopped
      Error(RewindFilesSessionStopped)
    }
  }
}

/// Internal function to send rewind_files request (after checkpointing check).
fn rewind_files_internal(
  session: Subject(ActorMessage),
  user_message_id: String,
  timeout_ms: Int,
) -> Result(Nil, RewindFilesError) {
  // Generate a request ID for this operation
  let request_id = generate_request_id()
  let request = control.RewindFiles(request_id, user_message_id)

  // Create subject to receive the result
  let result_subject: Subject(RequestResult) = process.new_subject()

  // Send the request
  send_control_request(session, request, result_subject)

  // Wait for response with specified timeout (default 30s for disk I/O)
  let effective_timeout = case timeout_ms > 0 {
    True -> timeout_ms
    False -> rewind_files_timeout_ms
  }

  case process.receive(result_subject, effective_timeout) {
    Ok(RequestSuccess(_)) -> Ok(Nil)
    Ok(RequestError(message)) -> Error(RewindFilesCliError(message))
    Ok(RequestTimeout) -> Error(RewindFilesTimeout)
    Ok(RequestSessionStopped) -> Error(RewindFilesSessionStopped)
    Error(Nil) -> {
      // Client timed out before actor response - cancel the pending request
      cancel_pending_request(session, request_id)
      Error(RewindFilesTimeout)
    }
  }
}

/// Generate a unique request ID for control operations.
///
/// Uses unique_integer for guaranteed uniqueness within a session.
fn generate_request_id() -> String {
  let id = unique_integer()
  "req_" <> int.to_string(id)
}

@external(erlang, "claude_agent_sdk_ffi", "unique_integer")
fn unique_integer() -> Int

// =============================================================================
// Backpressure Helpers
// =============================================================================

/// Maximum number of operations that can be queued during initialization.
const max_queued_ops: Int = 16

/// Maximum number of pending SDK-initiated requests.
const max_pending_requests: Int = 64

/// Maximum number of pending hook callbacks.
const max_pending_hooks: Int = 32

/// Queue an operation during initialization with backpressure limit.
///
/// Returns Error(InitQueueOverflow) if the queue is at capacity (16).
/// Operations are prepended for O(1) insert; flush_queued_ops reverses to send in FIFO order.
pub fn queue_operation(
  state: SessionState,
  op: QueuedOperation,
) -> Result(SessionState, SessionError) {
  case list.length(state.queued_ops) >= max_queued_ops {
    True ->
      Error(InitQueueOverflow(
        "Too many control operations queued during initialization (max 16)",
      ))
    False -> Ok(SessionState(..state, queued_ops: [op, ..state.queued_ops]))
  }
}

/// Add a pending request with backpressure limit.
///
/// Returns Error(TooManyPendingRequests) if at capacity (64).
pub fn add_pending_request(
  state: SessionState,
  request_id: String,
  pending: PendingRequest,
) -> Result(SessionState, SessionError) {
  case dict.size(state.pending_requests) >= max_pending_requests {
    True ->
      Error(TooManyPendingRequests("Too many pending control requests (max 64)"))
    False ->
      Ok(
        SessionState(
          ..state,
          pending_requests: dict.insert(
            state.pending_requests,
            request_id,
            pending,
          ),
        ),
      )
  }
}

/// Add a pending hook with backpressure limit.
///
/// Returns (state, None) on success.
/// Returns (state, Some(immediate_response)) if at capacity (32).
/// When at capacity, the hook is NOT added and an immediate fail-open response
/// should be sent to CLI without spawning a handler task.
pub fn add_pending_hook(
  state: SessionState,
  _callback_id: String,
  hook: PendingHook,
) -> #(SessionState, Option(String)) {
  case dict.size(state.pending_hooks) >= max_pending_hooks {
    True -> {
      // Return immediate fail-open response using hook.request_id for correlation
      // Use proper JSON encoding to handle special characters in request_id
      let response =
        json.object([
          #("type", json.string("control_response")),
          #(
            "response",
            json.object([
              #("subtype", json.string("success")),
              #("request_id", json.string(hook.request_id)),
              #("response", json.object([#("continue", json.bool(True))])),
            ]),
          ),
        ])
        |> json.to_string
      #(state, Some(response))
    }
    False -> #(
      SessionState(
        ..state,
        // Key by request_id for lookup in handle_hook_done
        pending_hooks: dict.insert(state.pending_hooks, hook.request_id, hook),
      ),
      None,
    )
  }
}
