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
import gleam/erlang/atom
import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor

import claude_agent_sdk/control.{
  type IncomingControlRequest, type IncomingControlResponse,
  type IncomingMessage, type OutgoingControlResponse, CanUseTool, ControlRequest,
  ControlResponse, Error as ControlError, HookCallback, HookResponse,
  HookSuccess, McpMessage, RegularMessage, Success,
}
import claude_agent_sdk/hook.{type HookEvent}
import claude_agent_sdk/internal/bidir_runner.{type BidirRunner}
import claude_agent_sdk/internal/control_decoder
import claude_agent_sdk/internal/control_encoder
import claude_agent_sdk/message

// FFI: Convert any value to Dynamic (identity function at runtime)
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: a) -> Dynamic

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
    /// Request ID of the init request (for correlation).
    init_request_id: Option(String),
    /// Subject for receiving injected messages (for testing).
    inject_subject: Option(Subject(String)),
    /// Timeout for initialization handshake (ms).
    init_timeout_ms: Int,
    /// Timer reference for init timeout (for cancellation on cleanup).
    init_timer_ref: Option(Dynamic),
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
  )
}

/// Create a default start configuration.
pub fn default_config(subscriber: Subject(SubscriberMessage)) -> StartConfig {
  StartConfig(
    subscriber: subscriber,
    default_timeout_ms: 60_000,
    hook_timeouts: dict.new(),
    init_timeout_ms: 10_000,
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
          init_request_id: Some(init_request_id),
          inject_subject: None,
          init_timeout_ms: config.init_timeout_ms,
          init_timer_ref: None,
        )

      // Perform the initialization handshake
      let state_after_init = perform_init_handshake(initial_state)

      // Build a selector that handles:
      // 1. Messages on the actor's subject (via process.selecting)
      // 2. Init timeout message {init_timeout, nil} (via select_record)
      let init_timeout_tag = atom.create("init_timeout")
      let selector =
        process.new_selector()
        |> process.select_map(self_subject, fn(msg) { msg })
        |> process.select_record(init_timeout_tag, 1, fn(_dyn) {
          InitTimeoutFired
        })

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
      enable_file_checkpointing: False,
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

/// Cancel a timer by its reference.
///
/// Returns True if the timer was cancelled, False if it had already fired.
@external(erlang, "bidir_ffi", "cancel_timer")
fn cancel_timer(timer_ref: Dynamic) -> Bool

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
  case dict.get(state.hooks.handlers, callback_id) {
    Ok(handler) -> {
      // Invoke the handler and wrap result in HookSuccess
      let result = handler(input)
      let response = HookResponse(request_id, HookSuccess(result))

      // Send response to CLI
      send_control_response(state, response)
      actor.continue(state)
    }
    Error(Nil) -> {
      // Unknown callback_id - log warning and continue (fail-open)
      // TODO: Add proper logging
      actor.continue(state)
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
  // TODO: Actually process hook_callback, can_use_tool, mcp_message
  // For now, just continue - request processing will be added in future task
  let _ = request
  actor.continue(flushed_state)
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

/// Flush queued operations after successful init.
///
/// Sends all queued operations to CLI without blocking.
/// Each operation gets its own pending request entry.
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

                // Register in pending_requests for response correlation
                // Note: sent_at=0 is acceptable since timeout logic is not yet implemented
                let pending_req =
                  PendingRequest(
                    request_id: request_id,
                    reply_to: reply_to,
                    sent_at: 0,
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

  // 2. Resolve all pending SDK-initiated requests with session stopped error
  let _ =
    dict.each(state.pending_requests, fn(_request_id, pending) {
      process.send(pending.reply_to, RequestSessionStopped)
    })

  // 3. Resolve all queued operations with session stopped error
  list.each(state.queued_ops, fn(op) {
    case op {
      QueuedRequest(_request_id, _payload, reply_to) -> {
        process.send(reply_to, RequestSessionStopped)
      }
    }
  })

  // 4. Close the runner (terminates CLI process)
  let close_fn = state.runner.close
  close_fn()

  // 5. Notify subscriber that session has ended
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
  callback_id: String,
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
        pending_hooks: dict.insert(state.pending_hooks, callback_id, hook),
      ),
      None,
    )
  }
}
