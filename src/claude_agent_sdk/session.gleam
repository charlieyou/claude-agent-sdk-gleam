/// Session type for bidirectional mode.
///
/// This module defines the Session opaque type that represents a handle to
/// a bidirectional CLI session. The Session wraps an OTP Subject for
/// communicating with the underlying GenServer actor.
import gleam/erlang/process.{type Subject}

import claude_agent_sdk/event.{type SessionEvent}
import claude_agent_sdk/internal/bidir/actor.{
  type ActorMessage, type SubscriberMessage,
}
import claude_agent_sdk/message.{type Message}

/// Opaque session handle for bidirectional mode.
///
/// Wraps a Subject for communicating with the session GenServer.
/// The session actor manages the CLI process, message routing,
/// and hook callbacks.
///
/// Contains:
/// - actor: Subject for sending commands to the session GenServer
/// - messages: Subject for receiving Message pushes
/// - events: Subject for receiving SessionEvent lifecycle events
/// - subscriber: Subject for receiving SubscriberMessage notifications
pub opaque type Session {
  Session(
    actor: Subject(ActorMessage),
    messages: Subject(Message),
    events: Subject(SessionEvent),
    subscriber: Subject(SubscriberMessage),
  )
}

/// Get the underlying actor subject from a session (internal use only).
pub fn get_actor(session: Session) -> Subject(ActorMessage) {
  session.actor
}

/// Get the message subscription Subject from a session.
///
/// The returned Subject receives Message values pushed by the session GenServer.
/// Use `process.receive` or `process.selecting` to consume messages.
pub fn get_messages(session: Session) -> Subject(Message) {
  session.messages
}

/// Get the events subscription Subject from a session.
///
/// The returned Subject receives SessionEvent values pushed by the session GenServer.
/// Use `process.receive` or `process.selecting` to consume lifecycle events.
pub fn get_events(session: Session) -> Subject(SessionEvent) {
  session.events
}

/// Create a new Session with the given subjects (internal use only).
///
/// Used by the session GenServer to construct a Session handle.
pub fn new(
  actor: Subject(ActorMessage),
  messages: Subject(Message),
  events: Subject(SessionEvent),
  subscriber: Subject(SubscriberMessage),
) -> Session {
  Session(
    actor: actor,
    messages: messages,
    events: events,
    subscriber: subscriber,
  )
}

/// Get the subscriber Subject from a session (internal use only).
///
/// The returned Subject receives SubscriberMessage values from the actor.
pub fn get_subscriber(session: Session) -> Subject(SubscriberMessage) {
  session.subscriber
}
