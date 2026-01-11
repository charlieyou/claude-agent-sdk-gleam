// Spec Verification Results - Phase 1
// Date: 2026-01-11
// Spec version: plans/CLAUDE_AGENT_SDK_SPEC.md (commit bfb17717657136334390902e97a9f20c669f913f)
//
// Field: type (all messages)
// Heading: SPEC: 4. Message Protocol
// Verified: YES - "All messages are JSON objects with a `type` field"
//
// Field: session_id (SystemMessage)
// Heading: SPEC: 4.2 System Message -> System Message Fields
// Verified: NO - No MUST/REQUIRED language found, field listed in table only
// Decision: Treat as Optional per procedure (SHOULD language only)
//
// Field: subtype (SystemMessage)
// Heading: SPEC: 4.2 System Message -> System Message Fields
// Verified: NO - Field documented but no MUST/REQUIRED language
// Decision: Treat as Optional per procedure
//
// Field: uuid (SystemMessage)
// Heading: SPEC: 4.2 System Message -> System Message Fields
// Verified: NO - Field documented but no MUST/REQUIRED language
// Decision: Treat as Optional per procedure
