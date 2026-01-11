// Spec Verification Results - Phase 1
// Date: 2026-01-11
// Spec version: plans/CLAUDE_AGENT_SDK_SPEC.md (commit e28b49020340c1d2251535687fd48e65b4e9712d)
//
// Field: type (all messages)
// Heading: SPEC: 4. Message Protocol
// Verified: NO - Spec states "All messages are JSON objects with a `type` field"
//                but no MUST/REQUIRED keywords present (spec uses no RFC 2119 keywords).
// Decision: Treat as Required - semantically required for message dispatch despite
//           lack of explicit MUST/REQUIRED language (would break all deserialization).
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
//
// Field: message.content (AssistantMessage)
// Heading: SPEC: 4.3 Assistant Message
// Verified: NO - Field shown in JSON example (line 241) but no MUST/REQUIRED language
// Decision: Treat as Optional per procedure
//
// Field: message.content (UserMessage)
// Heading: SPEC: 4.4 User Message
// Verified: NO - Field shown in JSON example (line 298) but no MUST/REQUIRED language
// Decision: Treat as Optional per procedure
//
// Field: subtype (ResultMessage)
// Heading: SPEC: 4.5 Result Message -> Result Subtypes
// Verified: NO - Field shown in JSON example (line 322) and table (lines 359-364)
//                but no MUST/REQUIRED language
// Decision: Treat as Optional per procedure
