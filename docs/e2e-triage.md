# E2E Test Troubleshooting Guide

This document covers common E2E test failures and how to resolve them.

## Artifacts Location

E2E test runs produce artifacts in `artifacts/e2e/<run_id>/`:
- `events.jsonl` - Structured event log (machine-readable)
- `summary.txt` - Human-readable results summary
- `stdout.txt` - CLI stdout (redacted)
- `stderr.txt` - CLI stderr (redacted)
- `metadata.json` - Run metadata (versions, timing, etc.)

In CI, these are uploaded as the `e2e-artifacts` artifact and retained for 7 days.

## Common Failures

### CLI Not Found

**Symptom:** Error indicates `claude` command not found or similar.

**Events log pattern:**
```json
{"event": "skip", "error": {"kind": "cli_missing", ...}}
```

**Cause:** Claude CLI is not installed or not in PATH.

**Resolution:**
1. Install the CLI: `npm install -g @anthropic-ai/claude-code`
2. Verify installation: `claude --version`
3. Ensure the installation directory is in PATH

### Authentication Missing

**Symptom:** Tests skip with auth-related warnings.

**Events log pattern:**
```json
{"event": "skip", "error": {"kind": "auth_missing", ...}}
```

**Cause:** `ANTHROPIC_API_KEY` environment variable not set or invalid.

**Resolution:**
1. Set the API key: `export ANTHROPIC_API_KEY=sk-ant-...`
2. For CI: ensure the secret is configured in repository settings
3. Verify the key is valid by running a simple CLI command

### NDJSON Parse Errors (Impurity)

**Symptom:** Tests fail parsing CLI output with JSON decode errors.

**Events log pattern:**
```json
{"event": "error", "error": {"kind": "ndjson_impurity", "message": "...", "details": {"line_number": N}}}
```

**Cause:** CLI output contains non-JSON lines mixed with expected NDJSON.

**Resolution:**
1. Check `stdout.txt` for the offending line(s)
2. Ensure `CLAUDE_INTEGRATION_TEST=1` is set (enables machine output mode)
3. If the CLI prints debug output, report as a CLI bug
4. Temporary workaround: use `CLAUDE_INTEGRATION_ALLOW_NONJSON=1` to skip non-JSON lines

### Timeout Failures

**Symptom:** Tests fail with timeout errors.

**Events log pattern:**
```json
{"event": "command_end", "exit_code": null, "error": {"kind": "timeout", ...}}
```

**Cause:** CLI command took longer than the configured timeout.

**Resolution:**
1. Check network connectivity to Anthropic API
2. Review the prompt complexity - simpler prompts respond faster
3. Increase timeout if appropriate for the scenario
4. Check API status at https://status.anthropic.com

### Unexpected Exit Codes

**Symptom:** CLI exits with non-zero code unexpectedly.

**Events log pattern:**
```json
{"event": "command_end", "exit_code": 1, ...}
```

**Resolution:**
1. Check `stderr.txt` for error messages
2. Common causes: rate limiting, invalid input, network errors
3. Review the command in `events.jsonl` to reproduce locally

## Debugging Locally

To reproduce a CI failure locally:

1. Download the `e2e-artifacts` from the failed CI run
2. Review `summary.txt` for an overview
3. Search `events.jsonl` for failed scenarios:
   ```bash
   grep '"result":"fail"' events.jsonl
   ```
4. Extract the failed command and run manually with debug output

## Redaction Policy

Logs are redacted to prevent secret leakage:
- API keys and tokens are replaced with `REDACTED`
- Only allowlisted environment variables are logged
- Long output lines are truncated to 4KB

If you need unredacted logs, run tests locally with appropriate safeguards.

## CI Artifact Retention

- Artifacts are retained for **7 days**
- Download artifacts promptly for failed runs
- For historical analysis, save artifacts externally before expiration
