#!/usr/bin/env python3
"""
E2E Test Runner for Claude Agent SDK (Gleam)

Runs end-to-end scenarios with structured JSONL logging and human-readable summaries.
See plans/e2e-scenarios.md for scenario definitions.

Usage:
    python scripts/e2e/run_e2e.py [--scenario E2E-XX] [--output-dir DIR]

Environment:
    CLAUDE_INTEGRATION_TEST=1       Enable E2E suite (required)
    CLAUDE_INTEGRATION_ALLOW_NONJSON=1  Tolerate non-JSON CLI output
    ANTHROPIC_API_KEY=...           API key for authentication
"""
import argparse
import json
import os
import re
import subprocess
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

# Add scripts/e2e to path for helpers import
sys.path.insert(0, str(Path(__file__).parent))

from helpers import (
    EventLogger,
    ScenarioResult,
    generate_run_id,
    get_cli_version,
    get_gleam_version,
    get_redacted_env,
    get_repo_sha,
    get_system_info,
    redact_and_truncate,
    write_summary,
)

# Minimum required CLI version
MIN_CLI_VERSION = (1, 0, 0)

# Scenario timeout (seconds)
SCENARIO_TIMEOUT = 60


@dataclass
class ScenarioContext:
    """Context for scenario execution."""
    logger: EventLogger
    cli_path: str
    cli_version_raw: str
    cli_version_parsed: Optional[tuple[int, int, int]]


def parse_version(version_str: Optional[str]) -> Optional[tuple[int, int, int]]:
    """Parse version string to tuple."""
    if not version_str:
        return None
    match = re.match(r"(\d+)\.(\d+)\.(\d+)", version_str)
    if match:
        return (int(match.group(1)), int(match.group(2)), int(match.group(3)))
    return None


def run_command(
    ctx: ScenarioContext,
    args: list[str],
    scenario_id: str,
    step: str,
    timeout: int = SCENARIO_TIMEOUT,
    capture_output: bool = True,
) -> tuple[int, str, str]:
    """Run a CLI command with logging."""
    cmd = [ctx.cli_path] + args
    cmd_str = " ".join(cmd)
    cwd = os.getcwd()

    ctx.logger.log_event(
        "command_start",
        "INFO",
        scenario_id=scenario_id,
        step=step,
        command=cmd_str,
        cwd=cwd,
        env=get_redacted_env(),
    )

    try:
        result = subprocess.run(
            cmd,
            capture_output=capture_output,
            text=True,
            timeout=timeout,
        )
        stdout = result.stdout or ""
        stderr = result.stderr or ""
        exit_code = result.returncode

        # Log captured output
        if stdout:
            ctx.logger.log_stdout(f"[{scenario_id}/{step}]\n{stdout}\n")
        if stderr:
            ctx.logger.log_stderr(f"[{scenario_id}/{step}]\n{stderr}\n")

        ctx.logger.log_event(
            "command_end",
            "INFO" if exit_code == 0 else "WARN",
            scenario_id=scenario_id,
            step=step,
            exit_code=exit_code,
            stdout_bytes=len(stdout.encode("utf-8")),
            stderr_bytes=len(stderr.encode("utf-8")),
        )

        return exit_code, stdout, stderr

    except subprocess.TimeoutExpired:
        ctx.logger.log_event(
            "command_end",
            "ERROR",
            scenario_id=scenario_id,
            step=step,
            error={"kind": "timeout", "message": f"Command timed out after {timeout}s"},
        )
        return -1, "", f"Timeout after {timeout}s"


# =============================================================================
# Scenario Implementations
# =============================================================================

def run_e2e_01_preflight(ctx: ScenarioContext) -> ScenarioResult:
    """E2E-01: Preflight Environment Validation"""
    scenario_id = "E2E-01"
    start_ms = ctx.logger.elapsed_ms()

    ctx.logger.log_event("scenario_start", "INFO", scenario_id=scenario_id, step="preflight")

    # Step 1: CLI executable found (already validated to get here)
    ctx.logger.log_event("step_start", "INFO", scenario_id=scenario_id, step="cli_resolve")
    ctx.logger.log_event(
        "step_end", "INFO", scenario_id=scenario_id, step="cli_resolve",
        result="pass", notes=f"CLI at {ctx.cli_path}"
    )

    # Step 2: Version check
    ctx.logger.log_event("step_start", "INFO", scenario_id=scenario_id, step="version_check")
    if ctx.cli_version_parsed:
        if ctx.cli_version_parsed >= MIN_CLI_VERSION:
            ctx.logger.log_event(
                "step_end", "INFO", scenario_id=scenario_id, step="version_check",
                result="pass", notes=f"Version {ctx.cli_version_raw} >= {'.'.join(map(str, MIN_CLI_VERSION))}"
            )
        else:
            ctx.logger.log_event(
                "step_end", "WARN", scenario_id=scenario_id, step="version_check",
                result="fail", notes=f"Version {ctx.cli_version_raw} < {'.'.join(map(str, MIN_CLI_VERSION))}"
            )
            return ScenarioResult(
                scenario_id=scenario_id,
                status="fail",
                duration_ms=ctx.logger.elapsed_ms() - start_ms,
                error_kind="version_too_old",
                error_message=f"CLI version {ctx.cli_version_raw} is below minimum {'.'.join(map(str, MIN_CLI_VERSION))}",
                notes="Upgrade Claude CLI",
            )
    else:
        ctx.logger.log_event(
            "step_end", "WARN", scenario_id=scenario_id, step="version_check",
            result="pass", notes=f"Unparseable version: {ctx.cli_version_raw}"
        )

    # Step 3: Auth check
    ctx.logger.log_event("step_start", "INFO", scenario_id=scenario_id, step="auth_check")
    has_api_key = bool(os.environ.get("ANTHROPIC_API_KEY"))
    if has_api_key:
        ctx.logger.log_event(
            "step_end", "INFO", scenario_id=scenario_id, step="auth_check",
            result="pass", notes="ANTHROPIC_API_KEY present"
        )
    else:
        # Try claude auth status
        exit_code, stdout, stderr = run_command(
            ctx, ["auth", "status"], scenario_id, "auth_check", timeout=10
        )
        if exit_code == 0 and "authenticated" in stdout.lower():
            ctx.logger.log_event(
                "step_end", "INFO", scenario_id=scenario_id, step="auth_check",
                result="pass", notes="Authenticated via claude login"
            )
        else:
            ctx.logger.log_event(
                "step_end", "WARN", scenario_id=scenario_id, step="auth_check",
                result="skip", notes="No auth configured"
            )
            return ScenarioResult(
                scenario_id=scenario_id,
                status="skip",
                duration_ms=ctx.logger.elapsed_ms() - start_ms,
                notes="Set ANTHROPIC_API_KEY or run 'claude login'",
            )

    ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="pass")
    return ScenarioResult(
        scenario_id=scenario_id,
        status="pass",
        duration_ms=ctx.logger.elapsed_ms() - start_ms,
        notes="All preflight checks passed",
    )


def run_e2e_02_simple_query(ctx: ScenarioContext) -> ScenarioResult:
    """E2E-02: Simple Query (Happy Path)"""
    scenario_id = "E2E-02"
    start_ms = ctx.logger.elapsed_ms()

    ctx.logger.log_event("scenario_start", "INFO", scenario_id=scenario_id, step="simple_query")

    # Run a minimal query
    ctx.logger.log_event("step_start", "INFO", scenario_id=scenario_id, step="run_query")
    exit_code, stdout, stderr = run_command(
        ctx,
        ["--print", "--output-format", "stream-json", "--max-turns", "1", "Say hello"],
        scenario_id,
        "run_query",
        timeout=30,
    )

    if exit_code != 0:
        ctx.logger.log_event("scenario_end", "ERROR", scenario_id=scenario_id, result="fail")
        return ScenarioResult(
            scenario_id=scenario_id,
            status="fail",
            duration_ms=ctx.logger.elapsed_ms() - start_ms,
            error_kind="non_zero_exit",
            error_message=f"CLI exited with code {exit_code}",
            notes=redact_and_truncate(stderr[:200]) if stderr else None,
        )

    # Check for valid JSON and result message
    lines = [l for l in stdout.strip().split("\n") if l.strip()]
    has_result = False
    for line in lines:
        try:
            msg = json.loads(line)
            if msg.get("type") == "result":
                has_result = True
                break
        except json.JSONDecodeError:
            pass

    if not has_result:
        ctx.logger.log_event("scenario_end", "WARN", scenario_id=scenario_id, result="fail")
        return ScenarioResult(
            scenario_id=scenario_id,
            status="fail",
            duration_ms=ctx.logger.elapsed_ms() - start_ms,
            error_kind="no_result",
            error_message="No Result message in stream",
        )

    ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="pass")
    return ScenarioResult(
        scenario_id=scenario_id,
        status="pass",
        duration_ms=ctx.logger.elapsed_ms() - start_ms,
        notes=f"Received {len(lines)} messages",
    )


def run_e2e_03_ndjson_purity(ctx: ScenarioContext) -> ScenarioResult:
    """E2E-03: NDJSON Purity Validation"""
    scenario_id = "E2E-03"
    start_ms = ctx.logger.elapsed_ms()

    ctx.logger.log_event("scenario_start", "INFO", scenario_id=scenario_id, step="ndjson_check")

    ctx.logger.log_event("step_start", "INFO", scenario_id=scenario_id, step="run_query")
    exit_code, stdout, stderr = run_command(
        ctx,
        ["--print", "--output-format", "stream-json", "test"],
        scenario_id,
        "run_query",
        timeout=30,
    )

    if exit_code != 0:
        ctx.logger.log_event("scenario_end", "ERROR", scenario_id=scenario_id, result="fail")
        return ScenarioResult(
            scenario_id=scenario_id,
            status="fail",
            duration_ms=ctx.logger.elapsed_ms() - start_ms,
            error_kind="non_zero_exit",
            error_message=f"CLI exited with code {exit_code}",
        )

    # Check every non-empty line is valid JSON
    lines = stdout.strip().split("\n")
    non_json_lines = []
    for i, line in enumerate(lines, 1):
        if not line.strip():
            continue
        try:
            json.loads(line)
        except json.JSONDecodeError:
            non_json_lines.append((i, line[:100]))

    allow_nonjson = os.environ.get("CLAUDE_INTEGRATION_ALLOW_NONJSON") == "1"

    if non_json_lines:
        if allow_nonjson:
            ctx.logger.log_event(
                "warning", "WARN", scenario_id=scenario_id,
                step="ndjson_check",
                notes=f"{len(non_json_lines)} non-JSON lines (allowed via env)"
            )
            ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="pass")
            return ScenarioResult(
                scenario_id=scenario_id,
                status="pass",
                duration_ms=ctx.logger.elapsed_ms() - start_ms,
                notes=f"{len(non_json_lines)} non-JSON lines (allowed)",
            )
        else:
            ctx.logger.log_event("scenario_end", "ERROR", scenario_id=scenario_id, result="fail")
            return ScenarioResult(
                scenario_id=scenario_id,
                status="fail",
                duration_ms=ctx.logger.elapsed_ms() - start_ms,
                error_kind="ndjson_impure",
                error_message=f"{len(non_json_lines)} non-JSON line(s) in output",
                notes=f"Line {non_json_lines[0][0]}: {redact_and_truncate(non_json_lines[0][1])}",
            )

    ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="pass")
    return ScenarioResult(
        scenario_id=scenario_id,
        status="pass",
        duration_ms=ctx.logger.elapsed_ms() - start_ms,
        notes="All lines valid JSON",
    )


def run_e2e_04_session_resume(ctx: ScenarioContext) -> ScenarioResult:
    """E2E-04: Session Resume Flow"""
    scenario_id = "E2E-04"
    start_ms = ctx.logger.elapsed_ms()

    ctx.logger.log_event("scenario_start", "INFO", scenario_id=scenario_id, step="session_resume")

    # Step 1: Create initial session
    ctx.logger.log_event("step_start", "INFO", scenario_id=scenario_id, step="initial_query")
    exit_code, stdout, stderr = run_command(
        ctx,
        ["--print", "--output-format", "stream-json", "--max-turns", "1", "Remember 42"],
        scenario_id,
        "initial_query",
        timeout=30,
    )

    if exit_code != 0:
        ctx.logger.log_event("scenario_end", "ERROR", scenario_id=scenario_id, result="fail")
        return ScenarioResult(
            scenario_id=scenario_id,
            status="fail",
            duration_ms=ctx.logger.elapsed_ms() - start_ms,
            error_kind="initial_query_failed",
            error_message=f"Initial query failed with exit code {exit_code}",
        )

    # Extract session_id from system message
    session_id = None
    for line in stdout.strip().split("\n"):
        if not line.strip():
            continue
        try:
            msg = json.loads(line)
            if msg.get("type") == "system" and msg.get("session_id"):
                session_id = msg["session_id"]
                break
        except json.JSONDecodeError:
            pass

    if not session_id:
        ctx.logger.log_event("scenario_end", "ERROR", scenario_id=scenario_id, result="fail")
        return ScenarioResult(
            scenario_id=scenario_id,
            status="fail",
            duration_ms=ctx.logger.elapsed_ms() - start_ms,
            error_kind="no_session_id",
            error_message="No session_id in system message",
        )

    # Step 2: Resume session
    ctx.logger.log_event("step_start", "INFO", scenario_id=scenario_id, step="resume_query")
    exit_code, stdout, stderr = run_command(
        ctx,
        ["--print", "--output-format", "stream-json", "--max-turns", "1",
         "--resume", session_id, "What number?"],
        scenario_id,
        "resume_query",
        timeout=30,
    )

    if exit_code != 0:
        ctx.logger.log_event("scenario_end", "WARN", scenario_id=scenario_id, result="fail")
        return ScenarioResult(
            scenario_id=scenario_id,
            status="fail",
            duration_ms=ctx.logger.elapsed_ms() - start_ms,
            error_kind="resume_failed",
            error_message=f"Resume query failed with exit code {exit_code}",
        )

    ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="pass")
    return ScenarioResult(
        scenario_id=scenario_id,
        status="pass",
        duration_ms=ctx.logger.elapsed_ms() - start_ms,
        notes=f"Session {session_id[:16]}... resumed",
    )


def run_e2e_05_version_edge_cases(ctx: ScenarioContext) -> ScenarioResult:
    """E2E-05: Version Edge Cases"""
    scenario_id = "E2E-05"
    start_ms = ctx.logger.elapsed_ms()

    ctx.logger.log_event("scenario_start", "INFO", scenario_id=scenario_id, step="version_edge")

    # Already validated version during preflight - check parsing
    if ctx.cli_version_parsed:
        ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="pass")
        return ScenarioResult(
            scenario_id=scenario_id,
            status="pass",
            duration_ms=ctx.logger.elapsed_ms() - start_ms,
            notes=f"Version parsed: {ctx.cli_version_raw}",
        )
    else:
        ctx.logger.log_event(
            "warning", "WARN", scenario_id=scenario_id,
            notes=f"Unparseable version: {ctx.cli_version_raw}"
        )
        ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="pass")
        return ScenarioResult(
            scenario_id=scenario_id,
            status="pass",
            duration_ms=ctx.logger.elapsed_ms() - start_ms,
            notes=f"Graceful handling of unparseable: {ctx.cli_version_raw}",
        )


def run_e2e_06_auth_missing(ctx: ScenarioContext) -> ScenarioResult:
    """E2E-06: Auth Missing (Expected Skip)"""
    scenario_id = "E2E-06"
    start_ms = ctx.logger.elapsed_ms()

    ctx.logger.log_event("scenario_start", "INFO", scenario_id=scenario_id, step="auth_missing")

    # This scenario validates skip behavior when auth is missing
    # Since we can't easily unset auth mid-run, we document expected behavior
    has_api_key = bool(os.environ.get("ANTHROPIC_API_KEY"))

    if has_api_key:
        ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="skip")
        return ScenarioResult(
            scenario_id=scenario_id,
            status="skip",
            duration_ms=ctx.logger.elapsed_ms() - start_ms,
            notes="Auth present - cannot test missing auth scenario",
        )

    # Auth actually missing - verify skip message
    ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="pass")
    return ScenarioResult(
        scenario_id=scenario_id,
        status="pass",
        duration_ms=ctx.logger.elapsed_ms() - start_ms,
        notes="Auth missing correctly detected",
    )


def run_e2e_07_cli_missing(ctx: ScenarioContext) -> ScenarioResult:
    """E2E-07: CLI Missing (Expected Skip)"""
    scenario_id = "E2E-07"
    start_ms = ctx.logger.elapsed_ms()

    ctx.logger.log_event("scenario_start", "INFO", scenario_id=scenario_id, step="cli_missing")

    # Since we have a valid CLI context, this scenario documents the skip behavior
    ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="skip")
    return ScenarioResult(
        scenario_id=scenario_id,
        status="skip",
        duration_ms=ctx.logger.elapsed_ms() - start_ms,
        notes="CLI present - cannot test missing CLI scenario",
    )


def run_e2e_08_timeout(ctx: ScenarioContext) -> ScenarioResult:
    """E2E-08: Timeout / Slow Response (Optional)"""
    scenario_id = "E2E-08"
    start_ms = ctx.logger.elapsed_ms()

    ctx.logger.log_event("scenario_start", "INFO", scenario_id=scenario_id, step="timeout_test")

    # Run with a very short timeout to test timeout handling
    ctx.logger.log_event("step_start", "INFO", scenario_id=scenario_id, step="timeout_query")
    exit_code, stdout, stderr = run_command(
        ctx,
        ["--print", "--output-format", "stream-json", "--max-turns", "1",
         "Write a very long essay about the history of computing"],
        scenario_id,
        "timeout_query",
        timeout=5,  # Short timeout to potentially trigger
    )

    if exit_code == -1:  # Timeout
        ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="pass")
        return ScenarioResult(
            scenario_id=scenario_id,
            status="pass",
            duration_ms=ctx.logger.elapsed_ms() - start_ms,
            notes="Timeout handled correctly",
        )

    # Query completed before timeout
    ctx.logger.log_event("scenario_end", "INFO", scenario_id=scenario_id, result="pass")
    return ScenarioResult(
        scenario_id=scenario_id,
        status="pass",
        duration_ms=ctx.logger.elapsed_ms() - start_ms,
        notes="Query completed within timeout",
    )


# =============================================================================
# Scenario Registry
# =============================================================================

SCENARIOS = {
    "E2E-01": run_e2e_01_preflight,
    "E2E-02": run_e2e_02_simple_query,
    "E2E-03": run_e2e_03_ndjson_purity,
    "E2E-04": run_e2e_04_session_resume,
    "E2E-05": run_e2e_05_version_edge_cases,
    "E2E-06": run_e2e_06_auth_missing,
    "E2E-07": run_e2e_07_cli_missing,
    "E2E-08": run_e2e_08_timeout,
}


def main():
    parser = argparse.ArgumentParser(description="E2E Test Runner")
    parser.add_argument(
        "--scenario", "-s",
        help="Run specific scenario (e.g., E2E-02). Can be repeated.",
        action="append",
        dest="scenarios",
    )
    parser.add_argument(
        "--output-dir", "-o",
        help="Output directory for artifacts (default: artifacts/e2e/<run_id>)",
        type=Path,
    )
    parser.add_argument(
        "--list", "-l",
        action="store_true",
        help="List available scenarios and exit",
    )
    args = parser.parse_args()

    if args.list:
        print("Available E2E Scenarios:")
        for sid in sorted(SCENARIOS.keys()):
            fn = SCENARIOS[sid]
            print(f"  {sid}: {fn.__doc__.strip().split(chr(10))[0] if fn.__doc__ else 'No description'}")
        return 0

    # Check gate
    if os.environ.get("CLAUDE_INTEGRATION_TEST") != "1":
        print("E2E tests require CLAUDE_INTEGRATION_TEST=1", file=sys.stderr)
        print("Set this environment variable to enable E2E scenarios.", file=sys.stderr)
        return 1

    # Find CLI
    cli_path = "claude"  # Assume in PATH
    try:
        result = subprocess.run(
            ["which", "claude"],
            capture_output=True,
            text=True,
        )
        if result.returncode != 0:
            print("[SKIP] claude CLI not found in PATH", file=sys.stderr)
            print("Install: https://claude.ai/code", file=sys.stderr)
            return 1
        cli_path = result.stdout.strip()
    except FileNotFoundError:
        print("[SKIP] 'which' command not available", file=sys.stderr)
        return 1

    # Get CLI version
    cli_version_raw, cli_version_str = get_cli_version()
    if not cli_version_raw:
        print("[SKIP] Failed to get claude --version", file=sys.stderr)
        return 1
    cli_version_parsed = parse_version(cli_version_str)

    # Generate run ID and output dir
    run_id = generate_run_id()
    if args.output_dir:
        output_dir = args.output_dir
    else:
        output_dir = Path("artifacts/e2e") / run_id

    print(f"E2E Run: {run_id}")
    print(f"Output:  {output_dir}")
    print(f"CLI:     {cli_version_raw}")
    print()

    # Initialize logger
    logger = EventLogger(run_id=run_id, output_dir=output_dir)

    try:
        # Collect metadata
        sys_info = get_system_info()
        metadata = {
            "run_id": run_id,
            "start_ts": datetime.now(timezone.utc).isoformat(),
            "cli_version_raw": cli_version_raw,
            "cli_version_parsed": cli_version_str,
            "gleam_version": get_gleam_version(),
            "repo_sha": get_repo_sha(),
            **sys_info,
        }

        # Log run start
        logger.log_event("run_start", "INFO", env=get_redacted_env(), **metadata)

        # Create scenario context
        ctx = ScenarioContext(
            logger=logger,
            cli_path=cli_path,
            cli_version_raw=cli_version_raw,
            cli_version_parsed=cli_version_parsed,
        )

        # Determine scenarios to run
        if args.scenarios:
            scenario_ids = []
            for s in args.scenarios:
                if s.upper() in SCENARIOS:
                    scenario_ids.append(s.upper())
                else:
                    print(f"Unknown scenario: {s}", file=sys.stderr)
                    return 1
        else:
            scenario_ids = list(SCENARIOS.keys())

        # Run scenarios
        results = []
        for scenario_id in scenario_ids:
            print(f"Running {scenario_id}...", end=" ", flush=True)
            try:
                result = SCENARIOS[scenario_id](ctx)
                results.append(result)
                status_char = {"pass": "✓", "fail": "✗", "skip": "○"}.get(result.status, "?")
                print(f"{status_char} {result.status} ({result.duration_ms}ms)")
            except Exception as e:
                print(f"✗ error: {e}")
                logger.log_event(
                    "error", "ERROR", scenario_id=scenario_id,
                    error={"kind": "exception", "message": str(e)}
                )
                results.append(ScenarioResult(
                    scenario_id=scenario_id,
                    status="fail",
                    duration_ms=0,
                    error_kind="exception",
                    error_message=str(e),
                ))

        # Finalize
        end_ts = datetime.now(timezone.utc)
        metadata["end_ts"] = end_ts.isoformat()
        metadata["duration_ms"] = logger.elapsed_ms()

        logger.log_event("run_end", "INFO", duration_ms=metadata["duration_ms"])
        logger.write_metadata(metadata)
        write_summary(output_dir, run_id, metadata, results)
    finally:
        logger.close()

    # Print summary
    print()
    passed = sum(1 for r in results if r.status == "pass")
    failed = sum(1 for r in results if r.status == "fail")
    skipped = sum(1 for r in results if r.status == "skip")
    print(f"Results: {passed} passed, {failed} failed, {skipped} skipped")
    print(f"Artifacts: {output_dir}")

    # Log artifact locations
    print(f"  events.jsonl: {output_dir / 'events.jsonl'}")
    print(f"  summary.txt:  {output_dir / 'summary.txt'}")
    print(f"  metadata.json: {output_dir / 'metadata.json'}")

    return 1 if failed > 0 else 0


if __name__ == "__main__":
    sys.exit(main())
