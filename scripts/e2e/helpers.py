"""
E2E Test Helpers - Logging, redaction, and utility functions.
"""
import json
import os
import re
import subprocess
import sys
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Literal, Optional

# Event levels
Level = Literal["DEBUG", "INFO", "WARN", "ERROR"]

# Event types
EventType = Literal[
    "run_start", "run_end",
    "scenario_start", "scenario_end",
    "step_start", "step_end",
    "command_start", "command_end",
    "skip", "warning", "error",
    "artifact_written",
]

# Result types
Result = Literal["pass", "fail", "skip"]

# Allowlisted environment variables for logging
ENV_ALLOWLIST = frozenset([
    "CLAUDE_INTEGRATION_TEST",
    "CLAUDE_INTEGRATION_ALLOW_NONJSON",
    "PATH",
    "HOME",
    "USER",
    "SHELL",
    "TERM",
    "LANG",
    "LC_ALL",
])

# Maximum line length before truncation
MAX_LINE_LENGTH = 4096

# Patterns for secret detection
SECRET_PATTERNS = [
    re.compile(r"sk-ant-[a-zA-Z0-9_-]{20,}"),  # Anthropic API keys
    re.compile(r"[a-zA-Z0-9_-]{32,}"),  # Generic long tokens (conservative)
    re.compile(r"Bearer\s+[a-zA-Z0-9_.-]+"),  # Bearer tokens
    re.compile(r"api[_-]?key[=:]\s*[a-zA-Z0-9_-]+", re.IGNORECASE),
]


def generate_run_id() -> str:
    """Generate a unique run ID with timestamp prefix."""
    ts = datetime.now(timezone.utc).strftime("%Y%m%d-%H%M%S")
    short_uuid = uuid.uuid4().hex[:8]
    return f"{ts}-{short_uuid}"


def redact_secrets(text: str) -> str:
    """Redact potential secrets from text."""
    result = text
    for pattern in SECRET_PATTERNS:
        result = pattern.sub("REDACTED", result)
    return result


def truncate_line(line: str, max_len: int = MAX_LINE_LENGTH) -> str:
    """Truncate a line to max length, adding indicator if truncated."""
    if len(line) <= max_len:
        return line
    return line[:max_len - 20] + "... [TRUNCATED]"


def redact_and_truncate(text: str) -> str:
    """Apply redaction and truncation to text."""
    lines = text.split("\n")
    processed = []
    for line in lines:
        redacted = redact_secrets(line)
        truncated = truncate_line(redacted)
        processed.append(truncated)
    return "\n".join(processed)


def get_redacted_env() -> dict[str, str]:
    """Get allowlisted environment variables."""
    return {k: os.environ.get(k, "") for k in ENV_ALLOWLIST if k in os.environ}


def get_system_info() -> dict[str, str]:
    """Get system metadata."""
    import platform
    return {
        "os": platform.system(),
        "arch": platform.machine(),
        "python_version": platform.python_version(),
    }


def get_gleam_version() -> Optional[str]:
    """Get Gleam version if available."""
    try:
        result = subprocess.run(
            ["gleam", "--version"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except (FileNotFoundError, subprocess.TimeoutExpired):
        pass
    return None


def get_cli_version() -> tuple[Optional[str], Optional[str]]:
    """Get Claude CLI version. Returns (raw_output, parsed_version)."""
    try:
        result = subprocess.run(
            ["claude", "--version"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0:
            raw = result.stdout.strip()
            # Parse version like "2.1.4 (Claude Code)"
            match = re.match(r"(\d+\.\d+\.\d+)", raw)
            parsed = match.group(1) if match else None
            return raw, parsed
    except (FileNotFoundError, subprocess.TimeoutExpired):
        pass
    return None, None


def get_repo_sha() -> Optional[str]:
    """Get current git commit SHA if in a repo."""
    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0:
            return result.stdout.strip()[:12]
    except (FileNotFoundError, subprocess.TimeoutExpired):
        pass
    return None


@dataclass
class EventLogger:
    """JSONL event logger with elapsed time tracking."""
    run_id: str
    output_dir: Path
    start_time: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    _events_file: Any = field(default=None, repr=False)
    _stdout_file: Any = field(default=None, repr=False)
    _stderr_file: Any = field(default=None, repr=False)

    def __post_init__(self):
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self._events_file = open(self.output_dir / "events.jsonl", "w")
        self._stdout_file = open(self.output_dir / "stdout.txt", "w")
        self._stderr_file = open(self.output_dir / "stderr.txt", "w")

    def close(self):
        """Close all file handles."""
        if self._events_file:
            self._events_file.close()
        if self._stdout_file:
            self._stdout_file.close()
        if self._stderr_file:
            self._stderr_file.close()

    def elapsed_ms(self) -> int:
        """Get elapsed milliseconds since run start."""
        delta = datetime.now(timezone.utc) - self.start_time
        return int(delta.total_seconds() * 1000)

    def log_event(
        self,
        event: EventType,
        level: Level,
        scenario_id: Optional[str] = None,
        step: Optional[str] = None,
        **kwargs,
    ):
        """Log a structured event to JSONL."""
        record = {
            "ts": datetime.now(timezone.utc).isoformat(),
            "level": level,
            "event": event,
            "run_id": self.run_id,
            "elapsed_ms": self.elapsed_ms(),
        }
        if scenario_id:
            record["scenario_id"] = scenario_id
        if step:
            record["step"] = step
        record.update(kwargs)

        line = json.dumps(record, separators=(",", ":"))
        self._events_file.write(line + "\n")
        self._events_file.flush()

    def log_stdout(self, text: str):
        """Log redacted stdout content."""
        processed = redact_and_truncate(text)
        self._stdout_file.write(processed)
        self._stdout_file.flush()

    def log_stderr(self, text: str):
        """Log redacted stderr content."""
        processed = redact_and_truncate(text)
        self._stderr_file.write(processed)
        self._stderr_file.flush()

    def write_metadata(self, metadata: dict):
        """Write metadata.json file."""
        with open(self.output_dir / "metadata.json", "w") as f:
            json.dump(metadata, f, indent=2)


@dataclass
class ScenarioResult:
    """Result of a single scenario execution."""
    scenario_id: str
    status: Result
    duration_ms: int
    error_kind: Optional[str] = None
    error_message: Optional[str] = None
    notes: Optional[str] = None


def write_summary(
    output_dir: Path,
    run_id: str,
    metadata: dict,
    results: list[ScenarioResult],
):
    """Write human-readable summary.txt."""
    lines = []
    lines.append("=" * 60)
    lines.append("E2E Test Run Summary")
    lines.append("=" * 60)
    lines.append("")
    lines.append(f"Run ID:       {run_id}")
    lines.append(f"Date:         {metadata.get('start_ts', 'unknown')}")
    lines.append(f"CLI Version:  {metadata.get('cli_version_raw', 'unknown')}")
    lines.append(f"OS:           {metadata.get('os', 'unknown')}")
    lines.append(f"Gleam:        {metadata.get('gleam_version', 'unknown')}")
    lines.append(f"Duration:     {metadata.get('duration_ms', 0)}ms")
    lines.append("")
    lines.append("-" * 60)
    lines.append("Scenario Results")
    lines.append("-" * 60)
    lines.append("")
    lines.append(f"{'ID':<10} {'Status':<8} {'Duration':<12} Notes")
    lines.append("-" * 60)

    for r in results:
        notes = r.notes or ""
        lines.append(f"{r.scenario_id:<10} {r.status:<8} {r.duration_ms:>8}ms   {notes}")

    lines.append("")

    # Failure section
    failures = [r for r in results if r.status == "fail"]
    if failures:
        lines.append("-" * 60)
        lines.append("Failures")
        lines.append("-" * 60)
        for r in failures:
            lines.append(f"\n{r.scenario_id}:")
            lines.append(f"  Error: {r.error_kind or 'unknown'}")
            lines.append(f"  Message: {r.error_message or 'no message'}")
        lines.append("")

    # Skip section
    skips = [r for r in results if r.status == "skip"]
    if skips:
        lines.append("-" * 60)
        lines.append("Skipped")
        lines.append("-" * 60)
        for r in skips:
            lines.append(f"\n{r.scenario_id}:")
            lines.append(f"  Reason: {r.notes or 'no reason provided'}")
        lines.append("")

    # Final summary
    passed = sum(1 for r in results if r.status == "pass")
    failed = sum(1 for r in results if r.status == "fail")
    skipped = sum(1 for r in results if r.status == "skip")
    lines.append("=" * 60)
    lines.append(f"Total: {len(results)} | Passed: {passed} | Failed: {failed} | Skipped: {skipped}")
    lines.append("=" * 60)

    with open(output_dir / "summary.txt", "w") as f:
        f.write("\n".join(lines))
