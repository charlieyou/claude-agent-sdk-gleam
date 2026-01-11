#!/bin/bash
# Coverage wrapper script for claude-agent-sdk-gleam
#
# Usage:
#   ./scripts/coverage.sh          # Run coverage with text output
#   ./scripts/coverage.sh --json   # Run coverage with JSON output
#   ./scripts/coverage.sh --threshold 70.0  # Fail if below 70%
#
# Prerequisites:
#   gleam build must be run first to compile beam files

set -e

cd "$(dirname "$0")/.."

# Ensure project is built
if [ ! -d "build/dev/erlang/claude_agent_sdk/ebin" ]; then
    echo "Building project first..."
    gleam build --target=erlang
fi

# Run the escript coverage tool
exec escript scripts/coverage.escript "$@"
