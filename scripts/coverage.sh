#!/usr/bin/env bash
# Coverage reporter for claude-agent-sdk-gleam
# Uses Erlang's cover module to measure line coverage
#
# Usage:
#   ./scripts/coverage.sh                    # Text report
#   ./scripts/coverage.sh --json             # JSON output
#   ./scripts/coverage.sh --threshold 95.0   # Enforce minimum coverage
#   ./scripts/coverage.sh --eligible-only    # Only check eligible modules (default for threshold)
#
# Exit codes:
#   0 - Success
#   1 - Coverage below threshold
#   2 - Build/runtime error

set -euo pipefail

# Configuration
BUILD_DIR="build/dev/erlang/claude_agent_sdk_gleam"
EBIN_DIR="${BUILD_DIR}/ebin"

# Default options
JSON_OUTPUT=false
THRESHOLD=""
ELIGIBLE_ONLY=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --json)
            JSON_OUTPUT=true
            shift
            ;;
        --threshold)
            THRESHOLD="$2"
            ELIGIBLE_ONLY=true  # Threshold implies eligible-only
            shift 2
            ;;
        --eligible-only)
            ELIGIBLE_ONLY=true
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [--json] [--threshold PERCENT] [--eligible-only]"
            echo ""
            echo "Options:"
            echo "  --json          Output JSON instead of text"
            echo "  --threshold N   Exit 1 if coverage < N% (implies --eligible-only)"
            echo "  --eligible-only Only report on pure modules (excludes port_ffi, cli OS calls)"
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 2
            ;;
    esac
done

# Modules excluded from coverage thresholds (port/CLI boundary code)
# These are covered by integration/E2E tests, not unit tests
EXCLUDED_MODULES=(
    "claude_agent_sdk@internal@port_ffi"     # Port FFI - OS boundary
    "claude_agent_sdk_ffi"                    # Erlang FFI
    "bidir_ffi"                               # Bidirectional FFI
)

# Functions with OS dependencies (documented but not filtered at module level)
# - cli.detect_cli_version - spawns CLI process
# - cli.find_cli_path - PATH lookup

is_excluded_module() {
    local module="$1"
    local exc_mod
    for exc_mod in "${EXCLUDED_MODULES[@]}"; do
        if [[ "$module" == "$exc_mod" ]]; then
            return 0
        fi
    done
    return 1
}

is_test_or_support() {
    local module="$1"
    # Skip test modules and support code
    [[ "$module" == *"_test" ]] || \
    [[ "$module" == "support@"* ]] || \
    [[ "$module" == "phase1_"* ]] || \
    [[ "$module" == *"@@main" ]]
}

# Ensure build is up to date
if [[ ! -d "$EBIN_DIR" ]]; then
    echo "Building project..." >&2
    gleam build --target=erlang >/dev/null 2>&1 || {
        echo "Build failed" >&2
        exit 2
    }
fi

# Run tests first with gleam test
echo "Running tests..." >&2
gleam test >/dev/null 2>&1 || {
    echo "Tests failed" >&2
    exit 2
}

# Create temporary Erlang script for coverage analysis
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

cat > "$TMPDIR/coverage.erl" << 'ERLEOF'
-module(coverage).
-export([main/1]).

main([EbinDir]) ->
    cover:start(),

    % Get all beam files
    Beams = filelib:wildcard(EbinDir ++ "/*.beam"),

    % Compile each for coverage
    lists:foreach(fun(Beam) ->
        cover:compile_beam(Beam)
    end, Beams),

    % Get code paths for deps
    Deps = ["gleeunit", "gleam_stdlib", "gleam_erlang", "gleam_json",
            "gleam_otp", "simplifile", "gleam_yielder", "filepath"],
    DepPaths = [filename:join(["build", "dev", "erlang", Dep, "ebin"])
                || Dep <- Deps],
    lists:foreach(fun(P) -> code:add_path(P) end, DepPaths),

    % Discover test modules
    TestMods = [list_to_atom(filename:basename(F, ".beam"))
                || F <- Beams,
                   string:find(filename:basename(F), "_test.beam") =/= nomatch],

    % Run test functions
    lists:foreach(fun(Mod) ->
        try
            Exports = Mod:module_info(exports),
            TestFuns = [Fun || {Fun, 0} <- Exports,
                              is_test_fun(atom_to_list(Fun))],
            lists:foreach(fun(Fun) ->
                try Mod:Fun()
                catch _:_ -> ok
                end
            end, TestFuns)
        catch _:_ -> ok
        end
    end, TestMods),

    % Collect coverage
    Mods = cover:modules(),
    Results = lists:filtermap(fun(Mod) ->
        case cover:analyse(Mod, coverage, module) of
            {ok, {_, {Cov, NotCov}}} ->
                Total = Cov + NotCov,
                Pct = case Total of
                    0 -> 0.0;
                    _ -> (Cov / Total) * 100.0
                end,
                {true, {atom_to_list(Mod), Cov, Total, Pct}};
            _ -> false
        end
    end, Mods),

    % Output
    lists:foreach(fun({Name, Cov, Total, Pct}) ->
        io:format("~s|~p|~p|~.1f~n", [Name, Cov, Total, Pct])
    end, Results),

    cover:stop(),
    halt(0).

is_test_fun(Name) ->
    case lists:suffix("_test", Name) of
        true -> true;
        false -> false
    end.
ERLEOF

# Compile and run coverage module
erlc -o "$TMPDIR" "$TMPDIR/coverage.erl"

# Get all dep paths
DEP_PATHS=""
for dep in gleeunit gleam_stdlib gleam_erlang gleam_json gleam_otp simplifile gleam_yielder filepath; do
    DEP_PATHS="$DEP_PATHS -pa build/dev/erlang/$dep/ebin"
done

coverage_data=$(erl -noshell -pa "$TMPDIR" -pa "$EBIN_DIR" $DEP_PATHS \
    -run coverage main "$EBIN_DIR" 2>/dev/null) || {
    echo "Coverage analysis failed" >&2
    exit 2
}

# Parse and format results
declare -a modules
declare -a covered
declare -a totals
declare -a percents

total_covered=0
total_lines=0
eligible_covered=0
eligible_lines=0

while IFS='|' read -r module cov tot pct; do
    [[ -z "$module" ]] && continue

    # Skip test/support modules
    if is_test_or_support "$module"; then
        continue
    fi

    # Only include claude_agent_sdk modules
    if [[ "$module" != "claude_agent_sdk"* ]] && [[ "$module" != "bidir_ffi" ]]; then
        continue
    fi

    modules+=("$module")
    covered+=("$cov")
    totals+=("$tot")
    percents+=("$pct")

    total_covered=$((total_covered + cov))
    total_lines=$((total_lines + tot))

    # Track eligible modules separately
    if ! is_excluded_module "$module"; then
        eligible_covered=$((eligible_covered + cov))
        eligible_lines=$((eligible_lines + tot))
    fi
done <<< "$coverage_data"

# Calculate totals
if [[ $total_lines -gt 0 ]]; then
    total_pct=$(echo "scale=1; ($total_covered * 100) / $total_lines" | bc)
else
    total_pct="0.0"
fi

if [[ $eligible_lines -gt 0 ]]; then
    eligible_pct=$(echo "scale=1; ($eligible_covered * 100) / $eligible_lines" | bc)
else
    eligible_pct="0.0"
fi

# Output
if [[ "$JSON_OUTPUT" == "true" ]]; then
    echo "{"
    echo "  \"total_covered\": $total_covered,"
    echo "  \"total_lines\": $total_lines,"
    echo "  \"total_percent\": $total_pct,"
    echo "  \"eligible_covered\": $eligible_covered,"
    echo "  \"eligible_lines\": $eligible_lines,"
    echo "  \"eligible_percent\": $eligible_pct,"
    echo "  \"modules\": ["

    first=true
    for i in "${!modules[@]}"; do
        mod="${modules[$i]}"
        # Convert @ to / for display
        display_mod="${mod//@//}"
        excluded=""
        if is_excluded_module "$mod"; then
            excluded=", \"excluded\": true"
        fi

        if [[ "$first" == "true" ]]; then
            first=false
        else
            echo ","
        fi
        printf "    {\"module\": \"%s\", \"covered\": %s, \"total\": %s, \"percent\": %s%s}" \
            "$display_mod" "${covered[$i]}" "${totals[$i]}" "${percents[$i]}" "$excluded"
    done
    echo ""
    echo "  ],"
    echo "  \"excluded_modules\": ["
    first=true
    for exc in "${EXCLUDED_MODULES[@]}"; do
        if [[ "$first" == "true" ]]; then
            first=false
        else
            echo ","
        fi
        printf "    \"%s\"" "${exc//@//}"
    done
    echo ""
    echo "  ]"
    echo "}"
else
    # Text output
    printf "%-50s %10s %15s\n" "MODULE" "COVERAGE" "LINES"
    printf "%s\n" "$(printf '=%.0s' {1..75})"

    for i in "${!modules[@]}"; do
        mod="${modules[$i]}"
        display_mod="${mod//@//}"

        excluded_mark=""
        if is_excluded_module "$mod"; then
            excluded_mark=" [excluded]"
        fi

        if [[ "$ELIGIBLE_ONLY" == "true" ]] && is_excluded_module "$mod"; then
            continue
        fi

        printf "%-50s %9s%% %7s/%s%s\n" \
            "$display_mod" "${percents[$i]}" "${covered[$i]}" "${totals[$i]}" "$excluded_mark"
    done

    printf '%s\n' "$(printf -- '-%.0s' {1..75})"

    if [[ "$ELIGIBLE_ONLY" == "true" ]]; then
        printf "%-50s %9s%% %7s/%s\n" "ELIGIBLE TOTAL" "$eligible_pct" "$eligible_covered" "$eligible_lines"
    else
        printf "%-50s %9s%% %7s/%s\n" "TOTAL" "$total_pct" "$total_covered" "$total_lines"
    fi

    echo ""
    echo "Excluded from thresholds: ${EXCLUDED_MODULES[*]}"
fi

# Threshold check
if [[ -n "$THRESHOLD" ]]; then
    # Compare using bc for floating point
    below=$(echo "$eligible_pct < $THRESHOLD" | bc)
    if [[ "$below" == "1" ]]; then
        echo "" >&2
        echo "FAILED: Eligible coverage ${eligible_pct}% is below threshold ${THRESHOLD}%" >&2
        exit 1
    else
        echo "" >&2
        echo "PASSED: Eligible coverage ${eligible_pct}% meets threshold ${THRESHOLD}%" >&2
    fi
fi

exit 0
