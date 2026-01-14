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
#
# Note: This script instruments modules with cover BEFORE running tests,
# ensuring accurate coverage measurement. Tests are executed via the
# Erlang runtime with cover active, not via `gleam test`.
#
# Environment variables are inherited, so CLAUDE_INTEGRATION_TEST=1
# will gate integration tests the same as with `gleam test`.

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
            if [[ $# -lt 2 ]] || [[ ! "$2" =~ ^[0-9]+(\.[0-9]+)?$ ]]; then
                echo "Error: --threshold requires a numeric value (e.g., --threshold 95.0)" >&2
                exit 2
            fi
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
            echo "  --eligible-only Only report on pure modules (excludes FFI/port modules)"
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 2
            ;;
    esac
done

# Modules excluded from coverage thresholds (port/CLI boundary code)
# These contain OS-boundary code covered by integration/E2E tests
EXCLUDED_MODULES=(
    "claude_agent_sdk@internal@port_ffi"     # Port FFI - OS boundary
    "claude_agent_sdk_ffi"                    # Erlang FFI
    "bidir_ffi"                               # Bidirectional FFI
    "claude_agent_sdk@internal@cli"          # CLI detection uses port_ffi
)

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
    [[ "$module" == *"_test_ffi" ]] || \
    [[ "$module" == "support@"* ]] || \
    [[ "$module" == "phase1_"* ]] || \
    [[ "$module" == *"@@main" ]]
}

# Build project AND test modules (gleam build only compiles src, not test)
# We run gleam test briefly to compile test modules, then run our own coverage
echo "Building project and tests..." >&2
gleam build --target=erlang >/dev/null 2>&1 || {
    echo "Build failed" >&2
    exit 2
}
# Run gleam test to compile test modules to beam files
# Capture output to show on failure, but suppress on success
test_compile_output=$(gleam test --target=erlang 2>&1) || {
    echo "Test compilation failed:" >&2
    echo "$test_compile_output" >&2
    exit 2
}

if [[ ! -d "$EBIN_DIR" ]]; then
    echo "Build directory not found: $EBIN_DIR" >&2
    exit 2
fi

# Discover all dependency ebin paths dynamically
DEP_PATHS=""
for dep_dir in build/dev/erlang/*/ebin; do
    # Skip the main project - it's added separately
    if [[ "$dep_dir" != "$EBIN_DIR" ]]; then
        DEP_PATHS="$DEP_PATHS -pa $dep_dir"
    fi
done

# Run coverage analysis with tests executed under cover instrumentation
# This is the correct approach: instrument first, then run tests
echo "Running tests under coverage..." >&2
coverage_data=$(erl -noshell -pa "$EBIN_DIR" $DEP_PATHS -eval '
    %% Ensure all dependency modules are loaded first
    %% This is required because cover:compile_beam can interfere with module loading
    %% Discover all dep dirs dynamically (except the main project)
    AllDepDirs = filelib:wildcard("build/dev/erlang/*/ebin"),
    ProjectEbin = "'"$EBIN_DIR"'",
    DepDirs = [D || D <- AllDepDirs, D =/= ProjectEbin],
    lists:foreach(fun(Dir) ->
        DepBeams = filelib:wildcard(Dir ++ "/*.beam"),
        lists:foreach(fun(B) ->
            Mod = list_to_atom(filename:basename(B, ".beam")),
            code:ensure_loaded(Mod)
        end, DepBeams)
    end, DepDirs),

    %% Start cover AFTER loading dependencies
    cover:start(),

    %% Get all beam files from project
    EbinDir = "'"$EBIN_DIR"'",
    Beams = filelib:wildcard(EbinDir ++ "/*.beam"),

    %% Compile all project modules for coverage instrumentation
    %% This must happen BEFORE tests run so execution is tracked
    lists:foreach(fun(Beam) ->
        cover:compile_beam(Beam)
    end, Beams),

    %% Discover test modules (those ending in _test)
    TestMods = [list_to_atom(filename:basename(F, ".beam"))
                || F <- Beams,
                   string:find(filename:basename(F), "_test.beam") =/= nomatch],

    %% Fail immediately if no test modules found
    case TestMods of
        [] ->
            io:format(standard_error, "ERROR: No test modules found in ~s~n", [EbinDir]),
            cover:stop(),
            halt(2);
        _ -> ok
    end,

    %% Helper to check if a function name is a test function
    %% Matches both "test_foo" prefix and "foo_test" suffix patterns
    IsTestFun = fun(Name) ->
        lists:prefix("test_", Name) orelse lists:suffix("_test", Name)
    end,

    %% Run all test functions under cover instrumentation
    %% gleeunit exports test functions as test_Name/0 OR Name_test/0 with arity 0
    TestResults = lists:map(fun(Mod) ->
        try
            Exports = Mod:module_info(exports),
            TestFuns = [Fun || {Fun, 0} <- Exports,
                              IsTestFun(atom_to_list(Fun))],
            Results = lists:map(fun(Fun) ->
                try
                    case Mod:Fun() of
                        %% Gleam error tuple indicates test failure
                        {error, Err} ->
                            io:format(standard_error, "FAIL ~p:~p - error:~p~n",
                                      [Mod, Fun, Err]),
                            {Fun, fail};
                        %% Any other return (ok, nil, {ok,_}) is success
                        _ ->
                            {Fun, pass}
                    end
                catch
                    throw:skip -> {Fun, skip};
                    Class:Reason ->
                        io:format(standard_error, "FAIL ~p:~p - ~p:~p~n",
                                  [Mod, Fun, Class, Reason]),
                        {Fun, fail}
                end
            end, TestFuns),
            {Mod, ok, Results}
        catch C:R ->
            io:format(standard_error, "ERROR loading ~p: ~p:~p~n", [Mod, C, R]),
            {Mod, load_error, []}
        end
    end, TestMods),

    %% Count results and check for load errors
    LoadErrors = length([M || {M, load_error, _} <- TestResults]),
    AllResults = lists:flatten([Rs || {_, _, Rs} <- TestResults]),
    Passed = length([X || X = {_, pass} <- AllResults]),
    Failed = length([X || X = {_, fail} <- AllResults]),
    Skipped = length([X || X = {_, skip} <- AllResults]),

    %% Fail if any test modules failed to load
    case LoadErrors of
        0 -> ok;
        N ->
            io:format(standard_error, "~nERROR: ~p test module(s) failed to load~n", [N]),
            cover:stop(),
            halt(2)
    end,

    %% Fail if no tests were executed (prevents false positive coverage)
    TotalTests = Passed + Failed + Skipped,
    case TotalTests of
        0 ->
            io:format(standard_error, "~nERROR: No tests were executed~n", []),
            cover:stop(),
            halt(2);
        _ -> ok
    end,

    case Failed of
        0 -> ok;
        _ ->
            io:format(standard_error, "~n~p passed, ~p failed, ~p skipped~n",
                      [Passed, Failed, Skipped]),
            cover:stop(),
            halt(2)
    end,

    %% Collect coverage from all instrumented modules
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

    %% Output pipe-separated for bash parsing
    lists:foreach(fun({Name, Cov, Total, Pct}) ->
        io:format("~s|~p|~p|~.1f~n", [Name, Cov, Total, Pct])
    end, Results),

    io:format(standard_error, "~p passed, ~p skipped~n", [Passed, Skipped]),
    cover:stop(),
    halt(0).
' 2>&1) || {
    echo "Coverage analysis failed" >&2
    exit 2
}

# Separate stderr (test status) from stdout (coverage data)
# The coverage data lines contain | separators
coverage_lines=$(echo "$coverage_data" | grep '|' || true)
test_status=$(echo "$coverage_data" | grep -v '|' || true)

# Show test status
if [[ -n "$test_status" ]]; then
    echo "$test_status" >&2
fi

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

    # Only include claude_agent_sdk modules and bidir_ffi
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
done <<< "$coverage_lines"

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
    printf '%s\n' "$(printf '=%.0s' {1..75})"

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
