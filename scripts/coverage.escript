#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa build/dev/erlang/claude_agent_sdk/ebin -pa build/dev/erlang/gleeunit/ebin -pa build/dev/erlang/gleam_stdlib/ebin -pa build/dev/erlang/gleam_erlang/ebin -pa build/dev/erlang/gleam_json/ebin -pa build/dev/erlang/gleam_yielder/ebin -pa build/dev/erlang/simplifile/ebin -pa build/dev/erlang/filepath/ebin

%% Coverage runner for Gleam/Erlang projects using Erlang's cover module.
%% Compiles source modules for coverage instrumentation, runs tests, and reports.
%%
%% Usage: ./scripts/coverage.escript [--json] [--threshold N]
%%   --json       Output JSON format instead of text
%%   --threshold  Fail if total coverage below N% (default: no threshold)

main(Args) ->
    case parse_args(Args, {false, none}) of
        {error, Msg} ->
            io:format(standard_error, "Error: ~s~n", [Msg]),
            halt(1);
        {JsonOutput, Threshold} ->
            run_coverage(JsonOutput, Threshold)
    end.

run_coverage(JsonOutput, Threshold) ->
    BeamDir = "build/dev/erlang/claude_agent_sdk/ebin",

    Files = case file:list_dir(BeamDir) of
        {ok, F} -> F;
        {error, enoent} ->
            io:format(standard_error, "Error: Build directory not found: ~s~n", [BeamDir]),
            io:format(standard_error, "Run 'gleam build --target=erlang' first.~n", []),
            halt(1);
        {error, Reason} ->
            io:format(standard_error, "Error reading build directory: ~p~n", [Reason]),
            halt(1)
    end,

    cover:start(),

    IsSourceMod = fun(F) ->
        Ext = filename:extension(F),
        Base = filename:basename(F, ".beam"),
        Ext =:= ".beam" andalso
        not lists:suffix("_test", Base) andalso
        not lists:suffix("@@main", Base) andalso
        string:prefix(Base, "support@") =:= nomatch andalso
        string:prefix(Base, "phase1_") =:= nomatch
    end,

    SourceBeams = [filename:join(BeamDir, F) || F <- Files, IsSourceMod(F)],

    case JsonOutput of
        false -> io:format("Compiling ~p modules for coverage...~n", [length(SourceBeams)]);
        true -> ok
    end,

    FailedMods = lists:filtermap(fun(BeamFile) ->
        ModName = list_to_atom(filename:basename(BeamFile, ".beam")),
        case cover:compile_beam(BeamFile) of
            {ok, _} -> false;
            {error, R} -> {true, {ModName, R}}
        end
    end, SourceBeams),

    case {JsonOutput, FailedMods} of
        {false, [_|_]} ->
            io:format(standard_error, "Warning: ~p module(s) failed to instrument:~n", [length(FailedMods)]),
            lists:foreach(fun({M, R}) ->
                io:format(standard_error, "  ~p: ~p~n", [M, R])
            end, FailedMods);
        _ -> ok
    end,

    case JsonOutput of
        false -> io:format("Running tests...~n~n");
        true -> ok
    end,

    % Run test modules
    TestBeams = [F || F <- Files,
                 filename:extension(F) =:= ".beam",
                 lists:suffix("_test.beam", F)],

    run_tests(TestBeams, JsonOutput),

    Results = collect_results(),

    case JsonOutput of
        false -> print_text_report(Results);
        true -> print_json_report(Results)
    end,

    {TotalCov, TotalNotCov} = lists:foldl(fun({_, Cov, NotCov, _}, {AC, AN}) ->
        {AC + Cov, AN + NotCov}
    end, {0, 0}, Results),

    TotalPct = case TotalCov + TotalNotCov of
        0 -> 0.0;
        T -> TotalCov * 100.0 / T
    end,

    cover:stop(),

    ExitCode = case Threshold of
        none -> 0;
        N when TotalPct >= N -> 0;
        _ -> 1
    end,
    halt(ExitCode).

parse_args([], Acc) -> Acc;
parse_args(["--json" | Rest], {_, Thresh}) ->
    parse_args(Rest, {true, Thresh});
parse_args(["--threshold", N | Rest], {Json, _}) ->
    case parse_threshold(N) of
        {ok, Val} -> parse_args(Rest, {Json, Val});
        error -> {error, io_lib:format("--threshold requires a numeric value, got: ~s", [N])}
    end;
parse_args([_ | Rest], Acc) ->
    parse_args(Rest, Acc).

parse_threshold(S) ->
    case string:to_float(S) of
        {F, []} -> {ok, F};
        {F, ".0"} -> {ok, F};
        _ ->
            case string:to_integer(S) of
                {I, []} -> {ok, float(I)};
                _ -> error
            end
    end.

run_tests(TestBeams, Quiet) ->
    % When in JSON mode, redirect stdout to suppress test output that would pollute JSON
    SavedGL = erlang:group_leader(),
    NullHandle = case Quiet of
        true ->
            case file:open("/dev/null", [write]) of
                {ok, ND} ->
                    erlang:group_leader(ND, self()),
                    ND;
                {error, Reason} ->
                    io:format(standard_error, "Warning: Could not open /dev/null: ~p~n", [Reason]),
                    io:format(standard_error, "Test output may pollute JSON output~n", []),
                    undefined
            end;
        false ->
            undefined
    end,
    try
        lists:foreach(fun(TestFile) ->
            Mod = list_to_atom(filename:basename(TestFile, ".beam")),
            try
                Exports = Mod:module_info(exports),
                TestFuns = [Name || {Name, 0} <- Exports,
                            lists:suffix("_test", atom_to_list(Name))],
                lists:foreach(fun(Test) ->
                    try
                        Mod:Test(),
                        case Quiet of
                            false -> io:format("  ✓ ~p:~p~n", [Mod, Test]);
                            true -> ok
                        end
                    catch
                        C:E:_St ->
                            io:format(standard_error, "  ✗ ~p:~p: ~p:~p~n", [Mod, Test, C, E])
                    end
                end, TestFuns)
            catch
                _:_:_ -> ok
            end
        end, TestBeams)
    after
        % Restore stdout for JSON output
        erlang:group_leader(SavedGL, self()),
        case NullHandle of
            undefined -> ok;
            Handle -> file:close(Handle)
        end
    end.

collect_results() ->
    Modules = cover:modules(),
    lists:map(fun(M) ->
        case cover:analyse(M, coverage, module) of
            {ok, {M, {Cov, NotCov}}} ->
                Total = Cov + NotCov,
                Pct = case Total of
                    0 -> 0.0;
                    _ -> Cov * 100.0 / Total
                end,
                {M, Cov, NotCov, Pct};
            _ ->
                {M, 0, 0, 0.0}
        end
    end, lists:sort(Modules)).

print_text_report(Results) ->
    io:format("~n========================================~n"),
    io:format("          COVERAGE REPORT~n"),
    io:format("========================================~n~n"),

    io:format("~-50s ~10s ~15s~n", ["MODULE", "COVERAGE", "LINES"]),
    io:format("~s~n", [string:copies("-", 75)]),

    {TotalCov, TotalNotCov} = lists:foldl(fun({Mod, Cov, NotCov, Pct}, {AccCov, AccNotCov}) ->
        ModStr = atom_to_list(Mod),
        DisplayMod = re:replace(ModStr, "@", "/", [global, {return, list}]),
        io:format("~-50s ~8.1f% ~7p/~p~n", [DisplayMod, Pct, Cov, Cov + NotCov]),
        {AccCov + Cov, AccNotCov + NotCov}
    end, {0, 0}, Results),

    io:format("~s~n", [string:copies("-", 75)]),
    TotalLines = TotalCov + TotalNotCov,
    TotalPct = case TotalLines of
        0 -> 0.0;
        _ -> TotalCov * 100.0 / TotalLines
    end,
    io:format("~-50s ~8.1f% ~7p/~p~n~n", ["TOTAL", TotalPct, TotalCov, TotalLines]).

print_json_report(Results) ->
    {TotalCov, TotalNotCov} = lists:foldl(fun({_, Cov, NotCov, _}, {AC, AN}) ->
        {AC + Cov, AN + NotCov}
    end, {0, 0}, Results),
    TotalPct = case TotalCov + TotalNotCov of
        0 -> 0.0;
        T -> TotalCov * 100.0 / T
    end,

    ModulesJson = lists:map(fun({Mod, Cov, NotCov, Pct}) ->
        ModStr = atom_to_list(Mod),
        DisplayMod = re:replace(ModStr, "@", "/", [global, {return, list}]),
        io_lib:format("{\"module\": \"~s\", \"covered\": ~p, \"total\": ~p, \"percent\": ~.1f}",
                     [DisplayMod, Cov, Cov + NotCov, Pct])
    end, Results),

    io:format("{~n"),
    io:format("  \"total_covered\": ~p,~n", [TotalCov]),
    io:format("  \"total_lines\": ~p,~n", [TotalCov + TotalNotCov]),
    io:format("  \"total_percent\": ~.1f,~n", [TotalPct]),
    io:format("  \"modules\": [~n    ~s~n  ]~n", [string:join(ModulesJson, ",\n    ")]),
    io:format("}~n").
