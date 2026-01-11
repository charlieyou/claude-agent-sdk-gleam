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
    {JsonOutput, Threshold} = parse_args(Args, {false, none}),
    BeamDir = "build/dev/erlang/claude_agent_sdk/ebin",

    cover:start(),

    {ok, Files} = file:list_dir(BeamDir),

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

    lists:foreach(fun(F) ->
        case cover:compile_beam(F) of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end, SourceBeams),

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
    parse_args(Rest, {Json, list_to_float(N)});
parse_args([_ | Rest], Acc) ->
    parse_args(Rest, Acc).

run_tests(TestBeams, Quiet) ->
    lists:foreach(fun(F) ->
        Mod = list_to_atom(filename:basename(F, ".beam")),
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
    end, TestBeams).

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
