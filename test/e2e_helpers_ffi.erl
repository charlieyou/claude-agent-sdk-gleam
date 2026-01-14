-module(e2e_helpers_ffi).
-export([get_env/1, set_env/2, unset_env/1, get_plain_args/0, kill_pid/1, acquire_lock/0, release_lock/0,
         get_timestamp_iso8601/0, get_monotonic_ms/0, ensure_dir/1, append_line/2,
         is_concurrent_mode/0, port_close_safe/1, has_soak_flag/0, read_file_lines/1,
         with_cleanup/2]).

-define(LOCK_TABLE, e2e_query_lock_table).

%% Get environment variable, converting charlist to binary for Gleam compatibility.
%% Returns {ok, Binary} if set, {error, nil} if not set.
get_env(Name) ->
    NameStr = binary_to_list(Name),
    case os:getenv(NameStr) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.

%% Set environment variable. Both name and value are binaries from Gleam.
%% Returns nil (Gleam's Nil type).
set_env(Name, Value) ->
    NameStr = binary_to_list(Name),
    ValueStr = binary_to_list(Value),
    os:putenv(NameStr, ValueStr),
    nil.

%% Unset environment variable. Name is a binary from Gleam.
%% Returns nil (Gleam's Nil type).
unset_env(Name) ->
    NameStr = binary_to_list(Name),
    os:unsetenv(NameStr),
    nil.

%% Return plain command-line arguments (after --), as a list of binaries.
get_plain_args() ->
    Args = init:get_plain_arguments(),
    list_to_binary_list(Args).

list_to_binary_list([]) -> [];
list_to_binary_list([Head | Tail]) ->
    [list_to_binary(Head) | list_to_binary_list(Tail)].

%% Kill a process immediately.
kill_pid(Pid) ->
    erlang:exit(Pid, kill),
    nil.

%% Ensure ETS table exists for query locking.
ensure_lock_table() ->
    case ets:info(?LOCK_TABLE) of
        undefined ->
            try
                ets:new(?LOCK_TABLE, [named_table, public, set]),
                ok
            catch
                error:badarg -> ok
            end;
        _ -> ok
    end.

%% Check if concurrent mode is enabled (E2E_ALLOW_CONCURRENT=1).
is_concurrent_mode() ->
    case os:getenv("E2E_ALLOW_CONCURRENT") of
        "1" -> true;
        _ -> false
    end.

%% Acquire a global lock to serialize E2E CLI queries.
%% Skips lock acquisition if concurrent mode is enabled.
acquire_lock() ->
    case is_concurrent_mode() of
        true -> nil;
        false ->
            ensure_lock_table(),
            case ets:insert_new(?LOCK_TABLE, {lock, self()}) of
                true -> ok;
                false ->
                    timer:sleep(50),
                    acquire_lock()
            end,
            nil
    end.

%% Release the global lock for E2E CLI queries.
release_lock() ->
    ensure_lock_table(),
    ets:delete(?LOCK_TABLE, lock),
    nil.

%% Get current timestamp as ISO8601 binary string.
get_timestamp_iso8601() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    Ms = erlang:system_time(millisecond) rem 1000,
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                                  [Y, Mo, D, H, Mi, S, Ms])).

%% Get monotonic time in milliseconds for elapsed calculations.
get_monotonic_ms() ->
    erlang:monotonic_time(millisecond).

%% Ensure directory exists (recursive mkdir -p).
ensure_dir(Path) ->
    PathStr = binary_to_list(Path),
    case filelib:ensure_dir(PathStr ++ "/") of
        ok -> {ok, nil};
        {error, Reason} -> {error, list_to_binary(atom_to_list(Reason))}
    end.

%% Append a line to a file (creates if not exists).
append_line(Path, Line) ->
    PathStr = binary_to_list(Path),
    LineWithNewline = <<Line/binary, "\n">>,
    case file:write_file(PathStr, LineWithNewline, [append]) of
        ok -> {ok, nil};
        {error, Reason} -> {error, list_to_binary(atom_to_list(Reason))}
    end.

%% Close an Erlang port immediately.
%% Returns ok on success, {error, Reason} on failure.
port_close_safe(Port) ->
    try
        erlang:port_close(Port),
        {ok, nil}
    catch
        error:badarg -> {error, <<"port_not_found">>};
        _:Reason -> {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Check if --soak flag is present in plain arguments.
%% Uses get_plain_args/0 for consistency with has_e2e_flag() semantics.
has_soak_flag() ->
    Args = get_plain_args(),
    lists:any(fun(Arg) -> Arg =:= <<"--soak">> end, Args).

%% Read file lines (for golden file parsing).
%% Returns {ok, [Binary]} on success, {error, Reason} on failure.
%% Handles both LF and CRLF line endings.
read_file_lines(Path) ->
    PathStr = binary_to_list(Path),
    case file:read_file(PathStr) of
        {ok, Content} ->
            %% Handle both CRLF and LF line endings
            Lines = binary:split(Content, [<<"\r\n">>, <<"\n">>], [global, trim]),
            %% Filter empty lines
            NonEmpty = [L || L <- Lines, L =/= <<>>],
            {ok, NonEmpty};
        {error, Reason} ->
            {error, list_to_binary(atom_to_list(Reason))}
    end.

%% Execute a function with guaranteed cleanup via try/after.
%% Cleanup runs even if F panics/throws/exits.
with_cleanup(F, Cleanup) ->
    try
        F()
    after
        Cleanup()
    end.
