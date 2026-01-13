-module(e2e_helpers_ffi).
-export([get_env/1, set_env/2, unset_env/1, get_plain_args/0, kill_pid/1, acquire_lock/0, release_lock/0]).

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

%% Acquire a global lock to serialize E2E CLI queries.
acquire_lock() ->
    ensure_lock_table(),
    case ets:insert_new(?LOCK_TABLE, {lock, self()}) of
        true -> ok;
        false ->
            timer:sleep(50),
            acquire_lock()
    end,
    nil.

%% Release the global lock for E2E CLI queries.
release_lock() ->
    ensure_lock_table(),
    ets:delete(?LOCK_TABLE, lock),
    nil.
