-module(e2e_helpers_ffi).
-export([get_env/1, set_env/2, unset_env/1]).

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
