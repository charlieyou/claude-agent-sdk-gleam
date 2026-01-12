-module(e2e_helpers_ffi).
-export([get_env/1]).

%% Get environment variable, converting charlist to binary for Gleam compatibility.
%% Returns {ok, Binary} if set, {error, nil} if not set.
get_env(Name) ->
    NameStr = binary_to_list(Name),
    case os:getenv(NameStr) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.
