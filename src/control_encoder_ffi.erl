-module(control_encoder_ffi).
-export([encode_dynamic/1]).

%% Encodes a Dynamic (Erlang term) to JSON format compatible with gleam_json.
%% This uses Erlang's json:encode/1 (OTP 27+) to encode arbitrary terms.
%% The result can be used directly as a Json value in Gleam.
%%
%% Handles Gleam-specific types:
%% - Gleam proplists ([{<<"key">>, value}, ...]) become JSON objects
%% - Empty lists [] become JSON arrays []
%% - Non-proplist tuples become JSON arrays
%% - nil becomes null
%% - true/false remain booleans
%% - Other atoms become strings
encode_dynamic(Term) ->
    json:encode(Term, fun encoder/2).

%% Custom encoder function for Gleam-specific types
encoder(nil, Encode) ->
    %% Encode nil directly as JSON null (bypass atom handler)
    json:encode_value(null, Encode);
encoder(true, Encode) ->
    %% Preserve boolean true
    json:encode_value(true, Encode);
encoder(false, Encode) ->
    %% Preserve boolean false
    json:encode_value(false, Encode);
encoder([], Encode) ->
    %% Empty list is always a JSON array, not an object
    json:encode_value([], Encode);
encoder(List, Encode) when is_list(List) ->
    %% Check if this is a non-empty proplist (list of 2-tuples with binary keys)
    case is_proplist(List) of
        true ->
            %% Convert proplist to JSON object
            Map = proplist_to_map(List),
            json:encode_value(Map, Encode);
        false ->
            %% Regular list - encode as array
            json:encode_value(List, Encode)
    end;
encoder(Tuple, Encode) when is_tuple(Tuple) ->
    %% Convert non-proplist tuples (like #(1, 2) or Ok("val")) to arrays
    List = tuple_to_list(Tuple),
    Encode(List, Encode);
encoder(Atom, Encode) when is_atom(Atom) ->
    %% Convert other atoms to strings
    Encode(atom_to_binary(Atom, utf8), Encode);
encoder(Other, Encode) ->
    %% Default encoding for maps, numbers, binaries
    json:encode_value(Other, Encode).

%% Check if a non-empty list is a proplist (list of 2-tuples with binary keys)
%% Note: Empty list handled separately in encoder/2 to always produce []
is_proplist([{Key, _Value} | Rest]) when is_binary(Key) ->
    is_proplist_rest(Rest);
is_proplist(_) -> false.

is_proplist_rest([]) -> true;
is_proplist_rest([{Key, _Value} | Rest]) when is_binary(Key) ->
    is_proplist_rest(Rest);
is_proplist_rest(_) -> false.

%% Convert proplist to map for JSON object encoding
proplist_to_map(Proplist) ->
    lists:foldl(fun({Key, Value}, Acc) ->
        maps:put(Key, Value, Acc)
    end, #{}, Proplist).
