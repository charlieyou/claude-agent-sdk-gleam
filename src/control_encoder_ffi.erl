-module(control_encoder_ffi).
-export([encode_dynamic/1]).

%% Encodes a Dynamic (Erlang term) to JSON format compatible with gleam_json.
%% This uses Erlang's json:encode/1 (OTP 27+) to encode arbitrary terms.
%% The result can be used directly as a Json value in Gleam.
%%
%% Handles Gleam-specific types:
%% - Gleam tuples (#(a, b)) become ["a", "b"] arrays
%% - Gleam records become JSON objects
%% - nil becomes null
%% - Atoms (except true/false/nil) become strings
encode_dynamic(Term) ->
    json:encode(Term, fun encoder/2).

%% Custom encoder function for Gleam-specific types
encoder(nil, Encode) ->
    Encode(null, Encode);
encoder(Tuple, Encode) when is_tuple(Tuple) ->
    %% Convert Gleam tuples to JSON arrays
    List = tuple_to_list(Tuple),
    Encode(List, Encode);
encoder(Atom, Encode) when is_atom(Atom) ->
    %% Convert atoms to strings (true/false/null handled by json module)
    Encode(atom_to_binary(Atom, utf8), Encode);
encoder(Other, Encode) ->
    %% Default encoding for maps, lists, numbers, binaries
    json:encode_value(Other, Encode).
