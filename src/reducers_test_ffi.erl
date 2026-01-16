%% FFI functions for reducers_test.gleam
%%
%% Provides stub values for testing pure reducers without OTP dependencies.
%% These stubs are ONLY used by test code that exercises pure state transformations.

-module(reducers_test_ffi).

-export([stub_subject/0, stub_pid/0, stub_monitor/0, string_contains/2]).

%% Create a stub Subject for testing.
stub_subject() ->
    %% Subjects are references in Gleam/Erlang
    make_ref().

%% Create a stub Pid for testing.
stub_pid() ->
    self().

%% Create a stub Monitor reference for testing.
stub_monitor() ->
    make_ref().

%% Check if haystack contains needle.
string_contains(Haystack, Needle) ->
    binary:match(Haystack, Needle) =/= nomatch.
