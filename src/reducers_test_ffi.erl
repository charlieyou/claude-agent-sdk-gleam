%% FFI functions for reducers_test.gleam
%%
%% Provides stub values for testing pure reducers without OTP dependencies.
%% These stubs are ONLY used by test code that exercises pure state transformations.
%% The reducers never access runtime fields - they only transform pending/queue state.

-module(reducers_test_ffi).

-export([stub_runtime/0, stub_subject/0, stub_pid/0, stub_monitor/0, string_contains/2]).

%% Create a stub RuntimeState for testing.
%%
%% IMPORTANT: This creates a minimal valid RuntimeState structure.
%% Tests using this MUST NOT access fields within runtime (runner, lifecycle, etc.)
%% because these are stubs. The reducers only access state.pending and state.config,
%% never state.runtime, so this is safe.
%%
%% If RuntimeState changes, tests will fail at runtime when accessing invalid fields,
%% but since reducers never access runtime fields, this is acceptable for unit tests.
stub_runtime() ->
    %% RuntimeState has 6 fields: runner, lifecycle, subscriber, self_subject, capabilities, inject_subject
    %% BidirRunner has 3 fields: port, write, close
    {runtime_state,
     %% runner (BidirRunner with 3 fields)
     {bidir_runner, nil, nil, nil},
     %% lifecycle - Starting
     starting,
     %% subscriber - stub subject
     stub_subject(),
     %% self_subject - stub subject
     stub_subject(),
     %% capabilities - None
     none,
     %% inject_subject - None
     none
    }.

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
