%% FFI functions for reducers_test.gleam
%%
%% Provides stub values for testing pure reducers without OTP dependencies.

-module(reducers_test_ffi).

-export([stub_runtime/0, stub_subject/0, stub_pid/0, stub_monitor/0, string_contains/2]).

%% Create a stub RuntimeState for testing.
%% The actual record structure matches actor.gleam's RuntimeState type.
stub_runtime() ->
    %% RuntimeState(runner, lifecycle, subscriber, self_subject, capabilities, inject_subject)
    %% We use atoms/nils as placeholders since reducers don't access these fields.
    {runtime_state,
     %% runner (BidirRunner stub)
     {bidir_runner, nil, nil, nil, nil, nil},
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
