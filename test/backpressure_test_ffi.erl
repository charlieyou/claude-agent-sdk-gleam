-module(backpressure_test_ffi).
-export([stub_pid/0, stub_monitor/0]).

%% Returns a dummy Pid (self, since we just need a valid Pid type)
stub_pid() ->
    self().

%% Returns a dummy monitor reference
stub_monitor() ->
    make_ref().
