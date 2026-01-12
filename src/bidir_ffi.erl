-module(bidir_ffi).
-export([schedule_init_timeout/1]).

%% Schedule an init timeout message to be sent to the calling process.
%%
%% Uses erlang:send_after/3 to send a tagged tuple message after the
%% specified delay. The message is sent to the calling process (self()).
%%
%% The message format {init_timeout, nil} can be received by a selector
%% using select_record with tag=init_timeout and arity=1.
schedule_init_timeout(TimeoutMs) ->
    Self = self(),
    erlang:send_after(TimeoutMs, Self, {init_timeout, nil}),
    nil.
