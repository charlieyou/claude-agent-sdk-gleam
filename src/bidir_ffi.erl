-module(bidir_ffi).
-export([schedule_init_timeout/1, cancel_timer/1]).

%% Schedule an init timeout message to be sent to the calling process.
%%
%% Uses erlang:send_after/3 to send a tagged tuple message after the
%% specified delay. The message is sent to the calling process (self()).
%%
%% The message format {init_timeout, nil} can be received by a selector
%% using select_record with tag=init_timeout and arity=1.
%%
%% Returns the timer reference for later cancellation.
schedule_init_timeout(TimeoutMs) ->
    Self = self(),
    erlang:send_after(TimeoutMs, Self, {init_timeout, nil}).

%% Cancel a timer by its reference.
%%
%% Uses erlang:cancel_timer/1 to cancel the timer. Returns true if the
%% timer was cancelled, false if it had already fired or didn't exist.
cancel_timer(TimerRef) ->
    case erlang:cancel_timer(TimerRef) of
        false -> false;
        _ -> true
    end.
