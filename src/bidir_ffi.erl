-module(bidir_ffi).
-export([schedule_init_timeout/1, schedule_request_timeout/2, schedule_hook_timeout/2,
         cancel_timer/1, spawn_hook_task/4, demonitor_hook/1, kill_task/1]).

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

%% Schedule a request timeout message to be sent to the calling process.
%%
%% Uses erlang:send_after/3 to send a tagged tuple message after the
%% specified delay. The message includes the request_id for correlation.
%%
%% The message format {request_timeout, RequestId} can be received by a selector
%% using select_record with tag=request_timeout and arity=1.
%%
%% Returns the timer reference for later cancellation.
schedule_request_timeout(TimeoutMs, RequestId) ->
    Self = self(),
    erlang:send_after(TimeoutMs, Self, {request_timeout, RequestId}).

%% Cancel a timer by its reference.
%%
%% Uses erlang:cancel_timer/1 to cancel the timer. Returns true if the
%% timer was cancelled, false if it had already fired or didn't exist.
cancel_timer(TimerRef) ->
    case erlang:cancel_timer(TimerRef) of
        false -> false;
        _ -> true
    end.

%% Spawn a linked task to execute a hook callback.
%%
%% Spawns a new process that:
%% 1. Executes the handler function with the input (with crash protection)
%% 2. Sends {hook_done, RequestId, Result} back to the parent
%%
%% On crash, sends {hook_error, RequestId, Reason} instead of {hook_done, ...}
%% to allow the GenServer to handle failures gracefully.
%%
%% Returns {Pid, MonitorRef} tuple for tracking.
spawn_hook_task(Parent, RequestId, Handler, Input) ->
    Pid = erlang:spawn_link(fun() ->
        try
            Result = Handler(Input),
            Parent ! {hook_done, RequestId, Result}
        catch
            _Class:Reason:_Stack ->
                Parent ! {hook_error, RequestId, Reason}
        end
    end),
    MonitorRef = erlang:monitor(process, Pid),
    {Pid, MonitorRef}.

%% Demonitor a hook task.
%%
%% Removes the monitor and flushes any DOWN message from the mailbox.
demonitor_hook(MonitorRef) ->
    erlang:demonitor(MonitorRef, [flush]),
    nil.

%% Schedule a hook timeout message to be sent to the calling process.
%%
%% Uses erlang:send_after/3 to send a tagged tuple message after the
%% specified delay. The message includes the request_id for correlation.
%%
%% The message format {hook_timeout, RequestId} can be received by a selector
%% using select_record with tag=hook_timeout and arity=1.
%%
%% Returns the timer reference for later cancellation.
schedule_hook_timeout(TimeoutMs, RequestId) ->
    Self = self(),
    erlang:send_after(TimeoutMs, Self, {hook_timeout, RequestId}).

%% Kill a task process immediately.
%%
%% Unlinks from the task first to prevent the exit signal from propagating
%% back to the calling process, then sends kill signal.
%% This is used when a hook timeout fires before the task completes.
kill_task(Pid) ->
    erlang:unlink(Pid),
    erlang:exit(Pid, kill),
    nil.
