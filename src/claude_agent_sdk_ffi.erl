-module(claude_agent_sdk_ffi).
-export([open_port/3, receive_port_msg_blocking/1, receive_port_msg_timeout/2, close_port/1]).

%% Opens a port to spawn an executable with given args and working directory.
%% Returns the port reference.
open_port(Executable, Args, WorkingDir) ->
    erlang:open_port({spawn_executable, Executable}, [
        {args, Args},
        {cd, WorkingDir},
        stream,
        binary,
        exit_status,
        use_stdio
    ]).

%% Blocking receive for port messages.
%% Returns {"data", Bytes} | {"exit_status", Code} | {"eof", nil}
receive_port_msg_blocking(Port) ->
    receive
        {Port, {data, Data}} ->
            {"data", Data};
        {Port, {exit_status, Status}} ->
            {"exit_status", Status};
        {Port, eof} ->
            {"eof", nil}
    end.

%% Timed receive for port messages.
%% Returns {"data", Bytes} | {"exit_status", Code} | {"eof", nil} | {"timeout", nil}
receive_port_msg_timeout(Port, TimeoutMs) ->
    receive
        {Port, {data, Data}} ->
            {"data", Data};
        {Port, {exit_status, Status}} ->
            {"exit_status", Status};
        {Port, eof} ->
            {"eof", nil}
    after TimeoutMs ->
        {"timeout", nil}
    end.

%% Closes the port and drains any remaining messages.
%% Drains first to capture any final messages (like exit_status) before close.
%% Uses bounded drain: max 100 messages, 50ms timeout per message.
close_port(Port) ->
    drain_port_messages(Port, 100),
    erlang:port_close(Port),
    ok.

%% Internal: drains port messages from mailbox with bounded iterations.
drain_port_messages(_Port, 0) ->
    ok;
drain_port_messages(Port, Remaining) ->
    receive
        {Port, _} ->
            drain_port_messages(Port, Remaining - 1)
    after 50 ->
        ok
    end.
