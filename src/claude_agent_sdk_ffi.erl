-module(claude_agent_sdk_ffi).
-export([open_port/3, open_port_safe/3, receive_port_msg_blocking/1, receive_port_msg_timeout/2, close_port/1, find_cli_path/1, rescue/1, monotonic_time_ms/0]).

%% Opens a port to spawn an executable with given args and working directory.
%% Returns the port reference.
%% Gleam passes binaries; open_port expects charlists for executable/args/cd.
open_port(Executable, Args, WorkingDir) ->
    ExecStr = binary_to_list(Executable),
    ArgsStr = [binary_to_list(A) || A <- Args],
    Opts = case WorkingDir of
        <<>> -> [];
        _ -> [{cd, binary_to_list(WorkingDir)}]
    end,
    erlang:open_port({spawn_executable, ExecStr}, [
        {args, ArgsStr},
        stream,
        binary,
        exit_status,
        use_stdio
    ] ++ Opts).

%% Safe version of open_port that catches exceptions and returns {ok, Port} | {error, Reason}.
%% Use this for version detection where spawn failure should be a Result, not a crash.
open_port_safe(Executable, Args, WorkingDir) ->
    ExecStr = binary_to_list(Executable),
    ArgsStr = [binary_to_list(A) || A <- Args],
    Opts = case WorkingDir of
        <<>> -> [];
        _ -> [{cd, binary_to_list(WorkingDir)}]
    end,
    try
        Port = erlang:open_port({spawn_executable, ExecStr}, [
            {args, ArgsStr},
            stream,
            binary,
            exit_status,
            use_stdio
        ] ++ Opts),
        {ok, Port}
    catch
        error:Reason -> {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Blocking receive for port messages.
%% Returns {<<"data">>, Bytes} | {<<"exit_status">>, Code} | {<<"eof">>, nil}
receive_port_msg_blocking(Port) ->
    receive
        {Port, {data, Data}} ->
            {<<"data">>, Data};
        {Port, {exit_status, Status}} ->
            {<<"exit_status">>, Status};
        {Port, eof} ->
            {<<"eof">>, nil}
    end.

%% Timed receive for port messages.
%% Returns {<<"data">>, Bytes} | {<<"exit_status">>, Code} | {<<"eof">>, nil} | {<<"timeout">>, nil}
receive_port_msg_timeout(Port, TimeoutMs) ->
    receive
        {Port, {data, Data}} ->
            {<<"data">>, Data};
        {Port, {exit_status, Status}} ->
            {<<"exit_status">>, Status};
        {Port, eof} ->
            {<<"eof">>, nil}
    after TimeoutMs ->
        {<<"timeout">>, nil}
    end.

%% Closes the port and drains any remaining messages.
%% Drains first to capture any final messages (like exit_status) before close.
%% Uses bounded drain: max 100 messages, 50ms timeout per message.
%% Safe to call on already-closed ports (no-op).
close_port(Port) ->
    try
        drain_port_messages(Port, 100),
        erlang:port_close(Port)
    catch
        error:badarg -> ok  % Port already closed
    end,
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

%% Finds an executable in PATH using os:find_executable/1.
%% Returns {<<"ok">>, AbsolutePath} if found, {<<"error">>, <<"not_found">>} otherwise.
%% Name is expected to be a Gleam binary (e.g., <<"claude">>).
find_cli_path(Name) ->
    NameStr = binary_to_list(Name),
    case os:find_executable(NameStr) of
        false -> {<<"error">>, <<"not_found">>};
        Path -> {<<"ok">>, list_to_binary(Path)}
    end.

%% Rescue function to catch panics for testing.
%% Calls the thunk and returns {ok, Result} on success or {error, Reason} on panic/throw/exit.
%% Used to test that panics still trigger cleanup (e.g., with_stream closes port on panic).
rescue(Thunk) ->
    try
        Result = Thunk(),
        {ok, Result}
    catch
        error:#{gleam_error := panic, message := Message} ->
            {error, Message};
        error:#{gleam_error := panic} ->
            {error, <<"panic">>};
        error:Reason ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))};
        throw:Reason ->
            {error, list_to_binary(io_lib:format("throw: ~p", [Reason]))};
        exit:Reason ->
            {error, list_to_binary(io_lib:format("exit: ~p", [Reason]))}
    end.

%% Returns monotonic time in milliseconds.
%% Used for deadline-based timeouts that don't reset on each message.
monotonic_time_ms() ->
    erlang:monotonic_time(millisecond).
