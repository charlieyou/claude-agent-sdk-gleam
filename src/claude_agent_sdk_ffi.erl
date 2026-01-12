-module(claude_agent_sdk_ffi).
-export([open_port/3, open_port_safe/3, open_port_bidir/2, receive_port_msg_blocking/1, receive_port_msg_timeout/2, close_port/1, port_write/2, find_cli_path/1, rescue/1, monotonic_time_ms/0, get_plain_arguments/0, unique_integer/0, system_info/0, os_cmd/1, otp_version/0, check_stderr_support/0, exact_equals/2]).

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
        {<<"ok">>, Port}
    catch
        error:Reason -> {<<"error">>, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Opens a port for bidirectional protocol communication.
%% Uses spawn_executable with options optimized for streaming JSON:
%% - binary: receive data as binary, not list
%% - {packet, 0}: raw binary mode, no framing (we handle line parsing)
%% - exit_status: receive {Port, {exit_status, Code}} on process exit
%% - use_stdio: communicate via stdin/stdout
%% - stderr_to_stdout: merge stderr into stdout (OTP >= 25 only)
%% Returns {ok, Port} | {error, Reason}.
%%
%% Uses try-with-fallback for stderr_to_stdout detection to handle
%% non-standard OTP release strings that fail version parsing.
open_port_bidir(Executable, Args) ->
    ExecStr = binary_to_list(Executable),
    ArgsStr = [binary_to_list(A) || A <- Args],
    %% Build base options
    BaseOpts = [
        {args, ArgsStr},
        binary,
        {packet, 0},
        exit_status,
        use_stdio
    ],
    %% Try with stderr_to_stdout first (OTP >= 25), fall back without it
    OptsWithStderr = BaseOpts ++ [stderr_to_stdout],
    try
        Port = erlang:open_port({spawn_executable, ExecStr}, OptsWithStderr),
        {<<"ok">>, Port}
    catch
        error:badarg ->
            %% stderr_to_stdout not supported, retry without it
            try
                Port2 = erlang:open_port({spawn_executable, ExecStr}, BaseOpts),
                {<<"ok">>, Port2}
            catch
                error:Reason2 -> {<<"error">>, list_to_binary(io_lib:format("~p", [Reason2]))}
            end;
        error:Reason -> {<<"error">>, list_to_binary(io_lib:format("~p", [Reason]))}
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

%% Writes data to a port's stdin.
%% Returns {ok, nil} on success, {error, <<"port_closed">>} if port is closed.
%% Data should be a binary (Gleam String/BitArray).
port_write(Port, Data) ->
    try
        erlang:port_command(Port, Data),
        {<<"ok">>, nil}
    catch
        error:badarg -> {<<"error">>, <<"port_closed">>}
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

%% Returns command-line arguments passed to the runtime as binaries.
get_plain_arguments() ->
    [list_to_binary(Arg) || Arg <- init:get_plain_arguments()].

%% Returns a unique positive integer for run IDs.
unique_integer() ->
    erlang:unique_integer([positive]).

%% Returns basic system info as {Os, Arch} binaries.
system_info() ->
    {_OsFamily, OsName} = os:type(),
    Arch = erlang:system_info(system_architecture),
    Os = list_to_binary(atom_to_list(OsName)),
    {Os, list_to_binary(Arch)}.

%% Run a shell command and return stdout as a binary.
os_cmd(Command) ->
    CommandStr = binary_to_list(Command),
    list_to_binary(os:cmd(CommandStr)).

%% Returns OTP major version as {<<"ok">>, Version} or {<<"error">>, Reason}.
%% Uses erlang:system_info(otp_release) which returns a string like "27".
%% Uses string:to_integer/1 for safe parsing to avoid badarg on non-numeric values.
otp_version() ->
    Release = erlang:system_info(otp_release),
    case string:to_integer(Release) of
        {Version, []} when is_integer(Version) ->
            {<<"ok">>, Version};
        {_Version, [_|_]} ->
            {<<"error">>, <<"non_numeric_suffix">>};
        {error, Reason} ->
            {<<"error">>, list_to_binary(io_lib:format("~p", [Reason]))};
        _ ->
            {<<"error">>, <<"parse_failed">>}
    end.

%% Checks if stderr_to_stdout port option is supported (OTP >= 25).
%% Returns {supported, true} or {supported, false}.
%% If OTP version cannot be determined, defaults to unsupported (false).
check_stderr_support() ->
    case otp_version() of
        {<<"ok">>, Version} ->
            Supported = Version >= 25,
            {<<"supported">>, Supported};
        {<<"error">>, _} ->
            %% Cannot determine version, assume unsupported
            {<<"supported">>, false}
    end.

%% Exact equality comparison for Dynamic values.
%% Returns true if A =:= B (exact equality), false otherwise.
%% Used for comparing port references in decoded messages.
exact_equals(A, B) ->
    A =:= B.
