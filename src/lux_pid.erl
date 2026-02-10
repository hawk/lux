-module(lux_pid).

-export([pid/0,
         get_all/0,
         check_stale/1,
         kill/2]).
-export_type([env_map/0]).

-include("lux.hrl").

-type pid_info() :: #pid_info{}.
-type env_map() :: #{string() => string()}.

%% Get the PID for the current process, either from the LUX_PID environment
%% variable or from the OS.
-spec pid() -> string().
pid() ->
    case os:getenv("LUX_PID") of
        Pid when is_list(Pid) andalso Pid /= "" ->
            Pid;
        _ ->
            os:getpid()
    end.

%% Get a list of all running processes as PID strings.
-spec get_all() -> sets:set(string()).
get_all() ->
    case file:list_dir("/proc") of
        {ok, Processes} ->
            lists:foldl(
                fun(PidStr, Acc) ->
                    try erlang:list_to_integer(PidStr) of
                        _Pid ->
                            sets:add_element(PidStr, Acc)
                    catch
                        error:badarg ->
                            Acc
                    end
                end, sets:new(), Processes);
        {error, _Reason} ->
            %% /proc not mounted, can't collect process information.
            sets:new()
    end.

%% Get process information for the given PID string.
-spec get_process_info(PidStr :: string(), PidEnv :: env_map()) ->
    {ok, pid_info()} | error.
get_process_info(PidStr, PidEnv) ->
    maybe
        {ok, CmdPath} ?= get_command(PidStr),
        {ok, CmdLineData} ?= file:read_file(proc_cmdline_path(PidStr)),
        CmdLine = nul_list_to_list(CmdLineData),
        {ok, #pid_info{pid = PidStr,
                       env = PidEnv,
                       command_path = CmdPath,
                       command_name = filename:basename(CmdPath),
                       command_line = CmdLine}}
    else
        _ -> error
    end.

-spec get_command(PidStr :: string()) -> {ok, string()} | error.
get_command(PidStr) ->
    case file:read_link(proc_exe_path(PidStr)) of
        {ok, Cmd} ->
            {ok, Cmd};
        {error, _Reason} ->
            error
    end.

%% Get the environment variables for the given PID string.
-spec get_env(PidStr :: string()) -> {ok, env_map()} | error.
get_env(PidStr) ->
    case file:read_file(proc_environ_path(PidStr)) of
        {ok, EnvData} ->
            {ok, parse_env(EnvData)};
        {error, _Reason} ->
            error
    end.

%% Parse the data from /proc/PID/environ into a map of environment variables,
%% failed entries are silently ignored.
-spec parse_env(EnvData :: binary()) -> env_map().
parse_env(EnvData) ->
    lists:foldl(
        fun(Entry, Acc) ->
            case string:split(Entry, "=", leading) of
                [Key, Value] ->
                    maps:put(Key, Value, Acc);
                _ ->
                    Acc
            end
        end, #{}, nul_list_to_list(EnvData)).

%% Check for stale processes looking at the given list of PIDs identifying
%% processes with the LUX_PID environment set to the LUX process pid() value.
-spec check_stale([PidStr :: string()]) -> ok | {error, [pid_info()]}.
check_stale([]) ->
    ok;
check_stale(NewPids) ->
    check_stale(NewPids, pid(), []).

check_stale([], _LuxPid, _Errors = []) ->
    ok;
check_stale([], _LuxPid, Errors) ->
    {error, Errors};
check_stale([PidStr | T], LuxPidStr, Errors) ->
    case get_env(PidStr) of
        {ok, #{"LUX_PID" := LuxPidStr} = PidEnv} ->
            case get_process_info(PidStr, PidEnv) of
                {ok, Error} ->
                    check_stale(T, LuxPidStr, [Error|Errors]);
                error ->
                    check_stale(T, LuxPidStr, Errors)
            end;
        _ ->
            check_stale(T, LuxPidStr, Errors)
    end.

kill([], _Signum) ->
    ok;
kill([#pid_info{pid = PidStr} | T], Signum) ->
    kill_pid(PidStr, Signum),
    kill(T, Signum);
kill([PidStr | T], Signum) ->
    kill_pid(PidStr, Signum),
    kill(T, Signum);
kill(PidStr, Signum) ->
    kill_pid(PidStr, Signum).

nul_list_to_list(Bin) when is_binary(Bin) ->
    List = [?b2l(C) || C <- string:split(Bin, <<0>>, all)],
    case lists:reverse(List) of
        ["" | RevList] ->
            lists:reverse(RevList);
        _ ->
            List
    end.

%% Kill the process with the given PID string.
-spec kill_pid(PidStr :: integer() | string(), Signum :: integer() | string())
              -> ok.
kill_pid(Pid, Signum) when is_integer(Pid) ->
    kill_pid(integer_to_list(Pid), Signum);
kill_pid(PidStr, Signum) when is_integer(Signum) ->
    kill_pid(PidStr, integer_to_list(Signum));
kill_pid(PidStr, SignumStr) ->
    Flag = process_flag(trap_exit, true),
    try
        Args = {args, ["-" ++ SignumStr, PidStr]},
        Port = erlang:open_port({spawn_executable, "/bin/kill"}, [Args]),
        receive
            {'EXIT', Port, _Reason} ->
                ok
        end
    after
        process_flag(trap_exit, Flag)
    end.

%% Return path to the cmdline file for the given PID string.
-spec proc_cmdline_path(PidStr :: string()) -> string().
proc_cmdline_path(PidStr) ->
    filename:join(["/proc", PidStr, "cmdline"]).

%% Return path to the environ file for the given PID string.
-spec proc_environ_path(PidStr :: string()) -> string().
proc_environ_path(PidStr) ->
    filename:join(["/proc", PidStr, "environ"]).

%% Return path to the exe file for the given PID string.
-spec proc_exe_path(PidStr :: string()) -> string().
proc_exe_path(PidStr) ->
    filename:join(["/proc", PidStr, "exe"]).