%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2012 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_shell).

-export([start_monitor/3]).

-include("lux.hrl").

-record(cstate,
        {orig_file               :: string(),
         parent                  :: pid(),
         name                    :: string(),
         latest_cmd              :: #cmd{},
         wait_for_expect         :: undefined | pid(),
         mode = resume           :: resume | suspend,
         start_reason            :: fail | success | normal,
         progress                :: silent | brief | doc | compact | verbose,
         log_fun                 :: function(),
         log_prefix              :: string(),
         event_log_fd            :: {true, file:io_device()},
         stdin_log_fd            :: {false, file:io_device()},
         stdout_log_fd           :: {false, file:io_device()},
         multiplier              :: non_neg_integer(),
         poll_timeout            :: non_neg_integer(),
         flush_timeout           :: non_neg_integer(),
         timeout                 :: non_neg_integer() | infinity,
         shell_wrapper           :: undefined | string(),
         shell_cmd               :: string(),
         shell_args              :: [string()],
         port                    :: port(),
         waiting = false         :: boolean(),
         fail                    :: undefined | #cmd{},
         success                 :: undefined | #cmd{},
         expected                :: undefined | #cmd{},
         actual = <<>>           :: binary(),
         state_changed = false   :: boolean(),
         timed_out = false       :: boolean(),
         idle_count = 0          :: non_neg_integer(),
         no_more_input = false   :: boolean(),
         no_more_output = false  :: boolean(),
         timer                   :: undefined | infinity | reference(),
         timer_started_at        :: undefined | {non_neg_integer(),
                                                 non_neg_integer(),
                                                 non_neg_integer()},
         submatches = []         :: [binary()],
         events = []             :: [tuple()],
         macro_dict              :: [string()],   % ["name=val"]
         dict                    :: [string()],   % ["name=val"]
         builtin_dict            :: [string()],   % ["name=val"]
         system_dict             :: [string()]}). % ["name=val"]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client

start_monitor(I, Cmd, Name) ->
    OrigFile = I#istate.orig_file,
    Self = self(),
    Base = filename:basename(OrigFile),
    Prefix = filename:join([I#istate.log_dir, Base ++ "." ++ Name]),
    C = #cstate{orig_file = OrigFile,
                parent = Self,
                name = Name,
                start_reason = I#istate.cleanup_reason,
                latest_cmd = Cmd,
                progress = I#istate.progress,
                log_fun = I#istate.log_fun,
                event_log_fd = I#istate.event_log_fd,
                log_prefix = Prefix,
                multiplier = I#istate.multiplier,
                poll_timeout = I#istate.poll_timeout,
                flush_timeout = I#istate.flush_timeout,
                timeout = I#istate.timeout,
                shell_wrapper = I#istate.shell_wrapper,
                shell_cmd = I#istate.shell_cmd,
                shell_args = I#istate.shell_args,
                macro_dict = I#istate.macro_dict,
                dict = I#istate.dict,
                builtin_dict = I#istate.builtin_dict,
                system_dict = I#istate.system_dict},
    {Pid, Ref} = spawn_monitor(fun() -> init(C) end),
    Shell = #shell{name = Name, pid = Pid, ref = Ref, health = alive},
    I2 = I#istate{active = Pid, shells = [Shell | I#istate.shells]},
    receive
        {started, Pid, Logs} ->
            {ok, I2#istate{logs = I2#istate.logs ++ [Logs]}};
        {'DOWN', _, process, Pid, Reason} ->
            {error, I2, Pid, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server

init(C) when is_record(C, cstate) ->
    erlang:monitor(process, C#cstate.parent),
    process_flag(trap_exit, true),
    Name = C#cstate.name,
    {InFile, InFd} = open_logfile(C, "stdin"),
    {OutFile, OutFd} = open_logfile(C, "stdout"),
    C2 = C#cstate{stdin_log_fd = {false, InFd},
                  stdout_log_fd = {false, OutFd}},
    {Exec, Args} = choose_exec(C2),
    FlatExec = lists:flatten([Exec, [[" ", A] || A <- Args]]),
    Events = save_event(C2, start, FlatExec),
    StartReason = atom_to_list(C#cstate.start_reason),
    PortEnv = [{"LUX_SHELLNAME", Name},
               {"LUX_START_REASON", StartReason}],
    WorkDir = filename:dirname(C#cstate.orig_file),
    Opts = [binary, stream, use_stdio, stderr_to_stdout,
            {args, Args}, {cd, WorkDir}, {env, PortEnv}],
    try
        Port = open_port({spawn_executable, Exec}, Opts),
        Dict = ["PS1=SH-PROMPT:",
                "LUX_SHELLNAME=" ++ Name,
                "LUX_START_REASON=" ++ StartReason],
        C3 = C2#cstate{port = Port,
                       events = Events,
                       dict = Dict ++ C2#cstate.dict},

        Parent = C3#cstate.parent,
        erlang:monitor(process, Parent),
        Parent ! {started, self(), {Name, InFile, OutFile}},
        try
            shell_loop(C3, C3)
        catch
            error:LoopReason ->
                LoopBinErr = list_to_binary("Internal lux error: " ++
                                                file:format_error(LoopReason)),
                io:format("~s\n~p\n", [LoopBinErr, erlang:get_stacktrace()]),
                stop(C2, error, LoopBinErr)
        end
    catch
        error:InitReason ->
            InitBinErr = list_to_binary(FlatExec ++ ": " ++
                                            file:format_error(InitReason)),
            io:format("~s\n~p\n", [InitBinErr, erlang:get_stacktrace()]),
            stop(C2, error, InitBinErr)
    end.

open_logfile(C, Slogan) ->
    LogFile = C#cstate.log_prefix ++ "." ++ Slogan ++ ".log",
    case file:open(LogFile, [write, raw]) of
        {ok, Fd} ->
            {LogFile, Fd};
        {error, FileReason} ->
            String = file:format_error(FileReason),
            BinErr = list_to_binary(["Failed to open logfile: ", LogFile,
                                     " -> ", String]),
            io:format("~s\n~p\n", [BinErr, erlang:get_stacktrace()]),
            stop(C, error, BinErr)
    end.

choose_exec(C) ->
    case C#cstate.shell_wrapper of
        undefined -> {C#cstate.shell_cmd, C#cstate.shell_args};
        Wrapper   -> {Wrapper, [C#cstate.shell_cmd | C#cstate.shell_args]}
    end.

shell_loop(#cstate{idle_count = IdleCount} = C, OrigC) ->
    {C2, ProgressStr} =
        case IdleCount of
            0 ->
                {C, "."};
            1 ->
                {C#cstate{idle_count = IdleCount+1},
                 integer_to_list((C#cstate.latest_cmd)#cmd.lineno) ++ "?"};
            _ ->
                {C, "?"}
        end,
    lux_utils:progress_write(C2#cstate.progress, ProgressStr),
    C3 = expect_more(C2),
    C4 = shell_wait_for_event(C3, OrigC),
    shell_loop(C4, OrigC).

shell_wait_for_event(#cstate{name = _Name, port = Port} = C, OrigC) ->
    IdleThreshold = 3*1000,
    Timeout =
        if
            C#cstate.expected =:= undefined ->
                infinity;
            C#cstate.timer =/= undefined ->
                IdleThreshold;
            true ->
                IdleThreshold
        end,
    receive
        {ping, From, When} ->
            C2 = opt_ping_reply(C, From, When),
            shell_wait_for_event(C2, OrigC);
        {block, From} ->
            %% io:format("\nBLOCK ~s\n", [C#cstate.name]),
            receive
                {unblock, From} ->
                    %% io:format("\nUNBLOCK ~s\n", [C#cstate.name]),
                    shell_wait_for_event(C, OrigC);
                {ping, From, When} ->
                    C2 = opt_ping_reply(C, From, When),
                    shell_wait_for_event(C2, OrigC);
                {'DOWN', _, process, Pid, Reason}
                  when Pid =:= C#cstate.parent ->
                    Waste = flush_port(C,
                                       C#cstate.flush_timeout,
                                       C#cstate.actual),
                    clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
                    close_logs_and_exit(C, Reason);
                {'EXIT', Port, _PosixCode} ->
                    C2 = C#cstate{state_changed = true,
                                  no_more_output = true,
                                  events = save_event(C, recv, shell_exit)},
                    shell_wait_for_event(C2, OrigC);
                {shutdown = Data, _From} ->
                    stop(C, shutdown, Data)
            end;
        {progress, _From, Level} ->
            shell_wait_for_event(C#cstate{progress = Level}, OrigC);
        {change_mode, _From, Mode, Cmd}
          when Mode =:= resume; Mode =:= suspend ->
            C2 = C#cstate{latest_cmd = Cmd},
            case Mode of
                suspend ->
                    clog(C2, Mode, "", []);
                resume ->
                    clog(C2, Mode, "(idle since line ~p)", [Cmd#cmd.lineno])
            end,
            expect_more(C2#cstate{mode = Mode, waiting = false});
        {expand_vars, From, Bin, MissingVar} ->
            Res =
                try
                    {ok, expand_vars(C, Bin, MissingVar)}
                catch
                    throw:{no_such_var, BadName} ->
                        {no_such_var, BadName}
                end,
            From ! {expand_vars, self(), Res},
            C;
        {sync_eval, From, Cmd} ->
            assert_msg(C, Cmd, From),
            C2 =
                case C#cstate.no_more_input of
                    true  -> exit({no_more_input, Cmd});
                    false -> C#cstate{waiting = false}
                end,
            shell_eval(C2, Cmd, OrigC);
        {global, _From, VarVal} ->
            clog(C, global, "\"~s\"", [VarVal]),
            C#cstate{dict = [VarVal | C#cstate.dict]};
        {macro_dict, _From, MacroDict} ->
            %% clog(C, macro_dict, "\"~s\"", [MacroDict]),
            C#cstate{macro_dict = MacroDict};
        {shutdown = Data, _From} ->
            stop(C, shutdown, Data);
        {end_of_script, _From} ->
            clog(C, 'end', "of script", []),
            C#cstate{no_more_input = true, mode = resume};
        {Port, {data, Data}} ->
            Progress = C#cstate.progress,
            lux_utils:progress_write(Progress, ":"),
            %% Read all available data
            NewData = flush_port(C, C#cstate.poll_timeout, Data),
            lux_log:safe_write(Progress,
                               C#cstate.log_fun,
                               C#cstate.stdout_log_fd,
                               NewData),
            OldData = C#cstate.actual,
            Actual = <<OldData/binary, NewData/binary>>,
            C2 = C#cstate{state_changed = true,
                          actual = Actual,
                          events = save_event(C, recv, NewData)},
            C3 = match_success_pattern(C2, Actual),
            match_fail_pattern(C3, Actual);
        timeout ->
            C#cstate{state_changed = true,
                     timed_out = true,
                     events = save_event(C, recv, timeout)};
        {wake_up, Secs} ->
            clog(C, wake, "up (~p seconds)", [Secs]),
            C#cstate{mode = resume};
        {'EXIT', Port, _PosixCode} ->
            C#cstate{state_changed = true,
                     no_more_output = true,
                     events = save_event(C, recv, shell_exit)};
        {'DOWN', _, process, Pid, Reason} ->
            if
                Pid =:= C#cstate.parent ->
                    Waste = flush_port(C,
                                       C#cstate.flush_timeout,
                                       C#cstate.actual),
                    clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
                    close_logs_and_exit(C, Reason);
                true ->
                    %% Ignore
                    C
            end;
        Unexpected ->
            io:format("[shell ~s] got ~p\n",
                      [C#cstate.name, Unexpected]),
            C
    after multiply(C, Timeout) ->
        C#cstate{idle_count = C#cstate.idle_count + 1}
    end.

opt_ping_reply(C, From, When) when C#cstate.wait_for_expect =:= undefined ->
    case When of
        flush ->
            flush_logs(C),
            ping_reply(C, From);
        immediate ->
            ping_reply(C, From);
        wait_for_expect when C#cstate.expected =:= undefined ->
            ping_reply(C, From);
        wait_for_expect ->
            C#cstate{wait_for_expect = From}
    end.

opt_late_ping_reply(#cstate{wait_for_expect = From} = C) ->
    if
        is_pid(From) ->
            ping_reply(C, From);
        From =:= undefined ->
            C
    end.

ping_reply(C, From) ->
    From ! {pong, self()},
    C#cstate{wait_for_expect = undefined}.

assert_msg(C, Cmd, From) when From =/= C#cstate.parent ->
    %% Assert - invalid sender
    Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
    close_logs_and_exit(C, {internal_error, unexpected_sender,
                       From, C#cstate.parent, Cmd});
assert_msg(C, Cmd, _From) ->
    %% Assert - no send nor expect while expect is set
    if
        C#cstate.expected =:= undefined ->
            ok;
        C#cstate.expected =/= undefined,
        Cmd#cmd.type =/= expect,
        Cmd#cmd.type =/= send_lf,
        Cmd#cmd.type =/= send ->
            ok;
        true ->
            Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
            clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
            close_logs_and_exit(C, {internal_error, unexpected_msg,
                               Cmd, C#cstate.expected})
    end.

shell_eval(#cstate{name = Name} = C0,
           #cmd{type = Type, arg = Arg} = Cmd, OrigC) ->
    C = C0#cstate{latest_cmd = Cmd},
    case Type of
        variable ->
            try
                {Scope, Var, Val} = Arg,
                Val2 = expand_vars(C, Val, error),
                VarVal = lists:flatten([Var, $=, Val2]),
                clog(C, Scope, "\"~s\"", [ VarVal]),
                case Scope of
                    my ->
                        C#cstate.parent ! {my, self(), Name, VarVal},
                        C#cstate{macro_dict = [VarVal | C#cstate.macro_dict]};
                    local ->
                        C#cstate{dict = [VarVal | C#cstate.dict]};
                    global ->
                        C#cstate.parent ! {global, self(), Name, VarVal},
                        C#cstate{dict = [VarVal | C#cstate.dict]}
                end
            catch
                throw:{no_such_var, BadName} ->
                    BinErr = list_to_binary(["Variable $", BadName,
                                             " is not set"]),
                    %% io:format("~s\n~p\n", [BinErr, erlang:get_stacktrace()]),
                    stop(C, error, BinErr)
            end;
        send_lf when is_binary(Arg) ->
            send_to_port(C, <<Arg/binary, "\n">>);
        send when is_binary(Arg) ->
            send_to_port(C, Arg);
        expect when Arg =:= shell_exit ->
            clog(C, expect, "~p", [Arg]),
            C2 = start_timer(C),
            C2#cstate{state_changed = true,
                      expected = Cmd};
        expect when Arg =:= reset ->
            %% Reset output buffer
            C2 = cancel_timer(C),
            Waste = flush_port(C2, C2#cstate.flush_timeout, C#cstate.actual),
            clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
            clog(C, output, "reset", []),
            C2#cstate{state_changed = true,
                      actual = <<>>,
                      events = save_event(C, recv, Waste)};
        expect ->
            {C2, Cmd2, RegExp2} = compile_regexp(C, Arg),
            clog(C2, expect, "\"~s\"", [lux_utils:to_string(RegExp2)]),
            C3 = start_timer(C2),
            C3#cstate{state_changed = true,
                      expected = Cmd2};
        fail ->
            {C2, Cmd2, RegExp2} = compile_regexp(C, Arg),
            clog(C2, fail, "pattern ~p", [lux_utils:to_string(RegExp2)]),
            C3 = C2#cstate{state_changed = true,
                           fail = Cmd2},
            match_fail_pattern(C3, C3#cstate.actual);
        success ->
            {C2, Cmd2, RegExp2} = compile_regexp(C, Arg),
            clog(C2, success, "pattern ~p",
                 [lux_utils:to_string(RegExp2)]),
            C3 = C2#cstate{state_changed = true,
                           success = Cmd2},
            match_success_pattern(C3, C3#cstate.actual);
        sleep ->
            Secs = parse_int(C, Arg, Cmd),
            clog(C, sleep, "(~p seconds)", [Secs]),
            Progress = C#cstate.progress,
            Self = self(),
            WakeUp = {wake_up, Secs},
            Sleeper = spawn_link(fun() ->
                                         sleep_walker(Progress, Self, WakeUp)
                                 end),
            erlang:send_after(timer:seconds(Secs), Sleeper, WakeUp),
            C#cstate{mode = suspend};
        progress when is_list(Arg) ->
            try
                String = expand_vars(C, Arg, error),
                clog(C, progress, "\"~s\"", [String]),
                io:format("~s", [String]),
                C
            catch
                throw:{no_such_var, BadName} ->
                    BinErr = list_to_binary(["Variable $", BadName,
                                             " is not set"]),
                    stop(C, error, BinErr)
            end;
        change_timeout ->
            Millis =
                case Arg of
                    "" ->
                        OrigC#cstate.timeout;
                    "infinity" ->
                        infinity;
                    SecsStr ->
                        Secs = parse_int(C, SecsStr, Cmd),
                        timer:seconds(Secs)
                end,
            case Millis of
                infinity ->
                    clog(C, change, "expect timeout to infinity", []);
                _ ->
                    clog(C, change, "expect timeout to ~p seconds",
                         [Millis div timer:seconds(1)])
            end,
            C#cstate{timeout = Millis};
        cleanup ->
            C2 = cancel_timer(C),
            case C#cstate.fail of
                undefined -> ok;
                _         -> clog(C2, fail, "pattern reset", [])
            end,
            case C#cstate.success of
                undefined  -> ok;
                _          -> clog(C2, success, "pattern reset", [])
            end,
            C3 = C2#cstate{expected = undefined,
                           fail = undefined,
                           success = undefined},
            opt_late_ping_reply(C3);
        Unexpected ->
            Err = io_lib:format("[shell ~s] got cmd with type ~p ~p\n",
                                [Name, Unexpected, Arg]),
            stop(C, error, list_to_binary(Err))
    end.

send_to_port(C, RawData) ->
    try
        Data = expand_vars(C, RawData, error),
        lux_log:safe_write(C#cstate.progress, C#cstate.log_fun,
                           C#cstate.stdin_log_fd, Data),
        C2 = C#cstate{events = save_event(C, send, Data)},
        try
            true = port_command(C2#cstate.port, Data),
            C2
        catch
            error:_Reason ->
                receive
                    {'EXIT', Port, _PosixCode}
                      when Port =:= C2#cstate.port ->
                        C2#cstate{state_changed = true,
                                  no_more_output = true,
                                  events = save_event(C2, recv, shell_exit)}
                after 0 ->
                        C2
                end
        end
    catch
        throw:{no_such_var, BadName} ->
            VarErr = list_to_binary(["Variable $", BadName,
                                     " is not set"]),
            stop(C, error, VarErr)
    end.

parse_int(C, Chars, Cmd) ->
    Chars2 = expand_vars(C, Chars, error),
    try
        list_to_integer(Chars2)
    catch
        error:_ ->
            BinErr = list_to_binary(["Syntax error at line ",
                                     integer_to_list(Cmd#cmd.lineno),
                                     ": '", Chars2, "' integer expected"]),
            stop(C, error, BinErr)
    end.

sleep_walker(Progress, ReplyTo, WakeUp) ->
    %% Snore each second
    lux_utils:progress_write(Progress, "z"),
    receive
        WakeUp ->
            ReplyTo ! WakeUp,
            unlink(ReplyTo)
    after 1000 ->
            sleep_walker(Progress, ReplyTo, WakeUp)
    end.

want_more(#cstate{mode = Mode, expected = Expected, waiting = Waiting}) ->
    Mode =:= resume andalso Expected =:= undefined andalso not Waiting.

expect_more(C) ->
    C2 = expect(C),
    case want_more(C2) of
        true ->
            C2#cstate.parent ! {more, self(), C2#cstate.name},
            C2#cstate{waiting = true};
        false ->
            C2
    end.

expect(#cstate{state_changed = false,
               no_more_input = true,
               timer = undefined,
               expected = Expected} = C) ->
    %% End of script
    case Expected of
        undefined -> stop(C, success, end_of_script);
        _         -> start_timer(C)
    end;
expect(#cstate{state_changed = false} = C) ->
    %% Nothing has changed
    C;
expect(#cstate{state_changed = true,
               no_more_input = NoMoreInput,
               no_more_output = NoMoreOutput,
               expected = Expected,
               actual = Actual,
               timed_out = TimedOut,
               timer = Timer} = C0) ->
    %% Something has changed
    C = C0#cstate{state_changed = false},
    case Expected of
        undefined when NoMoreInput, Timer =:= undefined ->
            %% Success
            stop(C, success, end_of_script);
        undefined ->
            %% Nothing to wait for
            cancel_timer(C);
        #cmd{arg = Arg} ->
            if
                TimedOut ->
                    %% timeout - waited enough for more input
                    Earlier = C#cstate.timer_started_at,
                    clog(C, timer, "fail (~p seconds)",
                         [timer:now_diff(erlang:now(), Earlier) div 1000000]),
                    stop(C, fail, timeout);
                NoMoreOutput, Arg =:= shell_exit ->
                    %% Successful match of end of file (port program closed)
                    C2 = cancel_timer(C),
                    clog(C2, match, "\"shell_exit\"", []),
                    stop(C, shutdown, shell_exit);
                NoMoreOutput ->
                    %% Got end of file while waiting for more data
                    stop(C, fail, shell_exit);
                Arg =:= shell_exit ->
                    %% Still waiting for end of file
                    C;
                true ->
                    %% Waiting for more data
                    case match(Actual, C#cstate.expected) of
                        {match, Matches}  ->
                            %% Successful match
                            C2 = cancel_timer(C),
                            {Actual2, SubMatches} =
                                split_submatches(C2, Matches, Actual, ""),
                            case NoMoreInput of
                                true ->
                                    %% End of input
                                    stop(C2, success, end_of_script);
                                false ->
                                    C3 = C2#cstate{expected = undefined,
                                                   actual = Actual2,
                                                   submatches = SubMatches},
                                    opt_late_ping_reply(C3)
                            end;
                        nomatch ->
                            %% Main pattern does not match
                            %% Wait for more input
                            C
                    end
            end
    end.

match(Actual, Cmd) ->
    case Cmd of
        undefined ->
            nomatch;
        #cmd{type = Type, arg = Arg} ->
            if
                Type =:= expect; Type =:= fail; Type =:= success ->
                    case Arg of
                        {verbatim, Expected} ->
                            %% io:format("\nbinary:match(~p,\n"
                            %%           "            ~p,\n"
                            %%           "            ~p).\n",
                            %%           [Actual, Expected, []]),
                            lux_utils:verbatim_match(Actual, Expected);
                        {mp, _RegExp, MP} ->
                            Opts = [{capture, all, index}, notempty],
                            %% io:format("\nre:run(~p,\n"
                            %%           "       ~p,\n"
                            %%           "       ~p).\n",
                            %%           [Actual, _RegExp, Opts]),
                            re:run(Actual, MP, Opts)
                    end
            end
    end.

match_fail_pattern(C, Actual) ->
    case match(Actual, C#cstate.fail) of
        {match, Matches} ->
            C2 = cancel_timer(C),
            C3 = prepare_stop(C2, Actual, Matches, "fail pattern matched "),
            stop(C3, fail, fail_pattern_matched);
        nomatch ->
            C
    end.

match_success_pattern(C, Actual) ->
    case match(Actual, C#cstate.success) of
        {match, Matches} ->
            C2 = cancel_timer(C),
            C3 = prepare_stop(C2, Actual, Matches, "success pattern matched "),
            stop(C3, success, success_pattern_matched);
        nomatch ->
            C
    end.

split_submatches(C, [{First, TotLen} | Matches], Actual, Context) ->
    {Consumed, Rest} = split_binary(Actual, First+TotLen),
    {Skip, Match} = split_binary(Consumed, First),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Skip)]),
    clog(C, match, "~s\"~s\"", [Context, lux_utils:to_string(Match)]),
    Extract =
        fun(PosLen) ->
                case PosLen of
                    {-1,0} -> nosubmatch;
                    _      -> binary:part(Actual, PosLen)
                end
        end,
    SubBins = lists:map(Extract, Matches),
    {Rest, SubBins}.

prepare_stop(C, Actual, [{First, TotLen} | _], Context) ->
    {Skip, Rest} = split_binary(Actual, First),
    {Match, _Actual2} = split_binary(Rest, TotLen),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Skip)]),
    clog(C, match, "~s\"~s\"", [Context, lux_utils:to_string(Match)]),
    C2 = C#cstate{expected = undefined,
                  actual = Match,
                  submatches = []},
    opt_late_ping_reply(C2).

flush_port(#cstate{port = Port} = C, Timeout, Acc) ->
    case do_flush_port(Port, 0, 0, Acc) of
        {0, Acc2} when Timeout =/= 0 ->
            {_N, Acc3} = do_flush_port(Port, multiply(C, Timeout), 0, Acc2),
            Acc3;
        {_N, Acc2} ->
            Acc2
    end.

do_flush_port(Port, Timeout, N, Acc) ->
    receive
        {Port, {data, Data}} ->
            do_flush_port(Port, Timeout, N+1, <<Acc/binary, Data/binary>>)
    after Timeout ->
            {N, Acc}
    end.

start_timer(#cstate{timer = undefined, timeout = infinity} = C) ->
    clog(C, timer, "start (infinity)", []),
    C#cstate{timer = infinity, timer_started_at = erlang:now()};
start_timer(#cstate{timer = undefined} = C) ->
    Seconds = C#cstate.timeout div timer:seconds(1),
    Multiplier = C#cstate.multiplier / 1000,
    clog(C, timer, "start (~p seconds * ~.3f)", [Seconds, Multiplier]),
    Timer = safe_send_after(C, C#cstate.timeout, self(), timeout),
    C#cstate{timer = Timer, timer_started_at = erlang:now()};
start_timer(#cstate{} = C) ->
    clog(C, timer, "keep", []),
    C.

cancel_timer(#cstate{timer = undefined} = C) ->
    C;
cancel_timer(#cstate{timer = Timer, timer_started_at = Earlier} = C) ->
    clog(C, timer, "cancel (~p seconds)",
        [timer:now_diff(erlang:now(), Earlier) div 1000000]),
    case Timer of
        infinity -> ok;
        _        -> erlang:cancel_timer(Timer)
    end,
    C#cstate{idle_count = 0,
             timer = undefined,
             timer_started_at = undefined}.

compile_regexp(C, reset) ->
    {C, undefined, reset};
compile_regexp(C, shell_exit) ->
    {C, undefined, shell_exit};
compile_regexp(C, {verbatim, Verbatim}) ->
    {C, C#cstate.latest_cmd, Verbatim};
compile_regexp(C, {mp, RegExp, _MP}) ->
    {C, C#cstate.latest_cmd, RegExp};
compile_regexp(C, {template, Template}) ->
    Verbatim = expand_vars(C, Template, error),
    patch_latest(C, {verbatim, Verbatim}, Verbatim);
compile_regexp(C, {regexp, RegExp}) ->
    try
        RegExp2 = expand_vars(C, RegExp, error),
        RegExp3 = normalize_newlines(RegExp2),
        %% io:format("REGEXP: ~p ~p\n", [RegExp, RegExp3]),
        Opts = [multiline, {newline, anycrlf}],
        case re:compile(RegExp3, Opts) of
            {ok, MP2} ->
                patch_latest(C, {mp, RegExp3, MP2}, RegExp3);
            {error, {Reason, _Pos}} ->
                BinErr = list_to_binary(["Syntax error: ", Reason,
                                         " in regexp '", RegExp3, "'"]),
                stop(C, error, BinErr)
        end
    catch
        throw:{no_such_var, BadName} ->
            BinErr2 = list_to_binary(["Syntax error: Variable $",
                                      BadName, " is not set in regexp '",
                                      RegExp, "'"]),
            stop(C, error, BinErr2)
    end.

patch_latest(C, NewArg, Expect) ->
    Cmd = C#cstate.latest_cmd,
    Cmd2 = Cmd#cmd{arg = NewArg},
    {C#cstate{latest_cmd = Cmd2}, Cmd2, Expect}.

normalize_newlines(Bin) ->
    re:replace(Bin, <<"\n">>, <<"[\r\n]+">>, [global, {return, binary}]).

stop(C, Outcome, Actual) when is_binary(Actual); is_atom(Actual) ->
    Cmd = C#cstate.latest_cmd,
    Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
    clog(C, stop, "~p", [Outcome]),
    Events = lists:reverse(C#cstate.events),
    {Outcome2, Extra} =
        if
            Outcome =:= fail, Actual =:= fail_pattern_matched ->
                {Outcome, element(1, (C#cstate.fail)#cmd.arg)};
            Outcome =:= success, Actual =:= success_pattern_matched ->
                {Outcome, element(1, (C#cstate.success)#cmd.arg)};
            Outcome =:= error ->
                {fail, Actual};
            Outcome =:= shutdown ->
                {Outcome, undefined};
            true ->
                {Outcome, undefined}
        end,
    case Cmd of
        #cmd{type = expect, arg = {verbatim, Expected}} ->
            ok;
        #cmd{type = expect, arg = {mp, Expected, _MP}} ->
            ok;
        #cmd{} ->
            Expected = <<"">>
    end,
    Res =  #result{outcome = Outcome2,
                   name = C#cstate.name,
                   lineno = Cmd#cmd.lineno,
                   expected = Expected,
                   extra = Extra,
                   actual = Actual,
                   rest = C#cstate.actual,
                   events = Events},
    C2 = close_logs(C),
    C2#cstate.parent ! {stop, self(), Res},
    if
        Outcome =:= shutdown ->
            close_port_and_exit(C2, Outcome);
        Outcome =:= error ->
            close_port_and_exit(C2, {error, Actual});
        true ->
            %% Wait for potential cleanup to be run
            %% before we close the port
            ping_and_wait(C2)
    end.

close_logs_and_exit(C, Reason) ->
    C2 = close_logs(C),
    close_port_and_exit(C2, Reason).

close_port_and_exit(C, Reason) ->
    catch port_close(C#cstate.port),
    exit(Reason).

ping_and_wait(C) ->
    C2 = opt_late_ping_reply(C),
    wait_for_down(C2).

close_logs(#cstate{stdin_log_fd = InFd, stdout_log_fd = OutFd} = C) ->
    Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
    catch file:close(element(2, InFd)),
    catch file:close(element(2, OutFd)),
    C#cstate{log_fun = closed,
             event_log_fd = closed,
             stdin_log_fd = closed,
             stdout_log_fd = closed}.

flush_logs(#cstate{event_log_fd = {_,EventFd},
                   stdin_log_fd = {_,InFd},
                   stdout_log_fd = {_,OutFd}}) ->
    file:sync(EventFd),
    file:sync(InFd),
    file:sync(OutFd),
    ok;
flush_logs(#cstate{event_log_fd = closed,
                   stdin_log_fd = closed,
                   stdout_log_fd = closed}) ->
    ok.

wait_for_down(C) ->
    receive
        {ping, From, When} ->
            C2 = opt_ping_reply(C, From, When),
            wait_for_down(C2);
        {'DOWN', _, process, Pid, Reason} when Pid =:= C#cstate.parent ->
            close_port_and_exit(C, Reason);
        {'EXIT', Port, _PosixCode} when Port =:= C#cstate.port ->
            C2 = C#cstate{state_changed = true,
                          no_more_output = true,
                          events = save_event(C, recv, shell_exit)},
            wait_for_down(C2)
    end.

save_event(#cstate{latest_cmd = Cmd, events = Events} = C, Op, Data) ->
    clog(C, Op, "\"~s\"", [lux_utils:to_string(Data)]),
    [{Cmd#cmd.lineno, Op, Data} | Events].

safe_send_after(State, Timeout, Pid, Msg) ->
    case multiply(State, Timeout) of
        infinity   -> infinity;
        NewTimeout -> erlang:send_after(NewTimeout, Pid, Msg)
    end.

multiply(#cstate{multiplier = Factor}, Timeout) ->
    case Timeout of
        infinity ->
            infinity;
        _ ->
            lux_utils:multiply(Timeout, Factor)
    end.

clog(#cstate{progress = Progress, log_fun = LogFun, event_log_fd = Fd,
             name = Shell, latest_cmd = Cmd}, Op, Format, Args) ->
    E = {event, Cmd#cmd.lineno, Shell, Op, Format, Args},
    lux_log:write_event(Progress, LogFun, Fd, E).

expand_vars(#cstate{submatches   = SubMatches,
                    macro_dict   = MacroDict,
                    dict         = Dict,
                    builtin_dict = BuiltinDict,
                    system_dict  = SystemDict},
            Bin,
            MissingVar) ->
    Fun = fun(nosubmatch, {N, Acc}) ->
                  {N+1, Acc}; % Omit $N as its value is undefined
             (Sub, {N, Acc}) ->
                  List = [integer_to_list(N), $=,
                          binary_to_list(Sub)],
                  {N+1, [lists:flatten(List) | Acc]}
          end,
    {_,SubDict} = lists:foldl(Fun, {1, []}, SubMatches),
    Dicts = [SubDict, MacroDict, Dict, BuiltinDict, SystemDict],
    lux_utils:expand_vars(Dicts, Bin, MissingVar).
