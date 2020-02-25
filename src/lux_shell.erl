%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2020 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_shell).

-export([start_monitor/4, extract_regexp/1]).

-include("lux.hrl").

-define(match_fail, match_timeout).
-define(shell_exit, shell_exit).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client

start_monitor(I, Cmd, Name, ExtraLogs) ->
    OrigFile = I#istate.orig_file,
    Self = self(),
    Base = filename:basename(OrigFile),
    Prefix = filename:join([I#istate.case_log_dir, Base ++ "." ++ Name]),
    C = #cstate{orig_file = OrigFile,
                parent = Self,
                name = Name,
                start_reason = I#istate.cleanup_reason,
                latest_cmd = Cmd,
                cmd_stack = I#istate.cmd_stack,
                progress = I#istate.progress,
                log_fun = I#istate.log_fun,
                event_log_fd = I#istate.event_log_fd,
                log_prefix = Prefix,
                multiplier = I#istate.multiplier,
                poll_timeout = I#istate.poll_timeout,
                flush_timeout = I#istate.flush_timeout,
                match_timeout = I#istate.default_timeout,
                case_timeout = I#istate.case_timeout,
                suite_timeout = I#istate.suite_timeout,
                shell_wrapper = I#istate.shell_wrapper,
                shell_cmd = I#istate.shell_cmd,
                shell_args = I#istate.shell_args,
                shell_prompt_cmd = I#istate.shell_prompt_cmd,
                shell_prompt_regexp = I#istate.shell_prompt_regexp,
                loop_stack = I#istate.loop_stack,
                debug_level = I#istate.debug_level,
                emit_timestamp = I#istate.emit_timestamp},
    {Pid, Ref} = spawn_monitor(fun() -> init(C, ExtraLogs) end),
    receive
        {started, Pid, Logs, NewVarVals} ->
            Shell = #shell{name = Name,
                           pid = Pid,
                           ref = Ref,
                           health = alive,
                           vars = NewVarVals ++ I#istate.global_vars,
                           match_timeout = C#cstate.match_timeout},
            I2 = I#istate{active_shell = Shell,
                          active_name = Name},
            {ok, I2#istate{logs = I2#istate.logs ++ [Logs]}};
        {'DOWN', _, process, Pid, Reason} ->
            {error, I, Pid, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server

send_reply(C, To, Msg) ->
    TraceFrom = C#cstate.name,
    TraceTo =
        if
            To =:= C#cstate.parent -> 'case';
            true                   -> To
        end,
    ?TRACE_ME(50, TraceFrom, TraceTo, element(1, Msg), [Msg]),
    To ! Msg.

init(C, ExtraLogs) when is_record(C, cstate) ->
    process_flag(trap_exit, true),
    Name = C#cstate.name,
    ?TRACE_ME(50, 'case', Name, 'spawn', []),
    {InFile, InFd} = open_logfile(C, "stdin"),
    {OutFile, OutFd} = open_logfile(C, "stdout"),
    C2 = C#cstate{stdin_log_fd = {false, InFd},
                  stdout_log_fd = {false, OutFd}},
    {Exec, Args} = choose_exec(C2),
    FlatExec = lists:flatten([Exec, [[" ", A] || A <- Args]]),
    Events = save_event(C2, start, FlatExec),
    StartReason = ?a2l(C#cstate.start_reason),
    PortEnv = [{"LUX_SHELLNAME", Name},
               {"LUX_START_REASON", StartReason},
               {"LUX_EXTRA_LOGS", ExtraLogs}],
               WorkDir = filename:dirname(C2#cstate.orig_file),
    Opts = [binary, stream, use_stdio, stderr_to_stdout, exit_status,
            {args, Args}, {cd, WorkDir}, {env, PortEnv}],
    try
        Port = open_port({spawn_executable, Exec}, Opts),
        NewVarVals = ["LUX_SHELLNAME=" ++ Name,
                      "LUX_START_REASON=" ++ StartReason],
        C3 = C2#cstate{port = Port,
                       events = Events},
        Parent = C3#cstate.parent,
        erlang:monitor(process, Parent),
        send_reply(C3, Parent,
                   {started, self(), {Name, InFile, OutFile}, NewVarVals}),
        try
            shell_loop(C3, C3)
        catch
            ?CATCH_STACKTRACE(error, LoopReason, LoopEST)
                LoopErrBin = ?l2b(?FF("INTERNAL LUX ERROR: ~999999p ~999999p",
                                      [LoopReason, LoopEST])),
                io:format("\n~s\n", [LoopErrBin]),
                stop(C2, error, LoopErrBin)
        end
    catch
        ?CATCH_STACKTRACE(error, InitReason, InitEST)
            FileErrStr = file:format_error(InitReason),
            InitErrBin = ?l2b([FlatExec, ": ", FileErrStr]),
            io:format("\n~s\n", [InitErrBin]),
            clog(C2, error, "INTERNAL LUX ERROR: \"~999999s\" ~999999p",
                 [FileErrStr, InitEST]),
            stop(C2, error, InitErrBin)
    end.

open_logfile(C, Slogan) ->
    LogFile = C#cstate.log_prefix ++ "." ++ Slogan ++ ".log",
    case file:open(LogFile, [write, raw]) of
        {ok, Fd} ->
            {LogFile, Fd};
        {error, FileReason} ->
            String = file:format_error(FileReason),
            BinErr = ?l2b(["Failed to open logfile: ", LogFile,
                           " -> ", String]),
            io:format("~s\n~p\n", [BinErr, ?stacktrace()]),
            stop(C, error, BinErr)
    end.

choose_exec(C) ->
    case C#cstate.shell_wrapper of
        undefined -> {C#cstate.shell_cmd, C#cstate.shell_args};
        Wrapper   -> {Wrapper, [C#cstate.shell_cmd | C#cstate.shell_args]}
    end.

shell_loop(C, OrigC) ->
    {C2, ProgressStr} = progress_token(C),
    Progress = debug_progress(C2),
    lux_utils:progress_write(Progress, ProgressStr),
    C3 = expect_more(C2),
    C4 = shell_wait_for_event(C3, OrigC),
    shell_loop(C4, OrigC).

progress_token(#cstate{idle_count = IdleCount} = C) ->
    case IdleCount of
        0 ->
            {C, "."};
        1 ->
            {C#cstate{idle_count = IdleCount+1},
             ?i2l((C#cstate.latest_cmd)#cmd.lineno) ++ "?"};
        _ ->
            {C, "?"}
    end.

shell_wait_for_event(#cstate{name = _Name} = C, OrigC) ->
    ?TRACE_ME2(40, C#cstate.name, wait_for_event, []),
    LoopTimeout = loop_timeout(C),
    receive
        {block, From} ->
            %% io:format("\nBLOCK ~s\n", [C#cstate.name]),
            block(C, From, OrigC);
        {debug_shell, From, What} ->
            debug_shell(C, From, What);
        {sync, From, When} ->
            C2 = opt_sync_reply(C, From, When),
            shell_wait_for_event(C2, OrigC);
        {adjust_stacks, From, When, IsRootLoop, NewCmd, CmdStack, Fun} ->
            C2 = adjust_stacks(C, From, When, IsRootLoop,
                               NewCmd, CmdStack, Fun),
            shell_wait_for_event(C2, OrigC);
        {change_mode, From, Mode, Cmd, CmdStack}
          when Mode =:= resume; Mode =:= suspend ->
            C2 = change_mode(C, From, Mode, Cmd, CmdStack),
            expect_more(C2);
        {progress, _From, Level} ->
            shell_wait_for_event(C#cstate{progress = Level}, OrigC);
        {eval, From, Cmd} ->
            dlog(C, ?dmore, "eval (got ~p)", [Cmd#cmd.type]),
            %% clog(C, got_more, "~p ~p", [C#cstate.mode, Cmd#cmd.type]),
            assert_eval(C, Cmd, From),
            shell_eval(C, Cmd);
        {shutdown = Data, _From} ->
            stop(C, shutdown, Data);
        {relax = Data, _From} ->
            stop_relax(C, relax, Data);
        {end_of_script, _From} ->
            clog(C, 'end', "of script", []),
            dlog(C, ?dmore,"mode=resume (end_of_script)", []),
            %% C#cstate{no_more_input = true, mode = suspend};
            stop_relax(C, success, end_of_script);
        {Port, {data, Data}} when Port =:= C#cstate.port ->
            flush_port2(C, C#cstate.poll_timeout, [Data]);
        ?match_fail ->
            C#cstate{state_changed = true,
                     timed_out = true,
                     events = save_event(C, recv, ?match_fail)};
        {wakeup, Secs} ->
            clog(C, wake, "up (~p seconds)", [Secs]),
            dlog(C, ?dmore,"mode=resume (wakeup)", []),
            undefined = C#cstate.expected, % Assert
            C2 = C#cstate{mode = resume, wakeup_ref = undefined},
            expect_more(C2);
        {Port, {exit_status, ExitStatus}} when Port =:= C#cstate.port ->
            C#cstate{state_changed = true,
                     no_more_output = true,
                     exit_status = ExitStatus,
                     events = save_event(C, recv, ?shell_exit)};
        {'EXIT', Port, _PosixCode}
          when Port =:= C#cstate.port,
               C#cstate.exit_status =/= undefined ->
            C;
        {'DOWN', _, process, Pid, Reason} ->
            if
                Pid =:= C#cstate.parent ->
                    interpreter_died(C, Reason);
                true ->
                    %% Ignore
                    C
            end;
        Unexpected ->
            ?TRACE_ME2(70, C#cstate.name, internal_error,
                       [{shell_got, Unexpected}]),
            clog(C, internal, "\"shell_got_msg ~p\"", [element(1, Unexpected)]),
            io:format("\nINTERNAL LUX ERROR: Shell got: ~p\n",
                      [Unexpected]),
            io:format("\nDEBUG(~p):\n\t~p\n",
                      [?LINE, process_info(self(), messages)]),
            C
    after LoopTimeout ->
            C#cstate{idle_count = C#cstate.idle_count + 1}
    end.

interpreter_down(C, Reason) ->
    TraceFrom = 'case',
    TraceTo = C#cstate.name,
    ?TRACE_ME(50, TraceFrom, TraceTo, 'DOWN', [{reason, Reason}]).

interpreter_died(C, Reason) ->
    interpreter_down(C, Reason),
    C2 = flush_port(C),
    clog_skip(C2, C2#cstate.actual),
    IE = {internal_error, interpreter_died, C#cstate.latest_cmd, Reason},
    close_and_exit(C2, {error, internal}, IE).

loop_timeout(C) ->
    IdleThreshold = lux_utils:multiply(timer:seconds(3), C#cstate.multiplier),
    if
        C#cstate.expected =:= undefined ->
            infinity;
        C#cstate.timer_ref =/= undefined ->
            IdleThreshold;
        true ->
            IdleThreshold
    end.

adjust_stacks(C, From, When, IsRootLoop, NewCmd, CmdStack, Fun) ->
    Fun(),
    send_reply(C, From, {adjust_stacks_ack, self()}),
    LoopStack = C#cstate.loop_stack,
    case {NewCmd#cmd.type, When} of
        {loop, before} when IsRootLoop ->
            Loop = #loop{mode = iterate, cmd = NewCmd},
            C#cstate{latest_cmd = NewCmd,
                     cmd_stack = CmdStack,
                     loop_stack = [Loop | LoopStack]};
        {loop, 'after'} when IsRootLoop,
                             (hd(LoopStack))#loop.mode =:= iterate ->
            C#cstate{latest_cmd = NewCmd,
                     cmd_stack = CmdStack,
                     loop_stack = tl(LoopStack)};
        {loop, 'after'} when IsRootLoop,
                             (hd(LoopStack))#loop.mode =:= break ->
            C#cstate{latest_cmd = NewCmd,
                     cmd_stack = CmdStack,
                     loop_stack = tl(LoopStack)};
        {loop, 'after'} when IsRootLoop ->
            %% endloop with pending break pattern
            Loop = hd(LoopStack),
            LoopCmd = Loop#loop.mode,
            {_, RegExp} = extract_regexp(LoopCmd#cmd.arg),
            {C2, ActualStop} =
                do_prepare_stop(C, RegExp,
                                <<?loop_break_pattern_mismatch>>),
            stop(C2, fail, ActualStop);
        _ ->
            C#cstate{latest_cmd = NewCmd,
                     cmd_stack = CmdStack}
    end.

change_mode(C, From, NewMode, Cmd, CmdStack) ->
    Reply = {change_mode_ack, self()},
    OldMode = C#cstate.mode,
    case NewMode of
        suspend when OldMode =:= resume ->
            clog(C, NewMode, "", []),
            dlog(C, ?dmore,"mode=suspend waiting=false (~p)",
                 [Cmd#cmd.type]),
            send_reply(C, From, Reply),
            C#cstate{mode = NewMode,
                     waiting = false};
        resume when OldMode =:= suspend ->
            NewC =
                C#cstate{mode = NewMode,
                         waiting = false,
                         latest_cmd = Cmd,
                         cmd_stack = CmdStack},
            dlog(NewC, ?dmore, "mode=resume waiting=false (~p)",
                 [Cmd#cmd.type]),
            clog(NewC, NewMode, "(idle since line ~p)",
                 [C#cstate.latest_cmd#cmd.lineno]),
            send_reply(NewC, From, Reply),
            NewC
    end.

block(C, From, OrigC) ->
    ?TRACE_ME2(40, C#cstate.name, block, []),
    receive
        {unblock, From} ->
            ?TRACE_ME2(40, C#cstate.name, unblock, []),
            %% io:format("\nUNBLOCK ~s\n", [C#cstate.name]),
            shell_wait_for_event(C, OrigC);
        {debug_shell, From, What} ->
            debug_shell(C, From, What);
        {sync, From, When} ->
            C2 = opt_sync_reply(C, From, When),
            block(C2, From, OrigC);
        {adjust_stacks, From, When, IsRootLoop, NewCmd, CmdStack, Fun} ->
            C2 = adjust_stacks(C, From, When, IsRootLoop,
                               NewCmd, CmdStack, Fun),
            block(C2, From, OrigC);
        {'DOWN', _, process, Pid, Reason} when Pid =:= C#cstate.parent ->
            interpreter_died(C, Reason);
        {Port, {exit_status, ExitStatus}} when Port =:= C#cstate.port ->
            C2 = C#cstate{state_changed = true,
                          no_more_output = true,
                          exit_status = ExitStatus,
                          events = save_event(C, recv, ?shell_exit)},
            shell_wait_for_event(C2, OrigC);
        {'EXIT', Port, _PosixCode}
          when Port =:= C#cstate.port,
               C#cstate.exit_status =/= undefined ->
            shell_wait_for_event(C, OrigC);
        {shutdown = Data, _From} ->
            stop(C, shutdown, Data);
        {relax = Data, _From} ->
            stop_relax(C, relax, Data)
    end.

debug_shell(OldC, _From, What) ->
    Name = OldC#cstate.name,
    NewC = OldC#cstate{debug = What},
    case {What, OldC#cstate.debug} of
        {disconnect, D} when D =/= disconnect ->
            lux_debug:format("\nDisconnect from shell ~p.\n", [Name]),
            NewC;
        {{connect, Mode}, disconnect} ->
            lux_debug:format("\nConnect to shell ~p in ~p mode.\n",
                             [Name, Mode]),
            opt_show_debug(NewC);
        {{send, reset}, _} ->
            reset_output_buffer(OldC, debugger);
        {{send, Data}, {connect,background}} ->
            C = send_to_port(OldC, Data),
            timer:sleep(500),
            C;
        {{send, Data}, _} ->
            send_to_port(OldC, Data);
        {display_stdout, _} ->
            lux_debug:format("\nDisplay output buffer for shell ~p:\n", [Name]),
            opt_show_debug(OldC);
        {disconnect, _} ->
            OldC;
        {{connect, _Mode}, _} ->
            OldC
    end.

opt_show_debug(C) ->
    case C#cstate.actual of
        <<>>   -> C;
        Actual -> show_debug(C, "recv", Actual)
    end.

show_debug(#cstate{debug={connect,Mode}} = C, Prefix, Data) ->
    Data2 =
        if
            is_atom(Data) -> ?a2b(Data);
            true          -> Data
        end,
    case Mode of
        background ->
            Lines = binary:split(Data2, <<"\n">>, [global]),
            Data3 = ?l2b([["\n", C#cstate.name, "(", Prefix, "): ", L] ||
                             L <- Lines]),
            lux_debug:format("~s\n", [Data3]);
        foreground ->
            lux_debug:format("~s", [Data2])
    end,
    C;
show_debug(C, _Prefix, _Data) ->
    C.

opt_sync_reply(C, From, When) when C#cstate.wait_for_expect =:= undefined ->
    case When of
        flush ->
            flush_logs(C),
            sync_reply(C, From);
        immediate ->
            sync_reply(C, From);
        wait_for_expect when C#cstate.expected =:= undefined ->
            sync_reply(C, From);
        wait_for_expect ->
            C#cstate{wait_for_expect = From}
    end.

opt_late_sync_reply(#cstate{wait_for_expect = From} = C) ->
    if
        is_pid(From) ->
            sync_reply(C, From);
        From =:= undefined ->
            C
    end.

sync_reply(C, From) ->
    send_reply(C, From, {sync_ack, self()}),
    C#cstate{wait_for_expect = undefined}.

assert_eval(C, Cmd, From) when From =/= C#cstate.parent ->
    %% Assert - sender must be parent
    C2 = flush_port(C),
    clog_skip(C2, C2#cstate.actual),
    IE = {internal_error, invalid_sender, Cmd, process_info(From)},
    close_and_exit(C2, {error, internal}, IE);
assert_eval(C, Cmd, _From)
  when C#cstate.no_more_output andalso
       Cmd#cmd.type =:= send ->
    ErrBin = <<"The command must be executed",
               " in context of a running shell">>,
    C2 = C#cstate{latest_cmd = Cmd},
    stop(C2, error, ErrBin);
assert_eval(C, _Cmd, _From) when C#cstate.no_more_input ->
    stop(C, fail, endshell);
assert_eval(C, _Cmd, _From) when C#cstate.expected =:= undefined ->
    ok;
assert_eval(_C, Cmd, _From) when Cmd#cmd.type =:= variable;
                                 Cmd#cmd.type =:= sleep;
                                 Cmd#cmd.type =:= debug;
                                 Cmd#cmd.type =:= fail;
                                 Cmd#cmd.type =:= success;
                                 Cmd#cmd.type =:= break;
                                 Cmd#cmd.type =:= progress;
                                 Cmd#cmd.type =:= change_timeout;
                                 Cmd#cmd.type =:= no_cleanup;
                                 Cmd#cmd.type =:= cleanup;
                                 Cmd#cmd.type =:= post_cleanup ->
    ok;
assert_eval(C, Cmd, _From) ->
    C2 = flush_port(C),
    clog_skip(C2, C2#cstate.actual),
    IE = {internal_error, invalid_type, Cmd,
          C2#cstate.expected,
          C2#cstate.mode, Cmd#cmd.type, (C2#cstate.latest_cmd)#cmd.type,
          C2#cstate.latest_cmd},
    close_and_exit(C2, {error, internal}, IE).

shell_eval(#cstate{name = Name} = C0,
           #cmd{type = Type, arg = Arg} = Cmd) ->
    dlog(C0, ?dmore,"waiting=false (eval ~p)", [Cmd#cmd.type]),
    C = C0#cstate{latest_cmd = Cmd, waiting = false},
    case Type of
        send ->
            true = is_binary(Arg), % Assert
            send_to_port(C, Arg);
        expect when Arg =:= reset ->
            reset_output_buffer(C, script);
        expect when element(1, Arg) =:= endshell ->
            single = element(2, Arg), % Assert
            {ExpectTag, RegExp} = extract_regexp(Arg),
            clog(C, ExpectTag, "\"endshell retcode=~s\"",
                 [lux_utils:to_string(RegExp)]),
            dlog(C, ?dmore, "expected=regexp (~p)", [element(1, Arg)]),
            C2 = start_timer(C),
            C2#cstate{state_changed = true, expected = Cmd};
        expect when element(2, Arg) =:= expect_add orelse
                    element(2, Arg) =:= expect_add_strict ->
            %% Add alternate regexp
            Tag = element(2, Arg),
            {_, RegExp} = extract_regexp(Arg),
            clog(C, Tag, "\"~s\"", [lux_utils:to_string(RegExp)]),
            dlog(C, ?dmore, "expected=regexp (~p)", [Tag]),
            PreExpected = C#cstate.pre_expected,
            case PreExpected of
                [H|_T] when element(2, H#cmd.arg) =/= Tag ->
                    Err = ?FF("Illegal syntax: "
                              "?+ cannot be mixed with ?++\n", []),
                    stop(C, error, ?l2b(Err));
                _ ->
                    C#cstate{pre_expected = [Cmd | PreExpected]}
            end;
        expect when element(2, Arg) =:= multi andalso
                    C#cstate.pre_expected =/= [] ->
            %% Multiple regexps
            Prev = hd(C#cstate.pre_expected),
            PrevTag = element(2, Prev#cmd.arg),
            {_, RegExp} = extract_regexp(Arg),
            clog(C, PrevTag, "\"~s\"", [lux_utils:to_string(RegExp)]),
            PreExpected = C#cstate.pre_expected,
            {ok, NewCmd, NewRegExp} =
                rebuild_multi_regexps(C, [Cmd | PreExpected]),
            clog(C, perms, "\"~s\"", [lux_utils:to_string(NewRegExp)]),
            dlog(C, ?dmore, "expected=regexp (expect)", []),
            C2 = start_timer(C),
            C2#cstate{latest_cmd = NewCmd,
                      state_changed = true,
                      expected = NewCmd,
                      pre_expected = []};
        expect when C#cstate.expected =:= undefined andalso
                    C#cstate.pre_expected =:= [] ->
            %% Single regexp
            true = lists:member(element(2, Arg), [single,multi]), % Assert
            {ExpectTag, RegExp} = extract_regexp(Arg),
            clog(C, ExpectTag, "\"~s\"", [lux_utils:to_string(RegExp)]),
            dlog(C, ?dmore, "expected=regexp (expect)", []),
            C2 = start_timer(C),
            C2#cstate{state_changed = true, expected = Cmd};
        expect when element(2, Arg) =:= single andalso
                    C#cstate.pre_expected =/= [] ->
            %% Single regexp
            Err = ?FF("Illegal syntax: Pending ?+."
                      " Must be ended with ?.\n", []),
            stop(C, error, ?l2b(Err));
        fail ->
            PatCmd =
                if
                    Arg =:= reset -> undefined;
                    true          -> Cmd
                end,
            {_, RegExp} = extract_regexp(Arg),
            clog(C, fail, "pattern ~p", [lux_utils:to_string(RegExp)]),
            Pattern = #pattern{cmd = PatCmd, cmd_stack = C#cstate.cmd_stack},
            C#cstate{state_changed = true, fail = Pattern};
        success ->
            PatCmd =
                if
                    Arg =:= reset -> undefined;
                    true          -> Cmd
                end,
            {_, RegExp} = extract_regexp(Arg),
            clog(C, success, "pattern ~p", [lux_utils:to_string(RegExp)]),
            Pattern = #pattern{cmd = PatCmd,
                               cmd_stack = C#cstate.cmd_stack},
            C#cstate{state_changed = true, success = Pattern};
        break ->
            {_, RegExp} = extract_regexp(Arg),
            clog(C, break, "pattern ~p", [lux_utils:to_string(RegExp)]),
            [Loop | LoopStack] = C#cstate.loop_stack,
            Loop2 =
                if
                    Loop#loop.mode =:= break ->
                        Loop; % Ignore
                    Arg =:= reset ->
                        Loop#loop{mode = iterate};
                    true ->
                        Loop#loop{mode = Cmd}
                end,
            C#cstate{state_changed = true, loop_stack = [Loop2|LoopStack]};
        sleep ->
            Secs = Arg,
            clog(C, sleep, "(~p seconds)", [Secs]),
            true = is_integer(Secs), % Assert
            undefined = C#cstate.expected, % Assert
            Progress = C#cstate.progress,
            Self = self(),
            WakeUpMsg = {wakeup, Secs},
            Fun = fun() -> sleep_walker(Progress, Self, WakeUpMsg) end,
            Sleeper = spawn_link(Fun),
            T = Secs * ?ONE_SEC,
            M = ?ONE_SEC,
            WakeupRef = lux_utils:send_after(T, M, Sleeper, WakeUpMsg),
            dlog(C, ?dmore,"mode=suspend (sleep)", []),
            C#cstate{mode = suspend, wakeup_ref = WakeupRef};
        progress ->
            true = is_list(Arg), % Assert
            String = Arg,
            lux_utils:progress_write(C#cstate.progress, dequote(String)),
            C#cstate{events = save_event(C, progress, String)};
        change_timeout ->
            CaseTimeout = C#cstate.case_timeout,
            SuiteTimeout = C#cstate.suite_timeout,
            Millis = Arg,
            OldWarnings = C#cstate.warnings,
            NewWarnings =
                if
                    Millis =:= infinity,
                    CaseTimeout =:= infinity,
                    SuiteTimeout =:= infinity ->
                        clog(C, change, "expect timeout to infinity", []),
                        W = make_warning(C, "Infinite timer"),
                        [W | OldWarnings];
                    Millis =:= infinity ->
                        clog(C, change, "expect timeout to infinity", []),
                        OldWarnings;
                    is_integer(Millis) ->
                        clog(C, change, "expect timeout to ~p seconds",
                             [Millis div ?ONE_SEC]),
                        if
                            is_integer(CaseTimeout),
                            Millis > CaseTimeout ->
                                W = make_warning(C, "Match timeout > "
                                                 "test case_timeout"),
                                [W | OldWarnings];
                            is_integer(SuiteTimeout),
                            Millis > SuiteTimeout ->
                                W = make_warning(C, "Match timeout > "
                                                 "test suite_timeout"),
                                [W | OldWarnings];
                            true ->
                                OldWarnings
                        end
                end,
            C#cstate{match_timeout = Millis,
                     warnings = NewWarnings};
        no_cleanup ->
            cleanup(C, Type);
        cleanup ->
            cleanup(C, Type);
        post_cleanup ->
            cleanup(C, Type);
        Unexpected ->
            clog(C, shell_got_msg, "~p", [Unexpected]),
            Err = ?FF("[shell ~s] got cmd with type ~p\n\t~p\n",
                      [Name, Unexpected, Cmd]),
            stop(C, error, ?l2b(Err))
    end.

cleanup(C, _Type) ->
    C1 = clear_expected(C, "(cleanup)", suspend),
    C2 = flush_port(C1),
    C3 = match_patterns(C2),
    case C3#cstate.fail of
        undefined -> ok;
        _         -> clog(C3, fail, "pattern reset", [])
    end,
    case C3#cstate.success of
        undefined  -> ok;
        _          -> clog(C3, success, "pattern reset", [])
    end,
    LoopStack = C3#cstate.loop_stack,
    LoopStack2  = [L#loop{mode = break} || L <- LoopStack],
    case lists:keymember(pattern, 1, C3#cstate.loop_stack) of
        false -> ok;
        true  -> clog(C3, break, "pattern reset", [])
    end,
    C4 = C3#cstate{expected = undefined,
                   fail = undefined,
                   success = undefined,
                   loop_stack = LoopStack2},
    dlog(C4, ?dmore, "expected=undefined (cleanup)", []),
    opt_late_sync_reply(C4).

reset_output_buffer(C, Context) ->
    C2 = flush_port(C),
    Data = C2#cstate.actual,
    Waste = ?l2b(lists:concat([byte_size(Data), " bytes wasted"])),
    Events = save_event(C2, reset, Waste),
    C5 =
        case Context of
            debugger ->
                C2;
            script when Data =:= <<>> ->
                cancel_timer(C2);
            script ->
                C3 = match_patterns(C2),
                C4 = cancel_timer(C3),
                clog_skip(C4, Data),
                C4
        end,
    clog(C5, output, "reset ~p bytes", [byte_size(Data)]),
    C5#cstate{state_changed = true,
              actual = <<>>,
              events = Events}.

dequote([$\\,$\\|T]) ->
    [$\\|dequote(T)];
dequote([$\\,$n|T]) ->
    [$\n|dequote(T)];
dequote([$\\,$t|T]) ->
    [$\t|dequote(T)];
dequote([H|T]) ->
    [H|dequote(T)];
dequote([]) ->
    [].

debug_progress(#cstate{debug = {connect,_}, mode = resume}) ->
    silent;
debug_progress(#cstate{progress = Progress}) ->
    Progress.

send_to_port(C, Data) ->
    Progress = debug_progress(C),
    lux_log:safe_write(Progress,
                       C#cstate.log_fun,
                       C#cstate.stdin_log_fd,
                       Data),
    C2 = C#cstate{events = save_event(C, send, Data)},
    try
        true = port_command(C2#cstate.port, Data),
        C2
    catch
        error:_Reason ->
            receive
                {Port, {exit_status, ExitStatus}}
                  when Port =:= C2#cstate.port ->
                    C2#cstate{state_changed = true,
                              no_more_output = true,
                              exit_status = ExitStatus,
                              events = save_event(C, recv, ?shell_exit)};
                {'EXIT', Port, _PosixCode}
                  when Port =:= C2#cstate.port,
                       C#cstate.exit_status =/= undefined ->
                    C2
            after 0 ->
                    C2
            end
    end.

sleep_walker(Progress, ReplyTo, WakeUpMsg) ->
    %% Snore each second
    lux_utils:progress_write(Progress, "z"),
    receive
        WakeUpMsg ->
            ReplyTo ! WakeUpMsg,
            unlink(ReplyTo)
    after ?ONE_SEC ->
            sleep_walker(Progress, ReplyTo, WakeUpMsg)
    end.

want_more(#cstate{mode = Mode,
                  expected = Expected,
                  waiting = Waiting,
                  wakeup_ref = WakeupRef}) ->
    Mode =:= resume andalso
    Expected =:= undefined andalso
    not Waiting andalso
    WakeupRef =:= undefined.

expect_more(C) ->
    C2 = expect(C),
    HasEval =
        if
            C2#cstate.debug_level > 0 ->
                Msgs = element(2, process_info(self(), messages)),
                lists:keymember(eval, 1, Msgs);
            true ->
                false
        end,
    case want_more(C2) of
        true when HasEval ->
            dlog(C2, ?dmore, "messages=~p", [HasEval]),
            C2;
        true ->
            %% clog(C, want_more, "~p ~p",
            %%      [C#cstate.mode, (C#cstate.latest_cmd)#cmd.type]),
            dlog(C2, ?dmore, "messages=~p", [HasEval]),
            dlog(C2, ?dmore, "waiting=true (~p) send more",
                 [C2#cstate.latest_cmd#cmd.type]),
            send_reply(C2, C2#cstate.parent, {more, self(), C2#cstate.name}),
            C2#cstate{waiting = true};
        false ->
            C2
    end.

%% expect(#cstate{} = C) when ?end_of_script(C) ->
%%     %% Normal end of script
%%     stop(C, success, end_of_script);
expect(#cstate{state_changed = false} = C) ->
    %% Nothing has changed
    C;
expect(#cstate{state_changed = true,
               no_more_output = NoMoreOutput,
               expected = Expected,
               actual = Actual,
               timed_out = TimedOut} = C0) ->
    %% Something has changed
    C = C0#cstate{state_changed = false},
    case Expected of
        _ when C#cstate.wakeup_ref =/= undefined ->
            %% Sleeping
            undefined = Expected, % Assert
            suspend = C#cstate.mode, % Assert
            undefined = C#cstate.timer_ref, % Assert
            C;
        _ when C#cstate.mode =:= suspend ->
            %% Suspended
            undefined = Expected, % Assert
            undefined = C#cstate.timer_ref, % Assert
            C;
        _ when C#cstate.mode =:= stop ->
            %% Stopped
            undefined = Expected, % Assert
            undefined = C#cstate.timer_ref, % Assert
            C;
        undefined when C#cstate.mode =:= resume ->
            %% Nothing to wait for
            undefined = C#cstate.timer_ref, % Assert
            C;
        #cmd{arg = Arg} when C#cstate.mode =:= resume ->
            %% Waiting
            if
                TimedOut ->
                    %% timeout - waited enough for more input
                    C2 = match_patterns(C),
                    Earlier = C2#cstate.timer_started_at,
                    Diff = timer:now_diff(lux_utils:timestamp(), Earlier),
                    clog(C2, timer, "failed (after ~p micro seconds)", [Diff]),
                    {C3, AltExpected, AltActual} =
                        log_multi_nomatch(C2, Arg, Actual),
                    stop(C3, {fail, AltExpected, AltActual}, ?match_fail);
                NoMoreOutput, element(1, Arg) =:= endshell ->
                    %% Successful match of end of file (port program closed)
                    C2 = match_patterns(C),
                    C3 = cancel_timer(C2),
                    ExitStatus = ?l2b(?i2l(C3#cstate.exit_status)),
                    try_match(C3, ExitStatus, C3#cstate.expected, Actual),
                    opt_late_sync_reply(C3#cstate{expected = undefined});
                NoMoreOutput ->
                    %% Got end of file while waiting for more data
                    C2 = match_patterns(C),
                    ErrBin = <<"The command must be executed",
                               " in context of a shell">>,
                    stop(C2, error, ErrBin);
                element(1, Arg) =:= endshell ->
                    %% Still waiting for end of file
                    match_patterns(C);
                true ->
                    %% Waiting for more data
                    try_match(C, Actual, C#cstate.expected, undefined)

            end
    end.

try_match(C, Actual, Expected, AltSkip) ->
    Context = "",
    case match(Actual, Expected) of
        {{match, Matches}, single}  ->
            %% Successful single match
            C2 = cancel_timer(C),
            {Skip, SkipMatch, Rest, SubMatches, LogFun} =
                split_single_match(C2, Matches, Actual, Context, AltSkip),
            C3 = match_patterns(C2, SkipMatch, Rest, [LogFun]),
            match_more(C3, Skip, SubMatches);
        {{match, Matches}, {multi, Multi}}  ->
            %% Successful match
            C2 = cancel_timer(C),
            {Skip, SkipMatch, Rest, LogFuns} =
                split_multi_match(C2, Actual, Matches, Multi, Context),
            C3 = match_patterns(C2, SkipMatch, Rest, LogFuns),
            SubMatches = [], % Don't bother about sub-patterns
            match_more(C3, Skip, SubMatches);
        {nomatch, _} when AltSkip =:= undefined ->
            %% Main pattern does not match
            %% Wait for more input
             match_patterns(C);
        {nomatch, OptMulti} ->
            C2 = cancel_timer(C),
            C3 = match_patterns(C2),
            {C4, AltExpected, AltActual} =
                log_multi_nomatch(C3, OptMulti, Actual),
            stop(C4, {fail, AltExpected, AltActual}, ?match_fail);
        {{'EXIT', Reason}, _} ->
            {_, RegExp} = extract_regexp(Expected#cmd.arg),
            Err = ?FF("Bad regexp:\n\t~p\n"
                      "Reason:\n\t~p\n",
                      [RegExp, Reason]),
            stop(C, error, ?l2b(Err))
    end.

match_more(C, _Skip, SubMatches) ->
    case C#cstate.no_more_input of
        true ->
            %% End of input
            stop_relax(C, success, end_of_script);
        false ->
            SubVars = submatch_vars(SubMatches, 1),
            [clog(C, capture, "\"~s\"",
                  [ lux_utils:quote_newlines(SV)]) ||
                SV <- SubVars],
            send_reply(C, C#cstate.parent, {submatch_vars, self(), SubVars}),
            C2 = C#cstate{expected = undefined},
            dlog(C2, ?dmore, "expected=[] (waiting)", []),
            opt_late_sync_reply(C2)
    end.

submatch_vars([SubMatches | Rest], N) ->
    case SubMatches of
        nosubmatch ->
            submatch_vars(Rest, N+1); % Omit $N as its value is undefined
        Val ->
            VarVal = ?i2l(N) ++ "=" ++ ?b2l(Val),
            [VarVal | submatch_vars(Rest, N+1)]
    end;
submatch_vars([], _) ->
    [].

split_multi_match(C, Actual, Matches, Multi, Context) ->
    Zip =
        fun({Name, NamedRegExp, Cmd}, {Start, Len}) ->
                {Name, Start, Len, NamedRegExp, Cmd}
        end,
    NewMulti = lists:zipwith(Zip, Multi, Matches),
    SortedMulti = lists:keysort(2, NewMulti),
    {Skip, Rest, LogFuns} =
        log_multi_match(C, 0, Actual, SortedMulti, Context, undefined, []),
    Sz = byte_size(Actual) - byte_size(Rest),
    {SkipMatch, Rest} = split_binary(Actual, Sz),
    {Skip, SkipMatch, Rest, LogFuns}.

log_multi_match(C, Offset, Actual, [Multi | More], Context, OptSkip, LogFuns) ->
    {_Name, Pos, Len, _NamedRegExp, Cmd} = Multi,
    {Skip, TmpActual} = split_binary(Actual, Pos - Offset),
    TmpC = C#cstate{latest_cmd = Cmd},
    {Match, Rest} = split_binary(TmpActual, Len),
    LogFun =
        fun() ->
                clog_skip(TmpC, Skip),
                clog(TmpC, match, "~s\"~s\"",
                     [Context, lux_utils:to_string(Match)])
        end,
    NewOffset = Offset + byte_size(Skip) + byte_size(Match),
    NewSkip =
        if
            OptSkip =:= undefined -> Skip;
            true                  -> OptSkip
        end,
    NewLogFuns = [LogFun | LogFuns],
    log_multi_match(C, NewOffset, Rest, More, Context, NewSkip, NewLogFuns);
log_multi_match(_C, _Offset, Rest, [], _Context, Skip, LogFuns) ->
    {Skip, Rest, lists:reverse(LogFuns)}.

log_multi_nomatch(C, {multi, Multi}, Actual) ->
    LogFun =
        fun({_Name, _NamedRegExp, #cmd{arg = Arg}}, {E, A}) ->
                {mp, _RegExpOper, RegExp, MP, []} = Arg,
                Context = "",
                P = lux_utils:to_string(RegExp),
                clog(C, partitial, "~s\"~s\"", [Context, P]),
                PE = ["Sub-pattern: ", P, "\n"],
                F = lux_utils:to_string(".*"),
                FE = ["    Found: ", F, "\n"],
                {OptMatch, single} = match_single(Actual, MP),
                case OptMatch of
                    {match, [{First, TotLen} | _] = _Matches} ->
                        {_Skip, Match, _Rest} =
                            split_total(Actual, First, TotLen, undefined),
                        clog(C, found, "~s\"~s\"", [Context, F]),
                        FA = ["    Found: ",
                              lux_utils:to_string(Match), "\n"],
                        {[FE, PE | E], [FA, PE | A]};
                    nomatch ->
                        clog(C, missing, "~s", [Context]),
                        FA = ["    Missing\n"],
                        {[FE, PE | E], [FA, PE | A]}
                end
        end,
    {RevAltExpected, RevAltActual} = lists:foldl(LogFun, {[], []}, Multi),
    {C, ?l2b(lists:reverse(RevAltExpected)), ?l2b(lists:reverse(RevAltActual))};
log_multi_nomatch(C, {mp, regexp, _RegExp, _Mp, Multi}, _Actual) ->
    log_multi_nomatch(C, {multi, Multi}, _Actual);
log_multi_nomatch(C, _Single, Actual) ->
    Cmd = C#cstate.latest_cmd,
    TaggedExpected = lux_utils:cmd_expected(Cmd),
    {C, TaggedExpected, Actual}.

match_patterns(C) ->
    match_patterns(C, C#cstate.actual, C#cstate.actual, []).

match_patterns(C, Actual, AltRest, LogFuns) ->
    %% Match against fail+success+break patterns at:
    %%   - try match (but not beyond a successful match)
    %%   - reset of output buffer
    %%   - endshell
    %%     - shell exit
    %%     - pre cleanup
    %%     - relax
    %%     - end_of_script
    %% clog(C, match_patterns, "~p", [Actual]),
    C2 = match_fail_pattern(C, Actual, AltRest, LogFuns),
    C3 = match_success_pattern(C2, Actual, AltRest, LogFuns),
    match_break_patterns(C3, Actual, AltRest, LogFuns).

match_fail_pattern(C, Actual, AltRest, LogFuns) ->
    {Res, _OptMulti} = match(Actual, C#cstate.fail),
    case Res of
        {match, Matches} ->
            Actual2 =
                if
                    LogFuns =/= [] ->
                        <<Actual/binary, AltRest/binary>>;
                    true ->
                        Actual
                end,
            {C2, ActualStop} =
                prepare_stop(C, Actual2, Matches,
                             <<?fail_pattern_matched>>),
            stop(C2, fail, ActualStop);
        nomatch ->
            C;
        {{'EXIT', Reason}, _} ->
            {_, RegExp} = extract_regexp((C#cstate.fail)#cmd.arg),
            Err = ?FF("Bad regexp:\n\t~p\n"
                      "Reason:\n\t~p\n",
                      [RegExp, Reason]),
            stop(C, error, ?l2b(Err))
    end.

match_success_pattern(C, Actual, AltRest, LogFuns) ->
    {Res, _OptMulti} = match(Actual, C#cstate.success),
    case Res of
        {match, Matches} ->
            Actual2 =
                if
                    LogFuns =/= [] ->
                        <<Actual/binary, AltRest/binary>>;
                    true ->
                        Actual
                end,
            {C2, ActualStop} =
                prepare_stop(C, Actual2, Matches,
                             <<?success_pattern_matched>>),
            stop(C2, success, ActualStop);
        nomatch ->
            C;
        {{'EXIT', Reason}, _} ->
            {_, RegExp} = extract_regexp((C#cstate.success)#cmd.arg),
            Err = ?FF("Bad regexp:\n\t~p\n"
                      "Reason:\n\t~p\n",
                      [RegExp, Reason]),
            stop(C, error, ?l2b(Err))
    end.

match_break_patterns(C, Actual, AltRest, LogFuns) ->
    %% Search in reverse order. Top loop first.
    AllStack = lists:reverse(C#cstate.loop_stack),
    match_break_patterns(C, Actual, AltRest, AllStack, [], LogFuns).

match_break_patterns(C, Actual, AltRest, [Loop|Stack] = AllStack, Acc, LF) ->
    case Loop#loop.mode of
        iterate ->
            match_break_patterns(C, Actual, AltRest, Stack, [Loop|Acc], LF);
        break ->
            match_break_patterns(C, Actual, AltRest, Stack, [Loop|Acc], LF);
        BreakCmd = #cmd{type = break} ->
            {Res, _OptMulti} = match(Actual, BreakCmd),
            case Res of
                {match, Matches} ->
                    C2 = clear_expected(C, " (break loop)", C#cstate.mode),
                    C3 = opt_late_sync_reply(C2),
                    LoopCmd = Loop#loop.cmd,
                    BreakLoop = {break_pattern_matched, self(), LoopCmd},
                    send_reply(C3, C3#cstate.parent, BreakLoop),
                    {C4, _Match} =
                        post_match(C3, Actual, Matches,
                                   <<"loop break pattern matched ">>),
                    Rest = C4#cstate.actual,
                    %% Break all inner loops
                    Breaks = [L#loop{mode = break} || L <- AllStack],
                    Acc2 = Breaks ++ Acc,
                    C4#cstate{actual = Rest,
                              waiting = true,
                              loop_stack = Acc2};
                nomatch ->
                    match_break_patterns(C, Actual, AltRest, Stack,
                                         [Loop|Acc], LF);
                {{'EXIT', Reason}, _} ->
                    {_, RegExp} = extract_regexp(BreakCmd#cmd.arg),
                    Err = ?FF("Bad regexp:\n\t~p\n"
                              "Reason:\n\t~p\n",
                              [RegExp, Reason]),
                    stop(C, error, ?l2b(Err))
            end
    end;
match_break_patterns(C, _Actual, AltRest, [], Acc, LogFuns) ->
    %% No break. Log previous match and return alternate rest.
    [LF() || LF <- LogFuns],
    C#cstate{actual = AltRest, loop_stack = Acc}.

prepare_stop(C, Actual, Matches, Context) ->
    {C2, Match} = post_match(C, Actual, Matches, Context),
    Match2 = ?l2b(lux_utils:to_string(?b2l(Match))),
    do_prepare_stop(C2, Match2, Context).

do_prepare_stop(C, Match, Context) ->
    Actual = <<Context/binary, "\"", Match/binary, "\"">>,
    C2 = clear_expected(C, " (prepare stop)", suspend),
    C3 = opt_late_sync_reply(C2),
    {C3, Actual}.

clear_expected(C, Context, Mode0) ->
    case C#cstate.wakeup_ref of
        undefined -> ok;
        WakeupRef -> lux_utils:cancel_timer(WakeupRef)
    end,
    C2 = cancel_timer(C),
    OldMode = C#cstate.mode,
    Mode =
        case OldMode of
            stop -> OldMode;
            _    -> Mode0
        end,
    C3 = C2#cstate{mode = Mode, expected = undefined, wakeup_ref = undefined},
    dlog(C3, ?dmore, "expected=[]~s", [Context]),
    C3.

post_match(C, Actual, [{First, TotLen} | _], Context) ->
    {Skip, NonSkip} = split_binary(Actual, First),
    {Match, Rest} = split_binary(NonSkip, TotLen),
    clog_skip(C, Skip),
    clog(C, match, "~s\"~s\"", [Context, lux_utils:to_string(Match)]),
    C2 = C#cstate{actual = Rest},
    {C2, Match};
post_match(C, Actual, [], Context) ->
    post_match(C, Actual, [{0, byte_size(Actual)}], Context).

split_single_match(C, Matches, Actual, Context, AltSkip) ->
    [{First, TotLen} | SubMatches] = Matches,
    {Skip, Match, Rest} = split_total(Actual, First, TotLen, AltSkip),
    LogFun =
        fun() ->
                clog_skip(C, Skip),
                clog(C, match, "~s\"~s\"",
                     [Context, lux_utils:to_string(Match)])
        end,
    Extract =
        fun(PosLen) ->
                case PosLen of
                    {-1,0} -> nosubmatch;
                    _      -> binary:part(Actual, PosLen)
                end
        end,
    SubBins = lists:map(Extract, SubMatches),
    SkipMatch = <<Skip/binary, Match/binary>>,
    {Skip, SkipMatch, Rest, SubBins, LogFun}.

split_total(Actual, First, TotLen, AltSkip) ->
    {Consumed, Rest} = split_binary(Actual, First+TotLen),
    {Skip, Match} = split_binary(Consumed, First),
    case AltSkip of
        undefined -> {Skip, Match, Rest};
        _         -> {AltSkip, Actual, <<>>}
    end.

match(_Actual, undefined) ->
    {nomatch, single};
match(Actual, #pattern{cmd = Cmd}) -> % fail or success pattern
    match(Actual, Cmd);
match(Actual, #cmd{type = Type, arg = Arg}) ->
    if
        Type =:= expect; Type =:= fail; Type =:= success ; Type =:= break ->
            case Arg of
                {verbatim, _RegExpOper, Expected} ->
                    {lux_utils:verbatim_match(Actual, Expected), single};
                {mp, _RegExpOper, _RegExp, MP, []} -> % Single
                    match_single(Actual, MP);
                {mp, _RegExpOper, _RegExp, MP, Multi} ->
                    case erlang:system_info(otp_release) of
                        "R" ++ _ -> % Pre 17.0
                            {catch pre_r17_fix(Actual, Multi), {multi, Multi}};
                        _ ->
                            Opts = [{capture,all_names,index} | ?RE_RUN_OPTS],
                            {catch re:run(Actual, MP, Opts), {multi, Multi}}
                    end;
                {endshell, single, _RegExp, MP} ->
                    match_single(Actual, MP)
            end
    end.

match_single(Actual, MP) ->
    Opts = [{capture,all,index} | ?RE_RUN_OPTS],
    {catch re:run(Actual, MP, Opts), single}.

pre_r17_fix(Actual, Multi) ->
    Names = lists:sort([list_to_atom(?b2l(N)) || {N, _, _} <- Multi]),
    Perms = lux_utils:perms([Re || {_,Re,_} <- Multi]),
    Type = element(2, (element(3, hd(Multi)))#cmd.arg),
    Perms2 = [add_skip(Type, P) || P <- Perms],
    RegExps = [?l2b(Re) || Re <- Perms2],
    Opts = [{capture,Names,index}, dupnames | ?RE_RUN_OPTS],
    pre_r17_fix(Actual, RegExps, Opts).

pre_r17_fix(Actual, [RegExp | RegExps], Opts) ->
    case re:run(Actual, RegExp, Opts) of
        nomatch -> pre_r17_fix(Actual, RegExps, Opts);
        Match   -> Match
    end;
pre_r17_fix(_Actual, [], _Opts) ->
    nomatch.

flush_port(#cstate{flush_timeout = FlushTimeout} = C) ->
    flush_port2(C, FlushTimeout, []).

flush_port2(#cstate{port = Port} = C, VirtualTimeout, Acc0) ->
    Acc3 =
        case flush_port_loop(Port, 0, 0, Acc0) of
            {0, Acc} when VirtualTimeout =/= 0 ->
                RealTimeout = lux_utils:multiply(VirtualTimeout,
                                                 C#cstate.multiplier),
                {_N, Acc2} = flush_port_loop(Port, RealTimeout, 0, Acc),
                Acc2;
            {_N, Acc} ->
                Acc
        end,
    NewData = erlang:iolist_to_binary(lists:reverse(Acc3)),
    clog_recv(C, NewData).

flush_port_loop(Port, Timeout, N, Acc) ->
    receive
        {Port, {data, Data}} ->
            flush_port_loop(Port, Timeout, N+1, [Data | Acc])
    after Timeout ->
            {N,Acc}
    end.

clog_recv(C, <<>>) ->
    C;
clog_recv(C, _NewData) when C#cstate.stdout_log_fd =:= closed ->
    Progress = debug_progress(C),
    lux_utils:progress_write(Progress, "^"),
    C;
clog_recv(C, NewData) ->
    Progress = debug_progress(C),
    lux_utils:progress_write(Progress, ":"),
    lux_log:safe_write(Progress,
                       C#cstate.log_fun,
                       C#cstate.stdout_log_fd,
                       NewData),
    Events = save_event(C, recv, NewData),
    OldData = C#cstate.actual,
    Data = <<OldData/binary, NewData/binary>>,
    KeptData =
        case C#cstate.mode of
            resume ->
                Data;
            suspend ->
                Data;
            stop ->
                clog_skip(C, Data),
                <<>>
        end,
    C#cstate{state_changed = true,
             actual = KeptData,
             events = Events}.

start_timer(#cstate{timer_ref = undefined, match_timeout = Timeout} = C) ->
    M = C#cstate.multiplier,
    if
        Timeout =:= infinity ->
            clog(C, timer, "started (infinity)", []);
        is_integer(Timeout) ->
            Secs = C#cstate.match_timeout div ?ONE_SEC,
            MultiplierSecs = M / ?ONE_SEC,
            clog(C, timer, "started (~p seconds * ~.3f multiplier)",
                 [Secs, MultiplierSecs])
    end,
    TimerRef = lux_utils:send_after(Timeout, M, self(), ?match_fail),
    C#cstate{timer_ref = TimerRef, timer_started_at = lux_utils:timestamp()};
start_timer(#cstate{} = C) ->
    clog(C, timer, "already set", []),
    C.

cancel_timer(#cstate{timer_ref = undefined} = C) ->
    C;
cancel_timer(#cstate{timer_ref = TimerRef,
                     timer_started_at = Earlier,
                     warnings = OldWarnings} = C) ->
    ElapsedMicros = timer:now_diff(lux_utils:timestamp(), Earlier),
    clog(C, timer, "canceled (after ~p micro seconds)", [ElapsedMicros]),
    NewWarnings =
        case lux_utils:cancel_timer(TimerRef) of
            infinity ->
                OldWarnings;
            _TimeLeftMillis ->
                TimeoutMicros = TimerRef#timer_ref.timeout * 1000,
                ThresholdMicros =
                    erlang:trunc(TimeoutMicros * ?TIMER_THRESHOLD),
                if
                    ElapsedMicros > ThresholdMicros ->
                        Percent = ?i2l(trunc(?TIMER_THRESHOLD * 100)),
                        W = make_warning(C, "Risky timer > " ++
                                             Percent ++ "% of max"),
                        [W | OldWarnings];
                    true ->
                        OldWarnings
                end
        end,
    flush_fail(),
    C#cstate{idle_count = 0,
             timer_ref = undefined,
             timer_started_at = undefined,
             warnings = NewWarnings}.

make_warning(#cstate{orig_file = File} = C,
            Reason) ->
    Progress = debug_progress(C),
    lux_utils:progress_write(Progress, "W"),
    clog(C, warning, "\"" ++ Reason ++ "\"", []),
    FullLineNo = clog_stack(C),
    #warning{file = File,
             lineno = FullLineNo,
             details = ?l2b(Reason)}.

flush_fail() ->
    receive
        ?match_fail ->
            true
    after 0 ->
            false
    end.

extract_regexp(ExpectArg) ->
    case ExpectArg of
        reset ->
            {?EXPECTED_EQ, reset};
        {endshell, _RegExpOper, RegExp, _MP} ->
            {?EXPECTED_RE, RegExp};
        {verbatim, _RegExpOper, Verbatim} ->
            {?EXPECTED_EQ, Verbatim};
        {template, _RegExpOper, Template} ->
            {?EXPECTED_EQ, Template};
        {regexp, _RegExpOper, RegExp} ->
            {?EXPECTED_RE, RegExp};
        {mp, _RegExpOper, RegExp, _MP, _Multi} ->
            {?EXPECTED_RE, RegExp}
    end.

rebuild_multi_regexps(C, PreExpected0) ->
    PreExpected = lists:reverse(PreExpected0),
    Multi = named_regexps(PreExpected, 1, []),
    Perms = lux_utils:perms([Re || {_,Re,_} <- Multi]),
    Type = element(2, (hd(PreExpected))#cmd.arg),
    [First | Rest] = [add_skip(Type, P) || P <- Perms],
    NewRegExp = ?l2b(["(", First, ")", [["|(", R, ")"] ||  R <- Rest]]),
    Opts = [dupnames | ?RE_COMPILE_OPTS],
    case re:compile(NewRegExp, Opts) of
        {ok, NewMP} ->
            Cmd = hd(PreExpected),
            NewCmd = Cmd#cmd{arg = {mp, regexp, NewRegExp, NewMP, Multi}},
            {ok, NewCmd, NewRegExp};
        {error, {Reason, _Pos}} ->
            Err = ["Syntax error: ", Reason, " in regexp '", NewRegExp, "'"],
            stop(C, error, ?l2b(Err))
    end.

named_regexps([Cmd | Cmds], N, Acc) ->
    String = lists:concat(["LUX", N]),
    Name = ?l2b(String),
    {_, OrigRegExp} = extract_regexp(Cmd#cmd.arg),
    NamedRegExp = <<"(?<", Name/binary, ">", OrigRegExp/binary, ")">>,
    named_regexps(Cmds, N+1, [{Name, NamedRegExp, Cmd} | Acc]);
named_regexps([], _N, Acc) ->
    lists:reverse(Acc).

add_skip(expect_add, [First|Rest]) ->
    AnyChar = <<"(.|\\R)*?">>,
    [First, [[AnyChar, R] || R <- Rest]];
add_skip(_PreExpected, Perms) ->
    Perms.

stop_relax(C, OutCome, Actual) ->
    C2 = match_patterns(C#cstate{mode = stop}),
    stop(C2, OutCome, Actual).

stop(C, Outcome, _Actual) when C#cstate.pre_expected =/= [],
                               Outcome =/= error,
                               Outcome =/= fail ->
    Err = ["Shell ", C#cstate.name, " has dangling ?+ operations"],
    stop(C#cstate{pre_expected = []}, error, ?l2b(Err));
stop(C, Outcome0, Actual) when is_binary(Actual);
                                is_atom(Actual) ->
    Cmd = C#cstate.latest_cmd,
    case Outcome0 of
        {Outcome, {ExpectedTag, Expected}, Rest} ->
            ok;
        {Outcome, Expected, Rest} ->
            ExpectedTag = ?EXPECTED_RE;
        Outcome ->
            {ExpectedTag, Expected} = lux_utils:cmd_expected(Cmd),
            Rest = C#cstate.actual
    end,
    C2 = flush_port(C),
    clog_skip(C2, C2#cstate.actual),
    clog(C, stop, "~p", [Outcome]),
    clog_stack(C),
    {NewOutcome, Extra} = prepare_outcome(C2, Outcome, Actual),
    Res = #result{outcome = NewOutcome,
                  shell_name = C2#cstate.name,
                  latest_cmd = Cmd,
                  cmd_stack = C2#cstate.cmd_stack,
                  expected_tag = ExpectedTag,
                  expected = Expected,
                  extra = Extra,
                  actual = Actual,
                  rest = Rest,
                  events = lists:reverse(C2#cstate.events),
                  warnings = C#cstate.warnings},
    %% io:format("\nRES ~p\n", [Res]),
    ?TRACE_ME2(40, C2#cstate.name, Outcome, [{actual, Actual}, Res]),
%%  C3 = opt_late_sync_reply(C2#cstate{expected = undefined}),
    C3 = C2#cstate{expected = undefined, actual = <<>>, mode = stop},
    C4 = C3, % close_logs(C3),
    send_reply(C4, C4#cstate.parent, {stop, self(), Res}),
    if
        Outcome =:= shutdown ->
            close_and_exit(C4, Outcome, Res);
        Outcome =:= error ->
            close_and_exit(C4, {error, Actual}, Res);
        true ->
            %% Wait for potential cleanup to be run
            %% before we close the port
            wait_for_down(C4, Res)
    end.

clog_stack(#cstate{orig_file = File,
                   latest_cmd = LatestCmd,
                   cmd_stack = CmdStack} = C) ->
    CmdPos = lux_utils:cmd_pos(File, LatestCmd),
    FullStack = [CmdPos|CmdStack],
    FullLineNo = lux_utils:pretty_full_lineno(FullStack),
    clog(C, where, "\"" ++ FullLineNo ++ "\"", []),
    PrettyStack = lux_utils:pretty_stack(File, FullStack),
    [clog(C, stack, "\"" ++ PS ++ "\"", []) ||
        PS <- lists:reverse(PrettyStack)],
    FullLineNo.

prepare_outcome(C, Outcome, Actual) ->
    Context =
        case Actual of
            <<?fail_pattern_matched,    _/binary>> ->
                fail_pattern_matched;
            <<?success_pattern_matched, _/binary>> ->
                success_pattern_matched;
            <<?loop_break_pattern_mismatch, _/binary>> ->
                loop_break_pattern_mismatch;
            _ ->
                Actual
        end,
    if
        Outcome =:= fail, Context =:= fail_pattern_matched ->
            NewOutcome = Outcome,
            Fail = C#cstate.fail,
            FailCmd = Fail#pattern.cmd,
            {_, FailRegExp} = extract_regexp(FailCmd#cmd.arg),
            Extra = FailRegExp,
            clog(C, pattern, "~p", [lux_utils:to_string(FailRegExp)]);
        Outcome =:= success, Context =:= success_pattern_matched ->
            NewOutcome = Outcome,
            Success = C#cstate.success,
            SuccessCmd = Success#pattern.cmd,
            {_, SuccessRegExp} = extract_regexp(SuccessCmd#cmd.arg),
            Extra = SuccessRegExp,
            clog(C, pattern, "~p", [lux_utils:to_string(SuccessRegExp)]);
        Outcome =:= fail, Context =:= loop_break_pattern_mismatch ->
            NewOutcome = Outcome,
            Loop = hd(C#cstate.loop_stack),
            LoopCmd = Loop#loop.mode,
            {_, BreakRegExp} = extract_regexp(LoopCmd#cmd.arg),
            Extra = BreakRegExp,
            clog(C, pattern, "~p", [lux_utils:to_string(BreakRegExp)]);
        Outcome =:= error ->
            NewOutcome = fail,
            Extra = Actual;
        Outcome =:= shutdown ->
            NewOutcome = Outcome,
            Extra = undefined;
        Outcome =:= relax ->
            NewOutcome = Outcome,
            Extra = undefined;
        true ->
            NewOutcome = Outcome,
            Extra = undefined
    end,
    {NewOutcome, Extra}.

close_and_exit(C, Reason, #result{} = Res) ->
    ?TRACE_ME2(40, C#cstate.name, close_and_exit, [Res]),
    catch port_close(C#cstate.port),
    exit(Reason);
close_and_exit(C, Reason, Error) when element(1, Error) =:= internal_error ->
    ?TRACE_ME2(40, C#cstate.name, close_and_exit, [Error]),
    Cmd = element(3, Error),
    Why = element(2, Error),
    Res = #result{outcome = error,
                  shell_name = C#cstate.name,
                  latest_cmd = Cmd,
                  cmd_stack = C#cstate.cmd_stack,
                  expected = lux_utils:cmd_expected(Cmd),
                  extra = undefined,
                  actual = internal_error,
                  rest = C#cstate.actual,
                  events = lists:reverse(C#cstate.events),
                  warnings = C#cstate.warnings},
    if
        Why =:= interpreter_died ->
            clog(C, close, "~p", [C#cstate.mode]);
        true ->
            io:format("\nSHELL INTERNAL ERROR: ~p\n\t~p\n\t~p\n\t~p\n",
                      [Reason, Error, Res, ?stacktrace()]),
            clog(C, 'INTERNAL ERROR', "\"~p@~p\"", [Why, Cmd#cmd.lineno])
    end,
%%  C2 = opt_late_sync_reply(C#cstate{expected = undefined}),
    C2 = C#cstate{expected = undefined},
    C3 = close_logs(C2),
    send_reply(C3, C3#cstate.parent, {stop, self(), Res}),
    close_and_exit(C3, Reason, Res).

close_logs(#cstate{stdin_log_fd = InFd, stdout_log_fd = OutFd} = C) ->
    ?TRACE_ME2(40, C#cstate.name, close_logs, []),
    C2 = flush_port(C),
    clog_skip(C2, C2#cstate.actual),
    catch file:close(element(2, InFd)),
    catch file:close(element(2, OutFd)),
    C2#cstate{log_fun = closed,
              event_log_fd = closed, % Leave the log open for other processes
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

wait_for_down(C, Res) ->
    ?TRACE_ME2(40, C#cstate.name, wait_for_down, []),
    receive
        {debug_shell, From, What} ->
            debug_shell(C, From, What);
        {sync, From, When} ->
            C2 = opt_sync_reply(C, From, When),
            wait_for_down(C2, Res);
        {adjust_stacks, From, When, IsRootLoop, NewCmd, CmdStack, Fun} ->
            C2 = adjust_stacks(C, From, When, IsRootLoop,
                               NewCmd, CmdStack, Fun),
            wait_for_down(C2, Res);
        {change_mode, From, Mode, Cmd, CmdStack}
          when Mode =:= resume; Mode =:= suspend ->
            C2 = change_mode(C, From, Mode, Cmd, CmdStack),
            wait_for_down(C2, Res);
        {'DOWN', _, process, Pid, Reason} when Pid =:= C#cstate.parent ->
            interpreter_down(C, Reason),
            close_and_exit(C, Reason, Res);
        {Port, {exit_status, ExitStatus}} when Port =:= C#cstate.port ->
            C2 = C#cstate{state_changed = true,
                          no_more_output = true,
                          exit_status = ExitStatus,
                          events = save_event(C, recv, ?shell_exit)},
            wait_for_down(C2, Res);
        {'EXIT', Port, _PosixCode}
          when Port =:= C#cstate.port,
               C#cstate.exit_status =/= undefined ->
            wait_for_down(C, Res)
    end.

save_event(#cstate{latest_cmd = _Cmd, events = Events} = C, Op, Data) ->
    clog(C, Op, "\"~s\"", [lux_utils:to_string(Data)]),
    case Op of
        recv -> show_debug(C, "recv", Data);
        send -> show_debug(C, "send", Data);
        _    -> C
    end,
    %% [{Cmd#cmd.lineno, Op, Data} | Events].
    Events.

clog(#cstate{event_log_fd = closed},
     _Op, _Format, _Args) ->
    ok;
clog(#cstate{progress = Progress,
             log_fun = LogFun,
             event_log_fd = Fd,
             name = Shell,
             latest_cmd = Cmd,
             emit_timestamp = EmitTimestamp},
     Op, Format, Args) ->
    Timestamp =
        case EmitTimestamp of
            true  -> lux_utils:timestamp();
            false -> no_timestamp
        end,
    lux_log:write_event(Progress, LogFun, Fd,
                        Cmd#cmd.lineno, Shell, Op,
                        Timestamp, Format, Args).

clog_skip(_C, <<>>) ->
    ok;
clog_skip(C, Skip) ->
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Skip)]).

dlog(C, Level, Format, Args) when C#cstate.debug_level >= Level ->
    clog(C, debug, Format, Args);
dlog(_C, _Level, _Format, _Args) ->
    ok.
