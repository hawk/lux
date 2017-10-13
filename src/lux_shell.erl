%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_shell).

-export([start_monitor/4]).

-include("lux.hrl").

-define(match_fail, match_timeout).
-define(shell_exit, shell_exit).

-record(pattern,
        {cmd       :: #cmd{},
         cmd_stack :: [{string(), non_neg_integer(), atom()}]}).

-record(cstate,
        {orig_file               :: string(),
         parent                  :: pid(),
         name                    :: string(),
         debug = disconnect      :: connect | disconnect,
         latest_cmd              :: #cmd{},
         cmd_stack = []          :: [{string(), non_neg_integer(), atom()}],
         wait_for_expect         :: undefined | pid(),
         mode = resume           :: resume | suspend,
         start_reason            :: fail | success | normal,
         progress                :: silent | summary | brief |
                                    doc | compact | verbose,
         log_fun                 :: function(),
         log_prefix              :: string(),
         event_log_fd            :: {true, file:io_device()},
         stdin_log_fd            :: {false, file:io_device()},
         stdout_log_fd           :: {false, file:io_device()},
         multiplier              :: non_neg_integer(),
         poll_timeout            :: non_neg_integer(),
         flush_timeout           :: non_neg_integer(),
         match_timeout           :: non_neg_integer() | infinity,
         shell_wrapper           :: undefined | string(),
         shell_cmd               :: string(),
         shell_args              :: [string()],
         shell_prompt_cmd        :: string(),
         shell_prompt_regexp     :: string(),
         port                    :: port(),
         waiting = false         :: boolean(),
         fail                    :: undefined | #pattern{},
         success                 :: undefined | #pattern{},
         loop_stack = []         :: [#loop{}],
         expected                :: undefined | #cmd{},
         pre_expected = []       :: [#cmd{}],
         actual = <<>>           :: binary(),
         state_changed = false   :: boolean(),
         timed_out = false       :: boolean(),
         idle_count = 0          :: non_neg_integer(),
         no_more_input = false   :: boolean(),
         no_more_output = false  :: boolean(),
         exit_status             :: integer(),
         timer                   :: undefined | infinity | reference(),
         timer_started_at        :: undefined | {non_neg_integer(),
                                                 non_neg_integer(),
                                                 non_neg_integer()},
         wakeup                  :: undefined | reference(),
         debug_level = 0         :: non_neg_integer(),
         events = []             :: [tuple()]}).

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
                shell_wrapper = I#istate.shell_wrapper,
                shell_cmd = I#istate.shell_cmd,
                shell_args = I#istate.shell_args,
                shell_prompt_cmd = I#istate.shell_prompt_cmd,
                shell_prompt_regexp = I#istate.shell_prompt_regexp,
                loop_stack = I#istate.loop_stack,
                debug_level = I#istate.debug_level},
    {Pid, Ref} = spawn_monitor(fun() -> init(C, ExtraLogs) end),
    receive
        {started, Pid, Logs, NewVarVals} ->
            Shell = #shell{name = Name,
                           pid = Pid,
                           ref = Ref,
                           health = alive,
                           vars = NewVarVals ++ I#istate.global_vars},
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
    lux:trace_me(50, TraceFrom, TraceTo, element(1, Msg), [Msg]),
    To ! Msg.

init(C, ExtraLogs) when is_record(C, cstate) ->
    process_flag(trap_exit, true),
    Name = C#cstate.name,
    lux:trace_me(50, 'case', Name, 'spawn', []),
    {InFile, InFd} = open_logfile(C, "stdin"),
    {OutFile, OutFd} = open_logfile(C, "stdout"),
    C2 = C#cstate{stdin_log_fd = {false, InFd},
                  stdout_log_fd = {false, OutFd}},
    {Exec, Args} = choose_exec(C2),
    FlatExec = lists:flatten([Exec, [[" ", A] || A <- Args]]),
    Events = save_event(C2, start, FlatExec),
    StartReason = atom_to_list(C#cstate.start_reason),
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
            error:ShellReason ->
                LoopEST = erlang:get_stacktrace(),
                ErrBin = ?l2b(io_lib:format("~p", [ShellReason])),
                io:format("\nINTERNAL LUX ERROR: Shell crashed: ~s\n~p\n",
                          [ErrBin, LoopEST]),
                stop(C2, error, ErrBin)
        end
    catch
        error:InitReason ->
            InitEST = erlang:get_stacktrace(),
            FileErr = file:format_error(InitReason),
            InitBinErr = ?l2b([FlatExec, ": ", FileErr]),
            clog(C2, error, "\"~s\"", [FileErr]),
            io:format("\nINTERNAL LUX ERROR: ~s\n~p\n", [InitBinErr, InitEST]),
            stop(C2, error, InitBinErr)
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
    Timeout = timeout(C),
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
            C3 = match_patterns(C2, C2#cstate.actual),
            expect_more(C3);
        {progress, _From, Level} ->
            shell_wait_for_event(C#cstate{progress = Level}, OrigC);
        {eval, From, Cmd} ->
            dlog(C, ?dmore, "eval (got ~p)", [Cmd#cmd.type]),
            assert_eval(C, Cmd, From),
            shell_eval(C, Cmd);
        {shutdown = Data, _From} ->
            stop(C, shutdown, Data);
        {relax = Data, _From} ->
            stop(C, relax, Data);
        {end_of_script, _From} ->
            clog(C, 'end', "of script", []),
            dlog(C, ?dmore,"mode=resume (end_of_script)", []),
            %% C#cstate{no_more_input = true, mode = suspend};
            stop(C, success, end_of_script);
        {Port, {data, Data}} when Port =:= C#cstate.port ->
            safe_flush_port(C, Data, 0);
        match_timeout ->
            C#cstate{state_changed = true,
                     timed_out = true,
                     events = save_event(C, recv, ?match_fail)};
        {wakeup, Secs} ->
            clog(C, wake, "up (~p seconds)", [Secs]),
            dlog(C, ?dmore,"mode=resume (wakeup)", []),
            undefined = C#cstate.expected, % Assert
            C#cstate{mode = resume, wakeup = undefined};
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
            lux:trace_me(70, C#cstate.name, internal_error,
                         [{shell_got, Unexpected}]),
            clog(C, internal, "\"shell_got_msg ~p\"", [element(1, Unexpected)]),
            io:format("\nINTERNAL LUX ERROR: Shell got: ~p\n",
                      [Unexpected]),
            io:format("\nDEBUG(~p):\n\t~p\n",
                      [?LINE, process_info(self(), messages)]),
            C
    after multiply(C, Timeout) ->
            C#cstate{idle_count = C#cstate.idle_count + 1}
    end.

safe_flush_port(C, Data, ExtraPoll) ->
    Progress = debug_progress(C),
    lux_utils:progress_write(Progress, ":"),
    %% Read all available data
    NewData = flush_port(C, C#cstate.poll_timeout+ExtraPoll, Data),
    lux_log:safe_write(Progress,
                       C#cstate.log_fun,
                       C#cstate.stdout_log_fd,
                       NewData),
    OldData = C#cstate.actual,
    C#cstate{state_changed = true,
             actual = <<OldData/binary, NewData/binary>>,
             events = save_event(C, recv, NewData)}.

interpreter_down(C, Reason) ->
    TraceFrom = 'case',
    TraceTo = C#cstate.name,
    lux:trace_me(50, TraceFrom, TraceTo, 'DOWN', [{reason, Reason}]).

interpreter_died(C, Reason) ->
    interpreter_down(C, Reason),
    Waste = flush_port(C,
                       C#cstate.flush_timeout,
                       C#cstate.actual),
    clog_skip(C, Waste),
    close_and_exit(C,
                   {error, internal},
                   {internal_error, interpreter_died,
                    C#cstate.latest_cmd, Reason}).

timeout(C) ->
    IdleThreshold = timer:seconds(3),
    if
        C#cstate.expected =:= undefined ->
            infinity;
        C#cstate.timer =/= undefined ->
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
            {C2, RegExp2} = prepare_stop(C, RegExp, [],
                                         <<"Loop ended without match of ">>),
            stop(C2, fail, RegExp2);
        _ ->
            C#cstate{latest_cmd = NewCmd,
                     cmd_stack = CmdStack}
    end.

change_mode(C, From, Mode, Cmd, CmdStack) ->
    Reply = {change_mode_ack, self()},
    case Mode of
        suspend ->
            clog(C, Mode, "", []),
            dlog(C, ?dmore,"mode=suspend waiting=false (~p)",
                 [Cmd#cmd.type]),
            send_reply(C, From, Reply),
            C#cstate{mode = Mode,
                     waiting = false};
        resume ->
            NewC =
                C#cstate{mode = Mode,
                         waiting = false,
                         latest_cmd = Cmd,
                         cmd_stack = CmdStack},
            dlog(NewC, ?dmore, "mode=resume waiting=false (~p)",
                 [Cmd#cmd.type]),
            clog(NewC, Mode, "(idle since line ~p)",
                 [C#cstate.latest_cmd#cmd.lineno]),
            send_reply(NewC, From, Reply),
            NewC
    end.

block(C, From, OrigC) ->
    lux:trace_me(40, C#cstate.name, block, []),
    receive
        {unblock, From} ->
            lux:trace_me(40, C#cstate.name, unblock, []),
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
            stop(C, relax, Data)
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
            is_atom(Data) -> atom_to_list(Data);
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
    Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    clog_skip(C, Waste),
    close_and_exit(C,
                   {error, internal},
                   {internal_error, invalid_sender,
                    Cmd, process_info(From)});
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
                                 Cmd#cmd.type =:= cleanup ->
    ok;
assert_eval(C, Cmd, _From) ->
    Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    clog_skip(C, Waste),
    close_and_exit(C,
                   {error, internal},
                   {internal_error, invalid_type,
                    Cmd, C#cstate.expected, Cmd#cmd.type}).

shell_eval(#cstate{name = Name} = C0,
           #cmd{type = Type, arg = Arg} = Cmd) ->
    C = C0#cstate{latest_cmd = Cmd, waiting = false},
    dlog(C, ?dmore,"waiting=false (eval ~p)", [Cmd#cmd.type]),
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
            C2 = start_timer(C),
            dlog(C2, ?dmore, "expected=regexp (~p)", [element(1, Arg)]),
            C2#cstate{state_changed = true, expected = Cmd};
        expect when element(2, Arg) =:= expect_add;
                    element(2, Arg) =:= expect_add_strict ->
            %% Add alternate regexp
            Tag = element(2, Arg),
            {_, RegExp} = extract_regexp(Arg),
            clog(C, Tag, "\"~s\"", [lux_utils:to_string(RegExp)]),
            dlog(C, ?dmore, "expected=regexp (~p)", [Tag]),
            if
                element(2, (hd(C#cstate.pre_expected))#cmd.arg) =/= Tag ->
                    Err = io_lib:format("Illegal syntax: "
                                        "?+ cannot be mixed with ?++\n", []),
                    stop(C, error, ?l2b(Err));
                true ->
                    C#cstate{pre_expected = [Cmd | C#cstate.pre_expected]}
            end;
        expect when C#cstate.expected =:= undefined andalso
                    C#cstate.pre_expected =:= [] ->
            %% Single regexp
            true = lists:member(element(2, Arg), [single,multi]), % Assert
            {ExpectTag, RegExp} = extract_regexp(Arg),
            clog(C, ExpectTag, "\"~s\"", [lux_utils:to_string(RegExp)]),
            C2 = start_timer(C),
            dlog(C2, ?dmore, "expected=regexp (expect)", []),
            C2#cstate{state_changed = true, expected = Cmd, pre_expected = []};
        expect when element(2, Arg) =:= multi,
                    C#cstate.pre_expected =/= [] ->
            %% Multiple regexps
            Tag = element(2, (hd(C#cstate.pre_expected))#cmd.arg),
            {_, RegExp} = extract_regexp(Arg),
            clog(C, Tag, "\"~s\"", [lux_utils:to_string(RegExp)]),
            PreExpected = [Cmd | C#cstate.pre_expected],
            {ok, C2, NewRegExp} = rebuild_multi_regexps(C, PreExpected),
            clog(C2, perms, "\"~s\"", [lux_utils:to_string(NewRegExp)]),
            C3 = start_timer(C2),
            dlog(C3, ?dmore, "expected=regexp (expect)", []),
            C3;
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
            clog(C, success, "pattern ~p", [Arg]),
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
                        Loop#loop{mode = reset};
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
            WakeUp = {wakeup, Secs},
            Fun = fun() -> sleep_walker(Progress, Self, WakeUp) end,
            Sleeper = spawn_link(Fun),
            WakeupRef = erlang:send_after(timer:seconds(Secs), Sleeper, WakeUp),
            dlog(C, ?dmore,"mode=suspend (sleep)", []),
            C#cstate{mode = suspend, wakeup = WakeupRef};
        progress ->
            true = is_list(Arg), % Assert
            String = Arg,
            lux_utils:progress_write(C#cstate.progress, dequote(String)),
            C#cstate{events = save_event(C, progress, String)};
        change_timeout ->
            Millis = Arg,
            if
                Millis =:= infinity ->
                    clog(C, change, "expect timeout to infinity", []);
                is_integer(Millis) ->
                    clog(C, change, "expect timeout to ~p seconds",
                         [Millis div timer:seconds(1)])
            end,
            C#cstate{match_timeout = Millis};
        cleanup ->
            C1 = cancel_timer(C),
            Actual = flush_port(C1, C1#cstate.flush_timeout, C1#cstate.actual),
            C2 = match_patterns(C1#cstate{actual = Actual}, Actual),
            case C2#cstate.fail of
                undefined -> ok;
                _         -> clog(C2, fail, "pattern reset", [])
            end,
            case C2#cstate.success of
                undefined  -> ok;
                _          -> clog(C2, success, "pattern reset", [])
            end,
            LoopStack = C2#cstate.loop_stack,
            LoopStack2  = [L#loop{mode = break} || L <- LoopStack],
            case lists:keymember(pattern, 1, C2#cstate.loop_stack) of
                false -> ok;
                true  -> clog(C2, break, "pattern reset", [])
            end,
            C3 = C2#cstate{expected = undefined,
                           fail = undefined,
                           success = undefined,
                           loop_stack = LoopStack2},
            dlog(C3, ?dmore, "expected=undefined (cleanup)", []),
            opt_late_sync_reply(C3);
        Unexpected ->
            clog(C, shell_got_msg, "~p\n", [element(1, Unexpected)]),
            Err = io_lib:format("[shell ~s] got cmd with type ~p ~p\n",
                                [Name, Unexpected, Arg]),
            stop(C, error, ?l2b(Err))
    end.

reset_output_buffer(C, Context) ->
    Data = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    case Context of
        debugger ->
            C2 = C,
            Events = C#cstate.events;
        script when Data =:= <<>> ->
            C2 = cancel_timer(C),
            Events = C2#cstate.events;
        script ->
            C2 = cancel_timer(C),
            clog_skip(C2, Data),
            Events = save_event(C2, recv, Data)
    end,
    clog(C2, output, "reset", []),
    C2#cstate{state_changed = true,
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

debug_progress(#cstate{debug = {connect,_}, mode=resume}) ->
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

want_more(#cstate{mode = Mode,
                  expected = Expected,
                  waiting = Waiting,
                  wakeup = Wakeup}) ->
    Mode =:= resume andalso
    Expected =:= undefined andalso
    not Waiting andalso
    Wakeup =:= undefined.

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
               timed_out = TimedOut} = C) ->
    %% Something has changed
    case Expected of
        _ when C#cstate.wakeup =/= undefined ->
            %% Sleeping
            undefined = Expected, % assert
            resume = C#cstate.mode, % assert
            undefined = C#cstate.timer, % assert
            C;
        _ when C#cstate.mode =:= suspend ->
            %% Suspended
            undefined = Expected, % assert
            undefined = C#cstate.timer, % assert
            match_patterns(C, Actual);
        undefined when C#cstate.mode =:= resume ->
            %% Nothing to wait for
            undefined = C#cstate.timer, % assert
            C;
        #cmd{arg = Arg} when C#cstate.mode =:= resume ->
            if
                TimedOut ->
                    %% timeout - waited enough for more input
                    C2 = match_patterns(C, Actual),
                    Earlier = C2#cstate.timer_started_at,
                    Diff = timer:now_diff(lux_utils:timestamp(), Earlier),
                    clog(C2, timer, "failed (after ~p micro seconds)", [Diff]),
                    {C3, AltExpected, AltActual} =
                        log_multi_nomatch(C2, Arg, Actual),
                    stop(C3, {fail, AltExpected, AltActual}, ?match_fail);
                NoMoreOutput, element(1, Arg) =:= endshell ->
                    %% Successful match of end of file (port program closed)
                    C2 = match_patterns(C, Actual),
                    C3 = cancel_timer(C2),
                    ExitStatus = ?l2b(?i2l(C3#cstate.exit_status)),
                    try_match(C3, ExitStatus, C3#cstate.expected, Actual),
                    opt_late_sync_reply(C3#cstate{expected = undefined});
                NoMoreOutput ->
                    %% Got end of file while waiting for more data
                    C2 = match_patterns(C, Actual),
                    ErrBin = <<"The command must be executed",
                               " in context of a shell">>,
                    stop(C2, error, ErrBin);
                element(1, Arg) =:= endshell ->
                    %% Still waiting for end of file
                    match_patterns(C, Actual);
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
            {Skip, Rest, SubMatches} =
                split_single_match(C2, Matches, Actual, Context, AltSkip),
            match_more(C2, Skip, Rest, SubMatches);
        {{match, Matches}, {multi, Multi}}  ->
            %% Successful match
            C2 = cancel_timer(C),
            {Skip, Rest} =
                split_multi_match(C2, Actual, Matches, Multi, Context),
            SubMatches = [], % Don't bother about sub-patterns
            match_more(C2, Skip, Rest, SubMatches);
        {nomatch, _} when AltSkip =:= undefined ->
            %% Main pattern does not match
            %% Wait for more input
            C;
        {nomatch, OptMulti} ->
            C2 = cancel_timer(C),
            {C3, AltExpected, AltActual} =
                log_multi_nomatch(C2, OptMulti, Actual),
            stop(C3, {fail, AltExpected, AltActual}, ?match_fail);
        {{'EXIT', Reason}, _} ->
            {_, RegExp} = extract_regexp(Expected#cmd.arg),
            Err = io_lib:format("Bad regexp:\n\t~p\n"
                                "Reason:\n\t~p\n",
                                [RegExp, Reason]),
            stop(C, error, ?l2b(Err))
    end.

match_more(C, Skip, Rest, SubMatches) ->
    C2 = match_patterns(C, Skip),
    case C2#cstate.no_more_input of
        true ->
            %% End of input
            stop(C2, success, end_of_script);
        false ->
            SubVars = submatch_vars(SubMatches, 1),
            [clog(C2, capture, "\"~s\"",
                  [ lux_utils:quote_newlines(SV)]) ||
                SV <- SubVars],
            send_reply(C2, C2#cstate.parent,
                       {submatch_vars, self(), SubVars}),
            C3 = C2#cstate{expected = undefined, actual = Rest},
            dlog(C3, ?dmore, "expected=[] (waiting)", []),
            opt_late_sync_reply(C3)
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
    log_multi_match(C, 0, Actual, SortedMulti, Context, undefined).

log_multi_match(C, Offset, Actual, [Multi | More], Context, OptSkip) ->
    {_Name, Pos, Len, _NamedRegExp, Cmd} = Multi,
    {Skip, TmpActual} = split_binary(Actual, Pos - Offset),
    TmpC = C#cstate{latest_cmd = Cmd},
    clog_skip(TmpC, Skip),
    {Match, Rest} = split_binary(TmpActual, Len),
    clog(TmpC, match, "~s\"~s\"",
         [Context, lux_utils:to_string(Match)]),
    NewOffset = Offset + byte_size(Skip) + byte_size(Match),
    NewSkip =
        if
            OptSkip =:= undefined -> Skip;
            true                  -> OptSkip
        end,
    log_multi_match(C, NewOffset, Rest, More, Context, NewSkip);
log_multi_match(_C, _Offset, Rest, [], _Context, Skip) ->
    {Skip, Rest}.

log_multi_nomatch(C, {multi, Multi}, Actual) ->
    Log = fun({_Name, _NamedRegExp, #cmd{arg = Arg}}, {E, A}) ->
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
    {RevAltExpected, RevAltActual} = lists:foldl(Log, {[], []}, Multi),
    {C, ?l2b(lists:reverse(RevAltExpected)), ?l2b(lists:reverse(RevAltActual))};
log_multi_nomatch(C, {mp, regexp, _RegExp, _Mp, Multi}, _Actual) ->
    log_multi_nomatch(C, {multi, Multi}, _Actual);
log_multi_nomatch(C, _Single, Actual) ->
    Cmd = C#cstate.latest_cmd,
    TaggedExpected = lux_utils:cmd_expected(Cmd),
    {C, TaggedExpected, Actual}.

match_patterns(C, Actual) ->
    %% Match against these patterns when
    %%   - mode change
    %%   - main pattern matched
    %%   - main pattern timed out
    %%   - state change in suspended shell
    %%   - shell exit
    C2 = match_fail_pattern(C, Actual),
    C3 = match_success_pattern(C2, Actual),
    C4 = match_break_patterns(C3, Actual),
    C4#cstate{state_changed = false}.

match_fail_pattern(C, Actual) ->
    {Res, _OptMulti} = match(Actual, C#cstate.fail),
    case Res of
        {match, Matches} ->
            {C2, Actual2} = prepare_stop(C, Actual, Matches,
                                         <<?fail_pattern_matched>>),
            stop(C2, fail, Actual2);
        nomatch ->
            C;
        {{'EXIT', Reason}, _} ->
            {_, RegExp} = extract_regexp((C#cstate.fail)#cmd.arg),
            Err = io_lib:format("Bad regexp:\n\t~p\n"
                                "Reason:\n\t~p\n",
                                [RegExp, Reason]),
            stop(C, error, ?l2b(Err))
    end.

match_success_pattern(C, Actual) ->
    {Res, _OptMulti} = match(Actual, C#cstate.success),
    case Res of
        {match, Matches} ->
            {C2, Actual2} = prepare_stop(C, Actual, Matches,
                                         <<?success_pattern_matched>>),
            stop(C2, success, Actual2);
        nomatch ->
            C;
        {{'EXIT', Reason}, _} ->
            {_, RegExp} = extract_regexp((C#cstate.success)#cmd.arg),
            Err = io_lib:format("Bad regexp:\n\t~p\n"
                                "Reason:\n\t~p\n",
                                [RegExp, Reason]),
            stop(C, error, ?l2b(Err))
    end.

match_break_patterns(C, Actual) ->
    %% Search in reverse order. Top loop first.
    match_break_patterns(C, Actual, lists:reverse(C#cstate.loop_stack), []).

match_break_patterns(C, Actual, [Loop|Stack] = AllStack, Acc) ->
    case Loop#loop.mode of
        iterate ->
            match_break_patterns(C, Actual, Stack, [Loop|Acc]);
        break ->
            match_break_patterns(C, Actual, Stack, [Loop|Acc]);
        BreakCmd = #cmd{type = break} ->
            {Res, _OptMulti} = match(Actual, BreakCmd),
            case Res of
                {match, Matches} ->
                    C2 = clear_expected(C, " (break loop)"),
                    C3 = opt_late_sync_reply(C2),
                    LoopCmd = Loop#loop.cmd,
                    BreakLoop = {break_pattern_matched, self(), LoopCmd},
                    send_reply(C3, C3#cstate.parent, BreakLoop),
                    {C4, _Match} =
                        post_match(C3, Actual, Matches,
                                   <<"loop break pattern matched ">>),
                    %% Break all inner loops
                    Breaks = [L#loop{mode = break} || L <- AllStack],
                    C4#cstate{loop_stack = Breaks  ++ Acc};
                nomatch ->
                    match_break_patterns(C, Actual, Stack, [Loop|Acc]);
                {{'EXIT', Reason}, _} ->
                    {_, RegExp} = extract_regexp(BreakCmd#cmd.arg),
                    Err = io_lib:format("Bad regexp:\n\t~p\n"
                                        "Reason:\n\t~p\n",
                                        [RegExp, Reason]),
                    stop(C, error, ?l2b(Err))
            end
    end;
match_break_patterns(C, _Actual, [], Acc) ->
    C#cstate{loop_stack = Acc}.

prepare_stop(C, Actual, Matches, Context) ->
    {C2, Match} = post_match(C, Actual, Matches, Context),
    Match2 = ?l2b(lux_utils:to_string(?b2l(Match))),
    Actual2 = <<Context/binary, "\"", Match2/binary, "\"">>,
    C3 = clear_expected(C2, " (prepare stop)"),
    C4 = opt_late_sync_reply(C3),
    {C4, Actual2}.

clear_expected(C, Context) ->
    Mode =
        case C#cstate.wakeup of
            undefined -> C#cstate.mode;
            WakeupRef -> erlang:cancel_timer(WakeupRef), suspend
    end,
    C2 = cancel_timer(C),
    C3 = C2#cstate{mode = Mode, expected = undefined, wakeup = undefined},
    dlog(C3, ?dmore, "expected=[]~s", [Context]),
    C3.

post_match(C, Actual, [{First, TotLen} | _], Context) ->
    {Skip, Rest} = split_binary(Actual, First),
    {Match, _Actual2} = split_binary(Rest, TotLen),
    clog_skip(C, Skip),
    clog(C, match, "~s\"~s\"", [Context, lux_utils:to_string(Match)]),
    C2 = C#cstate{actual = Actual},
    {C2, Match};
post_match(C, Actual, [], Context) ->
    post_match(C, Actual, [{0, byte_size(Actual)}], Context).

split_single_match(C, Matches, Actual, Context, AltSkip) ->
    [{First, TotLen} | SubMatches] = Matches,
    {Skip, Match, Rest} = split_total(Actual, First, TotLen, AltSkip),
    clog_skip(C, Skip),
    clog(C, match, "~s\"~s\"", [Context, lux_utils:to_string(Match)]),
    Extract =
        fun(PosLen) ->
                case PosLen of
                    {-1,0} -> nosubmatch;
                    _      -> binary:part(Actual, PosLen)
                end
        end,
    SubBins = lists:map(Extract, SubMatches),
    {Skip, Rest, SubBins}.

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

start_timer(#cstate{timer = undefined, match_timeout = infinity} = C) ->
    clog(C, timer, "started (infinity)", []),
    C#cstate{timer = infinity, timer_started_at = lux_utils:timestamp()};
start_timer(#cstate{timer = undefined} = C) ->
    Seconds = C#cstate.match_timeout div timer:seconds(1),
    Multiplier = C#cstate.multiplier / 1000,
    clog(C, timer, "started (~p seconds * ~.3f multiplier)",
         [Seconds, Multiplier]),
    Timer = safe_send_after(C, C#cstate.match_timeout, self(), match_timeout),
    C#cstate{timer = Timer, timer_started_at = lux_utils:timestamp()};
start_timer(#cstate{} = C) ->
    clog(C, timer, "already set", []),
    C.

cancel_timer(#cstate{timer = undefined} = C) ->
    C;
cancel_timer(#cstate{timer = Timer, timer_started_at = Earlier} = C) ->
    %% io:format("\n\n========>\n\t~p\n", [?stacktrace()]),
    Diff = timer:now_diff(lux_utils:timestamp(), Earlier),
    clog(C, timer, "canceled (after ~p micro seconds)", [Diff]),
    case Timer of
        infinity -> ok;
        _        -> erlang:cancel_timer(Timer)
    end,
    C#cstate{idle_count = 0,
             timer = undefined,
             timer_started_at = undefined}.

extract_regexp(ExpectArg) ->
    case ExpectArg of
        reset ->
            {'expected=', reset};
        {endshell, _RegExpOper, RegExp, _MP} ->
            {'expected*', RegExp};
        {verbatim, _RegExpOper, Verbatim} ->
            {'expected=', Verbatim};
        {template, _RegExpOper, Template} ->
            {'expected=', Template};
        {regexp, _RegExpOper, RegExp} ->
            {'expected*', RegExp};
        {mp, _RegExpOper, RegExp, _MP, _Multi} ->
            {'expected*', RegExp}
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
            {ok,
             C#cstate{latest_cmd = NewCmd,
                      state_changed = true,
                      expected = NewCmd,
                      pre_expected = []},
             NewRegExp};
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

stop(C, Outcome, _Actual) when C#cstate.pre_expected =/= [],
                               Outcome =/= error,
                               Outcome =/= fail ->
    Err = ["Shell ", C#cstate.name, " has dangling ?+ operations"],
    stop(C#cstate{pre_expected = []}, error, ?l2b(Err));
stop(C0, Outcome0, Actual) when is_binary(Actual);
                                is_atom(Actual) ->
    Waste = flush_port(C0, C0#cstate.flush_timeout, C0#cstate.actual),
    C =
        if
            Actual =:= end_of_script; Actual =:= relax ->
                match_patterns(C0#cstate{mode = suspend}, Waste);
            true ->
                C0
        end,
    Cmd = C#cstate.latest_cmd,
    case Outcome0 of
        {Outcome, {ExpectedTag, Expected}, Rest} ->
            ok;
        {Outcome, Expected, Rest} ->
            ExpectedTag = 'expected*';
        Outcome ->
            {ExpectedTag, Expected} = lux_utils:cmd_expected(Cmd),
            Rest = C#cstate.actual
    end,
    clog_skip(C, Waste),
    clog(C, stop, "~p", [Outcome]),
    {NewOutcome, Extra} = prepare_outcome(C, Outcome, Actual),
    Res = #result{outcome = NewOutcome,
                  name = C#cstate.name,
                  latest_cmd = Cmd,
                  cmd_stack = C#cstate.cmd_stack,
                  expected_tag = ExpectedTag,
                  expected = Expected,
                  extra = Extra,
                  actual = Actual,
                  rest = Rest,
                  events = lists:reverse(C#cstate.events)},
    lux:trace_me(40, C#cstate.name, Outcome, [{actual, Actual}, Res]),
%%  C2 = opt_late_sync_reply(C#cstate{expected = undefined}),
    C2 = C#cstate{expected = undefined, actual = <<>>},
    C3 = close_logs(C2),
    send_reply(C3, C3#cstate.parent, {stop, self(), Res}),
    if
        Outcome =:= shutdown ->
            close_and_exit(C3, Outcome, Res);
        Outcome =:= error ->
            close_and_exit(C3, {error, Actual}, Res);
        true ->
            %% Wait for potential cleanup to be run
            %% before we close the port
            wait_for_down(C3, Res)
    end.

prepare_outcome(C, Outcome, Actual) ->
    Context =
        case Actual of
            <<?fail_pattern_matched,    _/binary>> -> fail_pattern_matched;
            <<?success_pattern_matched, _/binary>> -> success_pattern_matched;
            _                                      -> Actual
        end,
    if
        Outcome =:= fail, Context =:= fail_pattern_matched ->
            NewOutcome = Outcome,
            Fail = C#cstate.fail,
            FailCmd = Fail#pattern.cmd,
            Extra = element(2, FailCmd#cmd.arg),
%% ??            Extra2 = lux_utils:normalize_match_regexp(
%%                       lux_utils:to_string(Extra)),
            Extra2 = lux_utils:to_string(Extra),
            clog(C, pattern, "\"~p\"", [Extra2]);
        Outcome =:= success, Context =:= success_pattern_matched ->
            NewOutcome = Outcome,
            Success = C#cstate.success,
            SuccessCmd = Success#pattern.cmd,
            Extra = element(2, SuccessCmd#cmd.arg),
%% ??            Extra2 = lux_utils:normalize_match_regexp(
%%                       lux_utils:to_string(Extra)),
            Extra2 = lux_utils:to_string(Extra),
            clog(C, pattern, "\"~p\"", [Extra2]);
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

close_and_exit(C, Reason, #result{}) ->
    lux:trace_me(40, C#cstate.name, close_and_exit, []),
    catch port_close(C#cstate.port),
    exit(Reason);
close_and_exit(C, Reason, Error) when element(1, Error) =:= internal_error ->
    Cmd = element(3, Error),
    Why = element(2, Error),
    Res = #result{outcome = error,
                  name = C#cstate.name,
                  latest_cmd = Cmd,
                  cmd_stack = C#cstate.cmd_stack,
                  expected = lux_utils:cmd_expected(Cmd),
                  extra = undefined,
                  actual = internal_error,
                  rest = C#cstate.actual,
                  events = lists:reverse(C#cstate.events)},
    if
        Why =:= interpreter_died ->
            clog(C, error, "stop", []);
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
    Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    clog_skip(C, Waste),
    catch file:close(element(2, InFd)),
    catch file:close(element(2, OutFd)),
    C#cstate{log_fun = closed,
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
    lux:trace_me(40, C#cstate.name, wait_for_down, []),
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

clog(#cstate{event_log_fd = closed},
     _Op, _Format, _Args) ->
    ok;
clog(#cstate{progress = Progress,
             log_fun = LogFun,
             event_log_fd = Fd,
             name = Shell,
             latest_cmd = Cmd},
     Op, Format, Args) ->
    E = {log_event, Cmd#cmd.lineno, Shell, Op, Format, Args},
    lux_log:write_event(Progress, LogFun, Fd, E).

clog_skip(_C, <<>>) ->
    ok;
clog_skip(C, Skip) ->
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Skip)]).

dlog(C, Level, Format, Args) when C#cstate.debug_level >= Level ->
    clog(C, debug, Format, Args);
dlog(_C, _Level, _Format, _Args) ->
    ok.
