%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2021 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_interpret).

-include("lux.hrl").

-export([
         init/1,
         lookup_macro/2,
         flush_logs/1,
         ilog/5,
         raw_ilog/3,
         expand_vars/3,
         extra_trace_modules/1
        ]).

-define(call_level(I), I#istate.call_level).

init(I) ->
    I2 = opt_start_etrace(I),
    StartTimeStr = lux_utils:now_to_string(I2#istate.start_time),
    SuiteRef = I2#istate.suite_timer_ref,
    T = I2#istate.case_timeout,
    M = I2#istate.multiplier,
    CaseRef = lux_utils:send_after(T, M, self(), {case_timeout, T}),
    ilog(I2, "start_time \"~s\"\n", [StartTimeStr], "lux", 0),
    ilog(I2, "suite_timeout ~s\n", [timer_left(I2, SuiteRef)], "lux", 0),
    ilog(I2, "case_timeout ~s\n", [timer_left(I2, CaseRef)], "lux", 0),
    IB = I2#istate{case_timer_ref = CaseRef},
    try
        OrigCmds = IB#istate.commands,
        Macros = collect_macros(IB, OrigCmds),
        I3 =
            IB#istate{macros = Macros,
                      blocked = false,
                      has_been_blocked = false,
                      want_more = true,
                      old_want_more = undefined,
                      orig_commands = OrigCmds,
                      case_timer_ref = CaseRef},
        I5 =
            if
                I3#istate.stopped_by_user =:= suite ->
                    stopped_by_user(I3, I3#istate.stopped_by_user);
                I3#istate.debug orelse I3#istate.debug_file =/= undefined ->
                    DebugState = {attach, temporary},
                    {_, I4} = lux_debug:cmd_attach(I3, [], DebugState),
                    lux_debug:format("\nDebugger for lux. "
                                     "Try help or continue.\n",
                                     []),
                    I4;
                true ->
                    I3
            end,
        I6 = check_infinite_timers(I5),
        I7 = loop(I6),
        I8 = stop(I7),
        {ok, I8}
    catch
        throw:{error, Reason, IA} ->
            IA2 = stop(IA),
            {error, Reason, IA2};
        ?CATCH_STACKTRACE(error, Reason, EST)
            ErrBin = ?l2b(?FF("~p", [Reason])),
            io:format("\nINTERNAL LUX ERROR: Interpreter crashed: ~s\n~p\n",
                      [ErrBin, EST]),
        IB2 = stop(IB),
        {error, ErrBin, IB2}
    end.

check_infinite_timers(I) ->
    if
        I#istate.default_timeout =:= infinity,
        I#istate.suite_timeout =:= infinity;
        I#istate.case_timeout =:= infinity ->
            add_warning(I, "Infinite timer");
        true ->
            I
    end.

stop(I) ->
    %% Ensure that no shell writes to the event log after this point
    case all_shells(I) of
        [] ->
            ok;
        Shells ->
            io:format("\nINTERNAL LUX ERROR: Alive shells:\n\t~p\n", [Shells]),
            [exit(S#shell.pid, kill) || S <- Shells]
    end,

    CaseRef = I#istate.case_timer_ref,
    SuiteRef = I#istate.suite_timer_ref,
    I2 = check_risky_timer(I, "case_timeout", CaseRef),
    I3 = check_risky_timer(I2, "suite_timeout", SuiteRef),
    ilog(I3, "case_timeout ~s\n", [timer_left(I3, CaseRef)], "lux", 0),
    ilog(I3, "suite_timeout ~s\n", [timer_left(I3, SuiteRef)], "lux", 0),
    EndTime = lux_utils:now_to_string(lux_utils:timestamp()),
    ilog(I3, "end_time \"~s\"\n", [EndTime], "lux", 0),
    lux_utils:cancel_timer(CaseRef),
    I4 = I3#istate{case_timer_ref = undefined},
    opt_stop_etrace(I4).

timer_left(I, #timer_ref{} = TimerRef) ->
    timer_left(I, read_timer_left(TimerRef));
timer_left(I, TimerLeft) ->
    case TimerLeft of
        infinity ->
            "infinity";
        Millis ->
            Seconds = Millis div ?ONE_SEC,
            Multiplier = I#istate.multiplier / ?ONE_SEC,
            L = io_lib:format("~p micros left (~p seconds * ~.3f multiplier)",
                              [Millis * 1000, Seconds, Multiplier]),
            lists:flatten(L)
    end.

read_timer_left(#timer_ref{timeout = infinity}) ->
    infinity;
read_timer_left(#timer_ref{ref = Ref}) ->
    case erlang:read_timer(Ref) of
        false  -> 0;
        Millis -> Millis
    end.

check_risky_timer(I, TimerName, TimerRef) ->
    case read_timer_left(TimerRef) of
        infinity ->
            I;
        LeftMillis ->
            MaxMillis = TimerRef#timer_ref.timeout,
            RiskyThreshold = I#istate.risky_threshold,
            ThresholdMillis = erlang:trunc(MaxMillis * RiskyThreshold),
            ElapsedMillis = MaxMillis - LeftMillis,
            if
                ElapsedMillis > ThresholdMillis ->
                    Percent = ?i2l(trunc(RiskyThreshold * 100)),
                    add_warning(I, "Risky " ++ TimerName ++ " > " ++
                                    Percent ++ "% of max");
                true ->
                    I
            end
    end.

opt_start_etrace(#istate{progress = Progress, trace_mode = none} = I)
  when Progress =:= etrace; Progress =:= ctrace ->
    FirstTracePid = self(),
    Str = fun(X) -> lux_utils:to_string(X) end,
    Fun = fun(Trace, Acc) ->
                  display_trace(I#istate.progress, Trace, Str),
                  Acc
          end,
    TraceTarget = {log, Fun, undefined},
    ExtraMods = extra_trace_modules(I),
    {ok, log} =
        lux_trace:start_trace(event, TraceTarget, FirstTracePid, ExtraMods),
    I#istate{trace_mode = progress};
opt_start_etrace(I) ->
    I.

extra_trace_modules(I) ->
    case I#istate.escript_mod of
        undefined ->
            [];
        EscriptMod ->
            [EscriptMod]
    end.

display_trace(Progress, _Trace, _Str)
  when Progress =/= etrace, Progress =/= ctrace ->
    ignore;
display_trace(Progress, Trace, Str)
  when element(1, Trace) =:= trace_ts ->
    [trace_ts | Rest] = tuple_to_list(Trace),
    Rest2 = lists:reverse(tl(lists:reverse(Rest))),
    display_trace(Progress, list_to_tuple([trace | Rest2]), Str);
display_trace(_Progress, {trace, _Pid, call, {lux, trace_me, A}}, _Str)
  when length(A) =:= 4 ->
    ignore;
display_trace(Progress, {trace, _Pid, call,
               {lux, trace_me, [_DL, FromTo, FromTo, Label, C]}}, Str) ->
    io:format("~s(0): etrace \"call ~s\"\n",
              [Str(FromTo), Str(Label)]),
    case Progress of
        etrace -> ignore;
        ctrace when C =/= [] -> io:format("\t~p\n", [C]);
        ctrace -> ignore
    end;
display_trace(Progress, {trace, _Pid, call,
               {lux, trace_me, [_DL, From, To, Label, C]}}, Str) ->
    io:format("~s(0): etrace \"send ~s to ~s\"\n",
              [Str(From), Str(Label), Str(To)]),
    case Progress of
        etrace -> ignore;
        ctrace -> io:format("\t~p\n", [C])
    end;
display_trace(Progress, Trace, _Str) ->
    Type = element(3, Trace),
    case Progress of
        etrace ->
            ignore;
        ctrace when Type =:= return_from;
                    Type =:= exception_from;
                    Type =:= spawn;
                    Type =:= spawned;
                    Type =:= exit;
                    Type =:= link;
                    Type =:= unlink;
                    Type =:= getting_unlinked ->
            ignore;
        ctrace ->
            io:format("TRACE: ~p\n", [Trace])
    end.

opt_stop_etrace(#istate{progress = Progress, trace_mode = TraceMode} = I)
  when Progress =:= detail, TraceMode =:= progress ->
    lux_trace:stop_trace(),
    I#istate{trace_mode = none};
opt_stop_etrace(I) ->
    I.

collect_macros(#istate{orig_file = OrigFile} = I, OrigCmds) ->
    Collect =
        fun(Cmd, RevFile, _PosStack, Acc) ->
                case Cmd of
                    #cmd{type = macro,
                         arg = {macro, Name, _ArgNames,
                                LineNo, _LastLineNo, _Body}} ->
                        RelFile = lux_utils:pretty_filename(RevFile),
                        AbsFile = filename:absname(RelFile),
                        MacroFile = lux_utils:normalize_filename(AbsFile),
                        case lists:keyfind(Name, #macro.name, Acc) of
                            false ->
                                NewMacro = #macro{name = Name,
                                                  file = MacroFile,
                                                  lineno = LineNo,
                                                  cmd = Cmd},
                                [NewMacro | Acc];
                            OldMacro ->
                                Dir = filename:dirname(OrigFile),
                                Reason =
                                    [
                                     "Ambiguous macro '", Name,
                                     "'. Both defined in file ",
                                     lux_utils:drop_prefix(Dir, MacroFile),
                                     " at line ",
                                     ?i2l(LineNo),
                                     " and in file ",
                                     lux_utils:drop_prefix(Dir,
                                                           OldMacro#macro.file),
                                     " at line ",
                                     ?i2l(OldMacro#macro.lineno)
                                    ],
                                throw({error, ?l2b(Reason), I})
                        end;
                    _ ->
                        Acc
                end
        end,
    lux_utils:foldl_cmds(Collect, [], OrigFile, [], OrigCmds).

loop(#istate{mode = stopping,
             shells = [],
             active_shell = no_shell} = I) ->
    %% Stop main
    I;
loop(I)
  when ?call_level(I) > 1 andalso I#istate.commands =:= [] ->
    %% Stop include
    I2 = multisync(I, wait_for_expect),
    %% Check for stop and down before popping the pos_stack
    sync_return(I2);
loop(I) ->
    LoopTimeout = loop_timeout(I),
    receive
        {debug_call, FromPid, DbgCmd, CmdState} ->
            I2 = lux_debug:eval_cmd(I, FromPid, DbgCmd, CmdState),
            loop(I2);
        {stopped_by_user, Scope} ->
            %% Ordered to stop by user
            I2 = stopped_by_user(I, Scope),
            loop(I2);
        {break_pattern_matched, _Pid, LoopCmd} ->
            %% Top down search
            I2 = break_loop(I, LoopCmd),
            loop(I2#istate{want_more = true});
        {more, Pid, _Name} ->
            if
                Pid =/= I#istate.active_shell#shell.pid ->
                    %% ilog(I, "ignore_more \"~s\"\n",
                    %%      [Name],
                    %%      I#istate.active_name,
                    %%      (I#istate.latest_cmd)#cmd.lineno),
                    loop(I);
                I#istate.blocked, not I#istate.want_more ->
                    %% Block more
                    I2 = I#istate{old_want_more = true},
                    loop(I2);
                not I#istate.blocked, I#istate.old_want_more =:= undefined ->
                    dlog(I, ?dmore, "want_more=true (got more)", []),
                    I2 = I#istate{want_more = true},
                    loop(I2)
            end;
        {submatch_vars, _From, SubVars} ->
            I2 = I#istate{submatch_vars = SubVars},
            loop(I2);
        {stop, Pid, Res} ->
            %% One shell has finished. Stop the others if needed
            I2 = prepare_stop(I, Pid, Res),
            loop(I2);
        {'DOWN', _, process, Pid, Reason} ->
            I2 = prepare_stop(I, Pid, {'EXIT', Reason}),
            loop(I2);
        {TimeoutType, TimeoutMillis} when TimeoutType =:= suite_timeout;
                                          TimeoutType =:= case_timeout ->
            %% TraceFile = filename:join([I#istate.case_log_dir,
            %%                           "timeout_trace"]),
            %% io:format("\n=======> ~p ->\n lux --display_trace  ~s\n",
            %%           [TimeoutType, TraceFile]),
            %% lux_trace:stop_trace(),
            %% ExtraMods = extra_trace_modules(I),
            %% lux_trace:start_trace('case', TraceFile,
            %%                       I#istate.top_pid,[c, p, sos],
            %%                       ExtraMods),
            I2 = opt_timeout_stop(I, TimeoutType, TimeoutMillis),
            loop(I2);
        Unexpected ->
            ?TRACE_ME2(70, 'case', ignore_msg,
                       [{interpreter_got, Unexpected}]),
            ilog(I, "internal \"int_got_msg ~p\"\n",
                 [element(1, Unexpected)],
                 I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno),
            %% io:format("\nINTERNAL LUX ERROR: Interpreter got: ~p\n",
            %%           [Unexpected]),
            %% io:format("\nDEBUG(~p):\n\t~p\n",
            %%        [?LINE, process_info(self(), messages)]),
            loop(I)
    after LoopTimeout ->
            I2 = opt_dispatch_cmd(I),
            loop(I2)
    end.

break_all_loops(#istate{loop_stack = LoopStack} = I) ->
    Breaks = [L#loop{mode = break} || L <- LoopStack],
    I#istate{loop_stack = Breaks}.

break_loop(#istate{loop_stack = LoopStack} = I, LoopCmd) ->
    break_loop(I, LoopCmd, lists:reverse(LoopStack), []).

break_loop(I, LoopCmd, RevStack, Acc)
  when (hd(RevStack))#loop.cmd =:= LoopCmd ->
    %% Break all inner loops
    RevBreaks = [L#loop{mode = break} || L <- RevStack],
    I#istate{commands = [], loop_stack = lists:reverse(RevBreaks, Acc)};
break_loop(I, LoopCmd, [Loop|RevStack], Acc) ->
    break_loop(I, LoopCmd, RevStack, [Loop|Acc]);
break_loop(I, _LoopCmd, [], _Acc) ->
    %% Ignore missing loop
    I.

stopped_by_user(I, Scope) ->
    %% Ordered to stop by user
    ilog(I, "stopped_by_user\n",
         [],
         I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno),
    I2 =
        if
            I#istate.commands =:= I#istate.orig_commands ->
                I#istate{commands = []}; % Nothing to cleanup
            true ->
                I
        end,
    I3 = prepare_stop(I2, dummy_pid, {fail, stopped_by_user}),
    I3#istate{stopped_by_user = Scope, cleanup_reason = fail}.

loop_timeout(I) ->
    if
        I#istate.want_more,
        not I#istate.blocked,
        I#istate.mode =/= stopping ->
            0;
        true ->
            infinity
    end.

opt_timeout_stop(I, TimeoutType, TimeoutMillis)
  when I#istate.has_been_blocked ->
    ?TRACE_ME2(70, 'case', TimeoutType, [{ignored, TimeoutMillis}]),
    ilog(I, "~p (ignored)\n",
         [TimeoutType],
         I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno),
    io:format("WARNING: Ignoring ~p"
              " as the script has been attached by the debugger.\n",
              [TimeoutType]),
    I;
opt_timeout_stop(I, TimeoutType, TimeoutMillis) ->
    ?TRACE_ME2(70, 'case', TimeoutType, [{premature, TimeoutMillis}]),
    ilog(I, "~p failed after ~p micros\n",
         [TimeoutType, TimeoutMillis * 1000], I#istate.active_name,
         (I#istate.latest_cmd)#cmd.lineno),
    premature_stop(I, {fail, TimeoutType}, {fail, TimeoutType}).

premature_stop(I, CleanupReason, StopRes) ->
    I2 = break_all_loops(I),
    OldMode = I2#istate.mode,
    case OldMode of
        running ->
            %% An error has occurred, maybe the test case (or suite)
            %% has timed out.
            prepare_stop(I2, dummy_pid, StopRes);
        cleanup ->
            %% Error (or timeout) during cleanup,
            %% initiate stop by sending shutdown to all shells.
            multicast(I2, {shutdown, self()}),
            I#istate{mode = mode(OldMode, stopping),
                     cleanup_reason = CleanupReason};
        stopping ->
            %% Shutdown has already been sent to the normal shells,
            %% continue to collect their states as well as cleanup shells.
            multicast(I2, {shutdown, self()}),
            I2
    end.

sync_return(I) ->
    receive
        {stop, Pid, Res} ->
            %% One shell has finished. Stop the others if needed
            I2 = prepare_stop(I, Pid, Res),
            sync_return(I2);
        {'DOWN', _, process, Pid, Reason} ->
            I2 = prepare_stop(I, Pid, {'EXIT', Reason}),
            sync_return(I2)
    after 0 ->
            I
    end.

opt_dispatch_cmd(#istate{commands = Cmds, want_more = WantMore} = I) ->
    case Cmds of
        [#cmd{lineno = CmdLineNo} = Cmd | Rest] when WantMore ->
            case lux_debug:check_breakpoint(I, CmdLineNo) of
                {skip, I2} ->
                    I2#istate{commands = Rest, latest_cmd = Cmd};
                {dispatch, I2} ->
                    I3 = I2#istate{commands = Rest, latest_cmd = Cmd},
                    dispatch_cmd(I3, Cmd);
                {wait, I2} ->
                    %% Encountered a breakpoint - wait for user to continue
                    I2
            end;
        [_|_] ->
            %% Active shell is not ready for more commands yet
            I;
        [] ->
            %% End of script
            CallLevel = ?call_level(I),
            OldMode = I#istate.mode,
            if
                CallLevel > 1 ->
                    I;
                OldMode =:= stopping ->
                    %% Already stopping
                    I;
                true ->
                    %% Initiate stop by sending end_of_script to all shells.
                    multicast(I, {end_of_script, self()}),
                    I#istate{mode = mode(OldMode, stopping)}
            end
    end.

dispatch_cmd(I,
             #cmd{lineno = LineNo,
                  type = Type,
                  arg = Arg} = Cmd) ->
    %% io:format("~p\n", [Cmd]),
    ?TRACE_ME2(60, 'case', Type, [Cmd]),
    case Type of
        comment ->
            I;
        variable ->
            {Scope, Var, Val} = Arg,
            case safe_expand_vars(I, Val) of
                {ok, Val2} ->
                    QuotedVal = lux_utils:quote_newlines(Val2),
                    ilog(I, "~p \"~s=~s\"\n",
                         [Scope, Var, QuotedVal],
                         I#istate.active_name, LineNo),
                    I2 =
                        case lists:member(?SPACE , Var) of
                            true ->
                                add_warning(I, ["Variable name \"", Var,
                                                "\" contains whitespace"]);
                            false ->
                                I
                        end,
                    VarVal = lists:flatten([Var, $=, Val2]),
                    case Scope of
                        my ->
                            Vars = [VarVal | I#istate.macro_vars],
                            I2#istate{macro_vars = Vars};
                        local when I2#istate.active_shell =:= no_shell ->
                            Reason = <<"A local variable can only be set"
                                       " in context of a shell">>,
                            handle_error(I2, Reason);
                        local ->
                            add_active_var(I2, VarVal);
                        global ->
                            I3 = add_active_var(I2, VarVal),
                            Shells =
                                [S#shell{vars = [VarVal | S#shell.vars]} ||
                                    S <- I3#istate.shells],
                            GlobalVars = [VarVal | I3#istate.global_vars],
                            I3#istate{shells = Shells,
                                      global_vars = GlobalVars}
                    end;
                {no_such_var, BadName} ->
                    no_such_var(I, Cmd, LineNo, BadName)
            end;
        send when is_binary(Arg) ->
            expand_send(I, Cmd, Arg);
        expect when is_atom(Arg) ->
            shell_eval(I, Cmd);
        expect when is_tuple(Arg) ->
            case compile_regexp(I, Cmd, Arg) of
                {ok, Cmd2} ->
                    shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
                {bad_regexp, I2} ->
                    I2
            end;
        fail ->
            case compile_regexp(I, Cmd, Arg) of
                {ok, Cmd2} ->
                    ShellPos = #shell.fail_pattern,
                    {_ExpectTag, RegExp} =
                        lux_shell:extract_regexp(Cmd2#cmd.arg),
                    RegExp2 = opt_regexp(RegExp),
                    change_shell_var(I, ShellPos, RegExp2, Cmd2);
                {bad_regexp, I2} ->
                    I2
            end;
        success ->
            case compile_regexp(I, Cmd, Arg) of
                {ok, Cmd2} ->
                    ShellPos = #shell.success_pattern,
                    {_ExpectTag, RegExp} =
                        lux_shell:extract_regexp(Cmd2#cmd.arg),
                    RegExp2 = opt_regexp(RegExp),
                    change_shell_var(I, ShellPos, RegExp2, Cmd2);
                {bad_regexp, I2} ->
                    I2
            end;
        break ->
            case compile_regexp(I, Cmd, Arg) of
                {ok, _Cmd2} when I#istate.loop_stack =:= [] ->
                    Reason = <<"The break command must be executed"
                               " in context of a loop">>,
                    handle_error(I, Reason);
                {ok, Cmd2} ->
                    shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
                {bad_regexp, I2} ->
                    I2
            end;
        sleep ->
            case parse_int(I, Arg, Cmd) of
                {ok, Secs} ->
                    Cmd2 = Cmd#cmd{arg = Secs},
                    shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
                {bad_int, I2} ->
                    I2
            end;
        progress ->
            case safe_expand_vars(I, Arg) of
                {ok, String} ->
                    Cmd2 = Cmd#cmd{arg = String},
                    shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
                {no_such_var, BadName} ->
                    no_such_var(I, Cmd, LineNo, BadName)
            end;
        debug ->
            case safe_expand_vars(I, Arg) of
                {ok, DbgCmd} ->
                    Dbgpid = I#istate.debug_pid,
                    CmdState = undefined,
                    lux_debug:eval_cmd(I, Dbgpid, DbgCmd, CmdState);
                {no_such_var, BadName} ->
                    no_such_var(I, Cmd, LineNo, BadName)
            end;
        change_timeout ->
            ShellPos = #shell.match_timeout,
            case Arg of
                "" ->
                    Millis = I#istate.default_timeout,
                    Cmd2 = Cmd#cmd{arg = Millis},
                    change_shell_var(I, ShellPos, Millis, Cmd2);
                "infinity" ->
                    Infinity = infinity,
                    Cmd2 = Cmd#cmd{arg = Infinity},
                    change_shell_var(I, ShellPos, Infinity, Cmd2);
                SecsStr ->
                    case parse_int(I, SecsStr, Cmd) of
                        {ok, Secs} ->
                            Millis = Secs*?ONE_SEC,
                            Cmd2 = Cmd#cmd{arg = Millis},
                            change_shell_var(I, ShellPos, Millis, Cmd2);
                        {bad_int, I2} ->
                            I2
                    end
            end;
        doc ->
            DisplayDoc =
                fun({Level, Doc}) ->
                        Indent = lists:duplicate((Level-1)*4, $\ ),
                        ilog(I, "doc \"~s~s\"\n",
                             [Indent, Doc],
                             I#istate.active_name, LineNo),
                        case I#istate.progress of
                            doc -> io:format("\n~s~s\n", [Indent, Doc]);
                            _   -> ok
                        end
                end,
            lists:foreach(DisplayDoc, Arg),
            I;
        config ->
            {config, Var, Val} = Arg,
            ilog(I, "config \"~s=~s\"\n",
                 [Var, Val],
                 I#istate.active_name, LineNo),
            I;
        no_cleanup ->
            prepare_cleanup(I, Cmd);
        cleanup ->
            I2 = prepare_cleanup(I, Cmd),
            {I3, ShellCmd} = cleanup_cmd(I2, Cmd, "cleanup"),
            ensure_shell(I3, ShellCmd);
        post_cleanup ->
            I2 = prepare_cleanup(I, Cmd),
            ensure_shell(I2, Arg);
        shell ->
            ensure_shell(I, Cmd);
        newshell ->
            ensure_shell(I, Cmd);
        include ->
            {include, InclFile, FirstLineNo, LastLineNo, InclCmds} = Arg,
            ilog(I, "include_file \"~s\"\n",
                 [InclFile],
                 I#istate.active_name, LineNo),
            case lux_case:copy_orig(I, InclFile) of
                {ok, _} ->
                    eval_include(I, LineNo, FirstLineNo, LastLineNo,
                                 InclFile, InclCmds, Cmd);
                {error, FileReason} ->
                    CaseLogDir = I#istate.case_log_dir,
                    Reason =
                        ["Cannot copy file ", InclFile, " to ", CaseLogDir,
                         ": ", file:format_error(FileReason)],
                    handle_error(I, ?l2b(Reason))
            end;
        macro ->
            I;
        invoke ->
            case lookup_macro(I, Cmd) of
                {ok, NewCmd, MatchingMacros} ->
                    invoke_macro(I, NewCmd, MatchingMacros);
                {error, BadName} ->
                    E = ?l2b(["Variable \"$", BadName, "\" is not set"]),
                    ilog(I, "error ~s\n",
                         [E],
                         I#istate.active_name, LineNo),
                    OrigLine =
                        lux_utils:strip_leading_whitespaces(Cmd#cmd.orig),
                    handle_error(I, <<E/binary, ". Bad line: ",
                                      OrigLine/binary>>)
            end;
        loop when element(2, Arg) =:= forever ->
            eval_loop(I, Cmd);
        loop ->
            {loop, Name, ItemStr, LineNo, LastLineNo, Body} = Arg,
            case safe_expand_vars(I, ItemStr) of
                {ok, NewItemStr} ->
                    ilog(I, "loop items \"~s\"\n",
                         [NewItemStr],
                         I#istate.active_name, LastLineNo),
                    Items = string:tokens(NewItemStr, " "),
                    NewArgs = {loop, Name, Items, LineNo, LastLineNo, Body},
                    eval_loop(I, Cmd#cmd{arg = NewArgs});
                {no_such_var, BadName} ->
                    no_such_var(I, Cmd, LineNo, BadName)
            end;
        _ ->
            %% Send next command to active shell
            shell_eval(I, Cmd)
    end.

cleanup_cmd(I, Cmd, Prefix) ->
    I2 = refresh_case_timer(I),
    Suffix =
        case ?call_level(I2) of
            1 -> "";
            N -> ?i2l(N)
        end,
    ShellType =
        case I2#istate.newshell of
            true  -> newshell;
            false -> shell
        end,
    ShellCmd = Cmd#cmd{type = ShellType, arg = Prefix ++ Suffix},
    OldMode = I2#istate.mode,
    NewMode =
        if
            OldMode =:= stopping ->
                OldMode;
            I2#istate.cleanup_reason =:= normal ->
                OldMode;
            true ->
                mode(OldMode, cleanup)
        end,
    I3 = I2#istate{mode = NewMode,
                   default_timeout = I2#istate.cleanup_timeout,
                   debug_shell = no_shell},
    {I3, ShellCmd}.

shell_eval(I, Cmd) ->
    dlog(I, ?dmore, "want_more=false (send ~p)", [Cmd#cmd.type]),
    case cast(I, {eval, self(), Cmd}) of
        {ok, _Pid} ->
            I#istate{want_more = false};
        {bad_shell, I2} ->
            I2
    end.

change_shell_var(#istate{active_shell = Shell} = I, Pos, Val, Cmd) ->
    Shell2 =
        case Shell of
            no_shell -> Shell;
            #shell{} -> setelement(Pos, Shell, Val)
        end,
    I2 = I#istate{latest_cmd = Cmd, active_shell = Shell2},
    shell_eval(I2, Cmd).

eval_include(OldI, InclLineNo, FirstLineNo, LastLineNo,
             InclFile, InclCmds, InclCmd) ->
    DefaultFun = get_eval_fun(),
    eval_body(OldI, InclLineNo, FirstLineNo, LastLineNo,
              InclFile, InclCmds, InclCmd, DefaultFun, false).

get_eval_fun() ->
    fun(I) when is_record(I, istate) -> loop(I) end.

eval_body(OldI, InvokeLineNo, FirstLineNo, LastLineNo,
          CmdFile, Body, Cmd, Fun, IsRootLoop) ->
    Enter =
        fun() ->
                timestamp_ilog(OldI, "file_enter ~p ~p ~p ~p\n",
                               [InvokeLineNo, FirstLineNo, LastLineNo, CmdFile])
        end,
    OldStack = OldI#istate.pos_stack,
    CurrentPos = lux_utils:cmd_pos(CmdFile, Cmd#cmd{lineno = InvokeLineNo}),
    NewStack = [CurrentPos | OldStack],
    BeforeI = OldI#istate{call_level = ?call_level(OldI) + 1,
                          file = CmdFile,
                          latest_cmd = Cmd,
                          pos_stack = NewStack,
                          commands = Body},
    BeforeI2 = adjust_stacks(before, BeforeI, Cmd, NewStack,
                             Enter, IsRootLoop),
    try
        lux_utils:progress_write(BeforeI2#istate.progress, "("),
        AfterI = Fun(BeforeI2),
        lux_utils:progress_write(AfterI#istate.progress, ")"),
        AfterExit =
            fun() ->
                    catch timestamp_ilog(AfterI, "file_exit ~p ~p ~p ~p\n",
                                         [InvokeLineNo, FirstLineNo,
                                          LastLineNo, CmdFile])
            end,
        AfterI2 = adjust_stacks('after', AfterI, Cmd, OldStack,
                                AfterExit, IsRootLoop),
        NewI = AfterI2#istate{call_level = ?call_level(OldI),
                              file = OldI#istate.file,
                              latest_cmd = OldI#istate.latest_cmd,
                              pos_stack = OldI#istate.pos_stack,
                              commands = OldI#istate.commands},
        if
            NewI#istate.cleanup_reason =:= normal ->
                %% Everything OK - no cleanup needed
                NewI#istate{default_timeout = OldI#istate.default_timeout};
            OldI#istate.cleanup_reason =:= normal ->
                %% New cleanup initiated in body - continue on this call level
                goto_cleanup(NewI);
            true ->
                %% Already cleaning up when we started eval of body
                NewI
        end
    catch
        ?CATCH_STACKTRACE(Class, Reason, EST)
            lux_utils:progress_write(OldI#istate.progress, ")"),
            BeforeExit =
                fun() ->
                        catch timestamp_ilog(BeforeI2,"file_exit ~p ~p ~p ~p\n",
                                             [InvokeLineNo, FirstLineNo,
                                              LastLineNo, CmdFile])
                end,
            if
                Class =:= throw, element(1, Reason) =:= error ->
                    BeforeExit();
                true ->
                    adjust_stacks('after', BeforeI2, Cmd, OldStack,
                                  BeforeExit, IsRootLoop)
            end,
            erlang:raise(Class, Reason, EST)
    end.

lookup_macro(I, #cmd{arg = {invoke, Name, ArgVals}} = Cmd) ->
    case safe_expand_vars(I, Name) of
        {ok, NewName} ->
            Macros = [M || M <- I#istate.macros,
                           M#macro.name =:= NewName],
            NewArgs = {invoke, NewName, ArgVals},
            {ok, Cmd#cmd{arg = NewArgs}, Macros};
        {no_such_var, BadName} ->
            {error, BadName}
    end.

invoke_macro(I,
             #cmd{arg = {invoke, Name, ArgVals},
                  lineno = LineNo} = InvokeCmd,
             [#macro{name = Name,
                     file = MacroFile,
                     cmd = #cmd{arg = {macro, Name, ArgNames, FirstLineNo,
                                       LastLineNo, Body}} = MacroCmd}]) ->
    OldMacroVars = I#istate.macro_vars,
    case macro_vars(I, ArgNames, ArgVals, InvokeCmd) of
        {ok, MacroVars} ->
            QuotedMacroVars = format_macro_vars(MacroVars),
            ilog(I, "invoke_~s~s\n",
                 [Name, QuotedMacroVars],
                 I#istate.active_name, LineNo),
            I2 =
                case lists:member(?SPACE , Name) of
                    true ->
                        add_warning(I, ["Macro name \"", Name,
                                        "\" contains whitespace"]);
                    false ->
                        I
                end,

            BeforeI = I2#istate{macro_vars = MacroVars, latest_cmd = InvokeCmd},
            DefaultFun = get_eval_fun(),
            AfterI = eval_body(BeforeI, LineNo, FirstLineNo, LastLineNo,
                               MacroFile, Body, MacroCmd, DefaultFun, false),

            ilog(I2, "exit_~s~s\n",
                 [Name, QuotedMacroVars],
                 I2#istate.active_name, LineNo),
            AfterI#istate{macro_vars = OldMacroVars};
        {bad_vars, I2} ->
            I2
    end;
invoke_macro(I, #cmd{arg = {invoke, Name, _Values}}, []) ->
    BinName = ?l2b(Name),
    handle_error(I, <<"No such macro: ", BinName/binary>>).
%% invoke_macro(I, #cmd{arg = {invoke, Name, _Values}}, [_|_]) ->
%%     BinName = ?l2b(Name),
%%     handle_error(I, <<"Ambiguous macro: ", BinName/binary>>).

format_macro_vars([]) ->
    " \"\"";
format_macro_vars(MacroVars) ->
    [[" ", io_lib:write_string(MV)] || MV <- MacroVars].

macro_vars(I, [Name | Names], [Val | Vals], Invoke) ->
    case safe_expand_vars(I, Val) of
        {ok, Val2} ->
            case macro_vars(I, Names, Vals, Invoke) of
                {ok, MacroVars} ->
                    {ok, [lists:flatten([Name, $=, Val2]) | MacroVars]};
                {bad_vars, I2} ->
                    {bad_vars, I2}
            end;
        {no_such_var, BadName} ->
            {bad_vars, no_such_var(I, Invoke, Invoke#cmd.lineno, BadName)}
    end;
macro_vars(_I, [], [], _Invoke) ->
    {ok, []};
macro_vars(I, _Names, _Vals, #cmd{arg = {invoke, Name, _}, lineno = LineNo}) ->
    BinName = ?l2b(Name),
    BinLineNo = ?l2b(?i2l(LineNo)),
    Reason = <<"at ", BinLineNo/binary,
               ": Argument mismatch in macro: ", BinName/binary>>,
    {bad_vars, handle_error(I, Reason)}.

compile_regexp(_I, Cmd, reset) ->
    {ok, Cmd};
compile_regexp(I, Cmd, {endshell, RegExpOper, RegExp}) ->
    case compile_regexp(I, Cmd, {regexp, RegExpOper, RegExp}) of
        {ok, Cmd2} ->
            {mp, RegExpOper, RegExp2, MP2, _Multi} = Cmd2#cmd.arg,
            {ok, Cmd2#cmd{arg = {endshell, RegExpOper, RegExp2, MP2}}};
        {bad_regexp, I2} ->
            {bad_regexp, I2}
    end;
compile_regexp(_I, Cmd, {verbatim, _RegExpOper, _Verbatim}) ->
    {ok, Cmd};
compile_regexp(_I, Cmd, {mp, _RegExpOper, _RegExp, _MP, _Multi}) ->
    {ok, Cmd};
compile_regexp(I, Cmd, {template, RegExpOper, Template}) ->
    case safe_expand_vars(I, Template) of
        {ok, Verbatim} ->
            {ok, Cmd#cmd{arg = {verbatim, RegExpOper, Verbatim}}};
        {no_such_var, BadName} ->
            {bad_regexp, no_such_var(I, Cmd, Cmd#cmd.lineno, BadName)}
    end;
compile_regexp(I, Cmd, {regexp, RegExpOper, RegExp}) ->
    case safe_expand_vars(I, RegExp) of
        {ok, RegExp2} ->
            RegExp3 = lux_utils:normalize_match_regexp(RegExp2),
            case re:compile(RegExp3, ?RE_COMPILE_OPTS) of
                {ok, MP3} ->
                    {ok, Cmd#cmd{arg = {mp, RegExpOper, RegExp3, MP3, []}}};
                {error, {Reason, _Pos}} ->
                    BinErr = ?l2b(["Syntax error: ", Reason,
                                   " in regexp '", RegExp3, "'"]),
                    {bad_regexp, handle_error(I, BinErr)}
            end;
        {no_such_var, BadName} ->
            {bad_regexp, no_such_var(I, Cmd, Cmd#cmd.lineno, BadName)}
    end.

expand_send(I, Cmd, Arg) ->
    case safe_expand_vars(I, Arg) of
        {ok, Arg2} ->
            Cmd2 = Cmd#cmd{arg = Arg2},
            shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
        {no_such_var, BadName} ->
            no_such_var(I, Cmd, Cmd#cmd.lineno, BadName)
    end.

no_such_var(I, Cmd, LineNo, BadName) ->
    E = ?l2b(["Variable ${", BadName, "} is not set"]),
    ilog(I, "error ~s\n",
         [E],
         I#istate.active_name, LineNo),
    OrigLine = lux_utils:strip_leading_whitespaces(Cmd#cmd.orig),
    handle_error(I, <<E/binary, ". Bad line: ", OrigLine/binary>>).

add_warning(I, Reason) ->
    W = make_warning(I, Reason),
    I#istate{warnings = [W | I#istate.warnings]}.

make_warning(#istate{orig_file = File,
                     latest_cmd = LatestCmd,
                     progress = Progress,
                     active_name = ShellName} = I,
             Reason0) ->
    lux_utils:progress_write(Progress, "W"),
    Reason = lists:flatten(Reason0),
    LineNo = LatestCmd#cmd.lineno,
    ilog(I, "warning ~p\n", [Reason], ShellName, LineNo),
    FullLineNo = ilog_stack(I),
    lux_utils:make_warning(File, FullLineNo, Reason).

parse_int(I, Chars, Cmd) ->
    case safe_expand_vars(I, Chars) of
        {ok, Chars2} ->
            try
                {ok, list_to_integer(Chars2)}
            catch
                error:_ ->
                    BinErr =
                        ?l2b(["Syntax error at line ",
                              ?i2l(Cmd#cmd.lineno),
                              ": '", Chars2, "' integer expected"]),
                    {bad_int, handle_error(I, BinErr)}
            end;
        {no_such_var, BadName} ->
            {bad_int, no_such_var(I, Cmd, Cmd#cmd.lineno, BadName)}
    end.

eval_loop(OldI, #cmd{arg = {loop,Name,Items,First,Last,Body}} = LoopCmd) ->
    DefaultFun = get_eval_fun(),
    Loop = #loop{mode = iterate, cmd = LoopCmd},
    LoopStack = [Loop | OldI#istate.loop_stack],
    NewI = eval_body(OldI#istate{loop_stack = LoopStack}, First, First, First,
                     OldI#istate.file, Body, LoopCmd,
                     fun(I) ->
                             do_eval_loop(I, Name, Items, First, Last, Body,
                                          LoopCmd, DefaultFun, 1)
                     end,
                     true),
    NewI#istate{loop_stack = tl(NewI#istate.loop_stack)}.

do_eval_loop(OldI, _Name, _Items, _First, _Last, _Body, _LoopCmd, _LoopFun, _N)
  when (hd(OldI#istate.loop_stack))#loop.mode =:= break ->
    %% Exit the loop
    OldI;
do_eval_loop(OldI, Name, Items, First, Last, Body, LoopCmd, LoopFun, N)
  when Name =:= forever, Items =:= undefined ->
    BeforeI = OldI#istate{latest_cmd = LoopCmd},
    SyntheticLineNo = -N,
    AfterI = eval_body(BeforeI, SyntheticLineNo, First, Last,
                       BeforeI#istate.file, Body, LoopCmd,
                       fun(I) ->
                               ilog(I, "loop forever\n",
                                    [],
                                    I#istate.active_name, LoopCmd#cmd.lineno),
                               LoopFun(I)
                       end,
                       false),
    do_eval_loop(AfterI, Name, Items, First, Last, Body, LoopCmd,
                 LoopFun, N+1);
do_eval_loop(OldI, Name, Items, First, Last, Body, LoopCmd, LoopFun, N) ->
    case pick_item(Items) of
        {item, Item, Rest} ->
            LoopVar = lists:flatten([Name, $=, Item]),
            MacroVars = [LoopVar|OldI#istate.macro_vars],
            BeforeI = OldI#istate{macro_vars = MacroVars,
                                  latest_cmd = LoopCmd},
            SyntheticLineNo = -N,
            AfterI = eval_body(BeforeI, SyntheticLineNo, First, Last,
                               BeforeI#istate.file, Body, LoopCmd,
                               fun(I) ->
                                       ilog(I, "loop \"~s\"\n",
                                            [LoopVar],
                                            I#istate.active_name,
                                            First),
                                       LoopFun(I)
                               end,
                               false),
            do_eval_loop(AfterI, Name, Rest, First, Last, Body, LoopCmd,
                         LoopFun, N+1);
        endloop ->
            ilog(OldI, "endloop \"~s\"\n",
                 [Name],
                 OldI#istate.active_name, Last),
            OldI
    end.

pick_item([Item|Items]) ->
    case split_range(Item, []) of
        false ->
            %% Not a range
            {item, Item, Items};
        {Begin, Mid} ->
            try
                From = list_to_integer(Begin),
                {To, Incr} =
                    case split_range(Mid, []) of
                        false ->
                            {list_to_integer(Mid), 1};
                        {End, Incr0} ->
                            {list_to_integer(End), list_to_integer(Incr0)}
                    end,
                Seq =
                    if
                        From =< To ->
                            lists:seq(From, To, Incr);
                        true ->
                            lists:reverse(lists:seq(To, From, Incr))
                    end,
                [NewItem|NewItems] = [?i2l(I) || I <- Seq],
                {item, NewItem, NewItems ++ Items}
            catch
                _:_ ->
                    {item, Item, Items}
            end
    end;
pick_item([]) ->
    endloop.

split_range([$., $. | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
split_range([H|T], Acc) ->
    split_range(T, [H|Acc]);
split_range([], _Acc) ->
    false.

prepare_stop(#istate{results = Acc} = I, Pid, RawRes) ->
    %% Handle stop procedure
    {CleanupReason, Res} = prepare_result(I, RawRes),
    NewLevel =
        case Res#result.actual of
            internal_error -> ?dmore;
            _              -> I#istate.debug_level
        end,
    I2 = I#istate{results = [Res | Acc],
                  debug_level = NewLevel, % Activate debug after first error
                  cleanup_reason = CleanupReason},
    {Health, ShellName, I3} = delete_shell(I2, Pid),
    OldMode = I3#istate.mode,
    ?TRACE_ME2(50, 'case', prepare_stop,
               [{mode, OldMode},
                {stop, Res#result.outcome, Res#result.actual},
                {shell, ShellName, Health},
                {active_shell, I3#istate.active_shell},
                {shells, I3#istate.shells},
                Res]),
    case {OldMode, Res#result.outcome, Health} of
        {_Mode, _Outcome, zombie} ->
            I3;
        {_Mode, relax, _} ->
            I3;
        {_Mode, shutdown, _} ->
            I3;
        {running, _Outcome, _} ->
            multicast(I3, {relax, self()}),
            goto_cleanup(I3);
        {cleanup, _Outcome, _} when I3#istate.post_cleanup_cmd =:= undefined ->
            %% Initiate stop by sending shutdown to the remaining shells.
            multicast(I3, {shutdown, self()}),
            I3#istate{mode = mode(OldMode, stopping)};
        {cleanup, _Outcome, _} when I3#istate.post_cleanup_cmd =/= undefined ->
            %% Post cleanup
            multicast(I3, {relax, self()}),
            goto_post_cleanup(I3);
        {stopping, _Outcome, _} ->
            %% Shutdown has already been sent to the other shells.
            %% Continue to collect their states if needed.
            I3
    end.

prepare_result(#istate{mode = Mode,
                       latest_cmd = LatestCmd,
                       pos_stack = PosStack,
                       active_name = ActiveName,
                       cleanup_reason = OrigCleanupReason},
               Res) ->
    {CleanupReason, Res2} =
        case Res of
            #result{outcome = shutdown} ->
                {OrigCleanupReason, Res};
            #result{outcome = relax} ->
                {OrigCleanupReason, Res#result{outcome = shutdown}};
            #result{outcome = NewOutcome} ->
                {NewOutcome, Res};
            {'EXIT', {error, FailReason}} ->
                fail_result(LatestCmd, PosStack, ActiveName, FailReason);
            {fail, FailReason} ->
                fail_result(LatestCmd, PosStack, ActiveName, FailReason)
        end,
    Res3 =
        case Res2 of
            #result{actual = <<?fail_pattern_matched, _/binary>>} ->
                Res2#result{latest_cmd = LatestCmd,
                            pos_stack = PosStack};
            #result{actual = <<?success_pattern_matched, _/binary>>} ->
                Res2#result{latest_cmd = LatestCmd,
                            pos_stack = PosStack};
            #result{actual = <<?loop_break_mismatch, _/binary>>} ->
                Res2#result{latest_cmd = LatestCmd,
                            pos_stack = PosStack};
            _ ->
                Res2
        end,
    {CleanupReason, Res3#result{mode = Mode}}.

fail_result(LatestCmd, PosStack, ActiveName, FailReason) ->
    {ExpectedTag, Expected} = lux_utils:cmd_expected(LatestCmd),
    {fail,
     #result{outcome      = fail,
             latest_cmd   = LatestCmd,
             pos_stack    = PosStack,
             shell_name   = ActiveName,
             expected_tag = ExpectedTag,
             expected     = Expected,
             extra        = undefined,
             actual       = FailReason,
             rest         = fail,
             warnings     = []}}.

goto_cleanup(#istate{cleanup_reason = CleanupReason} = I)
  when CleanupReason =/= normal -> % Assert
    ?TRACE_ME2(50, 'case', goto_cleanup, [{reason, CleanupReason}]),
    LineNo = (I#istate.latest_cmd)#cmd.lineno,
    LineNoStr = ?i2l(LineNo),
    ilog(I, "goto cleanup\n",
         [],
         I#istate.active_name, LineNo),
    ResLineNoStr = result_lineno(I, LineNoStr),
    lux_utils:progress_write(I#istate.progress, ResLineNoStr),
    do_goto_cleanup(I).

do_goto_cleanup(I) ->
    I2 = break_all_loops(I),
    CleanupCmds = find_cleanup(I2),
    I2#istate{want_more = true, commands = CleanupCmds}.

find_cleanup(I) ->
    Cmds = I#istate.commands,
    %% Fast forward to (optional) cleanup command
    CleanupFun =
        fun(#cmd{type = CmdType}) ->
                CmdType =/= cleanup andalso CmdType =/= no_cleanup
        end,
    lists:dropwhile(CleanupFun, Cmds).

goto_post_cleanup(I) ->
    dlog(I, ?dmore, "want_more=false (post_cleanup)", []),

    %% Shell cmd
    LatestCmd = I#istate.latest_cmd,
    {I2, ShellCmd} = cleanup_cmd(I, LatestCmd, "post_cleanup"),
    PostCleanup = ShellCmd#cmd{type = post_cleanup,
                               arg = ShellCmd},

    %% Eval post cleanup
    CmdStr = ?l2b([I#istate.post_cleanup_cmd, "\n"]),
    EvalCmd = LatestCmd#cmd{type = send,
                            arg = CmdStr},

    %% Wait for the prompt
    CmdRegExp = ?l2b(I#istate.shell_prompt_regexp),
    SyncPrompt = LatestCmd#cmd{type = expect,
                               arg = {regexp, single, CmdRegExp}},

    I2#istate{want_more = true,
              commands = [PostCleanup, EvalCmd, SyncPrompt],
              post_cleanup_cmd = undefined}.

result_lineno(I, LineNoStr) ->
    %% Ensure that the cleanup does not take too long time
    case I#istate.results of
        [#result{actual= <<?fail_pattern_matched, _/binary>>}|_] ->
            "-" ++ LineNoStr;
        [#result{actual= <<?success_pattern_matched, _/binary>>}|_] ->
            "+" ++ LineNoStr;
        [#result{actual= <<?loop_break_mismatch, _/binary>>}|_] ->
            "+" ++ LineNoStr;
        [#result{outcome = success}|_] ->
            "";
        _ ->
            LineNoStr
    end.

refresh_case_timer(I) ->
    TimerRef = I#istate.case_timer_ref,
    case TimerRef#timer_ref.timeout of
        infinity ->
            I;
        TimeoutMillis when is_integer(TimeoutMillis) ->
            case erlang:read_timer(TimerRef#timer_ref.ref) of
                TimeLeft when is_integer(TimeLeft), TimeLeft > 0 ->
                    I;
                _TimerHasExpired ->
                    CaseRef = lux_utils:send_after(TimeoutMillis, 1, self(),
                                                   TimerRef#timer_ref.msg),
                    I#istate{case_timer_ref = CaseRef}
            end
    end.

prepare_cleanup(I, Cmd) ->
    cleanup_progress(I, Cmd),
    I2 = break_all_loops(I),
    I3 = zombify_shells(I2, Cmd),
    NewMode = cleanup_mode(I3),
    I3#istate{mode = NewMode, want_more = true}.

cleanup_mode(I) ->
    case I#istate.pos_stack of
        [] -> Context = main;
        [#cmd_pos{type = Context} | _] -> ok
    end,
    OldMode = I#istate.mode,
    if
        OldMode =/= stopping ->
            mode(OldMode, cleanup);
        Context =:= main ->
            %% Initiate stop by sending shutdown to the remaining shells.
            multicast(I, {shutdown, self()}),
            mode(OldMode, stopping);
        true ->
            mode(OldMode, stopping)
    end.

cleanup_progress(#istate{active_name = ShellName,
                         cleanup_reason = CleanupReason,
                         progress = Progress} = I,
                 #cmd{lineno = LineNo, type = Type}) ->
    {PrefixStr, ProgressStr} = cleanup_strings(Type, CleanupReason),
    ilog(I, "~scleanup\n",
         [PrefixStr],
         ShellName, LineNo),
    lux_utils:progress_write(Progress, ProgressStr).

cleanup_strings(cleanup, normal) ->
    {"", "c"};
cleanup_strings(cleanup, _CleanupReason) ->
    {"", "C"};
cleanup_strings(no_cleanup, normal) ->
    {"no_", ""};
cleanup_strings(no_cleanup, _CleanupReason) ->
    {"no_", ""};
cleanup_strings(post_cleanup, normal) ->
    {"no_", "p"};
cleanup_strings(post_cleanup, _CleanupReason) ->
    {"no_", "P"}.

zombify_shells(I, Cmd) ->
    ?TRACE_ME2(50, 'case', zombify_shells, [Cmd, {shells, I#istate.shells}]),
    multicast(I, {eval, self(), Cmd}),
    I2 = multisync(I, immediate, false),
    I3 = inactivate_shell(I2, zombify),
    if
        I3#istate.debug_shell =:= no_shell ->
            ok;
        true ->
            lux_debug:format("\nCleanup. "
                             "Turn existing shells into zombies.\n",
                             [])
    end,
    multicast(I3, {debug_shell, self(), disconnect}),
    Zombies = [S#shell{health = zombie} || S <- I3#istate.shells],
    ?TRACE_ME2(50, 'case', zombify_done, [Cmd, {shells, Zombies}]),
    I3#istate{shells = Zombies}.

delete_shell(#istate{active_shell=ActiveShell, shells=OldShells} = I, Pid) ->
    case lists:keyfind(Pid, #shell.pid, [ActiveShell | OldShells]) of
        false ->
            {dead, Pid, I};
        #shell{ref = Ref, name = Name, health = Health} ->
            erlang:demonitor(Ref, [flush]),
            {I2, NewShells} =
                if
                    Pid =:= ActiveShell#shell.pid ->
                        {inactivate_shell(I, delete), OldShells};
                    true ->
                        {I, lists:keydelete(Pid, #shell.pid, OldShells)}
                end,
            {Health, Name, I2#istate{shells = NewShells}}
    end.

all_shells(#istate{shells = OtherShells, active_shell = no_shell}) ->
    OtherShells;
all_shells(#istate{shells = OtherShells, active_shell = ActiveShell}) ->
    [ActiveShell | OtherShells].

multicast(I, Msg) ->
    Shells = all_shells(I),
    ?TRACE_ME2(50, 'case', multicast, [{shells, Shells}, Msg]),
    Send = fun(#shell{pid = Pid} = S) -> trace_msg(S, Msg), Pid ! Msg, Pid end,
    lists:map(Send, Shells).

cast(#istate{active_shell = no_shell, latest_cmd = Cmd} = I, Msg) ->
    Type = ?a2b(Cmd#cmd.type),
    Tag = ?a2b(element(1, Msg)),
    Reason = <<"The ", Type/binary,
               " command must be executed in context of a shell",
               " (", Tag/binary, ")" >>,
    {bad_shell, handle_error(I, Reason)};
cast(#istate{active_shell = #shell{pid =Pid}, active_name = Name}, Msg) ->
    trace_msg(#shell{name=Name}, Msg),
    Pid ! Msg,
    {ok, Pid}.

trace_msg(#shell{name = Name}, Msg) ->
    ?TRACE_ME(50, 'case', Name, element(1, Msg), [Msg]).

multisync(I, When) ->
    multisync(I, When, true).

multisync(I, When, HandleStop)
  when When =:= flush;
       When =:= immediate;
       When =:= wait_for_expect ->
    Pids = multicast(I, {sync, self(), When}),
    ?TRACE_ME2(50, 'case', waiting,
               [{active_shell, I#istate.active_shell},
                {shells, I#istate.shells},
                When]),
    I2 = wait_for_reply(I, Pids, sync_ack, undefined, infinity, HandleStop),
    ?TRACE_ME2(50, 'case', collected, []),
    I2.

wait_for_reply(I, Pids, Expect, Fun, FlushTimeout) ->
    wait_for_reply(I, Pids, Expect, Fun, FlushTimeout, true).

wait_for_reply(I, [Pid|Pids]=AllPids, Expect, Fun, FlushTimeout, HandleStop) ->
    receive
        {debug_call, FromPid, DbgCmd, CmdState} ->
            I2 = lux_debug:eval_cmd(I, FromPid, DbgCmd, CmdState),
            wait_for_reply(I2, AllPids, Expect, Fun, FlushTimeout, HandleStop);
        {Expect, Pid} ->
            wait_for_reply(I, Pids, Expect, Fun, FlushTimeout, HandleStop);
        %%      {Expect, Pid, Expected} when Expect =:= ?EXPECTED_OLD,
        %%                                   Pids =:= [] ->
        %%          Expected;
        {stop, FromPid, Res} when HandleStop ->
            I2 = prepare_stop(I, FromPid, Res),
            %% Flush spurious message
            receive {Expect, FromPid} -> ok after 0 -> ok end,
            Pids2 = [Pid|Pids] -- [FromPid],
            wait_for_reply(I2, Pids2, Expect, Fun, FlushTimeout, HandleStop);
        {'DOWN', _, process, Pid, Reason} ->
            opt_apply(Fun),
            shell_crashed(I, Pid, Reason);
        {TimeoutType, TimeoutMillis} when TimeoutType =:= suite_timeout;
                                          TimeoutType =:= case_timeout ->
            I2 = opt_timeout_stop(I, TimeoutType, TimeoutMillis),
            wait_for_reply(I2, [Pid|Pids], Expect, Fun, 500, HandleStop);
        Unexpected when FlushTimeout =/= infinity ->
            ?TRACE_ME2(70, 'case', ignore_msg,
                       [{interpreter_got,Unexpected}]),
            ilog(I, "internal \"int_got_msg ~p\"\n",
                 [element(1, Unexpected)],
                 I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno),
            %% io:format("\nINTERNAL LUX ERROR: Interpreter got: ~p\n",
            %%           [Unexpected]),
            %% io:format("DEBUG(~p): ~p ~p\n\t~p\n\t~p\n\t~p\n",
            %%           [?LINE, Expect, [Pid|Pids],
            %%            process_info(self(), messages),
            %%            process_info(Pid, messages),
            %%            process_info(Pid, current_stacktrace)]),
            wait_for_reply(I, [Pid|Pids], Expect, Fun, FlushTimeout, HandleStop)
    after FlushTimeout ->
            I
    end;
wait_for_reply(I, [], _Expect, _Fun, _FlushTimeout, _HandleStop) ->
    I.

opt_apply(Fun) when is_function(Fun) ->
    Fun();
opt_apply(_Fun) ->
    ignore.

flush_logs(I) ->
    flush_summary_log(I),
    multisync(I, flush).

flush_summary_log(#istate{summary_log_fd=undefined}) ->
    ok;
flush_summary_log(#istate{summary_log_fd=SummaryFd}) ->
    file:sync(SummaryFd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Control a shell

ensure_shell(I, #cmd{arg = Name, type = Type})
  when Type =:= shell, Name == "" ->
    %% No name. Inactivate the shell
    inactivate_shell(I, abandon);
ensure_shell(I, #cmd{lineno = LineNo, arg = Name, type = Type} = Cmd)
  when Name =/= "" ->
    case safe_expand_vars(I, Name) of
        {ok, NewName} ->
            I2 =
                case lists:member(?SPACE , NewName) of
                    true ->
                        add_warning(I, ["Shell name \"", NewName,
                                        "\" contains whitespace"]);
                    false ->
                        I
                end,
            if
                I2#istate.active_shell#shell.name =:= NewName ->
                    case Type of
                        shell ->
                            %% Keep active shell
                            I;
                        newshell ->
                            handle_error(I, ?l2b("shell " ++ NewName ++
                                                     " already exists." ++
                                                     " Use shell command to" ++
                                                     " change active shell."))
                    end;
                true ->
                    I3 = I2#istate{want_more = false},
                    Shells = I3#istate.shells,
                    case lists:keyfind(NewName, #shell.name, Shells) of
                        false ->
                            %% New shell
                            shell_start(I3, Cmd#cmd{arg = NewName});
                        Shell ->
                            %% Existing shell
                            shell_switch(I3, Cmd, Shell)
                    end
            end;
        {no_such_var, BadName} ->
            no_such_var(I, Cmd, LineNo, BadName)
    end.

shell_start(I, #cmd{arg = Name, type = Type})
  when I#istate.newshell andalso Type =:= shell ->
    handle_error(I, ?l2b("In newshell mode the shell " ++ Name ++
                             " must be started with newshell"));
shell_start(I, #cmd{arg = Name} = Cmd) ->
    case change_active_mode(I, Cmd, suspend) of
        {ok, I2} ->
            I3 = inactivate_shell(I2, suspend),
            case safe_expand_vars(I3, "$LUX_EXTRA_LOGS") of
                {ok, ExtraLogs} ->
                    case lux_shell:start_monitor(I3, Cmd, Name, ExtraLogs) of
                        {ok, I4} ->
                            prepare_shell_prompt(I4, Cmd);
                        {error, I4, Pid, Reason} ->
                            shell_crashed(I4, Pid, Reason)
                    end;
                {no_such_var, BadName} ->
                    no_such_var(I3, Cmd, Cmd#cmd.lineno, BadName)
            end;
        {bad_mode, I2} ->
            I2
    end.

prepare_shell_prompt(I, Cmd) ->
    %% Wait for some shell output
    WaitForOutput = Cmd#cmd{type = expect,
                            arg = {regexp, single, <<".+">>}},
    %% Set the prompt (after the rc files has ben run)
    CmdStr = ?l2b([I#istate.shell_prompt_cmd, "\n"]),
    SetPrompt = Cmd#cmd{type = send,
                        arg = CmdStr},
    %% Wait for the prompt
    CmdRegExp = ?l2b(I#istate.shell_prompt_regexp),
    SyncPrompt = Cmd#cmd{type = expect,
                         arg = {regexp, single, CmdRegExp}},
    Cmds = [WaitForOutput, SetPrompt, SyncPrompt | I#istate.commands],
    dlog(I, ?dmore, "want_more=false (shell_start)", []),
    I#istate{commands = Cmds}.

shell_switch(I, #cmd{type = Type}, #shell{name = Name})
  when Type =:= newshell ->
    ilog(I, "error The newshell cmd may not be used to change shell\n",
         [],
         Name, (I#istate.latest_cmd)#cmd.lineno),
    handle_error(I, ?l2b("shell " ++ Name ++ " already exists"));
shell_switch(OldI, Cmd, #shell{health = alive, name = NewName} = NewShell) ->
    %% Activate shell
    case change_active_mode(OldI, Cmd, suspend) of
        {ok, I2} ->
            I3 = inactivate_shell(I2, suspend),
            NewShells = lists:keydelete(NewName, #shell.name, I3#istate.shells),
            NewI = I3#istate{active_shell = NewShell,
                             active_name = NewName,
                             shells = NewShells},
            case change_active_mode(NewI, Cmd, resume) of
                {ok, NewI2} ->
                    NewI2;
                {bad_mode, NewI2} ->
                    NewI2
            end;
        {bad_mode, I2} ->
            I2
    end;
shell_switch(OldI, _Cmd, #shell{name = Name, health = zombie}) ->
    ilog(OldI, "zombie shell at cleanup\n",
         [],
         Name, (OldI#istate.latest_cmd)#cmd.lineno),
    handle_error(OldI, ?l2b(Name ++ " is a zombie shell")).

inactivate_shell(#istate{active_shell = no_shell} = I, _Reason) ->
    I;
inactivate_shell(#istate{active_shell = ActiveShell, shells = Shells} = I,
                 Reason) ->
    ilog(I, "inactivate after ~p\n",
         [Reason],
         I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno),
    I#istate{active_shell = no_shell,
             active_name = "lux",
             shells = [ActiveShell | Shells]}.

change_active_mode(I, Cmd, NewMode)
  when is_pid(I#istate.active_shell#shell.pid) ->
    case cast(I, {change_mode, self(), NewMode, Cmd, I#istate.pos_stack}) of
        {ok, Pid} ->
            I2 = wait_for_reply(I, [Pid], change_mode_ack, undefined, infinity),
            {ok, I2};
        {bad_shell, I2} ->
            {bad_mode, I2}
    end;
change_active_mode(I, _Cmd, _NewMode) ->
    %% No active shell
    {ok, I}.

adjust_stacks(When, #istate{active_shell = no_shell} = I,
              NewCmd, PosStack, Fun, IsRootLoop) ->
    Fun(),
    adjust_suspended_stacks(When, I, NewCmd, PosStack, IsRootLoop, []);
adjust_stacks(When, #istate{active_shell = #shell{pid = ActivePid}} = I,
           NewCmd, PosStack, Fun, IsRootLoop) ->
    Msg = {adjust_stacks, self(), When, IsRootLoop, NewCmd, PosStack, Fun},
    case cast(I, Msg) of
        {ok, ActivePid} ->
            adjust_suspended_stacks(When, I, NewCmd, PosStack,
                                    IsRootLoop, [ActivePid]);
        {bad_shell, I2} ->
            I2
    end.

adjust_suspended_stacks(When, I, NewCmd, PosStack, IsRootLoop, ActivePids) ->
    Fun = fun() -> ignore end,
    Msg = {adjust_stacks, self(), When, IsRootLoop, NewCmd, PosStack, Fun},
    OtherPids = multicast(I#istate{active_shell = no_shell}, Msg),
    Pids = ActivePids ++ OtherPids,
    wait_for_reply(I, Pids, adjust_stacks_ack, Fun, infinity).

shell_crashed(I, Pid, Reason) when Pid =:= I#istate.active_shell#shell.pid ->
    I2 = inactivate_shell(I, crash),
    shell_crashed(I2, Pid, Reason);
shell_crashed(I, Pid, Reason) ->
    What =
        case lists:keyfind(Pid, #shell.pid, I#istate.shells) of
            false -> ["Process ", ?FF("~p", [Pid])];
            Shell -> ["Shell ", Shell#shell.name]
        end,
    ErrBin2 =
        case Reason of
            {error, ErrBin} ->
                ErrBin;
            _ ->
                ?l2b([What, " crashed during ",
                      I#istate.latest_cmd#cmd.type, " command"])
        end,
    io:format("\nINTERNAL LUX ERROR: ~s\n~p\n", [ErrBin2, ?stacktrace()]),
    handle_error(I, ErrBin2).

safe_expand_vars(I, Bin) ->
    MissingVar = error,
    try
        {ok, expand_vars(I, Bin, MissingVar)}
    catch
        throw:{no_such_var, BadName} ->
            {no_such_var, BadName}
    end.

expand_vars(#istate{active_shell  = Shell,
                    submatch_vars = SubVars,
                    macro_vars    = MacroVars,
                    global_vars   = OptGlobalVars,
                    builtin_vars  = BuiltinGlobalVars,
                    system_vars   = SystemVars},
            Val,
            MissingVar) ->
    case Shell of
        #shell{vars = LocalVars,
               match_timeout = Millis,
               fail_pattern = FailPattern,
               success_pattern = SuccessPattern} ->
            Secs =
                case Millis of
                    infinity -> infinity;
                    _        ->  Millis div ?ONE_SEC
                end,
            BuiltinLocalVars =
                [
                 lists:flatten("LUX_TIMEOUT=",
                               ?FF("~p", [Secs])),
                 lists:flatten("LUX_FAIL_PATTERN=",
                               ?FF("~s", [opt_binary(FailPattern)])),
                 lists:flatten("LUX_SUCCESS_PATTERN=",
                               ?FF("~s", [opt_binary(SuccessPattern)]))
                ];
        no_shell ->
            LocalVars = OptGlobalVars,
            BuiltinLocalVars = []
    end,
    Varss = [SubVars, MacroVars,
             LocalVars, BuiltinLocalVars,
             BuiltinGlobalVars, SystemVars],
    lux_utils:expand_vars(Varss, Val, MissingVar).

opt_binary(OptBin) ->
    case OptBin of
        undefined               -> <<"">>;
        Bin when is_binary(Bin) -> Bin
    end.

opt_regexp(OptRegExp) ->
    case OptRegExp of
        reset -> undefined;
        RegExp when is_binary(RegExp) -> RegExp
    end.

add_active_var(#istate{active_shell = no_shell} = I, _VarVal) ->
    I;
add_active_var(#istate{active_shell = Shell} = I, VarVal) ->
    LocalVars = [VarVal | Shell#shell.vars],
    Shell2 = Shell#shell{vars = LocalVars},
    I#istate{active_shell = Shell2}.

ilog(I, Format, Args, ShellName, LineNo) ->
    timestamp_ilog(I, "~s(~p): " ++ Format, [ShellName, LineNo | Args]).

timestamp_ilog(I, Format, Args) ->
    case I#istate.emit_timestamp of
        true ->
            Now = lux_utils:timestamp(),
            {_Mega, _Secs, Micros} = Now,
            {_Date, {Hours, Mins, Secs}} = calendar:now_to_local_time(Now),
            raw_ilog(I, "~2..0w:~2..0w:~2..0w.~6..0w " ++ Format,
                     [Hours, Mins, Secs, Micros | Args]);
        false ->
            raw_ilog(I, Format, Args)
    end.

raw_ilog(#istate{progress = Progress, log_fun = LogFun, event_log_fd = Fd},
         Format,
         Args) ->
    lux_log:safe_format(Progress, LogFun, Fd, Format, Args).

ilog_stack(#istate{orig_file = File,
                   latest_cmd = LatestCmd,
                   pos_stack = PosStack,
                   active_name = ShellName} = C) ->
    CmdPos = lux_utils:cmd_pos(File, LatestCmd),
    FullStack = [CmdPos|PosStack],
    FullLineNo = lux_utils:pretty_full_lineno(FullStack),
    LineNo = LatestCmd#cmd.lineno,
    ilog(C, "where \"~s\"\n", [FullLineNo], ShellName, LineNo),
    PrettyStack = lux_utils:pretty_stack(File, FullStack),
    [ilog(C, "stack \"~s\" ~p ~s\n", [PS, T, N], ShellName, LineNo) ||
        {PS, #cmd_pos{type = T, name = N}} <- PrettyStack],
    FullLineNo.

dlog(I, Level, Format, Args) when I#istate.debug_level >= Level ->
    ilog(I, "debug2 \"" ++ Format ++ "\"\n",
         Args,
         I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno);
dlog(_I, _Level, _Format, _Args) ->
    ok.

handle_error(#istate{active_shell = ActiveShell,
                     shells = Shells,
                     latest_cmd = Cmd} = I, Reason)
  when is_binary(Reason) ->
    ?TRACE_ME2(50, 'case', error,
               [{active_shell, ActiveShell},
                {shells, Shells},
                {cmd, Cmd},
                {reason, Reason}]),
    ilog(I, "error \"~s\"\n",
         [Reason],
         I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno),
    premature_stop(I, error, {'EXIT', {error, Reason}}).

mode(Mode, Mode) ->
    Mode;
mode(OldMode, NewMode) ->
    ?TRACE_ME2(50, 'case', change_run_mode,
               [{old_mode, OldMode}, {new_mode, NewMode}]),
    NewMode.
