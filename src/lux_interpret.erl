%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_interpret).

-include("lux.hrl").

-export([
         init/2,
         lookup_macro/2,
         flush_logs/1,
         ilog/3,
         expand_vars/3
        ]).

init(I, StartTime) ->
    ilog(I,
         "lux(0): start_time \"~s\"\n",
         [lux_utils:now_to_string(StartTime)]),
    Ref = safe_send_after(I, I#istate.case_timeout, self(),
                          {case_timeout, I#istate.case_timeout}),
    OrigCmds = I#istate.commands,
    try
        I2 =
            I#istate{macros = collect_macros(I, OrigCmds),
                     blocked = false,
                     has_been_blocked = false,
                     want_more = true,
                     old_want_more = undefined,
                     orig_commands = OrigCmds},
        I4 =
            if
                I2#istate.stopped_by_user =:= suite ->
                    stopped_by_user(I2, I2#istate.stopped_by_user);
                I2#istate.debug orelse I2#istate.debug_file =/= undefined ->
                    DebugState = {attach, temporary},
                    {_, I3} = lux_debug:cmd_attach(I2, [], DebugState),
                    lux_debug:format("\nDebugger for lux. "
                                     "Try help or continue.\n",
                                     []),
                    I3;
                true ->
                    I2
            end,
        Res = loop(I4),
        {ok, Res}
    catch
        throw:{error, Reason, I5} ->
            {error, Reason, I5};
        error:Reason ->
            EST = erlang:get_stacktrace(),
            ErrBin = ?l2b(io_lib:format("~p", [Reason])),
            io:format("\nINTERNAL LUX ERROR: Interpreter crashed: ~s\n~p\n",
                      [ErrBin, EST]),
            {error, ErrBin, I}
    after
        safe_cancel_timer(Ref),
        EndTime = lux_utils:timestamp(),
        lux_interpret:ilog(I,
                           "lux(0): end_time \"~s\"\n",
                           [lux_utils:now_to_string(EndTime)])
    end.

collect_macros(#istate{orig_file = OrigFile} = I, OrigCmds) ->
    Collect =
        fun(Cmd, RevFile, _CmdStack, Acc) ->
                case Cmd of
                    #cmd{type = macro,
                         arg = {macro, Name, _ArgNames,
                                LineNo, _LastLineNo, _Body}} ->
                        RelFile = lux_utils:pretty_filename(RevFile),
                        AbsFile = filename:absname(RelFile),
                        MacroFile = lux_utils:normalize_filename(AbsFile),
                        case lists:keymember(Name, #macro.name, Acc) of
                            false ->
                                Macro = #macro{name = Name,
                                               file = MacroFile,
                                               cmd = Cmd},
                                [Macro | Acc];
                            true ->
                                Reason =
                                    [
                                     "Ambiguous macro ", Name, " at ",
                                     MacroFile,
                                     ":",
                                     ?i2l(LineNo)
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
             active_shell = undefined} = I) ->
    %% Stop main
    I;
loop(#istate{commands = [], call_level = CallLevel} = I)
  when CallLevel > 1 ->
    %% Stop include
    I2 = multisync(I, wait_for_expect),
    %% Check for stop and down before popping the cmd_stack
    sync_return(I2);
loop(I) ->
    Timeout = timeout(I),
    receive
        {debug_call, Pid, Cmd, CmdState} ->
            I2 = lux_debug:eval_cmd(I, Pid, Cmd, CmdState),
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
                    %% ilog(I, "~s(~p): ignore_more \"~s\"\n",
                    %%      [I#istate.active_name,
                    %%       (I#istate.latest_cmd)#cmd.lineno,
                    %%       Name]),
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
            I2 = opt_timeout_stop(I, TimeoutType, TimeoutMillis),
            loop(I2);
        Unexpected ->
            lux:trace_me(70, 'case', ignore_msg,
                         [{interpreter_got, Unexpected}]),
            ilog(I, "~s(~p): internal \"int_got_msg ~p\"\n",
                 [I#istate.active_name,
                  (I#istate.latest_cmd)#cmd.lineno, element(1, Unexpected)]),
             %% io:format("\nINTERNAL LUX ERROR: Interpreter got: ~p\n",
             %%           [Unexpected]),
             %% io:format("\nDEBUG(~p):\n\t~p\n",
             %%        [?LINE, process_info(self(), messages)]),
            loop(I)
    after multiply(I, Timeout) ->
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
    I#istate{loop_stack = lists:reverse(RevBreaks) ++ Acc};
break_loop(I, LoopCmd, [Loop|RevStack], Acc) ->
    break_loop(I, LoopCmd, RevStack, [Loop|Acc]);
break_loop(I, _LoopCmd, [], _Acc) ->
    %% Ignore missing loop
    I.

stopped_by_user(I, Scope) ->
    %% Ordered to stop by user
    ilog(I, "~s(~p): stopped_by_user\n",
         [I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno]),
    I2 =
        if
            I#istate.commands =:= I#istate.orig_commands ->
                I#istate{commands = []}; % Nothing to cleanup
            true ->
                I
        end,
    I3 = prepare_stop(I2, dummy_pid, {fail, stopped_by_user}),
    I3#istate{stopped_by_user = Scope, cleanup_reason = fail}.

timeout(I) ->
    if
        I#istate.want_more,
        not I#istate.blocked ->
            0;
        true ->
            infinity
    end.

opt_timeout_stop(I, TimeoutType, TimeoutMillis)
  when I#istate.has_been_blocked ->
    lux:trace_me(70, 'case', TimeoutType, [{ignored, TimeoutMillis}]),
    ilog(I, "~s(~p): ~p (ignored)\n",
         [I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno, TimeoutType]),
    io:format("WARNING: Ignoring ~p"
              " as the script has been attached by the debugger.\n",
              [TimeoutType]),
    I;
opt_timeout_stop(I, TimeoutType, TimeoutMillis) ->
    lux:trace_me(70, 'case', TimeoutType, [{premature, TimeoutMillis}]),
    Seconds = TimeoutMillis div timer:seconds(1),
    Multiplier = I#istate.multiplier / 1000,
    ilog(I, "~s(~p): ~p (~p seconds * ~.3f multiplier)\n",
         [I#istate.active_name,
          (I#istate.latest_cmd)#cmd.lineno,
          TimeoutType,
          Seconds,
          Multiplier]),
    premature_stop(I, {fail, TimeoutType}, {fail, TimeoutType}).

premature_stop(I, CleanupReason, StopRes) ->
    I2 = break_all_loops(I),
    case I2#istate.mode of
        running ->
            %% An error has occurred, maybe the test case (or suite)
            %% has timed out.
            prepare_stop(I2, dummy_pid, StopRes);
        cleanup ->
            %% Error (or timeout)  during cleanup

            %% Initiate stop by sending shutdown to all shells.
            multicast(I2, {shutdown, self()}),
            I#istate{mode = stopping, cleanup_reason = CleanupReason};
        stopping ->
            %% Shutdown has already been sent to the shells.
            %% Continue to collect their states.
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
            CallLevel = call_level(I),
            if
                CallLevel > 1 ->
                    I;
                I#istate.mode =:= stopping ->
                    %% Already stopping
                    I;
                true ->
                    %% Initiate stop by sending end_of_script to all shells.
                    multicast(I, {end_of_script, self()}),
                    I#istate{mode = stopping}
            end
    end.

dispatch_cmd(I,
             #cmd{lineno = LineNo,
                  type = Type,
                  arg = Arg} = Cmd) ->
    %% io:format("~p\n", [Cmd]),
    lux:trace_me(60, 'case', Type, [Cmd]),
    case Type of
        comment ->
            I;
        variable ->
            {Scope, Var, Val} = Arg,
            case safe_expand_vars(I, Val) of
                {ok, Val2} ->
                    QuotedVal = lux_utils:quote_newlines(Val2),
                    ilog(I, "~s(~p): ~p \"~s=~s\"\n",
                         [I#istate.active_name, LineNo, Scope, Var, QuotedVal]),
                    VarVal = lists:flatten([Var, $=, Val2]),
                    case Scope of
                        my ->
                            Vars = [VarVal | I#istate.macro_vars],
                            I#istate{macro_vars = Vars};
                        local when I#istate.active_shell =:= undefined ->
                            Reason = <<"The command must be executed"
                                       " in context of a shell">>,
                            handle_error(I, Reason);
                        local ->
                            add_active_var(I, VarVal);
                        global ->
                            I2 = add_active_var(I, VarVal),
                            Shells =
                                [S#shell{vars = [VarVal | S#shell.vars]} ||
                                    S <- I#istate.shells],
                            GlobalVars = [VarVal | I#istate.global_vars],
                            I2#istate{shells = Shells,
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
                    shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
                {bad_regexp, I2} ->
                    I2
            end;
        success ->
            case compile_regexp(I, Cmd, Arg) of
                {ok, Cmd2} ->
                    shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
                {bad_regexp, I2} ->
                    I2
            end;
        break ->
            case compile_regexp(I, Cmd, Arg) of
                {ok, _Cmd2} when I#istate.loop_stack =:= [] ->
                    Reason = <<"The command must be executed"
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
        change_timeout ->
            case Arg of
                "" ->
                    Millis = I#istate.default_timeout,
                    Cmd2 = Cmd#cmd{arg = Millis},
                    shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
                "infinity" ->
                    Millis = infinity,
                    Cmd2 = Cmd#cmd{arg = Millis},
                    shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
                SecsStr ->
                    case parse_int(I, SecsStr, Cmd) of
                        {ok, Secs} ->
                            Millis = timer:seconds(Secs),
                            Cmd2 = Cmd#cmd{arg = Millis},
                            shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
                        {bad_int, I2} ->
                            I2
                    end
            end;
        doc ->
            DisplayDoc =
                fun({Level, Doc}) ->
                        Indent = lists:duplicate((Level-1)*4, $\ ),
                        ilog(I, "~s(~p): doc \"~s~s\"\n",
                             [I#istate.active_name, LineNo, Indent, Doc]),
                        case I#istate.progress of
                            doc -> io:format("\n~s~s\n", [Indent, Doc]);
                            _   -> ok
                        end
                end,
            lists:foreach(DisplayDoc, Arg),
            I;
        config ->
            {config, Var, Val} = Arg,
            ilog(I, "~s(~p): config \"~s=~s\"\n",
                 [I#istate.active_name, LineNo, Var, Val]),
            I;
        cleanup ->
            ProgressStr =
                case I#istate.cleanup_reason of
                    normal -> "c";
                    _      -> "C"
                end,
            lux_utils:progress_write(I#istate.progress, ProgressStr),
            ilog(I, "~s(~p): cleanup\n",
                 [I#istate.active_name, LineNo]),
            multicast(I, {eval, self(), Cmd}),
            I2 = multisync(I, immediate),
            NewMode =
                if
                    I2#istate.mode =:= stopping ->
                        I2#istate.mode;
                    I2#istate.cleanup_reason =:= normal ->
                        I2#istate.mode;
                    true ->
                        cleanup
                end,
            I3 = inactivate_shell(I2, I2#istate.want_more),
            I3#istate.debug_shell =:= undefined orelse
                lux_debug:format("\nCleanup. "
                                 "Turn existing shells into zombies.\n",
                                 []),
            multicast(I3, {debug_shell, self(), disconnect}),
            Zombies = [S#shell{health = zombie} || S <- I3#istate.shells],
            I4 = I3#istate{mode = NewMode,
                           default_timeout = I3#istate.cleanup_timeout,
                           shells = Zombies,
                           debug_shell = undefined},
            Suffix =
                case call_level(I4) of
                    1 -> "";
                    N -> ?i2l(N)
                end,
            ShellCmd = Cmd#cmd{type = shell, arg = "cleanup" ++ Suffix},
            ensure_shell(I4, ShellCmd);
        shell ->
            ensure_shell(I, Cmd);
        include ->
            {include, InclFile, FirstLineNo, LastLineNo, InclCmds} = Arg,
            ilog(I, "~s(~p): include_file \"~s\"\n",
                 [I#istate.active_name, LineNo, InclFile]),
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
                    E = ?l2b(["Variable $", BadName, " is not set"]),
                    ilog(I, "~s(~p): ~s\n",
                         [I#istate.active_name, LineNo, E]),
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
                    ilog(I, "~s(~p): loop items \"~s\"\n",
                         [I#istate.active_name, LastLineNo, NewItemStr]),
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

shell_eval(I, Cmd) ->
    dlog(I, ?dmore, "want_more=false (send ~p)", [Cmd#cmd.type]),
    case cast(I, {eval, self(), Cmd}) of
        {ok, _Pid} ->
            I#istate{want_more = false};
        {bad_shell, I2} ->
            I2
    end.

eval_include(OldI, InclLineNo, FirstLineNo, LastLineNo,
             InclFile, InclCmds, InclCmd) ->
    DefaultFun = get_eval_fun(),
    eval_body(OldI, InclLineNo, FirstLineNo, LastLineNo,
              InclFile, InclCmds, InclCmd, DefaultFun, false).

get_eval_fun() ->
    fun(I) when is_record(I, istate) -> loop(I) end.

eval_body(OldI, InvokeLineNo, FirstLineNo, LastLineNo,
          CmdFile, Body, #cmd{type = Type} = Cmd, Fun, IsRootLoop) ->
    Enter =
        fun() ->
                ilog(OldI, "file_enter ~p ~p ~p ~p\n",
                     [InvokeLineNo, FirstLineNo, LastLineNo, CmdFile])
        end,
    OldStack = OldI#istate.cmd_stack,
    CurrentPos =
        #cmd_pos{rev_file = lux_utils:filename_split(CmdFile),
                 lineno = InvokeLineNo,
                 type = Type},
    NewStack = [CurrentPos | OldStack],
    BeforeI = OldI#istate{call_level = call_level(OldI) + 1,
                          file = CmdFile,
                          latest_cmd = Cmd,
                          cmd_stack = NewStack,
                          commands = Body},
    BeforeI2 = adjust_stacks(before, BeforeI, Cmd, NewStack,
                             Enter, IsRootLoop),
    try
        lux_utils:progress_write(BeforeI2#istate.progress, "("),
        AfterI = Fun(BeforeI2),
        lux_utils:progress_write(AfterI#istate.progress, ")"),
        AfterExit =
            fun() ->
                    catch ilog(AfterI, "file_exit ~p ~p ~p ~p\n",
                               [InvokeLineNo, FirstLineNo, LastLineNo,
                                CmdFile])
            end,
        AfterI2 = adjust_stacks('after', AfterI, Cmd, OldStack,
                                AfterExit, IsRootLoop),
        NewI = AfterI2#istate{call_level = call_level(OldI),
                              file = OldI#istate.file,
                              latest_cmd = OldI#istate.latest_cmd,
                              cmd_stack = OldI#istate.cmd_stack,
                              commands = OldI#istate.commands},
        if
            NewI#istate.cleanup_reason =:= normal ->
                %% Everything OK - no cleanup needed
                NewI#istate{default_timeout = OldI#istate.default_timeout};
            OldI#istate.cleanup_reason =:= normal ->
                %% New cleanup initiated in body - continue on this call level
                goto_cleanup(NewI, NewI#istate.cleanup_reason);
            true ->
                %% Already cleaning up when we started eval of body
                NewI
        end
    catch
        Class:Reason ->
            EST = erlang:get_stacktrace(),
            lux_utils:progress_write(OldI#istate.progress, ")"),
            BeforeExit =
                fun() ->
                        catch ilog(BeforeI2, "file_exit ~p ~p ~p ~p\n",
                                   [InvokeLineNo, FirstLineNo, LastLineNo,
                                    CmdFile])
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

call_level(#istate{call_level = CallLevel}) ->
    CallLevel.

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
            ilog(I, "~s(~p): invoke_~s \"~s\"\n",
                 [I#istate.active_name,
                  LineNo,
                  Name,
                  lists:flatten(string:join(MacroVars, " "))]),

            BeforeI = I#istate{macro_vars = MacroVars, latest_cmd = InvokeCmd},
            DefaultFun = get_eval_fun(),
            AfterI = eval_body(BeforeI, LineNo, FirstLineNo, LastLineNo,
                               MacroFile, Body, MacroCmd, DefaultFun, false),

            ilog(I, "~s(~p): exit_~s \"~s\"\n",
                 [I#istate.active_name,
                  LineNo,
                  Name,
                  lists:flatten(string:join(MacroVars, " "))]),
            AfterI#istate{macro_vars = OldMacroVars};
        {bad_vars, I2} ->
            I2
    end;
invoke_macro(I, #cmd{arg = {invoke, Name, _Values}}, []) ->
    BinName = ?l2b(Name),
    handle_error(I, <<"No such macro: ", BinName/binary>>);
invoke_macro(I, #cmd{arg = {invoke, Name, _Values}}, [_|_]) ->
    BinName = ?l2b(Name),
    handle_error(I, <<"Ambiguous macro: ", BinName/binary>>).

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
    E = ?l2b(["Variable $", BadName, " is not set"]),
    ilog(I, "~s(~p): ~s\n", [I#istate.active_name, LineNo, E]),
    OrigLine = lux_utils:strip_leading_whitespaces(Cmd#cmd.orig),
    handle_error(I, <<E/binary, ". Bad line: ", OrigLine/binary>>).

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
                               ilog(I, "~s(~p): loop forever\n",
                                    [I#istate.active_name,
                                     LoopCmd#cmd.lineno]),
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
                                       ilog(I, "~s(~p): loop \"~s\"\n",
                                            [I#istate.active_name,
                                             First,
                                             LoopVar]),
                                       LoopFun(I)
                               end,
                               false),
            do_eval_loop(AfterI, Name, Rest, First, Last, Body, LoopCmd,
                         LoopFun, N+1);
        endloop ->
            ilog(OldI, "~s(~p): endloop \"~s\"\n",
                 [OldI#istate.active_name, Last, Name]),
            OldI
    end.

pick_item([Item|Items])                                                 ->
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

prepare_stop(#istate{results = Acc} = I, Pid, Res) ->
    %% Handle stop procedure
    {CleanupReason, Res3} = prepare_result(I, Res),
    NewLevel =
        case Res3#result.actual of
            internal_error -> ?dmore;
            _              -> I#istate.debug_level
        end,
    I2 = I#istate{results = [Res3 | Acc],
                  debug_level = NewLevel}, % Activate debug after first error
    {ShellName, I3} = delete_shell(I2, Pid),
    lux:trace_me(50, 'case', stop,
                 [{mode, I3#istate.mode},
                  {stop, ShellName, Res3#result.outcome, Res3#result.actual},
                  {active_shell, I3#istate.active_shell},
                  {shells, I3#istate.shells},
                  Res3]),
    case I3#istate.mode of
        running ->
            multicast(I3, {relax, self()}),
            goto_cleanup(I3, CleanupReason);
        cleanup when Res#result.outcome =:= relax -> % Orig outcome
            I3; % Continue with cleanup
        cleanup ->
            %% Initiate stop by sending shutdown to the remaining shells.
            multicast(I3, {shutdown, self()}),
            I3#istate{mode = stopping};
        stopping ->
            %% Shutdown has already been sent to the other shells.
            %% Continue to collect their states if needed.
            I3
    end.

prepare_result(#istate{latest_cmd = LatestCmd,
                       cmd_stack = CmdStack,
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
                {ExpectedTag, Expected} = lux_utils:cmd_expected(LatestCmd),
                {fail,
                 #result{outcome      = fail,
                         latest_cmd   = LatestCmd,
                         cmd_stack    = CmdStack,
                         expected_tag = ExpectedTag,
                         expected     = Expected,
                         extra        = undefined,
                         actual       = FailReason,
                         rest         = fail}};
            {fail, FailReason} ->
                {ExpectedTag, Expected} = lux_utils:cmd_expected(LatestCmd),
                {fail,
                 #result{outcome      = fail,
                         latest_cmd   = LatestCmd,
                         cmd_stack    = CmdStack,
                         expected_tag = ExpectedTag,
                         expected     = Expected,
                         extra        = undefined,
                         actual       = FailReason,
                         rest         = fail}}
        end,
    Res3 =
        case Res2 of
            #result{actual = <<?fail_pattern_matched, _/binary>>} ->
                Res2#result{latest_cmd = LatestCmd,
                            cmd_stack = CmdStack};
            #result{actual = <<?success_pattern_matched, _/binary>>} ->
                Res2#result{latest_cmd = LatestCmd,
                            cmd_stack = CmdStack};
            _ ->
                Res2
        end,
    {CleanupReason, Res3}.

goto_cleanup(OldI, CleanupReason) ->
    lux:trace_me(50, 'case', goto_cleanup, [{reason, CleanupReason}]),
    LineNo = ?i2l((OldI#istate.latest_cmd)#cmd.lineno),
    NewLineNo =
        case OldI#istate.results of
            [#result{actual= <<"fail pattern matched ", _/binary>>}|_] ->
                "-" ++ LineNo;
            [#result{actual= <<"success pattern matched ", _/binary>>}|_] ->
                "+" ++ LineNo;
            [#result{outcome = success}|_] ->
                "";
            _ ->
                LineNo
        end,
    lux_utils:progress_write(OldI#istate.progress, NewLineNo),

    %% Ensure that the cleanup does not take too long time
    safe_send_after(OldI, OldI#istate.case_timeout, self(),
                    {case_timeout, OldI#istate.case_timeout}),
    dlog(OldI, ?dmore, "want_more=true (goto_cleanup)", []),
    do_goto_cleanup(OldI, CleanupReason, LineNo).

do_goto_cleanup(I, CleanupReason, LineNo) ->
    case I#istate.cmd_stack of
        []                             -> Context = main;
        [#cmd_pos{type = Context} | _] -> ok
    end,
    %% Fast forward to (optional) cleanup command
    CleanupFun = fun(#cmd{type = CmdType}) -> CmdType =/= cleanup end,
    CleanupCmds = lists:dropwhile(CleanupFun, I#istate.commands),
    case CleanupCmds of
        [#cmd{lineno = CleanupLineNo} | _] ->
            ilog(I, "~s(~s): goto cleanup at line ~p\n",
                 [I#istate.active_name, LineNo, CleanupLineNo]);
        [] ->
            ilog(I, "~s(~s): no cleanup\n",
                 [I#istate.active_name, LineNo])
    end,
    NewMode =
        if
            I#istate.mode =/= stopping ->
                cleanup;
            Context =:= main ->
                %% Initiate stop by sending shutdown to the remaining shells.
                multicast(I, {shutdown, self()}),
                stopping;
            true ->
                stopping
        end,
    %% Break active loops
    LoopStack = [L#loop{mode = break} || L <- I#istate.loop_stack],
    I#istate{mode = NewMode,
             loop_stack = LoopStack,
             cleanup_reason = CleanupReason,
             want_more = true,
             commands = CleanupCmds}.

delete_shell(I, Pid) ->
    ActiveShell = I#istate.active_shell,
    OldShells = I#istate.shells,
    case lists:keyfind(Pid, #shell.pid, [ActiveShell | OldShells]) of
        false ->
            {Pid, I};
        #shell{ref = Ref, name = Name} ->
            erlang:demonitor(Ref, [flush]),
            if
                Pid =:= ActiveShell#shell.pid ->
                    I2 = inactivate_shell(I, I#istate.want_more),
                    {Name, I2#istate{shells = OldShells}};
                true ->
                    NewShells = lists:keydelete(Pid, #shell.pid, OldShells),
                    {Name, I#istate{shells = NewShells}}
            end
    end.

multicast(#istate{shells = OtherShells, active_shell = undefined}, Msg) ->
    multicast(OtherShells, Msg);
multicast(#istate{shells = OtherShells, active_shell = ActiveShell}, Msg) ->
    multicast([ActiveShell | OtherShells], Msg);
multicast(Shells, Msg) when is_list(Shells) ->
    lux:trace_me(50, 'case', multicast, [{shells, Shells}, Msg]),
    Send = fun(#shell{pid = Pid} = S) -> trace_msg(S, Msg), Pid ! Msg, Pid end,
    lists:map(Send, Shells).

cast(#istate{active_shell = undefined} = I, _Msg) ->
    Reason = <<"The command must be executed in context of a shell">>,
    {bad_shell, handle_error(I, Reason)};
cast(#istate{active_shell = #shell{pid =Pid}, active_name = Name}, Msg) ->
    trace_msg(#shell{name=Name}, Msg),
    Pid ! Msg,
    {ok, Pid}.

trace_msg(#shell{name = Name}, Msg) ->
    lux:trace_me(50, 'case', Name, element(1, Msg), [Msg]).

multisync(I, When) when When =:= flush;
                        When =:= immediate;
                        When =:= wait_for_expect ->
    Pids = multicast(I, {sync, self(), When}),
    lux:trace_me(50, 'case', waiting,
                 [{active_shell, I#istate.active_shell},
                  {shells, I#istate.shells},
                  When]),
    I2 = wait_for_reply(I, Pids, sync_ack, undefined, infinity),
    lux:trace_me(50, 'case', collected, []),
    I2.

wait_for_reply(I, [Pid | Pids], Expect, Fun, FlushTimeout) ->
    receive
        {Expect, Pid} ->
            wait_for_reply(I, Pids, Expect, Fun, FlushTimeout);
        %%      {Expect, Pid, Expected} when Expect =:= expected, Pids =:= [] ->
        %%          Expected;
        {stop, SomePid, Res} ->
            I2 = prepare_stop(I, SomePid, Res),
            %% Flush spurious message
            receive {Expect, SomePid} -> ok after 0 -> ok end,
            OtherPids = [Pid|Pids] -- [SomePid],
            wait_for_reply(I2, OtherPids, Expect, Fun, FlushTimeout);
        {'DOWN', _, process, Pid, Reason} ->
            opt_apply(Fun),
            shell_crashed(I, Pid, Reason);
        {TimeoutType, TimeoutMillis} when TimeoutType =:= suite_timeout;
                                          TimeoutType =:= case_timeout ->
            I2 = opt_timeout_stop(I, TimeoutType, TimeoutMillis),
            wait_for_reply(I2, [Pid|Pids], Expect, Fun, 500);
        Unexpected when FlushTimeout =/= infinity ->
            lux:trace_me(70, 'case', ignore_msg,
                         [{interpreter_got,Unexpected}]),
            ilog(I, "~s(~p): internal \"int_got_msg ~p\"\n",
                 [I#istate.active_name,
                  (I#istate.latest_cmd)#cmd.lineno, element(1, Unexpected)]),
            %% io:format("\nINTERNAL LUX ERROR: Interpreter got: ~p\n",
            %%           [Unexpected]),
            %% io:format("DEBUG(~p): ~p ~p\n\t~p\n\t~p\n\t~p\n",
            %%           [?LINE, Expect, [Pid|Pids],
            %%            process_info(self(), messages),
            %%            process_info(Pid, messages),
            %%            process_info(Pid, current_stacktrace)]),
            wait_for_reply(I, [Pid|Pids], Expect, Fun, FlushTimeout)
    after FlushTimeout ->
            I
    end;
wait_for_reply(I, [], _Expect, _Fun, _FlushTimeout) ->
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

ensure_shell(I, #cmd{arg = ""}) ->
    %% No name. Inactivate the shell
    inactivate_shell(I, I#istate.want_more);
ensure_shell(I, #cmd{lineno = LineNo, arg = Name} = Cmd) ->
    case safe_expand_vars(I, Name) of
        {ok, Name2} when I#istate.active_shell#shell.name =:= Name2 ->
            %% Keep active shell
            I;
        {ok, Name2} ->
            I2 = I#istate{want_more = false},
            case lists:keyfind(Name2, #shell.name, I2#istate.shells) of
                false ->
                    %% New shell
                    shell_start(I2, Cmd#cmd{arg = Name2});
                Shell ->
                    %% Existing shell
                    shell_switch(I2, Cmd, Shell)
            end;
        {no_such_var, BadName} ->
            no_such_var(I, Cmd, LineNo, BadName)
    end.

shell_start(I, #cmd{arg = Name} = Cmd) ->
    case change_active_mode(I, Cmd, suspend) of
        {ok, I2} ->
            I3 = inactivate_shell(I2, I2#istate.want_more),
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
    Wait = Cmd#cmd{type = expect,
                   arg = {regexp, single, <<".+">>}},
    %% Set the prompt (after the rc files has ben run)
    CmdStr = ?l2b([I#istate.shell_prompt_cmd, "\n"]),
    Prompt = Cmd#cmd{type = send,
                     arg = CmdStr},
    %% Wait for the prompt
    CmdRegExp = ?l2b(I#istate.shell_prompt_regexp),
    Sync = Cmd#cmd{type = expect,
                   arg = {regexp, single, CmdRegExp}},
    Cmds = [Wait, Prompt, Sync | I#istate.commands],
    dlog(I, ?dmore, "want_more=false (shell_start)", []),
    I#istate{commands = Cmds}.

shell_switch(OldI, Cmd, #shell{health = alive, name = NewName} = NewShell) ->
    %% Activate shell
    case change_active_mode(OldI, Cmd, suspend) of
        {ok, I2} ->
            I3 = inactivate_shell(I2, I2#istate.want_more),
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
    ilog(OldI, "~s(~p): zombie shell at cleanup\n",
         [Name, (OldI#istate.latest_cmd)#cmd.lineno]),
    handle_error(OldI, ?l2b(Name ++ " is a zombie shell")).

inactivate_shell(#istate{active_shell = undefined} = I, _WantMore) ->
    I;
inactivate_shell(#istate{active_shell = ActiveShell, shells = Shells} = I,
                 WantMore) ->
    I#istate{active_shell = undefined,
             active_name = "lux",
             want_more = WantMore,
             shells = [ActiveShell | Shells]}.

change_active_mode(I, Cmd, NewMode)
  when is_pid(I#istate.active_shell#shell.pid) ->
    case cast(I, {change_mode, self(), NewMode, Cmd, I#istate.cmd_stack}) of
        {ok, Pid} ->
            I2 = wait_for_reply(I, [Pid], change_mode_ack, undefined, infinity),
            {ok, I2};
        {bad_shell, I2} ->
            {bad_mode, I2}
    end;
change_active_mode(I, _Cmd, _NewMode) ->
    %% No active shell
    {ok, I}.

adjust_stacks(When, #istate{active_shell = undefined} = I,
              NewCmd, CmdStack, Fun, IsRootLoop) ->
    Fun(),
    adjust_suspended_stacks(When, I, NewCmd, CmdStack, IsRootLoop, []);
adjust_stacks(When, #istate{active_shell = #shell{pid = ActivePid}} = I,
           NewCmd, CmdStack, Fun, IsRootLoop) ->
    Msg = {adjust_stacks, self(), When, IsRootLoop, NewCmd, CmdStack, Fun},
    case cast(I, Msg) of
        {ok, ActivePid} ->
            adjust_suspended_stacks(When, I, NewCmd, CmdStack,
                                    IsRootLoop, [ActivePid]);
        {bad_shell, I2} ->
            I2
    end.

adjust_suspended_stacks(When, I, NewCmd, CmdStack, IsRootLoop, ActivePids) ->
    Fun = fun() -> ignore end,
    Msg = {adjust_stacks, self(), When, IsRootLoop, NewCmd, CmdStack, Fun},
    OtherPids = multicast(I#istate{active_shell = undefined}, Msg),
    Pids = ActivePids ++ OtherPids,
    wait_for_reply(I, Pids, adjust_stacks_ack, Fun, infinity).

shell_crashed(I, Pid, Reason) when Pid =:= I#istate.active_shell#shell.pid ->
    I2 = inactivate_shell(I, I#istate.want_more),
    shell_crashed(I2, Pid, Reason);
shell_crashed(I, Pid, Reason) ->
    What =
        case lists:keyfind(Pid, #shell.pid, I#istate.shells) of
            false -> ["Process ", io_lib:format("~p", [Pid])];
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
                    builtin_vars  = BuiltinVars,
                    system_vars   = SystemVars},
            Val,
            MissingVar) ->
    case Shell of
        #shell{vars = LocalVars} -> ok;
        undefined                -> LocalVars = OptGlobalVars
    end,
    Varss = [SubVars, MacroVars, LocalVars, BuiltinVars, SystemVars],
    lux_utils:expand_vars(Varss, Val, MissingVar).

add_active_var(#istate{active_shell = undefined} = I, _VarVal) ->
    I;
add_active_var(#istate{active_shell = Shell} = I, VarVal) ->
    LocalVars = [VarVal | Shell#shell.vars],
    Shell2 = Shell#shell{vars = LocalVars},
    I#istate{active_shell = Shell2}.

ilog(#istate{progress = Progress, log_fun = LogFun, event_log_fd = Fd},
     Format,
     Args)->
    lux_log:safe_format(Progress, LogFun, Fd, Format, Args).

dlog(I, Level, Format, Args) when I#istate.debug_level >= Level ->
    ilog(I, "~s(~p): debug2 \"" ++ Format ++ "\"\n",
         [I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno] ++ Args);
dlog(_I, _Level, _Format, _Args) ->
    ok.

safe_send_after(State, Timeout, Pid, Msg) ->
    case multiply(State, Timeout) of
        infinity   -> infinity;
        NewTimeout -> erlang:send_after(NewTimeout, Pid, Msg)
    end.

safe_cancel_timer(Timer) ->
    case Timer of
        infinity  -> undefined;
        undefined -> undefined;
        Ref       -> erlang:cancel_timer(Ref)
    end.

multiply(#istate{multiplier = Factor}, Timeout) ->
    case Timeout of
        infinity -> infinity;
        _        -> lux_utils:multiply(Timeout, Factor)
    end.

handle_error(#istate{active_shell = ActiveShell,
                     shells = Shells,
                     latest_cmd = Cmd} = I, Reason)
  when is_binary(Reason) ->
    lux:trace_me(50, 'case', error,
                 [{active_shell, ActiveShell},
                  {shells, Shells},
                  {cmd, Cmd},
                  {reason, Reason}]),
    ilog(I, "~s(~p): error \"~s\"\n",
         [I#istate.active_name,
          (I#istate.latest_cmd)#cmd.lineno, Reason]),
    premature_stop(I, error, {'EXIT', {error, Reason}}).
