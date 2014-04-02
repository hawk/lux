%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2012 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_interpret).

-include("lux.hrl").

-export([interpret_commands/3,
         default_istate/1,
         parse_iopts/2,
         parse_iopt/3
        ]).
-export([opt_dispatch_cmd/1,
         flush_logs/1]).

interpret_commands(Script, Commands, Opts) ->
    I = default_istate(Script),
    I2 = I#istate{commands = Commands, orig_commands = Commands},
    try
        case parse_iopts(I2, Opts) of
            {ok, I3} ->
                LogDir = I3#istate.log_dir,
                Config = config_data(I3),
                ConfigFd = lux_log:open_config_log(LogDir, Script, Config),
                Flag = process_flag(trap_exit, true),
                Progress = I3#istate.progress,
                LogFun = I3#istate.log_fun,
                Verbose = true,
                case lux_log:open_event_log(LogDir, Script, Progress,
                                            LogFun, Verbose) of
                    {ok, EventLog, EventFd} ->
                        try
                            I4 = I3#istate{event_log_fd = {Verbose, EventFd},
                                           config_log_fd = {Verbose, ConfigFd}},
                            lux_log:safe_format(Progress, LogFun, undefined,
                                                "~s~s\n",
                                                [?TAG("script"),
                                                 I4#istate.file]),
                            lux_log:safe_format(Progress, LogFun, undefined,
                                                "~s~s\n",
                                                [?TAG("event log"), EventLog]),
                            lux_utils:progress_write(Progress,
                                                     ?TAG("progress")),
                            ReplyTo = self(),
                            Interpret =
                                fun() ->
                                        lux_debug:start_link(I3),
                                        Res = interpret_init(I4),
                                        unlink(ReplyTo),
                                        ReplyTo ! {done, self(), Res},
                                        exit(shutdown)
                                end,
                            Pid = spawn_link(Interpret),
                            wait_for_done(I4, Pid)
                        after
                            process_flag(trap_exit, Flag),
                            lux_log:close_event_log(EventFd),
                            lux_log:close_config_log(ConfigFd, I3#istate.logs)
                        end;
                    {error, FileReason} ->
                        internal_error(I3, file:format_error(FileReason))
                end;
            {error, ParseReason} ->
                internal_error(I2, ParseReason)
        end
    catch
        error:FatalReason ->
            internal_error(I2, {'EXIT', FatalReason});
        Class:Reason ->
            internal_error(I2, {'EXIT', {fatal_error, Class, Reason}})
    end.

internal_error(I, ReasonTerm) ->
    ReasonBin = list_to_binary(io_lib:format("Internal error: ~p",
                                             [ReasonTerm])),
    fatal_error(I, ReasonBin).

fatal_error(I, ReasonBin) when is_binary(ReasonBin) ->
    FullLineNo = full_lineno(I, I#istate.latest_lineno, I#istate.incl_stack),
    double_ilog(I, "~sERROR ~s\n",
                [?TAG("result"),
                 binary_to_list(ReasonBin)]),
    {error, I#istate.file, FullLineNo, ReasonBin}.

parse_iopts(I, [{Name, Val} | T]) when is_atom(Name) ->
    case parse_iopt(I, Name, Val) of
        {ok, I2} ->
            parse_iopts(I2, T);
        {error, Reason} ->
            {error, Reason}
    end;
parse_iopts(I, []) ->
    File = filename:absname(I#istate.file),
    case I#istate.shell_wrapper of
        "" -> ShellWrapper = undefined;
        ShellWrapper -> ok
    end,
    I2 = I#istate{file = File,
                  orig_file = File,
                  shell_wrapper = ShellWrapper,
                  log_dir = filename:absname(I#istate.log_dir)},
    {ok, I2}.

parse_iopt(I, Name, Val) when is_atom(Name) ->
    case config_type(Name) of
        {ok, Pos, Types} ->
            config_val(Types, Name, Val, Pos, I);
        {error, Reason} ->
            {error, Reason}
    end.

config_type(Name) ->
    case Name of
        debug  ->
            {ok, #istate.debug, [{atom, [true, false]}]};
        debug_file  ->
            {ok, #istate.debug_file, [string, {atom, [undefined]}]};
        skip ->
            {ok, #istate.skip, [{list, [string]}]};
        skip_unless ->
            {ok, #istate.skip_unless, [{list, [string]}]};
        require ->
            {ok, #istate.require, [{list, [string]}]};
        config_dir ->
            {ok, #istate.config_dir, [{list, [string]}]};
        progress ->
            {ok, #istate.progress,
             [{atom, [silent, brief, doc, compact, verbose]}]};
        log_dir ->
            {ok, #istate.log_dir, [string]};
        log_fun->
            {ok, #istate.log_fun, [{function, 1}]};
        log_fd->
            {ok, #istate.summary_log_fd, [io_device]};
        multiplier ->
            {ok, #istate.multiplier, [{integer, 0, infinity}]};
        suite_timeout ->
            {ok, #istate.suite_timeout, [{integer, 0, infinity},
                                         {atom, [infinity]}]};
        case_timeout ->
            {ok, #istate.case_timeout, [{integer, 0, infinity},
                                        {atom, [infinity]}]};
        flush_timeout ->
            {ok, #istate.flush_timeout, [{integer, 0, infinity}]};
        poll_timeout ->
            {ok, #istate.poll_timeout, [{integer, 0, infinity}]};
        timeout ->
            {ok, #istate.timeout, [{integer, 0, infinity},
                                   {atom, [infinity]}]};
        cleanup_timeout ->
            {ok, #istate.cleanup_timeout, [{integer, 0, infinity},
                                           {atom, [infinity]}]};
        shell_wrapper ->
            {ok, #istate.shell_wrapper, [string,
                                         {atom, [undefined]}]};
        shell_cmd ->
            {ok, #istate.shell_cmd, [string]};
        shell_args ->
            {ok, #istate.shell_args, [{list, [string]}]};
        var ->
            {ok, #istate.dict, [{list, [string]}]};
        _ ->
            {error, Name}
    end.

config_val([Type | Types], Name, Val, Pos, I) ->
    try
        case Type of
            string when is_list(Val) ->
                Val2 = expand_vars(I, Val, error),
                {ok, setelement(Pos, I, Val2)};
            binary when is_binary(Val) ->
                Val2 = expand_vars(I, Val, error),
                {ok, setelement(Pos, I, Val2)};
            binary when is_list(Val) ->
                Val2 = expand_vars(I, Val, error),
                config_val([Type], Name, list_to_binary(Val2), Pos, I);
            {atom, Atoms} when is_atom(Val) ->
                true = lists:member(Val, Atoms),
                {ok, setelement(Pos, I, Val)};
            {atom, _Atoms} when is_list(Val) ->
                config_val([Type], Name, list_to_atom(Val), Pos, I);
            {function, Arity} when is_function(Val, Arity) ->
                {ok, setelement(Pos, I, Val)};
            {integer, infinity, infinity} when is_integer(Val) ->
                {ok, setelement(Pos, I, Val)};
            {integer, infinity, Max}
              when is_integer(Val), is_integer(Max), Val =< Max ->
                {ok, setelement(Pos, I, Val)};
            {integer, Min, infinity}
              when is_integer(Val), is_integer(Min), Val >= Min ->
                {ok, setelement(Pos, I, Val)};
            {integer, Min, Max}
              when is_integer(Val), is_integer(Min), is_integer(Max),
                   Val >= Min, Val =< Max ->
                {ok, setelement(Pos, I, Val)};
            {integer, _Min, _Max} when is_list(Val) ->
                config_val([Type], Name, list_to_integer(Val), Pos, I);
            {list, SubTypes} when is_list(SubTypes) ->
                config_val(SubTypes, Name, Val, Pos, I);
            io_device ->
                {ok, setelement(Pos, I, Val)}
        end
    catch
        throw:{no_such_var, BadName} ->
            {error, lists:concat(["Bad argument: ", Name, "=", Val,
                                  "; $", BadName, " is not set"])};
        _:_ ->
            config_val(Types, Name, Val, Pos, I)
    end;
config_val([], Name, Val, _Pos, _I) ->
    {error, lists:concat(["Bad argument: ", Name, "=", Val])}.

expand_vars(#istate{macro_dict   = MacroDict,
                    dict         = Dict,
                    builtin_dict = BuiltinDict,
                    system_dict  = SystemDict},
            Val,
            MissingVar) ->
    Dicts = [MacroDict, Dict, BuiltinDict, SystemDict],
    lux_utils:expand_vars(Dicts, Val, MissingVar).

wait_for_done(I, Pid) ->
    receive
        {suite_timeout, SuiteTimeout} ->
            %% double_ilog(I, "\n~s~p\n",
            %%             [?TAG("suite timeout"),
            %%              SuiteTimeout]),
            Pid ! {suite_timeout, SuiteTimeout},
            wait_for_done(I, Pid);
        {done, Pid, Res} ->
            lux_utils:progress_write(I#istate.progress, "\n"),
            case Res of
                {ok, I2} ->
                    I3 = post_ilog(I2),
                    File = I3#istate.file,
                    Results = I3#istate.results,
                    case lists:keyfind('EXIT', 1, Results) of
                        false ->
                            Pos = #result.outcome,
                            case lists:keyfind(fail, Pos, Results) of
                                false ->
                                    Reason = I3#istate.cleanup_reason,
                                    if
                                        Reason =:= normal;
                                        Reason =:= success ->
                                            print_success(I3, File, Results);
                                        true ->
                                            Latest = I3#istate.latest_lineno,
                                            Stack = I3#istate.incl_stack,
                                            R = #result{outcome    = fail,
                                                        lineno     = Latest,
                                                        incl_stack = Stack,
                                                        expected   = success,
                                                        extra      = undefined,
                                                        actual     = Reason,
                                                        rest       = fail},
                                            print_fail(I, File, Results, R)
                                    end;
                                #result{outcome = fail} = Fail ->
                                    print_fail(I, File, Results, Fail)
                            end;
                        {'EXIT', Reason} ->
                            I3 = post_ilog(I2),
                            internal_error(I3, {'EXIT', Reason})
                    end;
                {error, ReasonBin, I2} ->
                    I3 = post_ilog(I2),
                    fatal_error(I3, ReasonBin)
            end;
        {'EXIT', _Pid, Reason} ->
            I2 = post_ilog(I),
            internal_error(I2, {'EXIT', Reason})
    end.

print_success(I, File, Results) ->
    double_ilog(I, "~sSUCCESS\n", [?TAG("result")]),
    L = length(I#istate.commands),
    FullLineNo = integer_to_list(L),
    {ok, File, success, FullLineNo, Results}.

print_fail(I, File, Results,
           #result{outcome    = fail,
                   lineno     = LineNo,
                   incl_stack = InclStack,
                   expected   = Expected,
                   extra      = _Extra,
                   actual     = Actual,
                   rest       = Rest}) ->
    FullLineNo = full_lineno(I, LineNo, InclStack),
    double_ilog(I, "~sFAIL at ~s:~s\n",
                [?TAG("result"), File, FullLineNo]),
    io:format("expected\n\t~s\n",
              [simple_to_string(Expected)]),
    double_ilog(I, "expected\n\"~s\"\n",
                [lux_utils:to_string(Expected)]),
    if
        is_atom(Actual) ->
            io:format("actual ~p\n\t~s\n",
                      [Actual, simple_to_string(Rest)]),
            double_ilog(I, "actual ~p\n\"~s\"\n",
                        [Actual, lux_utils:to_string(Rest)]);
        is_binary(Actual) ->
            io:format("actual error\n\t~s\n",
                      [simple_to_string(Actual)]),
            double_ilog(I, "actual error\n\"~s\"\n",
                        [lux_utils:to_string(Actual)])
    end,
    {ok, File, fail, FullLineNo, Results}.

full_lineno(I, LineNo, InclStack) ->
    RevFile = lux_utils:filename_split(I#istate.file),
    FullStack = [{RevFile, LineNo} | InclStack],
    lux_utils:full_lineno(FullStack).

flush_logs(I) ->
    flush_summary_log(I),
    multi_ping(I, flush).

flush_summary_log(#istate{summary_log_fd=undefined}) ->
    ok;
flush_summary_log(#istate{summary_log_fd=SummaryFd}) ->
    file:sync(SummaryFd).

post_ilog(#istate{logs = Logs, config_log_fd = {_, ConfigFd}}=I) ->
    lux_log:close_config_log(ConfigFd, Logs),
    log_doc(I),
    ilog(I, "\n", []),
    I#istate{progress = silent,
             log_fun = fun(Bin) ->
                               console_write(binary_to_list(Bin)),
                               (I#istate.log_fun)(Bin),
                               Bin
                       end}.

log_doc(#istate{log_fun = LogFun, orig_file = File, orig_commands = Cmds}) ->
    Prefix = list_to_binary(?TAG("doc")),
    Fun =
        fun(#cmd{type = doc, arg = {Level, Doc}}, _RevFile, _InclStack, Acc) ->
                Tabs = list_to_binary(lists:duplicate(Level-1, $\t)),
                LogFun(<<Prefix/binary, Tabs/binary, Doc/binary, "\n">>),
                Acc;
           (_, _RevFile, _FileStack, Acc) ->
                Acc
        end,
    lux_utils:foldl_cmds(Fun, ok, File, [], Cmds).

simple_to_string(Atom) when is_atom(Atom) ->
    simple_to_string(atom_to_list(Atom));
simple_to_string(Bin) when is_binary(Bin) ->
    simple_to_string(binary_to_list(Bin));
simple_to_string([H | T]) when is_integer(H) ->
    case H of
        $\r ->
            simple_to_string(T);
        $\n ->
            [$\n, $\t | simple_to_string(T)];
        Char ->
            [Char | simple_to_string(T)]
    end;
simple_to_string([H | T]) ->
    simple_to_string(H) ++ simple_to_string(T);
simple_to_string([]) ->
    [].

config_data(I) ->
    [
     {'config log',    string, ?CONFIG_LOG_VERSION},
     {script,          string, I#istate.file},
     {debug,           term,   I#istate.debug},
     {debug_file,      term,   I#istate.debug_file},
     {progress,        term,   I#istate.progress},
     {skip,            term,   I#istate.skip},
     {skip_unless,     term,   I#istate.skip_unless},
     {require,         term,   I#istate.require},
     {log_dir,         string, I#istate.log_dir},
     {multiplier,      term,   I#istate.multiplier},
     {suite_timeout,   term,   I#istate.suite_timeout},
     {case_timeout,    term,   I#istate.case_timeout},
     {flush_timeout,   term,   I#istate.flush_timeout},
     {poll_timeout,    term,   I#istate.poll_timeout},
     {timeout,         term,   I#istate.timeout},
     {cleanup_timeout, term,   I#istate.cleanup_timeout},
     {shell_wrapper,   term,   I#istate.shell_wrapper},
     {shell_cmd,       term,   I#istate.shell_cmd},
     {shell_args,      term,   I#istate.shell_args},
     {var,             dict,   I#istate.dict},
     {builtin,         dict,   I#istate.builtin_dict},
     {system_env,      dict,   I#istate.system_dict}
    ].

interpret_init(I) ->
    Ref = safe_send_after(I, I#istate.case_timeout, self(),
                          {case_timeout, I#istate.case_timeout}),
    Collect = fun(Cmd, _RevFile, _InclStack, Acc) ->
                      case Cmd of
                          #cmd{type = macro,
                               arg = {macro, Name, _ArgNames,
                                      _FirstLineNo, _LastLineNo, _Body}} ->
                              [#macro{name = Name,
                                      file = I#istate.file, cmd = Cmd} | Acc];
                          _ ->
                              Acc
                      end
              end,
    Macros = lux_utils:foldl_cmds(Collect,
                                       [],
                                       I#istate.orig_file,
                                       [],
                                       I#istate.orig_commands),
    I2 = I#istate{macros = Macros,
                  blocked = false,
                  has_been_blocked = false,
                  want_more = true,
                  old_want_more = undefined},

    I4 =
        case I2#istate.debug orelse I2#istate.debug_file =/= undefined of
            false ->
                I2;
            true ->
                DebugState = {attach, temporary},
                {_, I3} = lux_debug:cmd_attach(I2, [], DebugState),
                io:format("\nDebugger for lux. Try help or continue.\n",
                          []),
                I3
        end,
    try
        Res = interpret_loop(I4),
        {ok, Res}
    catch
        throw:{error, Reason, I5} ->
            {error, Reason, I5}
    after
        safe_cancel_timer(Ref)
    end.

interpret_loop(#istate{mode = stopping, shells = []} = I) ->
    %% Stop main
    I;
interpret_loop(#istate{commands = [], file_level = FileLevel} = I)
  when FileLevel > 1 ->
    %% Stop include
    multi_ping(I, wait_for_expect),
    %% Collect stop and down before popping the incl_stack
    sync_return(I);
interpret_loop(I) ->
    Timeout =
        if
            I#istate.want_more,
            not I#istate.blocked ->
                0;
            true ->
                infinity
        end,
    receive
        {debug_call, Pid, Cmd, CmdState} ->
            I2 = lux_debug:eval_cmd(I, Pid, Cmd, CmdState),
            interpret_loop(I2);
        stopped_by_user ->
            %% Ordered to stop by user
            ilog(I, "lux(~p): stopped_by_user\n", [I#istate.latest_lineno]),
            I2 = prepare_stop(I, dummy_pid, {fail, stopped_by_user}),
            interpret_loop(I2);
        {stop, Pid, Res} ->
            %% One shell has finished. Stop the others if needed
            I2 = prepare_stop(I, Pid, Res),
            interpret_loop(I2);
        {more, Pid, _Name} ->
            if
                Pid =/= I#istate.active ->
                    %% ilog(I, "lux(~p): ignore_more \"~s\"\n",
                    %%      [I#istate.latest_lineno, Name]),
                    interpret_loop(I);
                I#istate.blocked, not I#istate.want_more ->
                    I2 = I#istate{old_want_more = true},
                    interpret_loop(I2);
                not I#istate.blocked, I#istate.old_want_more =:= undefined ->
                    I2 = I#istate{want_more = true},
                    interpret_loop(I2)
            end;
        {my, Pid, _Name, VarVal} ->
            Shells = I#istate.shells,
            Others = [S || S <- Shells, S#shell.pid =/= Pid],
            MacroDict = [VarVal | I#istate.macro_dict],
            multi_cast(I#istate{shells = Others},
                       {macro_dict, self(), MacroDict}),
            interpret_loop(I#istate{macro_dict = MacroDict});
        {global, Pid, _Name, VarVal} ->
            Shells = I#istate.shells,
            Others = [S || S <- Shells, S#shell.pid =/= Pid],
            multi_cast(I#istate{shells = Others}, {global, self(), VarVal}),
            interpret_loop(I#istate{dict = [VarVal | I#istate.dict]});
        {'DOWN', _, process, Pid, Reason} ->
            I2 = prepare_stop(I, Pid, {'EXIT', Reason}),
            interpret_loop(I2);
        {TimeoutType, _TimeoutMillis} when TimeoutType =:= suite_timeout;
                                           TimeoutType =:= case_timeout,
                                           I#istate.has_been_blocked ->
            io:format("WARNING: Ignoring ~p"
                      " as the script has been attached by the debugger.\n",
                      [TimeoutType]),
            interpret_loop(I);
        {TimeoutType, TimeoutMillis} when TimeoutType =:= suite_timeout;
                                          TimeoutType =:= case_timeout ->
            Seconds = TimeoutMillis div timer:seconds(1),
            Multiplier = I#istate.multiplier / 1000,
            ilog(I, "lux(~p): ~p (~p seconds * ~.3f)\n",
                 [I#istate.latest_lineno, TimeoutType, Seconds, Multiplier]),
            case I#istate.mode of
                running ->
                    %% The test case (or suite) has timed out.
                    I2 = prepare_stop(I, dummy_pid, {fail, TimeoutType}),
                    interpret_loop(I2);
                cleanup ->
                    %% Timeout during cleanup

                    %% Initiate stop by sending shutdown to all shells.
                    multi_cast(I, {shutdown, self()}),
                    interpret_loop(I#istate{mode = stopping,
                                            cleanup_reason = TimeoutType});
                stopping ->
                    %% Shutdown has already been sent to the shells.
                    %% Continue to collect their states.
                    interpret_loop(I)
            end;
        Msg ->
            exit({interpreter_got, Msg})
    after multiply(I, Timeout) ->
            I2 = opt_dispatch_cmd(I),
            interpret_loop(I2)
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

opt_dispatch_cmd(#istate{commands = Cmds} = I) ->
    case Cmds of
        [#cmd{lineno = LineNo} = Cmd | Rest] ->
            {DoDispatch, I2} = lux_debug:check_break(I, LineNo),
            case DoDispatch of
                true ->
                    I3 = I2#istate{commands = Rest, latest_lineno = LineNo},
                    dispatch_cmd(I3, Cmd);
                false ->
                    I2
            end;
        [] ->
            %% End of script
            if
                I#istate.file_level > 1 ->
                    I;
                I#istate.mode =:= stopping ->
                    %% Already stopping
                    I;
                true ->
                    %% Initiate stop by sending end_of_script to all shells.
                    multi_cast(I, {end_of_script, self()}),
                    I#istate{mode = stopping}
            end
    end.

dispatch_cmd(#istate{want_more = false} = I, _Cmd) ->
    I;
dispatch_cmd(#istate{want_more = true} = I,
             #cmd{lineno = LineNo,
                  type = Type,
                  arg = Arg} = Cmd) ->
    case Type of
        comment ->
            I;
        doc ->
            {Level, Doc} = Arg,
            Indent = lists:duplicate((Level-1)*4, $\ ),
            ilog(I, "lux(~p): doc \"~s~s\"\n", [LineNo, Indent, Doc]),
            case I#istate.progress of
                doc -> io:format("\n~s~s\n", [Indent, Doc]);
                _   -> ok
            end,
            I;
        config ->
            {config, Var, Val} = Arg,
            ilog(I, "lux(~p): config \"~s=~s\"\n", [LineNo, Var, Val]),
            I;
        cleanup ->
            lux_utils:progress_write(I#istate.progress, "c"),
            ilog(I, "lux(~p): cleanup\n", [LineNo]),
            multi_cast(I, {sync_eval, self(), Cmd}),
            multi_ping(I, immediate),
            Shells = [S#shell{health = zombie} || S <- I#istate.shells],
            I2 = I#istate{mode = cleanup,
                          timeout = I#istate.cleanup_timeout,
                          shells = Shells,
                          active = undefined},
            Suffix =
                case I#istate.file_level of
                    1 -> "";
                    N -> integer_to_list(N)
                end,
            ShellCmd = Cmd#cmd{type = shell, arg = "cleanup" ++ Suffix},
            safe_shell_switch(I2, ShellCmd);
        shell ->
            safe_shell_switch(I, Cmd);
        include ->
            {include, InclFile, FirstLineNo, LastLineNo, InclCmds} = Arg,
            ilog(I, "lux(~p): include_file \"~s\"\n", [LineNo, InclFile]),
            eval_include(I, LineNo, FirstLineNo, LastLineNo,
                         InclFile, InclCmds, Cmd);
        macro ->
            I;
        invoke ->
            {invoke, Name, ArgVals} = Arg,
            I2 = I#istate{latest_lineno = LineNo},
            case shell_expand_vars(I2, Name, error) of
                {ok, Name2} ->
                    Macros = [M || M <- I2#istate.macros,
                                   M#macro.name =:= Name2],
                    invoke_macro(I2,
                                 Cmd#cmd{arg = {invoke, Name2, ArgVals}},
                                 Macros);
                {no_such_var, BadName} ->
                    E = list_to_binary(["Variable $", BadName, " is not set"]),
                    ilog(I2, "lux(~p): ~s\n", [LineNo, E]),
                    Raw = Cmd#cmd.raw,
                    throw({error, <<Raw/binary, " ", E/binary>>, I2})
            end;
        variable when element(1, Arg) =:= global,
                      I#istate.active =:= undefined ->
            %% Allow global variables to be set without any active shell
            I2 = I#istate{latest_lineno = LineNo},
            {Scope, Var, Val} = Arg,
            Dicts = [I2#istate.dict,
                     I2#istate.builtin_dict,
                     I2#istate.system_dict],
            try
                Val2 = lux_utils:expand_vars(Dicts, Val, error),
                VarVal = lists:flatten([Var, $=, Val2]),
                ilog(I2, "lux(~p): ~p \"~s\"\n", [LineNo, Scope, VarVal]),
                I2#istate{dict = [VarVal | I2#istate.dict]}
            catch
                throw:{no_such_var, BadName} ->
                    Err = list_to_binary(["Variable $",
                                          BadName, " is not set"]),
                    ilog(I2, "lux(~p): ~s\n", [LineNo, Err]),
                    throw({error, Err, I2})
            end;
        _ ->
            %% Send next command to active shell
            cast(I, {sync_eval, self(), Cmd}),
            I#istate{want_more = false}
    end.

eval_include(OldI, InclLineNo, FirstLineNo, LastLineNo, InclFile, InclCmds,
             #cmd{} = _Include) ->
    lux_utils:progress_write(OldI#istate.progress, "("),
    ilog(OldI, "include_begin ~p ~p ~p ~p\n",
         [InclLineNo, FirstLineNo, LastLineNo, InclFile]),
    InclFile2 = lux_utils:filename_split(InclFile),
    NewStack = [{InclFile2, InclLineNo} | OldI#istate.incl_stack],
    BeforeI = OldI#istate{file_level = OldI#istate.file_level + 1,
                          file = InclFile,
                          latest_lineno = InclLineNo,
                          incl_stack = NewStack,
                          commands = InclCmds},
    AfterI =
        try
            interpret_loop(BeforeI)
        catch
            Class:Reason ->
                erlang:raise(Class, Reason, erlang:get_stacktrace())
        after
            lux_utils:progress_write(OldI#istate.progress, ")"),
            ilog(OldI, "include_end ~p ~p ~p ~p\n",
                 [InclLineNo, FirstLineNo, LastLineNo, InclFile])
        end,
    NewI = AfterI#istate{file_level = OldI#istate.file_level,
                         file = OldI#istate.file,
                         latest_lineno = OldI#istate.latest_lineno,
                         incl_stack = OldI#istate.incl_stack,
                         commands = OldI#istate.commands},
    if
        %% NewI#istate.cleanup_reason =:= normal,
        %% NewI#istate.mode =:= cleanup,
        %% OldI#istate.mode =:= normal ->
        %%     %% Normal cleanup has been run
        %%     %%CleanupShells = [S || S <- NewI#istate.shells,
        %%     %%                      S#shell.health =/= zombie],
        %%     shell_switch(OldI, Cmd, #shell{pid = Pid, health = alive}),
        %%
        %%     NewI#istate{mode = OldI#istate.mode,
        %%                 timeout = OldI#istate.timeout,
        %%                 shells = OldI#istate.shells,
        %%                 active = OldI#istate.active};
        NewI#istate.cleanup_reason =:= normal ->
            %% No cleanup needed
            NewI;
        OldI#istate.cleanup_reason =:= normal ->
            goto_cleanup(NewI, NewI#istate.cleanup_reason);
        true ->
            %% Already cleaning up
            NewI
    end.

invoke_macro(I,
             #cmd{arg = {invoke, Name, ArgVals},
                  lineno = LineNo} = Invoke,
             [#macro{name = Name,
                     file = File,
                     cmd = #cmd{arg = {macro, Name, ArgNames, FirstLineNo,
                                       LatestLineNo, Body}} = Macro}]) ->
    OldMacroDict = I#istate.macro_dict,
    I2 = I#istate{latest_lineno = LineNo},
    MacroDict = macro_dict(I2, ArgNames, ArgVals, Invoke),
    ilog(I2, "lux(~p): invoke_~s \"~s\"\n",
         [LineNo, Name, lists:flatten([[M, " "] || M <- MacroDict])]),

    multi_cast(I2, {macro_dict, self(), MacroDict}),
    BeforeI = I2#istate{macro_dict = MacroDict, latest_lineno = LineNo},
    AfterI = eval_include(BeforeI, LineNo, FirstLineNo,
                          LatestLineNo, File, Body, Macro),
    multi_cast(AfterI, {macro_dict, self(), OldMacroDict}),

    AfterI#istate{macro_dict = OldMacroDict};
invoke_macro(I, #cmd{arg = {invoke, Name, _Values}, lineno = LineNo}, []) ->
    I2 = I#istate{latest_lineno = LineNo},
    BinName = list_to_binary(Name),
    throw({error, <<"No such macro: ", BinName/binary>>, I2});
invoke_macro(I, #cmd{arg = {invoke, Name, _Values}, lineno = LineNo}, [_|_]) ->
    I2 = I#istate{latest_lineno = LineNo},
    BinName = list_to_binary(Name),
    throw({error, <<"Ambiguous macro: ", BinName/binary>>, I2}).

macro_dict(I, [Name | Names], [Val | Vals], Invoke) ->
    case shell_expand_vars(I, Val, error) of
        {ok, Val2} ->
            [lists:flatten([Name, $=, Val2]) |
             macro_dict(I, Names, Vals, Invoke)];
        {no_such_var, BadName} ->
            Err = list_to_binary(["Variable $", BadName, " is not set"]),
            ilog(I, "lux(~p): ~s\n", [Invoke#cmd.lineno, Err]),
            Raw = Invoke#cmd.raw,
            throw({error, <<Raw/binary, " ", Err/binary>>, I})
    end;
macro_dict(_I, [], [], _Invoke) ->
    [];
macro_dict(I, _Names, _Vals, #cmd{arg = {invoke, Name, _}, lineno = LineNo}) ->
    BinName = list_to_binary(Name),
    BinLineNo = list_to_binary(integer_to_list(LineNo)),
    Reason = <<"at ", BinLineNo/binary,
               ": Argument mismatch in macro: ", BinName/binary>>,
    throw({error, Reason, I}).

prepare_stop(#istate{results = Acc,
                     latest_lineno = Latest,
                     incl_stack = Stack,
                     cleanup_reason = OrigCleanupReason} = I,
             Pid,
             Res) ->
    I2 = delete_shell(I, Pid),
    {CleanupReason, Res2} =
        case Res of
            %% #result{lineno = _LineNo, outcome = shutdown}
            %%   when OrigCleanupReason =:= fail ->
            %%     {OrigCleanupReason,
            %%      Res#result{lineno = Latest,
            %%                 incl_stack = I#istate.incl_stack,
            %%                 outcome = fail}};
            #result{lineno = _LineNo, outcome = shutdown} ->
                {OrigCleanupReason,
                 Res#result{lineno = Latest, incl_stack = Stack}};
            #result{lineno = _LineNo, outcome = NewOutcome} ->
                {NewOutcome,
                 Res#result{lineno = Latest, incl_stack = Stack}};
            {'EXIT', Res} ->
                {fail, Res};
            {fail, FailReason} ->
                {fail,
                 #result{outcome    = fail,
                         lineno     = Latest,
                         incl_stack = Stack,
                         expected   = <<"">>,
                         extra      = undefined,
                         actual     = FailReason,
                         rest       = fail}}
        end,
    I3 = I2#istate{results = [Res2 | Acc]},
    case I3#istate.mode of
        running when Res2#result.outcome =:= shutdown,
                     Res2#result.actual =:= shell_exit ->
            %% Successful end of file in shell. Delete the shell.
            I3#istate{active = undefined, want_more = true};
        running ->
            goto_cleanup(I3, CleanupReason);
        cleanup when I3#istate.shells =:= [] ->
            %% All shell states has been collected. Full stop.
            I3#istate{mode = stopping};
        cleanup ->
            %% Initiate stop by sending shutdown to the remaining shells.
            multi_cast(I3, {shutdown, self()}),
            I3#istate{mode = stopping};
        stopping ->
            %% Shutdown has already been sent to the other shells.
            %% Continue to collect their states if needed.
            I3
    end.

goto_cleanup(I, CleanupReason) ->
    Prefix =
        case I#istate.results of
            [#result{actual=fail_pattern_matched}|_] ->
                "-";
            [#result{actual=success_pattern_matched}|_] ->
                "+";
            _ ->
                ""
        end,
    LineNo = integer_to_list(I#istate.latest_lineno),
    lux_utils:progress_write(I#istate.progress, Prefix ++ LineNo),

    %% Ensure that the cleanup does not take too long time
    safe_send_after(I, I#istate.case_timeout, self(),
                    {case_timeout, I#istate.case_timeout}),

    %% Fast forward to (optional) cleanup command
    Cmds = I#istate.commands,
    {_, Cleanup} =
        lists:splitwith(fun(#cmd{type = Type}) -> Type =/= cleanup end, Cmds),
    NewI = I#istate{cleanup_reason = CleanupReason,
                    want_more = true,
                    commands = Cleanup},
    Mode = I#istate.mode,
    case Cleanup of
        [_|_] when Mode =:= running;
                   Mode =:= cleanup ->
            ilog(I, "lux(~s): goto cleanup\n", [LineNo]),
            NewI#istate{mode = cleanup};
        _ when I#istate.file_level > 1 ->
            ilog(I, "lux(~s): no cleanup\n", [LineNo]),
            NewI#istate{mode = cleanup};
        _  ->
            %% Initiate stop by sending shutdown to the remaining shells.
            multi_cast(I, {shutdown, self()}),
            NewI#istate{mode = stopping}
    end.

delete_shell(I, Pid) ->
    Shells = I#istate.shells,
    case lists:keyfind(Pid, #shell.pid, Shells) of
        false ->
            I;
        #shell{ref = Ref} ->
            erlang:demonitor(Ref, [flush]),
            Active = I#istate.active,
            Active2 =
                if
                    Active =:= Pid -> undefined;
                    true           -> Active
                end,
            Shells2 = lists:keydelete(Pid, #shell.pid, Shells),
            I#istate{active = Active2,
                     shells = Shells2}
    end.

multi_cast(#istate{shells = Shells}, Msg) ->
    Send = fun(#shell{pid = Pid}) -> Pid ! Msg, Pid end,
    lists:map(Send, Shells).

cast(#istate{active = undefined} = I, _Msg) ->
    throw({error, <<"The command must be executed in context of a shell">>, I});
cast(#istate{active = Pid}, Msg) ->
    Pid ! Msg,
    Pid.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Control a shell

safe_shell_switch(I, #cmd{lineno = LineNo, arg = Name} = Cmd) ->
    I2 = I#istate{latest_lineno = LineNo},
    case shell_expand_vars(I2, Name, error) of
        {ok, Name2} ->
            case lists:keyfind(Name2, #shell.name, I2#istate.shells) of
                false ->
                    shell_start(I2, Cmd#cmd{arg = Name2});
                Shell ->
                    shell_switch(I2, Cmd, Shell)
            end;
        {no_such_var, BadName} ->
            Err = list_to_binary(["Variable $", BadName, " is not set"]),
            ilog(I2, "~s(~p): ~s\n", [Name, LineNo, Err]),
            BinName = list_to_binary(Name),
            throw({error, <<"[shell ", BinName/binary, "] ", Err/binary>>, I2})
    end.

shell_start(I, #cmd{arg = Name} = Cmd) ->
    I2 = change_shell_mode(I, Cmd, suspend),
    case lux_shell:start_monitor(I2, Cmd, Name) of
        {ok, I3} ->
            %% Wait for some shell output
            Wait = Cmd#cmd{type = expect,
                           arg = {regexp, <<".+">>}},
            %% Set the prompt (after the rc files has ben run)
            Prompt = Cmd#cmd{type = send_lf,
                             arg = <<"export PS1=SH-PROMPT:">>},
            %% Wait for the prompt
            Sync = Cmd#cmd{type = expect,
                           arg = {regexp, <<"^SH-PROMPT:">>}},
            Cmds = [Wait, Prompt, Sync | I3#istate.commands],
            I3#istate{commands = Cmds, want_more = false};
        {error, I3, Pid, Reason} ->
            shell_crashed(I3, Pid, Reason)
    end.

shell_switch(OldI, Cmd, #shell{pid = Pid, health = alive}) ->
    %% Activate shell
    change_shell_mode(OldI, Cmd, suspend),
    NewI = OldI#istate{active = Pid},
    change_shell_mode(NewI, Cmd, resume);
shell_switch(OldI, _Cmd, #shell{name = Name, health = zombie}) ->
    ilog(OldI, "~s(~p): zombie shell at cleanup\n",
         [Name, OldI#istate.latest_lineno]),
    throw({error, list_to_binary(Name ++ " is a zombie shell"), OldI}).

ping(I, When) when When =:= immediate; When =:= wait_for_expect ->
    if
        is_pid(I#istate.active) ->
            Pid = cast(I, {ping, self(), When}),
            receive
                {pong, Pid} ->
                    I;
                {'DOWN', _, process, Pid, shutdown} ->
                    %% Take care of this later
                    I#istate{active = undefined};
                {'DOWN', _, process, Pid, Reason} ->
                    shell_crashed(I, Pid, Reason)
            end;
        I#istate.active =:= undefined ->
            I
    end.

multi_ping(I, When) when  When =:= flush;
                          When =:= immediate;
                          When =:= wait_for_expect ->
    Pids = multi_cast(I, {ping, self(), When}),
    wait_for_pong(I, Pids).

wait_for_pong(I, [Pid | Pids]) ->
    receive
        {pong, Pid} ->
            wait_for_pong(I, Pids);
        {'DOWN', _, process, Pid, shutdown} = ShutdownMsg ->
            self() ! ShutdownMsg,
            wait_for_pong(I, Pids);
        {'DOWN', _, process, Pid, Reason} ->
            shell_crashed(I, Pid, Reason)
    end;
wait_for_pong(I, []) ->
    I.

change_shell_mode(I, Cmd, NewMode) when is_pid(I#istate.active) ->
    cast(I, {change_mode, self(), NewMode, Cmd}),
    ping(I, immediate);
change_shell_mode(I, _Cmd, _NewMode) when I#istate.active =:= undefined ->
    I.

shell_crashed(I, Pid, Reason) ->
    What =
        case lists:keyfind(Pid, #shell.pid, I#istate.shells) of
            false -> ["Process ", io_lib:format("~p", [Pid])];
            Shell -> ["Shell ", Shell#shell.name]
        end,
    Error =
        case Reason of
            {error, ErrBin} ->
                ErrBin;
            _ ->
                list_to_binary( [What, " crashed: ",
                                 io_lib:format("~p\n~p", [Reason, ?stack()])])
        end,
    throw({error, Error, I}).

shell_expand_vars(I, Bin, MissingVar) when is_pid(I#istate.active) ->
    Pid = cast(I, {expand_vars, self(), Bin, MissingVar}),
    receive
        {expand_vars, Pid, Res} ->
            Res;
        {'DOWN', _, process, Pid, Reason} ->
            shell_crashed(I, Pid, Reason)
    end;
shell_expand_vars(I, Bin, MissingVar) when I#istate.active =:= undefined ->
    try
        {ok, expand_vars(I, Bin, MissingVar)}
    catch
        throw:{no_such_var, BadName} ->
            {no_such_var, BadName}
    end.

double_ilog(#istate{progress = Progress, log_fun = LogFun, event_log_fd = Fd},
            Format,
            Args) ->
    Bin = lux_log:safe_format(silent, LogFun, undefined, Format, Args),
    lux_log:safe_write(Progress, LogFun, Fd, Bin).

ilog(#istate{progress = Progress, log_fun = LogFun, event_log_fd = Fd},
     Format,
     Args)->
    lux_log:safe_format(Progress, LogFun, Fd, Format, Args).

console_write(String) ->
    io:format("~s", [String]).

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
        infinity ->
            infinity;
        _ ->
            lux_utils:multiply(Timeout, Factor)
    end.

default_istate(File) ->
    #istate{file = filename:absname(File),
            log_fun = fun(Bin) -> console_write(binary_to_list(Bin)), Bin end,
            shell_wrapper = default_shell_wrapper(),
            builtin_dict = lux_utils:builtin_dict(),
            system_dict = lux_utils:system_dict()}.

default_shell_wrapper() ->
    Wrapper = filename:join([code:priv_dir(?APPLICATION), "bin", "runpty"]),
    case filelib:is_regular(Wrapper) of
        true  -> Wrapper;
        false -> undefined
    end.
