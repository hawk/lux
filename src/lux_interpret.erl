%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2016 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_interpret).

-include("lux.hrl").

-export([
         interpret_commands/4,
         default_istate/1,
         parse_iopts/2,
         config_type/1,
         user_config_types/0,
         set_config_val/6,
         set_config_vals/6,
         case_log_dir/2,
         copy_orig/2
        ]).
-export([
         opt_dispatch_cmd/1,
         lookup_macro/2,
         flush_logs/1
        ]).

interpret_commands(Script, Cmds, Opts, Opaque) ->
    I = default_istate(Script),
    case lists:keyfind(stopped_by_user, 1, Opaque) of
        {_, Context = suite} -> ok;
        _                    -> Context = undefined
    end,
    I2 = I#istate{commands = Cmds,
                  orig_commands = shrinked,
                  stopped_by_user = Context},
    try
        case parse_iopts(I2, Opts) of
            {ok, I3} ->
                CaseLogDir = case_log_dir(I3, Script),
                I4 = I3#istate{case_log_dir = CaseLogDir},
                case copy_orig(I4, Script) of
                    {ok, Base} ->
                        ExtraLogs = filename:join([CaseLogDir,
                                                   Base ++ ".extra.logs"]),
                        ExtraVars = "LUX_EXTRA_LOGS=" ++ ExtraLogs,
                        GlobalVars = [ExtraVars | I4#istate.global_vars],
                        I5 = I4#istate{global_vars = GlobalVars},
                        Config = config_data(I5),
                        ConfigFd =
                            lux_log:open_config_log(CaseLogDir, Script, Config),
                        Progress = I5#istate.progress,
                        LogFun = I5#istate.log_fun,
                        Verbose = true,
                        case lux_log:open_event_log(CaseLogDir, Script,
                                                    Progress,
                                                    LogFun, Verbose) of
                            {ok, EventLog, EventFd} ->
                                Docs = docs(I5#istate.orig_file, Cmds),
                                eval(I5, Progress, Verbose, LogFun,
                                     EventLog, EventFd, ConfigFd, Docs);
                            {error, FileReason} ->
                                internal_error(I5,
                                               file:format_error(FileReason))
                        end;
                    {error, FileReason} ->
                        internal_error(I4, file:format_error(FileReason))
                end;
            {error, ParseReason} ->
                internal_error(I2, ParseReason)
        end
    catch
        Class:Reason ->
            Stack = erlang:get_stacktrace(),
            internal_error(I2, {'EXIT', {fatal_error, Class, Reason, Stack}})
    end.

case_log_dir(#istate{suite_log_dir = SuiteLogDir}, AbsScript) ->
    case_log_dir(SuiteLogDir, AbsScript);
case_log_dir(SuiteLogDir, AbsScript) ->
    RelScript0 = lux_utils:drop_prefix(AbsScript),
    RelScript =
        case filename:pathtype(RelScript0) of
            absolute -> tl(RelScript0);
            _Type    -> RelScript0
        end,
    RelDir = filename:dirname(RelScript),
    filename:join([SuiteLogDir, RelDir]).

copy_orig(I, Script) ->
    CaseLogDir = case_log_dir(I, Script),
    Base = filename:basename(Script),
    OrigScript = filename:join([CaseLogDir, Base ++ ".orig"]),
    case filelib:ensure_dir(OrigScript) of % Ensure LogDir
        ok ->
            case file:copy(Script, OrigScript) of
                {ok, _} ->
                    {ok, Base};
                {error, FileReason} ->
                    {error, FileReason}
            end;
        {error, FileReason} ->
            {error, FileReason}
    end.

eval(OldI, Progress, Verbose, LogFun, EventLog, EventFd, ConfigFd, Docs) ->
    NewI = OldI#istate{event_log_fd =  {Verbose, EventFd},
                       config_log_fd = {Verbose, ConfigFd}},
    Flag = process_flag(trap_exit, true),
    try
        lux_log:safe_format(Progress, LogFun, undefined,
                            "~s~s\n",
                            [?TAG("script"),
                             NewI#istate.file]),
        lux_log:safe_format(Progress, LogFun, undefined,
                            "~s~s\n",
                            [?TAG("event log"), EventLog]),
        lux_utils:progress_write(Progress, ?TAG("progress")),
        ReplyTo = self(),
        Interpret =
            fun() ->
                    lux_debug:start_link(OldI#istate.debug_file),
                    Res = interpret_init(NewI),
                    lux:trace_me(70, 'case', shutdown, []),
                    unlink(ReplyTo),
                    ReplyTo ! {done, self(), Res},
                    exit(shutdown)
            end,
        Pid = spawn_link(Interpret),
        %% Poor mans hibernate
        ShrinkedI =
            NewI#istate{commands     = shrinked,
                        macro_vars   = shrinked,
                        global_vars  = shrinked,
                        builtin_vars = shrinked,
                        system_vars  = shrinked},
        garbage_collect(),
        wait_for_done(ShrinkedI, Pid, Docs)
    after
        process_flag(trap_exit, Flag),
        lux_log:close_event_log(EventFd),
        file:write(ConfigFd, "\n"), file:close(ConfigFd) % Don't care of failure
    end.

internal_error(I, ReasonTerm) ->
    ReasonBin = list_to_binary(io_lib:format("INTERNAL LUX ERROR: ~p\n",
                                             [ReasonTerm])),
    fatal_error(I, ReasonBin).

fatal_error(I, ReasonBin) when is_binary(ReasonBin) ->
    FullLineNo = full_lineno(I, I#istate.latest_cmd, I#istate.cmd_stack),
    double_ilog(I, "~sERROR ~s\n",
                [?TAG("result"),
                 binary_to_list(ReasonBin)]),
    {error, I#istate.file, FullLineNo, I#istate.case_log_dir, ReasonBin}.

parse_iopts(I, Opts) ->
    {Res, _U} = do_parse_iopts(I, Opts, []),
    Res.

do_parse_iopts(I, [{Name, Val} | T], U) when is_atom(Name) ->
    case parse_iopt(I, Name, Val, U) of
        {{ok, I2}, U2} ->
            do_parse_iopts(I2, T, U2);
        {{error, _Reason}, _U2} = Res ->
            Res
    end;
do_parse_iopts(I, [], U) ->
    File = lux_utils:normalize(I#istate.file),
    case I#istate.shell_wrapper of
        "" -> ShellWrapper = undefined;
        ShellWrapper -> ok
    end,
    SuiteLogDir = lux_utils:normalize(I#istate.suite_log_dir),
    I2 = I#istate{file = File,
                  orig_file = File,
                  shell_wrapper = ShellWrapper,
                  suite_log_dir = SuiteLogDir,
                  case_log_dir = SuiteLogDir},
    {{ok, I2}, U}.

parse_iopt(I, Name, Val, U) when is_atom(Name) ->
    case config_type(Name) of
        {ok, Pos, Types} ->
            set_config_val(Name, Val, Types, Pos, I, U);
        {error, Reason} ->
            {{error, Reason}, U}
    end.

config_type(Name) ->
    case Name of
        debug ->
            {ok, #istate.debug, [{atom, [true, false]}]};
        debug_file ->
            {ok, #istate.debug_file, [string, {atom, [undefined]}]};
        skip ->
            {ok, #istate.skip, [{std_list, [string]}]};
        skip_unless ->
            {ok, #istate.skip_unless, [{std_list, [string]}]};
        require ->
            {ok, #istate.require, [{std_list, [string]}]};
        case_prefix ->
            {ok, #istate.case_prefix, [string]};
        config_dir ->
            {ok, #istate.config_dir, [string]};
        progress ->
            {ok, #istate.progress,
             [{atom, [silent, summary, brief, doc, compact, verbose]}]};
        log_dir ->
            {ok, #istate.suite_log_dir, [string]};
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
            {ok, #istate.default_timeout, [{integer, 0, infinity},
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
            {ok, #istate.shell_args, [{reset_list, [string]}]};
        shell_prompt_cmd ->
            {ok, #istate.shell_prompt_cmd, [string]};
        shell_prompt_regexp ->
            {ok, #istate.shell_prompt_regexp, [string]};
        var ->
            {ok, #istate.global_vars, [{std_list, [string]}]};
        system_env ->
            {ok, #istate.system_vars, [{std_list, [string]}]};
        _ ->
            {error, iolist_to_binary(lists:concat(["Bad argument: ", Name]))}
    end.

user_config_types() ->
    Fun = fun(Key) ->
                  {ok, Pos, Types} = config_type(Key),
                  {Key, Pos, Types}
          end,
    lists:map(Fun, user_config_keys()).

set_config_val(Name, Val, [Type | Types], Pos, I, U) ->
    U2 = [Name | U],
    try
        case Type of
            string when is_list(Val) ->
                Val2 = expand_vars(I, Val, error),
                {{ok, setelement(Pos, I, Val2)}, U2};
            binary when is_binary(Val) ->
                Val2 = expand_vars(I, Val, error),
                {{ok, setelement(Pos, I, Val2)}, U2};
            binary when is_list(Val) ->
                Val2 = expand_vars(I, Val, error),
                set_config_val(Name, list_to_binary(Val2), [Type], Pos, I, U);
            {atom, Atoms} when is_atom(Val) ->
                true = lists:member(Val, Atoms),
                {{ok, setelement(Pos, I, Val)}, U2};
            {atom, _Atoms} when is_list(Val) ->
                set_config_val(Name, list_to_atom(Val), [Type], Pos, I, U);
            {function, Arity} when is_function(Val, Arity) ->
                {{ok, setelement(Pos, I, Val)}, U2};
            {integer, infinity, infinity} when is_integer(Val) ->
                {{ok, setelement(Pos, I, Val)}, U2};
            {integer, infinity, Max}
              when is_integer(Val), is_integer(Max), Val =< Max ->
                {{ok, setelement(Pos, I, Val)}, U2};
            {integer, Min, infinity}
              when is_integer(Val), is_integer(Min), Val >= Min ->
                {{ok, setelement(Pos, I, Val)}, U2};
            {integer, Min, Max}
              when is_integer(Val), is_integer(Min), is_integer(Max),
                   Val >= Min, Val =< Max ->
                {{ok, setelement(Pos, I, Val)}, U2};
            {integer, _Min, _Max} when is_list(Val) ->
                set_config_val(Name, list_to_integer(Val), [Type], Pos, I, U);
            {std_list, SubTypes} when is_list(SubTypes) ->
                append_config_val(Name, Val, append, SubTypes, Pos, I, U);
            {reset_list, SubTypes} when is_list(SubTypes) ->
                append_config_val(Name, Val, reset, SubTypes, Pos, I, U);
            io_device ->
                {{ok, setelement(Pos, I, Val)}, U2}
        end
    catch
        throw:{no_such_var, BadName} ->
            Msg = ["Bad argument: ", Name, "=", Val,
                   "; $", BadName, " is not set"],
            {{error, iolist_to_binary(lists:concat(Msg))}, U};
        _Class:_Reason ->
            set_config_val(Name, Val, Types, Pos, I, U)
    end;
set_config_val(Name, Val, [], _Pos, _I, Updated) ->
    {{error, iolist_to_binary(lists:concat(["Bad argument: ",
                                            Name, "=", Val]))},
     [Name | Updated]}.

set_config_vals(Name, Vals, Types, Pos, I, U) ->
    Fun = fun(Val, {{ok, AccI}, AccU}) ->
                  set_config_val(Name, Val, Types, Pos, AccI, AccU);
             (_Val, {{error, _Reason}, _Updated} = Res) ->
                  Res
          end,
    lists:foldl(Fun, {{ok, I}, U}, Vals).

append_config_val(Name, Val, Oper, SubTypes, Pos, OldI, OldU) ->
    case set_config_val(Name, Val, SubTypes, Pos, OldI, OldU) of
        {{ok, NewI}, NewU} ->
            OldVals = element(Pos, OldI),
            NewVal = element(Pos, NewI),
            NewVals =
                case Oper =:= reset andalso not lists:member(Name, OldU) of
                    true  -> [NewVal];
                    false -> OldVals ++ [NewVal]
                end,
            {{ok, setelement(Pos, NewI, NewVals)}, NewU};
        {error, Reason} ->
            {{error, Reason}, OldU}
    end.

wait_for_done(I, Pid, Docs) ->
    receive
        {suite_timeout, SuiteTimeout} ->
            %% double_ilog(I, "\n~s~p\n",
            %%             [?TAG("suite timeout"),
            %%              SuiteTimeout]),
            Pid ! {suite_timeout, SuiteTimeout},
            case wait_for_done(I, Pid, Docs) of
                {ok, _Summary, File, FullLineNo, CaseLogDir,
                 _Events, _FailBin, _NewOpaque} ->
                    ok;
                {error, File, CaseLogDir, FullLineNo, _} ->
                    ok
            end,
            {error, File, CaseLogDir, FullLineNo, <<"suite_timeout">>};
        {done, Pid, Res} ->
            lux_utils:progress_write(I#istate.progress, "\n"),
            case Res of
                {ok, I2} ->
                    handle_done(I, I2, Docs);
                {error, ReasonBin, I2} ->
                    I3 = post_ilog(I2, Docs),
                    fatal_error(I3, ReasonBin)
            end;
        {'EXIT', _Pid, Reason} ->
            I2 = post_ilog(I, Docs),
            internal_error(I2, {'EXIT', Reason})
    end.

handle_done(OldI, NewI0, Docs) ->
    NewI = post_ilog(NewI0, Docs),
    File = NewI#istate.file,
    Results = NewI#istate.results,
    case lists:keyfind('EXIT', 1, Results) of
        false ->
            case lists:keyfind(fail, #result.outcome, Results) of
                #result{outcome = fail} = Fail ->
                    print_fail(OldI, NewI, File, Results, Fail);
                false ->
                    Reason = NewI#istate.cleanup_reason,
                    if
                        Reason =:= normal;
                        Reason =:= success ->
                            print_success(NewI, File, Results);
                        true ->
                            LatestCmd = NewI#istate.latest_cmd,
                            CmdStack = NewI#istate.cmd_stack,
                            R = #result{outcome    = fail,
                                        latest_cmd = LatestCmd,
                                        cmd_stack  = CmdStack,
                                        expected   = success,
                                        extra      = undefined,
                                        actual     = Reason,
                                        rest       = fail},
                            print_fail(OldI, NewI, File, Results, R)
                    end
            end;
        {'EXIT', Reason} ->
            internal_error(NewI, {'EXIT', Reason})
    end.

print_success(I, File, Results) ->
    double_ilog(I, "~sSUCCESS\n", [?TAG("result")]),
    LatestCmd = I#istate.latest_cmd,
    FullLineNo = integer_to_list(LatestCmd#cmd.lineno),
    {ok, success, File, FullLineNo, I#istate.case_log_dir,
     Results, <<>>, [{stopped_by_user, I#istate.stopped_by_user}]}.

print_fail(OldI0, NewI, File, Results,
           #result{outcome    = fail,
                   latest_cmd = LatestCmd,
                   cmd_stack  = CmdStack,
                   expected   = Expected,
                   extra      = _Extra,
                   actual     = Actual,
                   rest       = Rest}) ->
    {NewActual, NewRest} =
        case Actual of
            <<"fail pattern matched ",  _/binary>> ->
                {Actual, Rest};
            <<"success pattern matched ", _/binary>> ->
                {Actual, Rest};
            _ when is_atom(Actual) ->
                {atom_to_list(Actual), Rest};
            _ when is_binary(Actual) ->
                {<<"error">>, Actual}
        end,
    OldI = OldI0#istate{progress = silent},
    FullLineNo = full_lineno(OldI, LatestCmd, CmdStack),
    ResStr = double_ilog(OldI, "~sFAIL at ~s\n",
                         [?TAG("result"), FullLineNo]),
    FailBin =
        iolist_to_binary(
          [
           io_lib:format("expected\n\t~s\n",
                         [simple_to_string(Expected)]),
           io_lib:format("actual ~s\n\t~s",
                         [NewActual, simple_to_string(NewRest)])
          ]),
    case OldI0#istate.progress of
        silent ->
            ok;
        _ ->
            io:format("~s", [ResStr]),
            io:format("~s\n", [FailBin])
    end,
    double_ilog(OldI, "expected\n\"~s\"\n",
                [lux_utils:to_string(Expected)]),
    double_ilog(OldI, "actual ~s\n\"~s\"\n",
                [NewActual, lux_utils:to_string(NewRest)]),
    Opaque = [{stopped_by_user,NewI#istate.stopped_by_user}],
    {ok, fail, File, FullLineNo, NewI#istate.case_log_dir,
     Results, FailBin, Opaque}.

full_lineno(I, #cmd{lineno = LineNo, type = Type}, CmdStack) ->
    RevFile = lux_utils:filename_split(I#istate.file),
    CmdPos = #cmd_pos{rev_file = RevFile, lineno = LineNo, type = Type},
    FullStack = [CmdPos | CmdStack],
    lux_utils:pretty_full_lineno(FullStack).

flush_logs(I) ->
    flush_summary_log(I),
    multisync(I, flush).

flush_summary_log(#istate{summary_log_fd=undefined}) ->
    ok;
flush_summary_log(#istate{summary_log_fd=SummaryFd}) ->
    file:sync(SummaryFd).

post_ilog(#istate{progress = Progress,
                  logs = Logs,
                  config_log_fd = {_, ConfigFd}} = I,
          Docs) ->
    lux_log:close_config_log(ConfigFd, Logs),
    log_doc(I, Docs),
    ilog(I, "\n", []),
    LogFun =
        fun(Bin) ->
                case Progress of
                    silent -> ok;
                    _      -> console_write(binary_to_list(Bin))
                end,
                (I#istate.log_fun)(Bin),
                Bin
        end,
    I#istate{progress = silent,log_fun = LogFun}.

docs(File, OrigCmds) ->
    Fun =
        fun(#cmd{type = doc, arg = Arg}, _RevFile, _CmdStack, Acc)
           when tuple_size(Arg) =:= 2 ->
                [Arg | Acc];
           (_, _RevFile, _FileStack, Acc) ->
                Acc
        end,
    lists:reverse(lux_utils:foldl_cmds(Fun, [], File, [], OrigCmds)).

log_doc(#istate{log_fun = LogFun}, Docs) ->
    Prefix = list_to_binary(?TAG("doc")),
    Fun =
        fun({Level, Doc}) ->
                Tabs = list_to_binary(lists:duplicate(Level-1, $\t)),
                LogFun(<<Prefix/binary, Tabs/binary, Doc/binary, "\n">>)
        end,
    lists:foreach(Fun, Docs).

simple_to_string(Atom) when is_atom(Atom) ->
    simple_to_string(atom_to_list(Atom));
simple_to_string(Bin) when is_binary(Bin) ->
    simple_to_string(binary_to_list(Bin));
simple_to_string([$\r | T]) ->
    simple_to_string(T);
simple_to_string([$\n | T]) ->
    [$\n, $\t | simple_to_string(T)];
simple_to_string([$\\, $\R | T]) ->
    [$\n, $\t | simple_to_string(T)];
simple_to_string([Char | T]) when is_integer(Char) ->
    [Char | simple_to_string(T)];
simple_to_string([H | T]) ->
    simple_to_string(H) ++ simple_to_string(T);
simple_to_string([]) ->
    [].

config_data(I) ->
    {ok, Cwd} = file:get_cwd(),
    UserConfigTypes = user_config_types(),
    [
     {script,     [string], I#istate.file},
     {run_dir,    [string], Cwd}
    ]
        ++
    [{Key, Types, element(Pos, I)} || {Key, Pos, Types} <- UserConfigTypes]
        ++
    [
     {builtin,    [{std_list, [string]}], I#istate.builtin_vars},
     {system_env, [{std_list, [string]}], I#istate.system_vars}
    ].

user_config_keys() ->
    [
     log_dir,
     debug,
     debug_file,
     skip,
     skip_unless,
     require,
     case_prefix,
     progress,
     multiplier,
     suite_timeout,
     case_timeout,
     flush_timeout,
     poll_timeout,
     timeout,
     cleanup_timeout,
     shell_wrapper,
     shell_cmd,
     shell_args,
     shell_prompt_cmd,
     shell_prompt_regexp,
     var
    ].

interpret_init(I) ->
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
                    io:format("\nDebugger for lux. Try help or continue.\n",
                              []),
                    I3;
                true ->
                    I2
            end,
        Res = interpret_loop(I4),
        {ok, Res}
    catch
        throw:{error, Reason, I5} ->
            {error, Reason, I5}
    after
        safe_cancel_timer(Ref)
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
                        MacroFile = lux_utils:normalize(AbsFile),
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
                                     integer_to_list(LineNo)
                                    ],
                                throw({error, iolist_to_binary(Reason), I})
                        end;
                    _ ->
                        Acc
                end
        end,
    lux_utils:foldl_cmds(Collect, [], OrigFile, [], OrigCmds).

interpret_loop(#istate{mode = stopping,
                       shells = [],
                       active_shell = undefined} = I) ->
    %% Stop main
    I;
interpret_loop(#istate{commands = [], call_level = CallLevel} = I)
  when CallLevel > 1 ->
    %% Stop include
    I2 = multisync(I, wait_for_expect),
    %% Check for stop and down before popping the cmd_stack
    sync_return(I2);
interpret_loop(I) ->
    Timeout = timeout(I),
    receive
        {debug_call, Pid, Cmd, CmdState} ->
            I2 = lux_debug:eval_cmd(I, Pid, Cmd, CmdState),
            interpret_loop(I2);
        {stopped_by_user, Scope} ->
            %% Ordered to stop by user
            I2 = stopped_by_user(I, Scope),
            interpret_loop(I2);
        {stop, Pid, Res} ->
            %% One shell has finished. Stop the others if needed
            I2 = prepare_stop(I, Pid, Res),
            interpret_loop(I2);
        {break_pattern_matched, _Pid, LoopCmd} ->
            %% Top down search
            LoopStack = I#istate.loop_stack,
            I2 = break_loop(I, LoopCmd, lists:reverse(LoopStack), []),
            interpret_loop(I2#istate{want_more = true});
        {more, Pid, _Name} ->
            if
                Pid =/= I#istate.active_shell#shell.pid ->
                    %% ilog(I, "~s(~p): ignore_more \"~s\"\n",
                    %%      [I#istate.active_name,
                    %%       (I#istate.latest_cmd)#cmd.lineno,
                    %%       Name]),
                    interpret_loop(I);
                I#istate.blocked, not I#istate.want_more ->
                    %% Block more
                    I2 = I#istate{old_want_more = true},
                    interpret_loop(I2);
                not I#istate.blocked, I#istate.old_want_more =:= undefined ->
                    dlog(I, ?dmore, "want_more=true (got more)", []),
                    I2 = I#istate{want_more = true},
                    interpret_loop(I2)
            end;
        {submatch_vars, _From, SubVars} ->
            I2 = I#istate{submatch_vars = SubVars},
            interpret_loop(I2);
        {'DOWN', _, process, Pid, Reason} ->
            I2 = prepare_stop(I, Pid, {'EXIT', Reason}),
            interpret_loop(I2);
        {TimeoutType, TimeoutMillis} when TimeoutType =:= suite_timeout;
                                          TimeoutType =:= case_timeout ->
            I2 = premature_stop(I, TimeoutType, TimeoutMillis),
            interpret_loop(I2);
        IgnoreMsg ->
            lux:trace_me(70, 'case', ignore_msg, [{interpreter_got,IgnoreMsg}]),
            io:format("\nINTERNAL LUX ERROR: Interpreter got: ~p\n",
                      [IgnoreMsg]),
            interpret_loop(I)
    after multiply(I, Timeout) ->
            I2 = opt_dispatch_cmd(I),
            interpret_loop(I2)
    end.

break_loop(I, LoopCmd, [Loop|_Stack] = AllStack, Acc)
  when Loop#loop.cmd =:= LoopCmd ->
    %% Break all inner loops
    Breaks = [L#loop{mode = break} || L <- AllStack],
    I#istate{loop_stack = Breaks  ++ Acc};
break_loop(I, LoopCmd, [Loop|Stack], Acc) ->
    break_loop(I, LoopCmd, Stack, [Loop|Acc]).

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

premature_stop(I, TimeoutType, TimeoutMillis) when I#istate.has_been_blocked ->
    lux:trace_me(70, 'case', TimeoutType, [{ignored, TimeoutMillis}]),
    ilog(I, "~s(~p): ~p (ignored)\n",
         [I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno, TimeoutType]),
    io:format("WARNING: Ignoring ~p"
              " as the script has been attached by the debugger.\n",
              [TimeoutType]),
    I;
premature_stop(I, TimeoutType, TimeoutMillis) ->
    lux:trace_me(70, 'case', TimeoutType, [{premature, TimeoutMillis}]),
    Seconds = TimeoutMillis div timer:seconds(1),
    Multiplier = I#istate.multiplier / 1000,
    ilog(I, "~s(~p): ~p (~p seconds * ~.3f)\n",
         [I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno,
          TimeoutType,
          Seconds,
          Multiplier]),
    case I#istate.mode of
        running ->
            %% The test case (or suite) has timed out.
            prepare_stop(I, dummy_pid, {fail, TimeoutType});
        cleanup ->
            %% Timeout during cleanup

            %% Initiate stop by sending shutdown to all shells.
            multicast(I, {shutdown, self()}),
            I#istate{mode = stopping, cleanup_reason = TimeoutType};
        stopping ->
            %% Shutdown has already been sent to the shells.
            %% Continue to collect their states.
            I
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
                    QuotedVal = quote_val(Val2),
                    ilog(I, "~s(~p): ~p \"~s=~s\"\n",
                         [I#istate.active_name, LineNo, Scope, Var, QuotedVal]),
                    VarVal = lists:flatten([Var, $=, Val2]),
                    case Scope of
                        my ->
                            Vars = [VarVal | I#istate.macro_vars],
                            I#istate{macro_vars = Vars};
                        local when I#istate.active_shell =:= undefined ->
                            throw_error(I, <<"The command must be executed"
                                             " in context of a shell">>);
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
            Cmd2 = compile_regexp(I, Cmd, Arg),
            shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
        fail ->
            Cmd2 = compile_regexp(I, Cmd, Arg),
            shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
        success ->
            Cmd2 = compile_regexp(I, Cmd, Arg),
            shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
        break ->
            Cmd2 = compile_regexp(I, Cmd, Arg),
            if
                I#istate.loop_stack =:= [] ->
                    throw_error(I, <<"The command must be executed"
                                     " in context of a loop">>);
                true ->
                    shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2)
            end;
        sleep ->
            Secs = parse_int(I, Arg, Cmd),
            Cmd2 = Cmd#cmd{arg = Secs},
            shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
        progress ->
            case safe_expand_vars(I, Arg) of
                {ok, String} ->
                    Cmd2 = Cmd#cmd{arg = String},
                    shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
                {no_such_var, BadName} ->
                    no_such_var(I, Cmd, LineNo, BadName)
            end;
        change_timeout ->
            Millis =
                case Arg of
                    "" ->
                        I#istate.default_timeout;
                    "infinity" ->
                        infinity;
                    SecsStr ->
                        Secs = parse_int(I, SecsStr, Cmd),
                        timer:seconds(Secs)
                end,
            Cmd2 = Cmd#cmd{arg = Millis},
            shell_eval(I#istate{latest_cmd = Cmd2}, Cmd2);
        doc ->
            {Level, Doc} = Arg,
            Indent = lists:duplicate((Level-1)*4, $\ ),
            ilog(I, "~s(~p): doc \"~s~s\"\n",
                 [I#istate.active_name, LineNo, Indent, Doc]),
            case I#istate.progress of
                doc -> io:format("\n~s~s\n", [Indent, Doc]);
                _   -> ok
            end,
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
            Zombies = [S#shell{health = zombie} || S <- I3#istate.shells],
            I4 = I3#istate{mode = NewMode,
                           default_timeout = I3#istate.cleanup_timeout,
                           shells = Zombies},
            Suffix =
                case call_level(I4) of
                    1 -> "";
                    N -> integer_to_list(N)
                end,
            ShellCmd = Cmd#cmd{type = shell, arg = "cleanup" ++ Suffix},
            ensure_shell(I4, ShellCmd);
        shell ->
            ensure_shell(I, Cmd);
        include ->
            {include, InclFile, FirstLineNo, LastLineNo, InclCmds} = Arg,
            ilog(I, "~s(~p): include_file \"~s\"\n",
                 [I#istate.active_name, LineNo, InclFile]),
            case copy_orig(I, InclFile) of
                {ok, _} ->
                    eval_include(I, LineNo, FirstLineNo, LastLineNo,
                                 InclFile, InclCmds, Cmd);
                {error, FileReason} ->
                    CaseLogDir = I#istate.case_log_dir,
                    Reason =
                        ["Cannot copy file ", InclFile, " to ", CaseLogDir,
                         ": ", file:format_error(FileReason)],
                    throw_error(I, iolist_to_binary(Reason))
            end;
        macro ->
            I;
        invoke ->
            case lookup_macro(I, Cmd) of
                {ok, NewCmd, MatchingMacros} ->
                    invoke_macro(I, NewCmd, MatchingMacros);
                {error, BadName} ->
                    E = list_to_binary(["Variable $", BadName, " is not set"]),
                    ilog(I, "~s(~p): ~s\n",
                         [I#istate.active_name, LineNo, E]),
                    OrigLine =
                        lux_utils:strip_leading_whitespaces(Cmd#cmd.orig),
                    throw_error(I, <<E/binary, ". Bad line: ",
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

quote_val(IoList) ->
    Replace = fun({From, To}, Acc) ->
                      re:replace(Acc, From, To, [global, {return, binary}])
              end,
    Map = [{<<"\r">>, <<"\\\\r">>},
           {<<"\n">>, <<"\\\\n">>}],
    lists:foldl(Replace, IoList, Map).

shell_eval(I, Cmd) ->
    dlog(I, ?dmore, "want_more=false (send ~p)", [Cmd#cmd.type]),
    cast(I, {eval, self(), Cmd}),
    I#istate{want_more = false}.

eval_include(OldI, InclLineNo, FirstLineNo, LastLineNo,
             InclFile, InclCmds, InclCmd) ->
    DefaultFun = get_eval_fun(),
    eval_body(OldI, InclLineNo, FirstLineNo, LastLineNo,
              InclFile, InclCmds, InclCmd, DefaultFun, false).

get_eval_fun() ->
    fun(I) when is_record(I, istate) -> interpret_loop(I) end.

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
    BeforeI2 = switch_cmd(before, BeforeI, NewStack, Cmd,
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
        AfterI2 = switch_cmd('after', AfterI, OldStack, Cmd,
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
                    switch_cmd('after', BeforeI2, OldStack, Cmd,
                               BeforeExit, IsRootLoop)
            end,
            erlang:raise(Class, Reason, erlang:get_stacktrace())
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
    MacroVars = macro_vars(I, ArgNames, ArgVals, InvokeCmd),
    ilog(I, "~s(~p): invoke_~s \"~s\"\n",
         [I#istate.active_name,
          LineNo,
          Name,
          lists:flatten(string:join(MacroVars, " "))]),

    BeforeI = I#istate{macro_vars = MacroVars, latest_cmd = InvokeCmd},
    DefaultFun = get_eval_fun(),
    AfterI = eval_body(BeforeI, LineNo, FirstLineNo, LastLineNo,
                       MacroFile, Body, MacroCmd, DefaultFun, false),

    AfterI#istate{macro_vars = OldMacroVars};
invoke_macro(I, #cmd{arg = {invoke, Name, _Values}}, []) ->
    BinName = list_to_binary(Name),
    throw_error(I, <<"No such macro: ", BinName/binary>>);
invoke_macro(I, #cmd{arg = {invoke, Name, _Values}}, [_|_]) ->
    BinName = list_to_binary(Name),
    throw_error(I, <<"Ambiguous macro: ", BinName/binary>>).

macro_vars(I, [Name | Names], [Val | Vals], Invoke) ->
    case safe_expand_vars(I, Val) of
        {ok, Val2} ->
            [lists:flatten([Name, $=, Val2]) |
             macro_vars(I, Names, Vals, Invoke)];
        {no_such_var, BadName} ->
            no_such_var(I, Invoke, Invoke#cmd.lineno, BadName)
    end;
macro_vars(_I, [], [], _Invoke) ->
    [];
macro_vars(I, _Names, _Vals, #cmd{arg = {invoke, Name, _}, lineno = LineNo}) ->
    BinName = list_to_binary(Name),
    BinLineNo = list_to_binary(integer_to_list(LineNo)),
    Reason = <<"at ", BinLineNo/binary,
               ": Argument mismatch in macro: ", BinName/binary>>,
    throw_error(I, Reason).

compile_regexp(_I, Cmd, reset) ->
    Cmd;
compile_regexp(I, Cmd, {endshell, RegExpOper, RegExp}) ->
    Cmd2 = compile_regexp(I, Cmd, {regexp, RegExpOper, RegExp}),
    {mp, RegExpOper, RegExp2, MP2, _Multi} = Cmd2#cmd.arg,
    Cmd2#cmd{arg = {endshell, RegExpOper, RegExp2, MP2}};
compile_regexp(_I, Cmd, {verbatim, _RegExpOper, _Verbatim}) ->
    Cmd;
compile_regexp(_I, Cmd, {mp, _RegExpOper, _RegExp, _MP, _Multi}) ->
    Cmd;
compile_regexp(I, Cmd, {template, RegExpOper, Template}) ->
    case safe_expand_vars(I, Template) of
        {ok, Verbatim} ->
            Cmd#cmd{arg = {verbatim, RegExpOper, Verbatim}};
        {no_such_var, BadName} ->
            no_such_var(I, Cmd, Cmd#cmd.lineno, BadName)
    end;
compile_regexp(I, Cmd, {regexp, RegExpOper, RegExp}) ->
    case safe_expand_vars(I, RegExp) of
        {ok, RegExp2} ->
            RegExp3 = lux_utils:normalize_newlines(RegExp2),
            Opts = [multiline, {newline, anycrlf}],
            case re:compile(RegExp3, Opts) of
                {ok, MP3} ->
                    Cmd#cmd{arg = {mp, RegExpOper, RegExp3, MP3, []}};
                {error, {Reason, _Pos}} ->
                    BinErr = list_to_binary(["Syntax error: ", Reason,
                                             " in regexp '", RegExp3, "'"]),
                    throw_error(I, BinErr)
            end;
        {no_such_var, BadName} ->
            no_such_var(I, Cmd, Cmd#cmd.lineno, BadName)
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
    E = list_to_binary(["Variable $", BadName, " is not set"]),
    ilog(I, "~s(~p): ~s\n", [I#istate.active_name, LineNo, E]),
    OrigLine = lux_utils:strip_leading_whitespaces(Cmd#cmd.orig),
    throw_error(I, <<E/binary, ". Bad line: ", OrigLine/binary>>).

parse_int(I, Chars, Cmd) ->
    case safe_expand_vars(I, Chars) of
        {ok, Chars2} ->
            try
                list_to_integer(Chars2)
            catch
                error:_ ->
                    BinErr =
                        list_to_binary(["Syntax error at line ",
                                        integer_to_list(Cmd#cmd.lineno),
                                        ": '", Chars2, "' integer expected"]),
                    throw_error(I, BinErr)
            end;
        {no_such_var, BadName} ->
            no_such_var(I, Cmd, Cmd#cmd.lineno, BadName)
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
                [NewItem|NewItems] = [integer_to_list(I) || I <- Seq],
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
                Expected = lux_utils:cmd_expected(LatestCmd),
                {fail,
                 #result{outcome    = fail,
                         latest_cmd = LatestCmd,
                         cmd_stack  = CmdStack,
                         expected   = Expected,
                         extra      = undefined,
                         actual     = FailReason,
                         rest       = fail}};
            {fail, FailReason} ->
                Expected = lux_utils:cmd_expected(LatestCmd),
                {fail,
                 #result{outcome    = fail,
                         latest_cmd = LatestCmd,
                         cmd_stack  = CmdStack,
                         expected   = Expected,
                         extra      = undefined,
                         actual     = FailReason,
                         rest       = fail}}
        end,
    Res3 =
        case Res2 of
            #result{actual = <<"fail pattern matched ", _/binary>>} ->
                Res2#result{latest_cmd = LatestCmd,
                            cmd_stack = CmdStack};
            #result{actual = <<"success pattern matched ", _/binary>>} ->
                Res2#result{latest_cmd = LatestCmd,
                            cmd_stack = CmdStack};
            _ ->
                Res2
        end,
    {CleanupReason, Res3}.

goto_cleanup(OldI, CleanupReason) ->
    lux:trace_me(50, 'case', goto_cleanup, [{reason, CleanupReason}]),
    LineNo = integer_to_list((OldI#istate.latest_cmd)#cmd.lineno),
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
    CleanupFun = fun(#cmd{type = Type}) -> Type =/= cleanup end,
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
    throw_error(I, <<"The command must be executed in context of a shell">>);
cast(#istate{active_shell = #shell{pid =Pid}, active_name = Name}, Msg) ->
    trace_msg(#shell{name=Name}, Msg),
    Pid ! Msg,
    Pid.

trace_msg(#shell{name = Name}, Msg) ->
    lux:trace_me(50, 'case', Name, element(1, Msg), [Msg]).

multisync(I, Msg) when Msg =:= flush;
                       Msg =:= immediate;
                       Msg =:= wait_for_expect ->
    Pids = multicast(I, {sync, self(), Msg}),
    lux:trace_me(50, 'case', waiting,
                 [{active_shell, I#istate.active_shell},
                  {shells, I#istate.shells},
                  Msg]),
    I2 = wait_for_reply(I, Pids, sync_ack, undefined, infinity),
    lux:trace_me(50, 'case', collected, []),
    I2.

wait_for_reply(I, [Pid | Pids], Expect, Fun, FlushTimeout) ->
    receive
        {Expect, Pid} ->
            wait_for_reply(I, Pids, Expect, Fun, FlushTimeout);
        {Expect, Pid, Expected} when Expect =:= expected, Pids =:= [] ->
            Expected;
        {stop, SomePid, Res} ->
            I2 = prepare_stop(I, SomePid, Res),
            wait_for_reply(I2, [Pid|Pids], Expect, Fun, FlushTimeout);
        {'DOWN', _, process, Pid, Reason} ->
            opt_apply(Fun),
            shell_crashed(I, Pid, Reason);
        {TimeoutType, TimeoutMillis} when TimeoutType =:= suite_timeout;
                                          TimeoutType =:= case_timeout ->
            I2 = premature_stop(I, TimeoutType, TimeoutMillis),
            wait_for_reply(I2, [], Expect, Fun, 500);
        IgnoreMsg when FlushTimeout =/= infinity ->
            lux:trace_me(70, 'case', ignore_msg, [{interpreter_got,IgnoreMsg}]),
            io:format("\nINTERNAL LUX ERROR: Interpreter got: ~p\n",
                      [IgnoreMsg]),
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
    I2 = change_active_mode(I, Cmd, suspend),
    I3 = inactivate_shell(I2, I2#istate.want_more),
    case safe_expand_vars(I3, "$LUX_EXTRA_LOGS") of
        {ok, ExtraLogs} ->
            case lux_shell:start_monitor(I3, Cmd, Name, ExtraLogs) of
                {ok, I4} ->
                    %% Wait for some shell output
                    Wait = Cmd#cmd{type = expect,
                                   arg = {regexp, single, <<".+">>}},
                    %% Set the prompt (after the rc files has ben run)
                    CmdStr = iolist_to_binary([I4#istate.shell_prompt_cmd,
                                               "\n"]),
                    Prompt = Cmd#cmd{type = send,
                                     arg = CmdStr},
                    %% Wait for the prompt
                    CmdRegExp = list_to_binary(I4#istate.shell_prompt_regexp),
                    Sync = Cmd#cmd{type = expect,
                                   arg = {regexp, single, CmdRegExp}},
                    Cmds = [Wait, Prompt, Sync | I4#istate.commands],
                    dlog(I4, ?dmore, "want_more=false (shell_start)", []),
                    I4#istate{commands = Cmds};
                {error, I4, Pid, Reason} ->
                    shell_crashed(I4, Pid, Reason)
            end;
        {no_such_var, BadName} ->
            no_such_var(I3, Cmd, Cmd#cmd.lineno, BadName)
    end.

shell_switch(OldI, Cmd, #shell{health = alive, name = NewName} = NewShell) ->
    %% Activate shell
    I2 = change_active_mode(OldI, Cmd, suspend),
    I3 = inactivate_shell(I2, I2#istate.want_more),
    NewShells = lists:keydelete(NewName, #shell.name, I3#istate.shells),
    NewI = I3#istate{active_shell = NewShell,
                     active_name = NewName,
                     shells = NewShells
                    },
    change_active_mode(NewI, Cmd, resume);
shell_switch(OldI, _Cmd, #shell{name = Name, health = zombie}) ->
    ilog(OldI, "~s(~p): zombie shell at cleanup\n",
         [Name, (OldI#istate.latest_cmd)#cmd.lineno]),
    throw_error(OldI, list_to_binary(Name ++ " is a zombie shell")).

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
    Pid = cast(I, {change_mode, self(), NewMode, Cmd, I#istate.cmd_stack}),
    wait_for_reply(I, [Pid], change_mode_ack, undefined, infinity);
change_active_mode(I, _Cmd, _NewMode) ->
    %% No active shell
    I.

switch_cmd(When, #istate{active_shell = undefined} = I,
           CmdStack, NewCmd, Fun, IsRootLoop) ->
    Fun(),
    do_switch_cmd(When, I, CmdStack, NewCmd, IsRootLoop, []);
switch_cmd(When, #istate{active_shell = #shell{pid = ActivePid}} = I,
           CmdStack, NewCmd, Fun, IsRootLoop) ->
    Msg = {switch_cmd, self(), When, IsRootLoop, NewCmd, CmdStack, Fun},
    ActivePid = cast(I, Msg),
    do_switch_cmd(When, I, CmdStack, NewCmd, IsRootLoop, [ActivePid]).

do_switch_cmd(When, I, CmdStack, NewCmd, IsRootLoop, ActivePids) ->
    Fun = fun() -> ignore end,
    Msg = {switch_cmd, self(), When, IsRootLoop, NewCmd, CmdStack, Fun},
    OtherPids = multicast(I#istate{active_shell = undefined}, Msg),
    Pids = ActivePids ++ OtherPids,
    wait_for_reply(I, Pids, switch_cmd_ack, Fun, infinity).

shell_crashed(I, Pid, Reason) when Pid =:= I#istate.active_shell#shell.pid ->
    I2 = inactivate_shell(I, I#istate.want_more),
    shell_crashed(I2, Pid, Reason);
shell_crashed(I, Pid, Reason) ->
    I2 = prepare_stop(I, Pid, {'EXIT', Reason}),
    What =
        case lists:keyfind(Pid, #shell.pid, I2#istate.shells) of
            false -> ["Process ", io_lib:format("~p", [Pid])];
            Shell -> ["Shell ", Shell#shell.name]
        end,
    Error =
        case Reason of
            {error, ErrBin} ->
                ErrBin;
            _ ->
                list_to_binary( [What, " crashed: ",
                                 io_lib:format("~p\n~p",
                                               [Reason, ?stacktrace()])])
        end,
    throw_error(I2, Error).

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

double_ilog(#istate{progress = Progress, log_fun = LogFun, event_log_fd = Fd},
            Format,
            Args) ->
    Bin = lux_log:safe_format(silent, LogFun, undefined, Format, Args),
    lux_log:safe_write(Progress, LogFun, Fd, Bin).

ilog(#istate{progress = Progress, log_fun = LogFun, event_log_fd = Fd},
     Format,
     Args)->
    lux_log:safe_format(Progress, LogFun, Fd, Format, Args).

dlog(I, Level, Format, Args) when I#istate.debug_level >= Level ->
    ilog(I, "~s(~p): debug2 \"" ++ Format ++ "\"\n",
         [I#istate.active_name, (I#istate.latest_cmd)#cmd.lineno] ++ Args);
dlog(_I, _Level, _Format, _Args) ->
    ok.

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
        infinity -> infinity;
        _        -> lux_utils:multiply(Timeout, Factor)
    end.

default_istate(File) ->
    #istate{file = lux_utils:normalize(File),
            log_fun = fun(Bin) -> console_write(binary_to_list(Bin)), Bin end,
            shell_wrapper = default_shell_wrapper(),
            builtin_vars = lux_utils:builtin_vars(),
            system_vars = lux_utils:system_vars()}.

default_shell_wrapper() ->
    Wrapper = filename:join([code:priv_dir(?APPLICATION), "bin", "runpty"]),
    case filelib:is_regular(Wrapper) of
        true  -> Wrapper;
        false -> undefined
    end.

throw_error(#istate{active_shell = ActiveShell, shells = Shells,
                    file = _File, latest_cmd = _Cmd} = I, Reason)
  when is_binary(Reason) ->
    %% Reason = iolist_to_binary([File, ":", integer_to_list(Cmd#cmd.lineno),
    %%                            ": ", Reason0]),
    lux:trace_me(50, 'case', error,
                 [{active_shell, ActiveShell}, {shells, Shells}, Reason]),
    %% Exit all shells before the interpreter is exited
    multicast(I, {'DOWN', undefined, process, self(), shutdown}), % Fake exit
    I2 = flush_stop(I#istate{mode = stopping}, [ActiveShell | Shells]),
    throw({error, Reason, I2}).

flush_stop(I, []) ->
    I;
flush_stop(I, [undefined | Shells]) ->
    flush_stop(I, Shells);
flush_stop(I, [#shell{pid = Pid} | Shells]) ->
    receive
        {'DOWN', _, process, P, Reason} when P =:= Pid ->
            I2 = prepare_stop(I, Pid, {'EXIT', Reason}),
            flush_stop(I2, Shells);
        {stop, P, Res} when P =:= Pid ->
            %% One shell has finished. Stop the others if needed
            I2 = prepare_stop(I, Pid, Res),
            flush_stop(I2, Shells)
    end.
