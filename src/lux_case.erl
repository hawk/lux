%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_case).

-include("lux.hrl").

-export([
         interpret_commands/6,
         default_istate/1,
         parse_iopts/2,
         config_type/1,
         user_config_types/0,
         set_config_val/6,
         set_config_vals/6,
         copy_orig/2,
         case_log_dir/2
        ]).

interpret_commands(Script, Cmds, Warnings, StartTime, Opts, Opaque) ->
    %% io:format("\nCmds ~p\n", [Cmds]),
    I = default_istate(Script),
    case lists:keyfind(stopped_by_user, 1, Opaque) of
        {_, Context = suite} -> ok;
        _                    -> Context = undefined
    end,
    I2 = I#istate{commands = Cmds,
                  warnings = Warnings,
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
                                eval(I5, StartTime, Progress, Verbose, LogFun,
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
            EST = erlang:get_stacktrace(),
            internal_error(I2, {'EXIT', {fatal_error, Class, Reason, EST}})
    end.

default_istate(File) ->
    #istate{file = lux_utils:normalize_filename(File),
            log_fun = fun(Bin) -> console_write(?b2l(Bin)), Bin end,
            shell_wrapper = default_shell_wrapper(),
            builtin_vars = lux_utils:builtin_vars(),
            system_vars = lux_utils:system_vars()}.

default_shell_wrapper() ->
    Wrapper = filename:join([code:priv_dir(?APPLICATION), "bin", "runpty"]),
    case filelib:is_regular(Wrapper) of
        true  -> Wrapper;
        false -> undefined
    end.

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

eval(OldI, StartTime, Progress, Verbose,
     LogFun, EventLog, EventFd, ConfigFd, Docs) ->
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
                    Res = lux_interpret:init(NewI, StartTime),
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
                        global_vars  = shrinked},
        garbage_collect(),
        wait_for_done(ShrinkedI, Pid, Docs)
    after
        process_flag(trap_exit, Flag),
        lux_log:close_event_log(EventFd),
        file:write(ConfigFd, "\n"),
        file:close(ConfigFd) % Don't care of failure
    end.

internal_error(I, ReasonTerm) ->
    ReasonBin = ?l2b(io_lib:format("INTERNAL LUX ERROR: ~p\n",
                                             [ReasonTerm])),
    fatal_error(I, ReasonBin).

fatal_error(I, ReasonBin) when is_binary(ReasonBin) ->
    FullLineNo = full_lineno(I, I#istate.latest_cmd, I#istate.cmd_stack),
    double_ilog(I, "~sERROR ~s\n", [?TAG("result"), ?b2l(ReasonBin)]),
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
    File = lux_utils:normalize_filename(I#istate.file),
    case I#istate.shell_wrapper of
        "" -> ShellWrapper = undefined;
        ShellWrapper -> ok
    end,
    SuiteLogDir = lux_utils:normalize_filename(I#istate.suite_log_dir),
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
        unstable ->
            {ok, #istate.unstable, [{std_list, [string]}]};
        unstable_unless ->
            {ok, #istate.unstable_unless, [{std_list, [string]}]};
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
            {error, ?l2b(lists:concat(["Bad argument: ", Name]))}
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
                Val2 = lux_interpret:expand_vars(I, Val, error),
                {{ok, setelement(Pos, I, Val2)}, U2};
            binary when is_binary(Val) ->
                Val2 = lux_interpret:expand_vars(I, Val, error),
                {{ok, setelement(Pos, I, Val2)}, U2};
            binary when is_list(Val) ->
                Val2 = lux_interpret:expand_vars(I, Val, error),
                set_config_val(Name, ?l2b(Val2), [Type], Pos, I, U);
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
            {{error, ?l2b(lists:concat(Msg))}, U};
        _Class:_Reason ->
            set_config_val(Name, Val, Types, Pos, I, U)
    end;
set_config_val(Name, Val, [], _Pos, _I, Updated) ->
    {{error, ?l2b(lists:concat(["Bad argument: ",
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
                 _Warnings, _Results, _FailBin, _NewOpaque} ->
                    ok;
                {error, File, FullLineNo, CaseLogDir, _} ->
                    ok
            end,
            {error, File, FullLineNo, CaseLogDir, <<"suite_timeout">>};
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
    LatestCmd = I#istate.latest_cmd,
    FullLineNo = ?i2l(LatestCmd#cmd.lineno),
    Warnings = I#istate.warnings,
    Outcome =
        if
            Warnings =/= [] ->
                double_ilog(I, "~sWARNING\n", [?TAG("result")]),
                warning;
            true ->
                double_ilog(I, "~sSUCCESS\n", [?TAG("result")]),
                success
        end,
    {ok, Outcome, File, FullLineNo, I#istate.case_log_dir, Warnings,
     Results, <<>>, [{stopped_by_user, I#istate.stopped_by_user}]}.

print_fail(OldI0, NewI, File, Results,
           #result{outcome    = fail,
                   latest_cmd = LatestCmd,
                   cmd_stack  = CmdStack,
                   expected   = Expected,
                   extra      = _Extra,
                   actual     = Actual,
                   rest       = Rest}) ->
    OldI = OldI0#istate{progress = silent},
    OldWarnings = OldI#istate.warnings,
    FullLineNo = full_lineno(OldI, LatestCmd, CmdStack),
    UnstableWarnings = unstable_warnings(OldI, FullLineNo),
    {Outcome, Warnings, ResStr} =
        if
            UnstableWarnings =/= [] ->
                {warning,
                 OldWarnings ++ UnstableWarnings,
                 double_ilog(OldI, "~sWARNING at ~s\n",
                             [?TAG("result"), FullLineNo])
                };
            true ->
                {fail,
                 OldWarnings,
                 double_ilog(OldI, "~sFAIL at ~s\n",
                             [?TAG("result"), FullLineNo])
                }
        end,
    {NewActual, NewRest} =
        case Actual of
            <<"fail pattern matched ",  _/binary>> ->
                {Actual, Rest};
            <<"success pattern matched ", _/binary>> ->
                {Actual, Rest};
            _ when is_atom(Actual), is_binary(Rest) ->
                {atom_to_list(Actual), Rest};
            _ when is_atom(Actual), is_atom(Rest) ->
                {atom_to_list(Actual), list_to_binary(atom_to_list(Rest))};
            _ when is_binary(Actual) ->
                {<<"error">>, Actual}
        end,
    Diff = lux_utils:shrink_diff(Expected, NewRest),
    FailBin =
        ?l2b(
          [
           io_lib:format("expected\n\t~s\n",
                         [simple_to_string(Expected)]),
           io_lib:format("actual ~s\n\t~s\n",
                         [NewActual, simple_to_string(NewRest)]),
           io_lib:format("diff\n\t~s",
                         [simple_to_string(Diff)])
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
    {ok, Outcome, File, FullLineNo, NewI#istate.case_log_dir, Warnings,
     Results, FailBin, Opaque}.

unstable_warnings(#istate{unstable=U, unstable_unless=UU} = I, FullLineNo) ->
    F = fun(Var, NameVal) -> filter_unstable(I, FullLineNo, Var, NameVal) end,
    Unstable = lists:zf(fun(Val) -> F("unstable", Val) end, U),
    UnstableUnless = lists:zf(fun(Val) -> F("unstable_unless", Val) end, UU),
    Unstable ++ UnstableUnless.

filter_unstable(#istate{orig_file = File} = I, FullLineNo, Var, NameVal) ->
    case Var of
        "unstable" ->
            {IsSet, Name} = test_var(I, NameVal),
            case IsSet of
                false ->
                    false;
                true ->
                    Format = "Fail but UNSTABLE as variable ~s is set",
                    Reason = ?l2b(io_lib:format(Format, [Name])),
                    {true, {warning, File, FullLineNo, Reason}}
            end;
        "unstable_unless" ->
            {IsSet, Name} = test_var(I, NameVal),
            case IsSet of
                true ->
                    false;
                false ->
                    Format = "Fail but UNSTABLE as variable ~s is not set",
                    Reason = ?l2b(io_lib:format(Format, [Name])),
                    {true, {warning, File, FullLineNo, Reason}}
            end
    end.

test_var(#istate{builtin_vars = BuiltinVars,
                 system_vars  = SystemVars},
              VarVal) ->
    MultiVars = [BuiltinVars, SystemVars],
    lux_utils:test_var(MultiVars, VarVal).

full_lineno(I, #cmd{lineno = LineNo, type = Type}, CmdStack) ->
    RevFile = lux_utils:filename_split(I#istate.file),
    CmdPos = #cmd_pos{rev_file = RevFile, lineno = LineNo, type = Type},
    FullStack = [CmdPos | CmdStack],
    lux_utils:pretty_full_lineno(FullStack).

post_ilog(#istate{progress = Progress,
                  logs = Logs,
                  config_log_fd = {_, ConfigFd}} = I,
          Docs) ->
    lux_log:close_config_log(ConfigFd, Logs),
    log_doc(I, Docs),
    lux_interpret:ilog(I, "\n", []),
    LogFun =
        fun(Bin) ->
                case Progress of
                    silent -> ok;
                    _      -> console_write(?b2l(Bin))
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
    Prefix = ?l2b(?TAG("doc")),
    Fun =
        fun({Level, Doc}) ->
                Tabs = ?l2b(lists:duplicate(Level-1, $\t)),
                LogFun(<<Prefix/binary, Tabs/binary, Doc/binary, "\n">>)
        end,
    lists:foreach(Fun, Docs).

simple_to_string(Atom) when is_atom(Atom) ->
    simple_to_string(atom_to_list(Atom));
simple_to_string(Bin) when is_binary(Bin) ->
    simple_to_string(?b2l(Bin));
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
     unstable_unless,
     unstable,
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

double_ilog(#istate{progress = Progress, log_fun = LogFun, event_log_fd = Fd},
            Format,
            Args) ->
    Bin = lux_log:safe_format(silent, LogFun, undefined, Format, Args),
    lux_log:safe_write(Progress, LogFun, Fd, Bin).

console_write(String) ->
    io:format("~s", [String]).
