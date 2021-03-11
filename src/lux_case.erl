%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2021 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_case).

-export([
         interpret_commands/7,
         default_istate/1,
         parse_iopts/2,
         config_type/1,
         user_config_types/0,
         set_config_val/6,
         set_config_vals/6,
         copy_orig/2,
         case_log_dir/2
        ]).

-include("lux.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interpret parsed script

-spec interpret_commands(filename(),
                         cmds(),
                         [#warning{}],
                         erlang:timestamp(),
                         #timer_ref{},
                         opts(),
                         [{atom(), term()}]) ->
             [{ok, summary(), filename(), [result()]} | error()].

interpret_commands(Script, Cmds, Warnings, StartTime, SuiteRef, Opts, Opaque) ->
    %% io:format("\nCmds ~p\n", [Cmds]),
    I = default_istate(Script),
    case lists:keyfind(stopped_by_user, 1, Opaque) of
        {_, Context = suite} -> ok;
        _                    -> Context = undefined
    end,
    I2 = I#istate{commands = Cmds,
                  warnings = Warnings,
                  orig_commands = shrinked,
                  stopped_by_user = Context,
                  start_time = StartTime,
                  suite_timer_ref = SuiteRef},
    try
        case parse_iopts(I2, Opts) of
            {ok, I3} ->
                I4 = check_timeout(I3),
                open_logs_and_eval(I4);
            {error, ParseReason} ->
                internal_error(I2, ParseReason)
        end
    catch
        ?CATCH_STACKTRACE(Class, Reason, EST)
            internal_error(I2, {'EXIT', {fatal_error, Class, Reason, EST}})
    end.

check_timeout(#istate{suite_timeout = SuiteTimeout,
                      case_timeout = CaseTimeout,
                      orig_file = File,
                      latest_cmd = LatestCmd,
                      pos_stack = PosStack} = I)
  when is_integer(SuiteTimeout),
       is_integer(CaseTimeout),
       CaseTimeout > SuiteTimeout ->
    FullLineNo = lux_utils:full_lineno(File, LatestCmd, PosStack),
    Reason =  "case_timeout > suite_timeout",
    W = lux_utils:make_warning(File, FullLineNo, Reason),
    I#istate{warnings = [W | I#istate.warnings]};
check_timeout(I) ->
    I.

open_logs_and_eval(I) ->
    Script = I#istate.file,
    Cmds = I#istate.commands,
    CaseLogDir = case_log_dir(I, Script),
    I2 = I#istate{case_log_dir = CaseLogDir},
    case copy_orig(I2, Script) of
        {ok, Base} ->
            ExtraLogs = filename:join([CaseLogDir,
                                       Base ++ ?CASE_EXTRA_LOGS]),
            ExtraVars = "LUX_EXTRA_LOGS=" ++ ExtraLogs,
            GlobalVars = [ExtraVars | I2#istate.global_vars],
            I3 = I2#istate{global_vars = GlobalVars},
            Config = config_data(I3),
            ConfigFd = lux_log:open_config_log(CaseLogDir, Script, Config),
            Progress = I3#istate.progress,
            LogFun = I3#istate.log_fun,
            Verbose = true,
            EmitTimestamp = I3#istate.emit_timestamp,
            case lux_log:open_event_log(CaseLogDir, Script,
                                        Progress,
                                        LogFun, Verbose,
                                        EmitTimestamp) of
                {ok, EventLog, EventFd} ->
                    Docs = docs(I3#istate.orig_file, Cmds),
                    eval(I3, Progress, Verbose, LogFun,
                         EventLog, EventFd, ConfigFd, Docs);
                {error, FileReason} ->
                    internal_error(I3, file:format_error(FileReason))
            end;
        {error, FileReason} ->
            internal_error(I2, file:format_error(FileReason))
    end.

default_istate(File) ->
    #istate{top_pid = self(),
            file = lux_utils:normalize_filename(File),
            log_fun = fun(Bin) -> console_write(?b2l(Bin)), Bin end,
            shell_wrapper = default_shell_wrapper(),
            builtin_vars = lux_utils:builtin_vars(),
            system_vars = lux_utils:system_vars(),
            emit_timestamp = lux_utils:has_timestamp()}.

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
    case lux_utils:drop_prefix(AbsScript) of
        ".." ++ _  -> RelScript0 = AbsScript;
        RelScript0 -> ok
    end,
    RelScript =
        case filename:pathtype(RelScript0) of
            absolute -> tl(RelScript0);
            _Type    -> RelScript0
        end,
    RelDir = filename:dirname(RelScript),
    filename:join([SuiteLogDir, RelDir]).

eval(OldI, Progress, Verbose,
     LogFun, EventLog, EventFd, ConfigFd, Docs) ->
    TraceMode =
        case dbg:get_tracer() of
            {error, _} -> none;
            {ok, _}    -> suite
        end,
    NewI = OldI#istate{event_log_fd =  {Verbose, EventFd},
                       config_log_fd = {Verbose, ConfigFd},
                       trace_mode = TraceMode},
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
        progress_warnings(OldI, OldI#istate.warnings),
        ReplyTo = self(),
        Interpret =
            fun() ->
                    Dpid = lux_debug:start_link(OldI#istate.debug_file),
                    DbgI = NewI#istate{debug_pid = Dpid},
                    ReplyTo ! {debug_pid, DbgI},
                    Res = lux_interpret:init(DbgI),
                    ?TRACE_ME2(70, 'case', shutdown, []),
                    unlink(ReplyTo),
                    ReplyTo ! {done, self(), Res},
                    exit(shutdown)
            end,
        Ipid = spawn_link(Interpret),
        receive
            {debug_pid, DbgI} -> ok
        end,
        %% Poor mans hibernate
        ShrinkedI =
            DbgI#istate{commands     = shrinked,
                        macro_vars   = shrinked,
                        global_vars  = shrinked},
        garbage_collect(),
        wait_for_done(ShrinkedI, Ipid, Docs)
    after
        process_flag(trap_exit, Flag),
        lux_log:close_event_log(EventFd),
        file:write(ConfigFd, "\n"),
        file:close(ConfigFd) % Don't care of failure
    end.

internal_error(I, ReasonTerm) ->
    ReasonBin = ?l2b(?FF("INTERNAL LUX ERROR: ~p\n", [ReasonTerm])),
    fatal_error(I, ReasonBin).

fatal_error(I, ReasonBin) when is_binary(ReasonBin) ->
    FullLineNo = lux_utils:full_lineno(I#istate.file,
                                       I#istate.latest_cmd,
                                       I#istate.pos_stack),
    RunWarnings = I#istate.warnings,
    UnstableWarnings = [],
    print_warnings(I, RunWarnings, UnstableWarnings),
    double_ilog(I, "~sERROR ~s\n", [?TAG("result"), ?b2l(ReasonBin)]),
    {error, I#istate.file, FullLineNo, I#istate.case_log_dir,
     RunWarnings, UnstableWarnings, ReasonBin}.

print_warnings(I, RunWarnings, UnstableWarnings) ->
    P = fun(#warning{lineno=FullLineNo, reason=Reason}) ->
                double_ilog(I, "~s~s: ~s\n",
                            [?TAG("warning"), FullLineNo, Reason])
        end,
    lists:foreach(P, RunWarnings),
    lists:foreach(P, UnstableWarnings).

progress_warnings(I, Warnings) ->
    [lux_utils:progress_write(I#istate.progress, "W") || _W <- Warnings].

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
    case I#istate.post_cleanup_cmd of
        "" -> PostCleanup = undefined;
        PostCleanup -> ok
    end,
    SuiteLogDir = lux_utils:normalize_filename(I#istate.suite_log_dir),
    I2 = I#istate{file = File,
                  orig_file = File,
                  shell_wrapper = ShellWrapper,
                  suite_log_dir = SuiteLogDir,
                  case_log_dir = SuiteLogDir,
                  post_cleanup_cmd = PostCleanup},
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
        skip_skip ->
            {ok, #istate.skip_skip, [{atom, [true, false]}]};
        fail_when_warning ->
            {ok, #istate.fail_when_warning, [{atom, [true, false]}]};
        require ->
            {ok, #istate.require, [{std_list, [string]}]};
        case_prefix ->
            {ok, #istate.case_prefix, [string]};
        config_dir ->
            {ok, #istate.config_dir, [string]};
        progress ->
            {ok, #istate.progress,
             [{atom, [silent, summary, brief, doc, compact, verbose,
                      etrace, ctrace]}]};
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
            {ok, #istate.poll_timeout, [{integer, -1, infinity}]};
        timeout ->
            {ok, #istate.default_timeout, [{integer, 0, infinity},
                                           {atom, [infinity]}]};
        cleanup_timeout ->
            {ok, #istate.cleanup_timeout, [{integer, 0, infinity},
                                           {atom, [infinity]}]};
        risky_threshold ->
            {ok, #istate.risky_threshold, [{float, 0.0, infinity}]};
        sloppy_threshold ->
            {ok, #istate.sloppy_threshold, [{float, 0.0, infinity}]};
        newshell ->
            {ok, #istate.newshell, [{atom, [false, true]}]};
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
        post_cleanup_cmd ->
            {ok, #istate.post_cleanup_cmd, [string,
                                            {atom, [undefined]}]};
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
                set_config_val(Name, ?l2a(Val), [Type], Pos, I, U);
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

            {float, infinity, infinity} when is_float(Val) ->
                {{ok, setelement(Pos, I, Val)}, U2};
            {float, infinity, Max}
              when is_float(Val), is_float(Max), Val =< Max ->
                {{ok, setelement(Pos, I, Val)}, U2};
            {float, Min, infinity}
              when is_float(Val), is_float(Min), Val >= Min ->
                {{ok, setelement(Pos, I, Val)}, U2};
            {float, Min, Max}
              when is_float(Val), is_float(Min), is_float(Max),
                   Val >= Min, Val =< Max ->
                {{ok, setelement(Pos, I, Val)}, U2};
            {float, _Min, _Max} when is_list(Val) ->
                set_config_val(Name, list_to_float(Val), [Type], Pos, I, U);

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
                   "; ${", BadName, "} is not set"],
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
                 RunWarnings, UnstableWarnings, _Results,
                 _FailBin, _NewOpaque} ->
                    ok;
                {error, File, FullLineNo, CaseLogDir,
                 RunWarnings, UnstableWarnings, _} ->
                    ok
            end,
            {error, File, FullLineNo, CaseLogDir,
             RunWarnings, UnstableWarnings, <<"suite_timeout">>};
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
    File = NewI0#istate.file,
    Results = NewI0#istate.results,
    OldWarnings = NewI0#istate.warnings,
    ExtraWarnings = [R#result.warnings || R <- Results],
    NewWarnings = lists:flatten([OldWarnings, ExtraWarnings]),
    NewI = post_ilog(NewI0#istate{warnings = NewWarnings}, Docs),
    case lists:keyfind('EXIT', 1, Results) of
        false ->
            R = pick_fail(NewI, Results),
            IsSuccess = (R =:= false),
            if
                IsSuccess,
                NewWarnings =/= [],
                NewI#istate.fail_when_warning ->
                    WarnR = make_warning_fail(NewI),
                    print_fail(OldI, NewI, File, Results, WarnR);
                IsSuccess ->
                    print_success(NewI, File);
                not IsSuccess ->
                    print_fail(OldI, NewI, File, Results, R)
            end;
        {'EXIT', Reason} ->
            internal_error(NewI, {'EXIT', Reason})
    end.

pick_fail(NewI, Results) ->
    Failed =
        [R || R <- Results,
              R#result.outcome =:= fail],
    Stopping =
        [R || R <- Failed,
              R#result.mode =:= stopping],
    case Stopping of
        [] ->
            Cleanup =
                [R || R <- Failed,
                      R#result.mode =:= cleanup],
            case Cleanup of
                [] ->
                    Reason = NewI#istate.cleanup_reason,
                    case Failed of
                        [] when Reason =:= normal;
                                Reason =:= success ->
                            false;
                        [] ->
                            make_cleanup_fail(NewI, Reason);
                        [R | _] ->
                            R
                    end;
                [R | _] ->
                    R
            end;
        [R | _] ->
            R
    end.

make_cleanup_fail(I, Reason) ->
    #result{outcome      = fail,
            latest_cmd   = I#istate.latest_cmd,
            pos_stack    = I#istate.pos_stack,
            shell_name   = I#istate.active_name,
            expected_tag = ?EXPECTED_OLD,
            expected     = success,
            extra        = undefined,
            actual       = Reason,
            rest         = fail,
            warnings     = I#istate.warnings}.

make_warning_fail(#istate{warnings = Warnings} = I) when Warnings =/= [] ->
    N = length(Warnings),
    Suffix =
        if
            N =:= 1 -> "";
            N > 1   -> "s"
        end,
    Reason = ?l2b(lists:concat(["Has ", length(Warnings), " warning", Suffix,
                                ". No warnings are accepted."])),
    #result{outcome      = fail,
            latest_cmd   = I#istate.latest_cmd,
            pos_stack    = I#istate.pos_stack,
            shell_name   = I#istate.active_name,
            expected_tag = ?EXPECTED_OLD,
            expected     = success,
            extra        = undefined,
            actual       = Reason,
            rest         = fail,
            warnings     = Warnings}.

print_success(I, File) ->
    LatestCmd = I#istate.latest_cmd,
    FullLineNo = ?i2l(LatestCmd#cmd.lineno),
    RunWarnings = I#istate.warnings,
    UnstableWarnings = [],
    print_warnings(I, RunWarnings, UnstableWarnings),
    Outcome =
        if
            RunWarnings =/= [] ->
                double_ilog(I, "~sWARNING\n", [?TAG("result")]),
                warning;
            true ->
                double_ilog(I, "~sSUCCESS\n", [?TAG("result")]),
                success
        end,
    Results = [],
    {ok, Outcome, File, FullLineNo, no_shell, I#istate.case_log_dir,
     RunWarnings, UnstableWarnings, Results,
     <<>>, [{stopped_by_user, I#istate.stopped_by_user}]}.

print_fail(OldI0, NewI, File, Results,
           #result{outcome      = fail,
                   mode         = _Mode,
                   latest_cmd   = LatestCmd,
                   pos_stack    = PosStack,
                   shell_name   = ShellName,
                   expected_tag = ExpectedTag,
                   expected     = Expected,
                   extra        = _Extra,
                   actual       = Actual,
                   rest         = Rest} = Fail) ->
    OldI = OldI0#istate{progress = silent},
    OldWarnings = NewI#istate.warnings,
    FullLineNo = lux_utils:full_lineno(File, LatestCmd, PosStack),
    HiddenWarnings = [hidden_warning(OldI, File, R) ||
                         R <- Results,
                         R#result.outcome =:= fail,
                         R =/= Fail],
    RunWarnings = OldWarnings ++ HiddenWarnings,
    UnstableWarnings = unstable_warnings(NewI),
    print_warnings(NewI, RunWarnings, UnstableWarnings),
    {Outcome, ResStr} =
        if
            UnstableWarnings =/= [] andalso
            not NewI#istate.fail_when_warning ->
                {warning,
                 double_ilog(OldI, "~sWARNING at line ~s in shell ~s\n",
                             [?TAG("result"), FullLineNo, ShellName])
                };
            true ->
                {fail,
                 double_ilog(OldI, "~sFAIL at line ~s in shell ~s\n",
                             [?TAG("result"), FullLineNo, ShellName])
                }
        end,
    {OldActual, NewActual, NewExpected, NewRest} =
        new_actual(Actual, Expected, Rest),
    FailBin = fail_bin(ExpectedTag, NewExpected, OldActual, NewRest),
    case OldI0#istate.progress of
        silent ->
            ok;
        _ ->
            io:format("~s", [ResStr]),
            io:format("~s\n", [FailBin])
    end,
    ExpectStr = ?a2l(ExpectedTag),
    double_ilog(OldI, "~s\n\"~s\"\n",
                [ExpectStr, lux_utils:to_string(Expected)]),
    double_ilog(OldI, "actual ~s\n\"~s\"\n",
                [NewActual, lux_utils:to_string(NewRest)]),
    Opaque = [{stopped_by_user,NewI#istate.stopped_by_user}],
    NewResults = [Fail],
    {ok, Outcome, File, FullLineNo, ShellName, NewI#istate.case_log_dir,
     RunWarnings, UnstableWarnings, NewResults,
     FailBin, Opaque}.

new_actual(Actual, Expected, Rest) when is_atom(Expected) ->
    NewExpected = ?a2b(Expected),
    new_actual(Actual, NewExpected, Rest);
new_actual(Actual, Expected, Rest) when is_atom(Rest) ->
    NewRest = ?a2b(Rest),
    new_actual(Actual, Expected, NewRest);
new_actual(Actual, Expected, Rest) when is_binary(Expected), is_binary(Rest) ->
    case Actual of
        <<?fail_pattern_matched, _/binary>> ->
            {Actual, Actual, Expected, Rest};
        <<?success_pattern_matched, _/binary>> ->
            {Actual, Actual, Expected, Rest};
        <<?loop_break_mismatch, _/binary>> ->
            {Actual, Actual, Expected, Rest};
        {fail, OldActual} when is_atom(OldActual) ->
            NewActual = ?a2b(OldActual),
            {OldActual, NewActual, Expected, Rest};
        _ when is_atom(Actual) ->
            NewActual = ?a2b(Actual),
            {Actual, NewActual, Expected, Rest};
        _ when is_binary(Actual) ->
            NewActual = <<"error">>,
            {Actual, NewActual, Expected, Actual}
    end.

fail_bin(ExpectedTag, Expected, NewActual, NewRest) ->
    Diff = lux_utils:shrink_diff(ExpectedTag, Expected, NewRest),
    ?l2b(
       [
        ?FF("~s\n\t~s\n", [ExpectedTag, simple_to_string(Expected)]),
        ?FF("actual ~s\n\t~s\n", [NewActual, simple_to_string(NewRest)]),
        ?FF("diff\n\t~s", [simple_to_string(Diff)])
       ]).

hidden_warning(I,
               File,
               #result{outcome      = fail,
                       mode         = _Mode,
                       latest_cmd   = LatestCmd,
                       pos_stack    = PosStack,
                       shell_name   = ShellName,
                       expected_tag = _ExpectedTag,
                       expected     = _Expected,
                       extra        = _Extra,
                       actual       = _Actual,
                       rest         = _Rest}) ->
    FullLineNo = lux_utils:full_lineno(File, LatestCmd, PosStack),
    FailIoList = lists:concat(["FAIL at line ", FullLineNo,
                               " in shell ", ShellName]),
    W = lux_utils:make_warning(File, FullLineNo, FailIoList),
    progress_warnings(I, [W]),
    W.

unstable_warnings(#istate{unstable=U,
                          unstable_unless=UU,
                          latest_cmd = LatestCmd} = I) ->
    FullLineNo = ?i2l(LatestCmd#cmd.lineno),
    F = fun(Var, NameVal) -> filter_unstable(I, FullLineNo, Var, NameVal) end,
    Unstable = lists:zf(fun(Val) -> F("unstable", Val) end, U),
    UnstableUnless = lists:zf(fun(Val) -> F("unstable_unless", Val) end, UU),
    Unstable ++ UnstableUnless.

filter_unstable(#istate{skip_skip = true}, _FullLineNo, _Var, _NameVal) ->
    false;
filter_unstable(#istate{skip_skip = false, orig_file = File} = I,
                FullLineNo, Var, NameVal) ->
    case Var of
        "unstable" ->
            {IsSet, Name, Val} = test_var(I, NameVal),
            case IsSet of
                false ->
                    false;
                true ->
                    Format = "FAIL but UNSTABLE as variable ~s is set",
                    Reason = format_val(Format, [Name], Val),
                    W = lux_utils:make_warning(File, FullLineNo, Reason),
                    progress_warnings(I, [W]),
                    {true, W}
            end;
        "unstable_unless" ->
            {IsSet, Name, Val} = test_var(I, NameVal),
            case IsSet of
                true ->
                    false;
                false ->
                    Format = "FAIL but UNSTABLE as variable ~s is not set",
                    Reason = format_val(Format, [Name], Val),
                    W = lux_utils:make_warning(File, FullLineNo, Reason),
                    progress_warnings(I, [W]),
                    {true, W}
            end
    end.

test_var(#istate{builtin_vars = BuiltinVars,
                 system_vars  = SystemVars},
              VarVal) ->
    MultiVars = [BuiltinVars, SystemVars],
    lux_utils:test_var(MultiVars, VarVal).

format_val(Format, Args, false) ->
    ?FF(Format, Args);
format_val(Format, Args, Val) ->
    ?FF(Format ++ " to ~p", Args ++ [Val]).

post_ilog(#istate{progress = Progress,
                  logs = Logs,
                  config_log_fd = {_, ConfigFd}} = I,
          Docs) ->
    lux_log:close_config_log(ConfigFd, Logs),
    log_doc(I, Docs),
    lux_interpret:raw_ilog(I, "\n", []),
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
        fun(#cmd{type = doc, arg = Arg}, _RevFile, _PosStack, Acc)
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
    simple_to_string(?a2l(Atom));
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
     skip_skip,
     fail_when_warning,
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
     risky_threshold,
     sloppy_threshold,
     newshell,
     shell_wrapper,
     shell_cmd,
     shell_args,
     shell_prompt_cmd,
     shell_prompt_regexp,
     post_cleanup_cmd,
     var
    ].

double_ilog(#istate{progress = Progress, log_fun = LogFun, event_log_fd = Fd},
            Format,
            Args) ->
    Bin = lux_log:safe_format(silent, LogFun, undefined, Format, Args),
    lux_log:safe_write(Progress, LogFun, Fd, Bin).

console_write(String) ->
    io:format("~s", [String]).
