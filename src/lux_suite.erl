%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2015 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_suite).

-export([run/2]).

-include("lux.hrl").
-include_lib("kernel/include/file.hrl").

-define(FF(Format, Args), lists:flatten(io_lib:format(Format, Args))).

-record(rstate,
        {files                      :: [string()],
         mode = execute             :: list | doc | validate | execute | doc,
         skip_skip = false          :: boolean(),
         config_dir                 :: string(),
         file_pattern = "^[^\\\.].*\\\.lux" ++ [$$] :: string(),
         log_fd                     :: file:io_device(),
         log_dir                    :: file:filename(),
         summary_log                :: string(),
         config_name                :: string(),
         config_file                :: string(),
         suite = "unknown"          :: string(),
         start_time                 :: {non_neg_integer(),
                                        non_neg_integer(),
                                        non_neg_integer()},
         run                        :: string(),
         extend_run = false         :: boolean(),
         revision = ""              :: string(),
         html = enable              :: enable | success | skip | warning |
                                       fail | error | disable,
         warnings = []              :: [{warning, string(),
                                         string(), string()}],
         internal_opts = []         :: [{atom(), term()}], % Internal opts
         user_opts = []             :: [{atom(), term()}], % Command line opts
         file_opts = []             :: [{atom(), term()}], % Script opts
         config_opts = []           :: [{atom(), term()}], % Arch spec opts
         default_opts = []          :: [{atom(), term()}], % Default opts
         builtin_dict = lux_utils:builtin_dict()
                                    :: [string()], % ["name=val"]
         system_dict = lux_utils:system_dict()
                                    :: [string()], % ["name=val"]
         tap_opts = []              :: [string()],
         tap                        :: term() % #tap{}
        }).

run(Files, Opts) when is_list(Files) ->
    case parse_ropts(Opts, #rstate{files = Files}) of
        {ok, _R} when Files =:= [] ->
            FileErr = lux_log:safe_format(undefined,
                                          "ERROR: Mandatory script files "
                                          "are missing\n",
                                          []),
            {error, "", FileErr};
        {ok, R} when R#rstate.mode =:= list; R#rstate.mode =:= doc ->
            R2 = R#rstate{log_fd = undefined, summary_log = undefined},
            {_ConfigData, R3} = parse_config(R2),
            {R4, Summary, Results} =
                run_suite(R3, R3#rstate.files, success, []),
            write_results(R4, Summary, Results);
        {ok, R} ->
            SummaryLog = filename:join([R#rstate.log_dir, "lux_summary.log"]),
            case ensure_log_dir(R, SummaryLog) of
                {ok, R2} ->
                    do_run(R2, SummaryLog);
                {error, File, DirErr} ->
                    {error, File, DirErr}
            end;
        {error, {badarg, Name, Val}} ->
            ArgErr =
                lux_log:safe_format(undefined,
                                    "ERROR: ~p is an illegal argument (~p)\n",
                                    [Name, Val]),
            {error, hd(Files), ArgErr};
        {error, File, ArgErr} ->
            {error, File, ArgErr}
    end.

run_suite(R0, SuiteFiles, Summary, Results) ->
    Scripts = expand_suite(R0,  SuiteFiles, []),
    lux:trace_me(80, suite, string:join(SuiteFiles, " "), []),
    {ok, R} = tap_suite_begin(R0, Scripts, ""),
    try
        Res = {NewR, NewSummary, NewResults} =
            run_cases(R, Scripts, Summary, Results, 1),
        lux:trace_me(80, suite, NewSummary, []),
        tap_suite_end(NewR, NewSummary, NewResults),
        Res
    catch Class:Reason ->
            lux:trace_me(80, suite, Class, [Reason]),
            lux_tap:bail_out(R#rstate.tap, "Internal error"),
            erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.

expand_suite(R, [SuiteFile | SuiteFiles], Acc) ->
    case list_files(R, SuiteFile) of
        {ok, CaseFiles} ->
            Expanded = [{SuiteFile, {ok, CF}} || CF <- CaseFiles],
            expand_suite(R, SuiteFiles, [Expanded | Acc]);
        {error, Reason} ->
            expand_suite(R, SuiteFiles, [{SuiteFile, {error, Reason}} | Acc])
    end;
expand_suite(_R, [], Acc) ->
    lists:append(lists:reverse(Acc)).

list_files(R, File) ->
    case file:read_file_info(File) of
        {ok, #file_info{type = directory}} ->
            Fun = fun(F, Acc) -> [F | Acc] end,
            RegExp = R#rstate.file_pattern,
            Files = lux_utils:fold_files(File, RegExp, true, Fun, []),
            {ok, lists:sort(Files)};
        {ok, _} ->
            {ok, [File]};
        {error, Reason} ->
            {error, Reason}
    end.

do_run(R, SummaryLog) ->
    case lux_log:open_summary_log(SummaryLog, R#rstate.extend_run) of
        {ok, Exists, SummaryFd} ->
            TimerRef = start_suite_timer(R),
            try
                R2 = R#rstate{log_fd = SummaryFd, summary_log = SummaryLog},
                {ConfigData, R3} = parse_config(R2),
                HtmlPrio = lux_utils:summary_prio(R2#rstate.html),
                InitialSummary = success,
                SummaryPrio0 = lux_utils:summary_prio(InitialSummary),
                InitialRes =
                    case Exists of
                        true ->
                            TmpLog = SummaryLog ++ ".tmp",
                            case lux_log:parse_summary_log(TmpLog) of
                                {ok, _, Groups, _, _, _} ->
                                    initial_results(Groups);
                                {error, _, _} ->
                                    []
                            end;
                        false ->
                            LogDir = filename:dirname(SummaryLog),
                            ConfigLog = filename:join([LogDir,
                                                       "lux_config.log"]),
                            ok = lux_log:write_config_log(ConfigLog,ConfigData),
                            lux_log:write_results(SummaryLog, skip, [], []),
                            %% Generate initial html log
                            if
                                SummaryPrio0 >= HtmlPrio,
                                R3#rstate.mode =/= list ->
                                    annotate_summary_log(R3, skip, []);
                                true ->
                                    ok
                            end,
                            []
                    end,
                {R5, Summary, Results} =
                    run_suite(R3, R3#rstate.files, InitialSummary, InitialRes),
                print_results(R5, Summary, Results),
                _ = write_results(R5, Summary, Results),
                lux_log:close_summary_log(SummaryFd, SummaryLog),
                SummaryPrio = lux_utils:summary_prio(Summary),
                if
                    SummaryPrio >= HtmlPrio, R5#rstate.mode =/= list ->
                        case lux_html:annotate_log(false, SummaryLog) of
                            ok ->
                                io:format("\nfile://~s\n",
                                          [SummaryLog ++ ".html"]),
                                {ok, Summary, SummaryLog, Results};
                            {error, _File, _ReasonStr} = Error ->
                                Error
                        end;
                    true ->
                        {ok, Summary, SummaryLog, Results}
                end
            catch
                throw:{error, File, ReasonStr} ->
                    lux_log:close_summary_tmp_log(SummaryFd),
                    {error, File, ReasonStr};
                Class:Reason ->
                    lux_log:close_summary_tmp_log(SummaryFd),
                    ReasonStr =
                        lists:flatten(io_lib:format("~p:~p ~p",
                                                    [Class,
                                                     Reason,
                                                     erlang:get_stacktrace()])),
                    {error, SummaryLog, ReasonStr}
            after
                cancel_timer(TimerRef)
            end;
        {error, FileReason} ->
            FileErr =
                lux_log:safe_format(undefined,
                                    "ERROR: Failed to open logfile:"
                                    " ~s -> ~s\n",
                                    [SummaryLog,
                                     file:format_error(FileReason)]),
            {error, SummaryLog, FileErr}
    end.

initial_results(Groups) ->
    Fun =
        fun(Script, {result, Res}) ->
                case Res of
                    success ->
                        {ok, Script, Res, "0", []};
                    skip ->
                        {ok, Script, Res, "0", []};
                    {error_line, RawLineNo, Reason} ->
                        {error, Script, RawLineNo, Reason};
                    {error, [Reason]} ->
                        case binary:split(Reason, <<": ">>, [global]) of
                            [_, <<"Syntax error at line ", N/binary>>, _] ->
                                {error, Script, binary_to_list(N), Reason};
                            _ ->
                                {error, Script, "0", Reason}
                        end;
                  {error, Reason} ->
                        {error, Script, "0", Reason};
                    {fail, _Script, RawLineNo, _Expected, _Actual, _Details} ->
                        {ok, Script, fail, binary_to_list(RawLineNo), []}
                end
        end,
    [Fun(Script, Res) ||
        {test_group, _Group, Cases} <- Groups,
        {test_case, Script, _Log, _Doc, _HtmlLog, Res} <- Cases].

parse_ropts([{Name, Val} = NameVal | T], R) ->
    case Name of
        %% suite options
        file_pattern when is_list(Val) ->
            parse_ropts(T, R#rstate{file_pattern = Val});
        config_dir when is_list(Val) ->
            parse_ropts(T, R#rstate{config_dir = filename:absname(Val)});
        config_name when is_list(Val) ->
            parse_ropts(T, R#rstate{config_name = Val});
        suite when is_list(Val) ->
            parse_ropts(T, R#rstate{suite = Val});
        run when is_list(Val) ->
            parse_ropts(T, R#rstate{run = Val});
        extend_run when Val =:= true; Val =:= false ->
            parse_ropts(T, R#rstate{extend_run = Val});
        revision when is_list(Val) ->
            parse_ropts(T, R#rstate{revision = Val});
        skip_skip when Val =:= true; Val =:= false ->
            parse_ropts(T, R#rstate{skip_skip = Val});
        mode when Val =:= list; Val =:= doc;
                  Val =:= validate; Val =:= execute ->
            parse_ropts(T, R#rstate{mode = Val});
        html when Val =:= enable; Val =:= success;
                  Val =:= skip; Val =:= warning;
                  Val =:= fail; Val =:= error;
                  Val =:= disable ->
            parse_ropts(T, R#rstate{html = Val});

        tap when Val =:= stdout; Val =:= stderr; is_list(Val) ->
            parse_ropts(T, R#rstate{tap_opts = [Val|R#rstate.tap_opts]});

        %% case options
        _ ->
            UserOpts = [NameVal | R#rstate.user_opts],
            parse_ropts(T, R#rstate{user_opts = UserOpts})
    end;
parse_ropts([], R) ->
    {ok, adjust_log_dir(R)}.

adjust_log_dir(R) ->
    Now = erlang:now(),
    UniqStr = uniq_str(Now),
    UniqRun = "run_" ++ UniqStr,
    Run =
        case R#rstate.run of
            undefined -> UniqRun;
            UserRun   -> UserRun
        end,
    UserLogDir = pick_val(log_dir, R, undefined),
    RelLogDir =
        case UserLogDir of
            undefined -> filename:join(["lux_logs", UniqRun]);
            LogDir    -> LogDir
        end,
    AbsLogDir0 = filename:absname(RelLogDir),
    AbsLogDir =
        if
            UserLogDir =:= undefined, R#rstate.extend_run ->
                ParentDir0 = filename:dirname(AbsLogDir0),
                Link0 = filename:join([ParentDir0, "latest_run"]),
                case file:read_link(Link0) of
                    {ok, LinkTo} ->
                        %% Reuse old log dir
                        filename:absname(filename:join([ParentDir0, LinkTo]));
                    {error, _} ->
                        AbsLogDir0
                end;
            true ->
                AbsLogDir0
        end,
    UserOpts = merge_opts([{log_dir, AbsLogDir}], R#rstate.user_opts),
    R#rstate{start_time = Now,
             run = Run,
             log_dir = AbsLogDir,
             user_opts = UserOpts}.

ensure_log_dir(#rstate{log_dir = AbsLogDir, extend_run = ExtendRun} = R,
               SummaryLog) ->
    case opt_ensure_dir(ExtendRun, SummaryLog) of
        ok ->
            ParentDir = filename:dirname(AbsLogDir),
            Link = filename:join([ParentDir, "latest_run"]),
            Base = filename:basename(AbsLogDir),
            _ = file:delete(Link),
            _ = file:make_symlink(Base, Link),

            RelFiles = R#rstate.files,
            AbsFiles = [filename:absname(F) || F <- RelFiles],
            TagFiles = [{config_dir, R#rstate.config_dir} |
                        [{file, F} || F <- RelFiles]],
            try
                lists:foreach(fun check_file/1, TagFiles),
                {ok, R#rstate{files = AbsFiles}}
            catch
                throw:{error, File, Reason} ->
                    {error, File, Reason}
            end;
        summary_log_exists ->
            {error,
             AbsLogDir,
             lux_log:safe_format(undefined,
                                 "ERROR: Summary log file already exists:"
                                 " ~s\n",
                                 [SummaryLog])};
        {error, FileReason} ->
            {error,
             AbsLogDir,
             lux_log:safe_format(undefined,
                                 "ERROR: Failed to create log directory:"
                                 " ~s -> ~s\n",
                                 [AbsLogDir, file:format_error(FileReason)])}
    end.

opt_ensure_dir(ExtendRun, SummaryLog) ->
    case not ExtendRun andalso filelib:is_dir(SummaryLog) of
        true  -> summary_log_exists;
        false -> filelib:ensure_dir(SummaryLog)
    end.

check_file({Tag, File}) ->
    case Tag of
        config_dir when File =:= undefined ->
            ok;
        config_dir ->
            case filelib:is_dir(File) of
                true ->
                    ok;
                false ->
                    BinErr =
                        lux_log:safe_format(undefined,
                                            "ERROR: ~p ~s: ~s \n",
                                            [Tag,
                                             File,
                                             file:format_error(enoent)]),
                    throw({error, File, BinErr})
            end;
        file ->
            case filelib:is_file(File) of
                true ->
                    ok;
                false ->
                    BinErr =
                        lux_log:safe_format(undefined,
                                            "ERROR: ~s: ~s \n",
                                            [File,
                                             file:format_error(enoent)]),
                    throw({error, File, BinErr})
            end
    end.

uniq_str(Now) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:now_to_datetime(Now),
    lists:concat([Year, "_", r2(Month), "_", r2(Day), "_",
                  r2(Hour), "_", r2(Min), "_", r2(Sec)]).

r2(Int) when is_integer(Int) ->
    r2(integer_to_list(Int));
r2(String) when is_list(String) ->
    string:right(String, 2, $0).

run_cases(R, [{_SuiteFile,{error,_Reason}}|Scripts], OldSummary, Results, CC)
  when R#rstate.mode =:= list ->
    run_cases(R, Scripts, OldSummary, Results, CC+1);
run_cases(R, [{SuiteFile, {error,Reason}}|Scripts], OldSummary, Results, CC) ->
    double_rlog(R, "\n~s~s\n", [?TAG("test case"), SuiteFile]),
    ListErr =
        double_rlog(R, "~s~s: ~s\n",
                    [?TAG("error"), SuiteFile, file:format_error(Reason)]),
    Results2 = [{error, SuiteFile, ListErr} | Results],
    lux:trace_me(70, suite, 'case', SuiteFile, []),
    tap_case_begin(R, SuiteFile),
    lux:trace_me(70, 'case', suite, error, [Reason]),
    tap_case_end(R, CC, SuiteFile, error, "0", Reason),
    run_cases(R, Scripts, OldSummary, Results2, CC+1);
run_cases(R, [{SuiteFile,{ok,Script}} | Scripts], OldSummary, Results, CC) ->
    RelScript = lux_utils:drop_prefix(Script),
    Mode = R#rstate.mode,
    case parse_script(R#rstate{warnings = []}, SuiteFile, Script) of
        {ok, R2, Script2, Commands, Opts} ->
            SkipNames0 = list_matching_variables(R2, skip, false),
            SkipUnlessNames0 = list_matching_variables(R2, skip_unless, true),
            {SkipNames, SkipUnlessNames} =
                case R2#rstate.skip_skip of
                    true  -> {[], []};
                    false -> {SkipNames0, SkipUnlessNames0}
                end,
            SkipReason0 =
                case {SkipNames0, SkipUnlessNames0} of
                    {[SkipName0 | _], _} ->
                        ?FF("SKIP as variable ~s is set",
                            [SkipName0]);
                    {_, [SkipUnlessName0 | _]} ->
                        ?FF("SKIP as variable ~s is not set",
                            [SkipUnlessName0]);
                    _ ->
                        ""
                end,
            RequireNames =
                case Mode of
                    doc -> [];
                    _   -> list_matching_variables(R2, require, true)
                end,
            NewWarnings = R2#rstate.warnings,
            AllWarnings = R#rstate.warnings ++ NewWarnings,
            case Mode of
                list when SkipNames =/= []; SkipUnlessNames =/= [] ->
                    run_cases(R, Scripts, OldSummary, Results, CC+1);
                list ->
                    io:format("~s\n", [Script]),
                    run_cases(R, Scripts, OldSummary, Results, CC+1);
                _ when SkipNames =/= []; SkipUnlessNames =/= [] ->
                    lux:trace_me(70, suite, 'case', RelScript, []),
                    double_rlog(R2, "\n~s~s\n",
                                [?TAG("test case"), Script]),
                    double_rlog(R2, "~s~s\n",
                                [?TAG("result"), SkipReason0]),
                    Summary = skip,
                    lux:trace_me(70, 'case', suite, Summary,
                                 [SkipNames, SkipUnlessNames]),
                    tap_case_end(R, CC, Script, Summary,
                                 "0", SkipReason0),
                    NewSummary = lux_utils:summary(OldSummary, Summary),
                    Res = {ok, Script2, Summary, "0", []},
                    Results2 = [Res | Results],
                    run_cases(R#rstate{warnings = AllWarnings},
                              Scripts, NewSummary, Results2, CC+1);
                _ when RequireNames =/= [] ->
                    lux:trace_me(70, suite, 'case', RelScript, []),
                    double_rlog(R2, "\n~s~s\n",
                                [?TAG("test case"), Script]),
                    FailReason = ?FF("FAIL as required variable ~s is not set",
                                     [hd(RequireNames)]),
                    double_rlog(R2,
                                "~s~s\n",
                                [?TAG("result"), FailReason]),
                    Summary = fail,
                    lux:trace_me(70, 'case', suite, Summary, [RequireNames]),
                    tap_case_end(R, CC, Script, Summary,
                                 "0", FailReason),
                    NewSummary = lux_utils:summary(OldSummary, Summary),
                    Res = {ok, Script2, Summary, "0", []},
                    Results2 = [Res | Results],
                    run_cases(R#rstate{warnings = AllWarnings},
                              Scripts, NewSummary, Results2, CC+1);
                doc ->
                    Docs = extract_doc(Script2, Commands),
                    io:format("~s:\n",
                              [lux_utils:drop_prefix(Script)]),
                    [io:format("~s~s\n", [lists:duplicate(Level, $\t), Str]) ||
                        #cmd{arg = {Level, Str}} <- Docs],
                    case NewWarnings of
                        [] ->
                            NewSummary = OldSummary,
                            Results2 = Results;
                        _ ->
                            Summary = warning,
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            Results2 = [{Summary, Script2, NewWarnings} |
                                        Results]
                    end,
                    run_cases(R#rstate{warnings = AllWarnings},
                              Scripts, NewSummary, Results2, CC+1);
                validate ->
                    double_rlog(R2, "\n~s~s\n",
                                [?TAG("test case"), Script]),
                    case NewWarnings of
                        [] ->
                            Summary = success,
                            NewSummary = OldSummary,
                            Results2 = Results;
                        _ ->
                            Summary = warning,
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            Results2 = [{NewSummary, Script2, NewWarnings} |
                                        Results]
                    end,
                    double_rlog(R2, "~s~s\n",
                                [?TAG("result"),
                                 string:to_upper(atom_to_list(Summary))]),
                    run_cases(R#rstate{warnings = AllWarnings},
                              Scripts, NewSummary, Results2, CC+1);
                execute ->
                    lux:trace_me(70, suite, 'case', RelScript, []),
                    double_rlog(R2, "\n~s~s\n",
                                [?TAG("test case"), Script]),
                    Res = lux:interpret_commands(Script2, Commands, Opts),
                    case Res of
                        {ok, _, CaseLogDir, Summary, FullLineNo, Events} ->
                            lux:trace_me(70, 'case', suite, Summary,
                                  []),
                            tap_case_end(R2, CC, Script, Summary,
                                         FullLineNo, SkipReason0),
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            Res2 = {ok, Script, Summary, FullLineNo, Events},
                            NewResults = [Res2 | Results],
                            NewScripts = Scripts;
                        {error, _, CaseLogDir, FullLineNo, ErrorMsg}
                          when ErrorMsg =:= <<"suite_timeout" >> ->
                            Summary = error,
                            lux:trace_me(70, 'case', suite, Summary,
                                         [FullLineNo]),
                            tap_case_end(R2, CC, Script, Summary,
                                         FullLineNo, SkipReason0),
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            NewResults = [Res | Results],
                            NewScripts = [];
                        {error, _, CaseLogDir, FullLineNo, _} ->
                            Summary = error,
                            lux:trace_me(70, 'case', suite, Summary,
                                         [FullLineNo]),
                            tap_case_end(R2, CC, Script, Summary,
                                         FullLineNo, SkipReason0),
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            NewResults = [Res | Results],
                            NewScripts = Scripts
                    end,
                    HtmlPrio = lux_utils:summary_prio(R2#rstate.html),
                    SummaryPrio = lux_utils:summary_prio(NewSummary),
                    R3 = R2#rstate{warnings = AllWarnings},
                    if
                        SummaryPrio >= HtmlPrio ->
                            Base = filename:basename(Script),
                            EventLog = filename:join([CaseLogDir,
                                                      Base ++ ".event.log"]),
                            lux_html:annotate_log(false, EventLog),
                            annotate_summary_log(R3, NewSummary, NewResults);
                        true ->
                            ignore
                    end,
                    run_cases(R3, NewScripts, NewSummary, NewResults, CC+1)
            end;
        {error, _R2, _ErrorStack, _ErrorBin} when Mode =:= list ->
            io:format("~s\n", [Script]),
            run_cases(R, Scripts, OldSummary, Results, CC+1);
        {error, _R2, ErrorStack, ErrorBin} when Mode =:= doc ->
            {MainFile, _FullLineNo, ErrorBin2} =
                parse_error(ErrorStack, ErrorBin),
            io:format("~s:\n\tERROR: ~s: ~s\n",
                      [Script, MainFile, ErrorBin2]),
            run_cases(R, Scripts, OldSummary, Results, CC+1);
        {error, R2, ErrorStack, ErrorBin} ->
            {MainFile, FullLineNo, ErrorBin2} =
                parse_error(ErrorStack, ErrorBin),
            double_rlog(R2, "\n~s~s\n",
                        [?TAG("test case"), Script]),
            double_rlog(R2, "~sERROR ~s: ~s\n",
                        [?TAG("result"), MainFile, ErrorBin2]),
            NewWarnings = R2#rstate.warnings,
            AllWarnings = R#rstate.warnings ++ NewWarnings,
            Summary = error,
            NewSummary = lux_utils:summary(OldSummary, Summary),
            Results2 = [{error, MainFile, FullLineNo, ErrorBin2} | Results],
            run_cases(R#rstate{warnings = AllWarnings},
                      Scripts, NewSummary, Results2, CC+1)
    end;
run_cases(R, [], Summary, Results, _CC) ->
    {R, Summary, lists:reverse(Results)}.

annotate_summary_log(R, NewSummary, NewResults) ->
    file:sync(R#rstate.log_fd), % Flush summary log
    _ = write_results(R, NewSummary, NewResults),
    SummaryLog = R#rstate.summary_log,
    TmpLog = SummaryLog ++ ".tmp",
    lux_html:annotate_log(false, TmpLog),
    file:rename(TmpLog++".html",SummaryLog++".html").

list_matching_variables(R, Tag, DoNegate) ->
    Fun =
        fun(NameVal) ->
                Bool = test_variable(R, NameVal),
                case DoNegate of
                    true  -> not Bool;
                    false -> Bool
                end
        end,
    NameVals = pick_vals(Tag, R),
    lists:filter(Fun, NameVals).

test_variable(R, NameVal) ->
    Pred = fun(Char) -> Char =/= $= end,
    {Name, OptVal} = lists:splitwith(Pred, NameVal),
    UnExpanded = [$$ | Name],
    try
        Expanded = expand_vars(R, UnExpanded, error),
        case OptVal of
            [$= | Val] when Val =:= Expanded ->
                %% Test on value. Possible empty.
                true;
            [] when Expanded =/= UnExpanded ->
                %% Test on existence
                true;
            _ ->
                %% No match
                false
        end
    catch
        throw:{no_such_var, _} ->
            false
    end.


extract_doc(File, Cmds) ->
    Fun = fun(Cmd, _RevFile, _CmdStack, Acc) ->
                  case Cmd of
                      #cmd{type = doc} ->
                          [Cmd | Acc];
                      _ ->
                          Acc
                  end
          end,
    lists:reverse(lux_utils:foldl_cmds(Fun, [], File, [], Cmds)).

write_results(#rstate{mode=Mode, summary_log=SummaryLog},
              Summary, Results)
  when Mode =:= list; Mode =:= doc ->
    {ok, Summary, SummaryLog, Results};
write_results(#rstate{summary_log=SummaryLog, warnings=Warnings},
              Summary, Results) when is_list(SummaryLog) ->
    lux_log:write_results(SummaryLog, Summary, Results, Warnings),
    {ok, Summary, SummaryLog, Results}.

print_results(#rstate{warnings=Warnings}, Summary, Results) ->
    lux_log:print_results({false,standard_io}, Summary, Results, Warnings).

parse_script(R, SuiteFile, Script) ->
    case lux:parse_file(Script, []) of
        {ok, Script2, Commands, FileOpts} ->
            FileOpts2 = merge_opts(FileOpts, R#rstate.file_opts),
            R2 = R#rstate{internal_opts=[],
                          file_opts = FileOpts2},
            LogDir = log_dir(R2, SuiteFile, Script2),
            LogFd = R#rstate.log_fd,
            LogFun = fun(Bin) -> lux_log:safe_write(LogFd, Bin) end,
            InternalOpts = [{log_dir, LogDir},
                            {log_fun, LogFun},
                            {log_fd,  LogFd}],
            UserOpts = R2#rstate.user_opts,
            Opts = lists:foldl(fun(New, Acc) -> merge_opts(New, Acc) end,
                               [],
                               [
                                R2#rstate.default_opts,
                                R2#rstate.config_opts,
                                FileOpts2,
                                UserOpts,
                                InternalOpts
                               ]),
            R3 = R2#rstate{user_opts = UserOpts,
                           file_opts = FileOpts2,
                           internal_opts = InternalOpts},
            {ok, R3, Script2, Commands, Opts};
        {error, ErrorStack, ErrorBin} ->
            {error, R, ErrorStack, ErrorBin}
    end.

parse_config(R) ->
    %% Default opts
    DefaultBase = "luxcfg",
    DefaultDir = filename:absname(code:lib_dir(?APPLICATION, priv)),
    DefaultFile = filename:join([DefaultDir, DefaultBase]),
    DefaultOpts = parse_config_file(R, DefaultFile),
    R2 = R#rstate{default_opts = DefaultOpts},

    %% Config dir
    case pick_val(config_dir, R2, R#rstate.config_dir) of
        undefined -> ConfigDir = code:lib_dir(?APPLICATION, priv);
        ConfigDir -> ok
    end,
    check_file({config_dir, ConfigDir}),

    %% Arch spec opts
    ActualConfigName = config_name(),
    {ConfigName, ConfigFile} =
        config_file(ConfigDir, R2#rstate.config_name, ActualConfigName),
    ConfigOpts = parse_config_file(R2, ConfigFile),
    R3 = R2#rstate{config_name = ConfigName,
                   config_dir = ConfigDir,
                   config_file = ConfigFile,
                   config_opts = ConfigOpts},
    ConfigData =
        builtins(R, ActualConfigName) ++
        [{'default file', [string], DefaultFile}] ++ DefaultOpts ++
        [{'config file', [string], ConfigFile}] ++ ConfigOpts,
    {ConfigData, R3}.

builtins(R, ActualConfigName) ->
    {ok, Cwd} = file:get_cwd(),
    [
     {'start time', [string], lux_utils:now_to_string(R#rstate.start_time)},
     {hostname, [string], hostname()},
     {architecture, [string], ActualConfigName},
     {'system info', [string], sys_info()},
     {suite, [string], R#rstate.suite},
     {run, [string], R#rstate.run},
     {revision, [string], R#rstate.revision},
     {workdir, [string], Cwd},
     {'config name', [string], R#rstate.config_name},
     {config_dir, [string], R#rstate.config_dir}
    ].

config_name() ->
    case string:tokens(os:cmd("uname -sm; echo $?"), "\n ") of
        [Kernel, Machine, CmdStatus] when CmdStatus =:= "0" ->
            Kernel ++ "-" ++ Machine;
        _Bad ->
            erlang:system_info(system_architecture)
    end.

parse_config_file(R, ConfigFile) ->
    Key = config_dir,
    case lux:parse_file(ConfigFile, []) of
        {ok, _File, _Commands, Opts} ->
            Opts2 =
                case lists:keyfind(Key, 1, Opts) of
                    false ->
                        Opts;
                    {_, Dir} ->
                        Top = filename:dirname(ConfigFile),
                        Dir2 = filename:absname(Dir, Top),
                        lists:keystore(Key, 1, Opts, {Key, Dir2})
                end,
            lists:keydelete(log_dir, 1, Opts2);
        {error, ErrorStack, ErrorBin} ->
            Enoent = list_to_binary(file:format_error(enoent)),
            if
                ErrorBin =:= Enoent ->
                    ok;
                true ->
                    {MainFile, FullLineNo, ErrorBin2} =
                        parse_error(ErrorStack, ErrorBin),
                    double_rlog(R, "~s~s: ~s: ~s\n",
                                [?TAG("error"), MainFile,
                                 ErrorBin2, FullLineNo])
            end,
            []
    end.

parse_error(ErrorStack, ErrorBin) ->
    {MainFile, _, _} = hd(ErrorStack),
    {ErrorFile, _, _} = lists:last(ErrorStack),
    FullLineNo = lux_utils:pretty_full_lineno(ErrorStack),
    if
        ErrorFile =:= MainFile ->
            {MainFile, FullLineNo, ErrorBin};
        true ->
            FileBin = list_to_binary(ErrorFile),
            ErrorBin2 = <<FileBin/binary, ": ", ErrorBin/binary>>,
            {MainFile, FullLineNo, ErrorBin2}
    end.

config_file(ConfigDir, UserConfigName, ActualConfigName) ->
    Ext = ".luxcfg",
    case UserConfigName of
        undefined ->
            Host = hostname(),
            HostFile = filename:join([ConfigDir, Host ++ Ext]),
            case filelib:is_regular(HostFile) of
                true  -> {Host, HostFile};
                false -> config_file2(ConfigDir, ActualConfigName, Ext)
            end;
        _ ->
            config_file2(ConfigDir, UserConfigName, Ext)
    end.

config_file2(ConfigDir, ConfigName, Ext) ->
    File = filename:join([ConfigDir, ConfigName ++ Ext]),
    case filelib:is_regular(File) of
        true ->
            {ConfigName, File};
        false ->
            DefaultFile = filename:join([ConfigDir, "luxcfg"]),
            {ConfigName, DefaultFile}
    end.

sys_info() ->
    lux_utils:chop_newline(os:cmd("uname -a")).

hostname() ->
    case inet:gethostname() of
        {ok, Host} -> Host;
        _          -> "localhost"
    end.

log_dir(R, SuiteFile, Script) ->
    LogDir = pick_val(log_dir, R, undefined),
    case Script =:= SuiteFile of
        true ->
            LogDir;
        false ->
            Suffix = lux_utils:drop_prefix(SuiteFile, Script),
            case filename:dirname(Suffix) of
                "."    -> LogDir;
                SubDir -> filename:join([LogDir, SubDir])
            end
    end.

double_rlog(#rstate{log_fd = Fd}, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    case Fd of
        undefined -> list_to_binary(IoList);
        _         -> lux_log:double_write(Fd, IoList)
    end.

start_suite_timer(R) ->
    SuiteTimeout = pick_val(suite_timeout, R, infinity),
    Msg = {suite_timeout, SuiteTimeout},
    Multiplier = pick_val(multiplier, R, 1000),
    case lux_utils:multiply(SuiteTimeout, Multiplier) of
        infinity   -> {infinity, Msg};
        NewTimeout -> {erlang:send_after(NewTimeout, self(), Msg), Msg}
    end.

cancel_timer({Ref, Msg}) ->
    case Ref of
        infinity ->
            ok;
        _ ->
            case erlang:cancel_timer(Ref) of
                false ->
                    receive
                        Msg -> ok
                    after 0 ->
                            ok
                    end;
                TimeLeft when is_integer(TimeLeft) ->
                    ok
            end
    end.

expand_vars(R, String, MissingVar) ->
    Dict = pick_vals(var, R),
    Dicts = [Dict, R#rstate.builtin_dict, R#rstate.system_dict],
    lux_utils:expand_vars(Dicts, String, MissingVar).

pick_val(Tag, R, Default) ->
    Dicts = opts_dicts(R),
    multi_key_find(Tag, 1, Dicts, Default).

multi_key_find(Tag, Pos, [Dict|Dicts], Default) ->
    case lists:keyfind(Tag, 1, Dict) of
        false ->
            multi_key_find(Tag, Pos, Dicts, Default);
        {_, Val} ->
            Val
    end;
multi_key_find(_Tag, _Pos, [], Default) ->
    Default.

pick_vals(Tag, R) ->
    Fun = fun(Dict) ->
                  case lists:keyfind(Tag, 1, Dict) of
                      false     -> [];
                      {_, Vals} -> Vals
                  end
          end,
    Dicts = opts_dicts(R),
    lists:flatmap(Fun, Dicts).

opts_dicts(#rstate{internal_opts = I,
                   user_opts = U,
                   file_opts = F,
                   config_opts = C,
                   default_opts = D}) ->
    [I, U, F, C, D].

merge_opts(KeyVals, Acc) ->
    merge_opts(KeyVals, Acc, []).

merge_opts([{Key, Val} | KeyVals] = AllKeyVals, Acc, Updated) ->
    case lists:keyfind(Key, 1, Acc) of
        false -> OldVal = [];
        {_, OldVal} -> ok
    end,
    Updated2 = [Key | Updated],
    case merge_oper(Key, Updated) of
        reset ->
            %% Multi - Clear old settings in order to override unwanted defaults
            Stripped = [{K,V} || {K,V} <- Acc, K =/= Key],
            merge_opts(AllKeyVals, Stripped, Updated2);
        append ->
            %% Multi - Append new val
            Val2 = OldVal ++ Val,
            Acc2 = lists:keystore(Key, 1, Acc, {Key, Val2}),
            merge_opts(KeyVals, Acc2, Updated2);
        env ->
            %% Multi - handle settings with KEY=VAL syntax
            Val2 = merge_env_opt(Val, OldVal),
            Acc2 = lists:keystore(Key, 1, Acc, {Key, Val2}),
            merge_opts(KeyVals, Acc2, Updated2);
        replace ->
            %% Single - replace old val
            Acc2 = lists:keystore(Key, 1, Acc, {Key, Val}),
            merge_opts(KeyVals, Acc2, Updated2)
    end;
merge_opts([], Acc, _) ->
    Acc.

merge_oper(Key, Updated) ->
    case lux_interpret:config_type(Key) of
        {ok, _Pos, [{env_list, _}]} ->
            env;
        {ok, _Pos, [{reset_list, _}]} ->
            case lists:member(Key, Updated) of
                true  -> append;
                false -> reset
            end;
        {ok, _Pos, _} ->
            replace
    end.

merge_env_opt(Val, OldVal) ->
    New = [list_to_tuple(string:tokens(V, "=")) || V <- lists:reverse(Val)],
    Old = [list_to_tuple(string:tokens(V, "=")) || V <- OldVal],
    Insert = fun(Tuple, Acc) ->
                     SubKey = element(1, Tuple),
                     lists:keystore(SubKey, 1, Acc, Tuple)
             end,
    Merged = lists:foldl(Insert, Old, New),
    [string:join(tuple_to_list(T), "=") || T <- lists:keysort(1, Merged)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tap_suite_begin(R, Scripts, Directive)
  when R#rstate.mode =/= list, R#rstate.mode =/= doc ->
    TapLog = filename:join([R#rstate.log_dir, "lux.tap"]),
    TapOpts = [TapLog | R#rstate.tap_opts],
    case lux_tap:open(TapOpts) of
        {ok, TAP} ->
            ok = lux_tap:plan(TAP, length(Scripts), Directive),
            ok = lux_tap:diag(TAP, "\n"),
            %% ok = lux_tap:diag(TAP, "LUX - LUcid eXpect scripting"),
            Host = hostname(),
            ok = lux_tap:diag(TAP, "ssh " ++ Host),
            {ok, Cwd} = file:get_cwd(),
            ok = lux_tap:diag(TAP, "cd " ++ Cwd),
            RelFiles = [lux_utils:drop_prefix(F) || F <- R#rstate.files],
            ok = lux_tap:diag(TAP, "lux " ++ string:join(RelFiles, " ")),
            SummaryLog = lux_utils:drop_prefix(R#rstate.summary_log),
            ok = lux_tap:diag(TAP, "$BROWSER " ++ SummaryLog ++ ".html"),
            ok = lux_tap:diag(TAP, "\n"),
            {ok, R#rstate{tap = TAP, tap_opts = TapOpts}};
        {error, Reason} ->
            {error, Reason}
    end;
tap_suite_begin(R, _Scripts, _Directive) ->
    {ok, R#rstate{tap = undefined}}.

tap_suite_end(#rstate{tap = TAP, warnings = Warnings}, Summary, Results)
  when TAP =/= undefined ->
    Len = fun(Res, Tag) ->
                  integer_to_list(length(lux_log:pick_result(Res, Tag)))
          end,
    ok = lux_tap:diag(TAP, "\n"),
    lux_tap:diag(TAP, ["Errors:     ", Len(Results, error)]),
    lux_tap:diag(TAP, ["Failed:     ", Len(Results, fail)]),
    lux_tap:diag(TAP, ["Warnings:   ", Len(Warnings, warning)]),
    lux_tap:diag(TAP, ["Skipped:    ", Len(Results, skip)]),
    lux_tap:diag(TAP, ["Successful: ", Len(Results, success)]),
    lux_tap:diag(TAP, ["Summary:    ", atom_to_list(Summary)]),
    lux_tap:close(TAP);
tap_suite_end(_R, _Summary, _Results) ->
    ok.

tap_case_begin(#rstate{tap = TAP}, Script)
  when TAP =/= undefined ->
    ok = lux_tap:diag(TAP, "lux " ++ Script);
tap_case_begin(_R, _Script) ->
    ok.

tap_case_end(#rstate{tap = TAP, skip_skip = SkipSkip},
             CaseCount, AbsScript, Result, FullLineNo, Reason)
  when TAP =/= undefined ->
    RelScript = lux_utils:drop_prefix(AbsScript),
    Descr0 = lists:concat([CaseCount, " ", RelScript]),
    LineDescr = Descr0 ++ ":" ++ FullLineNo,
    TodoReason =
        case Reason of
            "" -> "";
            _  -> "TODO - " ++ Reason
        end,
    {Outcome, Directive, Descr} =
        case Result of
            error                 -> {not_ok, "",         LineDescr};
            fail when SkipSkip    -> {not_ok, TodoReason, LineDescr};
            fail                  -> {not_ok, "",         LineDescr};
            skip                  -> {ok,     Reason,     "    " ++ Descr0};
            success when SkipSkip -> {ok,     TodoReason, "    " ++ Descr0};
            success               -> {ok,     "",         "    " ++ Descr0}
        end,
    lux_tap:test(TAP, Outcome, Descr, Directive);
tap_case_end(_R, _CaseCount, _Script, _Result, _FullLineNo, _Reason) ->
    ok.
