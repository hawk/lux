%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_suite).

-export([run/3, args_to_opts/3, annotate_log/3]).

-include("lux.hrl").
-include_lib("kernel/include/file.hrl").

-define(FF(Format, Args), lists:flatten(io_lib:format(Format, Args))).

-record(rstate,
        {files                      :: [string()],
         orig_files                 :: [string()],
         orig_args                  :: [string()],
         mode = execute             :: lux:run_mode(),
         skip_skip = false          :: boolean(),
         progress = brief           :: silent | summary | brief |
                                       doc | compact | verbose,
         config_dir                 :: string(),
         file_pattern = "^[^\\\.].*\\\.lux" ++ [$$] :: string(),
         case_prefix = ""           :: string(),
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
         hostname = real_hostname() :: string(),
         rerun = disable            :: enable | success | skip | warning |
                                       fail | error | disable,
         html = enable              :: validate |
                                       enable | success | skip | warning |
                                       fail | error | disable,
         warnings = []              :: [{warning,string(),string(),string()}],
         internal_args = []         :: [{atom(), term()}], % Internal opts
         user_args = []             :: [{atom(), term()}], % Command line opts
         file_args = []             :: [{atom(), term()}], % Script opts
         config_args = []           :: [{atom(), term()}], % Arch spec opts
         default_args = []          :: [{atom(), term()}], % Default opts
         builtin_vars = lux_utils:builtin_vars()
                                    :: [string()], % ["name=val"]
         system_vars = lux_utils:system_vars()
                                    :: [string()], % ["name=val"]
         tap_opts = []              :: [string()],
         tap                        :: term(), % #tap{}
         junit = false              :: boolean()
        }).

run(Files, Opts, OrigArgs) when is_list(Files) ->
    SuiteStartTime = lux_utils:timestamp(),
    R0 = #rstate{files = Files,
                 orig_files = Files,
                 orig_args = OrigArgs,
                 start_time = SuiteStartTime},
    case parse_ropts(Opts, R0) of
        {ok, R, _UserLogDir}
          when R#rstate.mode =:= list;
               R#rstate.mode =:= list_dir;
               R#rstate.mode =:= doc ->
            LogDir = R#rstate.log_dir,
            LogBase = "lux_summary.log",
            try
                R2 = compute_files(R, LogDir, LogBase),
                doc_run(R2)
            catch
                throw:{error, FileErr, Reason} ->
                    {error, FileErr, Reason};
                Class:Reason ->
                    EST = erlang:get_stacktrace(),
                    ReasonStr =
                        lists:flatten(io_lib:format("~p:~p ~p",
                                                    [Class, Reason, EST])),
                    {ok, Cwd} = file:get_cwd(),
                    {error, Cwd, ReasonStr}
            end;
        {ok, R, UserLogDir} ->
            TimerRef = start_suite_timer(R),
            LogDir = R#rstate.log_dir,
            LogBase = "lux_summary.log",
            SummaryLog = filename:join([LogDir, LogBase]),
            try
                {ConfigData, R2} = parse_config(R), % May throw error
                R3 = compute_files(R2, LogDir, LogBase),
                R4 = ensure_log_dir(R3, SummaryLog, UserLogDir),
                full_run(R4, ConfigData, SummaryLog)
            catch
                throw:{error, undefined, no_input_files} ->
                    {ok, Cwd} = file:get_cwd(),
                    {error, Cwd, "ERROR: No input files\n"};
                throw:{error, FileErr, ReasonStr} ->
                    {error, FileErr, ReasonStr};
                Class:Reason ->
                    EST = erlang:get_stacktrace(),
                    ReasonStr =
                        lists:flatten(io_lib:format("~p:~p ~p",
                                                    [Class, Reason, EST])),
                    {error, SummaryLog, ReasonStr}
            after
                cancel_timer(TimerRef)
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

doc_run(R) ->
    R2 = R#rstate{log_fd = undefined, summary_log = undefined},
    {_ConfigData, R3} = parse_config(R2),  % May throw error
    {R4, Summary, Results} = run_suite(R3, R3#rstate.files, success, []),
    write_results(R4, Summary, Results).

run_suite(R0, SuiteFiles, OldSummary, Results) ->
    {Scripts, Max} = expand_suite(R0,  SuiteFiles, [], 0),
    lux:trace_me(80, suite, string:join(SuiteFiles, " "), []),
    {ok, R} = tap_suite_begin(R0, Scripts, ""),
    try
        {NewR, Summary, NewResults} =
            run_cases(R, Scripts, OldSummary, Results, Max, 1, [], []),
        NewSummary =
            if
                NewResults =:= [], R#rstate.mode =/= validate ->
                    lux_utils:summary(Summary, warning);
                true ->
                    Summary
            end,
        lux:trace_me(80, suite, NewSummary, []),
        tap_suite_end(NewR, NewSummary, NewResults),
        {NewR, NewSummary, NewResults}
    catch Class:Reason ->
            lux:trace_me(80, suite, Class, [Reason]),
            if
                R#rstate.tap =/= undefined ->
                    lux_tap:bail_out(R#rstate.tap, "Internal error");
                true ->
                    ok
            end,
            erlang:Class(Reason)
    end.

expand_suite(R, [SuiteFile | SuiteFiles], Acc, Max) ->
    case list_files(R, SuiteFile) of
        {ok, CaseFiles} ->
            Expand = fun(CF, M) ->
                             P = prefixed_rel_script(R, CF),
                             Len = length(P),
                             NewM = lists:max([M, Len]),
                             {{SuiteFile, {ok, CF}, P, Len}, NewM}
                     end,
            {Expanded, NewMax} = lists:mapfoldl(Expand, Max, CaseFiles),
            expand_suite(R, SuiteFiles, [Expanded | Acc], NewMax);
        {error, _Reason} = Error ->
            P = prefixed_rel_script(R, SuiteFile),
            Len = length(P),
            NewMax = lists:max([Max, Len]),
            Expanded = [{SuiteFile, Error, P, Len}],
            expand_suite(R, SuiteFiles, [Expanded | Acc], NewMax)
    end;
expand_suite(_R, [], Acc, Max) ->
    {lists:append(lists:reverse(Acc)), Max}.

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

full_run(#rstate{progress = Progress} = R, ConfigData, SummaryLog) ->
    ExtendRun = R#rstate.extend_run,
    case lux_log:open_summary_log(Progress, SummaryLog, ExtendRun) of
        {ok, Exists, SummaryFd} ->
            R2 = R#rstate{log_fd = SummaryFd, summary_log = SummaryLog},
            HtmlPrio = lux_utils:summary_prio(R2#rstate.html),
            InitialSummary = success,
            InitialRes =
                initial_res(R2, Exists, ConfigData,
                            SummaryLog, InitialSummary),
            {R3, Summary, Results} =
                run_suite(R2, R2#rstate.files, InitialSummary, InitialRes),
            print_results(R3, Summary, Results),
            _ = write_results(R3, Summary, Results),
            SuiteEndTime = lux_utils:now_to_string(lux_utils:timestamp()),
            EndConfig = [{'end time', [string], SuiteEndTime}],
            write_config_log(SummaryLog, ConfigData ++ EndConfig),
            lux_log:close_summary_log(SummaryFd, SummaryLog),
            maybe_write_junit_report(R3, SummaryLog, ConfigData),
            annotate_final_summary_log(R3, Summary, HtmlPrio,
                                       SummaryLog, Results);
        {error, FileReason} ->
            FileErr =
                lux_log:safe_format(undefined,
                                    "ERROR: Failed to open logfile:"
                                    " ~s -> ~s\n",
                                    [SummaryLog,
                                     file:format_error(FileReason)]),
            {error, SummaryLog, FileErr}
    end.

maybe_write_junit_report(#rstate{junit = false}, _, _) ->
    ok;
maybe_write_junit_report(#rstate{junit = true}, SummaryLog, ConfigData) ->
    {run_dir, _, RunDir} = lists:keyfind(run_dir, 1, ConfigData),
    ok = lux_junit:write_report(SummaryLog, RunDir, []).

initial_res(_R, Exists, _ConfigData, SummaryLog, _Summary)
  when Exists =:= true ->
    TmpLog = SummaryLog ++ ".tmp",
    WWW = undefined,
    {Res, WWW} = lux_log:parse_summary_log(TmpLog, WWW), % assert
    NewRes =
        case Res of
            {ok, _, Groups, _, _, _} ->
                flatten_results(Groups);
            {error, _, _} ->
                []
        end,
    NewRes;
initial_res(R, Exists, ConfigData, SummaryLog, Summary)
  when Exists =:= false ->
    write_config_log(SummaryLog, ConfigData),
    lux_log:write_results(R#rstate.progress, SummaryLog, skip, [], []),
    annotate_tmp_summary_log(R, Summary, undefined),
    [].

write_config_log(SummaryLog, ConfigData) ->
    LogDir = filename:dirname(SummaryLog),
    ConfigLog = filename:join([LogDir, "lux_config.log"]),
    ok = lux_log:write_config_log(ConfigLog, ConfigData).

annotate_log(IsRecursive, LogFile, Opts) ->
    SuiteLogDir = filename:dirname(LogFile),
    annotate_log(IsRecursive, LogFile, SuiteLogDir, Opts).

annotate_log(IsRecursive, LogFile, SuiteLogDir, Opts) ->
    case lux_html_annotate:generate(IsRecursive, LogFile, SuiteLogDir, Opts) of
        {ok, HtmlFile} ->
            lux_html_parse:validate_html(HtmlFile, Opts);
        {error, File, Reason} ->
            {error, File, Reason}
    end.

annotate_event_log(R, Script, NewSummary, CaseLogDir, Opts) ->
    HtmlPrio = lux_utils:summary_prio(R#rstate.html),
    SummaryPrio = lux_utils:summary_prio(NewSummary),
    if
        SummaryPrio >= HtmlPrio ->
            Base = filename:basename(Script),
            EventLog = filename:join([CaseLogDir,
                                      Base ++ ".event.log"]),
            SuiteLogDir = R#rstate.log_dir,
            NoHtmlOpts = lists:keydelete(html, 1, Opts),
            case annotate_log(false, EventLog, SuiteLogDir, NoHtmlOpts) of
                ok ->
                    ok;
                {error, File, ReasonStr} ->
                    io:format("\nINTERNAL LUX ERROR\n\t~s:\n\t~s\n",
                              [File, ReasonStr]),
                    ok
            end;
        true ->
            ok
    end.

annotate_tmp_summary_log(R, Summary, NextScript) ->
    HtmlPrio = lux_utils:summary_prio(R#rstate.html),
    SummaryPrio = lux_utils:summary_prio(Summary),
    if
        SummaryPrio >= HtmlPrio,
        R#rstate.mode =/= doc,
        R#rstate.mode =/= list,
        R#rstate.mode =/= list_dir ->
            %% Generate premature html log
            NoHtmlOpts = [{case_prefix, R#rstate.case_prefix},
                          {next_script, NextScript}],
            SummaryLog = R#rstate.summary_log,
            TmpLog = SummaryLog ++ ".tmp",
            file:sync(R#rstate.log_fd), % Flush summary log
            case annotate_log(false, TmpLog, NoHtmlOpts) of
                ok ->
                    TmpHtml =  TmpLog ++ ".html",
                    SummaryHtml = SummaryLog ++ ".html",
                    ok = file:rename(TmpHtml, SummaryHtml);
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            ok
    end.

annotate_final_summary_log(R, Summary, HtmlPrio, SummaryLog, Results) ->
    SummaryPrio = lux_utils:summary_prio(Summary),
    if
        SummaryPrio >= HtmlPrio,
        R#rstate.mode =/= doc,
        R#rstate.mode =/= list,
        R#rstate.mode =/= list_dir ->
            Opts = [{case_prefix, R#rstate.case_prefix},
                    {html, R#rstate.html}],
            case annotate_log(false, SummaryLog, Opts) of
                ok ->
                    case R#rstate.progress of
                        silent ->
                            ok;
                        _ ->
                            io:format("\nfile://~s\n",
                                      [SummaryLog ++ ".html"])
                    end,
                    {ok, Summary, SummaryLog, Results};
                {error, _File, _ReasonStr} = Error ->
                    Error
            end;
        true ->
            {ok, Summary, SummaryLog, Results}
    end.

compute_files(R, _LogDir, _LogBase) when R#rstate.files =:= [],
                                         R#rstate.orig_files =:= [],
                                         R#rstate.rerun =:= disable ->
    throw_error(undefined, no_input_files);
compute_files(R, _LogDir, _LogBase) when R#rstate.rerun =:= disable ->
    R;
compute_files(R, _LogDir, LogBase) when R#rstate.files =/= [] ->
    OldLogDirs = R#rstate.files,
    compute_rerun_files(R, OldLogDirs, LogBase, []);
compute_files(R, LogDir, LogBase) ->
    OldLogDirs = [filename:join([filename:dirname(LogDir), "latest_run"])],
    compute_rerun_files(R, OldLogDirs, LogBase, []).

compute_rerun_files(R, LogDirs, LogBase, Acc) ->
    WWW = undefined,
    {Res, NewWWW} = compute_rerun_files(R, LogDirs, LogBase, Acc, WWW),
    lux_utils:stop_app(NewWWW),
    Res.

compute_rerun_files(R, [LogDir|LogDirs], LogBase, Acc, WWW) ->
    OldLog = filename:join([LogDir, LogBase]),
    {ParseRes, NewWWW} = lux_log:parse_summary_log(OldLog, WWW),
    LatestRes =
        case ParseRes of
            {ok, _, Groups, _, _, _} ->
                flatten_results(Groups);
            {error, _, _} ->
                []
        end,
    Files = filter_rerun_files(R, LatestRes),
    compute_rerun_files(R, LogDirs, LogBase, Files ++ Acc, NewWWW);
compute_rerun_files(R, [], _LogBase, Acc, WWW) ->
    {R#rstate{files = lists:usort(Acc)}, WWW}.

filter_rerun_files(R, InitialRes) ->
    MinCond = lux_utils:summary_prio(R#rstate.rerun),
    Return = fun(Res, Script) when is_list(Script) ->
                     Cond = lux_utils:summary_prio(Res),
                     if
                         Cond >= MinCond ->
                             RelScript = lux_utils:drop_prefix(Script),
                             {true, RelScript};
                         true ->
                             false
                     end
             end,
    Filter =
        fun(Res) ->
                case Res of
                    {ok, ScriptRes, Script, _RawLineNo, _} ->
                        Return(ScriptRes, Script);
                    {error, Script, _RawLineNo, _Reason} ->
                        Return(error, Script)
                end
        end,
    lists:zf(Filter, InitialRes).

flatten_results(Groups) ->
    Fun =
        fun(Script, {result, Res}) ->
                case Res of
                    success ->
                        {ok, Res, Script, "0", []};
                    skip ->
                        {ok, Res, Script, "0", []};
                    warning ->
                        {ok, Res, Script, "0", []};
                    {error_line, RawLineNo, Reason} ->
                        {error, Script, RawLineNo, Reason};
                    {error, [Reason]} ->
                        case binary:split(Reason, <<": ">>, [global]) of
                            [_, <<"Syntax error at line ", N/binary>>, _] ->
                                {error, Script, ?b2l(N), Reason};
                            _ ->
                                {error, Script, "0", Reason}
                        end;
                    {error, Reason} ->
                        {error, Script, "0", Reason};
                    {fail, RawLineNo, _ET, _Expected, _Actual, _Details} ->
                        {ok, fail, Script, ?b2l(RawLineNo), []}
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
        case_prefix when is_list(Val) ->
            UserArgs = [NameVal | R#rstate.user_args],
            parse_ropts(T, R#rstate{case_prefix = Val,
                                    user_args = UserArgs});
        progress when Val =:= silent;
                      Val =:= summary; Val =:= brief;
                      Val =:= doc;
                      Val =:= compact; Val =:= verbose ->
            UserArgs = [NameVal | R#rstate.user_args],
            parse_ropts(T, R#rstate{progress = Val,
                                    user_args = UserArgs});
        config_dir when is_list(Val) ->
            parse_ropts(T, R#rstate{config_dir = lux_utils:normalize_filename(Val)});
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
        hostname when is_list(Val) ->
            parse_ropts(T, R#rstate{hostname = Val});
        skip_skip when Val =:= true; Val =:= false ->
            parse_ropts(T, R#rstate{skip_skip = Val});
        mode when Val =:= list; Val =:= list_dir; Val =:= doc;
                  Val =:= validate; Val =:= execute ->
            parse_ropts(T, R#rstate{mode = Val});
        rerun when Val =:= enable; Val =:= success;
                   Val =:= skip; Val =:= warning;
                   Val =:= fail; Val =:= error;
                   Val =:= disable ->
            parse_ropts(T, R#rstate{rerun = Val});
        html when Val =:= validate;
                  Val =:= enable; Val =:= success;
                  Val =:= skip; Val =:= warning;
                  Val =:= fail; Val =:= error;
                  Val =:= disable ->
            parse_ropts(T, R#rstate{html = Val});
        tap when is_list(Val) ->
            TapOpts = [Val|R#rstate.tap_opts],
            parse_ropts(T, R#rstate{tap_opts = TapOpts});
        junit when Val =:= true; Val =:= false ->
            parse_ropts(T, R#rstate{junit = Val});


        %% case options
        _ ->
            UserArgs = [NameVal | R#rstate.user_args],
            parse_ropts(T, R#rstate{user_args = UserArgs})
    end;
parse_ropts([], R) ->
    UserArgs = opts_to_args(lists:reverse(R#rstate.user_args), []),
    R2 = R#rstate{user_args = UserArgs},
    UserLogDir = pick_val(log_dir, R2, undefined),
    {ok, adjust_log_dir(R2, UserLogDir), UserLogDir}.

adjust_log_dir(R, UserLogDir) ->
    UniqStr = uniq_str(R#rstate.start_time),
    UniqRun = "run_" ++ UniqStr,
    Run =
        case R#rstate.run of
            undefined -> UniqRun;
            UserRun   -> UserRun
        end,
    RelLogDir =
        case UserLogDir of
            undefined -> filename:join(["lux_logs", UniqRun]);
            LogDir    -> LogDir
        end,
    AbsLogDir0 = lux_utils:normalize_filename(RelLogDir),
    AbsLogDir =
        if
            UserLogDir =:= undefined, R#rstate.extend_run ->
                ParentDir0 = filename:dirname(AbsLogDir0),
                Link0 = filename:join([ParentDir0, "latest_run"]),
                case file:read_link(Link0) of
                    {ok, LinkTo} ->
                        %% Reuse old log dir
                        lux_utils:normalize_filename(
                          filename:join([ParentDir0, LinkTo]));
                    {error, _} ->
                        AbsLogDir0
                end;
            true ->
                AbsLogDir0
        end,
    UserArgs = opts_to_args([{log_dir, AbsLogDir}], R#rstate.user_args),
    R#rstate{run = Run,
             log_dir = AbsLogDir,
             user_args = UserArgs}.

ensure_log_dir(R, SummaryLog, UserLogDir) ->
    RelFiles = R#rstate.files,
    TagFiles = [{config_dir, R#rstate.config_dir} |
                [{file, F} || F <- RelFiles]],
    lists:foreach(fun check_file/1, TagFiles), % May throw error
    AbsLogDir = R#rstate.log_dir,
    case opt_ensure_dir(R#rstate.extend_run, SummaryLog) of
        ok ->
            opt_create_latest_link(UserLogDir, AbsLogDir),
            AbsFiles = [lux_utils:normalize_filename(F) || F <- RelFiles],
            R#rstate{files = AbsFiles};
        summary_log_exists ->
            throw_error(AbsLogDir,
                        "ERROR: Summary log file already exists: ~s\n",
                        [SummaryLog]);
        {error, FileReason} ->
            throw_error(AbsLogDir,
                        "ERROR: Failed to create log directory: ~s -> ~s\n",
                        [AbsLogDir, file:format_error(FileReason)])
    end.

opt_create_latest_link(undefined, AbsLogDir) ->
    ParentDir = filename:dirname(AbsLogDir),
    Link = filename:join([ParentDir, "latest_run"]),
    Base = filename:basename(AbsLogDir),
    _ = file:delete(Link),
    _ = file:make_symlink(Base, Link),
    ok;
opt_create_latest_link(_UserLogDir, _AbsLogDir) ->
    ok.

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
                    BinErr = io_lib:format("~p ~s: ~s\n",
                                           [Tag,
                                            File,
                                            file:format_error(enoent)]),
                    throw_error(File, BinErr)
            end;
        file ->
            case filelib:is_file(File) of
                true ->
                    ok;
                false ->
                    BinErr = io_lib:format("~s: ~s \n",
                                           [File,
                                            file:format_error(enoent)]),
                    throw_error(File, BinErr)
            end
    end.

uniq_str({_MegaSecs, _Secs, MicroSecs} = Now) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:now_to_universal_time(Now),
    lists:concat([Year, "_", r2(Month), "_", r2(Day), "_",
                  r2(Hour), "_", r2(Min), "_", r2(Sec), "_",
                  ?i2l(MicroSecs)]).

r2(Int) when is_integer(Int) ->
    r2(?i2l(Int));
r2(String) when is_list(String) ->
    string:right(String, 2, $0).

run_cases(R, [{SuiteFile,{error=Summary,Reason}, _P, _LenP}|Scripts],
          OldSummary, Results, Max, CC, List, Opaque)
  when R#rstate.mode =:= list;
       R#rstate.mode =:= list_dir;
       R#rstate.mode =:= doc ->
    ReasonStr = file:format_error(Reason),
    io:format("~s:\n", [lux_utils:drop_prefix(SuiteFile)]),
    io:format("\tERROR ~s\n", [ReasonStr]),
    NewSummary = lux_utils:summary(OldSummary, Summary),
    ListErr = ?l2b(io_lib:format( "~s~s: ~s\n",
                                  [?TAG("error"),
                                   SuiteFile,
                                   ReasonStr])),
    Results2 = [{error, SuiteFile, ListErr} | Results],
    run_cases(R, Scripts, NewSummary, Results2, Max, CC+1, List, Opaque);
run_cases(R, [{SuiteFile, {error,Reason}, P, LenP}|Scripts],
          OldSummary, Results, Max, CC, List, Opaque) ->
    init_case_rlog(R, P, SuiteFile),
    ListErr =
        double_rlog(R, "~s~s: ~s\n",
                    [?TAG("error"), SuiteFile, file:format_error(Reason)]),
    Results2 = [{error, SuiteFile, ListErr} | Results],
    lux:trace_me(70, suite, 'case', SuiteFile, []),
    tap_case_begin(R, SuiteFile),
    lux:trace_me(70, 'case', suite, error, [Reason]),
    tap_case_end(R, CC, SuiteFile, P, LenP, Max, error, "0", Reason, Reason),
    run_cases(R, Scripts, OldSummary, Results2, Max, CC+1, List, Opaque);
run_cases(OrigR, [{SuiteFile,{ok,Script}, P, LenP} | Scripts],
          OldSummary, Results, Max, CC, List, Opaque) ->
    RunMode = OrigR#rstate.mode,
    TmpR = OrigR#rstate{warnings = [], file_args = []},
    CaseStartTime = lux_utils:timestamp(),
    case parse_script(TmpR, SuiteFile, Script) of
        {ok, NewR, Script2, Cmds, Opts} ->
            ParseWarnings = NewR#rstate.warnings,
            case NewR#rstate.mode of
                list ->
                    run_cases(NewR, Scripts, OldSummary, Results,
                              Max, CC+1, [Script|List], Opaque);
                list_dir ->
                    run_cases(NewR, Scripts, OldSummary, Results,
                              Max, CC+1, [Script|List], Opaque);
                doc ->
                    DocCmds = extract_doc(Script2, Cmds),
                    Script3 = lux_utils:drop_prefix(Script2),
                    io:format("~s:\n", [Script3]),
                    DisplayDoc =
                        fun({Level, Doc}, MaxLevel) ->
                                Print =
                                    fun() ->
                                        Indent = lists:duplicate(Level, $\t),
                                        io:format("~s~s\n", [Indent, Doc])
                                    end,
                                if
                                    MaxLevel =:= once_only ->
                                        MaxLevel;
                                    MaxLevel =:= 0,
                                    Level =:= 1 ->
                                        Print(),
                                        once_only;
                                    MaxLevel =:= infinity;
                                    Level =< MaxLevel ->
                                        Print(),
                                        MaxLevel;
                                    true ->
                                        MaxLevel
                                end
                        end,
                    Docs =  [Doc || #cmd{arg = MultiDoc} <- DocCmds,
                                    Doc <- MultiDoc],
                    MaxLevel = pick_val(doc, NewR, infinity),
                    lists:foldl(DisplayDoc, MaxLevel, Docs),
                    case ParseWarnings of
                        [] ->
                            NewSummary = OldSummary,
                            Results2 = Results;
                        _ ->
                            Summary = warning,
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            Results2 = [{Summary, Script2, ParseWarnings} |
                                        Results]
                    end,
                    AllWarnings = OrigR#rstate.warnings ++ ParseWarnings,
                    run_cases(NewR#rstate{warnings = AllWarnings},
                              Scripts, NewSummary, Results2,
                              Max, CC+1, List, Opaque);
                validate ->
                    init_case_rlog(NewR, P, Script),
                    case ParseWarnings of
                        [] ->
                            Summary = success,
                            NewSummary = OldSummary,
                            Results2 = Results;
                        _ ->
                            Summary = warning,
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            Results2 = [{NewSummary, Script2, ParseWarnings} |
                                        Results]
                    end,
                    double_rlog(NewR, "~s~s\n",
                                [?TAG("result"),
                                 string:to_upper(atom_to_list(Summary))]),
                    AllWarnings = OrigR#rstate.warnings ++ ParseWarnings,
                    run_cases(NewR#rstate{warnings = AllWarnings},
                              Scripts, NewSummary, Results2,
                              Max, CC+1, List, Opaque);
                execute ->
                    annotate_tmp_summary_log(NewR, OldSummary, Script),
                    lux:trace_me(70, suite, 'case', P, []),
                    tap_case_begin(NewR, Script),
                    init_case_rlog(NewR, P, Script),
                    Res = lux:interpret_commands(Script2, Cmds,
                                                 ParseWarnings,
                                                 CaseStartTime,
                                                 Opts, Opaque),
                    SkipReason = "",
                    case Res of
                        {ok, Summary, _, FullLineNo, CaseLogDir, RunWarnings,
                         Events, FailBin, NewOpaque} ->
                            lux:trace_me(70, 'case', suite, Summary,
                                         []),
                            tap_case_end(NewR, CC, Script,
                                         P, LenP, Max, Summary,
                                         FullLineNo, SkipReason, FailBin),
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            Res2 = {ok, Summary, Script, FullLineNo,
                                    CaseLogDir, Events, FailBin, Opaque},
                            NewResults = [Res2 | Results],
                            NewScripts = Scripts;
                        {error, MainFile, FullLineNo, CaseLogDir, ErrorMsg}
                          when ErrorMsg =:= <<"suite_timeout" >> ->
                            RunWarnings = ParseWarnings,
                            Res2 = {error, MainFile, FullLineNo, ErrorMsg},
                            Summary = error,
                            lux:trace_me(70, 'case', suite, Summary,
                                         [FullLineNo]),
                            tap_case_end(NewR, CC, Script,
                                         P, LenP, Max, Summary,
                                         FullLineNo, SkipReason, ErrorMsg),
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            NewResults = [Res2 | Results],
                            NewScripts = [],
                            NewOpaque  = Opaque;
                        {error, MainFile, FullLineNo, CaseLogDir, ErrorMsg} ->
                            RunWarnings = ParseWarnings,
                            Res2 = {error, MainFile, FullLineNo, ErrorMsg},
                            Summary = error,
                            lux:trace_me(70, 'case', suite, Summary,
                                         [FullLineNo]),
                            tap_case_end(NewR, CC, Script,
                                         P, LenP, Max, Summary,
                                         FullLineNo, SkipReason, ErrorMsg),
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            NewResults = [Res2 | Results],
                            NewScripts = Scripts,
                            NewOpaque  = Opaque
                    end,
                    AllWarnings = OrigR#rstate.warnings ++ RunWarnings,
                    NewR2 = NewR#rstate{warnings = AllWarnings},
                    annotate_event_log(NewR2, Script, NewSummary,
                                       CaseLogDir, Opts),
                    _ = write_results(NewR2, NewSummary, NewResults),
                    run_cases(NewR2, NewScripts, NewSummary, NewResults,
                              Max, CC+1, List, NewOpaque)
            end;
        {skip, NewR, _ErrorStack, SkipReason}
          when RunMode =:= list;
               RunMode =:= list_dir;
               RunMode =:= doc ->
            Summary =
                case ?b2l(SkipReason) of
                    "FAIL" ++ _ -> fail;
                    _           -> skip
                end,
            NewSummary = lux_utils:summary(OldSummary, Summary),
            ParseWarnings = NewR#rstate.warnings,
            AllWarnings = OrigR#rstate.warnings ++ ParseWarnings,
            NewR2 = NewR#rstate{warnings = AllWarnings},
            run_cases(NewR2, Scripts, NewSummary, Results,
                      Max, CC+1, List, Opaque);
        {skip, NewR, ErrorStack, SkipReason} ->
            #cmd_pos{rev_file = RevScript2} = lists:last(ErrorStack),
            Script2 = lux_utils:pretty_filename(RevScript2),
            lux:trace_me(70, suite, 'case', P, []),
            tap_case_begin(NewR, Script),
            init_case_rlog(NewR, P, Script),
            double_rlog(NewR, "~s~s\n",
                        [?TAG("result"), SkipReason]),
            {ok, _} = lux_case:copy_orig(NewR#rstate.log_dir, Script2),
            Summary =
                case ?b2l(SkipReason) of
                    "FAIL" ++ _ -> fail;
                    _           -> skip
                end,
            lux:trace_me(70, 'case', suite, Summary, [SkipReason]),
            #cmd_pos{lineno = FullLineNo} = stack_error(ErrorStack, SkipReason),
            tap_case_end(NewR, CC, Script,
                         P, LenP, Max, Summary,
                         FullLineNo, ?b2l(SkipReason), <<>>),
            NewSummary = lux_utils:summary(OldSummary, Summary),
            Res = {ok, Summary, Script2, FullLineNo,
                   NewR#rstate.log_dir, [], <<>>, []},
            Results2 = [Res | Results],
            ParseWarnings = NewR#rstate.warnings,
            AllWarnings = OrigR#rstate.warnings ++ ParseWarnings,
            run_cases(NewR#rstate{warnings = AllWarnings},
                      Scripts, NewSummary, Results2,
                      Max, CC+1, List, Opaque);
        {error = Summary, _ErrR, _ErrorStack, _ErrorBin}
          when RunMode =:= list;
               RunMode =:= list_dir ->
            NewSummary = lux_utils:summary(OldSummary, Summary),
            run_cases(OrigR, Scripts, NewSummary, Results,
                      Max, CC+1, [Script|List], Opaque);
        {error = Summary, ErrR, ErrorStack, ErrorBin}
          when RunMode =:= doc ->
            #cmd_pos{rev_file = RevMainFile,
                     lineno = FullLineNo,
                     type = ErrorBin2} =
                stack_error(ErrorStack, ErrorBin),
            io:format("~s:\n", [lux_utils:drop_prefix(Script)]),
            io:format("\tERROR ~s\n", [ErrorBin]),
            MainFile = lux_utils:pretty_filename(RevMainFile),
            Results2 = [{error, MainFile, FullLineNo, ErrorBin2} | Results],
            NewWarnings = ErrR#rstate.warnings,
            AllWarnings = OrigR#rstate.warnings ++ NewWarnings,
            NewSummary = lux_utils:summary(OldSummary, Summary),
            run_cases(OrigR#rstate{warnings = AllWarnings},
                      Scripts, NewSummary, Results2,
                      Max, CC+1, List, Opaque);
        {error, ErrR, ErrorStack, ErrorBin} ->
            #cmd_pos{rev_file = RevMainFile,
                     lineno = FullLineNo,
                     type = ErrorBin2} =
                stack_error(ErrorStack, ErrorBin),
            MainFile = lux_utils:pretty_filename(RevMainFile),
            init_case_rlog(ErrR, P, Script),
            double_rlog(ErrR, "~sERROR ~s\n",
                        [?TAG("result"), ErrorBin2]),
            Summary = error,
            tap_case_begin(ErrR, Script),
            lux:trace_me(70, 'case', suite, Summary, []),
            tap_case_end(ErrR, CC, Script,
                         P, LenP, Max, Summary,
                         "0", ErrorBin, ErrorBin),
            NewWarnings = ErrR#rstate.warnings,
            AllWarnings = OrigR#rstate.warnings ++ NewWarnings,
            NewSummary = lux_utils:summary(OldSummary, Summary),
            Results2 = [{error, MainFile, FullLineNo, ErrorBin2} | Results],
            run_cases(OrigR#rstate{warnings = AllWarnings},
                      Scripts, NewSummary, Results2,
                      Max, CC+1, List, Opaque)
    end;
run_cases(R, [], Summary, Results, _Max, _CC, List, _Opaque) ->
    List2 = [lux_utils:drop_prefix(File) || File <- List],
    case R#rstate.mode of
        list ->
            [io:format("~s\n", [File]) ||
                File <- lists:usort(List2)];
        list_dir ->
            List3 = [filename:dirname(File) || File <- List2],
            [io:format("~s\n", [File]) ||
                File <- lists:usort(List3)];
        _ ->
            ok
    end,
    {R, Summary, lists:reverse(Results)}.

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
  when Mode =:= list; Mode =:= list_dir; Mode =:= doc ->
    {ok, Summary, SummaryLog, Results};
write_results(#rstate{progress=Progress,
                      summary_log=SummaryLog,
                      warnings=Warnings},
              Summary, Results) when is_list(SummaryLog) ->
    lux_log:write_results(Progress, SummaryLog, Summary, Results, Warnings),
    {ok, Summary, SummaryLog, Results}.

print_results(#rstate{progress=Progress,warnings=Warnings}, Summary, Results) ->
    lux_log:print_results(Progress, {false,standard_io},
                          Summary, Results, Warnings).

parse_script(R, _SuiteFile, Script) ->
    Opts0 = args_to_opts(lists:reverse(case_config_args(R)), case_style, []),
    case lux:parse_file(Script,
                        R#rstate.mode,
                        R#rstate.skip_skip,
                        true,
                        Opts0) of
        {ok, Script2, Cmds, FileOpts, NewWarnings} ->
            FileArgs = opts_to_args(FileOpts, R#rstate.file_args),
            R2 = R#rstate{internal_args = [],
                          file_args = FileArgs,
                          warnings = NewWarnings},
            LogDir = pick_val(log_dir, R, undefined),
            LogFd = R#rstate.log_fd,
            LogFun = fun(Bin) -> lux_log:safe_write(LogFd, Bin) end,
            InternalArgs = [{log_dir, LogDir},
                            {log_fun, LogFun},
                            {log_fd,  LogFd}],
            R3 = R2#rstate{internal_args = InternalArgs},
            MergeArgs = fun(A, Acc) ->
                                O = args_to_opts(A, case_style, []),
                                opts_to_args(O, Acc)
                        end,
            Args = lists:foldl(MergeArgs, [], args_dicts(R3)),
            Opts = args_to_opts(Args, case_style, []),
            {ok, R3, Script2, Cmds, Opts};
        {skip, ErrorStack, ErrorBin} ->
            {skip, R, ErrorStack, ErrorBin};
        {error, ErrorStack, ErrorBin} ->
            {error, R, ErrorStack, ErrorBin}
    end.

parse_config(R) ->
    %% Default opts
    DefaultBase = "luxcfg",
    PrivDir = code:lib_dir(?APPLICATION, priv),
    DefaultDir = lux_utils:normalize_filename(PrivDir),
    DefaultFile = filename:join([DefaultDir, DefaultBase]),
    {DefaultOpts, DefaultWarnings} = parse_config_file(R, DefaultFile),
    DefaultArgs = opts_to_args(DefaultOpts, []),
    R2 = R#rstate{default_args = DefaultArgs},

    %% Config dir
    case pick_val(config_dir, R2, R2#rstate.config_dir) of
        undefined    -> RelConfigDir = PrivDir;
        RelConfigDir -> ok
    end,
    AbsConfigDir = lux_utils:normalize_filename(RelConfigDir),
    check_file({config_dir, AbsConfigDir}),

    %% Arch spec opts
    ActualConfigName = config_name(),
    DefaultData =
        builtins(R, ActualConfigName) ++
        [{'default file', [string], DefaultFile}] ++ DefaultArgs,
    {ConfigName, AbsConfigFile} =
        config_file(R2, AbsConfigDir, R2#rstate.config_name, ActualConfigName),
    if
        AbsConfigFile =/= DefaultFile ->
            {ConfigOpts, ConfigWarnings} = parse_config_file(R2, AbsConfigFile),
            ConfigArgs = opts_to_args(ConfigOpts, []),
            ConfigData = [{'config file', [string], AbsConfigFile}] ++
                ConfigArgs;
        true ->
            ConfigArgs = [],
            ConfigData = [],
            ConfigWarnings = []
        end,
    NewWarnings = DefaultWarnings ++ ConfigWarnings,
    AllWarnings = R#rstate.warnings ++ NewWarnings,
    R3 = R2#rstate{config_name = ConfigName,
                   config_dir  = AbsConfigDir,
                   config_file = AbsConfigFile,
                   config_args = ConfigArgs,
                   warnings    = AllWarnings},
    {DefaultData ++ ConfigData, R3}.

builtins(R, ActualConfigName) ->
    {ok, Cwd} = file:get_cwd(),
    [
     {'start time', [string], lux_utils:now_to_string(R#rstate.start_time)},
     {version, [string], lux_utils:version()},
     {root_dir, [string], code:root_dir()},
     {run_dir, [string], Cwd},
     {log_dir, [string], R#rstate.log_dir},
     {command, [string], hd(R#rstate.orig_args)},
     {arguments, [string], string:join(tl(R#rstate.orig_args), " ")},
     {hostname, [string], R#rstate.hostname},
     {architecture, [string], ActualConfigName},
     {'system info', [string], sys_info()},
     {suite, [string], R#rstate.suite},
     {run, [string], R#rstate.run},
     {revision, [string], R#rstate.revision},
     {'config name', [string], R#rstate.config_name},
     {config_dir, [string], R#rstate.config_dir}
    ].

config_name() ->
    try
        {[Line], "0"} = lux_utils:cmd("uname -sm"),
        [Kernel, Machine] = string:tokens(Line, " "),
        Kernel ++ "-" ++ Machine
    catch
        Class:Reason when not (Class =:= error andalso Reason =:= undef) ->
            erlang:system_info(system_architecture)
    end.

parse_config_file(R, AbsConfigFile) ->
    Opts0 = args_to_opts(lists:reverse(case_config_args(R)), case_style, []),
    SkipSkip = true,
    case lux:parse_file(AbsConfigFile, R#rstate.mode, SkipSkip, false, Opts0) of
        {ok, _File, _Cmds, UpdatedOpts, NewWarnings} ->
            Key = config_dir,
            Opts2 =
                case lists:keyfind(Key, 1, UpdatedOpts) of
                    false ->
                        UpdatedOpts;
                    {_, Dir} ->
                        Top = filename:dirname(AbsConfigFile),
                        Dir2 = filename:absname(Dir, Top),
                        Dir3 = lux_utils:normalize_filename(Dir2),
                        lists:keystore(Key, 1, UpdatedOpts, {Key, Dir3})
                end,
            {lists:keydelete(log_dir, 1, Opts2), NewWarnings};
        {skip, _ErrorStack, _SkipBin} ->
            {[], []};
        {error, ErrorStack, ErrorBin} ->
            Enoent = ?l2b(file:format_error(enoent)),
            if
                ErrorBin =:= Enoent ->
                    {[], []};
                true ->
                    #cmd_pos{rev_file = MainFile,
                             type = ErrorBin2} =
                        stack_error(ErrorStack, ErrorBin),
                    throw_error(MainFile, ErrorBin2)
            end
    end.

case_config_args(R) ->
    [{Key, Val} || {Key, Val} <- all_config_args(R),
                   is_case_config_type(Key)].

all_config_args(R) ->
    lists:append(lists:reverse(args_dicts(R))).

stack_error(ErrorStack, ErrorBin) ->
    #cmd_pos{rev_file = RevMainFile} = hd(ErrorStack),
    #cmd_pos{rev_file = RevErrorFile} = lists:last(ErrorStack),
    FullLineNo = lux_utils:pretty_full_lineno(ErrorStack),
    if
        RevErrorFile =:= RevMainFile ->
            #cmd_pos{rev_file = RevMainFile,
                     lineno = FullLineNo,
                     type = ErrorBin};
        true ->
            ErrorFile = lux_utils:pretty_filename(RevErrorFile),
            FileBin = ?l2b(ErrorFile),
            ErrorBin2 = <<FileBin/binary, ": ", ErrorBin/binary>>,
            #cmd_pos{rev_file = RevMainFile,
                     lineno = FullLineNo,
                     type = ErrorBin2}
    end.

config_file(R, ConfigDir, UserConfigName, ActualConfigName) ->
    Ext = ".luxcfg",
    case UserConfigName of
        undefined ->
            Host = R#rstate.hostname,
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
    {[Line], "0"} = lux_utils:cmd("uname -a"),
    Line.

real_hostname() ->
    case inet:gethostname() of
        {ok, Host} -> Host;
        _          -> "localhost"
    end.

user_prefix() ->
    case os:getenv("USER") of
        false -> "";
        ""    -> "";
        User  -> User ++ "@"
    end.

double_rlog(#rstate{progress = Progress, log_fd = Fd}, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    case Fd of
        undefined -> ?l2b(IoList);
        _         -> lux_log:double_write(Progress, Fd, IoList)
    end.

init_case_rlog(#rstate{progress = Progress, log_fd = Fd},
               RelScript, AbsScript) ->
    Tag = ?TAG("test case"),
    AbsIoList = io_lib:format("\n~s~s\n", [Tag, AbsScript]),
    case Fd of
        undefined ->
            ?l2b(AbsIoList);
        _ ->
            AbsBin = lux_log:safe_write(Fd, AbsIoList),
            case Progress of
                silent ->
                    ok;
                _ ->
                    RelIoList = io_lib:format("\n~s~s\n", [Tag, RelScript]),
                    lux_log:safe_write(undefined, ?l2b(RelIoList))
            end,
            AbsBin
    end.

prefixed_rel_script(R, AbsScript) ->
    RelScript = lux_utils:drop_prefix(AbsScript),
    R#rstate.case_prefix ++ RelScript.

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

pick_val(Tag, R, Default) ->
    Dicts = args_dicts(R),
    pick_first(Tag, 1, Dicts, Default).

pick_first(Tag, Pos, [Dict|Dicts], Default) ->
    case lists:keyfind(Tag, 1, Dict) of
        false ->
            pick_first(Tag, Pos, Dicts, Default);
        {_, Val} ->
            Val
    end;
pick_first(_Tag, _Pos, [], Default) ->
    Default.

args_dicts(#rstate{internal_args = I,
                   user_args = U,
                   file_args = F,
                   config_args = C,
                   default_args = D}) ->
    [I, U, F, C, D].

opts_to_args(KeyVals, Acc) ->
    do_opts_to_args(KeyVals, Acc, []).

do_opts_to_args([], Acc, _) ->
    Acc;
do_opts_to_args([KeyVal | KeyVals], Acc, OldUpdated) ->
    Key = element(1, KeyVal),
    ValPos = tuple_size(KeyVal),
    Val = element(ValPos, KeyVal),
    case lists:keyfind(Key, 1, Acc) of
        false -> OldVal = [];
        {_, OldVal} -> ok
    end,
    NewUpdated = [Key | OldUpdated],
    case merge_oper(KeyVal, OldUpdated) of
        append ->
            %% Multi - Expand val
            Val2 = OldVal ++ [Val],
            KeyVal2 = setelement(ValPos, KeyVal, Val2),
            Acc2 = lists:keystore(Key, 1, Acc, KeyVal2),
            do_opts_to_args(KeyVals, Acc2, NewUpdated);
        reset ->
            %% Multi - Clear old settings in order to override unwanted defaults
            Stripped = [KV || KV <- Acc, element(1, KV) =/= Key],
            KeyVal2 = setelement(ValPos, KeyVal, [Val]),
            Acc2 = lists:keystore(Key, 1, Stripped, KeyVal2),
            do_opts_to_args(KeyVals, Acc2, NewUpdated);
        replace ->
            %% Single - replace old val
            Acc2 = lists:keystore(Key, 1, Acc, KeyVal),
            do_opts_to_args(KeyVals, Acc2, NewUpdated)
    end.

args_to_opts([{_Key, []} | KeyVals], Style, Acc) when Style =:= suite_style ->
    args_to_opts(KeyVals, Style, Acc);
args_to_opts([{Key, Val} | KeyVals], Style, Acc) ->
    case arg_arity({Key, Val}) of
        single when Style =:= case_style ->
            args_to_opts(KeyVals, Style, [{Key, Val} | Acc]);
        single when Style =:= suite_style ->
            SingleVal = lists:last(Val),
            args_to_opts(KeyVals, Style, [{Key, SingleVal} | Acc]);
        multi ->
            Split = [{Key, V} || V <- Val],
            args_to_opts(KeyVals, Style, Split ++ Acc)
    end;
args_to_opts([], _Style, Acc) ->
    Acc.

arg_arity(KeyVal) ->
    case merge_oper(KeyVal, []) of
        replace -> single;
        _       -> multi
    end.

merge_oper({Key, Val}, Updated) ->
    {ok, Type} = config_type(Key),
    merge_oper({Key, Type, Val}, Updated);
merge_oper({Key, Type, _Val}, Updated) ->
    case Type of
        [{std_list, _}] ->
            append;
        [{reset_list, _}] ->
            case lists:member(Key, Updated) of
                true  -> append;
                false -> reset
            end;
        _ ->
            %% Assume single val
            replace
    end.

config_type(Name) ->
    case suite_config_type(Name) of
        {ok, Type} ->
            {ok, Type};
        {error, _Reason} ->
            case lux_case:config_type(Name) of
                {ok, _Pos, Type} ->
                    {ok, Type};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

is_case_config_type(Name) ->
    case lux_case:config_type(Name) of
        {ok, _Pos, _Type} -> true;
        {error, _Reason}  -> false
    end.

suite_config_type(Name) ->
    Prio = [enable, success, skip, warning, fail, error, disable],
    case Name of
        rerun ->
            {ok, [{atom, Prio}]};
        html ->
            {ok, [{atom, Prio}]};
        skip_skip ->
            {ok, [{atom, [true, false]}]};
        mode ->
            {ok, [{atom, [list, list_dir, doc, validate, execute]}]};
        doc ->
            {ok, [{integer, 0, infinity}]};
        config_name ->
            {ok, [string]};
        suite ->
            {ok, [string]};
        run ->
            {ok, [string]};
        extend_run ->
            {ok, [string]};
        revision ->
            {ok, [string]};
        hostname ->
            {ok, [string]};
        file_pattern ->
            {ok, [string]};
        tap ->
            {ok, [{std_list, [string]}]};
        junit ->
            {ok, [{atom, [true, false]}]};
        _ ->
            {error, ?l2b(lists:concat(["Bad argument: ", Name]))}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tap_suite_begin(R, Scripts, Directive)
  when R#rstate.mode =/= list,
       R#rstate.mode =/= list_dir,
       R#rstate.mode =/= doc ->
    TapLog = filename:join([R#rstate.log_dir, "lux.tap"]),
    TapOpts = [TapLog | R#rstate.tap_opts],
    case lux_tap:open(TapOpts) of
        {ok, TAP} ->
            ok = lux_tap:plan(TAP, length(Scripts), Directive),
            ok = lux_tap:diag(TAP, "\n"),
            %% ok = lux_tap:diag(TAP, "LUX - LUcid eXpect scripting"),
            OptUser = user_prefix(),
            Host = real_hostname(),
            ok = lux_tap:diag(TAP, "ssh " ++ OptUser ++ Host),
            {ok, Cwd} = file:get_cwd(),
            ok = lux_tap:diag(TAP, "cd " ++ Cwd),
            Args = string:join(tl(R#rstate.orig_args), " "),
            ok = lux_tap:diag(TAP, "lux " ++ Args),
            SummaryLog = lux_utils:drop_prefix(R#rstate.summary_log),
            ok = lux_tap:diag(TAP, "open " ++ SummaryLog ++ ".html"),
            ok = lux_tap:diag(TAP, "\n"),
            {ok, R#rstate{tap = TAP, tap_opts = TapOpts}};
        {error, Reason} ->
            {error, Reason}
    end;
tap_suite_begin(R, _Scripts, _Directive) ->
    {ok, R#rstate{tap = undefined}}.

tap_suite_end(#rstate{tap = TAP, warnings = Warnings}, Summary, Results)
  when TAP =/= undefined ->
    Len = fun(Res, Tag) -> ?i2l(length(lux_log:pick_result(Res, Tag))) end,
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

%% tap_case_begin(#rstate{tap = TAP} = R, AbsScript)
%%   when TAP =/= undefined ->
%%     PrefixedRelScript = prefixed_rel_script(R, AbsScript),
%%     ok = lux_tap:diag(TAP, "lux " ++ PrefixedRelScript);
tap_case_begin(#rstate{}, _AbsScript) ->
    ok.

tap_case_end(#rstate{tap = TAP, skip_skip = SkipSkip, warnings = Warnings},
             CaseCount, _AbsScript,
             P, LenP, Max,
             Result, FullLineNo, Reason, Details)
  when TAP =/= undefined ->
    CaseCountStr = ?i2l(CaseCount),
    PrefixLen = lists:min([4, 5-length(CaseCountStr)]),
    Indent = lists:duplicate(PrefixLen, " "),
    Descr = Indent ++ CaseCountStr ++ " " ++ P,
    TodoReason =
        case Reason of
            "" -> "";
            _  -> "TODO - " ++ Reason
        end,
    {Outcome, Directive} =
        case Result of
            error                 -> {not_ok, ""};
            fail when SkipSkip    -> {not_ok, TodoReason};
            fail                  -> {not_ok, ""};
            warning               -> {ok,     Reason};
            skip                  -> {ok,     Reason};
            success when SkipSkip -> {ok,     TodoReason};
            success               -> {ok,     ""}
        end,
    lux_tap:test(TAP, Outcome, Descr, Directive, Max-LenP),
    Format = fun({warning, _File, LineNo, W}) ->
                     ok = lux_tap:diag(TAP, "WARNING at line " ++ LineNo),
                     ok = lux_tap:diag(TAP,  ?b2l(W))
             end,
    lists:foreach(Format, Warnings),
    case Details of
        <<>> ->
            ok;
        _ ->
            Prefix = string:to_upper(atom_to_list(Result)),
            Lines = [
                     ?l2b([Prefix, " at line ", FullLineNo]) |
                     binary:split(Details, <<"\n">>, [global])
                    ],
            [ok = lux_tap:diag(TAP, ?b2l(F)) ||
                F <- Lines, F =/= <<>>]
    end;
tap_case_end(#rstate{}, _CaseCount, _Script,
             _P, _LenP, _Max,
             _Result, _FullLineNo,
             _Reason, _Details) ->
    ok.

throw_error(File, Reason) ->
    throw({error, File, Reason}).

throw_error(File, Format, Args) ->
    throw_error(File,
                lux_log:safe_format(undefined, Format, Args)).
