%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2023 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_suite).

-export([run/4, args_to_opts/3, annotate_log/3, merge_logs/3]).

-include("lux.hrl").
-include_lib("kernel/include/file.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Run a test suite

-spec run(filename(), opts(), string(), [string()]) ->
          {ok, summary(), filename(), [result()]} | error() | no_input().

run(Files, Opts, PrevLogDir, OrigArgs) when is_list(Files) ->
    R0 = #rstate{files = Files,
                 orig_files = Files,
                 orig_args = OrigArgs,
                 prev_log_dir = PrevLogDir},
    case parse_ropts(Opts, R0) of
        {ok, R}
          when R#rstate.mode =:= list orelse
               R#rstate.mode =:= list_dir orelse
               R#rstate.mode =:= doc ->
            try
                R2 = compute_files(R, ?SUITE_SUMMARY_LOG),
                doc_run(R2)
            catch
                ?CATCH_STACKTRACE(throw, {error, FileErr, Reason}, _EST)
                {error, FileErr, Reason};
                ?CATCH_STACKTRACE(Class, Reason, EST)
                ReasonStr =
                lists:flatten(?FF("~p:~p\n\t~p", [Class, Reason, EST])),
                {ok, Cwd} = file:get_cwd(),
                {run_error, Cwd, ReasonStr}
            end;
        {ok, R} ->
            LogDir = R#rstate.log_dir,
            SummaryLog = filename:join([LogDir, ?SUITE_SUMMARY_LOG]),
            try
                {ConfigProps, R2} = parse_config(R), % May throw error
                PostCaseCmds = parse_post_case_cmds(R2),
                SuiteRef = start_suite_timer(R2),
                try
                    R3 = R2#rstate{suite_timer_ref = SuiteRef,
                                   post_case_cmds = PostCaseCmds},
                    R4 = compute_files(R3, ?SUITE_SUMMARY_LOG),
                    R5 = adjust_files(R4),
                    full_run(R5, ConfigProps, SummaryLog)
                catch
                    throw:{error, undefined, no_input_files} ->
                        {ok, Cwd} = file:get_cwd(),
                        {run_error, Cwd, "ERROR: No input files\n"};
                    throw:{error, FileErr, ReasonStr} ->
                        {run_error, FileErr, ReasonStr};
                    ?CATCH_STACKTRACE(Class, Reason, EST)
                    ReasonStr =
                    lists:flatten(?FF("~p:~p\n\t~p", [Class, Reason, EST])),
                    {run_error, SummaryLog, ReasonStr}
                after
                    lux_utils:cancel_timer(SuiteRef)
                end
            catch
                throw:{error, FileErr2, ReasonStr2} ->
                    {run_error, FileErr2, ReasonStr2};
                ?CATCH_STACKTRACE(Class2, Reason2, EST2)
                    ReasonStr2 =
                        lists:flatten(?FF("~p:~p\n\t~p",
                                          [Class2, Reason2, EST2])),
                {run_error, SummaryLog, ReasonStr2}
            end;
        {error, {badarg, Name, Val}} ->
            ArgErr =
                lux_log:safe_format(undefined,
                                    "ERROR: ~p is an illegal argument (~p)\n",
                                    [Name, Val]),
            {run_error, hd(Files), ArgErr};
        {error, File, ArgErr} ->
            {run_error, File, ArgErr}
    end.

adjust_files(R) ->
    ReRun = R#rstate.rerun,
    check_file({config_dir, R#rstate.config_dir}, ReRun),
    RelFiles = R#rstate.files,
    TagFiles = [{file, F} || F <- RelFiles],
    lists:foldl(fun check_file/2, ReRun, TagFiles),
    AbsFiles = [lux_utils:normalize_filename(F) || F <- RelFiles],
    R#rstate{files = AbsFiles}.

doc_run(R) ->
    R2 = R#rstate{log_fd = undefined, summary_log = undefined},
    {_ConfigProps, R3} = parse_config(R2),  % May throw error
    {R4, Summary, SuiteResults} = run_suite(R3, R3#rstate.files, success, []),
    write_results(R4, Summary, SuiteResults).

run_suite(R0, SuiteFiles, OldSummary, SuiteResults) ->
    {Scripts, Max} = expand_suite(R0,  SuiteFiles, [], 0),
    ?TRACE_ME2(80, suite, string:join(SuiteFiles, " "), []),
    {ok, R} = tap_suite_begin(R0, Scripts, ""),
    try
        {NewR, Summary, NewSuiteResults} =
            run_cases(R, Scripts, OldSummary, SuiteResults, Max, 1, [], []),
        NewSummary =
            if
                NewSuiteResults =:= []     andalso
                R#rstate.mode =/= validate andalso
                R#rstate.mode =/= dump     andalso
                R#rstate.mode =/= expand ->
                    lux_utils:summary(Summary, warning);
                true ->
                    Summary
            end,
        ?TRACE_ME2(80, suite, NewSummary, []),
        tap_suite_end(NewR, NewSummary, NewSuiteResults),
        {NewR, NewSummary, NewSuiteResults}
    catch
        ?CATCH_STACKTRACE(Class, Reason, EST)
            ?TRACE_ME2(80, suite, Class, [Reason]),
            case R#rstate.tap of
                undefined ->
                    ok;
                TAP ->
                    lux_tap:bail_out(TAP, "Internal error")
            end,
            erlang:raise(Class, Reason, EST)
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
            Recursive = true,
            Files = lux_utils:fold_files(File, RegExp, Recursive, Fun, []),
            {ok, lists:sort(Files)};
        {ok, _} ->
            {ok, [File]};
        {error, Reason} ->
            {error, Reason}
    end.

full_run(#rstate{mode = Mode} = R, _ConfigProps, SummaryLog)
  when Mode =:= dump orelse
       Mode =:= expand ->
    InitialSummary = success,
    InitialSuiteRes = [],
    {_R2, Summary, Results} =
        run_suite(R, R#rstate.files, InitialSummary, InitialSuiteRes),
    {run_ok, Summary, SummaryLog, Results};
full_run(#rstate{progress = Progress} = R, ConfigProps, SummaryLog) ->
    ExtendRun = R#rstate.extend_run,
    case lux_log:open_summary_log(Progress, SummaryLog, ExtendRun) of
        {ok, Exists, SummaryFd} ->
            R2 = R#rstate{log_fd = SummaryFd, summary_log = SummaryLog},
            HtmlPrio = lux_utils:summary_prio(R2#rstate.html),
            InitialSummary = success,
            InitialSuiteRes =
                initial_res(R2, Exists, ConfigProps,
                            SummaryLog, InitialSummary),
            {R3, Summary, SuiteResults} =
                run_suite(R2, R2#rstate.files, InitialSummary, InitialSuiteRes),
            print_results(R3, Summary, SuiteResults),
            _ = write_results(R3, Summary, SuiteResults),
            SuiteEndTime = lux_utils:now_to_string(lux_utils:timestamp()),
            EndConfig = [{'end time', [string], SuiteEndTime}],
            write_config_log(SummaryLog, ConfigProps ++ EndConfig),
            lux_log:close_summary_log(SummaryFd, SummaryLog),
            maybe_write_junit_report(R3, SummaryLog, ConfigProps),
            annotate_final_summary_log(R3, Summary, HtmlPrio,
                                       SummaryLog, SuiteResults);
        {error, FileReason} ->
            FileErr =
                lux_log:safe_format(undefined,
                                    "ERROR: Failed to open logfile:"
                                    " ~s -> ~s\n",
                                    [SummaryLog,
                                     file:format_error(FileReason)]),
            {run_error, SummaryLog, FileErr}
    end.

maybe_write_junit_report(#rstate{junit = false}, _, _) ->
    ok;
maybe_write_junit_report(#rstate{junit = true}, SummaryLog, ConfigProps) ->
    {run_dir, _, RunDir} = lists:keyfind(run_dir, 1, ConfigProps),
    ok = lux_junit:write_report(SummaryLog, RunDir, []).

initial_res(_R, Exists, _ConfigProps, SummaryLog, _Summary)
  when Exists =:= true ->
    TmpLog = SummaryLog ++ ".tmp",
    flat_parse_summary_log(TmpLog);
initial_res(R, Exists, ConfigProps, SummaryLog, Summary)
  when Exists =:= false, Summary =:= success ->
    ok = write_config_log(SummaryLog, ConfigProps),
    ok = lux_log:write_results(R#rstate.progress, SummaryLog, skip, [], []),
    ok = annotate_tmp_summary_log(R, Summary, undefined),
    [].

flat_parse_summary_log(Log) ->
    DeepLogRes = deep_parse_summary_log(Log),
    FlatLogRes = flatten_summary_results(DeepLogRes),
    FlatLogRes.

deep_parse_summary_log(Log) ->
    WWW = undefined,
    {DeepParseRes, NewWWW} = lux_log:parse_summary_log(Log, WWW),
    lux_utils:stop_app(NewWWW),
    DeepParseRes.

flatten_summary_results({ok, _ResSummary, Cases, _, _, _}) ->
    Init =
        fun(Summary, Script, FullLineNo) ->
                {suite_ok, Summary, Script, FullLineNo,
                 no_shell, no_case_log_dir,
                 [], <<>>, []}
        end,
    Fun =
        fun(Script, {warnings_and_result, _Warnings, Summary}) ->
                case Summary of
                    success ->
                        Init(Summary, Script, "0");
                    skip ->
                        Init(Summary, Script, "0");
                    warning ->
                        Init(Summary, Script, "0");
                    {warning, _FullLineNo, _SN, _ET, _E, _A, _D} ->
                        Init(warning, Script, "0");
                    {error_line, RawLineNo, ReasonBin} ->
                        {suite_error, Script, ?b2l(RawLineNo), ReasonBin};
                    {error, [ReasonBin]} ->
                        case binary:split(ReasonBin, <<": ">>, [global]) of
                            [_, <<"Syntax error at line ", N/binary>>, _] ->
                                {suite_error, Script, ?b2l(N), ReasonBin};
                            _ ->
                                {suite_error, Script, "0", ReasonBin}
                        end;
                    {error, ReasonBin} ->
                        {suite_error, Script, "0", ReasonBin};
                    {fail, RawLineNo, _SN, _ET, _E, _A, _D} ->
                        Init(fail, Script, ?b2l(RawLineNo))
                end
        end,
    [Fun(Script, Res) ||
        {test_case, Script, _Log, _Doc, _HtmlLog, Res} <- Cases];
flatten_summary_results({error, _File, _Reason}) ->
    [].

write_config_log(SummaryLog, ConfigProps) ->
    LogDir = filename:dirname(SummaryLog),
    ConfigLog = filename:join([LogDir, ?SUITE_CONFIG_LOG]),
    ok = lux_log:write_config_log(ConfigLog, ConfigProps).

-spec merge_logs([filename()], filename(), opts()) ->
          ok | error().

merge_logs(Sources, RelTargetDir, Opts) ->
    io:format("Invoke: ~s\n", [string:join(init:get_plain_arguments(), " ")]),
    RelDir = "",
    Cands = lux_utils:summary_log_candidates(),
    DeepLogRes = collect_logs(Sources, RelTargetDir, RelDir, Cands, []),
    case [{Log, flatten_summary_results(Res)} || {Log, Res} <- DeepLogRes] of
        [] ->
            {error, RelTargetDir, file:format_error(enoent)};
        FlatLogRes ->
            ok = do_merge_logs(RelTargetDir, FlatLogRes),
            RelSummaryLog = filename:join([RelTargetDir, ?SUITE_SUMMARY_LOG]),
            AbsSummaryLog = lux_utils:normalize_filename(RelSummaryLog),
            Transform = transform_summary_results(DeepLogRes),
            Res = do_annotate_log(false, AbsSummaryLog, Opts, Transform),
            case Res of
                ok ->
                    RelHtmlFile = RelSummaryLog ++ ".html",
                    {ok, RelHtmlFile};
                {error, File, ReasonStr} ->
                    {error, File, ReasonStr}
            end
    end.

collect_logs([Source | Sources], RelLogDir, RelDir, Cands, Acc) ->
    io:format("\n\t~s", [Source]),
    SourceAcc = do_collect_logs(Source, RelLogDir, RelDir, Cands, []),
    NewAcc = [SourceAcc | Acc],
    collect_logs(Sources, RelLogDir, RelDir, Cands, NewAcc);
collect_logs([], _RelLogDir, _RelDir, _Cands, Acc) ->
    lists:reverse(lists:append(Acc)).

do_collect_logs(RelSource, RelLogDir, RelDir, Cands, Acc)
  when is_list(RelDir) ->
    io:format(".", []),
    Dir0 = lux_utils:join(RelSource, RelDir),
    Dir = lux_utils:normalize_filename(Dir0),
    case file:list_dir(Dir) of
        {ok, Files} ->
            case lux_utils:multi_member(Cands, Files) of
                {true, Base} ->
                    IsLog = lists:suffix(".log", Base),
                    collect_log(IsLog, Dir, Base, Acc);
                false ->
                    %% No interesting file found. Search subdirs
                    Collect =
                        fun("latest_run", A) ->
                                %% Symlink
                                io:format("l", []),
                                A;
                           (Base, A) ->
                                RelDir2 =
                                    case RelDir of
                                        "" -> Base;
                                        _  -> lux_utils:join(RelDir, Base)
                                    end,
                                do_collect_logs(RelSource, RelLogDir,
                                                RelDir2, Cands, A)
                        end,
                    lists:foldl(Collect, Acc, Files)
            end;
        {error, _Reason} ->
            %% Not a dir or problem to read dir
            io:format("e", []),
            Acc
    end.

collect_log(true, Dir, Base, Acc) ->
    %% A summary log
    io:format("p", []),
    LogFile = filename:join([Dir, Base]),
    DeepParseRes = deep_parse_summary_log(LogFile),
    [{LogFile, DeepParseRes} | Acc];
collect_log(false, _Dir, _Base, Acc) ->
    %% Skip
    io:format("s", []),
    Acc.

transform_summary_results(DeepLogRes) ->
    Transform =
        fun({Log, {ok, _Result, Cases, SummaryConfig, _Ctime, _EventLogs}}) ->
                {Log, SummaryConfig, Cases}
        end,
    lists:map(Transform, DeepLogRes).

do_merge_logs(RelTargetDir, FlatLogRes) ->
    ok = filelib:ensure_dir(filename:join([RelTargetDir, "dummy"])),
    merge_summary_logs(RelTargetDir, FlatLogRes, undefined, []),
    merge_tap_logs(RelTargetDir, FlatLogRes, []),
    {RunDir, OrigRunDir} = merge_config_logs(RelTargetDir, FlatLogRes),
    merge_result_logs(RelTargetDir, RunDir, OrigRunDir, FlatLogRes),
    ok.

merge_summary_logs(RelTargetDir, [{SummaryLog, _Res} | Rest], _OldHead, Acc) ->
    {ok, Bin} = file:read_file(SummaryLog),
    [Head | Body] = binary:split(Bin, <<"\n">>, []),
    merge_summary_logs(RelTargetDir, Rest, Head, [Body | Acc]);
merge_summary_logs(_RelTargetDir, [], undefined, []) ->
    ok;
merge_summary_logs(RelTargetDir, [], Head, Acc) ->
    Contents = [Head, "\n" | lists:reverse(Acc)],
    TargetSummaryLog = filename:join([RelTargetDir, ?SUITE_SUMMARY_LOG]),
    ok = file:write_file(TargetSummaryLog, Contents).

merge_tap_logs(RelTargetDir, [{SummaryLog, _Res} | Rest], Acc) ->
    Dir = filename:dirname(SummaryLog),
    TapLog = filename:join([Dir, ?SUITE_TAP_LOG]),
    {ok, Bin} = file:read_file(TapLog),
    merge_tap_logs(RelTargetDir, Rest, [Bin | Acc]);
merge_tap_logs(RelTargetDir, [], Acc) ->
    Contents = lists:join("\n", lists:reverse(Acc)),
    TargetTapLog = filename:join([RelTargetDir, ?SUITE_TAP_LOG]),
    ok = file:write_file(TargetTapLog, Contents).

merge_result_logs(RelTargetDir, RunDir, OrigRunDir, FlatLogRes) ->
    FlatRes = lists:flatten([Res || {_Log, Res} <- FlatLogRes]),
    {ok, Cwd} = file:get_cwd(),
    RebaseScript =
        fun(F) ->
                   F2 = lux_utils:drop_prefix(OrigRunDir, F),
                   F3 = lux_utils:drop_prefix(RunDir, F2),
                   filename:join(Cwd, F3)
        end,
    RebaseSuiteRes =
        fun({suite_ok, Summary, Script, FullLineNo,
             ShellName, CaseLogDir,
             CaseResults, Details, Opaque}) ->
                Script2 = RebaseScript(Script),
                {suite_ok, Summary, Script2, FullLineNo,
                 ShellName, CaseLogDir,
                 CaseResults, Details, Opaque};
           ({suite_error, Script, FullLineNo, Reason}) ->
                Script2 = RebaseScript(Script),
                {suite_error, Script2, FullLineNo, Reason}
        end,
    RebasedFlatRes = lists:map(RebaseSuiteRes, FlatRes),
    Summary = case_summary(FlatRes, success),
    TargetSummaryLog = filename:join([RelTargetDir, ?SUITE_SUMMARY_LOG]),
    Warnings = [],
    lux_log:write_results(silent, TargetSummaryLog, Summary,
                         RebasedFlatRes, Warnings).

case_summary([CaseRes | Rest], OldSummary) ->
    {suite_ok, Summary, _Script, _FullLineNo,
     _ShellName, _CaseLogDir,
     _CaseResults, _Details, _Opaque} = CaseRes,
    NewSummary = lux_utils:summary(OldSummary, Summary),
    case_summary(Rest, NewSummary);
case_summary([], Summary) ->
    Summary.

merge_config_logs(RelTargetDir, [{FirstSummaryLog, _Res} | _Rest]) ->
    TargetConfigLog = filename:join([RelTargetDir, ?SUITE_CONFIG_LOG]),
    Dir = filename:dirname(FirstSummaryLog),
    FirstConfigLog = filename:join([Dir, ?SUITE_CONFIG_LOG]),
    {ok, ConfigBlob} = file:read_file(FirstConfigLog),
    ConfigBins = binary:split(ConfigBlob, <<"\n">>, [global]),
    ConfigProps = lux_log:split_config(ConfigBins),

    RunDir = lux_log:find_config(<<"run_dir">>, ConfigProps, no_config),
    OrigRunDir = lux_log:find_config(<<"orig_run_dir">>, ConfigProps, RunDir),
    RunLogDir = lux_log:find_config(<<"log_dir">>, ConfigProps, RunDir),
    OrigLogDir =
        lux_log:find_config(<<"orig_log_dir">>, ConfigProps, RunLogDir),

    BinAbsTargetDir = ?l2b(lux_utils:normalize_filename(RelTargetDir)),
    NewProps =
        [{<<"run_dir">>, BinAbsTargetDir},
         {<<"orig_run_dir">>, OrigRunDir},
         {<<"log_dir">>, BinAbsTargetDir},
         {<<"orig_log_dir">>, OrigLogDir}],
    Replace =
        fun({_Key, no_config}, CP) ->
                CP;
           ({Key, NewVal}, CP) ->
                lists:keystore(Key, 1, CP, {Key, NewVal})
        end,
    NewConfigProps = lists:foldl(Replace, ConfigProps, NewProps),
    lux_log:write_config_log(TargetConfigLog, NewConfigProps),
    {?b2l(RunDir), ?b2l(OrigRunDir)}.

-spec annotate_log(boolean(), filename(), opts()) ->
          ok | error().

annotate_log(IsRecursive, LogFile, Opts) ->
    do_annotate_log(IsRecursive, LogFile, Opts, []).

do_annotate_log(IsRecursive, LogFile, Opts, Transform) ->
    DefaultDir = filename:dirname(LogFile),
    SuiteLogDir = find_suite_log_dir(DefaultDir, DefaultDir),
    safe_annotate_log(IsRecursive, LogFile, SuiteLogDir, Opts, Transform).

find_suite_log_dir(Dir, DefaultDir) ->
    ConfigLog = filename:join([Dir, ?SUITE_CONFIG_LOG]),
    case filelib:is_regular(ConfigLog) of
        true ->
            Dir;
        false ->
            case filename:dirname(Dir) of
                ParentDir when ParentDir =/= Dir ->
                    find_suite_log_dir(ParentDir, DefaultDir);
                _ ->
                    DefaultDir
            end
    end.

safe_annotate_log(IsRecursive, LogFile, SuiteLogDir, Opts, Transform) ->
    Res = lux_html_annotate:generate(IsRecursive, LogFile, SuiteLogDir,
                                     Opts, Transform),
    case Res of
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
            EventLog = filename:join([CaseLogDir, Base ++ ?CASE_EVENT_LOG]),
            SuiteLogDir = R#rstate.log_dir,
            NoHtmlOpts = lists:keydelete(html, 1, Opts),
            Transform = [],
            Res = safe_annotate_log(false, EventLog, SuiteLogDir,
                                    NoHtmlOpts, Transform),
            case Res of
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
        R#rstate.mode =/= doc      andalso
        R#rstate.mode =/= list     andalso
        R#rstate.mode =/= list_dir andalso
        R#rstate.mode =/= dump     andalso
        R#rstate.mode =/= expand ->
            %% Generate premature html log
            NoHtmlOpts = [{case_prefix, R#rstate.case_prefix},
                          {next_script, NextScript}],
            SummaryLog = R#rstate.summary_log,
            TmpLog = SummaryLog ++ ".tmp",
            ok = file:sync(R#rstate.log_fd), % Flush summary log
            Res = annotate_log(false, TmpLog, NoHtmlOpts),
            case Res of
                ok ->
                    TmpHtml =  TmpLog ++ ".html",
                    SummaryHtml = SummaryLog ++ ".html",
                    ok = file:rename(TmpHtml, SummaryHtml);
                {error, File, Reason} ->
                    {error, File, Reason}
            end;
        true ->
            ok
    end.

annotate_final_summary_log(R, Summary, HtmlPrio, SummaryLog, SuiteResults) ->
    SummaryPrio = lux_utils:summary_prio(Summary),
    if
        SummaryPrio >= HtmlPrio,
        R#rstate.mode =/= doc      andalso
        R#rstate.mode =/= list     andalso
        R#rstate.mode =/= list_dir andalso
        R#rstate.mode =/= dump     andalso
        R#rstate.mode =/= expand ->
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
                    {run_ok, Summary, SummaryLog, SuiteResults};
                {error, File, ReasonStr} ->
                    {run_error, File, ReasonStr}
            end;
        true ->
            {run_ok, Summary, SummaryLog, SuiteResults}
    end.

compute_files(R, LogBase) ->
    if
        R#rstate.files =:= []      andalso
        R#rstate.orig_files =:= [] andalso
        R#rstate.rerun =:= disable ->
            throw_error(undefined, no_input_files);
        R#rstate.rerun =:= disable ->
            R;
        R#rstate.files =/= [] ->
            OldLogDirs = R#rstate.files,
            compute_rerun_files(R, OldLogDirs, LogBase, []);
        R#rstate.prev_log_dir =:= undefined ->
            throw_error(undefined, no_input_files);
        true ->
            compute_rerun_files(R, [R#rstate.prev_log_dir], LogBase, [])
    end.

compute_rerun_files(R, [LogDir|LogDirs], LogBase, Acc) ->
    OldLog = filename:join([LogDir, LogBase]),
    FlatParseRes = flat_parse_summary_log(OldLog),
    Files = filter_rerun_files(R, FlatParseRes),
    if
        R#rstate.mode =/= list andalso
        R#rstate.mode =/= list_dir andalso
        R#rstate.mode =/= doc ->
            io:format("~s~p test cases from ~s\n",
                      [?TAG("rerun log source"),
                       length(Files),
                       lux_utils:drop_prefix(OldLog)]);
        true ->
            ok
    end,
    compute_rerun_files(R, LogDirs, LogBase, Files ++ Acc);
compute_rerun_files(R, [], _LogBase, Acc) ->
    R#rstate{files = lists:usort(Acc)}.

filter_rerun_files(R, InitialRes) ->
    MinCond = lux_utils:summary_prio(R#rstate.rerun),
    Return = fun(Summary, Script) when is_list(Script) ->
                     Cond = lux_utils:summary_prio(Summary),
                     if
                         Cond >= MinCond ->
                             RelScript = lux_utils:drop_prefix(Script),
                             {true, RelScript};
                         true ->
                             false
                     end
             end,
    Filter =
        fun(IR) ->
                case IR of
                    {suite_ok, Summary, Script, _FullLineNo,
                     _ShellName, _CaseLogDir,
                     _CaseResults, _Details, _Opaque} ->
                        Return(Summary, Script);
                    {suite_error, Script, _FullLineNo, _ReasonBin} ->
                        Return(error, Script)
                end
        end,
    lists:zf(Filter, InitialRes).

parse_ropts([{Name, Val} = NameVal | T], R) ->
    case Name of
        %% suite options
        file_pattern when is_list(Val) ->
            parse_ropts(T, R#rstate{file_pattern = Val});
        case_prefix when is_list(Val) ->
            UserArgs = [NameVal | R#rstate.user_args],
            parse_ropts(T, R#rstate{case_prefix = Val,
                                    user_args = UserArgs});
        progress when Val =:= silent  orelse
                      Val =:= summary orelse
                      Val =:= brief   orelse
                      Val =:= doc     orelse
                      Val =:= compact orelse
                      Val =:= verbose orelse
                      Val =:= debug ->
            UserArgs = [NameVal | R#rstate.user_args],
            parse_ropts(T, R#rstate{progress = Val,
                                    user_args = UserArgs});
        config_dir when is_list(Val) ->
            parse_ropts(T, R#rstate{config_dir =
                                        lux_utils:normalize_filename(Val)});
        start_time when tuple_size(Val) =:= 3 ->
            parse_ropts(T, R#rstate{start_time = Val});
        log_dir when is_list(Val) ->
            parse_ropts(T, R#rstate{log_dir = Val});
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
        skip_unstable when Val =:= true; Val =:= false ->
            parse_ropts(T, R#rstate{skip_unstable = Val});
        skip_skip when Val =:= true orelse
                       Val =:= false ->
            parse_ropts(T, R#rstate{skip_skip = Val});
        mode when Val =:= list     orelse
                  Val =:= list_dir orelse
                  Val =:= doc      orelse
                  Val =:= validate orelse
                  Val =:= dump     orelse
                  Val =:= expand   orelse
                  Val =:= execute ->
            parse_ropts(T, R#rstate{mode = Val});
        rerun when Val =:= enable  orelse
                   Val =:= success orelse
                   Val =:= skip    orelse
                   Val =:= warning orelse
                   Val =:= fail    orelse
                   Val =:= error   orelse
                   Val =:= disable ->
            parse_ropts(T, R#rstate{rerun = Val});
        html when Val =:= validate orelse
                  Val =:= enable   orelse
                  Val =:= success  orelse
                  Val =:= skip     orelse
                  Val =:= warning  orelse
                  Val =:= fail     orelse
                  Val =:= error    orelse
                  Val =:= disable ->
            parse_ropts(T, R#rstate{html = Val});
        tap when is_list(Val) ->
            TapOpts = [Val|R#rstate.tap_opts],
            parse_ropts(T, R#rstate{tap_opts = TapOpts});
        junit when Val =:= true orelse
                   Val =:= false ->
            parse_ropts(T, R#rstate{junit = Val});

        %% case options
        _ ->
            UserArgs = [NameVal | R#rstate.user_args],
            parse_ropts(T, R#rstate{user_args = UserArgs})
    end;
parse_ropts([], R) ->
    UserArgs = opts_to_args(lists:reverse(R#rstate.user_args), []),
    Progress =
        case R#rstate.mode of
            M when M =:= list     orelse
                   M =:= list_dir orelse
                   M =:= doc      orelse
                   M =:= dump     orelse
                   M =:= expand ->
                silent;
            _ ->
                R#rstate.progress
        end,
    {ok, R#rstate{user_args = UserArgs,
                  progress = Progress}}.

check_file({Tag, File}, ReRun) ->
    case Tag of
        config_dir when File =:= undefined ->
            ReRun;
        config_dir ->
            case filelib:is_dir(File) of
                true ->
                    ReRun;
                false ->
                    BinErr = ?FF("~p ~s: ~s\n",
                                 [Tag, File, file:format_error(enoent)]),
                    throw_error(File, BinErr)
            end;
        file ->
            case filelib:is_file(File) of
                true ->
                    ReRun;
%%              false when ReRun =/= disable ->
%%                  ReRun;
                false ->
                    BinErr = ?FF("~s: ~s \n",
                                 [File, file:format_error(enoent)]),
                    throw_error(File, BinErr)
            end
    end.

run_cases(R, [{SuiteFile, {error, _FileReason}, _P, _LenP} | Scripts],
          OldSummary, SuiteResults, Max, CC, List, Opaque)
  when R#rstate.rerun =/= disable andalso
       (R#rstate.mode =:= list orelse
        R#rstate.mode =:= list_dir) ->
    %% Assume that the file actually has existed
    Script = SuiteFile,
    run_cases(R, Scripts, OldSummary, SuiteResults,
              Max, CC+1, [Script|List], Opaque);
run_cases(R, [{SuiteFile, {error, FileReason}, _P, _LenP} | Scripts],
          OldSummary, SuiteResults, Max, CC, List, Opaque)
  when R#rstate.rerun =/= disable andalso
       (R#rstate.mode =:= doc) ->
    %% Assume that the file actually has existed
    AbsScript = SuiteFile,
    ReasonStr = file:format_error(FileReason),
    DocCmds = [#cmd{type = doc, arg = [{1, "ERROR: " ++ ReasonStr}]}],
    W = lux_utils:make_warning(AbsScript, "0", ?l2b(ReasonStr)),
    run_case_doc(R, AbsScript, Scripts, DocCmds,
                 OldSummary, [W],
                 SuiteResults, Max, CC, List, Opaque);
run_cases(R, [{SuiteFile, {error=Summary, FileReason}, _P, _LenP} | Scripts],
          OldSummary, SuiteResults, Max, CC, List, Opaque)
  when R#rstate.mode =:= list orelse
       R#rstate.mode =:= list_dir orelse
       R#rstate.mode =:= doc ->
    ReasonStr = file:format_error(FileReason),
    io:format("~s:\n", [lux_utils:drop_prefix(SuiteFile)]),
    io:format("\tERROR ~s\n", [ReasonStr]),
    NewSummary = lux_utils:summary(OldSummary, Summary),
    ListErr = ?l2b(?FF("~s~s: ~s\n",
                       [?TAG("error"), SuiteFile, ReasonStr])),
    NewRes = {suite_error, SuiteFile, "0", ListErr},
    NewSuiteResults = [NewRes | SuiteResults],
    run_cases(R, Scripts, NewSummary, NewSuiteResults, Max, CC+1, List, Opaque);
run_cases(R, [{SuiteFile, {error, FileReason}, P, LenP} | Scripts],
          OldSummary, SuiteResults, Max, CC, List, Opaque) ->
%%   when R#rstate.rerun =/= disable ->
    AbsScript = SuiteFile,
    gen_logs(R, AbsScript, {error, FileReason}, P, LenP, Scripts,
             OldSummary, SuiteResults, Max, CC, List, Opaque);
%% run_cases(R, [{SuiteFile, {error, FileReason}, P, LenP} | Scripts],
%%           OldSummary, SuiteResults, Max, CC, List, Opaque) ->
%%     init_case_rlog(R, P, SuiteFile),
%%     ReasonStr = file:format_error(FileReason),
%%     ListErr =
%%         double_rlog(R, "~s~s: ~s\n",
%%                     [?TAG("error"), SuiteFile, ReasonStr]),
%%     NewRes = {error, SuiteFile, ListErr},
%%     NewSuiteResults = [NewRes | SuiteResults],
%%     ?TRACE_ME(70, suite, 'case', SuiteFile, []),
%%     tap_case_begin(R, SuiteFile),
%%     ?TRACE_ME(70, 'case', suite, error, [FileReason]),
%%     tap_case_end(R, R, CC, SuiteFile, P, LenP, Max, error,
%%                  "0", no_shell, FileReason, FileReason),
%%     run_cases(R, Scripts, OldSummary, NewSuiteResults, Max, CC+1, List, Opaque);
run_cases(OrigR, [{SuiteFile, {ok, AbsScript}, P, LenP} | Scripts],
          OldSummary, SuiteResults, Max, CC, List, Opaque) ->
    RunMode = OrigR#rstate.mode,
    TmpR = OrigR#rstate{warnings = [], file_args = []},
    CaseStartTime = lux_utils:timestamp(),
    case parse_script(TmpR, SuiteFile, AbsScript) of
        {ok, NewR, AbsScript2, Cmds, Opts} ->
            ParseWarnings = NewR#rstate.warnings,
            if
                RunMode =:= list ->
                    run_cases(NewR, Scripts, OldSummary, SuiteResults,
                              Max, CC+1, [AbsScript|List], Opaque);
                RunMode =:= list_dir ->
                    run_cases(NewR, Scripts, OldSummary, SuiteResults,
                              Max, CC+1, [AbsScript|List], Opaque);
                RunMode =:= doc ->
                    DocCmds = extract_doc(AbsScript, Cmds),
                    run_case_doc(NewR, AbsScript, Scripts, DocCmds,
                                 OldSummary, ParseWarnings,
                                 SuiteResults, Max, CC, List, Opaque);
                RunMode =:= validate orelse
                RunMode =:= dump orelse
                RunMode =:= expand ->
                    init_case_rlog(NewR, P, AbsScript),
                    {Summary, NewSummary, NewSuiteResults} =
                        adjust_warnings(AbsScript, success,
                                        ParseWarnings, SuiteResults),
                    double_rlog(NewR, "~s~s\n",
                                [?TAG("result"),
                                 string:to_upper(?a2l(Summary))]),
                    AllWarnings = OrigR#rstate.warnings ++ ParseWarnings,
                    run_cases(NewR#rstate{warnings = AllWarnings},
                              Scripts, NewSummary, NewSuiteResults,
                              Max, CC+1, List, Opaque);
                RunMode =:= execute ->
                    run_execute(OrigR, NewR,
                                [{SuiteFile, AbsScript2, P, LenP} | Scripts],
                                Cmds, ParseWarnings, CaseStartTime, Opts,
                                OldSummary, SuiteResults, Max, CC, List, Opaque)
            end;
        {skip, NewR, _ErrorStack, SkipReason}
          when RunMode =:= list orelse
               RunMode =:= list_dir orelse
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
            %%    XXX = gen_logs(NewR, SuiteFile, {skip, SkipReason)},
            run_cases(NewR2, Scripts, NewSummary, SuiteResults,
                      Max, CC+1, List, Opaque);
        {skip, NewR, ErrorStack, SkipReason} ->
            ?TRACE_ME(70, suite, 'case', P, []),
            tap_case_begin(NewR, AbsScript),
            init_case_rlog(NewR, P, AbsScript),
            double_rlog(NewR, "~s~s\n",
                        [?TAG("result"), SkipReason]),
            #cmd_pos{rev_file = RevScript2} = lists:last(ErrorStack),
            AbsScript2 = lux_utils:pretty_filename(RevScript2),
            {ok, _} = lux_case:copy_orig(NewR#rstate.log_dir, AbsScript2),
            Summary =
                case ?b2l(SkipReason) of
                    "FAIL" ++ _ -> fail;
                    _           -> skip
                end,
            %%            XXX = gen_logs(NewR, SuitFile, {skip, SkipReason)},
            ?TRACE_ME(70, 'case', suite, Summary, [SkipReason]),
            #cmd_pos{lineno = FullLineNo} = stack_error(ErrorStack, SkipReason),
            tap_case_end(OrigR, NewR, CC, AbsScript,
                         P, LenP, Max, Summary,
                         FullLineNo, no_shell,
                         ?b2l(SkipReason), <<>>),
            NewSummary = lux_utils:summary(OldSummary, Summary),
            NewRes = {suite_ok, Summary, AbsScript2, FullLineNo, no_shell,
                      NewR#rstate.log_dir, [], SkipReason, []},
            NewSuiteResults = [NewRes | SuiteResults],
            ParseWarnings = NewR#rstate.warnings,
            AllWarnings = OrigR#rstate.warnings ++ ParseWarnings,
            run_cases(NewR#rstate{warnings = AllWarnings},
                      Scripts, NewSummary, NewSuiteResults,
                      Max, CC+1, List, Opaque);
        {error = Summary, _ErrR, _ErrorStack, _ErrorBin}
          when RunMode =:= list orelse
               RunMode =:= list_dir ->
            %%    XXX = gen_logs(NewR, SuiteFile, {error, FileReason)},
            NewSummary = lux_utils:summary(OldSummary, Summary),
            run_cases(OrigR, Scripts, NewSummary, SuiteResults,
                      Max, CC+1, [AbsScript|List], Opaque);
        {error = Summary, ErrR, ErrorStack, ErrorBin}
          when RunMode =:= doc ->
            %%    XXX = gen_logs(NewR, SuiteFile, {error, FileReason)},
            #cmd_pos{rev_file = RevMainFile,
                     lineno = FullLineNo,
                     type = ErrorBin2} =
                stack_error(ErrorStack, ErrorBin),
            io:format("~s:\n", [lux_utils:drop_prefix(AbsScript)]),
            io:format("\tERROR ~s\n", [ErrorBin]),
            MainFile = lux_utils:pretty_filename(RevMainFile),
            NewRes = {suite_error, MainFile, FullLineNo, ErrorBin2},
            NewSuiteResults = [NewRes | SuiteResults],
            NewWarnings = ErrR#rstate.warnings,
            AllWarnings = OrigR#rstate.warnings ++ NewWarnings,
            NewSummary = lux_utils:summary(OldSummary, Summary),
            run_cases(OrigR#rstate{warnings = AllWarnings},
                      Scripts, NewSummary, NewSuiteResults,
                      Max, CC+1, List, Opaque);
        {error, ErrR, ErrorStack, ErrorBin} ->
            %%    XXX = gen_logs(NewR, SuiteFile, {error, FileReason)},
            #cmd_pos{rev_file = RevMainFile,
                     lineno = FullLineNo,
                     type = ErrorBin2} =
                stack_error(ErrorStack, ErrorBin),
            MainFile = lux_utils:pretty_filename(RevMainFile),
            init_case_rlog(ErrR, P, AbsScript),
            double_rlog(ErrR, "~sERROR ~s\n",
                        [?TAG("result"), ErrorBin2]),
            %% double_rlog2(ErrR, "~s~s\n",
            %%              [?TAG("result"), ErrorBin2],
            %%               "~sERROR as ~s\n",
            %%              [?TAG("result"), ErrorBin2]),
            Summary = error,
            tap_case_begin(ErrR, AbsScript),
            ?TRACE_ME(70, 'case', suite, Summary, []),
            {ok, _} = lux_case:copy_orig(ErrR#rstate.log_dir, MainFile),
            tap_case_end(OrigR, ErrR, CC, AbsScript,
                         P, LenP, Max, Summary,
                         "0", no_shell, ErrorBin, ErrorBin),
            NewWarnings = ErrR#rstate.warnings,
            AllWarnings = OrigR#rstate.warnings ++ NewWarnings,
            NewSummary = lux_utils:summary(OldSummary, Summary),
            NewRes = {suite_error, MainFile, FullLineNo, ErrorBin2},
            NewSuiteResults = [NewRes | SuiteResults],
            run_cases(OrigR#rstate{warnings = AllWarnings},
                      Scripts, NewSummary, NewSuiteResults,
                      Max, CC+1, List, Opaque)
    end;
run_cases(R, [], Summary, SuiteResults, _Max, _CC, List, _Opaque) ->
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
    {R, Summary, lists:reverse(SuiteResults)}.

adjust_warnings(Script, OldSummary, ParseWarnings, Results) ->
    case ParseWarnings of
        [] ->
            Summary = OldSummary,
            NewSummary = OldSummary,
            NewResults = Results;
        _ ->
            Summary = warning,
            NewSummary = lux_utils:summary(OldSummary, Summary),
            NewResults = [{Summary, Script, ParseWarnings} | Results]
    end,
    {Summary, NewSummary, NewResults}.

run_case_doc(R, AbsScript, Scripts, DocCmds, OldSummary, NewWarnings,
             SuiteResults, Max, CC, List, Opaque) ->
    RelScript = lux_utils:drop_prefix(AbsScript),
    io:format("~s:\n", [RelScript]),
    Docs = [Doc || #cmd{arg = MultiDoc} <- DocCmds,
                   Doc <- MultiDoc],
    MaxLevel = pick_val(doc, R, infinity),
    lists:foldl(fun display_doc/2, MaxLevel, Docs),
    {_Summary, NewSummary, NewSuiteResults} =
        adjust_warnings(AbsScript, OldSummary,
                        NewWarnings, SuiteResults),
    AllWarnings = R#rstate.warnings ++ NewWarnings,
    run_cases(R#rstate{warnings = AllWarnings},
              Scripts, NewSummary, NewSuiteResults,
              Max, CC+1, List, Opaque).

run_execute(OrigR, NewR,
            [{_SuiteFile, AbsScript, P, LenP} | Scripts],
            Cmds, ParseWarnings, CaseStartTime, Opts,
            OldSummary, SuiteResults, Max, CC, List, Opaque) ->
    ok = annotate_tmp_summary_log(NewR, OldSummary, AbsScript),
    ?TRACE_ME(70, suite, 'case', P, []),
    tap_case_begin(NewR, AbsScript),
    init_case_rlog(NewR, P, AbsScript),
    SuiteRef = NewR#rstate.suite_timer_ref,
    PostCaseCmds = NewR#rstate.post_case_cmds,
    CaseRes =
        lux_case:interpret_commands(AbsScript, Cmds,
                                    ParseWarnings,
                                    CaseStartTime,
                                    SuiteRef,
                                    PostCaseCmds,
                                    Opts, Opaque),
    SkipReason = "",
    case CaseRes of
        {case_ok, Summary, _, FullLineNo,
         ShellName, CaseLogDir,
         RunWarnings, UnstableWarnings,
         CaseResults, Details, NewOpaque} ->
            NewRes =
                {suite_ok, Summary, AbsScript, FullLineNo,
                 ShellName, CaseLogDir,
                 CaseResults, Details, Opaque},
            NewScripts = Scripts;
        {case_error, MainFile, FullLineNo, CaseLogDir,
         RunWarnings, UnstableWarnings, ReasonBin} ->
            Summary = error,
            NewOpaque = Opaque,
            ShellName = no_shell,
            NewRes =
                {suite_error, MainFile,
                 FullLineNo, ReasonBin},
            Details = ReasonBin,
            NewScripts =
                case ReasonBin of
                    <<"suite_timeout" >> -> [];
                    _                    -> Scripts
                end
    end,
    ?TRACE_ME(70, 'case', suite, Summary,
              [{result, NewRes},
               {run_warnings, RunWarnings},
               {unstable_warnings, UnstableWarnings}]),
    AllWarnings = OrigR#rstate.warnings ++
        RunWarnings ++ UnstableWarnings,
    NewR2 = NewR#rstate{warnings = AllWarnings},
    tap_case_end(OrigR, NewR2, CC, AbsScript,
                 P, LenP, Max, Summary,
                 FullLineNo, ShellName, SkipReason, Details),
    NewSummary = lux_utils:summary(OldSummary, Summary),
    annotate_event_log(NewR2, AbsScript, NewSummary,
                       CaseLogDir, Opts),
    NewSuiteResults = [NewRes | SuiteResults],
    _ = write_results(NewR2, NewSummary, NewSuiteResults),
    run_cases(NewR2, NewScripts, NewSummary, NewSuiteResults,
              Max, CC+1, List, NewOpaque).

%% gen_logs(R, Script, {skip, ReasonStr}) ->
%%     b();
gen_logs(R, AbsScript, {error, FileReason}, P, LenP, Scripts,
         OldSummary, SuiteResults, Max, CC, List, Opaque) ->
    ReasonStr = file:format_error(FileReason),
    W = lux_utils:make_warning(AbsScript, "0", ?l2b(ReasonStr)),
    ParseWarnings = [W],
    CaseStartTime = lux_utils:timestamp(),
    OrigR = R,
    NewR = R,
    SuiteFile = AbsScript,
    Cmds = [],
    Opts = [],
    run_execute(OrigR, NewR,
                [{SuiteFile, AbsScript, P, LenP} | Scripts],
                Cmds, ParseWarnings, CaseStartTime, Opts,
                OldSummary, SuiteResults, Max, CC, List, Opaque).

extract_doc(File, Cmds) ->
    Fun = fun(Cmd, _RevFile, _PosStack, Acc) ->
                  case Cmd of
                      #cmd{type = doc} ->
                          [Cmd | Acc];
                      _ ->
                          Acc
                  end
          end,
    lists:reverse(lux_utils:foldl_cmds(Fun, [], File, [], Cmds)).

display_doc({Level, Doc}, MaxLevel) ->
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
    end.

write_results(#rstate{mode=Mode, summary_log=SummaryLog},
              Summary, SuiteResults)
  when Mode =:= list     orelse
       Mode =:= list_dir orelse
       Mode =:= doc      orelse
       Mode =:= dump     orelse
       Mode =:= expand ->
    {run_ok, Summary, SummaryLog, SuiteResults};
write_results(#rstate{progress=Progress,
                      summary_log=SummaryLog,
                      warnings=Warnings},
              Summary, SuiteResults) when is_list(SummaryLog) ->
    lux_log:write_results(Progress, SummaryLog, Summary,
                          SuiteResults, Warnings),
    {run_ok, Summary, SummaryLog, SuiteResults}.

print_results(#rstate{progress=Progress,warnings=Warnings},
              Summary, SuiteResults) ->
    lux_log:print_results(Progress, {false,standard_io},
                          Summary, SuiteResults, Warnings).

parse_script(R, _SuiteFile, Script) ->
    Opts0 = args_to_opts(lists:reverse(case_config_args(R)), case_style, []),
    CheckDoc = false, % Ignore missing summary doc warning for the time being
    case lux_parse:parse_file(Script,
                              R#rstate.mode,
                              R#rstate.skip_unstable,
                              R#rstate.skip_skip,
                              CheckDoc,
                              Opts0) of
        {ok, Script2, Cmds, FileOpts, NewWarnings} ->
            FileArgs = opts_to_args(FileOpts, R#rstate.file_args),
            R2 = R#rstate{internal_args = [],
                          file_args = FileArgs,
                          warnings = NewWarnings},
            LogDir = R#rstate.log_dir,
            LogFd = R#rstate.log_fd,
            LogFun = fun(Bin) -> lux_log:safe_write(LogFd, Bin) end,
            InternalArgs = [{log_dir, LogDir},
                            {log_fun, LogFun},
                            {log_fd,  LogFd},
                            {skip_skip, R#rstate.skip_skip}],
            R3 = R2#rstate{internal_args = InternalArgs},
            MergeFun =
                fun(A, Acc) ->
                        O = args_to_opts(A, case_style, []),
                        opts_to_args(O, Acc)
                end,
            Args = lists:foldl(MergeFun, [], lists:reverse(args_dicts(R3))),
            Opts = args_to_opts(Args, case_style, []),
            {ok, R3, Script2, Cmds, Opts};
        {skip, ErrorStack, ErrorBin} ->
            {skip, R, ErrorStack, ErrorBin};
        {error, ErrorStack, ErrorBin} ->
            {error, R, ErrorStack, ErrorBin}
    end.

parse_post_case_cmds(R) ->
    {ok, Cwd} = file:get_cwd(),
    Opts = args_to_opts(lists:reverse(case_config_args(R)), case_style, []),
    PostCase = proplists:lookup_all(post_case, Opts),
    CheckDoc = false, % Ignore missing summary doc warning for the time being
    Prefix = "post_case",
    FirstLineNo = 1,
    DefaultCmd = #cmd{type = comment,
                      lineno = 0,
                      orig = <<>>},
    NoCleanupCmd = DefaultCmd#cmd{type = no_cleanup},
    Fun =
        fun({post_case, RelFile}, N) ->
                NextN = N + 1,
                PostName =
                    case N of
                        1 -> Prefix;
                        _ -> lists:concat([Prefix, N])
                    end,
                AbsFile = filename:absname(RelFile, Cwd),
                AbsFile2 = lux_utils:normalize_filename(AbsFile),
                case lux_parse:parse_file(AbsFile2,
                                          R#rstate.mode,
                                          R#rstate.skip_unstable,
                                          R#rstate.skip_skip,
                                          CheckDoc,
                                          Opts) of
                    {ok, PostScript, InclCmds, FileOpts, NewWarnings} ->
                        LastCmd =
                            case InclCmds of
                                [] -> DefaultCmd;
                                _  -> lists:last(InclCmds)
                            end,
                        LastLineNo = LastCmd#cmd.lineno,
                        PostCmd = DefaultCmd#cmd{type = post_case,
                                                 arg = {PostName, PostScript}},
                        InclCmd = DefaultCmd#cmd{type = include,
                                                 arg = {include, PostScript,
                                                        FirstLineNo, LastLineNo,
                                                        InclCmds}},
                        PostCmds = [PostCmd, InclCmd, NoCleanupCmd],
                        PCC = #post_case_cmd{name = PostName,
                                             script = PostScript,
                                             commands = PostCmds,
                                             file_opts = FileOpts,
                                             warnings = NewWarnings},
                        {PCC, NextN};
                    {skip, _ErrorStack, SkipBin} ->
                        PostScript = AbsFile2,
                        W = lux_utils:make_warning(PostScript, "0", SkipBin),
                        PCC = #post_case_cmd{name = PostName,
                                             script = PostScript,
                                             commands = [],
                                             file_opts = [],
                                             warnings = [W]},
                        {PCC, NextN};
                    {error, ErrorStack, ErrorBin} ->
                        #cmd_pos{rev_file = RevMainFile,
                                 type = ErrorBin2} =
                            stack_error(ErrorStack, ErrorBin),
                        MainFile =
                            lux_utils:pretty_filename(RevMainFile),
                        throw_error(MainFile, ErrorBin2)
                end
        end,
    {PostCaseCmds, _N} = lists:mapfoldl(Fun, 1, PostCase),
    PostCaseCmds.

parse_config(R) ->
    %% Default opts
    DefaultBase = "luxcfg",
    PrivDir = filename:join(code:lib_dir(?APPLICATION), "priv"),
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
    check_file({config_dir, AbsConfigDir}, R2#rstate.rerun),

    %% Common opts
    AbsCommonFile = filename:join([AbsConfigDir, DefaultBase]),
    case filelib:is_regular(AbsCommonFile) of
        true ->
            {CommonOpts, CommonWarnings} = parse_config_file(R2, AbsCommonFile),
            CommonArgs = opts_to_args(CommonOpts, []),
            CommonData = [{'common file', [string], AbsCommonFile}] ++
                CommonArgs;
        false ->
            CommonArgs = [],
            CommonData = [],
            CommonWarnings = []
    end,
    R3 = R2#rstate{common_args = CommonArgs},

    %% Arch spec opts
    ActualConfigName = config_name(),
    DefaultData =
        builtins(R, ActualConfigName) ++
        [{'default file', [string], DefaultFile}] ++ DefaultArgs ++ CommonData,
    {ConfigName, AbsConfigFile} =
        config_file(R3, AbsConfigDir, R2#rstate.config_name, ActualConfigName),
    if
        AbsConfigFile =/= DefaultFile ->
            {ConfigOpts, ConfigWarnings} = parse_config_file(R2, AbsConfigFile),
            ConfigArgs = opts_to_args(ConfigOpts, []),
            ConfigProps = [{'config file', [string], AbsConfigFile}] ++
                ConfigArgs;
        true ->
            ConfigArgs = [],
            ConfigProps = [],
            ConfigWarnings = []
        end,
    NewWarnings = DefaultWarnings ++ CommonWarnings ++ ConfigWarnings,
    AllWarnings = R#rstate.warnings ++ NewWarnings,
    R4 = R3#rstate{config_name = ConfigName,
                   config_dir  = AbsConfigDir,
                   config_file = AbsConfigFile,
                   config_args = ConfigArgs,
                   warnings    = AllWarnings},
    {DefaultData ++ ConfigProps, R4}.

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
     {case_prefix, [string], R#rstate.case_prefix},
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
    SkipUnstable = false,
    SkipSkip = true,
    CheckDoc = false,
    Opts0 = args_to_opts(lists:reverse(case_config_args(R)), case_style, []),
    case lux_parse:parse_file(AbsConfigFile,
                              R#rstate.mode,
                              SkipUnstable,
                              SkipSkip,
                              CheckDoc,
                              Opts0) of
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
                    #cmd_pos{rev_file = RevMainFile,
                             type = ErrorBin2} =
                        stack_error(ErrorStack, ErrorBin),
                    MainFile = lux_utils:pretty_filename(RevMainFile),
                    throw_error(MainFile, ErrorBin2)
            end
    end.

case_config_args(R) ->
    [{Key, Val} || {Key, Val} <- all_config_args(R),
                   is_case_config_type(Key)].

all_config_args(R) ->
    lists:append(lists:reverse(args_dicts(R))).

stack_error(ErrorStack, ErrorBin) ->
    #cmd_pos{rev_file = RevMainFile,  name = MainName} = lists:last(ErrorStack),
    #cmd_pos{rev_file = RevErrorFile, name = ErrorName} = hd(ErrorStack),
    FullLineNo = lux_utils:pretty_full_lineno(ErrorStack),
    if
        RevErrorFile =:= RevMainFile ->
            lux_utils:cmd_pos(RevMainFile, FullLineNo, ErrorBin, MainName);
        true ->
            ErrorFile = lux_utils:pretty_filename(RevErrorFile),
            FileBin = ?l2b(ErrorFile),
            ErrorBin2 = <<FileBin/binary, ": ", ErrorBin/binary>>,
            lux_utils:cmd_pos(RevMainFile, FullLineNo, ErrorBin2, ErrorName)
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

double_rlog(#rstate{progress = Progress, log_fd = Fd}, Format, Args) ->
    IoList = ?FF(Format, Args),
    case Fd of
        undefined -> ?l2b(IoList);
        _         -> lux_log:double_write(Progress, Fd, IoList)
    end.

%% double_rlog2(#rstate{progress = Progress, log_fd = Fd},
%%              ResFormat, ResArgs, ConFormat, ConArgs) ->
%%     ResIoList = ?FF(ResFormat, ResArgs),
%%     case Fd of
%%         undefined ->
%%             ?l2b(ResIoList);
%%         _ ->
%%             ConIoList = ?FF(ConFormat, ConArgs),
%%             lux_log:double_write(Progress, Fd, {ResIoList, ConIoList})
%%     end.

init_case_rlog(#rstate{progress = Progress, log_fd = Fd},
               RelScript, AbsScript) ->
    Tag = ?TAG("test case"),
    AbsIoList = ?FF("\n~s~s\n", [Tag, AbsScript]),
    case Fd of
        undefined ->
            ?l2b(AbsIoList);
        _ ->
            AbsBin = lux_log:safe_write(Fd, AbsIoList),
            case Progress of
                silent ->
                    ok;
                _ ->
                    RelIoList = ?FF("\n~s~s\n", [Tag, RelScript]),
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
    Multiplier = pick_val(multiplier, R, ?ONE_SEC),
    lux_utils:send_after(SuiteTimeout, Multiplier, self(), Msg).

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
                   common_args = M,
                   default_args = D}) ->
    [I, U, F, C, M, D].

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
        skip_unstable ->
            {ok, [{atom, [true, false]}]};
        skip_skip ->
            {ok, [{atom, [true, false]}]};
        mode ->
            {ok, [{atom, [list, list_dir, doc,
                          validate, dump, expand, execute]}]};
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
        history_cases ->
            {ok, [{atom, [latest, any]}]};
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
  when R#rstate.mode =/= list     andalso
       R#rstate.mode =/= list_dir andalso
       R#rstate.mode =/= doc      andalso
       R#rstate.mode =/= dump     andalso
       R#rstate.mode =/= expand ->
    TapLog = filename:join([R#rstate.log_dir, ?SUITE_TAP_LOG]),
    TapOpts = [TapLog | R#rstate.tap_opts],
    case lux_tap:open(TapOpts) of
        {ok, TAP} ->
            ok = lux_tap:plan(TAP, length(Scripts), Directive),
            ok = lux_tap:diag(TAP, "\n"),
            %% ok = lux_tap:diag(TAP, "LUX - LUcid eXpect scripting"),
            OptUser = lux_utils:user_prefix(),
            Host = lux_utils:real_hostname(),
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

tap_suite_end(#rstate{tap = undefined}, _Summary, _Results) ->
    ok;
tap_suite_end(#rstate{tap = TAP, warnings = Warnings}, Summary, Results) ->
    Len = fun(Res, Tag) -> ?i2l(length(lux_log:pick_result(Res, Tag))) end,
    ok = lux_tap:diag(TAP, "\n"),
    lux_tap:diag(TAP, ["Errors:     ", Len(Results,  error)]),
    lux_tap:diag(TAP, ["Failed:     ", Len(Results,  fail)]),
    lux_tap:diag(TAP, ["Warnings:   ", Len(Warnings, warning)]),
    lux_tap:diag(TAP, ["Skipped:    ", Len(Results,  skip)]),
    lux_tap:diag(TAP, ["Successful: ", Len(Results,  success)]),
    lux_tap:diag(TAP, ["Summary:    ", ?a2l(Summary)]),
    lux_tap:close(TAP).

tap_case_begin(#rstate{}, _AbsScript) ->
    ok.

tap_case_end(#rstate{},
             #rstate{tap = undefined}, _CaseCount, _Script,
             _P, _LenP, _Max,
             _Result, _FullLineNo, _ShellName,
             _Reason, _Details) ->
    ok;
tap_case_end(#rstate{warnings = OrigWarnings},
             #rstate{tap = TAP, skip_skip = SkipSkip, warnings = Warnings},
             CaseCount, _AbsScript,
             P, LenP, Max,
             Result, FullLineNo, ShellName, Reason, Details) ->
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
            skip                  -> {ok,     ""};
            success when SkipSkip -> {ok,     TodoReason};
            success               -> {ok,     ""}
        end,
    lux_tap:test(TAP, Outcome, Descr, Directive, Max-LenP),
    NewWarnings = Warnings -- OrigWarnings,
    TapComment =
        fun(#warning{file=F, lineno=L, reason=R}) ->
                tap_comment(TAP, prep, warning, F,
                            L, no_shell, R, [])
        end,
    lists:foreach(TapComment, NewWarnings),
    tap_comment(TAP, final, Result, no_file,
                FullLineNo, ShellName,
                Details, Reason).

tap_comment(TAP, Context, Outcome, _File,
            FullLineNo, OptShellName,
            Details, Reason) ->
    MakePrefix =
        fun(O) ->
                ?b2l(?l2b([string:to_upper(?a2l(O)),
                           " at line ", FullLineNo]))
        end,
    case binary:split(Details, <<"\n">>, [global]) of
        [<<>>] when Reason =/= "" ->
            Prefix = MakePrefix(Outcome),
            ok = lux_tap:diag(TAP, Prefix ++ " - " ++ Reason);
        [<<>>] ->
            ignore;
        [Single] ->
            Prefix = MakePrefix(Outcome),
            ok = lux_tap:diag(TAP, Prefix ++ " - " ++ ?b2l(Single));
        Multiline when Context =:= final ->
            Suffix =
                case OptShellName of
                    no_shell  -> "";
                    ShellName -> " in shell " ++ ShellName
                end,
            NewOutcome =
                case Outcome of
                    warning -> fail;
                    _       -> Outcome
                end,
            Prefix = MakePrefix(NewOutcome),
            ok = lux_tap:diag(TAP, Prefix ++ Suffix),
            [ok = lux_tap:diag(TAP, ?b2l(D)) || D <- Multiline]
    end.

throw_error(File, Reason) ->
    throw({error, File, Reason}).
