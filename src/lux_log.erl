%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2021 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_log).

-export([
         is_temporary/1, parse_summary_log/2, parse_run_summary/4,
         default_run/2,
         open_summary_log/3, close_summary_tmp_log/1, close_summary_log/2,
         write_config_log/2, split_config/1, find_config/3,
         write_results/5, print_results/5, parse_result/1, pick_result/2,
         safe_format/3, safe_write/2, double_write/3,
         open_event_log/6, close_event_log/1, write_events/7, scan_events/2,
         parse_events/2, parse_io_logs/2, open_config_log/3, close_config_log/2,
         extract_timers/1, timers_to_csv/1, csv_to_timers/1,
         safe_format/5, safe_write/4, unquote/1, dequote/1, split_quoted_lines/1
        ]).

-include_lib("kernel/include/file.hrl").
-include("lux.hrl").

-define(SUMMARY_LOG_VERSION, <<"0.3">>).
-define(EVENT_LOG_VERSION,   <<"0.8">>).
-define(CONFIG_LOG_VERSION,  <<"0.1">>).
-define(RESULT_LOG_VERSION,  <<"0.1">>).

-define(SUMMARY_TAG, <<"summary log">>).
-define(EVENT_TAG,   <<"event log">>).
-define(CONFIG_TAG,  <<"config log">>).
-define(RESULT_TAG,  <<"result log">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Summary log

is_temporary(SummaryLog) when is_list(SummaryLog) ->
    lists:suffix(".tmp", SummaryLog).

open_summary_log(Progress, SummaryLog, ExtendRun) when is_list(SummaryLog) ->
    TmpSummaryLog = SummaryLog ++ ".tmp",
    {WriteMode, Exists} =
        case ExtendRun of
            true ->
                case file:rename(SummaryLog, TmpSummaryLog) of
                    ok ->
                        %% Extend old run
                        {append, true};
                    {error, enoent} ->
                        %% New run
                        {append, false}
                end;
            false ->
                %% New run
                {write, filelib:is_regular(SummaryLog)}
        end,
    if
        WriteMode =:= write, Exists =:= true ->
            {error, eexist};
        true ->
            case file:open(TmpSummaryLog, [WriteMode]) of
                {ok, SummaryFd} ->
                    LogIoList = ?FF("~s~s\n",
                                    [?TAG(?SUMMARY_TAG),
                                     ?SUMMARY_LOG_VERSION]),
                    safe_write(SummaryFd, LogIoList),
                    case Progress of
                        silent ->
                            ok;
                        _ ->
                            StdoutIoList =
                                ?FF("~s~s\n",
                                    [?TAG(?SUMMARY_TAG),
                                     SummaryLog]),
                            safe_write(undefined, StdoutIoList)
                    end,
                    {ok, Exists, SummaryFd};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

close_summary_log(SummaryFd, SummaryLog) when is_list(SummaryLog) ->
    ok = close_summary_tmp_log(SummaryFd),
    TmpSummaryLog = SummaryLog ++ ".tmp",
    ok = file:rename(TmpSummaryLog, SummaryLog).

close_summary_tmp_log(SummaryFd) ->
    file:close(SummaryFd).

parse_summary_log(SummaryLog, WWW) when is_list(SummaryLog) ->
    Source = #source{branch=undefined,
                     suite_prefix=undefined,
                     file=?l2b(SummaryLog),
                     orig=?l2b(SummaryLog)},
    parse_summary_log(Source, WWW);
parse_summary_log(#source{file=SummaryLog} = Source, WWW)
  when is_binary(SummaryLog)->
    try
        try_parse_summary_log(Source, WWW)
    catch
        ?CATCH_STACKTRACE(error, Reason, EST)
            ReasonStr =
                lists:flatten(?FF("\nINTERNAL LUX ERROR"
                                  " in ~s\n~p\n\~p\n",
                                  [SummaryLog, Reason, EST])),
            io:format("~s\n", [ReasonStr]),
            {{error, SummaryLog, ReasonStr}, WWW}
    end.

try_parse_summary_log(#source{file=SummaryLogBin}, WWW) ->
    SummaryLog = ?b2l(SummaryLogBin),
    {ReadRes, NewWWW} = read_log(SummaryLog, ?SUMMARY_TAG, WWW),
    Res =
        case ReadRes of
            {ok, ?SUMMARY_LOG_VERSION, Sections} ->
                %% Latest version
                do_parse_summary_log(SummaryLog, Sections, NewWWW);
            {ok, <<"0.2">>, Sections} ->
                %% Prev version without warnings
                do_parse_summary_log(SummaryLog, Sections, NewWWW);
            {ok, Version, _Sections} ->
                {error, SummaryLog,
                 "Illegal summary log version: " ++ ?b2l(Version)};
            {error, FileReason, _} ->
                {error, SummaryLog, FileReason}
        end,
    {Res, NewWWW}.

do_parse_summary_log(SummaryLog, Sections, NewWWW) ->
    LogDir = filename:dirname(SummaryLog),
    ConfigLog = lux_utils:join(LogDir, ?SUITE_CONFIG_LOG),
    {{ok, RawConfig}, NewWWW} = scan_config(ConfigLog, NewWWW),
    SummaryConfig = parse_config(RawConfig),
    {{ok, Result}, NewWWW} = parse_summary_result(LogDir, NewWWW),
    {Cases, EventLogs} = split_cases(Sections, [], []),
    Ctime =
        case lux_utils:is_url(SummaryLog) of
            true ->
                <<"remote">>;
            false ->
                {ok, FI} = file:read_file_info(SummaryLog),
                Ctime0 = FI#file_info.ctime,
                ?l2b(lux_utils:datetime_to_string(Ctime0))
        end,
    {ok,
     Result,
     [{test_group, "", Cases}], SummaryConfig, Ctime, EventLogs}.

read_log(Log, ExpectedTag, WWW) when is_list(Log) ->
    {FetchRes, NewWWW} = fetch_log(Log, WWW),
    ReadRes =
        case FetchRes of
            {ok, Bin} ->
                [Head|Sections] = binary:split(Bin, <<"\n\n">>, [global]),
                case binary:split(Head, <<": ">>) of
                    [PaddedTag, Version] ->
                        Tag = lux_utils:strip_trailing_whitespaces(PaddedTag),
                        if
                            Tag =:= ExpectedTag ->
                                case lists:reverse(Sections) of
                                    [] ->
                                        Sections2 = [],
                                        [Version2|_] = % Chop potential newline
                                            binary:split(Version, <<"\n">>);
                                    [<<>> | Rev] ->
                                        Sections2 = lists:reverse(Rev),
                                        Version2 = Version;
                                    _ ->
                                        Sections2 = Sections,
                                        Version2 = Version
                                end,
                                {ok, Version2, Sections2};
                            true ->
                                Reason =
                                    "Illegal log type: " ++
                                    ?b2l(ExpectedTag) ++
                                    " expected",
                                {error, Reason, Bin}
                        end;
                    _ ->
                        Reason =
                            "Illegal log type: " ++
                            ?b2l(ExpectedTag) ++
                            " expected",
                        {error, Reason, Bin}
                end;
            {error, FileReason} ->
                {error, file:format_error(FileReason), <<>>};
            {error, Reason, X} ->
                {error, Reason, X}
        end,
    {ReadRes, NewWWW}.

fetch_log(Log, undefined) ->
    case lux_utils:start_app(inets) of
        {true, StopFun} ->
            fetch_log(Log, {0, StopFun});
        {false, _StopFun} ->
            fetch_log(Log, false)
    end;
fetch_log(Log, false = WWW) ->
    {file:read_file(Log), WWW};
fetch_log(Log, {N, StopFun} = WWW) when is_function(StopFun, 0) ->
    case lux_utils:is_url(Log) of
        true ->
            io:format(":", []),
            Res =

                case httpc:request(Log) of
                    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
                        {ok, ?l2b(Body)};
                    {ok, {{_Version, _Code, ReasonPhrase}, _Headers, _Body}} ->
                        {error, ReasonPhrase, <<>>};
                    {error, Reason} ->
                        String = lists:flatten(?FF("~p", [Reason])),
                        {error, String, <<>>}
                end,
            {Res, {N+1,StopFun}};
        false ->
            Res = file:read_file(Log),
            {Res, WWW}
   end.

write_log(File, Tag, Version, Sections) when is_list(File) ->
    file:write_file(File, [?TAG(Tag), Version, [["\n\n",S] || S <- Sections]]).

split_result([Result]) ->
    Lines = binary:split(Result, <<"\n">>, [global]),
    [_, Summary | Rest] = lists:reverse(Lines),
    [_, Summary2] = binary:split(Summary, <<": ">>),
    Lines2 = lists:reverse(Rest),
    Sections = split_result2(Lines2, []),
    {result_summary, Summary2, Sections}.

split_result2([Heading | Lines], Acc) ->
    [Slogan, Count] = binary:split(Heading, <<": ">>),
    [Slogan2, _] = binary:split(Slogan, <<" ">>),
    Pred = fun(Line) ->
                   case Line of
                       <<"\t", _File/binary>> -> true;
                       _ -> false
                   end
           end,
    {Files, Lines2} = lists:splitwith(Pred, Lines),
    Parse = fun(<<"\t", File/binary>>) ->
                    [File2, LineNo] = binary:split(File, <<":">>),
                    {file_lineno, File2, LineNo}
            end,
    Files2 = lists:map(Parse, Files),
    split_result2(Lines2, [{section, Slogan2, Count, Files2} | Acc]);
split_result2([], Acc) ->
    Acc. % Return in reverse order (most important first)

split_cases([Case | Cases], Acc, EventLogs) ->
    [NameRow | Sections] = binary:split(Case, <<"\n">>, [global]),
    Name =
        case binary:split(NameRow, <<": ">>) of
            [<<"test case", _/binary>>, NameBin] -> ?b2l(NameBin);
            [<<>>]                               -> ?DEFAULT_CASE
        end,
    case Sections of
        [] ->
            Res = {result_case, Name, <<"ERROR">>, ?DEFAULT_CASE},
            split_cases(Cases, [Res | Acc], EventLogs);
        [ScriptRow, LogRow | DocAndResult] when LogRow =/= <<>> ->
            case {binary:split(LogRow,  <<": ">>), ScriptRow} of
                {[<<"event log", _/binary>>, RawEventLog], _} ->
                    {Doc, ResultCase} = split_doc(DocAndResult, []),
                    Result = parse_result(ResultCase),
                    EventLog = ?b2l(RawEventLog),
                    HtmlLog = EventLog ++ ".html",
                    Res = {test_case, Name, EventLog, Doc, HtmlLog, Result},
                    split_cases(Cases, [Res | Acc], [EventLog|EventLogs]);
                {_, R = <<"result", _/binary>>} ->
                    Result = parse_result([R]),
                    Res = {test_case, Name, "", [], "", Result},
                    split_cases(Cases, [Res | Acc], EventLogs)
            end;
        [Reason|_] ->
            Res =
                case binary:split(Reason, <<": ">>) of
                    [<<"result", _/binary>>, Reason2] when Reason2 =/= <<>> ->
                        {result_case, Name, Reason2, Reason};
                    [<<"error", _/binary>>, Reason2] ->
                        {result_case, Name, <<"ERROR">>, Reason2};
                    [<<>>] ->
                        {result_case, Name, <<"ERROR">>, ?DEFAULT_CASE}
                end,
            split_cases(Cases, [Res | Acc], EventLogs)
    end;
split_cases([], Acc, EventLogs) ->
    {lists:reverse(Acc), EventLogs}.

split_doc([H|T] = Rest, AccDoc) ->
    case binary:split(H, <<": ">>) of
        [<<"doc", _/binary>>, Doc] ->
            split_doc(T, [Doc | AccDoc]);
        _ ->
            {lists:reverse(AccDoc), Rest}
    end.

parse_run_summary(Source, SummaryLog, Res, Opts) when is_list(SummaryLog) ->
    %% File is relative cwd
    try
        do_parse_run_summary(Source, SummaryLog, Res, Opts)
    catch
        ?CATCH_STACKTRACE(error, Reason, EST)
            ReasonStr =
                lists:flatten(?FF("\nINTERNAL LUX ERROR"
                                  " in ~s\n~p\n\~p\n",
                                  [SummaryLog, Reason, EST])),
            io:format("~s\n", [ReasonStr]),
            {error, SummaryLog, ReasonStr}
    end.

do_parse_run_summary(Source, SummaryLog, Res, Opts) ->
    R = default_run(Source, SummaryLog),
    case Res of
        {ok, Result, Groups, SummaryConfig, Ctime, _EventLogs} ->
            ConfigBins = binary:split(SummaryConfig, <<"\n">>, [global]),
            ConfigProps = split_config(ConfigBins),
            StartTime = find_config(<<"start time">>, ConfigProps, Ctime),
            Branch0 = Source#source.branch,
            case Branch0 of
                undefined ->
                    Branch = find_config(<<"branch">>, ConfigProps, Branch0);
                Branch ->
                    ok
            end,
            case lux_utils:pick_opt(hostname, Opts, undefined) of
                undefined ->
                    HostName = find_config(<<"hostname">>, ConfigProps,
                                           R#run.hostname);
                HostName ->
                    ok
            end,
            CN0 = R#run.config_name,
            ConfigName0 = find_config(<<"config name">>, ConfigProps, CN0),
            ConfigName =
                if
                    ConfigName0 =/= CN0,
                    ConfigName0 =/= <<"undefined">>, % Backwards compat
                    ConfigName0 =/= ?DEFAULT_CONFIG_NAME ->
                        ConfigName0;
                    true ->
                        find_config(<<"architecture">>, ConfigProps, CN0)
                end,
            OrigSuite = find_config(<<"suite">>, ConfigProps, R#run.test),
            Suite =
                case Source#source.suite_prefix of
                    undefined ->
                        OrigSuite;
                    SuitePrefix ->
                        Delim = "::",
                        case lux_utils:split(?b2l(OrigSuite), Delim) of
                            {_OrigSuitePrefix, OrigSuiteFile} ->
                                ?l2b([SuitePrefix, Delim, OrigSuiteFile]);
                            false ->
                                ?l2b([SuitePrefix, Delim, OrigSuite])
                        end
                end,
            RunId = find_config(<<"run">>, ConfigProps, R#run.id),
            ReposRev =
                find_config(<<"revision">>, ConfigProps, R#run.repos_rev),
            RunDir = find_config(<<"run_dir">>, ConfigProps, R#run.run_dir),
            RunLogDir = find_config(<<"log_dir">>, ConfigProps, RunDir),
            NewLogDir = R#run.new_log_dir,
            Cases = [parse_run_case(NewLogDir, RunDir, RunLogDir,
                                    StartTime, Branch, HostName, ConfigName,
                                    Suite, RunId, ReposRev, Case) ||
                        {test_group, _Group, Cases} <- Groups,
                        Case <- Cases],
            {RunWarnings, RunResult} = run_result(Result),
            R#run{test        = Suite,
                  id          = RunId,
                  result      = RunResult,
                  warnings    = RunWarnings,
                  start_time  = StartTime,
                  branch      = Branch,
                  hostname    = HostName,
                  config_name = ConfigName,
                  run_dir     = RunDir,
                  run_log_dir = true_drop_prefix(RunDir, RunLogDir),
                  repos_rev   = ReposRev,
                  runs        = Cases};
        {error, _SummaryLog, _ReasonStr} ->
            R
    end.

true_drop_prefix(Prefix, File) ->
    SplitPrefix = filename:split(Prefix),
    SplitFile = filename:split(File),
    true_drop_prefix2(SplitPrefix, SplitFile, File).

true_drop_prefix2([H|Prefix], [H|File], OrigFile) ->
    true_drop_prefix2(Prefix, File, OrigFile);
true_drop_prefix2([], [], OrigFile) when is_list(OrigFile) ->
    ".";
true_drop_prefix2([], [], OrigFile) when is_binary(OrigFile) ->
    <<".">>;
true_drop_prefix2([], File, _OrigFile) ->
    filename:join(File);
true_drop_prefix2(_Prefix, _File, OrigFile) ->
    OrigFile.

default_run(Source, SummaryLog) when is_list(SummaryLog) ->
    {ok, Cwd} = file:get_cwd(),
    RunDir = ?l2b(Cwd),
    Log = ?l2b(filename:basename(SummaryLog)),
    RunLogDir = true_drop_prefix(RunDir, Source#source.dir),
    NewLogDir0 = lux_utils:normalize_filename(filename:dirname(SummaryLog)),
    NewLogDir = ?l2b(true_drop_prefix(Cwd, NewLogDir0)),
    #run{log = Log,
         config_name = ?DEFAULT_CONFIG_NAME,
         run_dir = RunDir,
         run_log_dir = RunLogDir,
         new_log_dir = NewLogDir}.

split_config(ConfigBins) ->
    Split =
        fun(Config) ->
                case binary:split(Config, <<": ">>, []) of
                    [Key, Val] ->
                        {true,
                         {lux_utils:strip_trailing_whitespaces(Key),
                          Val}};
                    _  ->
                        false
                end
        end,
    lists:zf(Split, ConfigBins).

parse_run_case(NewLogDir, RunDir, RunLogDir,
               StartTime, Branch, Host, ConfigName,
               Suite, RunId, ReposRev,
               {test_case, AbsName, AbsEventLog, _Doc, _HtmlLog, CaseRes})
  when is_binary(NewLogDir), is_binary(RunDir), is_binary(RunLogDir),
       is_list(AbsName), is_list(AbsEventLog) ->
    RelEventLog = lux_utils:drop_prefix(RunLogDir, ?l2b(AbsEventLog)),
    RelNameBin = lux_utils:drop_prefix(RunDir, ?l2b(AbsName)),
    {RunWarnings, RunResult} = run_result(CaseRes),
    #run{test = <<Suite/binary, ":", RelNameBin/binary>>,
         id = RunId,
         result = RunResult,
         warnings = RunWarnings,
         log = RelEventLog,
         start_time = StartTime,
         branch = Branch,
         hostname = Host,
         config_name = ConfigName,
         run_dir = undefined,
         run_log_dir = undefined,
         new_log_dir = undefined,
         repos_rev = ReposRev,
         runs = []};
parse_run_case(NewLogDir, RunDir, RunLogDir,
               StartTime, Branch, Host, ConfigName,
               Suite, RunId, ReposRev,
               {result_case, AbsName, Res, _Reason})
  when is_binary(NewLogDir), is_binary(RunDir), is_binary(RunLogDir),
       is_list(AbsName) ->
    RelNameBin = lux_utils:drop_prefix(RunDir, ?l2b(AbsName)),
    {RunWarnings, RunResult} = run_result(Res),
    #run{test = <<Suite/binary, ":", RelNameBin/binary>>,
         id = RunId,
         result = RunResult,
         warnings = RunWarnings,
         start_time = StartTime,
         branch = Branch,
         hostname = Host,
         config_name = ConfigName,
         run_dir = undefined,
         run_log_dir = undefined,
         new_log_dir = undefined,
         repos_rev = ReposRev,
         runs = []}.

run_result({result_summary, Res, _Sections}) ->
    {[], run_result2(Res)};
run_result(Res) when is_binary(Res) ->
    {[], run_result2(Res)};
run_result({warnings_and_result, Warnings, Res}) ->
    {Warnings, run_result2(Res)}.

run_result2(Res) ->
    case Res of
        success                               -> success;
        warning                               -> warning;
        {skip, _}                             -> skip;
        {warning, _LN, _SN, _ET, _E, _A, _D}  -> warning;
        {fail, _LN, _SN, _ET, _E, _A, _D}     -> fail;
        {error, _Reason}                      -> fail;
        <<"SUCCESS">>                         -> success;
        <<"SKIP", _/binary>>                  -> skip;
        <<"FAIL", _/binary>>                  -> fail;
        <<"ERROR", _/binary>>                 -> fail;
        <<"WARNING", _/binary>>               -> warning
    end.

find_config(Key, Tuples, Default) ->
    case lists:keyfind(Key, 1, Tuples) of
        false    -> Default;
        {_, Val} -> Val
    end.

write_config_log(ConfigLog, ConfigData) when is_list(ConfigLog) ->
    PrettyConfig = format_config(ConfigData),
    write_log(ConfigLog, ?CONFIG_TAG, ?CONFIG_LOG_VERSION, [PrettyConfig]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Results

parse_summary_result(LogDir, WWW) when is_list(LogDir) ->
    ResultLog = lux_utils:join(LogDir, ?SUITE_RESULT_LOG),
    {ReadRes, NewWWW} = read_log(ResultLog, ?RESULT_TAG, WWW),
    Res =
        case ReadRes of
            {ok, ?RESULT_LOG_VERSION, Sections} ->
                %% Latest version
                {ok, split_result(Sections)};
            {ok, Version, _Sections} ->
                {error, ResultLog,
                 "Illegal result log version: " ++ ?b2l(Version)};
            {error, Reason, _} ->
                {error, ResultLog, Reason}
        end,
    {Res, NewWWW}.

write_results(Progress, SummaryLog, Summary, Results, Warnings)
  when is_list(SummaryLog) ->
    LogDir = filename:dirname(SummaryLog),
    ResultFile = lux_utils:join(LogDir, ?SUITE_RESULT_LOG),
    TmpResultFile = ResultFile++".tmp",
    case file:open(TmpResultFile, [write]) of
        {ok, Fd} ->
            try
                safe_format(Fd, "~s~s\n",
                            [?TAG(?RESULT_TAG), ?RESULT_LOG_VERSION]),
                IsTmp = is_temporary(SummaryLog),
                print_results(Progress, {IsTmp,Fd}, Summary, Results, Warnings),
                file:close(Fd),
                ok = file:rename(TmpResultFile, ResultFile)
            catch
                ?CATCH_STACKTRACE(Class, Reason, EST)
                    file:close(Fd),
                    file:delete(TmpResultFile),
                    erlang:raise(Class, Reason, EST)
            end;
        {error, FileReason} ->
            ReasonStr = ResultFile ++ ": " ++file:format_error(FileReason),
            erlang:error(ReasonStr)
    end.

print_results(Progress, Fd, Summary, Results, Warnings) ->
    %% Display most important results last
    result_format(Progress, Fd, "\n", []),
    print_success(Progress, Fd, Results),
    print_skip(Progress, Fd, Results),
    print_warning(Progress, Fd, Warnings),
    print_fail(Progress, Fd, Results),
    print_error(Progress, Fd, Results),
    result_format(Progress, Fd, "~s~s\n",
                  [?TAG("summary"),
                   [string:to_upper(Char) || Char <- ?a2l(Summary)]]).

print_success(Progress, Fd, Results) ->
    SuccessScripts = pick_result(Results, success),
    result_format(Progress, Fd, "~s~p\n",
                  [?TAG("successful"), length(SuccessScripts)]).

print_skip(Progress, Fd, Results) ->
    case pick_result(Results, skip) of
        [] ->
            ok;
        SkipScripts ->
            result_format(Progress, Fd, "~s~p\n",
                          [?TAG("skipped"), length(SkipScripts)]),
            [result_format(Progress, Fd, "\t~s:~s\n",
                           [lux_utils:drop_prefix(F), L]) ||
                {F, L, _R} <- SkipScripts]
    end.

print_warning(Progress, Fd, Warnings) ->
    case pick_result(Warnings, warning) of
        [] ->
            ok;
        WarnScripts ->
            result_format(Progress, Fd, "~s~p\n",
                          [?TAG("warnings"), length(WarnScripts)]),
            [result_format(Progress, Fd,
                           "\t~s:~s - ~s\n",
                           [lux_utils:drop_prefix(F), L, R]) ||
                {F, L, R} <- WarnScripts]
    end.

print_fail(Progress, Fd, Results) ->
    case pick_result(Results, fail) of
        [] ->
            ok;
        FailScripts ->
            Norm = fun({fail,R}) -> lux_utils:to_string(R);
                      (R)        -> lux_utils:to_string(R)
                   end,
            result_format(Progress, Fd, "~s~p\n",
                          [?TAG("failed"), length(FailScripts)]),
            [result_format(Progress, Fd, "\t~s:~s - ~s\n",
                           [lux_utils:drop_prefix(F), L, Norm(R)]) ||
                {F, L, R} <- FailScripts]
    end.

print_error(Progress, Fd, Results) ->
    case pick_result(Results, error) of
        [] ->
            ok;
        ErrorScripts ->
            result_format(Progress, Fd, "~s~p\n",
                          [?TAG("errors"), length(ErrorScripts)]),
            [result_format(Progress, Fd, "\t~s:~s - ~s\n",
                           [lux_utils:drop_prefix(F), L, R]) ||
                {F, L, R} <- ErrorScripts]
    end.

pick_result(Results, Outcome) when Outcome =:= error ->
    [{Script, FullLineNo, Reason} ||
        {error, Script, FullLineNo, Reason} <- Results];
pick_result(Warnings, Outcome) when Outcome =:= warning ->
    [{Script, FullLineNo, Reason} ||
        #warning{file = Script,
                 lineno = FullLineNo,
                 reason = Reason} <- Warnings];
pick_result(Results, Outcome) ->
    Pick = fun(fail, <<"FAIL", _/binary>> = FailBin, _Events) ->
                   FailBin;
              (_Outcome, _FailBin, Events) ->
                   Actual = fun(#result{actual=NewAcc}, _Acc) -> NewAcc;
                               (_, Acc)                       -> Acc
                            end,
                   lists:foldl(Actual, Outcome, Events)
           end,
    MatchRes =
        fun({ok, O, Script, FullLineNo, _S, _LD, Events, FailBin, _O}) ->
                if
                    O =:= Outcome ->
                        {true, {Script, FullLineNo, Pick(O, FailBin, Events)}};
                    true ->
                        false
                end
        end,
    OkRes = [R || R <- Results, element(1, R) =:= ok],
    lists:zf(MatchRes, OkRes).

result_format(Progress, {IsTmp, Fd}, Format, Args) ->
    IoList = ?FF(Format, Args),
    if
        Fd =:= undefined    -> ?l2b(IoList);
        Fd =:= standard_io,
        Progress =:= silent -> ?l2b(IoList);
        IsTmp               -> double_write(Progress, Fd, IoList);
        true                -> safe_write(Fd, IoList)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Event log

open_event_log(LogDir, Script, Progress, LogFun, Verbose, EmitTimestamp)
  when is_list(LogDir), is_list(Script) ->
    Base = filename:basename(Script),
    EventLog = lux_utils:join(LogDir, Base ++ ?CASE_EVENT_LOG),
    case file:open(EventLog, [write]) of
        {ok, EventFd} ->
            EventLogVersion =
                case EmitTimestamp of
                    true -> ?EVENT_LOG_VERSION;
                    false -> <<"0.3">>
                end,
            safe_format(Progress, LogFun, {Verbose, EventFd},
                        "~s~s\n", [?TAG(?EVENT_TAG), EventLogVersion]),
            safe_format(Progress, LogFun, {Verbose, EventFd},
                        "\n~s\n\n", [lux_utils:normalize_filename(Script)]),
            {ok, EventLog, EventFd};
        {error, Reason} ->
            {error, Reason}
    end.

close_event_log(EventFd) ->
    file:close(EventFd).

write_events(Progress, LogFun, Fd,
            LineNo, Shell, EmitTimestamp, Events) ->
    Prefix =
        case EmitTimestamp of
            true ->
                {_Mega, _Secs, Micros} = Now = lux_utils:timestamp(),
                {_Date, {Hours, Mins, Secs}} = calendar:now_to_local_time(Now),
                ?FF("~2..0w:~2..0w:~2..0w.~6..0w ~s(~p):",
                    [Hours, Mins, Secs, Micros, Shell, LineNo]);
            false ->
                ?FF("~s(~p):",
                    [Shell, LineNo])
        end,
    IoList = [format_event(Prefix, E) || E <- Events],
    safe_write(Progress, LogFun, Fd, IoList).

format_event(Prefix, {Op, Format, Args}) ->
    OpStr = ?a2l(Op),
    ?FF("~s ~s " ++ Format ++ "\n",
        [Prefix, OpStr] ++ Args).

scan_events(EventLog, WWW) when is_list(EventLog) ->
    {ReadRes, NewWWW} = read_log(EventLog, ?EVENT_TAG, WWW),
    case ReadRes of
        {ok, ?EVENT_LOG_VERSION, Sections} ->
            %% Latest version
            do_scan_events(EventLog, Sections, NewWWW);
        {ok, <<"0.7">>, Sections} ->
            %% Prev version with changes in shell exit codes
            do_scan_events(EventLog, Sections, NewWWW);
        {ok, <<"0.6">>, Sections} ->
            %% Prev version with old case_timeout format
            do_scan_events(EventLog, Sections, NewWWW);
        {ok, <<"0.5">>, Sections} ->
            %% Prev version with old case_timeout format
            do_scan_events(EventLog, Sections, NewWWW);
        {ok, <<"0.4">>, Sections} ->
            %% Prev version without where and stack
            do_scan_events(EventLog, Sections, NewWWW);
        {ok, <<"0.3">>, Sections} ->
            %% Prev version without timestamps
            do_scan_events(EventLog, Sections, NewWWW);
        {ok, <<"0.2">>, Sections} ->
            %% Prev version without warnings
            do_scan_events(EventLog, Sections, NewWWW);
        {ok, Version, _Sections} ->
            {{error, EventLog,
              "Illegal event log version: " ++ ?b2l(Version)},
             NewWWW};
        {error, FileReason, _} ->
            {{error, EventLog, FileReason}, NewWWW}
    end.

do_scan_events(EventLog, ES, WWW) ->
    EventSections = [binary:split(S, <<"\n">>, [global]) || S <- ES],
    case EventSections of
        [[ScriptBin], EventBins, ResultBins] -> ok;
        [[ScriptBin], ResultBins]            -> EventBins = []
    end,
    Script = ?b2l(ScriptBin),
    Dir = filename:dirname(EventLog),
    Base = filename:basename(EventLog, ?CASE_EVENT_LOG),
    ConfigLog = lux_utils:join(Dir, Base ++ ?CASE_CONFIG_LOG),
    {ScanRes, NewWWW} = scan_config(ConfigLog, WWW),
    Res =
        case ScanRes of
            {ok, []} ->
                LogBins = [],
                ConfigBins = [],
                {ok, EventLog, ConfigLog,
                 Script, EventBins, ConfigBins, LogBins, ResultBins};
            {ok, [ConfigSection]} ->
                LogBins = [],
                ConfigBins = binary:split(ConfigSection, <<"\n">>, [global]),
                {ok, EventLog, ConfigLog,
                 Script, EventBins, ConfigBins, LogBins, ResultBins};
            {ok, [ConfigSection,LogSection]} ->
                ConfigProps = binary:split(ConfigSection, <<"\n">>, [global]),
                LogBins = binary:split(LogSection, <<"\n">>, [global]),
                {ok, EventLog, ConfigLog,
                 Script, EventBins, ConfigProps, LogBins, ResultBins};
            {ok, Version, _Sections} ->
                {error, ConfigLog,
                 "Illegal config log version: " ++ ?b2l(Version)};
            {error, _, _} = Error ->
                Error
        end,
    {Res, NewWWW}.

parse_events([<<>>], Acc) ->
    %% Error case
    lists:reverse(Acc);
parse_events(Events, Acc) ->
    do_parse_events(Events, Acc).

do_parse_events([Event | Events], Acc) ->
    {Timestamp, Rest} = split_timestamp(Event),
    case Rest of
        <<"file_enter ", SubFile/binary>> ->
            %% file_enter 11 47 53 demo/test.lux
            %% file_exit 11 47 53 demo/test.lux
            parse_other_file(<<"file_exit ">>, SubFile, Events, Acc);
        _ ->
            [Prefix, Details] = binary:split(Rest, <<"): ">>),
            [Shell, RawLineNo] = binary:split(Prefix, <<"(">>),
            LineNo = list_to_integer(?b2l(RawLineNo)),
            {Op, Data} =
                case binary:split(Details, <<" ">>) of
                    [O2]     -> {O2, <<>>};
                    [O2, D2] -> {O2, D2}
                end,
            Acc2 = combine_recv_chunks(LineNo, Shell, Op, Timestamp, Data, Acc),
            do_parse_events(Events, Acc2)
    end;
do_parse_events([], Acc) ->
    lists:reverse(Acc).

split_timestamp(Event) ->
    case Event of
        <<H:2/binary, ":",
          M:2/binary, ":",
          S:2/binary, ".",
          I:6/binary, " ",
          Rest/binary>> ->
            Timestamp = <<H/binary,":",M/binary,":",S/binary,".",I/binary>>;
        _ ->
            Rest = Event,
            Timestamp = no_timestamp
    end,
    {Timestamp, Rest}.

combine_recv_chunks(LineNo, Shell, Op, Timestamp, Data, Acc) ->
    {Quote, Plain} = unquote(Data),
    case Acc of
        [#event{lineno = PrevLineNo,
                shell = PrevShell,
                op = PrevOp,
                %% timestamp = PrevTimestamp,
                data = PrevData,
                quote = PrevQuote} = E | Acc2]
          when PrevShell =:= Shell,
               PrevLineNo =:= LineNo,
               PrevQuote =:= quote,
               PrevOp =:= <<"recv">>,
               Op =:= <<"recv">>,
               Data =/= <<"timeout">> ->
            %% Combine consecutive two chunks of recv data
            %% into one in order to improve readability
            [E#event{data = [Plain | PrevData]} | Acc2];
        Acc2 ->
            E = #event{lineno = LineNo,
                       shell = Shell,
                       op = Op,
                       timestamp = Timestamp,
                       quote = Quote,
                       data = [Plain]},
            [E | Acc2]
    end.

parse_other_file(EndTag, SubFile, Events, Acc) when is_binary(SubFile) ->
    Pred =
        fun(E) ->
                {_Timestamp, Rest} = split_timestamp(E),
                EndSz = byte_size(EndTag),
                case Rest of
                    <<EndTag:EndSz/binary, SubFile/binary>> ->
                        false;
                    _ ->
                        true
                end
        end,
    {SubEvents, TmpEvents} = lists:splitwith(Pred, Events),
    [RawLineNoRange, SubFile2] = binary:split(SubFile, <<" \"">>),
    [RawLineNo, RawFirstLineNo, RawLastLineNo] =
        binary:split(RawLineNoRange, <<" ">>, [global]),
    Len = byte_size(SubFile2) - 1 ,
    <<SubFile3:Len/binary, _/binary>> = SubFile2,
    LineNo = list_to_integer(?b2l(RawLineNo)),
    FirstLineNo = list_to_integer(?b2l(RawFirstLineNo)),
    LastLineNo = list_to_integer(?b2l(RawLastLineNo)),
    SubEvents2 = parse_events(SubEvents, []),
    SubFile4 = ?b2l(SubFile3),
    E = #body{invoke_lineno = LineNo,
              first_lineno = FirstLineNo,
              last_lineno = LastLineNo,
              file = SubFile4,
              events = SubEvents2},
    case TmpEvents of
        [_| Events2] ->
            do_parse_events(Events2, [E | Acc]);
        [] ->
            do_parse_events([], [E | Acc])
    end.

extract_timers(Events) ->
    {_SendEvent, Case, RevTimers} =
        do_extract_timers(Events, undefined, undefined, undefined,
                          [<<>>], [], []),
    Timers = lists:reverse([Case | RevTimers]),
    %% io:format("\ntimers: ~p\n", [Timers]),
    Timers.

do_extract_timers([E | Events], Start, Send, Case, Calls, Nums, Acc) ->
    case E of
        #event{op = <<"start_time">>} ->
            do_extract_timers(Events, E, Send, Case, Calls, Nums, Acc);
        #event{op = <<"end_time">>} ->
            StartMicros = binary_datetime_to_micros(?l2b(Start#event.data)),
            EndMicros = binary_datetime_to_micros(?l2b(E#event.data)),
            ElapsedMicros = EndMicros - StartMicros,
            NewCase =
                if
                    Case#timer.max_time =:= infinity,
                    Case#timer.elapsed_time =:= undefined ->
                        Case#timer{elapsed_time = ElapsedMicros};
                    is_integer(Case#timer.max_time),
                    is_integer(Case#timer.elapsed_time) ->
                        %% Use the same time measurement even though not needed
                        Case#timer{elapsed_time = ElapsedMicros}
                        %% Case
                end,
            do_extract_timers(Events, Start, Send, NewCase, Calls, Nums, Acc);

        #event{op = <<"case_timeout">>} ->
            LeftMillis =
                case binary:split(?l2b(E#event.data), <<" ">>, [global]) of
                    [BinInt, <<"micros">> | _] ->
                        ?b2i(BinInt);
                    [<<"failed">>, <<"after">>, _BinInt, <<"micros">> | _] ->
                        %% Fail
                        0;
                    [<<"(0">>, <<"seconds">>,
                     <<"*">>,
                     _Mult, <<"multiplier)">>] ->
                        %% Fail - backwards compatibility
                        0;
                    [<<"infinity">>] ->
                        infinity
                end,
            NewCase =
                case Case of
                    undefined -> %% Start
                        #timer{send_lineno = [E#event.lineno],
                               send_data = <<>>,
                               match_lineno = [E#event.lineno],
                               match_data = <<>>,
                               shell = <<"case_timeout">>,
                               callstack = [],
                               macro = <<>>,
                               max_time = LeftMillis,
                               status = started,
                               elapsed_time = undefined};
                    #timer{} -> %% End
                        MaxTime = Case#timer.max_time,
                        ElapsedMillis =
                            if
                                is_integer(MaxTime),
                                is_integer(LeftMillis) ->
                                    MaxTime - LeftMillis;
                                is_integer(Case#timer.elapsed_time) ->
                                    Case#timer.elapsed_time;
                                MaxTime =:= infinity ->
                                    undefined
                            end,
                        Status =
                            if
                                is_integer(LeftMillis),
                                LeftMillis =< 0               -> failed;
                                Case#timer.status =:= failed  -> failed;
                                Case#timer.status =:= started -> matched
                            end,
                        Case#timer{status = Status,
                                   elapsed_time = ElapsedMillis}
                end,
            do_extract_timers(Events, Start, Send, NewCase,
                              Calls, Nums, Acc);
        #event{op = <<"send">>} ->
            NewSend = {E, Nums},
            do_extract_timers(Events, Start, NewSend, Case,
                              Calls, Nums, Acc);
        #event{op = <<"start">>} ->
            NewSend = {E, Nums},
            do_extract_timers(Events, Start, NewSend, Case,
                              Calls, Nums, Acc);
        #event{op = <<"expect", _/binary>>} ->
            {SendE, SendNums} = Send,
            SendLineNo = [SendE#event.lineno | SendNums],
            Callstack = format_calls(Calls, []),
            MatchLineNo = [E#event.lineno | Nums],
            T = #timer{send_lineno = SendLineNo,
                       send_data = SendE#event.data,
                       match_lineno = MatchLineNo,
                       match_data = E#event.data,
                       shell = E#event.shell,
                       callstack = Callstack,
                       status = expected},
            case Acc of
                [AddT | NewAcc] when AddT#timer.status =:= expected ->
                    ok; % Skip prev (expect_add | expect_add_strict) timer
                NewAcc ->
                    ok
            end,
            do_extract_timers(Events, Start, Send, Case,
                              Calls, Nums, [T | NewAcc]);
        #event{op = <<"timer">>, data = [Data]} ->
            [T | NewAcc] = Acc,
            case parse_timer(Data) of
                {started, MaxTime} when T#timer.status =:= expected  ->
                    NewT = T#timer{max_time = MaxTime,
                                   status = started},
                    do_extract_timers(Events, Start, Send, Case,
                                      Calls, Nums, [NewT|NewAcc]);
                {canceled, Elapsed} when T#timer.status =:= started ->
                    NewT = T#timer{status = matched,
                                   elapsed_time = Elapsed},
                    do_extract_timers(Events, Start, Send, Case,
                                      Calls, Nums, [NewT|NewAcc]);
                {failed, Elapsed} when T#timer.status =:= started ->
                    NewT = T#timer{status = failed,
                                   elapsed_time = Elapsed},
                    do_extract_timers(Events, Start, Send, Case,
                                      Calls, Nums, [NewT|NewAcc])
            end;
        #event{op = <<"invoke_", Macro/binary>>} ->
            NewCalls = [Macro | Calls],
            do_extract_timers(Events, Start, Send, Case,
                              NewCalls, Nums, Acc);
        #event{op = <<"exit_", Macro/binary>>} ->
            [Macro | NewCalls] = Calls,
            do_extract_timers(Events, Start, Send, Case,
                              NewCalls, Nums, Acc);
        #event{} ->
            do_extract_timers(Events, Start, Send, Case,
                              Calls, Nums, Acc);
        #body{events = EventsB} = B ->
            TmpNums = [B#body.invoke_lineno | Nums],
            {NewSend, NewCase, NewAcc} =
                do_extract_timers(EventsB, Start, Send, Case,
                                  Calls, TmpNums, Acc),
            do_extract_timers(Events, Start, NewSend, NewCase,
                              Calls, Nums, NewAcc)
    end;
do_extract_timers([], _Start, Send, Case, _Calls, _Nums, Acc) ->
    {Send, Case, Acc}.

binary_datetime_to_micros(DateTimeBin) ->
    %% <<"2020-08-06 16:09:32.872350">>
    [DateBin, TimeBin] = binary:split(DateTimeBin, <<" ">>, [global]),
    [YearsBin, MonthsBin, DaysBin] = binary:split(DateBin, <<"-">>, [global]),
    DateSecs = calendar:date_to_gregorian_days(?b2i(YearsBin),
                                               ?b2i(MonthsBin),
                                               ?b2i(DaysBin)),

    [HoursBin, MinsBin, SecsAndMicrosBin] =
        binary:split(TimeBin, <<":">>, [global]),
    [SecsBin, MicrosBin] = binary:split(SecsAndMicrosBin, <<".">>, [global]),
    TimeSecs =
        (?b2i(HoursBin) * 60 * 60) +
        (?b2i(MinsBin)  * 60) +
        ?b2i(SecsBin),

    TotalSecs = DateSecs + TimeSecs,
    (TotalSecs * ?ONE_SEC_MICROS) + ?b2i(MicrosBin).

format_calls([<<>>], Acc) ->
    ?l2b(join("->", Acc));
format_calls([H|T], Acc) ->
    format_calls(T, [H | Acc]).

%% ---------
%% new timer
%% ---------
%% started (10 seconds * 1.000 multiplier)
%% canceled (after 552 microseconds)
%% failed (after 10000764 microseconds)
%% ---------
%% old timer
%% ---------
%% started (10 seconds * 1.000)
%% canceled (after 0 seconds)
%% failed (after 10 seconds)
parse_timer(Data) ->
    case string:tokens(?b2l(Data), "() ") of
        ["started", "infinity"] ->
            {started, infinity};
        ["started", Secs, "seconds", "*", Multiplier] ->
            CeiledSecs = trunc((list_to_integer(Secs) *
                                    list_to_float(Multiplier)) + 0.5),
            {started, CeiledSecs * ?ONE_SEC_MICROS};
        ["started", Secs, "seconds", "*", Multiplier, "multiplier"] ->
            CeiledSecs = trunc((list_to_integer(Secs) *
                                    list_to_float(Multiplier)) + 0.5),
            {started, CeiledSecs * ?ONE_SEC_MICROS};
        ["canceled", "after", Secs, "seconds"] ->
            {canceled, list_to_integer(Secs) * ?ONE_SEC_MICROS};
        ["canceled", "after", MicroSecs, "microseconds"] ->
            {canceled, list_to_integer(MicroSecs)};
        ["canceled", "after", MicroSecs, "micro", "seconds"] ->
            {canceled, list_to_integer(MicroSecs)};
        ["failed", "after", Secs, "seconds"] ->
            {failed, list_to_integer(Secs) * ?ONE_SEC_MICROS};
        ["failed", "after", MicroSecs, "microseconds"] ->
            {failed, list_to_integer(MicroSecs)};
        ["failed", "after", MicroSecs, "micro", "seconds"] ->
            {failed, list_to_integer(MicroSecs)}
    end.

timers_to_csv(Timers) ->
    Header = [q(H) || H <- timer_header()],
    ElemLines = [timer_to_elems(T) || T <- Timers,
                                      T#timer.status =/= expected],
    Sep = ";",
    ?l2b([[join(Sep, E), "\n"] || E <- [Header | ElemLines]]).

timer_header() ->
    ["Send", "Match", "Shell", "Macro", "MaxTime", "Status", "Elapsed"].

join(_Sep, []) ->
    [];
join(Sep, [H|T]) ->
    [H|[[Sep, E] || E <- T]].

timer_to_elems(#timer{send_lineno  = SendStack,
                      send_data    = _SendData,
                      match_lineno = MatchStack,
                      shell        = Shell,
                      callstack    = Callstack,
                      max_time     = MaxTime,
                      status       = Status,
                      elapsed_time = Elapsed}) ->
    [
     case SendStack of
         [] -> q("@0");
         _  -> q(["@", lux_utils:pretty_full_lineno(SendStack)])
     end,
     q(["@", lux_utils:pretty_full_lineno(MatchStack)]),
     q(Shell),
     q(Callstack),
     case MaxTime of
         infinity -> q("infinity");
         _        -> ?i2l(MaxTime)
     end,
     q(?a2l(Status)),
     if
         Elapsed =:= undefined,
         Status =:= started ->
             "";
         Elapsed =:= undefined ->
             "?";
         is_integer(Elapsed),
         Status =/= started ->
             ?i2l(Elapsed)
     end
    ].

q(List) ->
    Replace = fun(C) -> case C of $; -> "<SEMI>"; _ -> C end end,
    ["\"", lists:map(Replace, ?b2l(?l2b(List))), "\""].

csv_to_timers(CsvFile) ->
    case file:read_file(CsvFile) of
        {ok, FileBin} ->
            RowBins = binary:split(FileBin, <<"\n">>, [global]),
            Fun =
                fun(RowBin) ->
                        [Send, Match, Shell, Macro, MaxTime, Status, Elapsed] =
                            binary:split(RowBin, <<"\n">>, [global]),
                        #timer{send_lineno  = Send,
                               send_data    = undefined,
                               match_lineno = Match,
                               shell        = Shell,
                               callstack    = Macro,
                               macro        = undefined,
                               max_time     = MaxTime,
                               status       = Status,
                               elapsed_time = Elapsed}
                end,
            [#timer{match_lineno = "Match"} | Timers] = lists:map(Fun, RowBins),
            Timers;
        {error, FileReason} ->
            {error, CsvFile, file:format_error(FileReason)}
    end.

scan_config(ConfigLog, WWW) when is_list(ConfigLog) ->
    {ReadRes, NewWWW} = read_log(ConfigLog, ?CONFIG_TAG, WWW),
    Res =
        case ReadRes of
            {ok, ?CONFIG_LOG_VERSION, Sections} ->
                {ok, Sections};
            {ok, Version, _Sections} ->
                {error, ConfigLog,
                 "Illegal config log version: " ++ ?b2l(Version)};
            {error, FileReason, _} ->
                case file:format_error(enoent) of
                    FR when FR =:= FileReason ->
                        {ok, []};
                    _ ->
                        {error, ConfigLog, FileReason}
                end
        end,
    {Res, NewWWW}.

parse_config([ConfigSection|_]) when is_binary(ConfigSection) ->
    ConfigSection;
parse_config(ConfigSection) when is_binary(ConfigSection) ->
    ConfigSection.

parse_io_logs([StdinLog, StdoutLog | Logs], Acc) ->
    [_, Shell, Stdin] = binary:split(StdinLog, <<": ">>, [global]),
    [_, Shell, Stdout] = binary:split(StdoutLog, <<": ">>, [global]),
    L = {log, Shell, ?b2l(Stdin), ?b2l(Stdout)},
    %% io:format("Logs: ~p\n", [L]),
    parse_io_logs(Logs, [L | Acc]);
parse_io_logs([<<>>], Acc) ->
    lists:reverse(Acc);
parse_io_logs([], Acc) ->
    lists:reverse(Acc).

parse_result([<<>> |  RawResult]) ->
    parse_result(RawResult);
parse_result(RawResult) ->
    Pred =
        fun(R) ->
                case R of
                    <<"warning", _/binary>> -> true;
                    _                       -> false
                end
        end,
    {LongWarnings, [LongResult|Rest]} = lists:splitwith(Pred, RawResult),
    Split = fun(R) -> binary:split(R, <<": ">>) end,
    Warnings = lists:map(fun(L) -> [_T, W] = Split(L), Split(W) end,
                         LongWarnings),
    [_, Result] = Split(LongResult),
    R =
        case Result of
            <<"SUCCESS">> ->
                success;
            <<"WARNING">> ->
                warning;
            <<"SKIP as ",Skip/binary>> ->
                {skip, [Skip | Rest]};
            <<"ERROR at ", Error/binary>> ->
                [RawLineNo, Reason] = binary:split(Error, <<":">>),
                {error_line, RawLineNo, [Reason | Rest]};
            <<"ERROR ", Reason/binary>> ->
                {error, [Reason | Rest]};
            <<"INTERNAL_ERROR ", Reason/binary>> ->
                {error, [Reason | Rest]};
            <<"WARNING at ", FailBin/binary>> ->
                [TagBin = <<"expected", _/binary>>,
                 Expected,
                 <<"actual ", Actual/binary>>,
                 Details | _] = Rest,
                ExpectedTag = list_to_existing_atom(?b2l(TagBin)),
                {RawLineNo, ShellName} = split_fail_bin(FailBin),
                {quote, Expected2} = unquote(Expected),
                Expected3 = split_quoted_lines(Expected2),
                {quote, Details2} = unquote(Details),
                Details3 = split_quoted_lines(Details2),
                {warning, RawLineNo, ShellName,
                 ExpectedTag, Expected3,
                 Actual, Details3};
            <<"FAIL at ", FailBin/binary>> ->
                [TagBin = <<"expected", _/binary>>, Expected,
                 <<"actual ", Actual/binary>>, Details | _] = Rest,
                ExpectedTag = list_to_existing_atom(?b2l(TagBin)),
                {RawLineNo, ShellName} = split_fail_bin(FailBin),
                {quote, Expected2} = unquote(Expected),
                Expected3 = split_quoted_lines(Expected2),
                {quote, Details2} = unquote(Details),
                Details3 = split_quoted_lines(Details2),
                {fail, RawLineNo, ShellName,
                 ExpectedTag, Expected3,
                 Actual, Details3};
            <<"FAIL as ", _/binary>> = Fail-> % Require
                {error, [Fail]}
        end,
    %% io:format("Result: ~p\n", [R]),
    {warnings_and_result, Warnings, R}.

split_quoted_lines(<<Chop:10000/binary, Rest/binary>>) ->
    Sz = ?i2b(byte_size(Rest)),
    Bin =
        <<Chop/binary,
          "\n\n...\n\n<LUX WARNING> This is an insane amount of output. ",
          Sz/binary,
          " bytes are ignored. See the textual event log for details...">>,
    do_split_quoted_lines(Bin);
split_quoted_lines(Bin) when is_binary(Bin) ->
    do_split_quoted_lines(Bin).

do_split_quoted_lines(Bin) ->
    Normalized = lux_utils:replace(Bin, [{quoted_crlf, <<"\n">>}]),
    lux_utils:split_lines(Normalized).

split_fail_bin(FailBin0) ->
    case FailBin0 of
        <<"line ", FailBin/binary>> -> ok;
        FailBin                     -> ok % Backwards compat
    end,
    case binary:split(FailBin, <<" in shell ">>, []) of
        [RawLineNo] -> {RawLineNo, <<"lux">>}; % Backwards compat
        [RawLineNo, ShellName] -> {RawLineNo, ShellName}
    end.

unquote(Bin) ->
    Quote = <<"\"">>,
    Size = byte_size(Bin)-2,
    case Bin of
        <<Quote:1/binary, Plain:Size/binary, Quote:1/binary>> ->
            {quote, Plain};
        Plain ->
            {plain, Plain}
    end.

dequote(" expect " ++ _ = L) ->
    re:replace(L, <<"\\\\\\\\R">>, <<"\n    ">>, [global, {return, list}]);
dequote([$\"|T]) ->
    [$\"|dequote1(T)];
dequote([H|T]) ->
    [H|dequote(T)];
dequote([]) ->
    [].

dequote1([$\\,$\\|T]) ->
    [$\\|dequote1(T)];
dequote1([$\\,$r,$\\,$n|T]) ->
    "\n    " ++ dequote1(T);
dequote1([$\\,$n|T]) ->
    "\n    " ++ dequote1(T);
dequote1([H|T]) ->
    [H|dequote1(T)];
dequote1([]) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Config log

open_config_log(LogDir, Script, ConfigData) ->
    Base = filename:basename(Script),
    ConfigFile = lux_utils:join(LogDir, Base ++ ?CASE_CONFIG_LOG),
    case filelib:ensure_dir(ConfigFile) of
        ok ->
            case write_config_log(ConfigFile, ConfigData) of
                ok ->
                    case file:open(ConfigFile, [append]) of
                        {ok, ConfigFd} ->
                            ConfigFd;
                        {error, FileReason} ->
                            ReasonStr = ConfigFile ++ ": " ++
                                file:format_error(FileReason),
                            erlang:error(ReasonStr)
                    end;
                {error, FileReason} ->
                    ReasonStr = LogDir ++ ": " ++
                        file:format_error(FileReason),
                    erlang:error(ReasonStr)
            end;
        {error, FileReason} ->
            ReasonStr = LogDir ++ ": " ++
                file:format_error(FileReason),
            erlang:error(ReasonStr)
    end.

close_config_log(ConfigFd, Logs) ->
    ok = file:write(ConfigFd, "\n"),
    ShowLog =
        fun({Name, Stdin, Stdout}) ->
                Data =
                    [
                     ?FF("~s~s: ~s\n",
                                   [?TAG("stdin  log file"), Name, Stdin]),
                     ?FF("~s~s: ~s\n",
                                   [?TAG("stdout log file"), Name, Stdout])
                    ],
                ok = file:write(ConfigFd, Data)
        end,
    lists:foreach(ShowLog, Logs),
    file:close(ConfigFd).

format_config(Config) ->
    Fun =
        fun({Tag, Types, Val}) ->
                lists:flatten(format_config(Tag, Val, Types));
           ({Tag, Val}) ->
                {ok, _Pos, Types} = lux_case:config_type(Tag),
                lists:flatten(format_config(Tag, Val, Types))
        end,
    lists:map(Fun, Config).

format_config(Tag, Val, Types) ->
    case format_val_choice(Tag, Val, Types) of
        [] ->
            ?FF("~s\n", [?TAG(Tag)]);
        [String] ->
            ?FF("~s~s\n", [?TAG(Tag), to_printable(String)]);
        [String|Strings] ->
            [?FF("~s~s\n", [?TAG(Tag), to_printable(String)]),
             [[lists:duplicate(?TAG_WIDTH, $\ ),to_printable(S),"\n"] ||
                 S <- Strings]
            ]
    end.

to_printable(Chars) ->
    Fun = fun(Char) ->
                  case Char >= $\ andalso io_lib:printable_list([Char]) of
                      true  -> Char;
                      false -> [$\\, string:right(?i2l(Char),3, $0)]
                  end
          end,
    lists:flatten(lists:map(Fun, Chars)).

format_val_choice(Tag, Val, [Type | Types]) ->
    try
        try_format_val(Tag, Val, Type)
    catch
        _Class:_Reason ->
            format_val_choice(Tag, Val, Types)
    end;
format_val_choice(_Tag, Val, []) ->
    [lists:flatten(?FF("~w\n", [Val]))].

try_format_val(_Tag, Val = undefined, _Type) ->
    [?a2l(Val)];
try_format_val(Tag, Val, Type) ->
    case Type of
        string when is_list(Val) ->
            [Val];
        binary when is_binary(Val) ->
            [?b2l(Val)];
        {atom, _Atoms} when is_atom(Val) ->
            [?a2l(Val)];
        {integer, _Min, _Max} when is_integer(Val) ->
            [?i2l(Val)];
        {integer, _Min, _Max} when Val =:= infinity ->
            [?a2l(Val)];
        {float, _Min, _Max} when is_float(Val) ->
            IoList = ?FF("~f", [Val]),
            [string:strip(lists:flatten(IoList), right, $0)];
        {float, _Min, _Max} when Val =:= infinity ->
            [?a2l(Val)];
        {std_list, SubTypes} ->
            [hd(format_val_choice(Tag, V, SubTypes)) || V <- Val];
        {reset_list, SubTypes} when is_list(SubTypes) ->
            [hd(format_val_choice(Tag, V, SubTypes)) || V <- Val]
    end.

safe_format(Fd, Format, Args) ->
    IoList = ?FF(Format, Args),
    safe_write(Fd, IoList).

safe_write(OptFd, IoList) when is_list(IoList) ->
    safe_write(OptFd, ?l2b(IoList));
safe_write(OptFd, Bin) when is_binary(Bin) ->
    if
        Bin =:= <<>>        -> ok;
        OptFd =:= undefined -> ok = io:format("~s", [Bin]);
        true                -> ok = file:write(OptFd, Bin)
    end,
    Bin.

double_write(Progress, Fd, {ResIoList, ConIoList}) when Fd =/= undefined ->
    Bin = safe_write(Fd, ResIoList),
    case Progress of
        silent -> ok;
        _      -> safe_write(undefined, ConIoList)
    end,
    Bin;
double_write(Progress, Fd, IoList) ->
    double_write(Progress, Fd, {IoList, IoList}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

safe_format(Progress, LogFun, Fd, Format, Args) ->
    IoList = ?FF(Format, Args),
    safe_write(Progress, LogFun, Fd, IoList).

safe_write(Progress, LogFun, Fd, IoList) when is_list(IoList) ->
    safe_write(Progress, LogFun, Fd, ?l2b(IoList));
safe_write(_Progress, _LogFun, _Fd, Bin) when Bin =:= <<>>  ->
    Bin;
safe_write(Progress, LogFun, Fd0, Bin) when is_binary(Bin) ->
    {Verbose, Fd} = split_fd(Fd0),
    write_progress(Progress, Verbose, Bin),
    write_to_log(Fd, LogFun, Bin),
    Bin.

split_fd(Fd0) ->
    case Fd0 of
        undefined ->
            Fd = Fd0,
            Verbose = false;
        {Verbose, Fd} ->
            ok
    end,
    {Verbose, Fd}.

write_progress(Progress, false, _Bin) ->
    case Progress of
        silent  -> ok;
        summary -> ok;
        brief   -> ok;
        doc     -> ok;
        compact -> ok;
        verbose -> ok;
        etrace  -> ok;
        ctrace  -> ok
    end;
write_progress(Progress, true, Bin) ->
    try
        case Progress of
            silent  -> ok;
            summary -> ok;
            brief   -> ok;
            doc     -> ok;
            compact -> io:format("~s", [?b2l(Bin)]);
            verbose -> io:format("~s", [dequote(?b2l(Bin))]);
            etrace  -> io:format("~s", [dequote(?b2l(Bin))]);
            ctrace  -> io:format("~s", [dequote(?b2l(Bin))])
        end
    catch
        _:Reason:EST ->
            io:format("\nINTERNAL LUX ERROR: progress write failed:"
                      " ~p\n\t~p\n\t~p\n",
                      [Reason, Bin, EST]),
            exit({safe_write, Progress, Bin, Reason})
    end.

write_to_log(undefined, LogFun, Bin) ->
    try
        case LogFun(Bin) of
            <<_/binary>> ->
                ok;
            BadRes ->
                BadEST = ?stacktrace(),
                io:format("\nINTERNAL LUX ERROR: log write failed:"
                          " ~p\n\t~p\n\t~p\n",
                      [BadRes, Bin, BadEST]),
                exit({safe_write, log_fun, Bin, BadRes})
        end
    catch
        _:LogReason:LogEST ->
            io:format("\nINTERNAL LUX ERROR: log write failed:"
                      " ~p\n\t~p\n\t~p\n",
                      [LogReason, Bin, LogEST]),
            exit({safe_write, log_fun, Bin, LogReason})
    end;
write_to_log(Fd, _LogFun, Bin) ->
    try file:write(Fd, Bin) of
        ok ->
            ok;
        {error, FileReason} ->
            Str = file:format_error(FileReason),
            io:format("\nINTERNAL LUX ERROR: file write failed:"
                      " ~s\n\t~p\n\t~p\n",
                      [Str, Bin, ?stacktrace()]),
            exit({safe_write, file, Fd, Bin, {error, FileReason}})
    catch
        _:WriteReason:WriteEST ->
            io:format("\nINTERNAL LUX ERROR: file write failed:"
                      " ~p\n\t~p\n\t~p\n",
                      [WriteReason, Bin, WriteEST]),
            exit({safe_write, file, Bin, WriteReason})
    end.
