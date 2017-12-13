%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_log).

-export([
         is_temporary/1, parse_summary_log/2, parse_run_summary/4,
         open_summary_log/3, close_summary_tmp_log/1, close_summary_log/2,
         write_config_log/2, split_config/1, find_config/3,
         write_results/5, print_results/5, parse_result/1, pick_result/2,
         safe_format/3, safe_write/2, double_write/3,
         open_event_log/5, close_event_log/1, write_event/4, scan_events/2,
         parse_events/2, parse_io_logs/2, open_config_log/3, close_config_log/2,
         extract_timers/1, timers_to_csv/1, csv_to_timers/1,
         safe_format/5, safe_write/4, dequote/1
        ]).

-include_lib("kernel/include/file.hrl").
-include("lux.hrl").

-define(SUMMARY_LOG_VERSION, <<"0.2">>).
-define(EVENT_LOG_VERSION,   <<"0.1">>).
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
                    LogIoList = io_lib:format("~s~s\n",
                                              [?TAG(?SUMMARY_TAG),
                                               ?SUMMARY_LOG_VERSION]),
                    safe_write(SummaryFd, LogIoList),
                    case Progress of
                        silent ->
                            ok;
                        _ ->
                            StdoutIoList =
                                io_lib:format("~s~s\n",
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
                     file=SummaryLog,
                     orig=SummaryLog},
    parse_summary_log(Source, WWW);
parse_summary_log(#source{file=SummaryLog} = Source, WWW) ->
    try
        do_parse_summary_log(Source, WWW)
    catch
        error:Reason ->
            EST = erlang:get_stacktrace(),
            ReasonStr =
                lists:flatten(io_lib:format("\nINTERNAL LUX ERROR"
                                            " in ~s\n~p\n\~p\n",
                                            [SummaryLog, Reason, EST])),
            io:format("~s\n", [ReasonStr]),
            {{error, SummaryLog, ReasonStr}, WWW}
    end.

do_parse_summary_log(#source{file=SummaryLog}, WWW) ->
    {ReadRes, NewWWW} = read_log(SummaryLog, ?SUMMARY_TAG, WWW),
    Res =
        case ReadRes of
            {ok, ?SUMMARY_LOG_VERSION, Sections} ->
                %% Latest version
                LogDir = filename:dirname(SummaryLog),
                ConfigLog = lux_utils:join(LogDir, "lux_config.log"),
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
                 [{test_group, "", Cases}], SummaryConfig, Ctime, EventLogs};
            {ok, Version, _Sections} ->
                {error, SummaryLog,
                 "Illegal summary log version: " ++ ?b2l(Version)};
            {error, FileReason, _} ->
                {error, SummaryLog, FileReason}
        end,
    {Res, NewWWW}.

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
            fetch_log(Log, StopFun);
        {false, _StopFun} ->
            fetch_log(Log, false)
    end;
fetch_log(Log, false = WWW) ->
    {file:read_file(Log), WWW};
fetch_log(Log, StopFun = WWW) when is_function(StopFun, 0) ->
    Res =
        case lux_utils:is_url(Log) of
            true ->
                io:format(":", []),
                case httpc:request(Log) of
                    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
                        {ok, ?l2b(Body)};
                    {ok, {{_Version, _Code, ReasonPhrase}, _Headers, _Body}} ->
                        {error, ReasonPhrase, <<>>};
                    {error, Reason} ->
                        String = lists:flatten(io_lib:format("~p", [Reason])),
                        {error, String, <<>>}
                end;
            false ->
                file:read_file(Log)
        end,
    {Res, WWW}.

write_log(File, Tag, Version, Sections) when is_list(File) ->
    file:write_file(File, [?TAG(Tag), Version, [["\n\n",S] || S <- Sections]]).

split_result([Result]) ->
    Lines = binary:split(Result, <<"\n">>, [global]),
    [_, Summary | Rest] = lists:reverse(Lines),
    [_, Summary2] = binary:split(Summary, <<": ">>),
    Lines2 = lists:reverse(Rest),
    Sections = split_result2(Lines2, []),
    {result, Summary2, Sections}.

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
                    {file, File2, LineNo}
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
            [<<>>]                               -> "unknown"
        end,
    case Sections of
        [] ->
            Res = {result_case, Name, <<"ERROR">>, <<"unknown">>},
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
                        {result_case, Name, <<"ERROR">>, <<"unknown">>}
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

parse_run_summary(Source, File, Res, Opts) ->
    %% File is relative cwd
    try
        do_parse_run_summary(Source, File, Res, Opts)
    catch
        error:Reason ->
            EST = erlang:get_stacktrace(),
            ReasonStr =
                lists:flatten(io_lib:format("\nINTERNAL LUX ERROR"
                                            " in ~s\n~p\n\~p\n",
                                            [File, Reason, EST])),
            io:format("~s\n", [ReasonStr]),
            {error, File, ReasonStr}
    end.

do_parse_run_summary(Source, File, Res, Opts) ->
    {ok, Cwd} = file:get_cwd(),
    CN0 = ?DEFAULT_CONFIG_NAME,
    Log = filename:basename(File),
    NewLogDir = lux_utils:normalize_filename(filename:dirname(File)),
    R = #run{test = ?DEFAULT_SUITE,
             id = ?DEFAULT_RUN,
             result = fail,
             log = Log,
             start_time = ?DEFAULT_TIME,
             hostname = ?DEFAULT_HOSTNAME,
             config_name = CN0,
             run_dir = Cwd,
             run_log_dir = Source#source.dir,
             new_log_dir = NewLogDir,
             repos_rev = ?DEFAULT_REV,
             details = []},
    case Res of
        {ok, Result, Groups, SummaryConfig, Ctime, _EventLogs} ->
            ConfigBins = binary:split(SummaryConfig, <<"\n">>, [global]),
            ConfigProps = split_config(ConfigBins),
            StartTime = find_config(<<"start time">>, ConfigProps, Ctime),
            Branch = Source#source.branch,
            case lux_utils:pick_opt(hostname, Opts, undefined) of
                undefined ->
                    HostName = find_config(<<"hostname">>, ConfigProps,
                                           R#run.hostname);
                HostName ->
                    ok
            end,
            ConfigName0 = find_config(<<"architecture">>, ConfigProps, CN0),
            ConfigName =
                if
                    ConfigName0 =/= CN0,
                    ConfigName0 =/= <<"undefined">> ->
                        ConfigName0;
                    true ->
                        find_config(<<"config name">>, ConfigProps, CN0)
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
            CwdBin = ?l2b(Cwd),
            RunDir = ?b2l(find_config(<<"run_dir">>, ConfigProps, CwdBin)),
            RunLogDir = ?b2l(find_config(<<"log_dir">>, ConfigProps, CwdBin)),
            Cases = [parse_run_case(NewLogDir, RunDir, RunLogDir,
                                    StartTime, Branch, HostName, ConfigName,
                                    Suite, RunId, ReposRev, Case) ||
                        {test_group, _Group, Cases} <- Groups,
                        Case <- Cases],
            R#run{test        = Suite,
                  id          = RunId,
                  result      = run_result(Result),
                  start_time  = StartTime,
                  branch      = Branch,
                  hostname    = HostName,
                  config_name = ConfigName,
                  run_dir     = RunDir,
                  run_log_dir = RunLogDir,
                  repos_rev   = ReposRev,
                  details     = Cases};
        {error, _SummaryLog, _ReasonStr} ->
            R
    end.

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
  when is_list(RunDir), is_list(RunLogDir),
       is_list(AbsName), is_list(AbsEventLog) ->
    RelEventLog = lux_utils:drop_prefix(RunLogDir, AbsEventLog),
    RelNameBin = ?l2b(lux_utils:drop_prefix(RunDir, AbsName)),
    #run{test = <<Suite/binary, ":", RelNameBin/binary>>,
         id = RunId,
         result = run_result(CaseRes),
         log = RelEventLog,
         start_time = StartTime,
         branch = Branch,
         hostname = Host,
         config_name = ConfigName,
         run_dir = RunDir,
         run_log_dir = RunLogDir,
         new_log_dir = NewLogDir,
         repos_rev = ReposRev,
         details = []};
parse_run_case(NewLogDir, RunDir, RunLogDir,
               StartTime, Branch, Host, ConfigName,
               Suite, RunId, ReposRev,
               {result_case, AbsName, Res, _Reason})
  when is_list(RunDir), is_list(RunLogDir),
       is_list(AbsName) ->
    RelNameBin = ?l2b(lux_utils:drop_prefix(RunDir, AbsName)),
    #run{test = <<Suite/binary, ":", RelNameBin/binary>>,
         id = RunId,
         result = run_result(Res),
         log = ?DEFAULT_LOG,
         start_time = StartTime,
         branch = Branch,
         hostname = Host,
         config_name = ConfigName,
         run_dir = RunDir,
         run_log_dir = RunLogDir,
         new_log_dir = NewLogDir,
         repos_rev = ReposRev,
         details = []}.

run_result({result, Res, _}) ->
    run_result(Res);
run_result({result, Res}) ->
    run_result(Res);
run_result(Res) ->
    case Res of
        success                                               -> success;
        warning                                               -> warning;
        {skip, _}                                             -> skip;
        {warning, _LineNo, _ET, _Expected, _Actual, _Details} -> warning;
        {fail, _LineNo, _ET,__Expected, _Actual, _Details}    -> fail;
        {error, _Reason}                                      -> fail;
        <<"SUCCESS">>                                         -> success;
        <<"SKIP", _/binary>>                                  -> skip;
        <<"FAIL", _/binary>>                                  -> fail;
        <<"ERROR", _/binary>>                                 -> fail;
        <<"WARNING", _/binary>>                               -> warning
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
    ResultLog = lux_utils:join(LogDir, "lux_result.log"),
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
    ResultFile = lux_utils:join(LogDir, "lux_result.log"),
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
                Class:Reason ->
                    file:close(Fd),
                    file:delete(TmpResultFile),
                    erlang:Class(Reason)
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
                   [string:to_upper(Char) || Char <- atom_to_list(Summary)]]).

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
            Norm = fun(Str) -> lux_utils:to_string(Str) end,
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
        {warning, Script, FullLineNo, Reason} <- Warnings];
pick_result(Results, Outcome) ->
    Actual = fun(#result{actual=NewAcc}, _Acc) -> NewAcc;
                (_, Acc)                       -> Acc
             end,
    [{Script, FullLineNo, lists:foldl(Actual, Outcome, Events)} ||
        {ok, O, Script, FullLineNo, _LogDir,
         Events, _FailBin, _Opaque} <- Results,
        O =:= Outcome].

result_format(Progress, {IsTmp, Fd}, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    if
        Fd =:= undefined    -> ?l2b(IoList);
        Fd =:= standard_io,
        Progress =:= silent -> ?l2b(IoList);
        IsTmp               -> double_write(Progress, Fd, IoList);
        true                -> safe_write(Fd, IoList)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Event log

open_event_log(LogDir, Script, Progress, LogFun, Verbose)
  when is_list(LogDir), is_list(Script) ->
    Base = filename:basename(Script),
    EventLog = lux_utils:join(LogDir, Base ++ ".event.log"),
    case file:open(EventLog, [write]) of
        {ok, EventFd} ->
            safe_format(Progress, LogFun, {Verbose, EventFd},
                        "~s~s\n", [?TAG(?EVENT_TAG), ?EVENT_LOG_VERSION]),
            safe_format(Progress, LogFun, {Verbose, EventFd},
                        "\n~s\n\n", [lux_utils:normalize_filename(Script)]),
            {ok, EventLog, EventFd};
        {error, Reason} ->
            {error, Reason}
    end.

close_event_log(EventFd) ->
    file:close(EventFd).

write_event(Progress, LogFun, Fd, {log_event, LineNo, Shell, Op, "", []}) ->
    OpStr = atom_to_list(Op),
    Data = io_lib:format("~s(~p): ~s\n", [Shell, LineNo, OpStr]),
    safe_write(Progress, LogFun, Fd, Data);
write_event(Progress, LogFun, Fd, {log_event, LineNo, Shell, Op, Fmt, Args}) ->
    OpStr = atom_to_list(Op),
    Data = io_lib:format(Fmt, Args),
%% ??   Data2 = lux_utils:normalize_match_regexp(Data),
    Data2 = Data,
    Data3 = io_lib:format("~s(~p): ~s ~s\n", [Shell, LineNo, OpStr, Data2]),
    safe_write(Progress, LogFun, Fd, Data3).

scan_events(EventLog, WWW) when is_list(EventLog) ->
    {ReadRes, NewWWW} = read_log(EventLog, ?EVENT_TAG, WWW),
    case ReadRes of
        {ok, ?EVENT_LOG_VERSION, Sections} ->
            do_scan_events(EventLog, Sections, NewWWW);
        {ok, Version, _Sections} ->
            {{error, EventLog,
              "Illegal event log version: " ++ ?b2l(Version)},
             NewWWW};
        {error, FileReason, _} ->
            {{error, EventLog, FileReason}, NewWWW}
    end.

do_scan_events(EventLog, EventSections, WWW) ->
    EventSections2 = [binary:split(S, <<"\n">>, [global]) ||
                         S <- EventSections],
    case EventSections2 of
        [[ScriptBin], EventBins, ResultBins] -> ok;
        [[ScriptBin], ResultBins]            -> EventBins = []
    end,
    Script = ?b2l(ScriptBin),
    Dir = filename:dirname(EventLog),
    Base = filename:basename(EventLog, ".event.log"),
    ConfigLog = lux_utils:join(Dir, Base ++ ".config.log"),
    {ScanRes, NewWWW} = scan_config(ConfigLog, WWW),
    Res =
        case ScanRes of
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
            {error, FileReason} ->
                {error, ConfigLog, file:format_error(FileReason)}
        end,
    {Res, NewWWW}.

parse_events([<<>>], Acc) ->
    %% Error case
    lists:reverse(Acc);
parse_events(Events, Acc) ->
    do_parse_events(Events, Acc).

do_parse_events([<<"file_enter ", SubFile/binary>> | Events], Acc) ->
    %% file_enter 11 47 53 demo/test.lux
    %% file_exit 11 47 53 demo/test.lux
    parse_other_file(<<"file_exit ">>, SubFile, Events, Acc);
do_parse_events([<<"include_begin ", SubFile/binary>> | Events], Acc) ->
    %% Old style
    %% include_begin 11 47 53 demo/test.luxinc
    %% include_end 11 47 53 demo/test.luxinc
    parse_other_file(<<"include_end ">>, SubFile, Events, Acc);
do_parse_events([Event | Events], Acc) ->
    [Prefix, Details] = binary:split(Event, <<"): ">>),
    [Shell, RawLineNo] = binary:split(Prefix, <<"(">>),
    LineNo = list_to_integer(?b2l(RawLineNo)),
    [Op | RawContents] = binary:split(Details, <<" ">>),
    Data =
        case RawContents of
            [] ->
                %% cli(86): suspend
                [<<>>];
            [Contents] ->
                case unquote(Contents) of
                    {quote, C} ->
                        %% cli(26): recv "echo ==$?==\r\n==0==\r\n$ "
                        split_quoted_lines(C);
                    {plain, C} ->
                        %% cli(70): timer start (10 seconds)
                        [C]
                end
        end,
    E = #event{lineno = LineNo,
               shell = Shell,
               op = Op,
               data = Data},
    do_parse_events(Events, [E | Acc]);
do_parse_events([], Acc) ->
    lists:reverse(Acc).

parse_other_file(EndTag, SubFile, Events, Acc) when is_binary(SubFile) ->
    Pred = fun(E) ->
                   EndSz = byte_size(EndTag),
                   case E of
                       <<EndTag:EndSz/binary, SubFile/binary>> ->
                           false;
                       _ ->
                           true
                   end
           end,
    {SubEvents, [_| Events2]} = lists:splitwith(Pred, Events),
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
    do_parse_events(Events2, [E | Acc]).

extract_timers(Events) ->
    {_SendEvent, RevTimers} =
        extract_timers(Events, undefined, [<<>>], [], []),
    Timers = lists:reverse(RevTimers),
    %% io:format("\ntimers: ~p\n", [Timers]),
    Timers.

extract_timers([E | Events], Send, Calls, Nums, Acc) ->
    case E of
        #event{op = <<"send">>} ->
            NewSend = {E, Nums},
            extract_timers(Events, NewSend, Calls, Nums, Acc);
        #event{op = <<"start">>} ->
            NewSend = {E, Nums},
            extract_timers(Events, NewSend, Calls, Nums, Acc);
        #event{op = <<"expect", _/binary>>} ->
            {SendE, SendNums} = Send,
            SendLineNo = [SendE#event.lineno | SendNums],
            Callstack = format_calls(Calls, []),
            MatchLineNo = [E#event.lineno | Nums],
            T = #timer{match_lineno = MatchLineNo,
                       match_data = E#event.data,
                       send_lineno = SendLineNo,
                       send_data = SendE#event.data,
                       shell = E#event.shell,
                       callstack = Callstack,
                       status = expected},
            case Acc of
                [AddT | NewAcc] when AddT#timer.status =:= expected ->
                    ok; % Skip prev (expect_add | expect_add_strict) timer
                NewAcc ->
                    ok
            end,
            extract_timers(Events, Send, Calls, Nums, [T | NewAcc]);
        #event{op = <<"timer">>, data = [Data]} ->
            [T | NewAcc] = Acc,
            case parse_timer(Data) of
                {started, MaxTime} when T#timer.status =:= expected  ->
                    NewT = T#timer{max_time = MaxTime,
                                   status = started},
                    extract_timers(Events, Send, Calls, Nums, [NewT|NewAcc]);
                {canceled, Elapsed} when T#timer.status =:= started ->
                    NewT = T#timer{status = matched,
                                   elapsed_time = Elapsed},
                    extract_timers(Events, Send, Calls, Nums, [NewT|NewAcc]);
                {failed, Elapsed} when T#timer.status =:= started ->
                    NewT = T#timer{status = failed,
                                   elapsed_time = Elapsed},
                    extract_timers(Events, Send, Calls, Nums, [NewT|NewAcc])
            end;
        #event{op = <<"invoke_", Macro/binary>>} ->
            NewCalls = [Macro | Calls],
            extract_timers(Events, Send, NewCalls, Nums, Acc);
        #event{op = <<"exit_", Macro/binary>>} ->
            [Macro | NewCalls] = Calls,
            extract_timers(Events, Send, NewCalls, Nums, Acc);
        #event{} ->
            extract_timers(Events, Send, Calls, Nums, Acc);
        #body{events = EventsB} = B ->
            TmpNums = [B#body.invoke_lineno | Nums],
    {NewSend, NewAcc} =
                extract_timers(EventsB, Send, Calls, TmpNums, Acc),
            extract_timers(Events, NewSend, Calls, Nums, NewAcc)
    end;
extract_timers([], Send, _Calls, _Nums, Acc) ->
    {Send, Acc}.

format_calls([<<>>], Acc) ->
    iolist_to_binary(join("->", Acc));
format_calls([H|T], Acc) ->
    format_calls(T, [H | Acc]).

%% ---------
%% new timer
%% ---------
%% started (10 seconds * 1.000 multiplier)
%% canceled (after 552 micro seconds)
%% failed (after 10000764 micro seconds)
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
            {started, CeiledSecs * 1000000};
        ["started", Secs, "seconds", "*", Multiplier, "multiplier"] ->
            CeiledSecs = trunc((list_to_integer(Secs) *
                                    list_to_float(Multiplier)) + 0.5),
            {started, CeiledSecs * 1000000};
        ["canceled", "after", Secs, "seconds"] ->
            {canceled, list_to_integer(Secs) * 1000000};
        ["canceled", "after", MicroSecs, "micro", "seconds"] ->
            {canceled, list_to_integer(MicroSecs)};
        ["failed", "after", Secs, "seconds"] ->
            {failed, list_to_integer(Secs) * 1000000};
        ["failed", "after", MicroSecs, "micro", "seconds"] ->
            {failed, list_to_integer(MicroSecs)}
    end.

timers_to_csv(Timers) ->
    Header = [q(H) || H <- timer_header()],
    ElemLines = [timer_to_elems(T) || T <- Timers,
                                      T#timer.status =/= expected],
    Sep = ";",
    [[join(Sep, E), "\n"] || E <- [Header | ElemLines]].

timer_header() ->
    ["Send", "Match", "Shell", "Macro", "MaxTime", "Status", "Elapsed"].

join(_Sep, []) ->
    [];
join(Sep, [H|T]) ->
    [H|[[Sep, E] || E <- T]].

timer_to_elems(#timer{match_lineno = MatchStack,
                      send_lineno  = SendStack,
                      send_data    = _SendData,
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
     q(atom_to_list(Status)),
     if
         Elapsed =:= undefined,
         Status =:= started ->
             "";
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
                        #timer{match_lineno = Match,
                               send_lineno  = Send,
                               send_data    = undefined,
                               shell        = Shell,
                               callstack    = Macro,
                               macro    = undefined,
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
                {error, ConfigLog, FileReason}
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

parse_result(RawResult) ->
    case RawResult of
        [<<>>, LongResult | Rest] -> ok;
        [LongResult | Rest]       -> ok
    end,
    [_, Result] = binary:split(LongResult, <<": ">>),
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
            <<"WARNING at ", Fail/binary>> ->
                [TagStr = <<"expected", _/binary>>, Expected,
                 <<"actual ", Actual/binary>>, Details | _] = Rest,
                ExpectedTag = list_to_existing_atom(?b2l(TagStr)),
                RawLineNo =
                    case binary:split(Fail, <<":">>) of
                        [_] -> % Main
                            Fail;
                        [Before, After] -> % Nested
                            try
                                _ = list_to_integer(?b2l(Before)),
                                Fail
                            catch
                                _:badarg ->
                                    After
                            end
                    end,
                {quote, Expected2} = unquote(Expected),
                Expected3 = split_quoted_lines(Expected2),
                {quote, Details2} = unquote(Details),
                Details3 = split_quoted_lines(Details2),
                {warning, RawLineNo, ExpectedTag, Expected3, Actual, Details3};
            <<"FAIL at ", Fail/binary>> ->
                [TagStr = <<"expected", _/binary>>, Expected,
                 <<"actual ", Actual/binary>>, Details | _] = Rest,
                ExpectedTag = list_to_existing_atom(?b2l(TagStr)),
                RawLineNo =
                    case binary:split(Fail, <<":">>) of
                        [_] -> % Main
                            Fail;
                        [Before, After] -> % Nested
                            try
                                _ = list_to_integer(?b2l(Before)),
                                Fail
                            catch
                                _:badarg ->
                                    After
                            end
                    end,
                {quote, Expected2} = unquote(Expected),
                Expected3 = split_quoted_lines(Expected2),
                {quote, Details2} = unquote(Details),
                Details3 = split_quoted_lines(Details2),
                {fail, RawLineNo, ExpectedTag, Expected3, Actual, Details3};
            <<"FAIL as ", _/binary>> = Fail->
                {error, [Fail]}
        end,
    %% io:format("Result: ~p\n", [R]),
    {result, R}.

split_quoted_lines(IoList) ->
    Normalized = lux_utils:replace(?l2b(IoList), [{quoted_crlf, <<"\n">>}]),
    lux_utils:split_lines(Normalized).

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
    ConfigFile = lux_utils:join(LogDir, Base ++ ".config.log"),
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
                     io_lib:format("~s~s: ~s\n",
                                   [?TAG("stdin  log file"), Name, Stdin]),
                     io_lib:format("~s~s: ~s\n",
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
            io_lib:format("~s\n", [?TAG(Tag)]);
        [String] ->
            io_lib:format("~s~s\n", [?TAG(Tag), to_printable(String)]);
        [String|Strings] ->
            [io_lib:format("~s~s\n", [?TAG(Tag), to_printable(String)]),
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
format_val_choice(Tag, Val, []) ->
    [lists:flatten(io_lib:format("~s~w\n", [?TAG(Tag), Val]))].

try_format_val(_Tag, Val = undefined, _Type) ->
    [atom_to_list(Val)];
try_format_val(Tag, Val, Type) ->
    case Type of
        string when is_list(Val) ->
            [Val];
        binary when is_binary(Val) ->
            [?b2l(Val)];
        {atom, _Atoms} when is_atom(Val) ->
            [atom_to_list(Val)];
        {integer, _Min, _Max} when is_integer(Val) ->
            [?i2l(Val)];
        {integer, _Min, _Max} when Val =:= infinity ->
            [atom_to_list(Val)];
        {std_list, SubTypes} ->
            [hd(format_val_choice(Tag, V, SubTypes)) || V <- Val];
        {reset_list, SubTypes} when is_list(SubTypes) ->
            [hd(format_val_choice(Tag, V, SubTypes)) || V <- Val]
    end.

safe_format(Fd, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    safe_write(Fd, IoList).

safe_write(OptFd, IoList) when is_list(IoList) ->
    safe_write(OptFd, ?l2b(IoList));
safe_write(OptFd, Bin) when is_binary(Bin) ->
    case OptFd of
        undefined ->
            ok = io:format("~s", [Bin]),
            Bin;
        Fd ->
            ok = file:write(Fd, Bin),
            Bin
    end.

double_write(Progress, Fd, IoList) when Fd =/= undefined ->
    Bin = safe_write(Fd, IoList),
    case Progress of
        silent -> Bin;
        _      -> safe_write(undefined, Bin)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

safe_format(Progress, LogFun, Fd, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    safe_write(Progress, LogFun, Fd, IoList).

safe_write(Progress, LogFun, Fd, IoList) when is_list(IoList) ->
    safe_write(Progress, LogFun, Fd, ?l2b(IoList));
safe_write(Progress, LogFun, Fd0, Bin) when is_binary(Bin) ->
    case Fd0 of
        undefined ->
            Fd = Fd0,
            Verbose = false;
        {Verbose, Fd} ->
            ok
    end,
    case Progress of
        silent ->
            ok;
        summary ->
            ok;
        brief ->
            ok;
        doc ->
            ok;
        compact when Verbose ->
            try
                io:format("~s", [?b2l(Bin)])
            catch
                _:CReason ->
                    exit({safe_write, compact, Bin, CReason})
            end;
        compact ->
            ok;
        verbose when Verbose ->
            try
                io:format("~s", [dequote(?b2l(Bin))])
            catch
                _:VReason ->
                    exit({safe_write, verbose, Bin, VReason})
            end;
        verbose ->
            ok
    end,
    case Fd of
        undefined ->
            try
                case LogFun(Bin) of
                    <<_/binary>> ->
                        ok;
                    BadRes ->
                        exit({safe_write, log_fun, Bin, BadRes})
                end
            catch
                _:LReason ->
                    exit({safe_write, log_fun, Bin, LReason})
            end;
        _ ->
            try file:write(Fd, Bin) of
                ok ->
                    ok;
                {error, FReason} ->
                    Str = file:format_error(FReason),
                    io:format("\nfile write failed: ~s\n", [Str]),
                    exit({safe_write, file, Fd, Bin, {error, FReason}})
            catch
                _:WReason ->
                    exit({safe_write, file, Bin, WReason})
            end
    end,
    Bin.
