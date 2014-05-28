%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2014 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_log).

-export([is_temporary/1, parse_summary_log/1, parse_run_summary/3,
         open_summary_log/2, close_summary_tmp_log/1, close_summary_log/2,
         write_config_log/2,
         write_results/4, print_results/4, parse_result/1,
         safe_format/3, safe_write/2, double_write/2,
         open_event_log/5, close_event_log/1, write_event/4, scan_events/1,
         parse_events/2, parse_config/1, parse_io_logs/2,
         open_config_log/3, close_config_log/2,
         safe_format/5, safe_write/4]).

-include_lib("kernel/include/file.hrl").
-include("lux.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Summary log

is_temporary(SummaryLog) ->
    case lists:reverse(SummaryLog) of
        "pmt."++_ -> true;
        _         -> false
    end.

open_summary_log(SummaryLog, ExtendRun) ->
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
                    IoList =
                        io_lib:format("~s~s\n",
                                      [?TAG("summary log"),
                                       ?SUMMARY_LOG_VERSION]),
                    safe_write(SummaryFd, IoList),
                    IoList2 =
                        io_lib:format("~s~s\n",
                                      [?TAG("summary log"), SummaryLog]),
                    safe_write(undefined, IoList2),
                    {ok, Exists, SummaryFd};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

close_summary_log(SummaryFd, SummaryLog) ->
    ok = close_summary_tmp_log(SummaryFd),
    TmpSummaryLog = SummaryLog ++ ".tmp",
    ok = file:rename(TmpSummaryLog, SummaryLog).

close_summary_tmp_log(SummaryFd) ->
    file:close(SummaryFd).

parse_summary_log(SummaryLog) ->
    case file:read_file(SummaryLog) of
        {ok, <<"summary log       : ", ?SUMMARY_LOG_VERSION, "\n",
               LogBin/binary>>} ->
            %% Latest version
            Sections =
                case LogBin of
                    <<"">> ->
                        []; % No test cases yet
                    <<"\n", LogBin2/binary>> ->
                        binary:split(LogBin2, <<"\n\n">>, [global])
                end,
            LogDir = filename:dirname(SummaryLog),
            ConfigLog = filename:join([LogDir, "lux_config.log"]),
            {ok, RawConfig} = scan_config(ConfigLog),
            ArchConfig = parse_config(RawConfig),
            {ok, Result} = parse_summary_result(LogDir),
            {Cases, EventLogs} = split_cases(Sections, [], []),
            {ok, FI} = file:read_file_info(SummaryLog),
            {ok, Result, [{test_group, "", Cases}], ArchConfig, FI, EventLogs};
        {ok, LogBin} ->
            %% 0.1
            Sections = binary:split(LogBin, <<"\n\n">>, [global]),
            [_AbsSummaryLog, ArchConfig | Rest] = Sections,
            [RawResult | Rest2] = lists:reverse(Rest),
            Result = split_result(RawResult),
            {Groups, EventLogs} = split_groups(Rest2, [], []),
            {ok, FI} = file:read_file_info(SummaryLog),
            {ok, Result, Groups, ArchConfig, FI, EventLogs};
        {error, FileReason} ->
            {error, file:format_error(FileReason)}
    end.

split_result(Result) ->
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

split_groups([GroupEnd | Groups], Acc, EventLogs) ->
    Pred = fun(Case) ->
                   case binary:split(Case, <<": ">>) of
                       %% BUGBUG: Kept for backwards compatibility a while
                       [<<"test suite begin", _/binary>> |_] -> false;
                       [<<"test group begin", _/binary>> |_] -> false;
                       _ -> true
                   end
           end,
    Split = lists:splitwith(Pred, Groups),
    {Cases, [GroupBegin | Groups2]} = Split,
    [_, Group] = binary:split(GroupBegin, <<": ">>),
    [_, Group] = binary:split(GroupEnd, <<": ">>),
    {Cases2, EventLogs2} =
        split_cases(lists:reverse(Cases), [], EventLogs),
    split_groups(Groups2, [{test_group, Group, Cases2} | Acc], EventLogs2);
split_groups([], Acc, EventLogs) ->
    {Acc, EventLogs}.

split_cases([Case | Cases], Acc, EventLogs) ->
    [NameRow | Sections] = binary:split(Case, <<"\n">>, [global]),
    [<<"test case", _/binary>>, Name] = binary:split(NameRow, <<": ">>),
    case Sections of
        [] ->
            Res = {result_case, Name, <<"ERROR">>, <<"unknown">>},
            split_cases(Cases, [Res | Acc], EventLogs);
        [Reason] ->
            Res =
                case binary:split(Reason,    <<": ">>) of
                    [<<"result", _/binary>>, Reason2] ->
                        {result_case, Name, Reason2, Reason};
                    [<<"error", _/binary>>, Reason2] ->
                        {result_case, Name, <<"ERROR">>, Reason2};
                    [<<>>] ->
                        {result_case, Name, <<"ERROR">>, <<"unknown">>}
                end,
            split_cases(Cases, [Res | Acc], EventLogs);
        [ScriptRow, LogRow | DocAndResult] ->
            case {binary:split(LogRow,  <<": ">>), ScriptRow} of
                {[<<"event log", _/binary>>, RawEventLog], _} ->
                    {Doc, ResultCase} = split_doc(DocAndResult, []),
                    Result = parse_result(ResultCase),
                    EventLog = binary_to_list(RawEventLog),
                    HtmlLog = EventLog ++ ".html",
                    Res = {test_case, Name, EventLog, Doc, HtmlLog, Result},
                    split_cases(Cases, [Res | Acc], [EventLog|EventLogs]);
                {_, R = <<"result", _/binary>>} ->
                    Result = parse_result([R]),
                    Res = {test_case, Name, "", [], "", Result},
                    split_cases(Cases, [Res | Acc], EventLogs)
            end
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

parse_run_summary(HtmlFile, SummaryLog, Res) ->
    HtmlDir = filename:dirname(HtmlFile),
    {ok, Cwd} = file:get_cwd(),
    CN0 = ?DEFAULT_CONFIG_NAME,
    R = #run{test = ?DEFAULT_SUITE,
             id = ?DEFAULT_RUN,
             result = fail,
             log = lux_utils:drop_prefix(HtmlDir, SummaryLog),
             start_time = ?DEFAULT_TIME,
             hostname = ?DEFAULT_HOSTNAME,
             config_name = CN0,
             run_dir = Cwd,
             repos_rev = ?DEFAULT_REV,
             details = []},
    case Res of
        {ok, Result, Groups, ArchConfig, FI, _EventLogs} ->
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
            Config = lists:zf(Split,
                              binary:split(ArchConfig, <<"\n">>, [global])),
            Ctime0 = FI#file_info.ctime,
            Ctime =  list_to_binary(lux_utils:datetime_to_string(Ctime0)),
            StartTime = find_config(<<"start time">>, Config, Ctime),
            HostName = find_config(<<"hostname">>, Config, R#run.hostname),
            ConfigName0 = find_config(<<"architecture">>, Config, CN0),
            ConfigName =
                if
                    ConfigName0 =/= CN0,
                    ConfigName0 =/= <<"undefined">> ->
                        ConfigName0;
                    true ->
                        find_config(<<"config name">>, Config, CN0)
                end,
            Suite = find_config(<<"suite">>, Config, R#run.test),
            RunId = find_config(<<"run">>, Config, R#run.id),
            ReposRev = find_config(<<"revision">>, Config, R#run.repos_rev),
            RunDir = binary_to_list(find_config(<<"workdir">>,
                                                Config,
                                                list_to_binary(Cwd))),
            HtmlDir = filename:dirname(HtmlFile),
            Cases = [parse_run_case(HtmlDir, RunDir, StartTime,
                                    HostName, ConfigName,
                                    Suite, RunId, ReposRev, Case) ||
                        {test_group, _Group, Cases} <- Groups,
                        Case <- Cases],
            R#run{test        = Suite,
                  id          = RunId,
                  result      = run_result(Result),
                  start_time  = StartTime,
                  hostname    = HostName,
                  config_name = ConfigName,
                  run_dir     = RunDir,
                  repos_rev   = ReposRev,
                  details     = Cases};
        {error, SummaryLog, _ReasonStr} ->
            R
    end.

parse_run_case(HtmlDir, RunDir, Start, Host, ConfigName,
               Suite, RunId, ReposRev,
               {test_case, Name, Log, _Doc, _HtmlLog, CaseRes}) ->
    File = lux_utils:drop_prefix(RunDir, Name),
    File2 = drop_some_dirs(File),
    #run{test = <<Suite/binary, ":", File2/binary>>,
         id = RunId,
         result = run_result(CaseRes),
         log = lux_utils:drop_prefix(HtmlDir, Log),
         start_time = Start,
         hostname = Host,
         config_name = ConfigName,
         run_dir = RunDir,
         repos_rev = ReposRev,
         details = []};
parse_run_case(_HtmlDir, RunDir, Start, Host, ConfigName, Suite,
               RunId, ReposRev,
               {result_case, Name, Res, _Reason}) ->
    File = lux_utils:drop_prefix(RunDir, Name),
    File2 = drop_some_dirs(File),
    #run{test = <<Suite/binary, ":", File2/binary>>,
         id = RunId,
         result = run_result(Res),
         log = ?DEFAULT_LOG,
         start_time = Start,
         hostname = Host,
         config_name = ConfigName,
         run_dir = RunDir,
         repos_rev = ReposRev,
         details = []}.

run_result({result, Res, _}) ->
    run_result(Res);
run_result({result, Res}) ->
    run_result(Res);
run_result(Res) ->
    case Res of
        success                                                -> success;
        {skip, _}                                              -> skip;
        {fail, _Script, _LineNo, _Expected, _Actual, _Details} -> fail;
        {error, _Reason}                                       -> fail;
        <<"SUCCESS">>                                          -> success;
        <<"SKIP", _/binary>>                                   -> skip;
        <<"FAIL", _/binary>>                                   -> fail;
        <<"ERROR", _/binary>>                                  -> fail
    end.

drop_some_dirs(File) when is_binary(File) -> % BUGBUG: Temporary solution
    Q = <<"lux">>,
    Comp = filename:split(File),
    case lists:dropwhile(fun(E) -> E =/= Q end, Comp) of
        [Q | Rest] -> filename:join(Rest);
        _Rest      -> File
    end.

find_config(Key, Tuples, Default) ->
    case lists:keyfind(Key, 1, Tuples) of
        false         -> Default;
        {_, Hostname} -> Hostname
    end.

write_config_log(ConfigLog, ConfigData) ->
    PrettyConfig = format_config(ConfigData),
    file:write_file(ConfigLog,
                    [
                     format_config([{'config log', ?CONFIG_LOG_VERSION}]),
                     "\n",
                     PrettyConfig
                    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Results

parse_summary_result(LogDir) ->
    ResultLog = filename:join([LogDir, "lux_result.log"]),
    case file:read_file(ResultLog) of
        {ok, <<"result log        : ",
               ?RESULT_LOG_VERSION,
               "\n\n",
               RawResult/binary>>} ->
            %% Latest version
            {ok, split_result(RawResult)};
        {error, FileReason} ->
            {error, ResultLog, file:format_error(FileReason)}
    end.

write_results(SummaryLog, Summary, Results, Warnings) ->
    LogDir = filename:dirname(SummaryLog),
    ResultFile = filename:join([LogDir, "lux_result.log"]),
    TmpResultFile = ResultFile++".tmp",
    case file:open(TmpResultFile, [write]) of
        {ok, Fd} ->
            try
                safe_format(Fd, "~s~s\n",
                            [?TAG("result log"), ?RESULT_LOG_VERSION]),
                IsTmp = is_temporary(SummaryLog),
                print_results({IsTmp, Fd}, Summary, Results, Warnings),
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

print_results(Fd, Summary, Results, Warnings) ->
    %% Display most important results last
    result_format(Fd, "\n", []),
    SuccessScripts =
        [Script || {ok, Script, success, _FullLineNo, _Events} <- Results],
    result_format(Fd, "~s~p\n",
                  [?TAG("successful"),
                   length(SuccessScripts)]),
    print_skip(Fd, Results),
    print_warning(Fd, Warnings),
    print_fail(Fd, Results),
    print_error(Fd, Results),
    result_format(Fd, "~s~s\n",
                  [?TAG("summary"),
                   [string:to_upper(Char) ||
                       Char <- atom_to_list(Summary)]]).

print_skip(Fd, Results) ->
    case [{Script, FullLineNo} ||
             {ok, Script, skip, FullLineNo, _Events} <- Results] of
        [] ->
            ok;
        SkipScripts ->
            result_format(Fd, "~s~p\n",
                          [?TAG("skipped"), length(SkipScripts)]),
            [result_format(Fd, "\t~s:~s\n", [F, L]) || {F, L} <- SkipScripts]
    end.


print_warning(Fd, Warnings) ->
    case [{Script, FullLineNo} ||
             {warning, Script, FullLineNo, _String} <- Warnings] of
        [] ->
            ok;
        WarnScripts ->
            result_format(Fd, "~s~p\n",
                          [?TAG("warnings"), length(WarnScripts)]),
            [result_format(Fd, "\t~s:~s\n", [F, L]) || {F, L} <- WarnScripts]
    end.

print_fail(Fd, Results) ->
    case [{Script, FullLineNo} ||
             {ok, Script, fail, FullLineNo, _Events} <- Results] of
        [] ->
            ok;
        FailScripts ->
            result_format(Fd, "~s~p\n",
                          [?TAG("failed"),  length(FailScripts)]),
            [result_format(Fd, "\t~s:~s\n", [F, L]) || {F, L} <- FailScripts]
    end.

print_error(Fd, Results) ->
    case [{Script, FullLineNo} ||
             {error, Script, FullLineNo, _String} <- Results] of
        [] ->
            ok;
        ErrorScripts ->
            result_format(Fd, "~s~p\n",
                          [?TAG("errors"), length(ErrorScripts)]),
            [result_format(Fd, "\t~s:~s\n", [F, L]) ||
                {F, L} <- ErrorScripts]
    end.

result_format({IsTmp, Fd}, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    case Fd of
        undefined     -> list_to_binary(IoList);
        Fd when IsTmp -> double_write(Fd, IoList);
        Fd            -> safe_write(Fd, IoList)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Event log

open_event_log(LogDir, Script, Progress, LogFun, Verbose) ->
    Base = filename:basename(Script),
    EventLog = filename:join([LogDir, Base ++ ".event.log"]),
    case file:open(EventLog, [write]) of
        {ok, EventFd} ->
            safe_format(Progress, LogFun, {Verbose, EventFd},
                        "~s~s\n", [?TAG("event log"), ?EVENT_LOG_VERSION]),
            safe_format(Progress, LogFun, {Verbose, EventFd},
                        "\n~s\n\n", [filename:absname(Script)]),
            {ok, EventLog, EventFd};
        {error, Reason} ->
            {error, Reason}
    end.

close_event_log(EventFd) ->
    file:close(EventFd).

write_event(Progress, LogFun, Fd, {event, LineNo, Shell, Op, "", []}) ->
    Data = io_lib:format("~s(~p): ~p\n", [Shell, LineNo, Op]),
    safe_write(Progress, LogFun, Fd, Data);
write_event(Progress, LogFun, Fd, {event, LineNo, Shell, Op, Format, Args}) ->
    Data = io_lib:format("~s(~p): ~p "++Format++"\n",
                         [Shell, LineNo, Op | Args]),
    safe_write(Progress, LogFun, Fd, Data).

scan_events(EventLog) ->
    case file:read_file(EventLog) of
        {ok, <<"event log         : ",
               ?EVENT_LOG_VERSION,
               "\n\n",
               LogBin/binary>>} ->
            scan_events_0_1(EventLog, LogBin);
        {ok, LogBin} ->
            scan_events_old(EventLog, LogBin);
        {error, FileReason} ->
            {error, EventLog, file:format_error(FileReason)}
    end.

scan_events_0_1(EventLog, LogBin) ->
    EventSections = binary:split(LogBin, <<"\n\n">>, [global]),
    EventSections2 = [binary:split(S, <<"\n">>, [global]) ||
                         S <- EventSections],
    case EventSections2 of
        [[Script], EventBins, ResultBins] -> ok;
        [[Script], ResultBins]            -> EventBins = []
    end,
    Dir = filename:dirname(EventLog),
    Base = filename:basename(EventLog, ".event.log"),
    ConfigLog = filename:join([Dir, Base ++ ".config.log"]),
    case file:read_file(ConfigLog) of
        {ok, <<"config log        : ",
               ?CONFIG_LOG_VERSION,
               "\n",
               ConfigBin/binary>>} ->
            ConfigSections = binary:split(ConfigBin, <<"\n\n">>, [global]),
            ConfigSections2 = [binary:split(S, <<"\n">>, [global])
                               || S <- ConfigSections],
            [ConfigBins, LogBins] = ConfigSections2,
            {ok, EventLog, ConfigLog,
             Script, EventBins, ConfigBins, LogBins, ResultBins};
        {error, FileReason} ->
            {error, ConfigLog, file:format_error(FileReason)}
    end.

scan_events_old(EventLog, LogBin) ->
    Sections = binary:split(LogBin, <<"\n\n">>, [global]),
    Sections2 = [binary:split(S, <<"\n">>, [global]) ||
                    S <- Sections],
    case Sections2 of
        [ScriptBins, EventBins, ConfigBins, LogBins, ResultBins] ->
            ok;
        [ScriptBins, EventBins, ConfigBins, [<<>>|ResultBins]] ->
            LogBins = [];
        [ScriptBins, [<<>>|ConfigBins], [<<>>|ResultBins]] ->
            LogBins = [],
            EventBins = []
    end,
    [Script] = ScriptBins,
    {ok, EventLog, <<"">>, Script, EventBins, ConfigBins, LogBins, ResultBins}.

parse_events([<<>>], Acc) ->
    %% Error case
    lists:reverse(Acc);
parse_events(Events, Acc) ->
    do_parse_events(Events, Acc).

do_parse_events([<<"include_begin ", SubFile/binary>> | Events], Acc) ->
    %% include_begin 11 47 53 demo/test.include
    %% include_end 11 47 53 demo/test.include
    Pred = fun(E) ->
                   case E of
                       <<"include_end ", SubFile/binary>> ->
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
    LineNo = list_to_integer(binary_to_list(RawLineNo)),
    FirstLineNo = list_to_integer(binary_to_list(RawFirstLineNo)),
    LastLineNo = list_to_integer(binary_to_list(RawLastLineNo)),
    SubEvents2 = parse_events(SubEvents, []),
    E = {include, LineNo, FirstLineNo, LastLineNo, SubFile3, SubEvents2},
    do_parse_events(Events2, [E | Acc]);
do_parse_events([Event | Events], Acc) ->
    [Prefix, Details] = binary:split(Event, <<"): ">>),
    [Shell, RawLineNo] = binary:split(Prefix, <<"(">>),
    LineNo = list_to_integer(binary_to_list(RawLineNo)),
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
                        split_lines(C);
                    {plain, C} ->
                        %% cli(70): timer start (10 seconds)
                        [C]
                end
        end,
    E = {event, LineNo, Shell, Op, Data},
    do_parse_events(Events, [E | Acc]);
do_parse_events([], Acc) ->
    lists:reverse(Acc).

split_lines(<<"">>) ->
    [];
split_lines(Bin) ->
    Opts = [global],
    Replace = fun(NL, B) -> binary:replace(B , NL, <<"\n">>, Opts) end,
    NLs = [<<"[\\r\\n]+">>, <<"\\r\\n">>,
           <<"\n\r">>, <<"\r\n">>,
           <<"\\n">>, <<"\\r">>],
    Normalized = lists:foldl(Replace, Bin, NLs),
    binary:split(Normalized, <<"\n">>, Opts).

scan_config(ConfigLog) ->
    case file:read_file(ConfigLog) of
        {ok, <<"config log        : ",
               ?CONFIG_LOG_VERSION,
               "\n\n",
               LogBin/binary>>} ->
            {ok, LogBin};
        {error, FileReason} ->
            {error, ConfigLog, file:format_error(FileReason)}
    end.

parse_config(RawConfig) ->
    %% io:format("Config: ~p\n", [RawConfig]),
    RawConfig.

parse_io_logs([StdinLog, StdoutLog | Logs], Acc) ->
    [_, Shell, Stdin] = binary:split(StdinLog, <<": ">>, [global]),
    [_, Shell, Stdout] = binary:split(StdoutLog, <<": ">>, [global]),
    L = {log, Shell, Stdin, Stdout},
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
            <<"SKIP as ",Skip/binary>> ->
                {skip, [Skip | Rest]};
            <<"ERROR at ", Error/binary>> ->
                [RawLineNo, Reason] = binary:split(Error, <<":">>),
                {error_line, RawLineNo, [Reason | Rest]};
            <<"ERROR ", Reason/binary>> ->
                {error, [Reason | Rest]};
            <<"FAIL at ", Fail/binary>> ->
                [<<"expected">>, Expected,
                 <<"actual ", Actual/binary>>, Details | _] = Rest,
                [Script, RawLineNo] = binary:split(Fail, <<":">>),
                {quote, Expected2} = unquote(Expected),
                Expected3 = split_lines(Expected2),
                {quote, Details2} = unquote(Details),
                Details3 = split_lines(Details2),
                {fail, Script, RawLineNo, Expected3, Actual, Details3}
        end,
    %% io:format("Result: ~p\n", [R]),
    {result, R}.

unquote(Bin) ->
    Quote = <<"\"">>,
    Size = byte_size(Bin)-2,
    case Bin of
        <<Quote:1/binary, Plain:Size/binary, Quote:1/binary>> ->
            {quote, Plain};
        Plain ->
            {plain, Plain}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Config log

open_config_log(LogDir, Script, Config) ->
    Base = filename:basename(Script),
    ConfigFile = filename:join([LogDir, Base ++ ".config.log"]),
    case filelib:ensure_dir(ConfigFile) of
        ok ->
            case file:open(ConfigFile, [write]) of
                {ok, ConfigFd} ->
                    Data = format_config(Config),
                    ok = file:write(ConfigFd, Data),
                    ok = file:write(ConfigFd, "\n"),
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
    end.

close_config_log(ConfigFd, Logs) ->
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
        fun({Tag, Type, Val}) ->
                case Type of
                    string ->
                        io_lib:format("~s~s\n", [?TAG(Tag), Val]);
                    dict ->
                        Val2 = [lux_utils:to_string(E) || E <- Val],
                        io_lib:format("~s~p\n", [?TAG(Tag), Val2]);
                    term ->
                        io_lib:format("~s~p\n", [?TAG(Tag), Val])
                end;
           ({Tag, Val}) when is_list(Val) ->
                io_lib:format("~s~s\n", [?TAG(Tag), Val]);
           ({Tag, Val}) ->
                io_lib:format("~s~p\n", [?TAG(Tag), Val])
        end,
    lists:map(Fun, Config).

safe_format(Fd, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    safe_write(Fd, IoList).

safe_write(OptFd, IoList) when is_list(IoList) ->
    safe_write(OptFd, list_to_binary(IoList));
safe_write(OptFd, Bin) when is_binary(Bin) ->
    case OptFd of
        undefined ->
            ok = io:format(Bin),
            Bin;
        Fd ->
            ok = file:write(Fd, Bin),
            Bin
    end.

double_write(Fd, IoList) when Fd =/= undefined ->
    Bin = safe_write(Fd, IoList),
    safe_write(undefined, Bin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

safe_format(Progress, LogFun, Fd, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    safe_write(Progress, LogFun, Fd, IoList).

safe_write(Progress, LogFun, Fd, IoList) when is_list(IoList) ->
    safe_write(Progress, LogFun, Fd, list_to_binary(IoList));
safe_write(Progress, LogFun, Fd0, Bin) when is_binary(Bin) ->
    case Fd0 of
        undefined  ->
            Fd = Fd0,
            Verbose = false;
        {Verbose, Fd} ->
            ok
    end,
    case Progress of
        silent ->
            ok;
        brief ->
            ok;
        doc ->
            ok;
        compact when Verbose ->
            try
                io:format("~s", [binary_to_list(Bin)])
            catch
                _:CReason ->
                    exit({safe_write, verbose, Bin, CReason})
            end;
        compact ->
            ok;
        verbose when Verbose ->
            try
                io:format("~s", [lux_utils:dequote(binary_to_list(Bin))])
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
