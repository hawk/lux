%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2015 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_log).

-export([is_temporary/1, parse_summary_log/1, parse_run_summary/3,
         open_summary_log/2, close_summary_tmp_log/1, close_summary_log/2,
         write_config_log/2,
         write_results/4, print_results/4, parse_result/1, pick_result/2,
         safe_format/3, safe_write/2, double_write/2,
         open_event_log/5, close_event_log/1, write_event/4, scan_events/1,
         parse_events/2, parse_io_logs/2,
         open_config_log/3, close_config_log/2,
         safe_format/5, safe_write/4]).

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
                    LogIoList = io_lib:format("~s~s\n",
                                              [?TAG(?SUMMARY_TAG),
                                               ?SUMMARY_LOG_VERSION]),
                    safe_write(SummaryFd, LogIoList),
                    StdoutIoList = io_lib:format("~s~s\n",
                                            ["summary log", SummaryLog]),
                    safe_write(undefined, StdoutIoList),
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
    try
        do_parse_summary_log(SummaryLog)
    catch
        error:Reason ->
            ReasonStr =
                lists:flatten(io_lib:format("\nERROR in ~s\n~p\n\~p\n",
                                            [SummaryLog,
                                             Reason,
                                             erlang:get_stacktrace()])),
            io:format("~s\n", [ReasonStr]),
            {error, SummaryLog, ReasonStr}
    end.

do_parse_summary_log(SummaryLog) ->
    case read_log(SummaryLog, ?SUMMARY_TAG) of
        {ok, ?SUMMARY_LOG_VERSION, Sections} ->
            %% Latest version
            LogDir = filename:dirname(SummaryLog),
            ConfigLog = filename:join([LogDir, "lux_config.log"]),
            {ok, RawConfig} = scan_config(ConfigLog),
            ArchConfig = parse_config(RawConfig),
            {ok, Result} = parse_summary_result(LogDir),
            {Cases, EventLogs} = split_cases(Sections, [], []),
            {ok, FI} = file:read_file_info(SummaryLog),
            {ok, Result, [{test_group, "", Cases}], ArchConfig, FI, EventLogs};
        {ok, Version, _Sections} ->
            {error, SummaryLog,
             "Illegal summary log version: " ++ binary_to_list(Version)};
        {error, FileReason, _} ->
            {error, SummaryLog, file:format_error(FileReason)}
    end.

read_log(Log, ExpectedTag) ->
  case file:read_file(Log) of
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
                              binary_to_list(ExpectedTag) ++
                              " expected",
                          {error, Reason, Bin}
                  end;
              _ ->
                  Reason =
                      "Illegal log type: " ++
                      binary_to_list(ExpectedTag) ++
                      " expected",
                  {error, Reason, Bin}
          end;
      {error, Reason} ->
          {error, file:format_error(Reason), <<"">>}
  end.

write_log(File, Tag, Version, Sections) ->
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
    case binary:split(NameRow, <<": ">>) of
        [<<"test case", _/binary>>, Name] ->  ok;
        [<<>>]                            -> Name = <<"unknown">>
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
                    EventLog = binary_to_list(RawEventLog),
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

parse_run_summary(HtmlFile, SummaryLog, Res) ->
    try
        do_parse_run_summary(HtmlFile, SummaryLog, Res)
    catch
        error:Reason ->
            ReasonStr =
                lists:flatten(io_lib:format("\nERROR in ~s\n~p\n\~p\n",
                                            [SummaryLog,
                                             Reason,
                                             erlang:get_stacktrace()])),
            io:format("~s\n", [ReasonStr]),
            {error, SummaryLog, ReasonStr}
    end.

do_parse_run_summary(HtmlFile, SummaryLog, Res) ->
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
    write_log(ConfigLog, ?CONFIG_TAG, ?CONFIG_LOG_VERSION, [PrettyConfig]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Results

parse_summary_result(LogDir) ->
    ResultLog = filename:join([LogDir, "lux_result.log"]),
    case read_log(ResultLog, ?RESULT_TAG) of
        {ok, ?RESULT_LOG_VERSION, Sections} ->
            %% Latest version
            {ok, split_result(Sections)};
        {ok, Version, _Sections} ->
            {error, ResultLog,
             "Illegal result log version: " ++ binary_to_list(Version)};
        {error, Reason, _} ->
            {error, ResultLog, Reason}
    end.

write_results(SummaryLog, Summary, Results, Warnings) ->
    LogDir = filename:dirname(SummaryLog),
    ResultFile = filename:join([LogDir, "lux_result.log"]),
    TmpResultFile = ResultFile++".tmp",
    case file:open(TmpResultFile, [write]) of
        {ok, Fd} ->
            try
                safe_format(Fd, "~s~s\n",
                            [?TAG(?RESULT_TAG), ?RESULT_LOG_VERSION]),
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
    SuccessScripts = pick_result(Results, success),
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
    case pick_result(Results, skip) of
        [] ->
            ok;
        SkipScripts ->
            result_format(Fd, "~s~p\n",
                          [?TAG("skipped"), length(SkipScripts)]),
            [result_format(Fd, "\t~s:~s\n", [F, L]) || {F, L} <- SkipScripts]
    end.


print_warning(Fd, Warnings) ->
    case pick_result(Warnings, warning) of
        [] ->
            ok;
        WarnScripts ->
            result_format(Fd, "~s~p\n",
                          [?TAG("warnings"), length(WarnScripts)]),
            [result_format(Fd, "\t~s:~s\n", [F, L]) || {F, L} <- WarnScripts]
    end.

print_fail(Fd, Results) ->
    case pick_result(Results, fail) of
        [] ->
            ok;
        FailScripts ->
            result_format(Fd, "~s~p\n",
                          [?TAG("failed"),  length(FailScripts)]),
            [result_format(Fd, "\t~s:~s\n", [F, L]) || {F, L} <- FailScripts]
    end.

print_error(Fd, Results) ->
    case pick_result(Results, error) of
        [] ->
            ok;
        ErrorScripts ->
            result_format(Fd, "~s~p\n",
                          [?TAG("errors"), length(ErrorScripts)]),
            [result_format(Fd, "\t~s:~s\n", [F, L]) ||
                {F, L} <- ErrorScripts]
    end.

pick_result(Results, Outcome) when Outcome =:= error ->
    [{Script, FullLineNo} ||
        {error, Script, _LogDir, FullLineNo, _Reason} <- Results];
pick_result(Warnings, Outcome) when Outcome =:= warning ->
    [{Script, FullLineNo} ||
        {warning, Script, FullLineNo, _Reason} <- Warnings];
pick_result(Results, Outcome) ->
    [{Script, FullLineNo} ||
        {ok, Script, O, FullLineNo, _Events} <- Results,
        O =:= Outcome].

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
                        "~s~s\n", [?TAG(?EVENT_TAG), ?EVENT_LOG_VERSION]),
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
    Data = io_lib:format(Format, Args),
    Data2 = lux_utils:normalize_newlines(Data),
    Data3 = io_lib:format("~s(~p): ~p ~s\n", [Shell, LineNo, Op, Data2]),
    safe_write(Progress, LogFun, Fd, Data3).

scan_events(EventLog) ->
    case read_log(EventLog, ?EVENT_TAG) of
        {ok, ?EVENT_LOG_VERSION, Sections} ->
            do_scan_events(EventLog, Sections);
        {ok, Version, _Sections} ->
            {error, EventLog,
             "Illegal event log version: " ++ binary_to_list(Version)};
        {error, FileReason, _} ->
            {error, EventLog, file:format_error(FileReason)}
    end.

do_scan_events(EventLog, EventSections) ->
    EventSections2 = [binary:split(S, <<"\n">>, [global]) ||
                         S <- EventSections],
    case EventSections2 of
        [[Script], EventBins, ResultBins] -> ok;
        [[Script], ResultBins]            -> EventBins = []
    end,
    Dir = filename:dirname(EventLog),
    Base = filename:basename(EventLog, ".event.log"),
    ConfigLog = filename:join([Dir, Base ++ ".config.log"]),
    case scan_config(ConfigLog) of
        {ok, [ConfigSection]} ->
            LogBins = [],
            ConfigBins = binary:split(ConfigSection, <<"\n">>, [global]),
            {ok, EventLog, ConfigLog,
             Script, EventBins, ConfigBins, LogBins, ResultBins};
        {ok, [ConfigSection,LogSection]} ->
            ConfigBins = binary:split(ConfigSection, <<"\n">>, [global]),
            LogBins = binary:split(LogSection, <<"\n">>, [global]),
            {ok, EventLog, ConfigLog,
             Script, EventBins, ConfigBins, LogBins, ResultBins};
        {ok, Version, _Sections} ->
            {error, ConfigLog,
             "Illegal config log version: " ++ binary_to_list(Version)};
        {error, FileReason} ->
            {error, ConfigLog, file:format_error(FileReason)}
    end.

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

parse_other_file(EndTag, SubFile, Events, Acc) ->
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
    LineNo = list_to_integer(binary_to_list(RawLineNo)),
    FirstLineNo = list_to_integer(binary_to_list(RawFirstLineNo)),
    LastLineNo = list_to_integer(binary_to_list(RawLastLineNo)),
    SubEvents2 = parse_events(SubEvents, []),
    E = {body, LineNo, FirstLineNo, LastLineNo, SubFile3, SubEvents2},
    do_parse_events(Events2, [E | Acc]).

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
    case read_log(ConfigLog, ?CONFIG_TAG) of
        {ok, ?CONFIG_LOG_VERSION, Sections} ->
            {ok, Sections};
        {ok, Version, _Sections} ->
            {error, ConfigLog,
             "Illegal config log version: " ++ binary_to_list(Version)};
        {error, FileReason, _} ->
            {error, ConfigLog, file:format_error(FileReason)}
    end.

parse_config([ConfigSection|_]) when is_binary(ConfigSection) ->
    ConfigSection;
parse_config(ConfigSection) when is_binary(ConfigSection) ->
    ConfigSection.

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
                {fail, Script, RawLineNo, Expected3, Actual, Details3};
            <<"FAIL as ", _/binary>> = Fail->
                {error, [Fail]}
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

open_config_log(LogDir, Script, ConfigData) ->
    Base = filename:basename(Script),
    ConfigFile = filename:join([LogDir, Base ++ ".config.log"]),
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
                {ok, _Pos, Types} = lux_interpret:config_type(Tag),
                lists:flatten(format_config(Tag, Val, Types))
        end,
    lists:map(Fun, Config).

format_config(Tag, Val, Types) ->
    if
        Tag =:= builtin ->
            ok;
        Tag =:= system_env ->
            ok;
        true ->
            ok
    end,
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
                      false -> [$\\, string:right(integer_to_list(Char),3, $0)]
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
    io_lib:format("~s~p\n", [?TAG(Tag), Val]).

try_format_val(_Tag, Val = undefined, _Type) ->
    [atom_to_list(Val)];
try_format_val(Tag, Val, Type) ->
    case Type of
        string when is_list(Val) ->
            [Val];
        binary when is_binary(Val) ->
            [binary_to_list(Val)];
        {atom, _Atoms} when is_atom(Val) ->
            [atom_to_list(Val)];
        {integer, _Min, _Max} when is_integer(Val) ->
            [integer_to_list(Val)];
        {integer, _Min, _Max} when Val =:= infinity ->
            [atom_to_list(Val)];
        {env_list, SubTypes} ->
            [hd(format_val_choice(Tag, V, SubTypes)) || V <- Val];
        {reset_list, SubTypes} when is_list(SubTypes) ->
            [hd(format_val_choice(Tag, V, SubTypes)) || V <- Val]
    end.

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
