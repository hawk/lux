%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2012 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_html).

-export([annotate_log/1, history/2]).

-include_lib("kernel/include/file.hrl").

-record(astate, {log_dir, log_file}).

annotate_log(LogFile) ->
    annotate_log(true, LogFile).

annotate_log(IsRecursive, LogFile) ->
    LogFile2 = filename:absname(LogFile),
    IsEventLog = lists:suffix("event.log", LogFile2),
    LogDir = filename:dirname(LogFile2),
    A = #astate{log_dir = LogDir, log_file = LogFile2},
    Res =
        case IsEventLog of
            true  -> annotate_event_log(A);
            false -> annotate_summary_log(IsRecursive, A)
        end,
    case Res of
        {ok, IoList} ->
            safe_write_file(LogFile2 ++ ".html", IoList);
        {error, _File, _ReasonStr} = Error ->
            Error
    end.

safe_write_file(File, IoList) ->
    case filelib:ensure_dir(File) of
        ok ->
            TmpFile = File ++ ".tmp",
            case file:write_file(TmpFile, IoList) of
                ok ->
                    case file:rename(TmpFile, File) of
                        ok ->
                            ok;
                        {error, FileReason} ->
                            {error, File, file:format_error(FileReason)}
                    end;
                {error, FileReason} ->
                    {error, TmpFile, file:format_error(FileReason)}
            end;
        {error, FileReason} ->
            {error, filename:dirname(File), file:format_error(FileReason)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Annotate a summary log and all its event logs

annotate_summary_log(IsRecursive, A) ->
    case parse_summary_log(IsRecursive, A) of
        {ok, SummaryLog, Result, Groups, ArchConfig, _FileInfo} ->
            Html = html_groups(A, SummaryLog, Result, Groups, ArchConfig),
            {ok, Html};
        {error, _File, _Reason} = Error ->
            Error
    end.

parse_summary_log(IsRecursive, #astate{log_file = SummaryLog}) ->
    try
        case file:read_file(SummaryLog) of
            {ok, LogBin} ->
                Sections = binary:split(LogBin, <<"\n\n">>, [global]),
                [Summary, ArchConfig | Rest] = Sections,
                [_, SummaryLog2] = binary:split(Summary, <<": ">>),
                [Result | Rest2] = lists:reverse(Rest),
                Result2 = split_result(Result),
                Groups = split_groups(IsRecursive, Rest2, []),
                {ok, FI} = file:read_file_info(SummaryLog),
                {ok, SummaryLog2, Result2, Groups, ArchConfig, FI};
            {error, FileReason} ->
                {error, SummaryLog, file:format_error(FileReason)}
        end
    catch
        error:Reason2 ->
            ReasonStr =
                lists:flatten(io_lib:format("ERROR in ~s\n~p\n\~p\n",
                                            [SummaryLog,
                                             Reason2,
                                             erlang:get_stacktrace()])),
            io:format("~s\n", [ReasonStr]),
            {error, SummaryLog, ReasonStr}
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

split_groups(IsRecursive, [GroupEnd | Groups], Acc) ->
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
    Cases2 = split_cases(IsRecursive, lists:reverse(Cases), []),
    split_groups(IsRecursive, Groups2, [{test_group, Group, Cases2} | Acc]);
split_groups(_IsRecursive, [], Acc) ->
    Acc.

split_cases(IsRecursive, [Case | Cases], Acc) ->
    [NameRow | Sections] = binary:split(Case, <<"\n">>, [global]),
    [<<"test case", _/binary>>, Name] = binary:split(NameRow, <<": ">>),
    case Sections of
        [] ->
            Res = {result_case, Name, <<"ERROR">>, <<"unknown">>},
            split_cases(IsRecursive, Cases, [Res | Acc]);
        [Reason] ->
            Res =
                case binary:split(Reason,    <<": ">>) of
                    [<<"result", _/binary>>, Reason2] ->
                        {result_case, Name, Reason2, Reason};
                    [<<"error", _/binary>>, Reason2] ->
                        {result_case, Name, <<"ERROR">>, Reason2}
                end,
            split_cases(IsRecursive, Cases, [Res | Acc]);
        [_ScriptRow, LogRow | DocAndResult] ->
            [<<"event log", _/binary>>, RawEventLog] =
                binary:split(LogRow,  <<": ">>),
            EventLog = binary_to_list(RawEventLog),
            case IsRecursive of
                true ->
                    case annotate_log(EventLog) of
                        ok ->
                            ok;
                        {error, _, Reason} ->
                            io:format("ERROR in ~s\n\~p\n", [EventLog, Reason])
                    end;
                false ->
                    ignore
            end,
            {Doc, ResultCase} = split_doc(DocAndResult, []),
            Result = parse_result(ResultCase),
            HtmlLog = EventLog ++ ".html",
            Res = {test_case, Name, EventLog, Doc, HtmlLog, Result},
            split_cases(IsRecursive, Cases, [Res | Acc])
    end;
split_cases(_IsRecursive, [], Acc) ->
    lists:reverse(Acc).

split_doc([H|T] = Rest, AccDoc) ->
    case binary:split(H, <<": ">>) of
        [<<"doc", _/binary>>, Doc] ->
            split_doc(T, [Doc | AccDoc]);
        _ ->
            {lists:reverse(AccDoc), Rest}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Return summary log as HTML

html_groups(A, SummaryLog, Result, Groups, ArchConfig) ->
    Dir = filename:basename(filename:dirname(binary_to_list(SummaryLog))),
    RelSummaryLog = drop_prefix(A, SummaryLog),
    [
     html_header(["Lux summary log (", Dir, ")"]),
     html_href("h2", "Raw summary log: ", "", RelSummaryLog, RelSummaryLog),
     html_href("h3", "", "", "#suite_config", "Suite configuration"),
     html_summary_result(A, Result, Groups),
     html_groups2(A, Groups),
     html_anchor("h2", "", "suite_config", "Suite configuration:"),
     html_div(<<"annotate">>, ArchConfig),
     html_footer()
    ].

html_summary_result(A, {result, Summary, Sections}, Groups) ->
    %% io:format("Sections: ~p\n", [Sections]),
    [
     "\n<h2>Result: ", Summary, "</h2>\n",
     "<div class=case><pre>",
     [html_summary_section(A, S, Groups) || S <- Sections],
     "</pre></div>"
    ].

html_summary_section(A, {section, Slogan, Count, Files}, Groups) ->
    [
     "<strong>", Slogan, ": ", Count, "</strong>\n",
     case Files of
         [] ->
             [];
         _ ->
             [
              "<div class=annotate><pre>",
              [html_summary_file(A, F, Groups) || F <- Files],
              "</pre></div>"
             ]
     end
    ].

html_summary_file(A, {file, File, LineNo}, Groups) ->
    Files =
        [HtmlLog ||
            {test_group, _Group, Cases} <- Groups,
            {test_case, Name, _Log, _Doc, HtmlLog, _Res} <- Cases,
            File =:= Name],
    RelFile = drop_prefix(A, File),
    Label = [RelFile, ":", LineNo],
    case Files of
        [] ->
            [html_href("", "#" ++ RelFile, Label), "\n"];
        [HtmlLog|_] ->
            [html_href("", drop_prefix(A, HtmlLog), Label), "\n"]
    end.

html_groups2(A, [{test_group, Group, Cases} | Groups]) ->
    [
     "\n\n<h2>Test group: ", drop_prefix(A, Group), "</h2>\n",
     html_cases(A, Cases),
     html_groups2(A, Groups)
    ];
html_groups2(_A, []) ->
    [].

html_cases(A, [{test_case, Name, EventLog, Doc, HtmlLog, Res} | Cases]) ->
    Tag = "a",
    RelFile = drop_prefix(A, Name),
    RelHtmlLog = drop_prefix(A, HtmlLog),
    RelEventLog = drop_prefix(A, EventLog),
    [
     html_anchor(RelFile, ""),
     "\n",
     html_href("h3", "Test case: ", "", RelHtmlLog, RelFile),
     "\n<div class=case><pre>",
     html_doc(Tag, Doc),
     html_href(Tag, "Raw event log: ", "", RelEventLog, RelEventLog),
     html_href(Tag, "Annotated script: ", "", RelHtmlLog, RelHtmlLog),
     html_result(Tag, Res, RelHtmlLog),
     "\n",
     "</pre></div>",
     html_cases(A, Cases)
    ];
html_cases(A, [{result_case, Name, Reason, Details} | Cases]) ->
    Tag = "a",
    File = drop_prefix(A, Name),
    [
     html_anchor(File, ""),
     html_href("h3", "Test case: ", "", File, File),
     "\n<div class=case><pre>",
     "\n<", Tag, ">Result: <strong>", Reason, "</strong></", Tag, ">\n",
     "\n",
     Details,
     "</pre></div>",
     html_cases(A, Cases)
    ];
html_cases(_A, []) ->
    [].

html_doc(_Tag, []) ->
    [];
html_doc(Tag, [Slogan | Desc]) ->
    [
     "\n<", Tag, ">Description: <strong>", Slogan,"</strong></", Tag, ">\n",
     case Desc of
         [] ->
             [];
         _ ->
             html_div(<<"annotate">>, expand_lines(Desc))
     end
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Annotate a lux with events from the log

annotate_event_log(#astate{log_file = EventLog} = A) ->
    try
        case scan(EventLog) of
            {ok, EventLog2, ConfigLog,
             Script, RawEvents, RawConfig, RawLogs, RawResult} ->
                Events = parse_events(RawEvents, []),
                Config = parse_config(RawConfig),
                Logs = parse_logs(RawLogs, []),
                Result = parse_result(RawResult),
                {Annotated, Files} =
                    interleave_code(A, Events, Script, 1, 999999, [], []),
                Html = html_events(A, EventLog2, ConfigLog, Script, Result,
                                   lists:reverse(Files),
                                   Logs, Annotated, Config),
                {ok, Html};
            {error, _File, _ReasonStr} = Error ->
                Error
        end
    catch
        error:Reason2 ->
            ReasonStr =
                lists:flatten(io_lib:format("ERROR in ~s\n~p\n\~p\n",
                                            [EventLog,
                                             Reason2,
                                             erlang:get_stacktrace()])),
            io:format("~s\n", [ReasonStr]),
            {error, EventLog, ReasonStr}
    end.

scan(EventLog) ->
    case file:read_file(EventLog) of
        {ok, <<"event log         : 0.1\n\n", LogBin/binary>>} ->
            scan_0_1(EventLog, LogBin);
        {ok, LogBin} ->
            scan_old(EventLog, LogBin);
        {error, FileReason} ->
            {error, EventLog, file:format_error(FileReason)}
    end.

scan_0_1(EventLog, LogBin) ->
    EventSections = binary:split(LogBin, <<"\n\n">>, [global]),
    EventSections2 = [binary:split(S, <<"\n">>, [global]) || S <- EventSections],
    case EventSections2 of
        [[Script], EventBins, ResultBins] -> ok;
        [[Script], ResultBins]            -> EventBins = []
    end,
    Dir = filename:dirname(EventLog),
    Base = filename:basename(EventLog, ".event.log"),
    ConfigLog = filename:join([Dir, Base ++ ".config.log"]),
    case file:read_file(ConfigLog) of
        {ok, <<"config log        : 0.1\n", ConfigBin/binary>>} ->
            ConfigSections = binary:split(ConfigBin, <<"\n\n">>, [global]),
            ConfigSections2 = [binary:split(S, <<"\n">>, [global])
                               || S <- ConfigSections],
            [ConfigBins, LogBins] = ConfigSections2,
            {ok, EventLog, ConfigLog,
             Script, EventBins, ConfigBins, LogBins, ResultBins};
        {error, FileReason} ->
            {error, ConfigLog, file:format_error(FileReason)}
    end.

scan_old(EventLog, LogBin) ->
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
    [Item | RawContents] = binary:split(Details, <<" ">>),
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
    E = {event, LineNo, Item, Shell, Data},
    do_parse_events(Events, [E | Acc]);
do_parse_events([], Acc) ->
    lists:reverse(Acc).

split_lines(Bin) ->
    Opts = [global],
    Bin2 = binary:replace(Bin, <<"[\\r\\n]+">>, <<"\n">>, Opts),
    Bin3 = binary:replace(Bin2, <<"\\r">>, <<"">>, Opts),
    binary:split(Bin3, <<"\\n">>, Opts).

parse_config(RawConfig) ->
    %% io:format("Config: ~p\n", [RawConfig]),
    RawConfig.

parse_logs([StdinLog, StdoutLog | Logs], Acc) ->
    [_, Shell, Stdin] = binary:split(StdinLog, <<": ">>, [global]),
    [_, Shell, Stdout] = binary:split(StdoutLog, <<": ">>, [global]),
    L = {log, Shell, Stdin, Stdout},
    %% io:format("Logs: ~p\n", [L]),
    parse_logs(Logs, [L | Acc]);
parse_logs([<<>>], Acc) ->
    lists:reverse(Acc);
parse_logs([], Acc) ->
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

interleave_code(A, Events, Script, FirstLineNo, MaxLineNo, InclStack, Files) ->
    ScriptComps = lux_utils:filename_split(Script),
    case file:read_file(Script) of
        {ok, ScriptBin} ->
            NewScript = orig_script(A, Script),
            case file:write_file(NewScript, ScriptBin) of
                ok ->
                    ok;
                {error, FileReason} ->
                    ReasonStr = binary_to_list(Script) ++ ": " ++
                        file:format_error(FileReason),
                    erlang:error(ReasonStr)
            end,
            CodeLines = binary:split(ScriptBin, <<"\n">>, [global]),
            CodeLines2 =
                try
                    lists:nthtail(FirstLineNo-1, CodeLines)
                catch
                    _X:_Y ->
                        CodeLines
                end,
            Files2 =
                case lists:keymember(Script, 2, Files) of
                    false -> [{file, Script, NewScript} | Files];
                    true  -> Files
                end,
            do_interleave_code(A, Events, ScriptComps, CodeLines2,
                               FirstLineNo, MaxLineNo, [], InclStack, Files2);
        {error, FileReason} ->
            ReasonStr = binary_to_list(Script) ++ ": " ++
                file:format_error(FileReason),
            io:format("ERROR(lux): ~s\n", [ReasonStr]),
            do_interleave_code(A, Events, ScriptComps, [],
                               FirstLineNo, MaxLineNo, [], InclStack, Files)
    end.

do_interleave_code(A, [{include, LineNo, FirstLineNo, LastLineNo,
                        SubScript, SubEvents} |
                       Events],
                   ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, InclStack, Files) ->
    InclStack2 = [{ScriptComps, LineNo} | InclStack],
    {SubAnnotated, Files2} =
        interleave_code(A, SubEvents, SubScript, FirstLineNo, LastLineNo,
                        InclStack2, Files),
    Event = {include_html, InclStack2, FirstLineNo, SubScript, SubAnnotated},
    do_interleave_code(A, Events, ScriptComps, CodeLines, CodeLineNo,
                       MaxLineNo, [Event | Acc], InclStack, Files2);
do_interleave_code(A, [{event, SingleLineNo, Item, Shell, Data},
                       {event, SingleLineNo, Item, Shell, Data2} | Events],
                   ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, InclStack, Files) when Item =:= <<"recv">>,
                                               Data2 =/= [<<"timeout">>]->
    %% Combine two chunks of recv data into one in order to improve readability
    [Last | Rev] = lists:reverse(Data),
    [First | Rest] = Data2,
    Data3 = lists:reverse(Rev, [<<Last/binary, First/binary>> | Rest]),
    do_interleave_code(A, [{event, SingleLineNo, Item, Shell, Data3} | Events],
                       ScriptComps, CodeLines, CodeLineNo,
                       MaxLineNo, Acc, InclStack, Files);
do_interleave_code(A, [{event, SingleLineNo, _Item, Shell, Data} | Events],
                   ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, InclStack, Files) ->
    {CodeLines2, CodeLineNo2, Code} =
        pick_code(ScriptComps, CodeLines, CodeLineNo, SingleLineNo,
                  [], InclStack),
    InclStack2 = [{ScriptComps, SingleLineNo} | InclStack],
    Acc2 = [{event_html, InclStack2, _Item, Shell, Data}] ++ Code ++ Acc,
    do_interleave_code(A, Events, ScriptComps, CodeLines2, CodeLineNo2,
                       MaxLineNo, Acc2, InclStack, Files);
do_interleave_code(_A, [], ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, InclStack, Files) ->
    X = pick_code(ScriptComps, CodeLines, CodeLineNo, MaxLineNo, [], InclStack),
    {_Skipped, _CodeLineNo, Code} = X,
    {lists:reverse(Code ++ Acc), Files}.

pick_code(ScriptComps, [Line | Lines], CodeLineNo, LineNo, Acc, InclStack)
  when LineNo >= CodeLineNo ->
    InclStack2 = [{ScriptComps, CodeLineNo} | InclStack],
    pick_code(ScriptComps, Lines, CodeLineNo+1, LineNo,
              [{code_html, InclStack2, Line} | Acc], InclStack);
pick_code(_ScriptComps, Lines, CodeLineNo, _LineNo, Acc, _InclStack) ->
    {Lines, CodeLineNo, Acc}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Return event log as HTML

html_events(A, EventLog, ConfigLog, Script, Result, Files, Logs, Annotated, Config) ->
    Dir = filename:basename(filename:dirname(EventLog)),
    [
     html_header(["Lux event log (", Dir, ")"]),
     "\n<h2>", drop_prefix(A, Script), "</h2>\n",
     html_result("h2", Result, ""),
     html_href("h3", "", "", "#config", "Script configuration"),
     "\n<h3>Source files: ",
     html_files(A, Files),
     "\n</h3>",
     html_href("h3", "", "", drop_prefix(A, EventLog), "Raw event log"),
     html_href("h3", "", "", drop_prefix(A, ConfigLog), "Raw config log"),
     html_logs(A, Logs),
     "\n<h2>Annotated source code</h2>\n",
     html_code(A, Annotated),
     html_anchor("h2", "", "config", "Script configuration:"),
     html_config(Config),
     html_footer()
    ].

html_result(Tag, {result, Result}, HtmlLog) ->
    case Result of
        success ->
            ["\n<", Tag, ">Result: <strong>SUCCESS</strong></", Tag, ">\n"];
        skip ->
            ["\n<", Tag, ">Result: <strong>SKIP</strong></", Tag, ">\n"];
        {error_line, RawLineNo, Reason} ->
            Anchor = RawLineNo,
            [
             "\n<", Tag, ">Result: <strong>ERROR at line ",
             html_href("", [HtmlLog, "#", Anchor], Anchor),
             "<h3>Reason</h3>",
             html_div(<<"annotate">>, expand_lines(Reason))
            ];
        {error, Reason} ->
            [
             "\n<", Tag, ">Result: <strong>ERROR</strong></", Tag, ">\n",
             "<h3>Reason</h3>",
             html_div(<<"annotate">>, expand_lines(Reason))
            ];
        {fail, _Script, RawLineNo, Expected, Actual, Details} ->
            Anchor = RawLineNo,
            [
             "\n<", Tag, ">Result: <strong>FAILED at line ",
             html_href("", [HtmlLog, "#", Anchor], Anchor),
             "</strong></", Tag, ">\n",
             "<h3>Expected</h3>",
             html_div(<<"annotate">>, expand_lines(Expected)),
             "<h3>Actual: ", html_cleanup(Actual), "</h3>",
             html_div(<<"annotate">>, expand_lines(Details))
            ]
    end.

html_config(Config) ->
    html_div(<<"annotate">>, expand_lines(Config)).

html_logs(A, [{log, Shell, Stdin, Stdout} | Logs]) ->
    [
     "\n<h3>Logs for shell ", Shell, ": ",
     html_href("", drop_prefix(A, Stdin), "stdin"),
     " ",
     html_href("", drop_prefix(A, Stdout), "stdout"),
     "</h3>\n",
     html_logs(A, Logs)
    ];
html_logs(_A, []) ->
    [].

html_code(A, Annotated) ->
    [
     "\n<div class=code><pre>\n",
     html_code2(A, Annotated, code),
     "</pre></div>\n"
    ].

html_code2(A, [Ann | Annotated], Prev) ->
    case Ann of
        {code_html, LineNoStack, Code} ->
            Curr = code,
            FullLineNo = lux_utils:full_lineno(LineNoStack),
            [
             html_toggle_div(Curr, Prev),
             html_anchor(FullLineNo, FullLineNo), ": ",
             html_cleanup(Code),
             "\n",
             html_code2(A, Annotated, Curr)
            ];
        {event_html, LineNoStack, Item, Shell, Data} ->
            Curr = event,
            FullLineNo = lux_utils:full_lineno(LineNoStack),
            Html = [Shell, "(", FullLineNo, "): ", Item, " "],
            [
             html_toggle_div(Curr, Prev),
             html_cleanup(Html),
             html_opt_div(Item, Data),
             html_code2(A, Annotated, Curr)
            ];
        {include_html, LineNoStack, _MacroLineNo, SubScript, SubAnnotated} ->
            FullLineNo = lux_utils:full_lineno(LineNoStack),
            RelSubScript = drop_prefix(A, SubScript),
            [
             html_toggle_div(code, Prev),
             html_toggle_div(event, code),
             html_opt_div(<<"include">>,
                          [<<"entering file: ", RelSubScript/binary>>]),
             "</pre></div>",
             html_code(A, SubAnnotated),
             html_toggle_div(code, event),
             html_anchor(FullLineNo, FullLineNo), ": ",
             html_toggle_div(event, code),
             html_opt_div(<<"include">>,
                          [<<"exiting file: ", RelSubScript/binary>>]),
             "</pre></div>",
             html_code(A, Annotated)
            ]
    end;
html_code2(_A, [], _Prev) ->
    [].

html_toggle_div(Curr, Prev) ->
    case {Curr, Prev} of
        {code, code}   -> "";
        {code, event}  -> "</pre></div>\n<div class=code><pre>";
        {event, event} -> "";
        {event, code}  -> "</pre></div>\n<div class=annotate><pre>\n"
    end.

html_opt_div(Item, Data) ->
    Html = expand_lines(Data),
    case Item of
        <<"send">>   -> html_div(Item, Html);
        <<"recv">>   -> html_div(Item, Html);
        <<"expect">> -> html_div(Item, Html);
        <<"skip">>   -> html_div(Item, Html);
        <<"match">>  -> html_div(Item, Html);
        _            -> html_cleanup(Html)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History

-define(DEFAULT_LOG, <<"unknown">>).
-define(DEFAULT_HOSTNAME, <<"unknown">>).
-define(DEFAULT_CONFIG_NAME, <<"unknown">>).
-define(DEFAULT_SUITE, <<"unknown">>).
-define(DEFAULT_RUN, <<"unknown">>).
-define(DEFAULT_REV, <<"">>).
-define(DEFAULT_TIME, <<"yyyy-mm-dd hh:mm:ss">>).

-record(run,
        {id,
         test,
         result,
         log,
         start_time,
         hostname,
         config_name,
         run_dir,
         repos_rev,
         details}).

history(TopDir, HtmlFile) ->
    TopDir2 = filename:absname(TopDir),
    HtmlFile2 = filename:absname(HtmlFile),
    HtmlDir = filename:dirname(HtmlFile2),
    AllRuns = parse_summary_logs(HtmlDir, TopDir2, []),
    io:format("~p test runs", [length(AllRuns)]),
    SplitHosts = keysplit(#run.hostname, AllRuns),
    SplitConfigNames = keysplit(#run.config_name, AllRuns),
    LatestRuns = latest_runs(SplitHosts),
    IoList =
        [
         html_history_header(AllRuns, SplitConfigNames, SplitHosts, HtmlFile),
         html_history_latest(LatestRuns, HtmlDir),
         html_history_all(AllRuns, HtmlDir),
         html_history_config_names(SplitConfigNames, HtmlDir),
         html_history_hosts(SplitHosts, HtmlDir),
         html_footer()
        ],
    safe_write_file(HtmlFile, IoList).

html_history_header(AllRuns, SplitConfigNames, SplitHosts, HtmlFile) ->
    Dir = filename:basename(filename:dirname(HtmlFile)),
    case lists:keysort(#run.repos_rev, AllRuns) of
        [] ->
            Default = <<"unknown">>,
            FirstRev = Default,
            LatestRev = Default,
            FirstTime = Default,
            LatestTime = Default,
            N = 0;
        SortedRuns ->
            FirstRev = (hd(SortedRuns))#run.repos_rev,
            LatestRev = (lists:last(SortedRuns))#run.repos_rev,
            FirstRuns = [R || R <- SortedRuns, R#run.repos_rev =:= FirstRev],
            LatestRuns = [R || R <- SortedRuns, R#run.repos_rev =:= LatestRev],
            FirstRuns2 = lists:keysort(#run.repos_rev, FirstRuns),
            LatestRuns2 = lists:keysort(#run.repos_rev, LatestRuns),
            FirstTime = (hd(FirstRuns2))#run.start_time,
            LatestTime = (hd(LatestRuns2))#run.start_time,
            N = integer_to_list(length(SortedRuns))
    end,
    [
     html_header(["Lux history (", Dir, ")"]),
     [
      "<h1>Lux history (", Dir, ") generated at ",
      lux_utils:now_to_string(erlang:now()),
      "</h1>"],

     "<h3>", N, " runs within this range of repository revisions</h3>\n",
     "<table border=0>",
     "<tr>",
     "<td>Latest:</td><td><strong>", LatestRev, "</strong></td>",
     "<td>at ", LatestTime, "</td>\n",
     "</tr>",
     "<tr>",
     "<td>First:</td><td><strong>", FirstRev, "</strong></td>",
     "<td>at ", FirstTime, "</td>\n",
     "</tr>",
     "</table>\n\n"

     "<h3>Configuration names</h3>",
     "  <table border=1>\n",
     "    <tr>\n",
     [["<td>", html_href("", "#" ++ ConfigName, ConfigName), "</td>"] ||
         {ConfigName, _} <- SplitConfigNames],
     "    </tr>\n",
     "  </table>\n",

     "<h3>Hosts</h3>",
     "  <table border=1>\n",
     "    <tr>\n",
     [["<td>", html_href("", "#" ++ Host, Host), "</td>"] ||
         {Host, _} <- SplitHosts],
     "    </tr>\n",
     "  </table>\n",

     html_history_legend()
    ].

html_history_legend() ->
    [
     "<h3>Legend</h3>\n",
     "  <table border=1>\n",
     "    <tr>\n",
     html_history_td("First fail", fail, "left"),
     html_history_td("Secondary fails on same host", secondary_fail, "left"),
     html_history_td("Skipped", none, "left"),
     html_history_td("Success", success, "left"),
     html_history_td("No data", no_data, "left"),
     "    </tr>\n",
     "  </table>\n"
    ].

html_history_latest(LatestRuns, HtmlDir) ->
    [
     "<h3>Latest run on each host</h3>\n",
     html_history_table("Latest", "All test suites", LatestRuns, HtmlDir, false)
    ].

html_history_all(AllRuns, HtmlDir) ->
    [
     "<h3>All runs</h3>\n",
     html_history_table("All", "All test suites", AllRuns, HtmlDir, false)
    ].

html_history_config_names(SplitConfigNames, HtmlDir) ->
    [html_history_double_table(ConfigName, "ConfigName: " ++ ConfigName, Runs, HtmlDir) ||
        {ConfigName, Runs} <- SplitConfigNames].

html_history_double_table(Name, Label, AllRuns, HtmlDir) ->
    Details = [D#run{details = [D]}  || R <- AllRuns, D <- R#run.details],
    [
     "<br><br>\n",
     ["<h3>", html_anchor(Name, Label), "</h3>\n"],
     html_history_table(Name, "All test suites", AllRuns, HtmlDir, false),
     html_history_table(Name, "Failed test cases", Details, HtmlDir, true)
    ].

html_history_table(_Name, Grain, Runs, HtmlDir, SuppressAllSuccess) ->
    SplitIds  = keysplit(#run.id, Runs),
    SplitIds2 = [{Id, lists:sort(fun test_run/2, IdRuns)} ||
                    {Id, IdRuns} <- SplitIds],
    SplitIds3 = lists:reverse(lists:sort(fun test_run/2, SplitIds2)), % rev
    SplitTests  = keysplit(#run.test, Runs),
    SplitTests2 = [{Test, lists:sort(fun test_run/2, TestRuns)} ||
                      {Test, TestRuns} <- SplitTests],
    [
     "  <table border=1>\n",
     "    <tr>\n",
     "      <td align=\"left\" rowspan=\"2\"><strong>",Grain,"</strong></td>\n",
     [["      <td>", Rev,
       "<br>", "<strong>", Id, "</strong>",
       "<br>", Time,
       "</td>\n"] ||
         {Id, [#run{start_time = Time, repos_rev = Rev} |_ ]} <- SplitIds3],
     "    </tr>\n",
     "    <tr>\n",
     [["      <td>",
       "<strong>", html_href("", "#" ++ Host, Host), "</strong>",
       "<br>", html_href("", "#" ++ ConfigName, ConfigName),
       "</td>\n"] ||
         {_, [#run{hostname = Host, config_name = ConfigName} |_ ]} <- SplitIds3],
     "    </tr>\n",
     [html_history_row(Test, TestRuns, SplitIds3, HtmlDir, SuppressAllSuccess)
      || {Test, TestRuns} <- SplitTests2],
     "  </table>\n"
    ].

html_history_hosts(SplitHosts, HtmlDir) ->
    [html_history_double_table(Host,
                               ["Host: ", Host,
                                " (", (hd(Runs))#run.config_name, ")"],
                               Runs,
                               HtmlDir) ||
        {Host, Runs} <- SplitHosts].

html_history_row(Test, Runs, SplitIds, HtmlDir, SuppressAllSuccess) ->
    RevRuns = lists:reverse(lists:keysort(#run.id, Runs)),
    Cells = [html_history_cell(Id, RevRuns, HtmlDir) || {Id, _} <- SplitIds],
    ValidRes = [Res || {Res, _} <- Cells, Res =/= no_data],
    case lists:usort(ValidRes) of
        [] when SuppressAllSuccess ->
            [];
        [success] when SuppressAllSuccess ->
            [];
        [none] when SuppressAllSuccess ->
            [];
        _ ->
            SplitHosts = keysplit(#run.hostname, Runs),
            LatestRuns = latest_runs(SplitHosts),
            Fun = fun(#run{result = New}, Acc) ->
                          lux_utils:summary(Acc, New)
                  end,
            WorstRes = lists:foldl(Fun, no_data, LatestRuns),
            [
             "    <tr>\n",
             html_history_td(Test, WorstRes, "left"),
             [Text || {_, Text} <- Cells],
             "    </tr>\n"
            ]
    end.

latest_runs(SplitRuns) ->
    Fun = fun({_Tag, Runs}) ->
                  SplitTests = keysplit(#run.test, Runs),
                  [lists:last(lists:keysort(#run.id, TestRuns)) ||
                      {_Test, TestRuns} <- SplitTests]
          end,
    lists:flatten(lists:map(Fun, SplitRuns)).

test_run(#run{repos_rev = R1}, #run{repos_rev = R2}) when R1 < R2 ->
    true;
test_run(#run{repos_rev = R1}, #run{repos_rev = R2}) when R1 > R2 ->
    false;
test_run(#run{start_time = R1}, #run{start_time = R2}) when R1 < R2 ->
    true;
test_run(#run{start_time = R1}, #run{start_time = R2}) when R1 > R2 ->
    false;
test_run(#run{hostname = R1}, #run{hostname = R2}) when R1 < R2 ->
    true;
test_run(#run{hostname = R1}, #run{hostname = R2}) when R1 > R2 ->
    false;
test_run(#run{id = R1}, #run{id = R2}) ->
    R1 =< R2;
test_run({_, []}, {_, [#run{}|_]}) ->
    true;
test_run({_, [#run{}|_]}, {_, []}) ->
    false;
test_run({_, [#run{}=R1|_]}, {_, [#run{}=R2|_]}) ->
    %% Test on first run
    test_run(R1, R2).

html_history_cell(Id, Runs, HtmlDir) ->
    case lists:keyfind(Id, #run.id, Runs) of
        false ->
            Text= "-",
            Res = no_data;
        Run ->
            RunN  = length([run  || #run{result = R} <- Run#run.details,
                                    R =/= skip]),
            FailN = length([fail || #run{result = R} <- Run#run.details,
                                    R =:= fail]),
            FailCount = lists:concat([FailN, " (", RunN, ")"]),
            Text =
                case Run#run.log of
                    ?DEFAULT_LOG ->
                        FailCount;
                    Log ->
                        html_href("", [drop_prefix(HtmlDir, Log), ".html"],
                                  FailCount)
                end,
            ThisRes =
                case RunN of
                    0 -> none;
                    _ -> Run#run.result
                end,
            HostRuns = [R || R <- Runs, R#run.hostname =:= Run#run.hostname],
            Pred = fun(#run{id = SomeId}) -> SomeId =/= Id end,
            {_Before, [Run | Rest]} = lists:splitwith(Pred, HostRuns),
            case Rest of
                [] -> PrevRes = success;
                [#run{result = PrevRes} | _] -> ok
            end,
            case {ThisRes, PrevRes} of
                {fail, fail} -> Res = secondary_fail;
                {fail, _}    -> Res = fail;
                {Res, _}     -> ok
            end
    end,
    {Res, html_history_td(Text, Res)}.

html_history_td(Text, Res) ->
    html_history_td(Text, Res, "right").

html_history_td(Text, skip, Align) ->
    html_history_td(Text, none, Align);
html_history_td(Text, Res, Align) ->
    ["    <td class=", atom_to_list(Res), " align=\"", Align, "\"> ",
     Text,
     "</td>\n"].

multi_member([H | T], Files) ->
    case lists:member(H, Files) of
        true ->
            {true, H};
        false ->
            multi_member(T, Files)
    end;
multi_member([], _Files) ->
    false.

parse_summary_logs(HtmlDir, Dir, Acc) ->
    Cands = ["lux.skip",
             "lux_summary.log",
             "lux_summary.log.tmp",
             "qmscript.skip",
             "qmscript_summary.log",
             "qmscript_summary.log.tmp",
             "qmscript.summary.log"],
    do_parse_summary_logs(HtmlDir, Dir, Acc, Cands).

do_parse_summary_logs(HtmlDir, Dir, Acc, Cands) ->
    %% io:format("~s\n", [Dir]),
    case file:list_dir(Dir) of
        {ok, Files} ->
            case multi_member(Cands, Files) of
                {true, Base} ->
                    case lists:suffix(".log", Base) of
                        true ->
                            %% A summary log
                            File = filename:join([Dir, Base]),
                            SumA = #astate{log_dir = Dir, log_file = File},
                            io:format(".", []),
                            Res = parse_summary_log(false, SumA),
                            [parse_run_summary(HtmlDir, Res) | Acc];
                        false ->
                            io:format("s", []),
                            %% Skip
                            Acc
                    end;
                false ->
                    %% No interesting file found. Search subdirs
                    Fun =
                        fun("latest_run", A) ->
                                %% Symlink
                                A;
                           (File, A) ->
                                SubDir = filename:join([Dir, File]),
                                do_parse_summary_logs(HtmlDir, SubDir, A, Cands)
                        end,
                    lists:foldl(Fun, Acc, Files)
            end;
        {error, _Reason} ->
            %% Not a dir or problem to read dir
            Acc
    end.

parse_run_summary(HtmlDir,
                  {ok, SummaryLog, SummaryRes, Groups, ArchConfig, FI}) ->
    Split =
        fun(Config) ->
                case binary:split(Config, <<": ">>, []) of
                    [Key, Val] ->
                        {true, {lux_utils:strip_trailing_whitespaces(Key),
                                Val}};
                    _          ->
                        false
                end
        end,
    Config = lists:zf(Split, binary:split(ArchConfig, <<"\n">>, [global])),
    Ctime =
        list_to_binary(lux_utils:datetime_to_string(FI#file_info.ctime)),
    StartTime = find_config(<<"start time">>, Config, Ctime),
    Hostname = find_config(<<"hostname">>, Config, ?DEFAULT_HOSTNAME),
    ConfigName0 = find_config(<<"architecture">>, Config, ?DEFAULT_CONFIG_NAME),
    ConfigName =
        if
            ConfigName0 =/= ?DEFAULT_CONFIG_NAME,
            ConfigName0 =/= <<"undefined">> ->
                ConfigName0;
            true ->
                find_config(<<"config name">>, Config, ?DEFAULT_CONFIG_NAME)
        end,
    Suite = find_config(<<"suite">>, Config, ?DEFAULT_SUITE),
    RunId = find_config(<<"run">>, Config, ?DEFAULT_RUN),
    ReposRev = find_config(<<"revision">>, Config, ?DEFAULT_REV),
    {ok, Cwd} = file:get_cwd(),
    RunDir = binary_to_list(find_config(<<"workdir">>,
                                        Config,
                                        list_to_binary(Cwd))),
    Cases = [parse_run_case(HtmlDir, RunDir, StartTime, Hostname, ConfigName,
                            Suite, RunId, ReposRev, Case) ||
                {test_group, _Group, Cases} <- Groups,
                Case <- Cases],
    #run{test = Suite,
         id = RunId,
         result = run_result(SummaryRes),
         log = drop_prefix(HtmlDir, SummaryLog),
         start_time = StartTime,
         hostname = Hostname,
         config_name = ConfigName,
         run_dir = RunDir,
         repos_rev = ReposRev,
         details = Cases};
parse_run_summary(HtmlDir, {error, SummaryLog, _ReasonStr}) ->
    {ok, Cwd} = file:get_cwd(),
    #run{test = ?DEFAULT_SUITE,
         id = ?DEFAULT_RUN,
         result = fail,
         log = drop_prefix(HtmlDir, SummaryLog),
         start_time = ?DEFAULT_TIME,
         hostname = ?DEFAULT_HOSTNAME,
         config_name = ?DEFAULT_CONFIG_NAME,
         run_dir = Cwd,
         repos_rev = ?DEFAULT_REV,
         details = []}.

parse_run_case(HtmlDir, RunDir, Start, Host, ConfigName, Suite, RunId, ReposRev,
               {test_case, Name, Log, _Doc, _HtmlLog, CaseRes}) ->
    File = drop_prefix(RunDir, Name),
    File2 = drop_some_dirs(File),
    #run{test = <<Suite/binary, ":", File2/binary>>,
         id = RunId,
         result = run_result(CaseRes),
         log = drop_prefix(HtmlDir, Log),
         start_time = Start,
         hostname = Host,
         config_name = ConfigName,
         run_dir = RunDir,
         repos_rev = ReposRev,
         details = []};
parse_run_case(_HtmlDir, RunDir, Start, Host, ConfigName, Suite, RunId, ReposRev,
               {result_case, Name, Res, _Reason}) ->
    File = drop_prefix(RunDir, Name),
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

drop_some_dirs(File) when is_binary(File) -> % BUGBUG: Temporary solution
    Q = <<"lux">>,
    Comp = filename:split(File),
    case lists:dropwhile(fun(E) -> E =/= Q end, Comp) of
        [Q | Rest] -> filename:join(Rest);
        _Rest      -> File
    end.

run_result({result, Res, _}) ->
    run_result(Res);
run_result({result, Res}) ->
    run_result(Res);
run_result(Res) ->
    case Res of
        success                                                -> success;
        {fail, _Script, _LineNo, _Expected, _Actual, _Details} -> fail;
        {error, _Reason}                                       -> fail;
        <<"SUCCESS">>                                          -> success;
        <<"SKIP", _/binary>>                                   -> skip;
        <<"FAIL", _/binary>>                                   -> fail;
        <<"ERROR", _/binary>>                                  -> fail
    end.

find_config(Key, Tuples, Default) ->
    case lists:keyfind(Key, 1, Tuples) of
        false         -> Default;
        {_, Hostname} -> Hostname
    end.

%% Keysort list of tuples and group items with same tag
keysplit(Pos, List) ->
    do_keysplit(Pos, lists:keysort(Pos, List), [], []).

do_keysplit(Pos, [H, N | T], Siblings, Acc)
  when element(Pos, H) =:= element(Pos, N) ->
    %% Collect items with same tag
    do_keysplit(Pos, [N | T], [H | Siblings], Acc);
do_keysplit(Pos, [H | T], Siblings, Acc) ->
    do_keysplit(Pos, T, [], [{element(Pos, H), [H | Siblings]} | Acc]);
do_keysplit(_Pos, [], [], Acc) ->
    lists:reverse(Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

expand_lines([Line | Lines]) ->
    [Line, "\n", expand_lines(Lines)];
expand_lines([]) ->
    [].

html_div(Class, Html) ->
    [
     "\n<div class=",
     Class,
     "><pre>",
     html_cleanup(Html),
     "</pre></div>\n"
    ].

html_cleanup(List) ->
    Bin = list_to_binary([List]),
    Opts = [global],
    Bin2 = binary:replace(Bin, <<"&">>, <<"&amp;">>, Opts),
    Bin3 = binary:replace(Bin2, <<"<">>, <<"&lt;">>, Opts),
    Bin4 = binary:replace(Bin3, <<">">>, <<"&gt;">>, Opts),
    binary:replace(Bin4, <<"\"">>, <<"&quot;">>, Opts).

html_files(A, [{file, Path, OrigPath} | Files]) ->
    [
     "\n<br>",
     html_href("", drop_prefix(A, OrigPath), drop_prefix(A, Path)),
     html_files(A, Files)
    ];
html_files(_A, []) ->
    [].

unquote(Bin) ->
    Quote = <<"\"">>,
    Size = byte_size(Bin)-2,
    case Bin of
        <<Quote:1/binary, Plain:Size/binary, Quote:1/binary>> ->
            {quote, Plain};
        Plain ->
            {plain, Plain}
    end.

drop_prefix(#astate{log_dir = LogDir}, File) ->
    drop_prefix(LogDir, File);
drop_prefix(LogDir, File) when is_binary(File) ->
    list_to_binary(drop_prefix(LogDir, binary_to_list(File)));
drop_prefix(LogDir, File) when is_binary(LogDir) ->
    drop_prefix(binary_to_list(LogDir), File);
drop_prefix(LogDir, File) when is_list(LogDir), is_list(File) ->
    lux_utils:drop_prefix(LogDir, File).

orig_script(A, Script) ->
    orig_script(A, A#astate.log_file, Script).

orig_script(A, LogFile, Script) ->
    Dir = filename:dirname(drop_prefix(A, LogFile)),
    Base = filename:basename(binary_to_list(Script)),
    filename:join([A#astate.log_dir, Dir, Base ++ ".orig"]).

html_href(Protocol, Name, Label) ->
    [
     "<a href=\"", Protocol, Name, "\">", Label, "</a>"
    ].

html_href("a", "", Protocol, Name, Label) ->
    ["\n",html_href(Protocol, Name, Label)];
html_href(Tag, Prefix, Protocol, Name, Label) when Tag =/= "" ->
    [
     "\n<", Tag, ">",
     Prefix, html_href(Protocol, Name, Label),
     "</", Tag, ">\n"
    ].

html_anchor(Name, Label) ->
    [
     "<a name=\"", Name, "\">", Label, "</a>"
    ].

html_anchor(Tag, Prefix, Name, Label) ->
    [
     "\n<", Tag, ">", Prefix, html_anchor(Name, Label), "</", Tag, ">\n"
    ].

html_header(Title) ->
    [
     <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n">>,

     <<"<html>\n">>,
     <<"<head>\n">>,
     html_style(),
     <<"<title>">>, Title, <<"</title>\n">>,
     <<"</head>\n\n">>,
     <<"<body>">>
    ].

html_footer() ->
    <<"</body>\n">>.

html_style() ->
<<"
<style>
  body {
        color: #000000;
        background-color: white
  }

  div {
  	<--- width: 300px; !--->
        overflow: auto;
  	padding: 2px;
  	border: 1px solid #b00;
  	margin-left: 2%;
  	margin-bottom: 2px;
  	margin-top: 2px;
        color: #000000;
        background-color: #FFFFE0
  }

  div.annotate {
        font-weight: normal;
  }

  div.result {
  }

  div.config {
  }

  div.code {
        font-weight: bold;
        overflow: visible;
  	padding: 0px;
  	border: 0px;
  	margin-left: 0%;
  	margin-bottom: 0px;
  	margin-top: 0px;
        color: #000000;
        background-color: white
  }

  div.send {
        background-color: #FFEC8B;
  }

  div.recv {
        background-color: #E9967A;
  }

  div.skip {
        background-color: #FFFFE0
  }

  div.match {
        background-color: #FFFFE0
  }

  div.expect {
        background-color: #FFFFE0
  }

  div.case {
        background-color: #D3D3D3
  }

  td.fail {
        background-color: #FF0000
  }

  td.secondary_fail {
        background-color: #FF4500
  }

  td.none {
        background-color: #FFCC66
  }

  td.success {
        background-color: #80FF80
  }

  td.no_data {
        background-color: #FFFFFF
  }
  </style>

">>.

%%  td.secondary_fail {
%%        background-color: #FF6600
%%  }
%% td.secondary_fail {
%%        background-color: #FF4500
%% }
%% td.secondary_fail {
%%        background-color: #DC143C
%% }
%%  td.secondary_fail {
%%        background-color: #FF3300 	
%%  }
%%  td.secondary_fail {
%%        background-color: #CC3300 	
%%  }
