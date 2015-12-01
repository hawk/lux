%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2015 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_html).

-export([annotate_log/3, annotate_log/4, history/3]).
-export([keysplit/2, keysplit/3]).

 -include("lux.hrl").

-record(astate, {run_dir,
                 run_log_dir,
                 new_log_dir,
                 suite_log_dir,
                 log_file,
                 script_file,
                 case_prefix,
                 opts}).

annotate_log(IsRecursive, LogFile, Opts) ->
    SuiteLogDir = filename:dirname(LogFile),
    annotate_log(IsRecursive, LogFile, SuiteLogDir, Opts).

annotate_log(IsRecursive, LogFile, SuiteLogDir, Opts) ->
    AbsLogFile = lux_utils:normalize(LogFile),
    IsEventLog = lists:suffix("event.log", AbsLogFile),
    LogDir = filename:dirname(AbsLogFile),
    CasePrefix = lux_utils:pick_opt(case_prefix, Opts, ""),
    A = #astate{new_log_dir = LogDir,
                log_file = AbsLogFile,
                suite_log_dir = SuiteLogDir,
                case_prefix = CasePrefix,
                opts = Opts},
    Res =
        case IsEventLog of
            true  -> annotate_event_log(A);
            false -> annotate_summary_log(IsRecursive, A)
        end,
    case Res of
        {ok, IoList} ->
            safe_write_file(AbsLogFile ++ ".html", IoList);
        {error, _File, _ReasonStr} = Error ->
            Error
    end.

safe_write_file(File, IoList) when is_binary(File) ->
    safe_write_file(binary_to_list(File), IoList);
safe_write_file(File, IoList) when is_list(File) ->
    Res = filelib:ensure_dir(File),
    if
        Res =:= ok; Res =:= {error, eexist} ->
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
        true ->
            {error, FileReason} = Res,
            {error, filename:dirname(File), file:format_error(FileReason)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Annotate a summary log and all its event logs

annotate_summary_log(IsRecursive, #astate{log_file=AbsSummaryLog} = A0) ->
    case lux_log:parse_summary_log(AbsSummaryLog) of
        {ok, Result, Groups, ConfigSection, _FileInfo, EventLogs} ->
            ConfigBins = binary:split(ConfigSection, <<"\n">>, [global]),
            ConfigProps = lux_log:split_config(ConfigBins),
            RunDir = pick_run_dir(ConfigProps),
            RunLogDir = pick_log_dir(ConfigProps),
            A = A0#astate{run_dir = RunDir,
                          run_log_dir = RunLogDir},
            Html = html_groups(A, AbsSummaryLog, Result, Groups, ConfigSection),
            case IsRecursive of
                true ->
                    O = A#astate.opts,
                    SuiteLogDir = filename:dirname(AbsSummaryLog),
                    AnnotateEventLog =
                        fun(EventLog0) ->
                                RelEventLog = drop_run_log_prefix(A, EventLog0),
                                EventLog = filename:join([SuiteLogDir,
                                                         RelEventLog]),
                                case annotate_log(IsRecursive,
                                                  EventLog,
                                                  A#astate.suite_log_dir,
                                                  O) of
                                    ok ->
                                        ok;
                                    {error, _, Reason} ->
                                        io:format("ERROR in ~s\n\~p\n",
                                                  [EventLog, Reason])
                                end
                        end,
                    lists:foreach(AnnotateEventLog, EventLogs);
                false ->
                    ignore
            end,
            {ok, Html};
        {error, _File, _Reason} = Error ->
            Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Return summary log as HTML

html_groups(A, SummaryLog, Result, Groups, ConfigSection) ->
    Dir = filename:basename(filename:dirname(SummaryLog)),
    RelSummaryLog = drop_new_log_prefix(A, SummaryLog),
    RelResultLog = "lux_result.log",
    RelConfigLog = "lux_config.log",
    RelTapLog = "lux.tap",
    IsTmp = lux_log:is_temporary(SummaryLog),
    LogFun =
        fun(L, S) ->
                ["    <td><strong>", html_href("", L, S), "</strong></td>\n"]
        end,
    [
     html_header(["Lux summary log (", Dir, ")"]),
     "<table border=1>\n",
     "  <tr>\n",
     "    <td><strong>Log files:</strong></td>\n",
     LogFun(RelSummaryLog, "Summary"),
     LogFun(RelResultLog, "Result"),
     LogFun(RelConfigLog, "Config"),
     LogFun(RelTapLog, "TAP"),
     "  </tr>\n",
     "</table>\n\n",
     html_href("h3", "", "", "#suite_config", "Suite configuration"),
     html_summary_result(A, Result, Groups, IsTmp),
     html_groups2(A, Groups),
     html_anchor("h2", "", "suite_config", "Suite configuration:"),
     html_div(<<"annotate">>, ConfigSection),
     html_footer()
    ].

html_summary_result(A, {result, Summary, Sections}, Groups, IsTmp) ->
    %% io:format("Sections: ~p\n", [Sections]),
    ResultString = choose_tmp(IsTmp, "Temporary ","Final "),
    [
     "\n<h2>", ResultString, "result: ", Summary, "</h2>\n",
     "<div class=case><pre>",
     [html_summary_section(A, S, Groups) || S <- Sections],
     "</pre></div>"
    ].

choose_tmp(IsTmp, TmpString, String) ->
    case IsTmp of
        true  -> TmpString;
        false -> String
    end.

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
    RelScript = prefix_script(A, File),
    RelFile = drop_run_dir_prefix(A, File),
    Label = [RelScript, ":", LineNo],
    case Files of
        [] ->
            [html_href("", "#" ++ RelFile, Label), "\n"];
        [HtmlLog|_] ->
            [html_href("", drop_run_log_prefix(A, HtmlLog), Label), "\n"]
    end.

html_groups2(A, [{test_group, _Group, Cases} | Groups]) ->
    [
     html_cases(A, Cases),
     html_groups2(A, Groups)
    ];
html_groups2(_A, []) ->
    [].

html_cases(A, [{test_case, AbsScript, _EventLog, Doc, HtmlLog, Res} | Cases]) ->
    Tag = "a",
    RelScript = prefix_script(A, AbsScript),
    RelFile = drop_run_dir_prefix(A, AbsScript),
    RelHtmlLog = drop_run_log_prefix(A, HtmlLog),
    [
     html_anchor(RelFile, ""),
     "\n",
     html_href("h2", "Test case: ", "", RelHtmlLog, RelScript),
     "\n<div class=case><pre>",
     html_doc(Tag, Doc),
     %% html_href(Tag, "Raw event log: ", "", RelEventLog, RelEventLog),
     %% html_href(Tag, "Annotated script: ", "", RelHtmlLog, RelHtmlLog),
     html_result(Tag, Res, RelHtmlLog),
     "\n",
     "</pre></div>",
     html_cases(A, Cases)
    ];
html_cases(A, [{result_case, AbsScript, Reason, Details} | Cases]) ->
    Tag = "a",
    RelScript = prefix_script(A, AbsScript),
    RelFile = drop_run_dir_prefix(A, AbsScript),
    [
     html_anchor(RelFile, ""),
     html_href("h3", "Test case: ", "", RelFile, RelScript),
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
             html_div(<<"annotate">>, lux_utils:expand_lines(Desc))
     end
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Annotate a lux with events from the log

annotate_event_log(#astate{log_file=EventLog} = A) ->
    try
        case lux_log:scan_events(EventLog) of
            {ok, EventLog2, ConfigLog,
             Script, EventBins, ConfigBins, LogBins, ResultBins} ->
                ConfigProps = lux_log:split_config(ConfigBins),
                RunDir = pick_run_dir(ConfigProps),
                RunLogDir = pick_log_dir(ConfigProps),
                A2 = A#astate{run_dir = RunDir,
                              run_log_dir = RunLogDir},
                OrigScript = orig_script(A2, Script),
                A3 = A2#astate{script_file = OrigScript},
                Events = lux_log:parse_events(EventBins, []),
                %% io:format("Events: ~p\n", [Events]),
                Logs = lux_log:parse_io_logs(LogBins, []),
                Result = lux_log:parse_result(ResultBins),
                {Annotated, Files} =
                    interleave_code(A3, Events, Script, 1, 999999, [], []),
                Html = html_events(A3, EventLog2, ConfigLog, Script, Result,
                                   Files, Logs, Annotated, ConfigBins),
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

interleave_code(A, Events, Script, FirstLineNo, MaxLineNo, CmdStack, Files) ->
    ScriptComps = lux_utils:filename_split(Script),
    OrigScript = A#astate.script_file,
    CodeLines2 =
        case file:read_file(OrigScript) of
            {ok, ScriptBin} ->
                CodeLines = binary:split(ScriptBin, <<"\n">>, [global]),
                try
                    lists:nthtail(FirstLineNo-1, CodeLines)
                catch
                    _Class:_Reason ->
                        CodeLines
                end;
            {error, FileReason} ->
                ReasonStr = binary_to_list(OrigScript) ++ ": " ++
                    file:format_error(FileReason),
                io:format("ERROR(lux): ~s\n", [ReasonStr]),
                []
        end,
    Files2 = lists:keystore(Script, 2, Files,
                            {file, Script, OrigScript}),
    Acc = [],
    do_interleave_code(A, Events, ScriptComps, CodeLines2,
                       FirstLineNo, MaxLineNo, Acc, CmdStack, Files2).

do_interleave_code(A, [{event, SingleLineNo, Shell, Op, Data},
                       {event, SingleLineNo, Shell, Op, Data2} | Events],
                   ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, CmdStack, Files) when Op =:= <<"recv">>,
                                              Data2 =/= [<<"timeout">>]->
    %% Combine two chunks of recv data into one in order to improve readability
    [Last | Rev] = lists:reverse(Data),
    [First | Rest] = Data2,
    Data3 = lists:reverse(Rev, [<<Last/binary, First/binary>> | Rest]),
    do_interleave_code(A, [{event, SingleLineNo, Shell, Op, Data3} | Events],
                       ScriptComps, CodeLines, CodeLineNo,
                       MaxLineNo, Acc, CmdStack, Files);
do_interleave_code(A, [{event, SingleLineNo, Shell, _Op, Data} | Events],
                   ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, CmdStack, Files) ->
    {CodeLines2, CodeLineNo2, Code} =
        pick_code(ScriptComps, CodeLines, CodeLineNo, SingleLineNo,
                  [], CmdStack),
    CmdStack2 = [{ScriptComps, SingleLineNo, undefined} | CmdStack],
    Acc2 = [{event_html, CmdStack2, _Op, Shell, Data}] ++ Code ++ Acc,
    do_interleave_code(A, Events, ScriptComps, CodeLines2, CodeLineNo2,
                       MaxLineNo, Acc2, CmdStack, Files);
do_interleave_code(A, [{body, InvokeLineNo, FirstLineNo, LastLineNo,
                        SubScript, SubEvents} | Events],
                   ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, CmdStack, Files) ->
    CmdStack2 = [{ScriptComps, InvokeLineNo, undefined} | CmdStack],
    OrigSubScript = orig_script(A, SubScript),
    SubA = A#astate{script_file = OrigSubScript},
    {SubAnnotated, Files2} =
        interleave_code(SubA, SubEvents, SubScript, FirstLineNo, LastLineNo,
                        CmdStack2, Files),
    Event = {body_html, CmdStack2, FirstLineNo, SubScript,
             OrigSubScript, SubAnnotated},
    do_interleave_code(A, Events, ScriptComps, CodeLines, CodeLineNo,
                       MaxLineNo, [Event | Acc], CmdStack, Files2);
do_interleave_code(_A, [], ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, CmdStack, Files) ->
    X = pick_code(ScriptComps, CodeLines, CodeLineNo, MaxLineNo, [], CmdStack),
    {_Skipped, _CodeLineNo, Code} = X,
    {lists:reverse(Code ++ Acc), Files}.

pick_code(ScriptComps, [Line | Lines], CodeLineNo, LineNo, Acc, CmdStack)
  when LineNo >= CodeLineNo ->
    CmdStack2 = [{ScriptComps, CodeLineNo, undefined} | CmdStack],
    pick_code(ScriptComps, Lines, CodeLineNo+1, LineNo,
              [{code_html, CmdStack2, Line} | Acc], CmdStack);
pick_code(_ScriptComps, Lines, CodeLineNo, _LineNo, Acc, _CmdStack) ->
    {Lines, CodeLineNo, Acc}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Return event log as HTML

html_events(A, EventLog, ConfigLog, Script, Result, Files,
            Logs, Annotated, ConfigBins) ->
    EventLogDir = A#astate.new_log_dir,
    EventLogBase = filename:join([EventLogDir, filename:basename(Script)]),
    ExtraLogs = <<EventLogBase/binary, ".extra.logs">>,
    Dir = filename:basename(EventLogDir),
    LogFun =
        fun(L, S) ->
                ["    <td><strong>",
                 html_href("", drop_prefix(EventLogDir, L), S),
                 "</strong></td>\n"]
        end,
    PrefixScript = A#astate.case_prefix ++ drop_run_dir_prefix(A, Script),
    [
     html_header(["Lux event log (", Dir, ")"]),
     "\n", html_href("h2", "", "", "#annotate", PrefixScript),
     html_result("h2", Result, ""),
     html_href("h3", "", "", "#config", "Script configuration"),
     html_href("h3", "", "", "#cleanup", "Cleanup"),
     "\n<h3>Source files: ",
     html_scripts(A, Files, main),
     "\n</h3>",
     "\n<h3>Log files:</h3>\n ",
     "<table border=1>\n",
     "  <tr>\n",
     LogFun(EventLog, "Event"),
     LogFun(ConfigLog, "Config"),
     case filelib:is_dir(ExtraLogs) of
         true  -> LogFun(ExtraLogs, "Extra");
         false -> "    <td><strong>No extra</strong></td>\n"
     end,
     html_logs(A, Logs),
     "  </tr>\n",
     "</table>\n\n",

     "\n",html_anchor("h2", "", "annotate", "Annotated source code"),"\n",
     html_code(A, Annotated),

     "<div class=code><pre><a name=\"cleanup\"></a></pre></div>\n",

     html_anchor("h2", "", "config", "Script configuration:"),
     html_config(ConfigBins),
     html_footer()
    ].

html_result(Tag, {result, Result}, HtmlLog) ->
    case Result of
        success ->
            ["\n<", Tag, ">Result: <strong>SUCCESS</strong></", Tag, ">\n"];
        skip ->
            ["\n<", Tag, ">Result: <strong>SKIP</strong></", Tag, ">\n"];
        {skip, _} ->
            ["\n<", Tag, ">Result: <strong>SKIP</strong></", Tag, ">\n"];
        {error_line, RawLineNo, Reason} ->
            Anchor = RawLineNo,
            [
             "\n<", Tag, ">Result: <strong>ERROR at line ",
             html_href("", [HtmlLog, "#", Anchor], Anchor),
             "<h3>Reason</h3>",
             html_div(<<"annotate">>, lux_utils:expand_lines(Reason))
            ];
        {error, Reason} ->
            [
             "\n<", Tag, ">Result: <strong>ERROR</strong></", Tag, ">\n",
             "<h3>Reason</h3>",
             html_div(<<"annotate">>, lux_utils:expand_lines(Reason))
            ];
        {fail, _Script, RawLineNo, Expected0, Actual, Details} ->
            Anchor = RawLineNo,
            Expected = lux_utils:expand_lines(Expected0),
            Expected2 = lux_utils:normalize_newlines(Expected),
            Expected3 = binary:split(Expected2, <<"\\R">>, [global]),
            Diff = lux_utils:diff(Expected3, Details),
            HtmlDiff = html_diff(Diff),
            [
             "\n<", Tag, ">Result: <strong>",
             html_href("", [HtmlLog, "#failed"], "FAILED"),
             " at line ",
             html_href("", [HtmlLog, "#", Anchor], Anchor),
             "</strong></", Tag, ">\n",
             "<h3>Expected</h3>",
              html_div(<<"annotate">>, lux_utils:expand_lines(Expected3)),
             "<h3>Actual: ", html_cleanup(Actual), "</h3>",
             [
              "\n<div class=annotate><pre>",
              lux_utils:expand_lines(HtmlDiff),
              "</pre></div>"
             ]
            ]
    end.

html_diff(Diff) ->
    html_diff(Diff, [], first, false).

html_diff([H|T], Acc, Where, Rep) ->
    Plain = "",
    Bold = "b",
    case H of
        {common, Com} ->
            html_diff(T, [{<<"  ">>, "black",Bold,clean,Com}|Acc], middle, Rep);
        {insert, Ins} when Where =/= first, element(1,hd(T)) =:= common ->
            html_diff(T, [{<<"+ ">>,"blue",Bold,clean,Ins}|Acc], middle, Rep);
        {insert, Ins} ->
            html_diff(T, [{<<"  ">>,"black",Plain,clean,Ins}|Acc], middle, Rep);
        {delete, Del} ->
            html_diff(T, [{<<"- ">>,"red",Bold,clean,Del}|Acc], middle, Rep);
        {replace, Ins, Del} when Where =:= first, T =:= [] ->
            %% Display single replace as insert
            {Clean, _Del2, Ins2} = html_part(Del, Ins),
%%          html_diff(T, [{<<"- ">>,"red",Bold,Clean,Del2},
%%                        {<<"+ ">>,"blue",Bold,Clean,Ins2}|Acc], middle, true)
            html_diff(T, [{<<"  ">>,"black",Bold,Clean,Ins2}|Acc],middle, true);
        {replace, Ins, Del} ->
            {Clean, Del2, Ins2} = html_part(Del, Ins),
            html_diff(T, [{<<"- ">>,"red",Bold,Clean,Del2},
                          {<<"+ ">>,"blue",Bold,Clean,Ins2}|Acc], middle, true)
    end;
html_diff([], Acc, _Where, Rep) ->
    html_color(lists:reverse(Acc), Rep).

html_part([Del], [Ins]) ->
    Diff = lux_utils:diff(binary_to_list(Del), binary_to_list(Ins)),
    html_part_diff(Diff, [], []);
html_part(Del, Ins) ->
    {clean, Del, Ins}.

html_part_diff([H|T], DelAcc, InsAcc) ->
    Underline = "u",
    case H of
        {common, Com} ->
            CleanCom = html_cleanup(Com),
            html_part_diff(T,
                           [CleanCom|DelAcc],
                           [CleanCom|InsAcc]);
        {insert, Ins} ->
            html_part_diff(T,
                           DelAcc,
                           [tag(Underline,html_cleanup(Ins))|InsAcc]);
        {delete, Del} ->
            html_part_diff(T,
                           [tag(Underline,html_cleanup(Del))|DelAcc],
                           InsAcc);
        {replace, Ins, Del} ->
            html_part_diff(T,
                           [tag(Underline,html_cleanup(Del))|DelAcc],
                           [tag(Underline,html_cleanup(Ins))|InsAcc])
    end;
html_part_diff([], DelAcc, InsAcc) ->
    {noclean,
     [list_to_binary(lists:reverse(DelAcc))],
     [list_to_binary(lists:reverse(InsAcc))]}.

html_color([{Prefix, Color, Style, Clean, [Line|Lines]} | LineSpec], Delay) ->
    [
     list_to_binary([if
                         Delay =:= true    -> "";
                         Color =:= "black" -> "";
                         true              -> html_anchor("failed", "")
                     end,
                     "<font color=\"",Color,"\">",
                     opt_tag(Style, opt_clean(Prefix, Clean, Line)),
                     "</font>"])
     | html_color([{Prefix, Color, Style, Clean, Lines} | LineSpec], Delay)
    ];
html_color([{_Prefix, _Color, _Style, _Clean, []} | LineSpec], _Delay) ->
    html_color(LineSpec, false);
html_color([], _Delay) ->
    [html_anchor("failed", "")].

opt_clean(Prefix, Clean, Line) ->
    [
     html_cleanup(Prefix),
     case Clean of
         clean   -> html_cleanup(Line);
         noclean -> Line
     end
    ].

opt_tag(Tag, Text) ->
    case Tag of
        "" -> Text;
        _  -> tag(Tag, Text)
    end.

tag(Tag, Text) ->
    ["<", Tag, ">", Text, "</", Tag, ">"].

html_config(Config) when is_list(Config) ->
    html_div(<<"annotate">>, lux_utils:expand_lines(Config));
html_config(Config) when is_binary(Config) ->
    html_div(<<"annotate">>, Config).

html_logs(A, [{log, ShellName, Stdin, Stdout} | Logs]) ->

    [
     "\n<tr>\n    ",
     "<td><strong>Shell ", ShellName, "</strong></td>",
     "<td><strong>",
     html_href("", rel_log(A, Stdin), "Stdin"),
     "</strong></td>",
     "<td><strong>",
     html_href("", rel_log(A, Stdout), "Stdout"),
     "</strong></td>",
     "\n</tr>\n",
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
            FullLineNo = lux_utils:pretty_full_lineno(LineNoStack),
            [
             html_toggle_div(code, Prev),
             case Code of
                 <<"[cleanup]">> -> "<a name=\"cleanup\"></a>";
                 _               -> ""
             end,
             html_anchor(FullLineNo, FullLineNo), ": ",
             html_cleanup(Code),
             "\n",
             html_code2(A, Annotated, code)
            ];
        {event_html, LineNoStack, Op, Shell, Data0} ->
            Data =
                case Op of
                    <<"expect">> ->
                        lists:append([binary:split(D, <<"\\\\R">>, [global]) ||
                                         D <- Data0]);
                    _ ->
                        Data0
                end,
            FullLineNo = lux_utils:pretty_full_lineno(LineNoStack),
            Html = [Shell, "(", FullLineNo, "): ", Op, " "],
            [
             html_toggle_div(event, Prev),
             html_cleanup(Html),
             html_opt_div(Op, Data),
             "</br>",
             html_code2(A, Annotated, event)
            ];
        {body_html, LineNoStack, _MacroLineNo, SubScript,
         OrigSubScript, SubAnnotated} ->
            FullLineNo = lux_utils:pretty_full_lineno(LineNoStack),
            RelSubScript = drop_run_dir_prefix(A, SubScript),
            if
                OrigSubScript =/= A#astate.script_file ->
                    [
                     html_toggle_div(code, Prev),
                     html_toggle_div(event, code),
                     html_opt_div(<<"file">>,
                                  [<<"entering file: ",
                                     RelSubScript/binary>>]),
                     "</pre></div>",
                     html_code(A, SubAnnotated),
                     html_toggle_div(code, event),
                     html_anchor(FullLineNo, FullLineNo), ": ",
                     html_toggle_div(event, code),
                     html_opt_div(<<"file">>,
                                  [<<"exiting file: ",
                                     RelSubScript/binary>>]),
                     "</pre></div>",
                     html_code(A, Annotated)
                    ];
                true ->
                    [
                     html_toggle_div(code, Prev),
                     html_code2(A, SubAnnotated, code),
                     %% html_anchor(FullLineNo, FullLineNo), ": \n",
                     html_code2(A, Annotated, code)
                    ]
            end
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

html_opt_div(Op, Data) ->
    Html = lux_utils:expand_lines(Data),
    case Op of
        <<"send">>   -> html_div(Op, Html);
        <<"recv">>   -> html_div(Op, Html);
        <<"expect">> -> html_div(Op, Html);
        <<"skip">>   -> html_div(Op, Html);
        <<"match">>  -> html_div(Op, Html);
        _            -> html_cleanup(Html)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History

-define(CURRENT_SUFFIX, "_current").
-define(CONFIG_SUFFIX, "_config").
-define(HOST_SUFFIX, "_host").

-record(table, {name, res, iolist}).
-record(row,   {res, iolist}).
-record(cell,  {res, run, iolist}).

history(TopDir, RelHtmlFile, Opts) ->
    TopDir2 = lux_utils:normalize(TopDir),
    AbsHtmlFile = lux_utils:normalize(RelHtmlFile),
    AllRuns = parse_summary_logs(AbsHtmlFile, TopDir2, [], Opts),
    io:format("~p test runs", [length(AllRuns)]),
    SplitHosts = keysplit(#run.hostname, AllRuns),
    LatestRuns = latest_runs(SplitHosts),
    HostTables = html_history_table_hosts(SplitHosts, AbsHtmlFile),
    SplitConfigs = keysplit(#run.config_name, AllRuns, fun compare_run/2),
    ConfigTables = html_history_table_configs(SplitConfigs, AbsHtmlFile),
    HtmlDir = filename:dirname(RelHtmlFile),
    OverviewIoList =
        [
         html_history_header("overview", AllRuns,
                             ConfigTables, HostTables, HtmlDir, RelHtmlFile),
         html_history_table_latest(LatestRuns, AbsHtmlFile),
         html_history_table_all(AllRuns, AbsHtmlFile),
         html_footer()
        ],
    CurrentIoList =
        [
         html_history_header("current failures", AllRuns,
                             ConfigTables, HostTables, HtmlDir, RelHtmlFile),
         html_history_table_current(AllRuns, AbsHtmlFile),
         html_footer()
        ],
    ConfigIoList =
        [
         html_history_header("config", AllRuns,
                             ConfigTables, HostTables, HtmlDir, RelHtmlFile),
         "</a name=\"#content\">",
         [T#table.iolist || T <- ConfigTables],
         html_footer()
        ],
    HostIoList =
        [
         html_history_header("host", AllRuns,
                             ConfigTables, HostTables, HtmlDir, RelHtmlFile),
         "</a name=\" #content\">",
         [T#table.iolist || T <- HostTables],
         html_footer()
        ],
    CurrentHtmlFile =
        filename:join(HtmlDir,
                      insert_html_suffix(RelHtmlFile, "", ?CURRENT_SUFFIX)),
    ConfigHtmlFile =
        filename:join(HtmlDir,
                      insert_html_suffix(RelHtmlFile, "", ?CONFIG_SUFFIX)),
    HostHtmlFile =
        filename:join(HtmlDir,
                      insert_html_suffix(RelHtmlFile, "", ?HOST_SUFFIX)),
    safe_write_file(RelHtmlFile, OverviewIoList),
    safe_write_file(CurrentHtmlFile, CurrentIoList),
    safe_write_file(ConfigHtmlFile, ConfigIoList),
    safe_write_file(HostHtmlFile, HostIoList).

latest_runs(SplitHosts) ->
    SplitHostTests =
        [{Host, keysplit(#run.test, HostRuns, fun compare_run/2)} ||
            {Host, HostRuns} <- SplitHosts],
    DeepIds =
        [(hd(TestRuns))#run.id ||
            {_Host, HostTests} <-SplitHostTests,
            {_Test, TestRuns} <- HostTests],
    Ids = lists:usort(lists:flatten(DeepIds)),
    [Run ||
        {_Host, HostTests} <-SplitHostTests,
        {_Test, TestRuns} <- HostTests,
        Run <- TestRuns,
        lists:member(Run#run.id, Ids)].

html_history_header(Section, AllRuns, ConfigTables, HostTables,
                    HtmlDir, HtmlFile) ->
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
     html_header(["Lux history ", Section, " (", Dir, ")"]),
     "<h1>Lux history ", Section, " (", Dir, ") generated at ",
     lux_utils:now_to_string(lux_utils:timestamp()),
     "</h1>",

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
     "</table>\n\n",

     html_history_legend(),

     "<h3>",
     html_href("",
               [drop_new_log_prefix(HtmlDir, HtmlFile), "#content"],
               "Overview"),
     "</h3>\n\n",

     "<h3>",
     html_suffix_href(HtmlFile,"","#content", "Still failing test cases",
                      ?CURRENT_SUFFIX),
     "</h3>\n\n",

     "<h3>",
     html_suffix_href(HtmlFile,"","#content", "Configurations", ?CONFIG_SUFFIX),
     "</h3>\n",
     "  <table border=1>\n",
     "    <tr>\n",
     [
      html_suffix_href_td(HtmlFile, ConfigName, ConfigRes, ?CONFIG_SUFFIX) ||
         #table{name=ConfigName, res=ConfigRes} <- ConfigTables
     ],
     "    </tr>\n",
     "  </table>\n",

     "<h3>",
     html_suffix_href(HtmlFile,"", "#content", "Hosts", ?HOST_SUFFIX),
     "</h3>\n",
     "  <table border=1>\n",
     "    <tr>\n",
     [
      html_suffix_href_td(HtmlFile, Host, HostRes, ?HOST_SUFFIX) ||
         #table{name=Host, res=HostRes} <- HostTables
     ],
     "    </tr>\n",
     "  </table>\n"
     "<br><hr>\n"
    ].

html_history_legend() ->
    [
     "<h3>Legend</h3>\n",
     "  <table border=1>\n",
     "    <tr>\n",
     html_history_td("First fail", fail, "left", ""),
     html_history_td("Secondary fails on same host", secondary_fail, "left",""),
     html_history_td("Skipped", none, "left", ""),
     html_history_td("Success", success, "left", ""),
     html_history_td("No data", no_data, "left", ""),
     "    </tr>\n",
     "  </table>\n"
    ].

html_history_table_latest(LatestRuns, HtmlFile) ->
    T =html_history_table("Latest", "All test suites",
                          LatestRuns, HtmlFile, none, worst),
    [
     html_anchor("content", "<h3>Latest run on each host</h3>\n"),
     T#table.iolist
    ].

html_history_table_all(AllRuns, HtmlFile) ->
    T = html_history_table("All", "All test suites",
                           AllRuns, HtmlFile, none, latest),
    [
     html_anchor("all_runs", "<h3>All runs</h3>\n"),
     T#table.iolist
    ].

html_history_table_current(AllRuns, HtmlFile) ->
    Details = [D#run{details=[D]} || R <- AllRuns, D <- R#run.details],
    T = html_history_table("All", "Still failing test cases",
                           Details, HtmlFile, latest_success, latest),
    [
     html_anchor("content", "<h3>Still failing test cases</h3>\n"),
     T#table.iolist
    ].

html_history_table_configs(SplitConfigs, HtmlFile) ->
    [
     html_history_double_table(ConfigName,
                               "Config: " ++ ConfigName,
                               Runs,
                               HtmlFile,
                               latest) ||
        {ConfigName, Runs} <- SplitConfigs
    ].

html_history_table_hosts(SplitHosts, HtmlFile) ->
    [
     html_history_double_table(Host,
                               ["Host: ", Host,
                                " (", (hd(Runs))#run.config_name, ")"],
                               Runs,
                               HtmlFile,
                               latest) ||
        {Host, Runs} <- SplitHosts
    ].

html_history_double_table(Name, Label, AllRuns, HtmlFile, Select) ->
    Details = [D#run{details=[D]} || R <- AllRuns, D <- R#run.details],
    AllT = html_history_table(Name, "All test suites",
                              AllRuns, HtmlFile, none, Select),
    FailedT = html_history_table(Name, "Failed test cases",
                                 Details, HtmlFile, any_success, Select),
    #table{name=Name,
           res=AllT#table.res,
           iolist=
               [
                "\n",
                ["<h3>", html_anchor(Name, Label), "</h3>\n"],
                AllT#table.iolist,
                "\n<br>\n",
                FailedT#table.iolist
               ]}.

html_history_table(Name, Grain, Runs, HtmlFile, Suppress, Select) ->
    SplitTests = keysplit(#run.test, Runs, fun compare_run/2),
    SplitIds = keysplit(#run.id, Runs, fun compare_run/2),
    SplitIds2 = lists:sort(fun compare_split/2, SplitIds),
    RowHistory =
        [
         html_history_row(Test, TestRuns, SplitIds2, HtmlFile,
                          Select, Suppress)
         || {Test, TestRuns} <- lists:reverse(SplitTests)
        ],
    PickRes = fun(#row{res=R}, Acc) -> lux_utils:summary(Acc, R) end,
    SelectedRes = lists:foldl(PickRes, no_data, RowHistory),
    #table{name=Name,
           res=SelectedRes,
           iolist=
               [
                "  <table border=1>\n",
                "    <tr>\n",
                html_history_table_td(Grain, SelectedRes, "left"),
                [["      <td>", Rev,
                  "<br>", "<strong>", Id, "</strong>",
                  "<br>", Time,
                  "</td>\n"] ||
                    {Id, [#run{start_time=Time, repos_rev=Rev} |_ ]}
                        <- SplitIds2
                ],
                "    </tr>\n",
                "    <tr>\n",
                [["      <td>",
                  "<strong>",
                  html_suffix_href(HtmlFile, "", "#"++Host, Host, ?HOST_SUFFIX),
                  "</strong>",
                  "<br>", html_suffix_href(HtmlFile,
                                           "",
                                           "#" ++ ConfigName,
                                           ConfigName,
                                           ?CONFIG_SUFFIX),
                  "</td>\n"] ||
                    {_, [#run{hostname=Host, config_name=ConfigName} |_ ]}
                        <- SplitIds2
                ],
                "    </tr>\n",
                [R#row.iolist || R <- RowHistory],
                "  </table>\n"
               ]
          }.

html_history_row(Test, Runs, SplitIds, HtmlFile, Select, Suppress) ->
    RevRuns = lists:reverse(lists:keysort(#run.id, Runs)),
    EmitCell =
        fun({Id, _R}, AccRes) ->
                html_history_cell(Test, Id, RevRuns, HtmlFile, AccRes)
        end,
    {Cells, _} = lists:mapfoldr(EmitCell, [], SplitIds),
    ValidResFilter = fun (Cell) -> valid_res_filter(Cell, Suppress) end,
    ValidRes = lists:zf(ValidResFilter, Cells),
    case lists:usort(ValidRes) of
        [] when Suppress =:= any_success ->
            #row{res=no_data, iolist=[]}; % Skip row
        [success] when Suppress =:= any_success ->
            #row{res=no_data, iolist=[]}; % Skip row
        [none] when Suppress =:= any_success ->
            #row{res=no_data, iolist=[]}; % Skip row
        [no_data, none] when Suppress =:= any_success ->
            #row{res=no_data, iolist=[]}; % Skip row
        _ ->
            SelectedRes = select_row_res(Cells, Select, no_data),
            case Suppress of
                latest_success
                  when SelectedRes =:= success;
                       SelectedRes =:= none;
                       SelectedRes =:= no_data ->
                    #row{res=no_data, iolist=[]}; % Skip row
                _ ->
                    #row{res=SelectedRes,
                         iolist=
                             [
                              "    <tr>\n",
                              html_history_td(Test, SelectedRes, "left", ""),
                              [C#cell.iolist || C <- Cells],
                              "    </tr>\n"
                             ]
                        }
            end
    end.

valid_res_filter(#cell{res=Res}, Suppress) ->
    case Res of
        no_data                               -> false;
        success when Suppress =:= any_success -> false;
        _                                     -> {true, Res}
    end.

select_row_res(Cells, worst, Acc) ->
    PickRes = fun(#cell{res=Res}, A) -> lux_utils:summary(A, Res) end,
    lists:foldl(PickRes, Acc, Cells);
select_row_res(Cells, latest, Acc) ->
    select_latest_row_res(Cells, Acc).

select_latest_row_res([#cell{res=Res} | Cells], Acc)
  when Res =:= no_data; Res =:= none ->
    %% Skip useless results
    NewAcc = lux_utils:summary(Acc, Res),
    select_latest_row_res(Cells, NewAcc);
select_latest_row_res([#cell{run=#run{repos_rev=Rev}}=C | Cells], Acc) ->
    PickSameRev = fun(#cell{run=#run{repos_rev=R}}) when R =:= Rev ->
                          true;
                     (_) ->
                          false
                  end,
    SameRevCells = lists:takewhile(PickSameRev, Cells),
    select_row_res([C|SameRevCells], worst, Acc);
select_latest_row_res([#cell{res=Res, run=undefined} | _Cells], _Acc) ->
    Res;
select_latest_row_res([], Acc) ->
    Acc.

%% Returns true if first run is newer than (or equal) to second run
%% Compare fields in this order: repos_rev, start_time, hostname and id
compare_run(#run{repos_rev=A}, #run{repos_rev=B}) when A < B ->
    false;
compare_run(#run{repos_rev=A}, #run{repos_rev=B}) when A > B ->
    true;
compare_run(#run{start_time=A}, #run{start_time=B}) when A < B ->
    false;
compare_run(#run{start_time=A}, #run{start_time=B}) when A > B ->
    true;
compare_run(#run{hostname=A}, #run{hostname=B}) when A < B ->
    false;
compare_run(#run{hostname=A}, #run{hostname=B}) when A > B ->
    true;
compare_run(#run{id=A}, #run{id=B}) ->
    A > B.

compare_split({_, []}, {_, [#run{}|_]}) ->
    true;
compare_split({_, [#run{}|_]}, {_, []}) ->
    false;
compare_split({_, [#run{}=R1|_]}, {_, [#run{}=R2|_]}) ->
    %% Test on first run
    compare_run(R1, R2).

html_history_cell(Test, Id, Runs, HtmlFile, AccRes) ->
    case lists:keyfind(Id, #run.id, Runs) of
        false ->
            Td = html_history_td("-", no_data, "right", Test),
            {#cell{res=no_data, run=undefined, iolist=Td}, AccRes};
        Run ->
            RunN  = length([run  || R <- Run#run.details,
                                    R#run.result =/= skip]),
            FailN = length([fail || R <- Run#run.details,
                                    R#run.result =:= fail]),
            FailCount = lists:concat([FailN, " (", RunN, ")"]),
            Text =
                case Run#run.log of
                    ?DEFAULT_LOG ->
                        FailCount;
                    Log ->
                        HtmlDir = filename:dirname(HtmlFile),
                        html_href("",
                                  [drop_new_log_prefix(HtmlDir, Log), ".html"],
                                  FailCount)
                end,
            OrigRes =
                case RunN of
                    0 -> none;
                    _ -> Run#run.result
                end,
            Host = Run#run.hostname,
            Res =
                case lists:keyfind(Host, 1, AccRes) of
                    {_, fail} when OrigRes =:= fail ->
                        secondary_fail;
                    _ ->
                        OrigRes
                end,
            AccRes2 = [{Host, OrigRes} | AccRes],
            ToolTip = [Test, "\n",
                       Run#run.config_name,"\n",
                       Run#run.hostname,"\n",
                       Run#run.start_time,"\n",
                       Run#run.id,"\n",
                       Run#run.repos_rev],
            Td = html_history_td(Text, Res, "right", ToolTip),
            {#cell{res=Res, run=Run, iolist=Td}, AccRes2}
    end.

html_suffix_href_td(HtmlFile, Text, skip, Suffix) ->
    html_suffix_href_td(HtmlFile, Text, none, Suffix);
html_suffix_href_td(HtmlFile, Text, Res, Suffix) ->
    [
     "    ",
     "<td class=", atom_to_list(Res), "> ",
     html_suffix_href(HtmlFile,"", "#" ++ Text, Text, Suffix),
     "</td>\n"
    ].

html_history_table_td(Text, skip, Align) ->
    html_history_table_td(Text, none, Align);
html_history_table_td(Text, Res, Align) ->
    [
     "      ",
     "<td class=", atom_to_list(Res), " align=\"", Align, "\" rowspan=\"2\">",
     "<strong>", Text, "</strong>",
     "</td>\n"
    ].

html_history_td(Text, skip, Align, Title) ->
    html_history_td(Text, none, Align, Title);
html_history_td(Text, Res, Align, Title) ->
    [
     "    ",
     "<td class=", atom_to_list(Res),
     " align=\"", Align, "\"",
     case Title of
         "" -> [];
         _  -> [" title=\"", Title, "\""]
     end,
     ">",
     Text,
     "</td>\n"
    ].

multi_member([H | T], Files) ->
    case lists:member(H, Files) of
        true ->
            {true, H};
        false ->
            multi_member(T, Files)
    end;
multi_member([], _Files) ->
    false.

parse_summary_logs(HtmlFile, Dir, Acc, Opts) ->
    Skip =
        ["lux.skip",
         "lux_summary.log",
         "lux_summary.log.tmp",
         "qmscript.skip",
         "qmscript_summary.log",
         "qmscript_summary.log.tmp",
         "qmscript.summary.log"],
    do_parse_summary_logs(HtmlFile, Dir, Acc, Skip, Opts).

do_parse_summary_logs(HtmlFile, Dir, Acc, Skip, Opts) ->
    %% io:format("~s\n", [Dir]),
    case file:list_dir(Dir) of
        {ok, Files} ->
            case multi_member(Skip, Files) of
                {true, Base} ->
                    case lists:suffix(".log", Base) of
                        true ->
                            %% A summary log
                            File = filename:join([Dir, Base]),
                            io:format(".", []),
                            case lux_log:parse_summary_log(File) of
                                {ok,_,_,_,_,_} = Res->
                                    R = lux_log:parse_run_summary(HtmlFile,
                                                                  File,
                                                                  Res,
                                                                  Opts),
                                    [R | Acc];
                                {error, _, _Reason} ->
                                    Acc
                            end;
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
                                do_parse_summary_logs(HtmlFile, SubDir,
                                                      A, Skip, Opts)
                        end,
                    lists:foldl(Fun, Acc, Files)
            end;
        {error, _Reason} ->
            %% Not a dir or problem to read dir
            Acc
    end.

%% Keysort list of tuples and group items with same tag
%%
%% Items are returned in reverse order:
%%
%%   lux_html:keysplit(1, [{3,3},{3,1},{3,2},{1,1},{1,2},{2,2},{2,1},{1,3}]).
%%   [{1,[{1,3},{1,2},{1,1}]},
%%    {2,[{2,1},{2,2}]},
%%    {3,[{3,2},{3,1},{3,3}]}]

keysplit(Pos, List) ->
    keysplit(Pos, List, undefined).

keysplit(Pos, List, Fun) ->
    do_keysplit(Pos, lists:keysort(Pos, List), Fun, [], []).

do_keysplit(Pos, [H, N | T], Fun, Siblings, Acc)
  when element(Pos, H) =:= element(Pos, N) ->
    %% Collect items with same tag
    do_keysplit(Pos, [N | T], Fun, [H | Siblings], Acc);
do_keysplit(Pos, [H | T], Fun, Siblings, Acc) ->
    Siblings2 = [H | Siblings],
    Siblings3 =
        if
            Fun =:= undefined ->
                Siblings2;
            is_function(Fun, 2) ->
                lists:sort(Fun, Siblings2)
        end,
    do_keysplit(Pos, T, Fun, [], [{element(Pos, H), Siblings3} | Acc]);
do_keysplit(_Pos, [], _Fun, [], Acc) ->
    lists:reverse(Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

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

html_scripts(A, [{file, Path, OrigScript} | Files], Level) ->
    RelScript = rel_orig_script(A, OrigScript, Level),
    RelPath0 = drop_run_dir_prefix(A, Path),
    RelPath = add_up_dir(A, RelPath0, Level),
    [
     "\n<br>",
     html_href("", RelScript, RelPath),
     html_scripts(A, Files, include)
    ];
html_scripts(_A, [], _Level) ->
    [].

prefix_script(A, AbsScript) ->
    RelScript = drop_rel_dir_prefix(A, rel_script(A, AbsScript)),
    A#astate.case_prefix ++ RelScript.

drop_rel_dir_prefix(A, RelPath) ->
    RelDir = drop_prefix(A#astate.suite_log_dir, A#astate.new_log_dir),
    drop_prefix(RelDir, RelPath).

add_up_dir(_A, RelPath, main) ->
    RelPath;
add_up_dir(_A, AbsPath, include) when hd(AbsPath) =:= $/ ->
    AbsPath;
add_up_dir(_A, <<"/", _/binary>> = AbsPath, include) ->
    AbsPath;
add_up_dir(A, RelPath, include) ->
    case drop_prefix(A#astate.suite_log_dir, A#astate.new_log_dir) of
        "." ->
            RelPath;
        RelDir ->
            UpDir = filename:join([".." || _ <- filename:split(RelDir)]),
            filename:join([UpDir, RelPath])
    end.

drop_run_dir_prefix(#astate{run_dir=LogDir}, File) ->
    drop_prefix(LogDir, File).

drop_run_log_prefix(#astate{run_log_dir=LogDir}, File) ->
    drop_prefix(LogDir, File).

drop_new_log_prefix(#astate{new_log_dir=LogDir}, File) ->
    drop_prefix(LogDir, File).

drop_prefix(Dir, File) ->
    lux_utils:drop_prefix(Dir, File).

pick_run_dir(ConfigProps) ->
    pick_prop(<<"run_dir">>, ConfigProps).

pick_log_dir(ConfigProps) ->
    pick_prop(<<"log_dir">>, ConfigProps).

pick_prop(Tag, ConfigProps) ->
    binary_to_list(lux_log:find_config(Tag, ConfigProps, undefined)).

orig_script(A, Script) when is_list(Script) ->
    RelScript = rel_script(A, Script),
    filename:join([A#astate.suite_log_dir, RelScript ++ ".orig"]);
orig_script(A, BinScript) when is_binary(BinScript) ->
    list_to_binary(orig_script(A, binary_to_list(BinScript))).

rel_script(A, Script) when is_list(Script) ->
    case drop_run_dir_prefix(A, Script) of
        "/" ++ RelScript ->
            ok;
        RelScript ->
            ok
    end,
    RelScript;
rel_script(A, BinScript) when is_binary(BinScript) ->
    list_to_binary(rel_script(A, binary_to_list(BinScript))).

rel_log(A, AbsLog) when is_list(AbsLog) ->
    case drop_run_log_prefix(A, AbsLog) of
        "/" ++ RelLog ->
            ok;
        RelLog ->
            ok
    end,
    drop_rel_dir_prefix(A, RelLog);
rel_log(A, BinLog) when is_binary(BinLog) ->
    list_to_binary(rel_log(A, binary_to_list(BinLog))).

rel_orig_script(A, AbsScript, Level) when is_list(AbsScript) ->
    case drop_prefix(A#astate.suite_log_dir, AbsScript) of
        "/" ++ RelScript ->
            RelScript;
        RelScript when Level =:= main ->
            drop_rel_dir_prefix(A, RelScript);
        RelScript when Level =:= include ->
            add_up_dir(A, RelScript, Level)
    end;
rel_orig_script(A, BinScript, Level) when is_binary(BinScript) ->
    list_to_binary(rel_orig_script(A, binary_to_list(BinScript), Level)).

html_suffix_href(HtmlFile, Protocol, Name, Label, Suffix) ->
    Name2 = insert_html_suffix(HtmlFile, Name, Suffix),
    html_href(Protocol, Name2, Label).

insert_html_suffix(HtmlFile, Name, Suffix) ->
    Ext = filename:extension(HtmlFile),
    BaseName = filename:basename(HtmlFile, Ext),
    BaseName ++ Suffix ++ Ext ++ Name.

html_href("a", "", Protocol, Name, Label) ->
    ["\n",html_href(Protocol, Name, Label)];
html_href(Tag, Prefix, Protocol, Name, Label) when Tag =/= "" ->
    [
     "\n<", Tag, ">",
     Prefix, html_href(Protocol, Name, Label),
     "</", Tag, ">\n"
    ].

html_href(Protocol, Name, Label) ->
    [
     "<a href=\"", Protocol, Name, "\">", Label, "</a>"
    ].

html_anchor(Tag, Prefix, Name, Label) ->
    [
     "\n<", Tag, ">", Prefix, html_anchor(Name, Label), "</", Tag, ">\n"
    ].

html_anchor(Name, Label) ->
    [
     "<a name=\"", Name, "\">", Label, "</a>"
    ].

html_header(Title) ->
    [
     <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" "
       "\"http://www.w3.org/TR/html4/strict.dtd\">\n">>,
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
        background-color: #CC3333
  }

  td.secondary_fail {
        background-color: #F26C4F
  }

  td.none {
        background-color: #80FF80
  }

  td.success {
        background-color: #00A651
  }

  td.no_data {
        background-color: #FFFFFF
  }
  </style>

">>.
