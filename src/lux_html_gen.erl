%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2016 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_html_gen).

-export([annotate_log/4, history/3]).
-export([keysplit/2, keysplit/3]).

-include("lux.hrl").

-record(astate,
        {run_dir,
         run_log_dir,
         new_log_dir,
         suite_log_dir,
         log_file,
         script_file,
         case_prefix,
         html,
         opts}).

annotate_log(IsRecursive, LogFile, SuiteLogDir, Opts)
  when is_list(LogFile), is_list(SuiteLogDir) ->
    A = init_astate(LogFile, SuiteLogDir, Opts),
    AbsLogFile = A#astate.log_file,
    IsEventLog = lists:suffix("event.log", AbsLogFile),
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

init_astate(LogFile, SuiteLogDir, Opts) ->
    AbsLogFile = lux_utils:normalize(LogFile),
    LogDir = filename:dirname(AbsLogFile),
    CasePrefix = lux_utils:pick_opt(case_prefix, Opts, ""),
    Html = lux_utils:pick_opt(html, Opts, enable),
    #astate{new_log_dir = LogDir,
            log_file = AbsLogFile,
            suite_log_dir = SuiteLogDir,
            case_prefix = CasePrefix,
            html = Html,
            opts = Opts}.

safe_write_file(File, IoList) when is_binary(File) ->
    safe_write_file(?b2l(File), IoList);
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

annotate_summary_log(IsRecursive, #astate{log_file=AbsSummaryLog} = A0)
  when is_list(AbsSummaryLog) ->
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
                                    {ok, _, _} = ValRes ->
                                        ValRes;
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

html_groups(A, SummaryLog, Result, Groups, ConfigSection)
  when is_list(SummaryLog) ->
    Dir = filename:basename(filename:dirname(SummaryLog)),
    RelSummaryLog = drop_new_log_prefix(A, SummaryLog),
    RelResultLog = "lux_result.log",
    RelConfigLog = "lux_config.log",
    RelTapLog = "lux.tap",
    IsTmp = lux_log:is_temporary(SummaryLog),
    LogFun =
        fun(L, S) ->
                ["    <td><strong>", html_href(L, S), "</strong></td>\n"]
        end,
    [
     html_header(["Lux summary log (", Dir, ")"]),
     "<table border=\"1\">\n",
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
     html_div(<<"event">>, ConfigSection),
     html_footer()
    ].

html_summary_result(A, {result, Summary, Sections}, Groups, IsTmp) ->
    %% io:format("Sections: ~p\n", [Sections]),
    ResultString = choose_tmp(IsTmp, "Temporary ","Final "),
    [
     "\n<h2>", ResultString, "result: ", Summary, "</h2>\n",
     "<div class=\"case\"><pre>",
     [html_summary_section(A, S, Groups) || S <- Sections],
     "</pre></div>"
    ].

choose_tmp(IsTmp, TmpString, String) ->
    case IsTmp of
        true  -> TmpString;
        false -> String
    end.

html_summary_section(A, {section, Slogan, Count, FileBins}, Groups) ->
    [
     "<strong>", html_quote(Slogan), ": ", Count, "</strong>\n",
     case FileBins of
         [] ->
             [];
         _ ->
             [
              "<div class=\"event\"><pre>",
              [html_summary_file(A, F, Groups) || F <- FileBins],
              "</pre></div>"
             ]
     end
    ].

html_summary_file(A, {file, FileBin, LineNo}, Groups)
  when is_binary(FileBin) ->
    File = ?b2l(FileBin),
    Files =
        [HtmlLog ||
            {test_group, _Group, Cases} <- Groups,
            {test_case, Name, _Log, _Doc, HtmlLog, _Res} <- Cases,
            File =:= Name],
    PrefixedRelScript = prefixed_rel_script(A, File),
    RelFile = chop_root(drop_run_dir_prefix(A, File)),
    Label = [PrefixedRelScript, ":", LineNo],
    case Files of
        [] ->
            [html_href("#" ++ RelFile, Label), "\n"];
        [HtmlLog|_] ->
            [html_href(drop_run_log_prefix(A, HtmlLog), Label), "\n"]
    end.

html_groups2(A, [{test_group, _Group, Cases} | Groups]) ->
    [
     html_cases(A, Cases),
     html_groups2(A, Groups)
    ];
html_groups2(_A, []) ->
    [].

html_cases(A, [{test_case, AbsScript, _Log, Doc, HtmlLog, Res} | Cases])
  when is_list(AbsScript) ->
    Tag = "a",
    PrefixedRelScript = prefixed_rel_script(A, AbsScript),
    RelScript = drop_run_dir_prefix(A, AbsScript),
    RelHtmlLog = drop_run_log_prefix(A, HtmlLog),
    [
     html_anchor(RelScript, ""),
     "\n",
     html_href("h2", "Test case: ", "", RelHtmlLog, PrefixedRelScript),
     "\n<div class=\"case\"><pre>",
     html_doc(Tag, Doc),
     %% html_href(Tag, "Raw event log: ", "", RelEventLog, RelEventLog),
     %% html_href(Tag, "Annotated script: ", "", RelHtmlLog, RelHtmlLog),
     html_result(Tag, Res, RelHtmlLog),
     "\n",
     "</pre></div>",
     html_cases(A, Cases)
    ];
html_cases(A, [{result_case, AbsScript, Reason, Details} | Cases])
  when is_list(AbsScript) ->
    Tag = "a",
    PrefixedRelScript = prefixed_rel_script(A, AbsScript),
    RelScript = chop_root(drop_run_dir_prefix(A, AbsScript)),
    [
     html_anchor(RelScript, ""),
     html_href("h2", "Test case: ", "", [RelScript, ".orig"],
               [PrefixedRelScript]),
     "\n<div class=\"case\"><pre>",
     "\n<", Tag, ">Result: <strong>", html_quote(Reason), "</strong></",
     Tag, ">\n",
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
     "\n<", Tag, ">Description: <strong>", html_quote(Slogan), "</strong></",
     Tag, ">\n",
     case Desc of
         [] ->
             [];
         _ ->
             html_div(<<"event">>, lux_utils:expand_lines(Desc))
     end
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Annotate a lux with events from the log

annotate_event_log(#astate{log_file=EventLog} = A) when is_list(EventLog) ->
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

interleave_code(A, Events, Script, FirstLineNo, MaxLineNo, CmdStack, Files)
  when is_list(Script) ->
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
                ReasonStr = ?b2l(OrigScript) ++ ": " ++
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
    %% Combine consecutive two chunks of recv data
    %% into one in order to improve readability
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
    SinglePos = #cmd_pos{rev_file = ScriptComps,
                         lineno = SingleLineNo,
                         type = undefined},
    CmdStack2 = [SinglePos | CmdStack],
    Acc2 = [{event_html, CmdStack2, _Op, Shell, Data}] ++ Code ++ Acc,
    do_interleave_code(A, Events, ScriptComps, CodeLines2, CodeLineNo2,
                       MaxLineNo, Acc2, CmdStack, Files);
do_interleave_code(A, [{body, InvokeLineNo, FirstLineNo, LastLineNo,
                        SubScript, SubEvents} | Events],
                   ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, CmdStack, Files) when is_list(SubScript) ->
    InvokePos = #cmd_pos{rev_file = ScriptComps,
                         lineno = InvokeLineNo,
                         type = undefined},
    CmdStack2 = [InvokePos | CmdStack],
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
    CodePos = #cmd_pos{rev_file = ScriptComps,
                       lineno = CodeLineNo,
                       type = undefined},
    CmdStack2 = [CodePos | CmdStack],
    pick_code(ScriptComps, Lines, CodeLineNo+1, LineNo,
              [{code_html, CmdStack2, Line} | Acc], CmdStack);
pick_code(_ScriptComps, Lines, CodeLineNo, _LineNo, Acc, _CmdStack) ->
    {Lines, CodeLineNo, Acc}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Return event log as HTML

html_events(A, EventLog, ConfigLog, Script, Result, Files,
            Logs, Annotated, ConfigBins)
  when is_list(EventLog), is_list(ConfigLog), is_list(Script) ->
    EventLogDir = A#astate.new_log_dir,
    EventLogBase = filename:join([EventLogDir, filename:basename(Script)]),
    ExtraLogs = EventLogBase ++ ".extra.logs",
    Dir = filename:basename(EventLogDir),
    LogFun =
        fun(L, S) ->
                ["    <td><strong>",
                 html_href(drop_prefix(EventLogDir, L), S),
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
     "<table border=\"1\">\n",
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
     html_code(A, Annotated, undefined),

     "<div class=\"code\"><pre><a name=\"cleanup\"></a></pre></div>\n",

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
             html_href([HtmlLog, "#", Anchor], Anchor),
             "<h3>Reason</h3>",
             html_div(<<"event">>, lux_utils:expand_lines(Reason))
            ];
        {error, Reason} ->
            [
             "\n<", Tag, ">Result: <strong>ERROR</strong></", Tag, ">\n",
             "<h3>Reason</h3>",
             html_div(<<"event">>, lux_utils:expand_lines(Reason))
            ];
        {fail, RawLineNo, Expected0, Actual, Details} ->
            Anchor = RawLineNo,
            Expected = lux_utils:expand_lines(Expected0),
            Expected2 = lux_utils:normalize_newlines(Expected),
            Expected3 = binary:split(Expected2, <<"\\R">>, [global]),
            Diff = lux_utils:diff(Expected3, Details),
            HtmlDiff = html_diff(Diff),
            [
             "\n<", Tag, ">Result: <strong>",
             html_href([HtmlLog, "#failed"], "FAILED"),
             " at line ",
             html_href([HtmlLog, "#", Anchor], Anchor),
             "</strong></", Tag, ">\n",
             "<h3>Expected</h3>",
              html_div(<<"event">>, lux_utils:expand_lines(Expected3)),
             "<h3>Actual: ", html_quote(Actual), "</h3>",
             [
              "\n<div class=\"event\"><pre>",
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
    Diff = lux_utils:diff(?b2l(Del), ?b2l(Ins)),
    html_part_diff(Diff, [], []);
html_part(Del, Ins) ->
    {clean, Del, Ins}.

html_part_diff([H|T], DelAcc, InsAcc) ->
    Underline = "u",
    case H of
        {common, Com} ->
            CleanCom = html_quote(Com),
            html_part_diff(T,
                           [CleanCom|DelAcc],
                           [CleanCom|InsAcc]);
        {insert, Ins} ->
            html_part_diff(T,
                           DelAcc,
                           [tag(Underline,html_quote(Ins))|InsAcc]);
        {delete, Del} ->
            html_part_diff(T,
                           [tag(Underline,html_quote(Del))|DelAcc],
                           InsAcc);
        {replace, Ins, Del} ->
            html_part_diff(T,
                           [tag(Underline,html_quote(Del))|DelAcc],
                           [tag(Underline,html_quote(Ins))|InsAcc])
    end;
html_part_diff([], DelAcc, InsAcc) ->
    {noclean,
     [?l2b(lists:reverse(DelAcc))],
     [?l2b(lists:reverse(InsAcc))]}.

html_color([{Prefix, Color, Style, Clean, [Line|Lines]} | LineSpec], Delay) ->
    [
     ?l2b([if
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
     html_quote(Prefix),
     case Clean of
         clean   -> html_quote(Line);
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
    html_div(<<"event">>, lux_utils:expand_lines(Config));
html_config(Config) when is_binary(Config) ->
    html_div(<<"event">>, Config).

html_logs(A, [{log, ShellName, Stdin, Stdout} | Logs]) ->

    [
     "\n<tr>\n    ",
     "<td><strong>Shell ", ShellName, "</strong></td>",
     "<td><strong>",
     html_href(rel_log(A, Stdin), "Stdin"),
     "</strong></td>",
     "<td><strong>",
     html_href(rel_log(A, Stdout), "Stdout"),
     "</strong></td>",
     "\n</tr>\n",
     html_logs(A, Logs)
    ];
html_logs(_A, []) ->
    [].

html_code(A, Annotated, Prev) ->
    html_code2(A, Annotated, Prev, Prev).

html_code2(A, [Ann | Annotated], Prev, Orig) ->
    case Ann of
        {code_html, CmdStack, Code} ->
            Curr = code,
            FullLineNo = lux_utils:pretty_full_lineno(CmdStack),
            [
             html_change_div_mode(Curr, Prev),
             case Code of
                 <<"[cleanup]">> -> "<a name=\"cleanup\"></a>\n";
                 _               -> ""
             end,
             html_anchor(FullLineNo, FullLineNo), ": ",
             html_quote(Code),
             "\n",
             html_code2(A, Annotated, Curr, Orig)
            ];
        {event_html, CmdStack, Op, Shell, Data0} ->
            Curr = event,
            Data =
                case Op of
                    <<"expect">> ->
                        lists:append([binary:split(D, <<"\\\\R">>, [global]) ||
                                         D <- Data0]);
                    _ ->
                        Data0
                end,
            FullLineNo = lux_utils:pretty_full_lineno(CmdStack),
            Html = ["\n", Shell, "(", FullLineNo, "): ", Op, " "],
            [
             html_change_div_mode(Curr, Prev),
             html_quote(Html),
             html_opt_div(Op, Data),
             html_code2(A, Annotated, Curr, Orig)
            ];
        {body_html, CmdStack, _MacroLineNo, SubScript,
         OrigSubScript, SubAnnotated} ->
            FullLineNo = lux_utils:pretty_full_lineno(CmdStack),
            RelSubScript = drop_run_dir_prefix(A, SubScript),
            if
                OrigSubScript =/= A#astate.script_file ->
                    [
                     html_change_div_mode(event, Prev),
                     html_opt_div(<<"file">>,
                                  ["entering file: ", RelSubScript]),
                     html_code(A, SubAnnotated, event),
                     html_change_div_mode(code, event),
                     html_anchor(FullLineNo, FullLineNo), ": ",
                     html_change_div_mode(event, code),
                     html_opt_div(<<"file">>,
                                  ["exiting file: ", RelSubScript]),
                     html_code2(A, Annotated, event, Orig)
                    ];
                true ->
                    [
                     html_code(A, SubAnnotated, Prev),
                     %% html_anchor(FullLineNo, FullLineNo), ": \n",
                     html_code2(A, Annotated, Prev, Orig)
                    ]
            end
    end;
html_code2(_A, [], Prev, Orig) ->
    html_change_div_mode(Orig, Prev).

html_change_div_mode(To, From) ->
    case {To, From} of
        {undefined, undefined} -> "";
        {undefined, code}      -> "</pre></div>\n";
        {undefined, event}     -> "</pre></div>\n";
        {code, undefined}      -> "\n<div class=\"code\"><pre>\n";
        {code, code}           -> "";
        {code, event}          -> "</pre></div>\n<div class=\"code\"><pre>";
        {event, undefined}     -> "\n<div class=\"event\"><pre>\n";
        {event, event}         -> "";
        {event, code}          -> "</pre></div>\n<div class=\"event\"><pre>\n"
    end.

html_opt_div(Op, Data) ->
    Html = lux_utils:expand_lines(Data),
    case Op of
        <<"send">>   -> html_div(Op, Html);
        <<"recv">>   -> html_div(Op, Html);
        <<"expect">> -> html_div(Op, Html);
        <<"skip">>   -> html_div(Op, Html);
        <<"match">>  -> html_div(Op, Html);
        _            -> html_quote(Html)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History

-define(CURRENT_SUFFIX, "_current").
-define(CONFIG_SUFFIX, "_config").
-define(HOST_SUFFIX, "_host").

-record(table, {name, res, iolist}).
-record(row,   {res, iolist}).
-record(cell,  {res, run, iolist}).

history(RelTopDir, RelHtmlFile, Opts) ->
    AbsTopDir = lux_utils:normalize(RelTopDir),
    AbsHtmlFile = lux_utils:normalize(RelHtmlFile),
    NewLogDir = filename:dirname(AbsHtmlFile),
    {AllRuns, Errors} = parse_summary_logs(AbsTopDir, [], [], Opts),
    io:format("~p test runs (~p errors)", [length(AllRuns), length(Errors)]),
    SplitHosts = keysplit(#run.hostname, AllRuns),
    LatestRuns = latest_runs(SplitHosts),
    HostTables = html_history_table_hosts(NewLogDir, SplitHosts, AbsHtmlFile),
    SplitConfigs = keysplit(#run.config_name, AllRuns, fun compare_run/2),
    ConfigTables =
        html_history_table_configs(NewLogDir, SplitConfigs, AbsHtmlFile),
    HtmlDir = filename:dirname(RelHtmlFile),
    OverviewIoList =
        [
         html_history_header("overview", AllRuns,
                             ConfigTables, HostTables, HtmlDir, RelHtmlFile),
         html_history_table_latest(NewLogDir, LatestRuns, AbsHtmlFile),
         html_history_table_all(NewLogDir, AllRuns, AbsHtmlFile),
         html_footer()
        ],
    CurrentIoList =
        [
         html_history_header("current failures", AllRuns,
                             ConfigTables, HostTables, HtmlDir, RelHtmlFile),
         html_history_table_current(NewLogDir, AllRuns, AbsHtmlFile),
         html_footer()
        ],
    ConfigIoList =
        [
         html_history_header("config", AllRuns,
                             ConfigTables, HostTables, HtmlDir, RelHtmlFile),
         "<a name=\"content\"/>",
         [T#table.iolist || T <- ConfigTables],
         html_footer()
        ],
    HostIoList =
        [
         html_history_header("host", AllRuns,
                             ConfigTables, HostTables, HtmlDir, RelHtmlFile),
         "<a name=\"content\"/>",
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
    safe_write_file(ConfigHtmlFile, ConfigIoList),
    safe_write_file(HostHtmlFile, HostIoList),
    safe_write_file(CurrentHtmlFile, CurrentIoList),
    safe_write_file(RelHtmlFile, OverviewIoList),
    lux_html_parse:validate_html(RelHtmlFile, Opts).

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
            N = length(SortedRuns)
    end,
    [
     html_header(["Lux history ", Section, " (", Dir, ")"]),
     "<h1>Lux history ", Section, " (", Dir, ") generated at ",
     lux_utils:now_to_string(lux_utils:timestamp()),
     "</h1>",

     "<h3>", integer_to_list(N),
     " runs within this range of repository revisions</h3>\n",
     "<table border=\"0\">\n",
     "  <tr>\n",
     "    <td>Latest:</td><td><strong>", LatestRev, "</strong></td>\n",
     "    <td>at ", LatestTime, "</td>\n",
     "  </tr>\n",
     "  <tr>\n",
     "    <td>First:</td><td><strong>", FirstRev, "</strong></td>\n",
     "    <td>at ", FirstTime, "</td>\n",
     "  </tr>\n",
     "</table>\n\n",

     html_history_legend(),

     "<h3>",
     html_href([drop_prefix(HtmlDir, HtmlFile), "#content"],
               "Overview"),
     "</h3>\n\n",

     "<h3>",
     html_suffix_href(HtmlFile,"","#content", "Still failing test cases",
                      ?CURRENT_SUFFIX),
     "</h3>\n\n",

     "<h3>",
     html_suffix_href(HtmlFile,"","#content", "Configurations", ?CONFIG_SUFFIX),
     "</h3>\n",
     "  <table border=\"1\">\n",
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
     "  <table border=\"1\">\n",
     "    <tr>\n",
     [
      html_suffix_href_td(HtmlFile, Host, HostRes, ?HOST_SUFFIX) ||
         #table{name=Host, res=HostRes} <- HostTables
     ],
     "    </tr>\n",
     "  </table>\n"
     "<br/><hr/>\n"
    ].

html_history_legend() ->
    [
     "<h3>Legend</h3>\n",
     "  <table border=\"1\">\n",
     "    <tr>\n",
     html_history_td("First fail", fail, "left", ""),
     html_history_td("Secondary fails on same host", secondary_fail, "left",""),
     html_history_td("Skipped", none, "left", ""),
     html_history_td("Success", success, "left", ""),
     html_history_td("No data", no_data, "left", ""),
     "    </tr>\n",
     "  </table>\n"
    ].

html_history_table_latest(NewLogDir, LatestRuns, HtmlFile) ->
    T = html_history_table(NewLogDir, "Latest", "All test suites",
                           LatestRuns, HtmlFile, none, worst),
    [
     html_anchor("h3", "", "content", "Latest run on each host"),
     "\n",
     T#table.iolist
    ].

html_history_table_all(NewLogDir, AllRuns, HtmlFile) ->
    T = html_history_table(NewLogDir, "All", "All test suites",
                           AllRuns, HtmlFile, none, latest),
    [
     html_anchor("h3", "", "all_runs", "All runs"),
     T#table.iolist
    ].

html_history_table_current(NewLogDir, AllRuns, HtmlFile) ->
    Details = [D#run{details=[D]} || R <- AllRuns, D <- R#run.details],
    T = html_history_table(NewLogDir, "All", "Still failing test cases",
                           Details, HtmlFile, latest_success, latest),
    [
     html_anchor("h3", "", "content", "Still failing test cases"),
     "\n",
     T#table.iolist
    ].

html_history_table_configs(NewLogDir, SplitConfigs, HtmlFile) ->
    [
     html_history_double_table(NewLogDir,
                               ConfigName,
                               "Config: " ++ ConfigName,
                               Runs,
                               HtmlFile,
                               latest) ||
        {ConfigName, Runs} <- SplitConfigs
    ].

html_history_table_hosts(NewLogDir, SplitHosts, HtmlFile) ->
    [
     html_history_double_table(NewLogDir,
                               Host,
                               ["Host: ", Host,
                                " (", (hd(Runs))#run.config_name, ")"],
                               Runs,
                               HtmlFile,
                               latest) ||
        {Host, Runs} <- SplitHosts
    ].

html_history_double_table(NewLogDir, Name, Label, AllRuns, HtmlFile, Select) ->
    Details = [D#run{details=[D]} || R <- AllRuns, D <- R#run.details],
    AllT = html_history_table(NewLogDir, Name, "All test suites",
                              AllRuns, HtmlFile, none, Select),
    FailedT = html_history_table(NewLogDir, Name, "Failed test cases",
                                 Details, HtmlFile, any_success, Select),
    #table{name=Name,
           res=AllT#table.res,
           iolist=
               [
                "\n",
                ["<h3>", html_anchor(Name, Label), "</h3>\n"],
                AllT#table.iolist,
                "\n<br/>\n",
                FailedT#table.iolist
               ]}.

html_history_table(NewLogDir, Name, Grain, Runs, HtmlFile, Suppress, Select) ->
    SplitTests = keysplit(#run.test, Runs, fun compare_run/2),
    SplitIds = keysplit(#run.id, Runs, fun compare_run/2),
    SplitIds2 = lists:sort(fun compare_split/2, SplitIds),
    RowHistory =
        [
         html_history_row(NewLogDir, Test, TestRuns, SplitIds2, HtmlFile,
                          Select, Suppress)
         || {Test, TestRuns} <- lists:reverse(SplitTests)
        ],
    PickRes = fun(#row{res=R}, Acc) -> lux_utils:summary(Acc, R) end,
    SelectedRes = lists:foldl(PickRes, no_data, RowHistory),
    #table{name=Name,
           res=SelectedRes,
           iolist=
               [
                "  <table border=\"1\">\n",
                "    <tr>\n",
                html_history_table_td(Grain, SelectedRes, "left"),
                [["      <td>", Rev,
                  "<br/>", "<strong>", Id, "</strong>", Time,
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
                  "<br/>", html_suffix_href(HtmlFile,
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

html_history_row(NewLogDir, Test, Runs, SplitIds,
                 HtmlFile, Select, Suppress) ->
    RevRuns = lists:reverse(lists:keysort(#run.id, Runs)),
    EmitCell =
        fun({Id, _R}, AccRes) ->
                html_history_cell(NewLogDir, Test, Id, RevRuns,
                                  HtmlFile, AccRes)
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

html_history_cell(_NewLogDir, Test, Id, Runs, _HtmlFile, AccRes) ->
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
                        html_href([Log, ".html"], FailCount)
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
     "<td class=\"", atom_to_list(Res), "\"> ",
     html_suffix_href(HtmlFile,"", "#" ++ Text, Text, Suffix),
     "</td>\n"
    ].

html_history_table_td(Text, skip, Align) ->
    html_history_table_td(Text, none, Align);
html_history_table_td(Text, Res, Align) ->
    [
     "      ",
     "<td class=\"", atom_to_list(Res), "\" align=\"", Align,
     "\" rowspan=\"2\">",
     "<strong>", Text, "</strong>",
     "</td>\n"
    ].

html_history_td(Text, skip, Align, Title) ->
    html_history_td(Text, none, Align, Title);
html_history_td(Text, Res, Align, Title) ->
    [
     "    ",
     "<td class=\"", atom_to_list(Res),
     "\" align=\"", Align, "\"",
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

parse_summary_logs(TopDir, Acc, Err, Opts) ->
    Skip =
        ["lux.skip",
         "lux_summary.log",
         "lux_summary.log.tmp",
         "qmscript.skip",
         "qmscript_summary.log",
         "qmscript_summary.log.tmp",
         "qmscript.summary.log"],
    do_parse_summary_logs(TopDir, [], Acc, Err, Skip, Opts).

do_parse_summary_logs(TopDir, RelDir, Acc, Err, Skip, Opts) ->
    Dir = filename:join([TopDir, RelDir]),
    %% io:format("DIR: ~s\n", [Dir]),
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
                                    case lux_log:parse_run_summary(TopDir,
                                                                   RelDir,
                                                                   Base,
                                                                   File,
                                                                   Res,
                                                                   Opts) of
                                        {error, F, Reason} ->
                                            {Acc, [{error, F, Reason} | Err]};
                                        #run{} = R ->
                                            {[R|Acc], Err}
                                    end;
                                {error, F, Reason} ->
                                    {Acc, [{error, F, Reason} | Err]}
                            end;
                        false ->
                            io:format("s", []),
                            %% Skip
                            {Acc, Err}
                    end;
                false ->
                    %% No interesting file found. Search subdirs
                    Fun =
                        fun("latest_run", {A,E}) ->
                                %% Symlink
                                {A,E};
                           (File, {A,E}) ->
                                RelDir2 =
                                    case RelDir of
                                        "" -> File;
                                        _  -> filename:join([RelDir, File])
                                    end,
                                do_parse_summary_logs(TopDir, RelDir2,
                                                      A, E, Skip, Opts)
                        end,
                    lists:foldl(Fun, {Acc, Err}, Files)
            end;
        {error, _Reason} ->
            %% Not a dir or problem to read dir
            {Acc, Err}
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
     "\n<div class=\"", Class, "\"><pre>", html_quote(Html), "</pre></div>\n"
    ].

html_quote(IoList) ->
    Opts = [global],
    lists:foldl(fun(Fun, Acc) -> Fun(Acc) end,
                ?l2b(IoList),
                [
                 fun(B) -> ?l2b(lists:map(fun safe_ctrl/1, ?b2l(B))) end,
                 fun(B) -> safe_latin1(B, []) end,
                 fun(B) -> binary:replace(B, <<"&">>, <<"&amp;">>, Opts) end,
                 fun(B) -> binary:replace(B, <<"<">>, <<"&lt;">>, Opts) end,
                 fun(B) -> binary:replace(B, <<">">>, <<"&gt;">>, Opts) end,
                 fun(B) -> binary:replace(B, <<"\"">>, <<"&quot;">>, Opts) end
                ]).

safe_ctrl(Char) ->
    if
        Char < 32, Char =/= 10 ->
            ["<ctrl char \\", integer_to_list(Char),
             " - see plain lux log for details>"];
        true ->
            Char
    end.

safe_latin1(<<>>, Acc) ->
    ?l2b(lists:reverse(Acc));
safe_latin1(Bin, Acc) ->
    case unicode:characters_to_binary(Bin, utf8, latin1) of
        {error, Good, _Rest} ->
            BadSz = integer_to_list(byte_size(Bin) - byte_size(Good)),
            Reason = ["<illegal char(s) - skipped ", BadSz,
                      " bytes - see plain lux log for details>"],
            safe_latin1(<<>>, [Reason, Good | Acc]);
        {incomplete, Good, _Rest} ->
            BadSz = integer_to_list(byte_size(Bin) - byte_size(Good)),
            Reason = ["<incomplete char - skipped ", BadSz,
                      " bytes - see plain lux log for details>"],
            safe_latin1(<<>>, [Reason, Good | Acc]);
        Good ->
            safe_latin1(<<>>, [Good | Acc])
    end.

html_scripts(A, [{file, Path, OrigScript} | Files], Level) ->
    RelScript = rel_orig_script(A, OrigScript, Level),
    RelPath0 = drop_run_dir_prefix(A, Path),
    RelPath = add_up_dir(A, RelPath0, Level),
    [
     "\n<br/>", html_href(RelScript, RelPath),
     html_scripts(A, Files, include)
    ];
html_scripts(_A, [], _Level) ->
    ["<br/>"].

prefixed_rel_script(A, AbsScript) when is_list(AbsScript) ->
    RelScript = drop_rel_dir_prefix(A, rel_script(A, AbsScript)),
    A#astate.case_prefix ++ RelScript.

drop_rel_dir_prefix(A, RelPath) when is_list(RelPath) ->
    RelDir = drop_prefix(A#astate.suite_log_dir, A#astate.new_log_dir),
    drop_prefix(RelDir, RelPath).

add_up_dir(_A, RelPath, main) when is_list(RelPath)->
    RelPath;
add_up_dir(_A, AbsPath, include) when hd(AbsPath) =:= $/ ->
    AbsPath;
add_up_dir(A, RelPath, include) when is_list(RelPath) ->
    case drop_prefix(A#astate.suite_log_dir, A#astate.new_log_dir) of
        "." ->
            RelPath;
        RelDir ->
            UpDir = filename:join([".." || _ <- filename:split(RelDir)]),
            filename:join([UpDir, RelPath])
    end.

drop_run_dir_prefix(#astate{run_dir=LogDir}, File)
  when is_list(LogDir), is_list(File) ->
    drop_prefix(LogDir, File).

drop_run_log_prefix(#astate{run_log_dir=LogDir}, File)
  when is_list(LogDir), is_list(File) ->
    drop_prefix(LogDir, File).

drop_new_log_prefix(#astate{new_log_dir=LogDir}, File)
  when is_list(LogDir), is_list(File) ->
    drop_prefix(LogDir, File).

drop_prefix(Dir, File) when is_list(Dir), is_list(File) ->
    lux_utils:drop_prefix(Dir, File).

pick_run_dir(ConfigProps) ->
    pick_prop(<<"run_dir">>, ConfigProps).

pick_log_dir(ConfigProps) ->
    pick_prop(<<"log_dir">>, ConfigProps).

pick_prop(Tag, ConfigProps) ->
    ?b2l(lux_log:find_config(Tag, ConfigProps, undefined)).

orig_script(A, Script) when is_list(Script) ->
    RelScript = rel_script(A, Script),
    filename:join([A#astate.suite_log_dir, RelScript ++ ".orig"]).

rel_script(A, Script) when is_list(Script) ->
    chop_root(drop_run_dir_prefix(A, Script)).

rel_log(A, AbsLog) when is_list(AbsLog) ->
    RelLog = chop_root(drop_run_log_prefix(A, AbsLog)),
    drop_rel_dir_prefix(A, RelLog).

chop_root(AbsPath) ->
    case AbsPath of
        "/" ++ RelPath -> ok;
        RelPath        -> ok
    end,
    RelPath.

rel_orig_script(A, AbsScript, Level) when is_list(AbsScript) ->
    OptRelScript = drop_prefix(A#astate.suite_log_dir, AbsScript),
    RelScript = chop_root(OptRelScript),
    if
        RelScript =/= OptRelScript -> RelScript;
        Level =:= main             -> drop_rel_dir_prefix(A, RelScript);
        Level =:= include          -> add_up_dir(A, RelScript, Level)
    end.

html_suffix_href(HtmlFile, Protocol, Name, Label, Suffix)
  when is_list(HtmlFile) ->
    Name2 = insert_html_suffix(HtmlFile, Name, Suffix),
    html_href(Protocol, Name2, Label).

insert_html_suffix(HtmlFile, Name, Suffix)
  when is_list(HtmlFile), is_list(Name), is_list(Suffix) ->
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

html_href(Protocol, Name, Label) when Protocol =:= "" -> % Temporary assert
    %% AbsHref = ?b2l(?l2b([Protocol, Name])),
    %% case chop_root(AbsHref) of
    %%     AbsHref ->
    %%         io:format("ABS HREF ~p\n\t~p\n", [AbsHref, ?callstack()]);
    %%     _RelHref ->
    %%         ok
    %% end,
    [
     "<a href=\"", Protocol, html_quote(Name), "\">", Label, "</a>"
    ].

html_href(Name, Label) ->
    html_href("", Name, Label).

html_anchor(Tag, Prefix, Name, Label) ->
    [
     "\n<", Tag, ">", Prefix, html_anchor(Name, Label), "</", Tag, ">\n"
    ].

html_anchor(Name, Label) ->
    [
     "\n<a name=\"", Name, "\">", html_quote(Label), "</a>"
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
     <<"<body>\n">>
    ].

html_footer() ->
    <<"\n</body>\n</html>\n">>.

html_style() ->
<<"
<style>
  body {
        color: #000000;
        background-color: white
  }

  div {
        overflow: auto;
        padding: 2px;
        border: 1px solid #b00;
        margin-left: 2%;
        margin-bottom: 2px;
        margin-top: 2px;
        color: #000000;
        background-color: #FFFFE0
  }

  div.event {
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
