%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_html_annotate).

-export([generate/4]).

-include("lux.hrl").

-record(astate,
        {run_dir,
         run_log_dir,
         new_log_dir,
         suite_log_dir,
         log_file,
         script_file,
         case_prefix,
         start_time,
         end_time,
         html,
         opts}).

generate(IsRecursive, LogFile, SuiteLogDir, Opts)
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
        {ok, "", Html} ->
            lux_html_utils:safe_write_file(AbsLogFile ++ ".html", Html);
        {ok, Csv, Html} ->
            lux_html_utils:safe_write_file(AbsLogFile ++ ".csv", Csv),
            lux_html_utils:safe_write_file(AbsLogFile ++ ".html", Html);
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
            StartTime = pick_time_prop(<<"start time">>, ConfigProps),
            EndTime = pick_time_prop(<<"end time">>, ConfigProps),
            A = A0#astate{run_dir = RunDir,
                          run_log_dir = RunLogDir,
                          start_time = StartTime,
                          end_time = EndTime},
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
                                case generate(IsRecursive,
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
            {ok, "", Html};
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
                ["    <td><strong>",
                 lux_html_utils:html_href(L, S), "</strong></td>\n"]
        end,
    [
     lux_html_utils:html_header(["Lux summary log (", Dir, ")"]),
     "<table border=\"1\">\n",
     "  <tr>\n",
     "    <td><strong>Log files:</strong></td>\n",
     LogFun(RelSummaryLog, "Summary"),
     LogFun(RelResultLog, "Result"),
     LogFun(RelConfigLog, "Config"),
     LogFun(RelTapLog, "TAP"),
     "  </tr>\n",
     "</table>\n\n",
     lux_html_utils:html_href("h3", "", "", "#suite_config",
                              "Suite configuration"),
     html_summary_result(A, Result, Groups, IsTmp),
     html_groups2(A, Groups),
     lux_html_utils:html_anchor("h2", "", "suite_config",
                                "Suite configuration:"),
     html_div(<<"event">>, ConfigSection),
     lux_html_utils:html_footer()
    ].

html_summary_result(A, {result, Summary, Sections}, Groups, IsTmp) ->
    %% io:format("Sections: ~p\n", [Sections]),
    ResultString = choose_tmp(IsTmp, "Preliminary ", "Final "),
    PrelScriptSection =
        case lux_utils:pick_opt(next_script, A#astate.opts, undefined) of
            undefined ->
                "";
            NextScript ->
                Base = filename:basename(NextScript),
                SuiteLogDir = A#astate.suite_log_dir,
                CaseLogDir = lux_case:case_log_dir(SuiteLogDir, NextScript),
                EventLog = filename:join([CaseLogDir, Base ++ ".event.log"]),
                ConfigLog = filename:join([CaseLogDir, Base ++ ".config.log"]),
                LogFun =
                    fun(L, S) ->
                            ["    <td><strong>",
                             lux_html_utils:html_href(drop_prefix(SuiteLogDir,
                                                                  L), S),
                             "</strong></td>\n"]
                    end,
                [
                 "\n<h2>Premature logs for current test case:",
                 "</h2>\n",
                 "<table border=\"1\">\n",
                 "  <tr>\n",
                 "    <td><strong>",
                 drop_run_dir_prefix(A, NextScript),
                 "</strong></td>\n",
                 LogFun(EventLog, "Event"),
                 LogFun(ConfigLog, "Config"),
                 "  </tr>\n",
                 "</table>\n\n<br\>\n"
                ]
        end,
    TimeHtml =
        case elapsed_time(A#astate.start_time, A#astate.end_time) of
            undefined ->
                ["<h3>Start time: ", A#astate.start_time, "</h3>\n"];
            MicrosDiff ->
                DiffStr = elapsed_time_to_str(MicrosDiff),
                ["<h3>Elapsed time: ", DiffStr, "</h3>\n"]
        end,

    [
     "\n", TimeHtml,
     "<h2>", ResultString, "result: ", Summary, "</h2>\n",
     PrelScriptSection,
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
     "<strong>", lux_html_utils:html_quote(Slogan), ": ", Count, "</strong>\n",
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
            [lux_html_utils:html_href("#" ++ RelFile, Label), "\n"];
        [HtmlLog|_] ->
            [lux_html_utils:html_href(drop_run_log_prefix(A, HtmlLog), Label),
             "\n"]
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
     lux_html_utils:html_anchor(RelScript, ""),
     "\n",
     lux_html_utils:html_href("h2", "Test case: ", "", RelHtmlLog,
                              PrefixedRelScript),
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
     lux_html_utils:html_anchor(RelScript, ""),
     lux_html_utils:html_href("h2", "Test case: ", "", [RelScript, ".orig"],
                              [PrefixedRelScript]),
     "\n<div class=\"case\"><pre>",
     "\n<", Tag, ">Result: <strong>",
     lux_html_utils:html_quote(Reason),
     "</strong></",
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
     "\n<", Tag, ">Description: <strong>",
     lux_html_utils:html_quote(Slogan),
     "</strong></",
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
                Events = lux_log:parse_events(EventBins, []),
                %% io:format("Events: ~p\n", [Events]),
                StartTime = pick_event_time(<<"start_time">>, Events),
                EndTime = pick_event_time(<<"end_time">>, Events),
                ConfigProps = lux_log:split_config(ConfigBins),
                RunDir = pick_run_dir(ConfigProps),
                RunLogDir = pick_log_dir(ConfigProps),
                A2 = A#astate{run_dir = RunDir,
                              run_log_dir = RunLogDir,
                              start_time = StartTime,
                              end_time = EndTime},
                Timers = lux_log:extract_timers(Events),
                %% io:format("\nTimers for ~s:\n\t~p\n",
                %%           [drop_run_dir_prefix(A2, Script), Timers]),
                Csv = lux_log:timers_to_csv(Timers),
                %% io:format("\nTimers for ~s:\t~s\n",
                %%           [drop_run_dir_prefix(A2, Script),
                %%            lists:flatten(Csv)]),
                OrigScript = orig_script(A2, Script),
                A3 = A2#astate{script_file = OrigScript},
                Logs = lux_log:parse_io_logs(LogBins, []),
                Result = lux_log:parse_result(ResultBins),
                {Annotated, Files} =
                    interleave_code(A3, Events, Script, 1, 999999, [], []),
                Html = html_events(A3, EventLog2, ConfigLog, Script, Result,
                                   Timers, Files, Logs, Annotated, ConfigBins),
                {ok, Csv, Html};
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

pick_event_time(Op, #event{lineno =  0,
                           shell = <<"lux">>,
                           op = Op,
                           data = [Time]}) ->
    ?b2l(Time);
pick_event_time(Op, Events = [_|_]) ->
    case Op of
        <<"start_time">> -> pick_event_time(Op, hd(Events));
        <<"end_time">>   -> pick_event_time(Op, lists:last(Events));
        _                -> pick_event_time(Op, [])
    end;
pick_event_time(_Op, _Event) ->
    "unknown".

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
                ReasonStr = OrigScript ++ ": " ++
                    file:format_error(FileReason),
                io:format("ERROR(lux): ~s\n", [ReasonStr]),
                []
        end,
    Files2 = lists:keystore(Script, 2, Files,
                            {file, Script, OrigScript}),
    Acc = [],
    do_interleave_code(A, Events, Events, ScriptComps, CodeLines2,
                       FirstLineNo, MaxLineNo, Acc, CmdStack, Files2).

do_interleave_code(A, [#event{lineno = SingleLineNo,
                              shell = Shell,
                              op = Op,
                              data = Data} = E,
                       #event{lineno =  SingleLineNo,
                              shell = Shell,
                              op = Op,
                              data = Data2} | Events],
                   OrigEvents,
                   ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, CmdStack, Files) when Op =:= <<"recv">>,
                                              Data2 =/= [<<"timeout">>]->
    %% Combine consecutive two chunks of recv data
    %% into one in order to improve readability
    [Last | Rev] = lists:reverse(Data),
    [First | Rest] = Data2,
    Data3 = lists:reverse(Rev, [<<Last/binary, First/binary>> | Rest]),
    do_interleave_code(A, [E#event{data = Data3} | Events],
                       OrigEvents,
                       ScriptComps, CodeLines, CodeLineNo,
                       MaxLineNo, Acc, CmdStack, Files);
do_interleave_code(A, [#event{lineno =  SingleLineNo,
                              shell = Shell,
                              op =_Op,
                              data = Data} | Events],
                   OrigEvents,
                   ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, CmdStack, Files) ->
    {CodeLines2, CodeLineNo2, Code} =
        pick_code(ScriptComps, CodeLines, CodeLineNo, SingleLineNo, false,
                  [], CmdStack),
    SinglePos = #cmd_pos{rev_file = ScriptComps,
                         lineno = SingleLineNo,
                         type = undefined},
    CmdStack2 = [SinglePos | CmdStack],
    Acc2 = [{event_html, CmdStack2, _Op, Shell, Data}] ++ Code ++ Acc,
    do_interleave_code(A, Events,
                       OrigEvents,
                       ScriptComps, CodeLines2, CodeLineNo2,
                       MaxLineNo, Acc2, CmdStack, Files);
do_interleave_code(A, [#body{} = B | Events],
                   OrigEvents,
                   ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, CmdStack, Files) ->
    InvokePos = #cmd_pos{rev_file = ScriptComps,
                         lineno = B#body.invoke_lineno,
                         type = undefined},
    CmdStack2 = [InvokePos | CmdStack],
    SubScript = B#body.file,
    OrigSubScript = orig_script(A, SubScript),
    SubA = A#astate{script_file = OrigSubScript},
    {SubAnnotated, Files2} =
        interleave_code(SubA, B#body.events, SubScript,
                        B#body.first_lineno, B#body.last_lineno,
                        CmdStack2, Files),
    Event = {body_html, CmdStack2, B#body.first_lineno, SubScript,
             OrigSubScript, SubAnnotated},
    do_interleave_code(A, Events,
                       OrigEvents,
                       ScriptComps, CodeLines, CodeLineNo,
                       MaxLineNo, [Event | Acc], CmdStack, Files2);
do_interleave_code(_A, [],
                   OrigEvents,
                   ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                   Acc, CmdStack, Files) ->
    X = pick_code(ScriptComps, CodeLines, CodeLineNo, MaxLineNo,
                  OrigEvents =:= [],
                  [], CmdStack),
    {_Skipped, _CodeLineNo, Code} = X,
    {lists:reverse(Code ++ Acc), Files}.

pick_code(ScriptComps, [Line | Lines], CodeLineNo, LineNo, Flush, Acc, CmdStack)
  when LineNo >= CodeLineNo; Flush ->
    CodePos = #cmd_pos{rev_file = ScriptComps,
                       lineno = CodeLineNo,
                       type = undefined},
    CmdStack2 = [CodePos | CmdStack],
    pick_code(ScriptComps, Lines, CodeLineNo+1, LineNo, Flush,
              [{code_html, CmdStack2, Line} | Acc], CmdStack);
pick_code(_ScriptComps, Lines, CodeLineNo, _LineNo, _Flush, Acc, _CmdStack) ->
    {Lines, CodeLineNo, Acc}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Return event log as HTML

html_events(A, EventLog, ConfigLog, Script, Result,
            Timers, Files, Logs, Annotated, ConfigBins)
  when is_list(EventLog), is_list(ConfigLog), is_list(Script) ->
    EventLogDir = A#astate.new_log_dir,
    EventLogBase = filename:join([EventLogDir, filename:basename(Script)]),
    ExtraLogs = EventLogBase ++ ".extra.logs",
    Dir = filename:basename(EventLogDir),
    LogFun =
        fun(L, S) ->
                ["    <td><strong>",
                 lux_html_utils:html_href(drop_prefix(EventLogDir, L), S),
                 "</strong></td>\n"]
        end,
    PrefixScript = A#astate.case_prefix ++ drop_run_dir_prefix(A, Script),
    TimeHtml =
        case elapsed_time(A#astate.start_time, A#astate.end_time) of
            undefined ->
                ["<h3>Start time: ", A#astate.start_time, "</h3>\n"];
            MicrosDiff ->
                DiffStr = elapsed_time_to_str(MicrosDiff),
                ["<h3>Elapsed time: ", DiffStr, "</h3>\n"]
        end,
    RelEventLogDir = filename:split(drop_run_log_prefix(A, EventLogDir)),
    RelSummaryLogDir = filename:join([".." || _ <- RelEventLogDir]),
    RelSummaryLog = filename:join([RelSummaryLogDir, "lux_summary.log.html"]),
    [
     lux_html_utils:html_header(["Lux event log (", Dir, ")"]),
     "\n", lux_html_utils:html_href("h2", "", "", "#annotate", PrefixScript),
     html_result("h2", Result, ""),
     "\n", TimeHtml,
     lux_html_utils:html_href("h3", "", "", "#config", "Script configuration"),
     lux_html_utils:html_href("h3", "", "", "#stats", "Script statistics"),
     lux_html_utils:html_href("h3", "", "", "#cleanup", "Cleanup"),
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
     "</table>\n",
     lux_html_utils:html_href("h3", "", "", RelSummaryLog,
                              "Back to summary log"),

     "\n", lux_html_utils:html_anchor("h2", "", "annotate",
                                      "Annotated source code"),"\n",
     html_code(A, Annotated, undefined),

     "<div class=\"code\"><pre><a name=\"cleanup\"></a></pre></div>\n",

     lux_html_utils:html_anchor("h2", "", "stats", "Script statistics:"),
     LogFun(EventLog ++ ".csv", "Csv data file"),
     html_stats(Timers),
     lux_html_utils:html_anchor("h2", "", "config", "Script configuration:"),
     html_config(ConfigBins),
     lux_html_utils:html_footer()
    ].

html_stats(OrigTimers) ->
    Timers = strip_timers(OrigTimers, []),
    [
     "<table border=\"0\">\n",
     "  <tr>\n",
     "    <td>\n",
     html_timers(#timer.shell, "Shells", Timers, OrigTimers),
     "    </td>\n",
     "    <td>\n",
     html_timers(#timer.macro, "Macros", Timers, OrigTimers),
     "    </td>\n",
     "  </tr>\n",
     "</table>\n"
    ].

strip_timers([T | Timers], Acc) ->
    case T#timer.send_lineno =:= T#timer.match_lineno andalso
         not lists:keymember(T#timer.shell, #timer.shell, Acc) of
        true  -> strip_timers(Timers, Acc); % Strip shell start
        false -> strip_timers(Timers, [T | Acc])
    end;
strip_timers([], Acc) ->
    lists:reverse(Acc).

html_timers(Pos, Label, Timers, _OrigTimers) ->
    SplitTimers = keysplit(Pos, Timers),
    Calc = fun({Tag, List}) ->
                   Sum = lists:sum([E || #timer{elapsed_time = E,
                                                status = matched} <- List]),
                   {Tag, Sum, List}
           end,
    SplitSums = lists:reverse(lists:keysort(2, lists:map(Calc, SplitTimers))),
    Total = lists:sum([Sum || {_Tag, Sum, _List} <- SplitSums]),
    F = fun(L, Data) ->
                Pretty = lux_utils:pretty_full_lineno(L),
                ToolTip = lux_utils:expand_lines(Data),
                [
                 "\n<a href=\"#", lux_html_utils:html_quote(Pretty),
                 "\" title=\"", lux_html_utils:html_quote(ToolTip),
                 "\">", Pretty, "</a>"
                ]
        end,
    Row = fun(Lab, Sum, List) ->
                  [
                   "  <tr>\n",
                   "    <td>",
                   case Lab of
                       ""   -> "N/A";
                       <<>> -> "N/A";
                       _    -> Lab
                   end,
                   "</td>\n",
                   if
                       Sum =:= undefined ->
                           [
                            "    <td></td>\n",
                            "    <td></td>\n"
                           ];
                       is_integer(Sum) ->
                           Perc =
                               case Total of
                                   0 -> 0;
                                   _ -> (Sum*100) div Total
                               end,
                           [
                            "    <td align=\"right\">", ?i2l(Sum), "</td>\n",
                            "    <td align=\"right\">", ?i2l(Perc), "%</td>\n"
                           ]
                   end,
                   "    <td></td>\n",
                   case List of
                       [] ->
                           [
                            "    <td>", "Match", "</td>\n",
                            "    <td>", "Send", "</td>\n"
                           ];
                       _ ->
                           [
                            "    <td></td>\n",
                            "    <td></td>\n"
                           ]
                   end,
                   "  </tr>\n",
                   [["  <tr>\n",
                     "    <td></td>\n",
                     "    <td></td>\n",
                     "    <td></td>\n",
                     "    <td align=\"right\">",
                     ?i2l(T#timer.elapsed_time),
                     "</td>\n",
                     "    <td>", F(T#timer.match_lineno,
                                   T#timer.match_data), "</td>\n",
                     "    <td>", F(T#timer.send_lineno,
                                   T#timer.send_data), "</td>\n",
                     "  </tr>\n"] || T <- List,
                                     T#timer.status =:= matched]

                  ]
          end,
    [
     "<table border=\"1\">\n",
     Row(["<strong>", Label, "</strong>"], Total, []),
     [Row(Tag, Sum, List) || {Tag, Sum, List} <- SplitSums],
     "</table>\n\n"
    ].

html_result(Tag, {result, Result}, HtmlLog) ->
    case Result of
        success ->
            ["\n<", Tag, ">Result: <strong>SUCCESS</strong></", Tag, ">\n"];
        warning ->
            ["\n<", Tag, ">Result: <strong>WARNING</strong></", Tag, ">\n"];
        skip ->
            ["\n<", Tag, ">Result: <strong>SKIP</strong></", Tag, ">\n"];
        {skip, _} ->
            ["\n<", Tag, ">Result: <strong>SKIP</strong></", Tag, ">\n"];
        {error_line, RawLineNo, Reason} ->
            Anchor = RawLineNo,
            [
             "\n<", Tag, ">Result: <strong>ERROR at line ",
             lux_html_utils:html_href([HtmlLog, "#", Anchor], Anchor),
             "<h3>Reason</h3>",
             html_div(<<"event">>, lux_utils:expand_lines(Reason))
            ];
        {error, Reason} ->
            [
             "\n<", Tag, ">Result: <strong>ERROR</strong></", Tag, ">\n",
             "<h3>Reason</h3>",
             html_div(<<"event">>, lux_utils:expand_lines(Reason))
            ];
        {warning, RawLineNo, Expected0, Actual, Details} ->
            Anchor = RawLineNo,
            Expected = lux_utils:expand_lines(Expected0),
            Expected2 = lux_utils:normalize_newlines(Expected),
            Expected3 = binary:split(Expected2, <<"\\R">>, [global]),
            Diff = lux_utils:diff(Expected3, Details),
            HtmlDiff = html_diff(Diff),
            [
             "\n<", Tag, ">Result: <strong>",
             lux_html_utils:html_href([HtmlLog, "#failed"], "FAILED"),
             " at line ",
             lux_html_utils:html_href([HtmlLog, "#", Anchor], Anchor),
             "</strong></", Tag, ">\n",
             "<h3>Expected</h3>",
              html_div(<<"event">>, lux_utils:expand_lines(Expected3)),
             "<h3>Actual: ", lux_html_utils:html_quote(Actual), "</h3>",
             [
              "\n<div class=\"event\"><pre>",
              lux_utils:expand_lines(HtmlDiff),
              "</pre></div>"
             ]
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
             lux_html_utils:html_href([HtmlLog, "#failed"], "FAILED"),
             " at line ",
             lux_html_utils:html_href([HtmlLog, "#", Anchor], Anchor),
             "</strong></", Tag, ">\n",
             "<h3>Expected</h3>",
              html_div(<<"event">>, lux_utils:expand_lines(Expected3)),
             "<h3>Actual: ", lux_html_utils:html_quote(Actual), "</h3>",
             [
              "\n<div class=\"event\"><pre>",
              lux_utils:expand_lines(HtmlDiff),
              "</pre></div>",
              lux_html_utils:html_anchor("failed", "")
             ]
            ]
    end.

html_diff(Diff) ->
    html_diff(Diff, [], first, false).

html_diff([H|T], Acc, Where, Rep) ->
    Plain = "",
    Bold = "b",
    case H of
        {'=', Com} ->
            html_diff(T, [{<<"  ">>, "black",Bold,clean,Com}|Acc], middle, Rep);
        {'+', Ins} when Where =/= first, element(1,hd(T)) =:= common ->
            html_diff(T, [{<<"+ ">>,"blue",Bold,clean,Ins}|Acc], middle, Rep);
        {'+', Ins} ->
            html_diff(T, [{<<"  ">>,"black",Plain,clean,Ins}|Acc], middle, Rep);
        {'-', Del} ->
            html_diff(T, [{<<"- ">>,"red",Bold,clean,Del}|Acc], middle, Rep);
        {'!', Ins, Del} when Where =:= first, T =:= [] ->
            %% Display single replace as insert
            {Clean, _Del2, Ins2} = html_part(Del, Ins),
%%          html_diff(T, [{<<"- ">>,"red",Bold,Clean,Del2},
%%                        {<<"+ ">>,"blue",Bold,Clean,Ins2}|Acc], middle, true)
            html_diff(T, [{<<"  ">>,"black",Bold,Clean,Ins2}|Acc],middle, true);
        {'!', Ins, Del} ->
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
        {'=', Com} ->
            CleanCom = lux_html_utils:html_quote(Com),
            html_part_diff(T,
                           [CleanCom|DelAcc],
                           [CleanCom|InsAcc]);
        {'+', Ins} ->
            html_part_diff(T,
                           DelAcc,
                           [tag(Underline,
                                lux_html_utils:html_quote(Ins))|InsAcc]);
        {'-', Del} ->
            html_part_diff(T,
                           [tag(Underline,
                                lux_html_utils:html_quote(Del))|DelAcc],
                           InsAcc);
        {'!', Ins, Del} ->
            html_part_diff(T,
                           [tag(Underline,
                                lux_html_utils:html_quote(Del))|DelAcc],
                           [tag(Underline,
                                lux_html_utils:html_quote(Ins))|InsAcc])
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
               true              -> lux_html_utils:html_anchor("failed", "")
           end,
           "<font color=\"",Color,"\">",
           opt_tag(Style, opt_clean(Prefix, Clean, Line)),
           "</font>"])
     | html_color([{Prefix, Color, Style, Clean, Lines} | LineSpec], Delay)
    ];
html_color([{_Prefix, _Color, _Style, _Clean, []} | LineSpec], _Delay) ->
    html_color(LineSpec, false);
html_color([], _Delay) ->
    [].

opt_clean(Prefix, Clean, Line) ->
    [
     lux_html_utils:html_quote(Prefix),
     case Clean of
         clean   -> lux_html_utils:html_quote(Line);
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
     lux_html_utils:html_href(rel_log(A, Stdin), "Stdin"),
     "</strong></td>",
     "<td><strong>",
     lux_html_utils:html_href(rel_log(A, Stdout), "Stdout"),
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
             lux_html_utils:html_anchor(FullLineNo, FullLineNo), ": ",
             lux_html_utils:html_quote(Code),
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
             lux_html_utils:html_quote(Html),
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
                                  [["\nentering file: ", RelSubScript]]),
                     html_code(A, SubAnnotated, event),
                     html_change_div_mode(code, event),
                     lux_html_utils:html_anchor(FullLineNo, FullLineNo), ": ",
                     html_change_div_mode(event, code),
                     html_opt_div(<<"file">>,
                                  [["exiting file: ", RelSubScript]]),
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
        _            -> lux_html_utils:html_quote(Html)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

html_div(Class, Html) ->
    [
     "\n<div class=\"", Class, "\"><pre>",
     lux_html_utils:html_quote(Html),
     "</pre></div>\n"
    ].

html_scripts(A, [{file, Path, OrigScript} | Files], Level) ->
    RelScript = rel_orig_script(A, OrigScript, Level),
    RelPath0 = drop_run_dir_prefix(A, Path),
    RelPath = add_up_dir(A, RelPath0, Level),
    [
     "\n<br/>", lux_html_utils:html_href(RelScript, RelPath),
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

pick_time_prop(Tag, ConfigProps) ->
    ?b2l(lux_log:find_config(Tag, ConfigProps, <<"unknown">>)).

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

elapsed_time(StartStr, EndStr) ->
    Start = parse_time(StartStr),
    End = parse_time(EndStr),
    if
        Start =:= undefined ->
            undefined;
        End =:= undefined ->
            undefined;
        true ->
            {StartDateTime, StartMicros} = Start,
            {EndDateTime, EndMicros} = End,
            MicrosDiff = EndMicros - StartMicros,
            if
                StartDateTime =:= EndDateTime ->
                    MicrosDiff;
                true ->
                    {Days, Time} = calendar:time_difference(StartDateTime,
                                                            EndDateTime),
                    {Hours, Mins, Secs} = Time,
                    Hours2 = Hours + (Days * 24),
                    Secs2 = calendar:time_to_seconds({Hours2, Mins, Secs}),
                    (Secs2 * 1000000) + MicrosDiff
            end
    end.

%% Format: 2016-11-25 10:51:18.307279
parse_time("unknown") ->
    undefined;
parse_time(DateTime) ->
    [Date, Time] = string:tokens(DateTime, " "),
    [Year, Mon, Day] = string:tokens(Date, "-"),
    [Time2, Millis] = string:tokens(Time, "."),
    [Hour, Min, Sec] = string:tokens(Time2, ":"),
    {{{list_to_integer(Year), list_to_integer(Mon), list_to_integer(Day)},
      {list_to_integer(Hour), list_to_integer(Min), list_to_integer(Sec)}},
     list_to_integer(Millis)}.

elapsed_time_to_str(MicrosDiff) ->
    TotalSecs = MicrosDiff div 1000000,
    Micros = MicrosDiff - (TotalSecs * 1000000),
    {Hours, Mins, Secs} = calendar:seconds_to_time(TotalSecs),
    lists:concat([Hours, ":", Mins, ":", Secs, ".",
                  string:right(?i2l(Micros), 6, $0), " (h:m:s.us)"]).

%% Collect list of tuples and group them according to their tag
%%
%% The internal ordering is kept:
%%
%%   keysplit(1, [{3,3},{3,1},{3,2},{1,1},{1,2},{2,2},{2,1},{1,3}]).
%%   -> [{3,[{3,3},{3,1},{3,2}]},
%%       {1,[{1,1},{1,2},{1,3}]},
%%       {2,[{2,2},{2,1}]}]

keysplit(Pos, List) ->
    do_keysplit(Pos, List, []).

do_keysplit(Pos, [H | T], Acc) ->
    Tag = element(Pos, H),
    case lists:keyfind(Tag, 1, Acc) of
        false ->
            NewAcc = [{Tag, [H]} | Acc],
            do_keysplit(Pos, T, NewAcc);
        {Tag, Old} ->
            NewAcc = lists:keyreplace(Tag, 1, Acc, {Tag, [H|Old]}),
            do_keysplit(Pos, T, NewAcc)
    end;
do_keysplit(_Pos, [], Acc) ->
    lists:reverse([{Tag, lists:reverse(List)} || {Tag, List} <- Acc]).
