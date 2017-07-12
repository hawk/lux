%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_html_history).

-export([generate/3]).

-include("lux.hrl").

-define(CURRENT_SUFFIX, "_current").
-define(CONFIG_SUFFIX, "_config").
-define(HOST_SUFFIX, "_host").

-record(table, {name, res, rows, iolist}).
-record(row,   {res, cells, iolist}).
-record(cell,  {res, run, n_run, n_fail, iolist}).

generate(PrefixedSources, RelHtmlFile, Opts) ->
    io:format("Assembling history of logs from...", []),
    Sources = lists:map(fun split_source/1, PrefixedSources),
    generate2(Sources, RelHtmlFile, [{top, Sources} | Opts], [], []).

split_source(PrefixedSource) ->
    case lux_utils:split(PrefixedSource, "::") of
        {SuitePrefix, File} ->
            #source{suite_prefix=SuitePrefix,
                    file=File,
                    orig=PrefixedSource};
        false ->
            #source{suite_prefix=undefined,
                    file=PrefixedSource,
                    orig=PrefixedSource}
    end.

generate2([Source | Sources], RelHtmlFile, Opts, RunAcc, ErrAcc) ->
    io:format("\n\t~s", [Source#source.orig]),
    {AllRuns, Errors} =
        parse_summary_logs(Source, RelHtmlFile, RunAcc, ErrAcc, Opts),
    generate2(Sources, RelHtmlFile, Opts, AllRuns, Errors);
generate2([], RelHtmlFile, Opts, AllRuns, Errors) ->
    %% io:format("\nERRORS ~p\n", [Errors]),
    io:format("\nAnalyzed ~p test runs (~p errors)",
              [length(AllRuns), length(Errors)]),
    generate3(RelHtmlFile, AllRuns, Errors, Opts).

generate3(RelHtmlFile, AllRuns, Errors, Opts) ->
    AbsHtmlFile = lux_utils:normalize_filename(RelHtmlFile),
    NewLogDir = filename:dirname(AbsHtmlFile),
    SplitHosts = keysplit(#run.hostname, AllRuns),
    LatestRuns = latest_runs(SplitHosts),
    HostTables = table_hosts(NewLogDir, SplitHosts, AbsHtmlFile),
    SplitConfigs = keysplit(#run.config_name, AllRuns, fun compare_run/2),
    ConfigTables =
        table_configs(NewLogDir, SplitConfigs, AbsHtmlFile),
    HtmlDir = filename:dirname(RelHtmlFile),
    HtmlArgs = args(RelHtmlFile, Opts),
    HtmlErrors = errors(Errors),
    OverviewIoList =
        [
         header("overview", AllRuns,
                ConfigTables, HostTables, HtmlDir, RelHtmlFile, Errors),
         table_latest(NewLogDir, LatestRuns, AbsHtmlFile),
         table_all(NewLogDir, AllRuns, AbsHtmlFile),
         HtmlArgs,
         HtmlErrors,
         lux_html_utils:html_footer()
        ],
    CurrentIoList =
        [
         header("current failures", AllRuns,
                ConfigTables, HostTables, HtmlDir, RelHtmlFile, Errors),
         table_current(NewLogDir, AllRuns, AbsHtmlFile),
         HtmlArgs,
         HtmlErrors,
         lux_html_utils:html_footer()
        ],
    ConfigIoList =
        [
         header("config", AllRuns,
                ConfigTables, HostTables, HtmlDir, RelHtmlFile, Errors),
         "<a name=\"content\"/>",
         [T#table.iolist || T <- ConfigTables],
         HtmlArgs,
         HtmlErrors,
         lux_html_utils:html_footer()
        ],
    HostIoList =
        [
         header("host", AllRuns,
                ConfigTables, HostTables, HtmlDir, RelHtmlFile, Errors),
         "<a name=\"content\"/>",
         [T#table.iolist || T <- HostTables],
         HtmlArgs,
         HtmlErrors,
         lux_html_utils:html_footer()
        ],
    CurrentHtmlFile =
        lux_utils:join(HtmlDir,
                       insert_html_suffix(RelHtmlFile, "", ?CURRENT_SUFFIX)),
    ConfigHtmlFile =
        lux_utils:join(HtmlDir,
                       insert_html_suffix(RelHtmlFile, "", ?CONFIG_SUFFIX)),
    HostHtmlFile =
        lux_utils:join(HtmlDir,
                       insert_html_suffix(RelHtmlFile, "", ?HOST_SUFFIX)),
    lux_html_utils:safe_write_file(ConfigHtmlFile, ConfigIoList),
    lux_html_utils:safe_write_file(HostHtmlFile, HostIoList),
    lux_html_utils:safe_write_file(CurrentHtmlFile, CurrentIoList),
    lux_html_utils:safe_write_file(RelHtmlFile, OverviewIoList).

args(RelHtmlFile, Opts) ->
    HostArg =
        case lux_utils:pick_opt(hostname, Opts, undefined) of
            undefined ->
                [];
            HostName ->
                [" --hostname=", HostName, " "]
        end,
    Files =
        case lux_utils:pick_opt(top, Opts, undefined) of
            undefined ->
                [];
            Sources ->
                [S#source.orig || S <- Sources]
        end,
    Dir = filename:dirname(RelHtmlFile),
    ["\n<h3>Invoke:</h3>\n",
     "lux ", HostArg, "--history ", Dir, " ", string:join(Files, " "), "\n",
     "\n"
    ].

errors(Errors) ->
    Fun = fun({error, File, Reason}) ->
                  [
                   "  <tr>\n",
                   "    <td>", File, "</td>\n",
                   "    <td>", Reason, "</td>\n",
                   "  </tr>\n"
                  ]
          end,
    [
     lux_html_utils:html_anchor("h3", "", "errors", "Errors:"), "\n",
     "<table border=\"0\">\n",
     lists:map(Fun, Errors),
     "</table>\n\n"
    ].

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

header(Section, AllRuns, ConfigTables, HostTables,
       HtmlDir, HtmlFile, Errors) ->
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
     lux_html_utils:html_header(["Lux history ", Section, " (", Dir, ")"]),
     "<h1>Lux history ", Section, " (", Dir, ") generated at ",
     lux_utils:now_to_string(lux_utils:timestamp()),
     "</h1>",

     "<h3>", ?i2l(N),
     " runs (",
     lux_html_utils:html_href(["#errors"], [?i2l(length(Errors)), " errors"]),
     ") within this range of repository revisions</h3>\n",
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

     legend(),

     "<h3>",
     lux_html_utils:html_href([lux_utils:drop_prefix(HtmlDir, HtmlFile),
                               "#content"],
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

legend() ->
    [
     "<h3>Legend</h3>\n",
     "  <table border=\"1\">\n",
     "    <tr>\n",
     td("First fail", fail, "left", ""),
     td("Secondary fails on same host", secondary_fail, "left",""),
     td("Warning", warning, "left", ""),
     td("Skipped", none, "left", ""),
     td("Success", success, "left", ""),
     td("No data", no_data, "left", ""),
     "    </tr>\n",
     "  </table>\n"
    ].

table_latest(NewLogDir, LatestRuns, HtmlFile) ->
    T = table(NewLogDir, "Latest", "All test suites",
              LatestRuns, HtmlFile, none, worst),
    [
     lux_html_utils:html_anchor("h3", "", "content",
                                "Latest run on each host"),
     "\n",
     T#table.iolist
    ].

table_all(NewLogDir, AllRuns, HtmlFile) ->
    T = table(NewLogDir, "All", "All test suites",
              AllRuns, HtmlFile, none, latest),
    [
     lux_html_utils:html_anchor("h3", "", "all_runs", "All runs"),
     T#table.iolist
    ].

table_current(NewLogDir, AllRuns, HtmlFile) ->
    Rebase =
        fun(#run{log=SL}, #run{log=EL})
              when SL =/= <<"unknown">>,
                   EL =/= <<"unknown">> ->
                case lux_utils:is_url(SL) of
                    true ->
                        SLD = filename:dirname(SL),
                        lux_utils:join(SLD, EL);
                    false ->
                        EL
                end;
           (#run{log=_SL}, #run{log=EL}) ->
                EL
        end,
    Details = [D#run{details=[D],
                     log = Rebase(R, D)} || R <- AllRuns,
                                            D <- R#run.details],
    T = table(NewLogDir, "All", "Still failing test cases",
              Details, HtmlFile, latest_success, latest),
    [
     lux_html_utils:html_anchor("h3", "", "content",
                                "Still failing test cases"),
     "\n",
     T#table.iolist
    ].

table_configs(NewLogDir, SplitConfigs, HtmlFile) ->
    [
     double_table(NewLogDir,
                  ConfigName,
                  "Config: " ++ ConfigName,
                  Runs,
                  HtmlFile,
                  latest) ||
        {ConfigName, Runs} <- SplitConfigs
    ].

table_hosts(NewLogDir, SplitHosts, HtmlFile) ->
    [
     double_table(NewLogDir,
                  Host,
                  ["Host: ", Host,
                   " (", (hd(Runs))#run.config_name, ")"],
                  Runs,
                  HtmlFile,
                  latest) ||
        {Host, Runs} <- SplitHosts
    ].

double_table(NewLogDir, Name, Label, AllRuns, HtmlFile, Select) ->
    Details = [D#run{details=[D]} || R <- AllRuns, D <- R#run.details],
    AllT = table(NewLogDir, Name, "All test suites",
                 AllRuns, HtmlFile, none, Select),
    FailedT = table(NewLogDir, Name, "Failed test cases",
                    Details, HtmlFile, any_success, Select),
    #table{name=Name,
           res=AllT#table.res,
           iolist=
               [
                "\n",
                ["<h3>", lux_html_utils:html_anchor(Name, Label), "</h3>\n"],
                AllT#table.iolist,
                "\n<br/>\n",
                FailedT#table.iolist
               ]}.

%% Suppress :: latest_success | any_success | none
%% Select   :: worst | latest
table(NewLogDir, Name, Grain, Runs, HtmlFile, Suppress, Select) ->
    SplitTests0 = keysplit(#run.test, Runs, fun compare_run/2),
    SplitTests = lists:keysort(1, SplitTests0),
    SplitIds = keysplit(#run.id, Runs, fun compare_run/2),
    SplitIds2 = lists:sort(fun compare_split/2, SplitIds),
    Rows =
        [
         row(NewLogDir, Test, TestRuns, SplitIds2, HtmlFile,
             Select, Suppress)
         || {Test, TestRuns} <- SplitTests
        ],
    PickRes = fun(#row{res=R}, Acc) -> lux_utils:summary(Acc, R) end,
    SelectedRes = lists:foldl(PickRes, no_data, Rows),
    #table{name=Name,
           res=SelectedRes,
           %% rows=Rows,
           iolist=
               [
                "  <table border=\"1\">\n",
                "    <tr>\n",
                table_td(Grain, SelectedRes, "left"),
                lists:map(fun run_info/1, SplitIds2),
                "    </tr>\n",
                "    <tr>\n",
                element(1, lists:mapfoldl(fun host_info/2, HtmlFile,SplitIds2)),
                "    </tr>\n",
                "    <tr>\n",
                element(1, lists:mapfoldl(fun run_cnt/2, {1,Rows}, SplitIds2)),
                "    </tr>\n",
                [R#row.iolist || R <- Rows],
                "  </table>\n"
               ]
          }.


run_info({Id, [#run{start_time=Time, repos_rev=Rev} | _]}) ->
    [
     "      <td>",
     Rev, "<br/>",
     "<strong>", Id, "</strong>", "<br/>",
     Time,
         "</td>\n"
    ].

host_info({_, [#run{hostname=Host, config_name=CN} | _]}, HtmlFile) ->
    Html =
        [
         "      <td>",
         "<strong>",
         html_suffix_href(HtmlFile, "", "#"++Host, Host, ?HOST_SUFFIX),
         "</strong>",
         "<br/>",
         html_suffix_href(HtmlFile, "", "#" ++ CN, CN, ?CONFIG_SUFFIX),
         "</td>\n"
        ],
    {Html, HtmlFile}.

run_cnt({_, _}, {N, Rows} ) ->
    Sum = fun(Pos) ->
                  lists:sum([element(Pos,lists:nth(N,Cells)) ||
                                #row{cells=Cells} <- Rows])
          end,
    FailN = Sum(#cell.n_fail),
    RunN = Sum(#cell.n_run),
    Html =
        [
         "      <td \" align=\"right\">",
         "<strong>", ?i2l(FailN), " (", ?i2l(RunN), ")</strong>\n",
         "</td>\n"
        ],
    {Html, {N+1,Rows}}.

row(NewLogDir, Test, Runs, SplitIds,
    HtmlFile, Select, Suppress) ->
    RevRuns = lists:reverse(lists:keysort(#run.id, Runs)),
    EmitCell =
        fun({Id, _R}, AccRes) ->
                cell(NewLogDir, Test, Id, RevRuns, HtmlFile, AccRes)
        end,
    {Cells, _} = lists:mapfoldr(EmitCell, [], SplitIds),
    ValidResFilter = fun (Cell) -> valid_res_filter(Cell, Suppress) end,
    ValidRes = lists:zf(ValidResFilter, Cells),
    SiblingFilter =
        fun(R) ->
                case R of
                    warning -> success;
                    none    -> no_data;
                    _       -> R
                end
        end,
    case lists:usort(lists:map(SiblingFilter, ValidRes)) of
        [] when Suppress =:= any_success ->
            #row{res=no_data, cells=Cells, iolist=[]}; % Skip row
        [success] when Suppress =:= any_success ->
            #row{res=no_data, cells=Cells, iolist=[]}; % Skip row
        [no_data] when Suppress =:= any_success ->
            #row{res=no_data, cells=Cells, iolist=[]}; % Skip row
        _X ->
            SelectedRes = select_row_res(Cells, Select, no_data),
            if
                Suppress =:= latest_success andalso
                (SelectedRes =:= success orelse
                 SelectedRes =:= warning orelse
                 SelectedRes =:= skip orelse
                 SelectedRes =:= none orelse
                 SelectedRes =:= no_data) ->
                    #row{res=no_data, cells = Cells, iolist=[]}; % Skip row
                true ->
                    #row{res=SelectedRes,
                         cells=Cells,
                         iolist=
                             [
                              "    <tr>\n",
                              td(Test, SelectedRes, "left", ""),
                              [C#cell.iolist || C <- Cells],
                              "    </tr>\n"
                             ]
                        }
            end
    end.

valid_res_filter(#cell{res=Res}, Suppress) ->
    case Res of
        no_data                               -> false;
        none    when Suppress =:= any_success -> false;
        success when Suppress =:= any_success -> false;
        warning when Suppress =:= any_success -> false;
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
    %% Pick the worst of all cells with same revision
    PickSameRev = fun(#cell{run=#run{repos_rev=R}}) -> R =:= Rev;
                     (#cell{res=_Res})              -> true
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

cell(NewLogDir, Test, Id, Runs, _HtmlFile, AccRes) ->
    case lists:keyfind(Id, #run.id, Runs) of
        false ->
            Td = td("-", no_data, "right", Test),
            {#cell{res=no_data, run=undefined,
                   n_run=0, n_fail=0,
                   iolist=Td}, AccRes};
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
                    OldLog ->
                        AbsLog = lux_utils:join(Run#run.run_log_dir, OldLog),
                        NewLog = lux_utils:drop_prefix(NewLogDir, AbsLog),
                        lux_html_utils:html_href([NewLog, ".html"], FailCount)
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
            Td = td(Text, Res, "right", ToolTip),
            {#cell{res=Res, run=Run,
                   n_run=RunN, n_fail=FailN,
                   iolist=Td}, AccRes2}
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

table_td(Text, skip, Align) ->
    table_td(Text, none, Align);
table_td(Text, Res, Align) ->
    [
     "      ",
     "<td class=\"", atom_to_list(Res), "\" align=\"", Align,
     "\" rowspan=\"3\">",
     "<strong>", Text, "</strong>",
     "</td>\n"
    ].

td(Text, skip, Align, Title) ->
    td(Text, none, Align, Title);
td(Text, Res, Align, Title) ->
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

html_suffix_href(HtmlFile, Protocol, Name, Label, Suffix)
  when is_list(HtmlFile) ->
    Name2 = insert_html_suffix(HtmlFile, Name, Suffix),
    lux_html_utils:html_href(Protocol, Name2, Label).

insert_html_suffix(HtmlFile, Name, Suffix)
  when is_list(HtmlFile), is_list(Name), is_list(Suffix) ->
    Ext = filename:extension(HtmlFile),
    BaseName = filename:basename(HtmlFile, Ext),
    BaseName ++ Suffix ++ Ext ++ Name.

parse_summary_logs(Source, RelHtmlFile, Acc, Err, Opts) ->
    RelFile = Source#source.file,
    RelDir = "",
    case {filename:basename(RelFile), filename:basename(RelHtmlFile)} of
        {HistoryFile, HistoryFile} ->
            %% Use history file as source
            case lux_html_parse:parse_files(shallow, RelFile) of
                [{ok, _, Links, html}] ->
                    SL = "lux_summary.log",
                    HL = SL ++ ".html",
                    Extract =
                        fun({link, Link, _Label}) ->
                                case lists:suffix(HL, Link) of
                                    true ->
                                        TopDir = filename:dirname(RelFile),
                                        LinkDir = filename:dirname(Link),
                                        Tmp = lux_utils:join(TopDir, LinkDir),
                                        File = lux_utils:join(Tmp, SL),
                                        {true, File};
                                    _ ->
                                        false
                                end;
                           (_Skip) ->
                                false
                        end,
                    Files = lists:zf(Extract, Links),
                    %% io:format("\nLINKS ~p\n", [Files]),
                    parse_summary_files(Source, RelDir, Files, Acc, Err, Opts);
                Errors ->
                    Strings = lux_html_parse:format_results(Errors),
                    Format = fun(E) ->
                                     io:format("\n\t\t~s\n", [E]),
                                     {error, RelFile, E}
                             end,
                    {Acc, lists:map(Format, Strings)}
            end;
        _ ->
            search_summary_dirs(Source, RelDir, Acc, Err, Opts)
    end.

search_summary_dirs(Source, RelDir, Acc, Err, Opts) ->
    RelFile = Source#source.file,
    Dir0 = lux_utils:join(RelFile, RelDir),
    Dir = lux_utils:normalize_filename(Dir0),
    case file:list_dir(Dir) of
        {ok, Files} ->
            Cands =
                ["lux.skip",
                 "lux_summary.log",
                 "lux_summary.log.tmp",
                 "qmscript.skip",
                 "qmscript_summary.log",
                 "qmscript_summary.log.tmp",
                 "qmscript.summary.log"],
            case multi_member(Cands, Files) of
                {true, Base} ->
                    case lists:suffix(".log", Base) of
                        true ->
                            %% A summary log
                            parse_summary_files(Source, RelDir, [Base],
                                                Acc, Err, Opts);
                        false ->
                            %% Skip
                            io:format("s", []),
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
                                        "" ->
                                            File;
                                        _  ->
                                            lux_utils:join(RelDir, File)
                                    end,
                                search_summary_dirs(Source, RelDir2,
                                                    A, E, Opts)
                        end,
                    lists:foldl(Fun, {Acc, Err}, Files)
            end;
        {error, _Reason} ->
            %% Not a dir or problem to read dir
            {Acc, Err}
    end.

parse_summary_files(Source, RelDir, [Base | Bases], Acc, Err, Opts) ->
    io:format(".", []),
    RelFile = Source#source.file,
    Tmp = lux_utils:join(RelFile, RelDir),
    File = lux_utils:join(Tmp, Base),
    {Acc2, Err2} =
        case lux_log:parse_summary_log(File) of
            {ok,_,_,_,_,_} = Res->
                case lux_log:parse_run_summary(Source,
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
        end,
    parse_summary_files(Source, RelDir, Bases, Acc2, Err2, Opts);
parse_summary_files(_Source, _RelDir, [], Acc, Err, _Opts) ->
    {Acc, Err}.

multi_member([H | T], Files) ->
    case lists:member(H, Files) of
        true  -> {true, H};
        false -> multi_member(T, Files)
    end;
multi_member([], _Files) ->
    false.

%% Keysort list of tuples and group items with same tag
%%
%% Items are by default returned in reverse order:
%%
%%   keysplit(1, [{3,3},{3,1},{3,2},{1,1},{1,2},{2,2},{2,1},{1,3}]).
%%   -> [{1,[{1,3},{1,2},{1,1}]},
%%       {2,[{2,1},{2,2}]},
%%       {3,[{3,2},{3,1},{3,3}]}]

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
