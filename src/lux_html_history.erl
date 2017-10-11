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
    case lists:usort([B || #source{branch=B} <- Sources]) of
        %% [_NoBranch=undefined] -> LatestOnly = false;
        [_SingleBranch]       -> LatestOnly = false;
        _MultiBranch          -> LatestOnly = true
    end,
    Opts2 = [{top, Sources} | Opts],
    {Runs, Errors, WWW} = collect(Sources, Opts2, [], [], undefined),
    io:format("\nAnalyzed ~p test runs (~p errors)",
              [length(Runs), length(Errors)]),
    %% io:format("\nERRORS ~p\n", [Errors]),
    Res = do_generate(RelHtmlFile, Runs, Errors, LatestOnly, Opts),
    lux_utils:stop_app(WWW),
    Res.

split_source(OrigSource) ->
    case lux_utils:split(OrigSource, ":::") of
        {Branch, PrefixedSource} ->
            ok;
        false ->
            Branch = undefined,
            PrefixedSource = OrigSource
    end,
    case lux_utils:split(PrefixedSource, "::") of
        {SuitePrefix, File} ->
            ok;
        false ->
            SuitePrefix = undefined,
            File = PrefixedSource
    end,
    #source{branch=Branch,
            suite_prefix=SuitePrefix,
            file=File,
            dir=source_dir(File),
            orig=OrigSource}.

source_dir(File) ->
    case filename:basename(File) of
        "lux_history.html" -> filename:dirname(File);
        "lux_summary.log"  -> filename:dirname(File);
        _                  -> File % Assume file is dir
    end.

collect([Source | Sources], Opts, Runs, Errors, WWW) ->
    io:format("\n\t~s", [Source#source.orig]),
    {{NewRuns, NewErrors}, NewWWW} =
        parse_summary_logs(Source, Runs, Errors, WWW, Opts),
    collect(Sources, Opts,
              NewRuns, NewErrors, NewWWW);
collect([], _Opts, Runs, Errors, WWW) ->
    {Runs, Errors, WWW}.

do_generate(RelHtmlFile, AllRuns, Errors, LatestOnly=true, Opts) ->
    AbsHtmlFile = lux_utils:normalize_filename(RelHtmlFile),
    HistoryLogDir = filename:dirname(AbsHtmlFile),
    SplitBranches = keysplit(#run.branch, AllRuns),
    LatestRuns = latest_runs(SplitBranches),
    ConfigTables = [],
    HostTables = [],
    HtmlDir = filename:dirname(RelHtmlFile),
    HtmlArgs = args(RelHtmlFile, Opts),
    HtmlErrors = errors(Errors),

    %% Headers
    OverviewHeader =
        ?l2b(header("overview", AllRuns,
                    ConfigTables, HostTables, HtmlDir,
                    RelHtmlFile, Errors, LatestOnly)),

    %% Overview
    OverviewIoList =
        [
         OverviewHeader,
         table_latest(HistoryLogDir, LatestRuns, AbsHtmlFile,
                      "Latest run on each branch"),
         HtmlArgs,
         HtmlErrors,
         lux_html_utils:html_footer()
        ],
    lux_html_utils:safe_write_file(RelHtmlFile, OverviewIoList);
do_generate(RelHtmlFile, AllRuns, Errors, LatestOnly=false, Opts) ->
    AbsHtmlFile = lux_utils:normalize_filename(RelHtmlFile),
    HistoryLogDir = filename:dirname(AbsHtmlFile),
    SplitHosts = keysplit(#run.hostname, AllRuns),
    LatestRuns = latest_runs(SplitHosts),
    HostTables =
        case SplitHosts of
            [_] ->
                [];
            _ ->
                table_hosts(HistoryLogDir, SplitHosts, AbsHtmlFile)
        end,
    SplitConfigs = keysplit(#run.config_name, AllRuns, fun compare_run/2),
    ConfigTables =
        case SplitConfigs of
            [_] ->
                [];
            _ ->
                table_configs(HistoryLogDir, SplitConfigs, AbsHtmlFile)
        end,
    HtmlDir = filename:dirname(RelHtmlFile),
    HtmlArgs = args(RelHtmlFile, Opts),
    HtmlErrors = errors(Errors),
    erlang:garbage_collect(),

    %% Headers
    HostHeader =
        ?l2b(header("host", AllRuns,
                    ConfigTables, HostTables, HtmlDir,
                    RelHtmlFile, Errors, LatestOnly)),
    ConfigHeader =
        ?l2b(header("config", AllRuns,
                    ConfigTables, HostTables, HtmlDir,
                    RelHtmlFile, Errors, LatestOnly)),
    CurrentHeader =
        ?l2b(header("current failures", AllRuns,
                    ConfigTables, HostTables, HtmlDir,
                    RelHtmlFile, Errors, LatestOnly)),
    OverviewHeader =
        ?l2b(header("overview", AllRuns,
                    ConfigTables, HostTables, HtmlDir,
                    RelHtmlFile, Errors, LatestOnly)),

    %% Host
    HostIoList =
        [
         HostHeader,
         "<a name=\"content\"/>",
         [T#table.iolist || T <- HostTables],
         HtmlArgs,
         HtmlErrors,
         lux_html_utils:html_footer()
        ],
    HostHtmlFile =
        lux_utils:join(HtmlDir,
                       insert_html_suffix(RelHtmlFile, "", ?HOST_SUFFIX)),
    lux_html_utils:safe_write_file(HostHtmlFile, HostIoList),

    %% Config
    ConfigIoList =
        [
         ConfigHeader,
         "<a name=\"content\"/>",
         [T#table.iolist || T <- ConfigTables],
         HtmlArgs,
         HtmlErrors,
         lux_html_utils:html_footer()
        ],
    ConfigHtmlFile =
        lux_utils:join(HtmlDir,
                       insert_html_suffix(RelHtmlFile, "", ?CONFIG_SUFFIX)),
    lux_html_utils:safe_write_file(ConfigHtmlFile, ConfigIoList),
    erlang:garbage_collect(),

    %% Current
    CurrentIoList =
        [
         CurrentHeader,
         table_current(HistoryLogDir, AllRuns, AbsHtmlFile),
         HtmlArgs,
         HtmlErrors,
         lux_html_utils:html_footer()
        ],
    CurrentHtmlFile =
        lux_utils:join(HtmlDir,
                       insert_html_suffix(RelHtmlFile, "", ?CURRENT_SUFFIX)),
    lux_html_utils:safe_write_file(CurrentHtmlFile, CurrentIoList),

    %% Overview
    LatestSlogan =
        case SplitHosts of
            [_] -> "Latest run";
            _   -> "Latest run on each host"
        end,
    OverviewIoList =
        [
         OverviewHeader,
         table_latest(HistoryLogDir, LatestRuns, AbsHtmlFile, LatestSlogan),
         table_all(HistoryLogDir, AllRuns, AbsHtmlFile),
         HtmlArgs,
         HtmlErrors,
         lux_html_utils:html_footer()
        ],
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

latest_runs(SplitRuns) ->
    SplitRunTests =
        [{Tag, keysplit(#run.test, TaggedRuns, fun compare_run/2)} ||
            {Tag, TaggedRuns} <- SplitRuns],
    DeepIds =
        [(hd(TestRuns))#run.id ||
            {_Tag, TaggedTests} <-SplitRunTests,
            {_SubTag, TestRuns} <- TaggedTests],
    Ids = lists:usort(lists:flatten(DeepIds)),
    [Run ||
        {_Tag, TaggedTests} <-SplitRunTests,
        {_SubTag, TestRuns} <- TaggedTests,
        Run <- TestRuns,
        lists:member(Run#run.id, Ids)].

header(Section, AllRuns, ConfigTables, HostTables,
       HtmlDir, HtmlFile, Errors, LatestOnly) ->
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

     [
      "<h3>",
      case LatestOnly of
          true ->
              "Multiple branches. No failed test cases page generated.";
          false ->
              html_suffix_href(HtmlFile,"","#content",
                               "Still failing test cases",
                               ?CURRENT_SUFFIX)
      end,
      "</h3>\n\n"
     ],
     if
         ConfigTables =:= [] ->
             "<h3>Only one config. No config page generated.</h3>\n";
         true ->
             [
              "<h3>",
              html_suffix_href(HtmlFile,"","#content",
                               "Configurations", ?CONFIG_SUFFIX),
              "</h3>\n",
              "  <table border=\"1\">\n",
              "    <tr>\n",
              [
               html_suffix_href_td(HtmlFile, ConfigName,
                                   ConfigRes, ?CONFIG_SUFFIX) ||
                  #table{name=ConfigName, res=ConfigRes} <- ConfigTables
              ],
              "    </tr>\n",
              "  </table>\n"
             ]
     end,
     if
         HostTables =:= [] ->
             "<h3>Only one host. No host page generated.</h3>\n";
         true ->
             [
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
             ]
     end
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

table_latest(HistoryLogDir, LatestRuns, HtmlFile, Slogan) ->
    T = table(HistoryLogDir, "Latest", "All test suites",
              LatestRuns, HtmlFile, none, worst),
    [
     lux_html_utils:html_anchor("h3", "", "content", Slogan),
     "\n",
     T#table.iolist
    ].

table_all(HistoryLogDir, AllRuns, HtmlFile) ->
    T = table(HistoryLogDir, "All", "All test suites",
              AllRuns, HtmlFile, none, latest),
    [
     lux_html_utils:html_anchor("h3", "", "all_runs", "All runs"),
     T#table.iolist
    ].

table_current(HistoryLogDir, AllRuns, HtmlFile) ->
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
    T = table(HistoryLogDir, "All", "Still failing test cases",
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

table_hosts(HistoryLogDir, SplitHosts, HtmlFile) ->
    [
     double_table(HistoryLogDir,
                  Host,
                  ["Host: ", Host,
                   " (", (hd(Runs))#run.config_name, ")"],
                  Runs,
                  HtmlFile,
                  latest) ||
        {Host, Runs} <- SplitHosts
    ].

double_table(HistoryLogDir, Name, Label, AllRuns, HtmlFile, Select) ->
    AllT = table(HistoryLogDir, Name, "All test suites",
                 AllRuns, HtmlFile, none, Select),
    Details = [D#run{details=[D]} || R <- AllRuns,
                                     D <- R#run.details],
    FailedT = table(HistoryLogDir, Name, "Failed test cases",
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
table(HistoryLogDir, Name, Grain, Runs, HtmlFile, Suppress, Select) ->
    SplitTests0 = keysplit(#run.test, Runs, fun compare_run/2),
    SplitTests = lists:keysort(1, SplitTests0),
    SplitIds = keysplit(#run.id, Runs, fun compare_run/2),
    SplitIds2 = lists:sort(fun compare_split/2, SplitIds),
    Rows =
        [
         row(HistoryLogDir, Test, TestRuns, SplitIds2, HtmlFile,
             Select, Suppress)
         || {Test, TestRuns} <- SplitTests
        ],
    PickRes = fun(#row{res=R}, Acc) -> lux_utils:summary(Acc, R) end,
    SelectedRes = lists:foldl(PickRes, no_data, Rows),
    #table{name=Name,
           res=SelectedRes,
           %% rows=Rows,
           iolist=
               ?l2b([
                     "  <table border=\"1\">\n",
                     "    <tr>\n",
                     table_td(Grain, SelectedRes, "left"),
                     lists:map(fun run_info/1, SplitIds2),
                     "    </tr>\n",
                     "    <tr>\n",
                     element(1, lists:mapfoldl(fun host_info/2,
                                               HtmlFile,
                                               SplitIds2)),
                     "    </tr>\n",
                     "    <tr>\n",
                     element(1, lists:mapfoldl(fun run_cnt/2,
                                               {1,Rows},
                                               SplitIds2)),
                     "    </tr>\n",
                     [R#row.iolist || R <- Rows],
                     "  </table>\n"
                    ])
          }.


run_info({Id, [#run{start_time=Time, branch=RunBranch, repos_rev=Rev} | _]}) ->
    OptBranch =
        case RunBranch of
            undefined -> "";
            ""        -> "";
            Branch   -> ["<br/>", Branch]
        end,
    [
     "      <td>",
     Rev, "<br/>",
     "<strong>", Id, "</strong>", "<br/>",
     Time,
     OptBranch,
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
         "      <td align=\"right\">",
         "<strong>", ?i2l(FailN), " (", ?i2l(RunN), ")</strong>\n",
         "</td>\n"
        ],
    {Html, {N+1,Rows}}.

row(HistoryLogDir, Test, Runs, SplitIds, HtmlFile, Select, Suppress) ->
    RevRuns = lists:reverse(lists:keysort(#run.id, Runs)),
    EmitCell =
        fun({Id, _R}, AccRes) ->
                cell(HistoryLogDir, Test, Id, RevRuns, HtmlFile, AccRes)
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

cell(HistoryLogDir, Test, Id, Runs, _HtmlFile, AccRes) ->
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
                        NewLog =
                            case lux_utils:is_url(HistoryLogDir) of
                                true ->
                                    lux_utils:join(HistoryLogDir, OldLog);
                                false ->
                                    NewLogDir = Run#run.new_log_dir,
                                    TmpLog = lux_utils:join(NewLogDir, OldLog),
                                    lux_utils:drop_prefix(HistoryLogDir, TmpLog)
                            end,
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
            OptBranch =
                case Run#run.branch of
                    undefined -> "";
                    ""        -> "";
                    Branch   -> ["\n", Branch]
            end,
            ToolTip = [Test, "\n",
                       Run#run.config_name,"\n",
                       Run#run.hostname,"\n",
                       Run#run.start_time,"\n",
                       Run#run.id,"\n",
                       Run#run.repos_rev,
                       OptBranch],
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

parse_summary_logs(Source, Acc, Err, WWW, Opts) ->
    RelFile = Source#source.file,
    Base = filename:basename(RelFile),
    RelDir = "",
    if
        Base =:= "lux_history.html" ->
            %% Use history file as source
            {ParseRes, NewWWW} =
                lux_html_parse:parse_files(shallow, RelFile, WWW),
            case ParseRes of
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
                    parse_summary_files(Source, RelDir, Files,
                                        Acc, Err, NewWWW, Opts);
                Errors ->
                    Strings = lux_html_parse:format_results(Errors, NewWWW),
                    Format = fun(E) ->
                                     io:format("\n\t\t~s\n", [E]),
                                     {error, RelFile, E}
                             end,
                    {{Acc, lists:map(Format, Strings)}, NewWWW}
            end;
        Base =:= "lux_summary.log" ->
            parse_summary_files(Source, RelDir, [Base],
                                Acc, Err, WWW, Opts);
        true ->
            search_summary_dirs(Source, RelDir, Acc, Err, WWW, Opts)
    end.

search_summary_dirs(Source, RelDir, Acc, Err, WWW, Opts) ->
    RelFile = Source#source.file,
    Dir0 = lux_utils:join(RelFile, RelDir),
    Dir = lux_utils:normalize_filename(Dir0),
    case file:list_dir(Dir) of
        {ok, Files} ->
            Cands = candidate_files(),
            case multi_member(Cands, Files) of
                {true, Base} ->
                    case lists:suffix(".log", Base) of
                        true ->
                            %% A summary log
                            parse_summary_files(Source, RelDir, [Base],
                                                Acc, Err, WWW, Opts);
                        false ->
                            %% Skip
                            io:format("s", []),
                            {{Acc, Err}, WWW}
                    end;
                false ->
                    %% No interesting file found. Search subdirs
                    Fun =
                        fun("latest_run", {{A,E},W}) ->
                                %% Symlink
                                {{A,E}, W};
                           (File, {{A,E},W}) ->
                                RelDir2 =
                                    case RelDir of
                                        "" ->
                                            File;
                                        _  ->
                                            lux_utils:join(RelDir, File)
                                    end,
                                search_summary_dirs(Source, RelDir2,
                                                    A, E, W, Opts)
                        end,
                    lists:foldl(Fun, {{Acc, Err}, WWW}, Files)
            end;
        {error, _Reason} ->
            %% Not a dir or problem to read dir
            {{Acc, Err}, WWW}
    end.

candidate_files() ->
    [
     "lux.skip",
     "lux_summary.log",
     "lux_summary.log.tmp",
     "qmscript.skip",
     "qmscript_summary.log",
     "qmscript_summary.log.tmp",
     "qmscript.summary.log"
    ].

parse_summary_files(Source, RelDir, [Base | Bases], Acc, Err, WWW, Opts) ->
    io:format(".", []),
    RelFile = Source#source.file,
    case filename:basename(RelFile) of
        "lux_summary.log" ->
            File = RelFile;
        _ ->
            Tmp = lux_utils:join(RelFile, RelDir),
            File = lux_utils:join(Tmp, Base)
    end,
    {ParseRes, NewWWW} = lux_log:parse_summary_log(File, WWW),
    {Acc2, Err2} =
        case ParseRes of
            {ok,_,_,_,_,_} = Res->
                case lux_log:parse_run_summary(Source, File, Res, Opts) of
                    {error, F, Reason} ->
                        {Acc, [{error, F, Reason} | Err]};
                    #run{} = R ->
                        {[R|Acc], Err}
                end;
            {error, F, Reason} ->
                {Acc, [{error, F, Reason} | Err]}
        end,
    parse_summary_files(Source, RelDir, Bases, Acc2, Err2, NewWWW, Opts);
parse_summary_files(_Source, _RelDir, [], Acc, Err, WWW, _Opts) ->
    {{Acc, Err}, WWW}.

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
