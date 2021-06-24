%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2021 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_html_history).

-export([generate/3, keysplit/3]).
-import(lux_html_utils, [html_table_td/3, html_td/4]).

-include("lux.hrl").

-record(cache, {threshold, runs, errors, opts}).
-record(table, {res, rows, iolist, split_tests}).
-record(row,   {res, test, cells, iolist}).
-record(cell,  {res, run, n_run, n_fail, iolist}).

-record(page,
        {title    :: iolist(),
         file     :: iolist(),
         tags     :: [overview | {all, host} | {all, config} |
                      {host, iolist()} | {config, iolist}],
         suppress :: suppress(),
         select   :: select(),
         runs     :: [#run{}]}).

-record(hwarning,
        {type   :: binary(),
         reason :: binary(),
         lineno :: lineno(),
         run    :: #run{}}).

generate(PrefixedSources, RelHtmlDir, Opts0) ->
    io:format("Invoke: ~s\n",
              [string:join(init:get_plain_arguments(), " ")]),
    io:format("Assembling history of logs from...", []),
    %% TIME: io:format("\nASSEMBLE ~p\n", [time()]),
    Sources = lists:map(fun split_source/1, PrefixedSources),
    SplitSources = keysplit(#source.branch, Sources),
    Opts = [{top, Sources} | Opts0],
    CacheDir = RelHtmlDir,
    CacheFile = filename:join([CacheDir, ?HISTORY_LOG_BASE ++ ".cache"]),
    case read_cache(CacheFile, Opts) of
        {ok, OldThreshold, OldRuns, OldErrors} ->
            ValidRuns = validate_runs(OldRuns),
            OldWWW = undefined,
            {Threshold, Runs, Errors, NewWWW} =
                collect(SplitSources, RelHtmlDir,
                        OldThreshold, OldThreshold,
                        ValidRuns, OldErrors, OldWWW, Opts),
            case NewWWW of
                {N, StopFun} -> lux_utils:stop_app(StopFun);
                undefined    -> N = 0;
                false        -> N = 0
            end,
            if
                N =:= 0 ->
                    %% No remote logs
                    write_cache(CacheFile, Threshold, Runs, Errors, Opts);
                true ->
                    file:rename(CacheFile, CacheFile ++ ".tmp"),
                    io:format("\n<WARNING> No caching of remote logs\n", [])
            end,

            Cases = lists:flatten([CaseRuns || #run{runs = CaseRuns} <- Runs]),
            io:format("\nAnalyzed ~p test runs with ~p test cases (~p errors)",
                      [length(Runs), length(Cases),length(Errors)]),
            %% TIME: io:format("\nDONE ~p\n", [time()]),
            do_generate(?l2b(RelHtmlDir), Runs, Errors, Opts);
        {error, ErrFile, ErrReason} ->
            {error, ErrFile, ErrReason}
    end.

read_cache(CacheFile, Opts) ->
    case file:read_file(CacheFile) of
        {ok, CacheBin} ->
            io:format("\n\t~s (~p bytes)\n", [CacheFile, byte_size(CacheBin)]),
            case erlang:binary_to_term(CacheBin) of
                #cache{threshold = CacheThreshold,
                       runs = CacheRuns,
                       errors = CacheErrors,
                       opts = CacheOpts} ->
                    if
                        CacheOpts =/= Opts ->
                            io:format("\n<WARNING> Cache file is incompatible"
                                      " with previous run ~s: ignoring cache\n",
                                      [CacheFile]),
                            {ok, 0, [], []};
                        true ->
                            {ok, CacheThreshold, CacheRuns, CacheErrors}
                    end;
                _ ->
                    io:format("\n<WARNING> Illegal cache file "
                              "format ~s: ignoring cache\n",
                              [CacheFile]),
                    {ok, 0, [], []}
            end;
        {error, enoent} ->
            {ok, 0, [], []};
        {error, FileReason} ->
            io:format("\n<WARNING> Failed to read run cache file ~s: ~p\n",
                      [CacheFile, file:format_error(FileReason)]),
            {ok, 0, [], []}
    end.

write_cache(CacheFile, Threshold, Runs, Errors, Opts) ->
    Cache = #cache{threshold = Threshold,
                   runs = Runs,
                   errors = Errors,
                   opts = Opts},
    CacheBin = erlang:term_to_binary(Cache, [compressed]),
    case lux_html_utils:safe_write_file(CacheFile, CacheBin) of
        {ok, CacheFile} ->
            io:format("\nWrote ~p bytes in run cache to file ~s\n",
                      [byte_size(CacheBin), CacheFile]);
        {error, FileReason} ->
            io:format("\n<WARNING> Failed to write run cache to file ~s: ~s\n",
                      [CacheFile, file:format_error(FileReason)])
    end.

split_source(OrigSourceStr) when is_list(OrigSourceStr) ->
    OrigSource = ?l2b(OrigSourceStr),
    case lux_utils:split(OrigSource, <<":::">>) of
        {Branch, PrefixedSource} ->
            ok;
        false ->
            Branch = undefined,
            PrefixedSource = OrigSource
    end,
    case lux_utils:split(PrefixedSource, <<"::">>) of
        {SuitePrefix, File} ->
            ok;
        false ->
            SuitePrefix = undefined,
            File = PrefixedSource
    end,
    #source{branch=Branch,
            suite_prefix = SuitePrefix,
            file = File,
            dir = source_dir(File),
            orig = OrigSource}.

source_dir(FileBin) when is_binary(FileBin) ->
    FileStr = ?b2l(FileBin),
    case filename:basename(FileStr) of
        ?HISTORY_LOG_BASE ++ ?HTML_EXT -> filename:dirname(FileBin);
        ?SUITE_SUMMARY_LOG             -> filename:dirname(FileBin);
        _                              -> FileBin % Assume file is dir
    end.

validate_runs(Runs) ->
    {ok, Cwd} = file:get_cwd(),
    validate_runs(Runs, Cwd, []).

validate_runs([Run | Runs], Cwd, ValidRuns) ->
    LogDir = filename:join(Cwd, Run#run.new_log_dir),
    case filelib:is_dir(LogDir) of
        true ->
            validate_runs(Runs, Cwd, [Run | ValidRuns]);
        false ->
            io:format("-", []),
            validate_runs(Runs, Cwd, ValidRuns)
    end;
validate_runs([], _Cwd, ValidRuns) ->
    lists:reverse(ValidRuns).

collect([{undefined, Sources} | SplitSources], RelHtmlDir,
        Threshold, Newest, Runs, Errors, WWW, Opts) ->
    collect([{<<"no_branch">>, Sources} | SplitSources], RelHtmlDir,
            Threshold, Newest,
            Runs, Errors, WWW, Opts);
collect([{Branch, Sources} | SplitSources], RelHtmlDir,
        Threshold, Newest, Runs, Errors, WWW, Opts) ->
    {NewNewest, OptRuns, OptErrors, NewWWW} =
        collect_branch(Sources, RelHtmlDir,
                       Threshold, Newest, [], Errors, WWW, Opts),
    case OptRuns of
        [] ->
            S = hd(Sources),
            SummaryLog = ?b2l(S#source.file),
            Reason = "HTML LUX WARNING: " ++ SummaryLog ++ ": No new runs",
            io:format("\n~s\n", [Reason]),
            MoreRuns =
                case Runs of
                    [] ->
                        R = lux_log:default_run(S, SummaryLog),
                        [R#run{id = Branch,
                               branch = Branch,
                               repos_rev = Branch}];
                    _ ->
                        []
                end,
            MoreErrors = [{error, SummaryLog, Reason}];
        MoreRuns ->
            MoreErrors = []
    end,
    NewRuns = MoreRuns ++ Runs,
    NewErrors = MoreErrors ++ OptErrors,
    collect(SplitSources, RelHtmlDir, Threshold, NewNewest,
            NewRuns, NewErrors, NewWWW, Opts);
collect([], _RelHtmlDir, _Threshold, Newest, Runs, Errors, WWW, _Opts) ->
    {Newest, Runs, Errors, WWW}.

collect_branch([Source | Sources], RelHtmlDir,
               Threshold, Newest,
               Runs, Errors, WWW, Opts) ->
    io:format("\n\t~s", [Source#source.orig]),
    {{NewNewest, NewRuns, NewErrors}, NewWWW} =
        parse_summary_logs(Source, RelHtmlDir,
                           Threshold, Newest, Runs, Errors, WWW, Opts),
    collect_branch(Sources, RelHtmlDir, Threshold, NewNewest,
                   NewRuns, NewErrors, NewWWW, Opts);
collect_branch([], _RelHtmlDir,
               _Threshold, Newest,
               Runs, Errors, WWW, _Opts) ->
    {Newest, Runs, Errors, WWW}.

do_generate(RelHtmlDir, AllRuns, Errors, Opts) ->
    SplitBranches = keysplit(#run.branch, AllRuns),
    MultiBranch = (length(SplitBranches) > 1),
    DeepPages = the_pages(AllRuns, SplitBranches, MultiBranch),
    {AllPages, TagDict} = adjust_pages(DeepPages),
    AllG = [gen_page(RelHtmlDir, MultiBranch, P, TagDict) || P <- AllPages],
    Footer =
        [
         args(RelHtmlDir, Opts),
         errors(Errors),
         lux_html_utils:html_footer()
        ],
    [write_page(RelHtmlDir, MultiBranch, AllRuns, CurrG, AllG,
                TagDict, Errors, Footer) ||
        CurrG <- AllG],
    RelHtmlFile = filename:join([RelHtmlDir, ?HISTORY_LOG_BASE ++ ?HTML_EXT]),
    {ok, RelHtmlFile}.

the_pages(AllRuns, SplitBranches, MultiBranch) ->
    case MultiBranch of
        true ->
            LatestBranches = latest_runs(SplitBranches),
            [
             #page{title    = "Latest run on each branch",
                   file     = "",
                   tags     = [overview, {all, host}, {all, config}],
                   suppress = suppress_none,
                   select   = select_worst,
                   runs     = LatestBranches}
            ];
        false ->
            SplitHosts = keysplit(#run.hostname, AllRuns),
            SplitConfigs =
                keysplit(#run.config_name, AllRuns, fun compare_run/2),
            [
             if
                 length(SplitHosts) > 1 ->
                     LatestHosts = latest_runs(SplitHosts),
                     [
                      #page{title    = "Latest run on each host",
                            file     = "",
                            tags     = [overview, {all, host}],
                            suppress = suppress_none,
                            select   = select_worst,
                            runs     = LatestHosts},
                      [#page{title    =  ["All runs on host ", Host,
                                          " (",(hd(Runs))#run.config_name,")"],
                             file     = ["_host_", Host],
                             tags     = [{host, Host}],
                             suppress = suppress_none,
                             select   = select_latest,
                             runs = Runs} ||
                          {Host, Runs} <- SplitHosts]
                     ];
                 true ->
                     [{Host, Runs}] = SplitHosts,
                     Config = (hd(Runs))#run.config_name,
                     [
                      #page{title   = ["All runs on host ", Host,
                                       " (", Config, ")"],
                            file    = "",
                            tags    = [overview, {all, host}, {host, Host}],
                            suppress= suppress_none,
                            select  = select_latest,
                            runs    = Runs}
                     ]
             end,
             if
                 length(SplitConfigs) > 1 ->
                     LatestConfigs = latest_runs(SplitConfigs),
                     [
                      #page{title   = "Latest run on each config",
                            file    = "_configs",
                            tags    = [{all, config}],
                            suppress= suppress_none,
                            select  = select_worst,
                            runs    = LatestConfigs},
                      [#page{title   = ["All runs on config ", Config],
                             file    = ["_config_", Config],
                             tags    = [{config, Config}],
                             suppress= suppress_none,
                             select  = select_latest,
                             runs    = Runs} ||
                          {Config, Runs} <- SplitConfigs]
                     ];
                 true ->
                     [{Config, Runs}] = SplitConfigs,
                     [
                      #page{title    = ["All runs on config ", Config],
                            file     = ["_config_", Config],
                            tags     = [{all, config}, {config, Config}],
                            suppress = suppress_none,
                            select   = select_latest,
                            runs     = Runs}
                     ]
             end
            ]
    end.

adjust_pages(DeepPages) ->
    ExpandPage =
        fun(P) ->
                File = [?HISTORY_LOG_BASE, P#page.file, ?HTML_EXT],
                P#page{title = ?l2b(P#page.title),
                       file  = ?l2b(File)}
        end,
    Pages = lists:map(ExpandPage, lists:flatten(DeepPages)),
    InsertAllTags =
        fun(Page, Dict) ->
                InsertTag =
                    fun(Tag, D) ->
                            false = dict:is_key(Tag, D), % assert
                            %% io:format("PAGE ~p\n", [Tag]),
                            dict:store(Tag, Page, D)
                    end,
                lists:foldl(InsertTag, Dict, Page#page.tags)
        end,
    TagDict = lists:foldl(InsertAllTags, dict:new(), Pages),
    {Pages, TagDict}.

target_page(Tag, TagDict) ->
    dict:fetch(Tag, TagDict).

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

args(RelHtmlDir, Opts) ->
    HostArg =
        case lux_utils:pick_opt(hostname, Opts, undefined) of
            undefined -> [];
            HostName  -> [" --hostname=", HostName, " "]
        end,
    Files =
        case lux_utils:pick_opt(top, Opts, undefined) of
            undefined -> [];
            Sources   -> string:join([?b2l(S#source.orig) || S <- Sources], " ")
        end,
    OptUser = lux_utils:user_prefix(),
    Host = lux_utils:real_hostname(),
    {ok, Cwd} = file:get_cwd(),
    ["\n<h3>Invoke:</h3>\n",
     "ssh ", OptUser, Host, "\n",
     "<br/>",
     "cd ", Cwd, "\n",
     "<br/>",
     "lux ", HostArg, "--history ", RelHtmlDir, " ", Files, "\n",
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

extract_failed_runs(T) ->
    IsFail = fun(#row{res = RowRes}) -> not is_success_res(RowRes) end,
    FailedRows = lists:takewhile(IsFail, T#table.rows),
    SortedFailedRows = lists:keysort(#row.test, FailedRows),
    SortedSplitTests = lists:keysort(1, T#table.split_tests),
    do_extract_failed_runs(SortedSplitTests, SortedFailedRows, []).

do_extract_failed_runs([{Test, Runs} | SortedSplitTests],
                       [FailedRow | SortedFailedRows] = AllSortedFailedRows,
                       Acc) ->
    if
        Test =:= FailedRow#row.test ->
            do_extract_failed_runs(SortedSplitTests,
                                   SortedFailedRows,
                                   [Runs | Acc]);
        true ->
            do_extract_failed_runs(SortedSplitTests,
                                   AllSortedFailedRows,
                                   Acc)
    end;
do_extract_failed_runs(_SortedSplitTests, [], Acc) ->
    lists:append(Acc).

extract_test_case_runs(AllRuns) ->
    Rebase =
        fun(#run{log=SL}, #run{log=EL})
              when SL =/= ?DEFAULT_LOG,
                   EL =/= ?DEFAULT_LOG ->
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
    Propagate =
        fun(SR, CR) ->
                CR2 = CR#run{run_dir     = SR#run.run_dir,
                             run_log_dir = SR#run.run_log_dir,
                             new_log_dir = SR#run.new_log_dir},
                CR2#run{runs = [CR2],
                        log = Rebase(SR, CR)}
        end,
    [Propagate(SR, CR) || SR <- AllRuns,
                          CR <- SR#run.runs].

gen_table(AbsHtmlDir, MultiBranch, Page, TagDict) ->
    #page{title    = Title,
          suppress = Suppress,
          select   = Select,
          runs     = Runs} = Page,

    %% TIME: io:format("\nTABLE ~p \t~p #~p\n", [time(), Title, length(Runs)]),
    %% Ensure order of the runs
    SplitTests = keysplit(#run.test, Runs, fun compare_run/2),
    UnsortedSplitIds = keysplit(#run.id, Runs, fun compare_run/2),
    SplitIds = lists:sort(fun compare_split/2, UnsortedSplitIds),

    %% Generate rows
    HostMap = maps:new(),
    RevSplitIds = lists:reverse(SplitIds),
    Rows = lists:zf(fun({Test, TestRuns}) ->
                            gen_row(AbsHtmlDir, Test, TestRuns, RevSplitIds,
                                    MultiBranch, TagDict,
                                    Select, Suppress, HostMap)
                    end,
                   SplitTests),
    %% Sort rows according to prio
    SplitRows = keysplit(#row.res, Rows),
    SortedSplitRows = keysort_prio(1, SplitRows),
    SortedRows = lists:append([lists:reverse(SubRows) ||
                                  {_Res, SubRows} <- SortedSplitRows]),
    case SortedRows of
        []                         -> TableRes = no_data;
        [#row{res = TableRes} | _] -> ok
    end,

    %% Generate header rows
    RowsIoList    = [R#row.iolist || R <- SortedRows],
    RunInfoIoList = lists:map(fun run_info/1, SplitIds),
    HostInfo      = fun(SI) -> host_info(SI, Page, TagDict, MultiBranch) end,
    HostIoList    = lists:map(HostInfo, SplitIds),
    CntTuple      = lists:mapfoldl(fun run_cnt/2, {1,Rows}, SplitIds),
    CntIoList     = element(1, CntTuple),

    %% Return the table
    TableIoList =
        ?l2b([
              "  <table border=\"1\">\n",
              "    <tr>\n",
              html_table_td(Title, TableRes, "left"),
              RunInfoIoList,
              "    </tr>\n",
              "    <tr>\n",
              HostIoList,
              "    </tr>\n",
              "    <tr>\n",
              CntIoList,
              "    </tr>\n",
              RowsIoList,
              "  </table>\n"
             ]),
    %% TIME: io:format("DONE  ~p \t~p\n", [time(), Title]),

    #table{res=TableRes,
           rows=SortedRows,
           iolist=TableIoList,
           split_tests=SplitTests}.

keysort_prio(Pos, List) ->
    Fun =
        fun(A, B) ->
                PrioA = element(Pos, A),
                PrioB = element(Pos, B),
                lux_utils:summary_prio(PrioA) > lux_utils:summary_prio(PrioB)
        end,
    lists:sort(Fun, List).

run_info({Id, [#run{start_time=Time, branch=RunBranch, repos_rev=Rev} | _]}) ->
    OptBranch =
        case RunBranch of
            undefined -> "";
            ""        -> "";
            Branch   -> ["<br/><strong>", Branch, "</strong>"]
        end,
    [
     "      <td>",
     Rev, "<br/>",
     "<strong>", Id, "</strong>", "<br/>",
     Time,
     OptBranch,
     "</td>\n"
    ].

page_name(overview) ->
    ["overview"];
page_name({all, host}) ->
    ["all", "hosts"];
page_name({all, config}) ->
    ["all", "configs"];
page_name({host, H}) ->
    ["host", H];
page_name({config, C}) ->
    ["config", C];
page_name(#page{tags = [Tag | _]}) ->
    page_name(Tag).

anchor_name(Tag) ->
    DeepName = page_name(Tag),
    string:join(DeepName, "_").

anchor_file(Tag, CurrP, TagDict) ->
    P = target_page(Tag, TagDict),
    File = P#page.file,
    if
        File =:= CurrP#page.file ->
            "";
        true ->
            File
    end.

full_anchor(Tag, CurrP, TagDict) ->
    File = anchor_file(Tag, CurrP, TagDict),
    [File, "#", anchor_name(Tag)].

host_info({_, [#run{hostname=H, config_name=C} | _]}, CurrP,
          TagDict, MultiBranch) ->
    OptLink =
        fun(Type, Name) ->
                case MultiBranch of
                    true ->
                        Name;
                    false ->
                        lux_html_utils:html_href(
                          "",
                          anchor_file({Type, Name}, CurrP, TagDict),
                          Name)
                end
        end,
    [
     "      <td>",
     "<strong>",
     OptLink(host, H),
     "</strong>",
     "<br/>",
     OptLink(config, C),
     "</td>\n"
    ].

run_cnt({_, _}, {N, Rows} ) ->
    Sum = fun(Pos) ->
                  lists:sum([element(Pos,lists:nth(N,Cells)) ||
                                #row{cells = Cells} <- Rows])
          end,
    FailN = Sum(#cell.n_fail),
    RunN = Sum(#cell.n_run),
    Html =
        [
         "      <td align=\"right\">",
         "<strong>", ?i2l(FailN), " (", ?i2l(RunN), ")</strong>",
         "</td>\n"
        ],
    {Html, {N+1,Rows}}.

gen_row(AbsHtmlDir, Test, TestRuns, RevSplitIds,
        MultiBranch, TagDict,
        Select, Suppress, HostMap) ->
    RevTestRuns = lists:reverse(TestRuns),
    {RowRes, Cells} =
        gen_cells(AbsHtmlDir, Test, RevTestRuns, RevSplitIds,
                  MultiBranch, TagDict, Select, Suppress,
                  HostMap, [], no_data),
    IsSuccess = is_success_res(RowRes),
    if
        IsSuccess andalso
        Suppress =:= suppress_any_success ->
            false;
        true ->
            IoList =
                if
                    RowRes =:= warning,
                    Suppress =:= suppress_any_success ->
                        [];
                    true ->
                        [
                         "    <tr>\n",
                         html_td(Test, RowRes, "left", ""),
                         [C#cell.iolist || C <- Cells],
                         "    </tr>\n"
                        ]
                end,
            Row =
                #row{res = RowRes,
                     test = Test,
                     cells = Cells,
                     iolist = IoList},
            {true, Row}
    end.

gen_cells(AbsHtmlDir, Test, [R1, R2 | TestRuns], SplitIds,
          MultiBranch, TagDict, Select, Suppress,
          HostMap, Cells, RowRes)
  when R1#run.id =:= R2#run.id ->
    %% Skip duplicate run
    gen_cells(AbsHtmlDir, Test, [R2 | TestRuns], SplitIds,
              MultiBranch, TagDict, Select, Suppress,
              HostMap, Cells, RowRes);
gen_cells(AbsHtmlDir, Test, TestRuns, [{Id, _} | SplitIds],
          MultiBranch, TagDict, Select, Suppress,
          HostMap, Cells, RowRes) ->
    if
        Id =:= (hd(TestRuns))#run.id ->
            [Run | RestRuns] = TestRuns,
            {Cell, NewHostMap} =
                gen_cell(AbsHtmlDir, Test, Run,
                         MultiBranch, TagDict, HostMap),
            NewRowRes =
                case Select of
                    select_latest when Cell#cell.res =:= no_data orelse
                                       Cell#cell.res =:= none ->
                        RowRes;
                    select_latest ->
                        Cell#cell.res;
                    select_worst ->
                        lux_utils:summary(RowRes, Cell#cell.res)
                end;
        true ->
            %% No test result
            RestRuns = TestRuns,
            Res = no_data,
            Td = html_td("-", Res, "right", Test),
            Cell = #cell{res=Res, run=undefined,
                         n_run=0, n_fail=0,
                         iolist=?l2b(Td)},
            NewHostMap = HostMap,
            NewRowRes = RowRes
    end,
    gen_cells(AbsHtmlDir, Test, RestRuns, SplitIds,
              MultiBranch, TagDict, Select, Suppress,
              NewHostMap, [Cell | Cells], NewRowRes);
gen_cells(_AbsHtmlDir, _Test, [], [],
          _MultiBranch, _TagDict, _Select, _Suppress,
          _HostMap, Cells, RowRes) ->
    %% Cells are already in the correct order. No need to revert.
    {RowRes, Cells}.

is_success_res(Res) ->
    case Res of
        no_data   -> true;
        no_branch -> true;
        success   -> true;
        none      -> true;
        skip      -> true;
        %% warning   -> true;
        _         -> false
    end.

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

gen_cell(AbsHtmlDir, Test, Run, _TagDict, MultiBranch, HostMap) ->
    RunN  = length([run  || CR <- Run#run.runs,
                            CR#run.result =/= skip]),
    FailN = length([fail || CR <- Run#run.runs,
                            CR#run.result =:= fail]),
    FailCount = lists:concat([FailN, " (", RunN, ")"]),
    Text = gen_log_link(AbsHtmlDir, Run, FailCount),
    OrigRes =
        case RunN of
            0 -> none;
            _ -> Run#run.result
        end,
    Host = Run#run.hostname,
    if
        MultiBranch ->
            Res = OrigRes,
            NewHostMap = HostMap;
        true ->
            if
                OrigRes =:= fail ->
                    Res =
                        case maps:get(Host, HostMap, no_host) of
                            fail           -> secondary_fail;
                            secondary_fail -> secondary_fail;
                            _              -> OrigRes
                        end;
                true ->
                    Res = OrigRes
            end,
            NewHostMap = maps:put(Host, OrigRes, HostMap)
    end,

    OptBranch =
        case Run#run.branch of
            undefined -> "";
            ""        -> "";
            Branch   -> ["\n", Branch]
        end,
    ToolTip = [Test, "\n",
               Run#run.config_name,"\n",
               Host,"\n",
               Run#run.start_time,"\n",
               Run#run.id,"\n",
               Run#run.repos_rev,
               OptBranch],
    Td = html_td(Text, Res, "right", ToolTip),
    Cell = #cell{res=Res, run=Run,
                 n_run=RunN, n_fail=FailN,
                 iolist=?l2b(Td)},
    {Cell, NewHostMap}.

gen_log_link(AbsHtmlDir, Run, Slogan) ->
    case Run#run.log of
        ?DEFAULT_LOG ->
            Slogan;
        OldLog ->
            NewLog =
                case lux_utils:is_url(AbsHtmlDir) of
                    true ->
                        lux_utils:join(AbsHtmlDir, OldLog);
                    false ->
                        {ok, Cwd} = file:get_cwd(),
                        NewLogDir = Run#run.new_log_dir,
                        TmpLog = filename:join([Cwd, NewLogDir, OldLog]),
                        lux_utils:drop_prefix(AbsHtmlDir, TmpLog)
                end,
            lux_html_utils:html_href([NewLog, ".html"], Slogan)
    end.

gen_page(RelHtmlDir, MultiBranch, CurrP, TagDict) ->
    AbsHtmlDir = lux_utils:normalize_filename(RelHtmlDir),
    SuiteP = CurrP#page{title = "All test suites"},
    SuiteT = gen_table(AbsHtmlDir, MultiBranch, SuiteP, TagDict),
    FailedRuns = extract_failed_runs(SuiteT),
    CaseRuns = extract_test_case_runs(FailedRuns),
    CaseP = CurrP#page{title = "Still failing test cases",
                       suppress = suppress_any_success,
                       select   = select_latest,
                       runs     = CaseRuns},
    CaseT = gen_table(AbsHtmlDir, MultiBranch, CaseP, TagDict),
    WarnIoList = gen_warnings(AbsHtmlDir, CaseT),
    {CurrP, SuiteT, CaseT, WarnIoList}.

gen_warnings(AbsHtmlDir, #table{rows = Rows}) ->
    Latest = [(hd(Row#row.cells))#cell.run  || Row <- Rows],
    HistWarns = lists:flatten([parse_warnings(R) || R <- Latest,
                                                    R =/= undefined]),
    case length(HistWarns) of
        0 ->
            <<>>;
        WarnN ->
            WarnPrio = lux_utils:summary_prio(warning),
            Count = fun(#hwarning{run = Run}, Acc) ->
                            case lux_utils:summary_prio(Run#run.result) of
                                Prio when Prio > WarnPrio -> Acc + 1;
                                _                         -> Acc
                            end
                    end,
            FailN = lists:foldl(Count, 0, HistWarns),
            SplitWarns = lists:keysort(1, keysplit(#hwarning.type, HistWarns)),
            Content = lists:flatten([gen_warning_type(AbsHtmlDir, SW) ||
                                        SW <- SplitWarns]),
            GenRow =
                fun({Type, Test, LineNo, Desc, Res})
                      when is_atom(Res) ->
                        [
                         "    <tr>\n"
                         "        ", html_td(Type,   warning, "left", ""),
                         "        ", html_td(Test,   Res,     "left", ""),
                         "        ", html_td(LineNo, warning, "left", ""),
                         "        ", html_td(Desc,   warning, "left", ""),
                         "    </tr>\n"
                        ]
                end,
            TextCalc = lists:concat([FailN, " (", WarnN, ")"]),
            ?l2b([
                  "<p/>",
                  "<a name=\"warnings\"/>\n",
                  "  <table border=\"1\">\n",
                  GenRow({["<strong>Warnings from latest run ",
                           TextCalc,"</strong>"],
                          "<strong>Test case</strong>",
                          "<strong>LineNo</strong>",
                          "<strong>Description</strong>",
                          warning}),
                  lists:map(GenRow, Content),
                  "  </table>\n"
                 ])
    end.

gen_warning_type(AbsHtmlDir, {Type, HistWarns}) ->
    Header = {Type, "", "", "", warning},
    Rows = [gen_warning(AbsHtmlDir, HW) || HW <- HistWarns],
    [Header | Rows].

gen_warning(AbsHtmlDir,
            #hwarning{reason = Reason,
                      lineno = LineNo,
                      run    = Run}) ->
    Test = Run#run.test,
    Link = gen_log_link(AbsHtmlDir, Run, Test),
    Res = Run#run.result,
    {"", Link, LineNo, Reason, Res}.

parse_warnings(R) ->
    Parse =
        fun([LineNo, Reason]) ->
                Type = classify_warning(Reason),
                #hwarning{type = Type,
                          reason = Reason,
                          lineno = LineNo,
                          run = R}
        end,
    lists:map(Parse, R#run.warnings).

%% Needs to be normalized:
%%     FAIL at line XXX in shell YYY
%%     Fail but UNSTABLE as variable XXX is not set
%%     Fail but UNSTABLE as variable XXX is set
%%     Risky timer XXX % of max
%%     Sloppy timer < XXX ppb of max
%%     Macro name XXX contains whitespace
%%     Shell name XXX contains whitespace
%%     Variable name XXX contains whitespace
%%
%% Good enoough:
%%
%%     Empty doc text
%%     Empty line expected after summary line
%%     Empty multi-line XXX command
%%     Empty send command
%%     Infinite timer
%%     Match timeout > test case_timeout
%%     Match timeout > test suite_timeout
%%     Missing summary doc (disabled for now)
%%     Missing summary line
%%     Trailing whitespaces
%%     case_timeout > suite_timeout

classify_warning(Text) ->
    Replacements =
        [
         {<<"at line \\S+ in shell \\S+">>, <<"at line XXX in shell YYY">>},
         {<<"at \\S+ in shell \\S+">>,      <<"at line XXX in shell YYY">>},
         {<<"in shell \\S+">>,              <<"in shell YYY">>},
         {<<"as variable \\S+ is">>,        <<"as variable XXX is">>},
         {<<"name \".*\" contains">>,       <<"name XXX contains">>},
         {<<"Risky timer > \\S+%">>,        <<"Risky timer > XXX%">>},
         {<<"Sloppy timer < \\S+">>,        <<"Sloppy timer < XXX">>},
         {<<"Shell \\S+ exited">>,          <<"Shell XXX exited">>},
         {<<"prematurely with status=0 and posix=normal">>, <<"prematurely">>},
         {<<"prematurely with.*">>,         <<"prematurely with error code">>}
        ],
    Opts = [{return, binary}],
    Replace = fun({From, To}, Acc) -> re:replace(Acc, From, To, Opts) end,
    lists:foldl(Replace, Text, Replacements).

write_page(RelHtmlDir, MultiBranch, AllRuns, CurrG, AllG,
           TagDict, Errors, Footer) ->
    {CurrP, SuiteT, CaseT, OptWarn} = CurrG,
    Header = header(RelHtmlDir, MultiBranch, AllRuns,
                    CurrG, AllG, TagDict, Errors),
    PageIoList =
        [
         Header,
         "<a name=\"overview\"/>\n",
         if
             SuiteT#table.res =:= no_data ->
                 "";
             true ->
                 [
                  "<h3>", CurrP#page.title, "</h3>",
                  "<p/>",
                  SuiteT#table.iolist,
                  "<p/>",
                  "<a name=\"failing_test_cases\"/>\n",
                  if
                      CaseT#table.res =:= no_data ->
                          "";
                      true ->
                          [
                           CaseT#table.iolist,
                           OptWarn
                          ]
                  end
                 ]
         end,
         Footer
        ],
    RelHtmlFile = filename:join([RelHtmlDir, CurrP#page.file]),
    case SuiteT#table.res =:= no_data andalso
        not lists:member(overview, CurrP#page.tags) of
        true ->
            %% <<"lux_history_config_no_config.html">>
            io:format("\nSkip empty file: ~s\n", [RelHtmlFile]);
        false ->
            lux_html_utils:safe_write_file(RelHtmlFile, PageIoList)
    end.

header(RelHtmlDir, MultiBranch, AllRuns, CurrG, AllG, TagDict, Errors) ->
    {CurrP, SuiteT, CaseT, WarnIoList} = CurrG,
    PageName = string:join(page_name(CurrP), " "),
    case lists:keysort(#run.repos_rev, AllRuns) of
        [] ->
            FirstRev   = ?DEFAULT_REV,
            LatestRev  = ?DEFAULT_REV,
            FirstTime  = ?DEFAULT_TIME,
            LatestTime = ?DEFAULT_TIME,
            N = 0;
        SortedRuns ->
            FirstRev    = (hd(SortedRuns))#run.repos_rev,
            LatestRev   = (lists:last(SortedRuns))#run.repos_rev,
            FirstRuns   = [R || R <- SortedRuns, R#run.repos_rev =:= FirstRev],
            LatestRuns  = [R || R <- SortedRuns, R#run.repos_rev =:= LatestRev],
            FirstRuns2  = lists:keysort(#run.repos_rev, FirstRuns),
            LatestRuns2 = lists:keysort(#run.repos_rev, LatestRuns),
            FirstTime   = (hd(FirstRuns2))#run.start_time,
            LatestTime  = (hd(LatestRuns2))#run.start_time,
            N           = length(SortedRuns)
    end,
    Top =
        [
         lux_html_utils:html_header(["Lux history ", PageName,
                                     " (", RelHtmlDir, ")"]),
         "<h1>Lux history ", PageName, " (", RelHtmlDir, ") generated at ",
         lux_utils:now_to_string(lux_utils:timestamp()),
         "</h1>",

         "<h3>", ?i2l(N),
         " runs (",
         lux_html_utils:html_href(["#errors"],
                                  [?i2l(length(Errors)),
                                   " errors"]),
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
         "</table>\n\n"
        ],
    QuickLinks =
        [
         "<h3>",
         lux_html_utils:html_href(full_anchor(overview, CurrP, TagDict),
                                  "Overview"),
         "</h3>\n",
         if
             CaseT#table.res =:= no_data ->
                 [
                  "<h3>",
                  "<strong>No failing test cases</strong>",
                  "</h3>\n"
                 ];
             true ->
                 [
                  "<h3>",
                  lux_html_utils:html_href("#failing_test_cases",
                                           "Still failing test cases"),
                  "</h3>\n"
                 ]
         end,
         if
             WarnIoList =:= <<>> ->
                 [
                  "<h3>",
                  "<strong>No warnings</strong>",
                  "</h3>\n"
                 ];
             true ->
                 [
                  "<h3>",
                  lux_html_utils:html_href("#warnings",
                                           "Warnings from latest run"),
                  "</h3>\n"
                 ]
         end,
         "\n"
        ],
    HostConfigFun =
        fun(Type, Title) ->
                ExtractedRes =
                    [{Name, ST#table.res} ||
                        {P, ST, _} <- AllG,
                        {T, Name} <- P#page.tags, T =:= Type],
                [
                 "  <table border=\"1\">\n",
                 "    <tr><td colspan=\"", ?i2l(length(ExtractedRes)), "\">\n",
                 case anchor_file({all, Type}, CurrP, TagDict) of
                     ""       ->  Title;
                     AllFile  ->  lux_html_utils:html_href(AllFile, Title)
                 end,
                 "    </td></tr>\n",
                 "    <tr>\n",
                 [
                  [
                   "    <td class=\"", ?a2l(Res), "\"> ",
                   case anchor_file({Type, Name}, CurrP, TagDict) of
                       ""       -> Name;
                       TypeFile -> lux_html_utils:html_href(TypeFile, Name)
                   end,
                   "</td>\n"
                  ] || {Name, Res} <- ExtractedRes
                 ],
                 "    </tr>\n",
                 "  </table>\n"
                ]
        end,
    [
     Top,
     legend(),
     QuickLinks,
     if
         SuiteT#table.res =:= no_data orelse
         MultiBranch ->
             "<h3>No test suites run</h3>";
         true ->
             [
              HostConfigFun(config, "All configurations"),
              "<p/>",
              HostConfigFun(host, "All hosts")
             ]
     end
    ].

legend() ->
    Legend =
        [
         {fail,           "First fail"},
         {secondary_fail, "Secondary fail on same host"},
         {warning,        "Warning"},
         {none,           "Skipped"},
         {success,        "Success"},
         {no_branch,      "No data"}
        ],
    SortedLegend = keysort_prio(1, Legend),
    LegendIo = [html_td(Text, Res, "left", "") || {Res, Text} <- SortedLegend],
    [
     "<h3>Legend / sort order</h3>\n",
     "  <table border=\"1\">\n",
     "    <tr>\n",
     LegendIo,
     "    </tr>\n",
     "  </table>\n"
    ].

parse_summary_logs(Source, RelHtmlDir,
                   Threshold, Newest, Acc, Err, WWW, Opts) ->
    RelFile = Source#source.file,
    Base = filename:basename(?b2l(RelFile)),
    RelDir = "",
    HistoryFile = ?HISTORY_LOG_BASE ++ ?HTML_EXT,
    if
        Base =:= HistoryFile ->
            %% Use history file as source
            {ParseRes, NewWWW} =
                lux_html_parse:parse_files(shallow, RelFile, WWW),
            case ParseRes of
                [{ok, _, Links, html}] ->
                    SL = ?SUITE_SUMMARY_LOG,
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
                    parse_summary_files(Source, RelHtmlDir, RelDir, Files,
                                        Threshold, Newest,
                                        Acc, Err, NewWWW, Opts);
                Errors ->
                    Strings = lux_html_parse:format_results(Errors, NewWWW),
                    Format = fun(E) ->
                                     io:format("\n\t\t~s\n", [E]),
                                     {error, RelFile, E}
                             end,
                    {{Newest, Acc, lists:map(Format, Strings)}, NewWWW}
            end;
        Base =:= ?SUITE_SUMMARY_LOG ->
            parse_summary_files(Source, RelHtmlDir, RelDir, [Base],
                                Threshold, Newest,
                                Acc, Err, WWW, Opts);
        true ->
            search_summary_dirs(Source, RelHtmlDir, RelDir,
                                Threshold, Newest,
                                Acc, Err, WWW, Opts)
    end.

search_summary_dirs(Source, RelHtmlDir, RelDir, Threshold,
                    Newest, Acc, Err, WWW, Opts)
  when is_list(RelDir) ->
    RelFile = ?b2l(Source#source.file),
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
                            parse_summary_files(Source, RelHtmlDir,
                                                RelDir, [Base],
                                                Threshold, Newest,
                                                Acc, Err, WWW, Opts);
                        false ->
                            %% Skip
                            io:format("s", []),
                            {{Newest,Acc,Err}, WWW}
                    end;
                false ->
                    %% No interesting file found. Search subdirs
                    Fun =
                        fun("latest_run", {{N,A,E}, W}) ->
                                %% Symlink
                                {{N,A,E}, W};
                           (Base, {{N,A,E}, W}) ->
                                RelDir2 =
                                    case RelDir of
                                        "" ->
                                            Base;
                                        _  ->
                                            lux_utils:join(RelDir, Base)
                                    end,
                                search_summary_dirs(Source, RelHtmlDir,
                                                    RelDir2, Threshold,
                                                    N, A, E, W, Opts)
                        end,
                    lists:foldl(Fun, {{Newest,Acc,Err}, WWW}, Files)
            end;
        {error, _Reason} ->
            %% Not a dir or problem to read dir
            {{Newest,Acc,Err}, WWW}
    end.

candidate_files() ->
    [
     "lux.skip",
     ?SUITE_SUMMARY_LOG,
     ?SUITE_SUMMARY_LOG ++ ".tmp",
     "qmscript.skip",
     "qmscript_summary.log",
     "qmscript_summary.log.tmp",
     "qmscript.summary.log"
    ].

parse_summary_files(Source, RelHtmlDir, RelDir, [Base | Bases],
                    Threshold, Newest,
                    Acc, Err, WWW, Opts)
  when is_list(RelDir) ->
    SummaryLog = source_file(Source, RelDir, Base),
    case filelib:last_modified(SummaryLog) of
        0 ->
            io:format("E", []),
            {Acc, [{error, SummaryLog, file:format_error(enoent)} | Err]};
        LastMod when LastMod > Threshold ->
            {ParseRes, NewWWW} = lux_log:parse_summary_log(SummaryLog, WWW),
            {Acc2, Err2} =
                case ParseRes of
                    {ok,_,_,_,_,_} = Res ->
                        case lux_log:parse_run_summary(Source,
                                                       SummaryLog,
                                                       Res, Opts) of
                            {error, F, Reason} ->
                                io:format("E", []),
                                {Acc, [{error, F, Reason} | Err]};
                            #run{} = R ->
                                io:format(".", []),
                                {[R|Acc], Err}
                        end;
                    {error, F, Reason} ->
                        io:format("E", []),
                        {Acc, [{error, F, Reason} | Err]}
                end,
            Newest2 = newest_datetime(LastMod, Newest),
            parse_summary_files(Source, RelHtmlDir, RelDir, Bases,
                                Threshold, Newest2,
                                Acc2, Err2, NewWWW, Opts);
        LastMod ->
            io:format("=", []), % Use cached run
            Newest2 = newest_datetime(LastMod, Newest),
            parse_summary_files(Source, RelHtmlDir, RelDir, Bases,
                                Threshold, Newest2,
                                Acc, Err, WWW, Opts)
    end;
parse_summary_files(_Source, _RelHtmlDir, _RelDir, [],
                    _Threshold, Newest,
                    Acc, Err, WWW, _Opts) ->
    {{Newest, Acc, Err}, WWW}.

newest_datetime(Newest, Oldest) when Newest > Oldest ->
    Newest;
newest_datetime(_Oldest, Newest) ->
    Newest.

source_file(Source, RelDir, Base) when is_list(RelDir), is_list(Base)  ->
    RelFile = ?b2l(Source#source.file),
    case filename:basename(RelFile) of
        ?SUITE_SUMMARY_LOG ->
            RelFile;
        _ ->
            Tmp = lux_utils:join(RelFile, RelDir),
            lux_utils:join(Tmp, Base)
    end.

multi_member([H | T], Files) ->
    case lists:member(H, Files) of
        true  -> {true, H};
        false -> multi_member(T, Files)
    end;
multi_member([], _Files) ->
    false.

%% Keysort list of tuples and group them according to the value of that field
%%
%% Items are by default returned in reverse order:
%%
%%   keysplit(1, [{3,3},{3,1},{3,2},{1,1},{1,2},{2,2},{2,1},{1,3}]).
%%   -> [{1,[{1,3},{1,2},{1,1}]},
%%       {2,[{2,1},{2,2}]},
%%       {3,[{3,2},{3,1},{3,3}]}]

keysplit(Pos, List) ->
    keysplit(Pos, List, reverse).

keysplit(Pos, List, Order) ->
    do_keysplit(Pos, lists:keysort(Pos, List), Order, [], []).

do_keysplit(Pos, [H, N | T], Order, Siblings, Acc)
  when element(Pos, H) =:= element(Pos, N) ->
    %% Collect items with same tag
    do_keysplit(Pos, [N | T], Order, [H | Siblings], Acc);
do_keysplit(Pos, [H | T], Order, Siblings, Acc) ->
    Siblings2 = [H | Siblings],
    Siblings3 = keysplit_item_sort(Order, Siblings2),
    do_keysplit(Pos, T, Order, [], [{element(Pos, H), Siblings3} | Acc]);
do_keysplit(_Pos, [], _Order, [], Acc) ->
    lists:reverse(Acc).

keysplit_item_sort(Order, ReverseItems) ->
    if
        Order =:= reverse ->
            ReverseItems;
        Order =:= original ->
            lists:reverse(ReverseItems);
        is_integer(Order) ->
            lists:keysort(Order, ReverseItems);
        is_function(Order, 1) ->
            Order(ReverseItems);
        is_function(Order, 2) ->
            lists:sort(Order, ReverseItems)
    end.
