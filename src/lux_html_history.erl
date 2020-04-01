%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2020 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_html_history).

-export([generate/3, keysplit/3]).
-import(lux_html_utils, [html_table_td/3, html_td/4]).

-include("lux.hrl").

-define(STILL_FAILING_SUFFIX, "_still_failing").
-define(CONFIG_SUFFIX, "_config").
-define(HOST_SUFFIX, "_host").

-record(cache, {threshold, runs, errors, opts}).
-record(table, {name, res, rows, iolist, split_tests}).
-record(row,   {res, test, cells, iolist}).
-record(cell,  {res, run, n_run, n_fail, iolist}).

generate(PrefixedSources, RelHtmlFile, Opts0) ->
    io:format("Assembling history of logs from...", []),
    io:format("\nASSEMBLE ~p\n", [time()]),
    Sources = lists:map(fun split_source/1, PrefixedSources),
    SplitSources = keysplit(#source.branch, Sources),
    Opts = [{top, Sources} | Opts0],
    CacheDir = filename:dirname(RelHtmlFile),
    HistoryBase = filename:basename(?HISTORY_LOG, ".html"),
    CacheFile = filename:join([CacheDir, HistoryBase ++ ".cache"]),
    AbsHtmlFile = lux_utils:normalize_filename(RelHtmlFile),
    HistoryLogDir = ?l2b(filename:dirname(AbsHtmlFile)),
    case read_cache(CacheFile, Opts) of
        {ok, OldThreshold, OldRuns, OldErrors} ->
            ValidRuns = validate_runs(OldRuns),
            OldWWW = undefined,
            {Threshold, Runs, Errors, NewWWW} =
                collect(SplitSources, HistoryLogDir,
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

            SplitBranches = keysplit(#run.branch, Runs),
            MultiBranch = is_multi(SplitBranches),
            Cases = lists:flatten([D || #run{details = D} <- Runs]),
            io:format("\nAnalyzed ~p test runs with ~p test cases (~p errors)",
                      [length(Runs), length(Cases),length(Errors)]),
            %% io:format("\nERRORS ~p\n", [Errors]),
            %% io:format("SOURCES ~p\n",  [[S || {S, _} <- SplitSources]]),
            %% io:format("BRANCHES ~p\n", [[B || {B, _} <- SplitBranches]]),
            erlang:garbage_collect(),
            io:format("\nDONE ~p\n", [time()]),
            do_generate(HistoryLogDir, AbsHtmlFile, RelHtmlFile, Runs, Errors,
                        MultiBranch, Opts);
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
        ?HISTORY_LOG       -> filename:dirname(FileBin);
        ?SUITE_SUMMARY_LOG -> filename:dirname(FileBin);
        _                  -> FileBin % Assume file is dir
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

collect([{undefined, Sources} | SplitSources], HistoryLogDir,
        Threshold, Newest, Runs, Errors, WWW, Opts) ->
    collect([{<<"no_branch">>, Sources} | SplitSources], HistoryLogDir,
            Threshold, Newest,
            Runs, Errors, WWW, Opts);
collect([{Branch, Sources} | SplitSources], HistoryLogDir,
        Threshold, Newest, Runs, Errors, WWW, Opts) ->
    {NewNewest, OptRuns, OptErrors, NewWWW} =
        collect_branch(Sources, HistoryLogDir,
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
    collect(SplitSources, HistoryLogDir, Threshold, NewNewest,
            NewRuns, NewErrors, NewWWW, Opts);
collect([], _HistoryLogDir, _Threshold, Newest, Runs, Errors, WWW, _Opts) ->
    {Newest, Runs, Errors, WWW}.

collect_branch([Source | Sources], HistoryLogDir,
               Threshold, Newest,
               Runs, Errors, WWW, Opts) ->
    io:format("\n\t~s", [Source#source.orig]),
    {{NewNewest, NewRuns, NewErrors}, NewWWW} =
        parse_summary_logs(Source, HistoryLogDir,
                           Threshold, Newest, Runs, Errors, WWW, Opts),
    collect_branch(Sources, HistoryLogDir, Threshold, NewNewest,
                   NewRuns, NewErrors, NewWWW, Opts);
collect_branch([], _HistoryLogDir,
               _Threshold, Newest,
               Runs, Errors, WWW, _Opts) ->
    {Newest, Runs, Errors, WWW}.

do_generate(HistoryLogDir, AbsHtmlFile, RelHtmlFile,
            AllRuns, Errors, MultiBranch, Opts) ->
    HtmlDir = filename:dirname(RelHtmlFile),
    HtmlArgs = args(RelHtmlFile, Opts),
    HtmlErrors = errors(Errors),
    HeaderFun =
        fun(Section, HostTables, ConfigTables) ->
                header(Section, AllRuns,
                       HostTables, ConfigTables, HtmlDir,
                       RelHtmlFile, Errors, MultiBranch)
        end,
    Footer =
        [
         HtmlArgs,
         HtmlErrors,
         lux_html_utils:html_footer()
        ],
    case MultiBranch of
        true ->
            SplitBranches = keysplit(#run.branch, AllRuns),
            LatestRuns = latest_runs(SplitBranches),
            HostTables = [],
            MultiHosts = true,
            ConfigTables = [],
            MultiConfig = false,

            %% Overview
            LatestSlogan = "Latest run on each branch",
            TableAllIoList = [];
        false ->
            SplitHosts = keysplit(#run.hostname, AllRuns),
            LatestRuns = latest_runs(SplitHosts),
            MultiHosts = is_multi(SplitHosts),
            SplitConfigs = keysplit(#run.config_name,
                                    AllRuns,
                                    fun compare_run/2),
            MultiConfig = is_multi(SplitConfigs),

            %% Overview
            LatestSlogan =
                case SplitHosts of
                    [_] -> "Latest run";
                    _   -> "Latest run on each host"
                end,
            {TableAll, TableAllIoList} =
                table_all(HistoryLogDir, "All", AllRuns, AbsHtmlFile,
                          MultiBranch, MultiHosts, MultiConfig),
            FailedRuns = extract_failed_runs(TableAll),

            HostTables =
                table_hosts(HistoryLogDir, SplitHosts, AbsHtmlFile,
                            MultiBranch, MultiHosts, MultiConfig),
            ConfigTables =
                table_configs(HistoryLogDir, SplitConfigs, AbsHtmlFile,
                              MultiBranch, MultiHosts, MultiConfig),

            %% Still failing
            StillFailingIoList =
                [
                 HeaderFun("current failures", HostTables, ConfigTables),
                 table_still_failing(HistoryLogDir, FailedRuns, AbsHtmlFile,
                                     MultiBranch, MultiHosts, MultiConfig),
                 Footer
                ],
            StillFailingSuffix =
                insert_html_suffix(RelHtmlFile, "", ?STILL_FAILING_SUFFIX),
            StillFailingHtmlFile = lux_utils:join(HtmlDir, StillFailingSuffix),
            lux_html_utils:safe_write_file(StillFailingHtmlFile,
                                           StillFailingIoList),

            HostConfigFun =
                fun(Section, Tables, Suffix) ->
                        IoList =
                            [
                             HeaderFun(Section, HostTables, ConfigTables),
                             "<a name=\"content\"/>",
                             [T#table.iolist || T <- Tables],
                             Footer
                            ],
                        Suffix2 = insert_html_suffix(RelHtmlFile, "", Suffix),
                        HtmlFile = lux_utils:join(HtmlDir, Suffix2),
                        lux_html_utils:safe_write_file(HtmlFile, IoList)
                end,

            HostConfigFun("host", HostTables, ?HOST_SUFFIX),
            HostConfigFun("config", ConfigTables, ?CONFIG_SUFFIX)
    end,
    OverviewIoList =
        [
         HeaderFun("overview", HostTables, ConfigTables),
         table_latest(HistoryLogDir, LatestRuns, ConfigTables,
                      AbsHtmlFile, MultiBranch, LatestSlogan,
                      MultiHosts, MultiConfig),
         TableAllIoList,
         Footer
        ],
    lux_html_utils:safe_write_file(RelHtmlFile, OverviewIoList).

args(RelHtmlFile, Opts) when is_list (RelHtmlFile) ->
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
                [?b2l(S#source.orig) || S <- Sources]
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

header(Section, AllRuns, HostTables, ConfigTables,
       HtmlDir, HtmlFile, Errors, MultiBranch) ->
    Dir = filename:basename(filename:dirname(HtmlFile)),
    case lists:keysort(#run.repos_rev, AllRuns) of
        [] ->
            FirstRev = ?DEFAULT_REV,
            LatestRev = ?DEFAULT_REV,
            FirstTime = ?DEFAULT_TIME,
            LatestTime = ?DEFAULT_TIME,
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
    Top =
        [
         lux_html_utils:html_header(["Lux history ", Section, " (", Dir, ")"]),
         "<h1>Lux history ", Section, " (", Dir, ") generated at ",
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
    OverviewLink =
        [
         "<h3>",
         lux_html_utils:html_href([lux_utils:drop_prefix(HtmlDir, HtmlFile),
                                   "#content"],
                                  "Overview")
        ],
    FailingLinks =
        [
         "</h3>\n\n",
         case MultiBranch of
             true ->
                 [
                  "<h3>",
                  "Multiple branches. "
                  "No still failing test cases page generated.",
                  "</h3>\n\n"
                 ];
             false ->
                 [
                  "<h3>",
                  html_suffix_href(HtmlFile,"","#failing_suites",
                                   "Still failing test suites",
                                   ?STILL_FAILING_SUFFIX),
                  "</h3>\n\n",
                  "<h3>",
                  html_suffix_href(HtmlFile,"","#failing_cases",
                                   "Still failing test cases",
                                   ?STILL_FAILING_SUFFIX),
                  "</h3>\n\n"
                 ]
         end
        ],
    ConfigSection =
        [
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
         end
        ],
    HostSection =
        [
         if
             HostTables =:= [] ->
                 "<h3>Only one host. No host page generated.</h3>\n";
             true ->
                 [
                  "<h3>",
                  html_suffix_href(HtmlFile,"", "#content",
                                   "Hosts", ?HOST_SUFFIX),
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
        ],
    IoList =
        [
         Top,
         legend(),
         OverviewLink,
         FailingLinks,
         ConfigSection,
         HostSection
        ],
    ?l2b(IoList).

html_suffix_href_td(HtmlFile, Text, skip, Suffix) ->
    html_suffix_href_td(HtmlFile, Text, none, Suffix);
html_suffix_href_td(HtmlFile, Text, Res, Suffix) ->
    [
     "    ",
     "<td class=\"", ?a2l(Res), "\"> ",
     html_suffix_href(HtmlFile,"", "#" ++ Text, Text, Suffix),
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

table_latest(HistoryLogDir, LatestRuns, ConfigTables,
             HtmlFile, MultiBranch, Slogan,
             MultiHosts, MultiConfig) ->
    {Slogan2, IoList} =
        if
            ConfigTables =:= [], not MultiBranch ->
                {"Only one config. No latest run generated.", <<>>};
            true ->
                Suppress = suppress_none,
                Select = select_worst,
                T = gen_table(HistoryLogDir, "Latest", "All test suites",
                              LatestRuns, HtmlFile, MultiBranch,
                              Suppress, Select,
                              MultiHosts, MultiConfig),
                {Slogan, T#table.iolist}
        end,
    [
     lux_html_utils:html_anchor("h3", "", "content", Slogan2),
     "\n",
     IoList
    ].

table_all(HistoryLogDir, Name, AllRuns, HtmlFile,
          MultiBranch, MultiHosts, MultiConfig) ->
    Suppress = suppress_none,
    Select = select_latest,
    T = gen_table(HistoryLogDir, Name, "All test suites",
                  AllRuns, HtmlFile, MultiBranch,
                  Suppress, Select,
                  MultiHosts, MultiConfig),
    IoList =
        [
         lux_html_utils:html_anchor("h3", "", "all_runs", "All runs"),
         T#table.iolist
        ],
    {T, IoList}.

table_still_failing(HistoryLogDir, SuiteRuns, HtmlFile,
                    MultiBranch, MultiHosts, MultiConfig) ->
    Suppress = suppress_any_success,
    Select = select_latest,
    TS = gen_table(HistoryLogDir, "All", "Still failing test suites",
                   SuiteRuns, HtmlFile, MultiBranch,
                   Suppress, Select,
                   MultiHosts, MultiConfig),
    CaseRuns = extract_test_case_runs(SuiteRuns),
    TC = gen_table(HistoryLogDir, "All", "Still failing test cases",
                   CaseRuns, HtmlFile, MultiBranch,
                   Suppress, Select,
                   MultiHosts, MultiConfig),
    [
     "<a name=\"content\"/>\n",
     still_failing_header(TS, "suites"),
     still_failing_header(TC, "cases")
    ].

extract_failed_runs(TableAll) ->
    IsFail = fun(Row) -> not is_success_res(Row#row.res) end,
    FailedRows = lists:takewhile(IsFail, TableAll#table.rows),
    SortedFailedRows = lists:keysort(#row.test, FailedRows),
    SortedSplitTests = lists:keysort(1, TableAll#table.split_tests),
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
        fun(R, D) ->
               D2 = D#run{run_dir     = R#run.run_dir,
                          run_log_dir = R#run.run_log_dir,
                          new_log_dir = R#run.new_log_dir},
                D2#run{details = [D2],
                       log = Rebase(R, D2)}
        end,
    [Propagate(R, D) || R <- AllRuns,
                        D <- R#run.details].

still_failing_header(#table{res = no_data}, Type) ->
    [
     lux_html_utils:html_anchor("h3", "", "failing_"++Type,
                                "No still failing test "++Type),
     "\n"
    ];
still_failing_header(#table{iolist = IoList}, Type) ->
    [
     lux_html_utils:html_anchor("h3", "", "failing_"++Type,
                                "Still failing test "++Type),
     "\n",
     IoList,
     "\n"
    ].

table_hosts(_HistoryLogDir, _SplitHosts, _HtmlFile,
            _MultiBranch, false, _MultiConfig) ->
    [];
table_hosts(HistoryLogDir, SplitHosts, HtmlFile,
            MultiBranch, true=MultiHosts, MultiConfig) ->
    [
     double_table(HistoryLogDir,
                  Host,
                  ["Host: ", Host, " (", (hd(Runs))#run.config_name, ")"],
                  Runs,
                  HtmlFile,
                  MultiBranch,
                  MultiHosts,
                  MultiConfig) ||
        {Host, Runs} <- SplitHosts
    ].

table_configs(_HistoryLogDir, _SplitConfigs, _HtmlFile,
              _MultiBranch, _MultiHosts, false) ->
    [];
table_configs(HistoryLogDir, SplitConfigs, HtmlFile,
              MultiBranch, MultiHosts, true=MultiConfig) ->
    [
     double_table(HistoryLogDir,
                  ConfigName,
                  "Config: " ++ ConfigName,
                  Runs,
                  HtmlFile,
                  MultiBranch,
                  MultiHosts,
                  MultiConfig) ||
        {ConfigName, Runs} <- SplitConfigs
    ].

double_table(HistoryLogDir, Name, Label, AllRuns, HtmlFile,
             MultiBranch, MultiHosts, MultiConfig) ->
    {TableAll, TableAllIoList} =
        table_all(HistoryLogDir, "All", AllRuns, HtmlFile,
                  MultiBranch, MultiHosts, MultiConfig),
    case MultiBranch of
        true ->
            TableFailedCasesIoList = [];
        false ->
            FailedRuns = extract_failed_runs(TableAll),
            Details = extract_test_case_runs(FailedRuns),
            Suppress = suppress_any_success,
            Select = select_latest,
            TableFailedCases =
                gen_table(HistoryLogDir, Name, "Still failing test cases",
                          Details, HtmlFile, MultiBranch,
                          Suppress, Select,
                          MultiHosts, MultiConfig),
            TableFailedCasesIoList =
                [
                 "\n<br/>\n",
                 TableFailedCases#table.iolist
                ]
    end,
    #table{name = Name,
           res = TableAll#table.res,
           iolist =
               [
                "\n",
                ["<h3>", lux_html_utils:html_anchor(Name, Label), "</h3>\n"],
                TableAllIoList,
                TableFailedCasesIoList
               ]}.

%% Suppress :: suppress_any_success | suppress_none
%% Select   :: select_worst | select_latest
gen_table(HistoryLogDir, Name, Grain, Runs, HtmlFile,
          MultiBranch, Suppress, Select,
          MultiHosts, MultiConfig) ->
    io:format("\nTABLE ~p \t~p #~p\n", [time(), Grain, length(Runs)]),
    %% Ensure order of the runs
    SplitTests = keysplit(#run.test, Runs, fun compare_run/2),
    UnsortedSplitIds = keysplit(#run.id, Runs, fun compare_run/2),
    SplitIds = lists:sort(fun compare_split/2, UnsortedSplitIds),

    %% Generate rows
    HostMap = maps:new(),
    RevSplitIds = lists:reverse(SplitIds),
    Rows = lists:zf(fun({Test, TestRuns}) ->
                            gen_row(HistoryLogDir, Test, TestRuns,
                                    RevSplitIds, HtmlFile,
                                    MultiBranch, Select,
                                    Suppress, HostMap)
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
    RowsIoList = [R#row.iolist || R <- SortedRows],
    RunInfoIoList = lists:map(fun run_info/1, SplitIds),
    HostInfo = fun(Rs, HF) -> host_info(Rs, HF, MultiHosts, MultiConfig) end,
    HostTuple = lists:mapfoldl(HostInfo, HtmlFile, SplitIds),
    HostIoList = element(1, HostTuple),
    CntTuple = lists:mapfoldl(fun run_cnt/2, {1,Rows}, SplitIds),
    CntIoList = element(1, CntTuple),

    %% Return the table
    TableIoList =
        ?l2b([
              "  <table border=\"1\">\n",
              "    <tr>\n",
              html_table_td(Grain, TableRes, "left"),
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
    io:format("DONE  ~p \t~p\n", [time(), Grain]),
    #table{name=Name, res=TableRes, rows=SortedRows,
           iolist=TableIoList, split_tests=SplitTests}.

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

host_info({_, [#run{hostname=HN, config_name=CN} | _]}, HtmlFile,
          MultiHosts, MultiConfig) ->
    Html =
        [
         "      <td>",
         "<strong>",
         case MultiHosts of
             true ->
                 html_suffix_href(HtmlFile, "", "#" ++ HN, HN, ?HOST_SUFFIX);
             false ->
                 HN
         end,
         "</strong>",
         "<br/>",
         case MultiConfig of
             true ->
                 html_suffix_href(HtmlFile, "", "#" ++ CN, CN, ?CONFIG_SUFFIX);
             false ->
                 CN
         end,
         "</td>\n"
        ],
    {Html, HtmlFile}.

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
         "<strong>", ?i2l(FailN), " (", ?i2l(RunN), ")</strong>\n",
         "</td>\n"
        ],
    {Html, {N+1,Rows}}.

gen_row(HistoryLogDir, Test, TestRuns, RevSplitIds, HtmlFile,
        MultiBranch, Select, Suppress, HostMap) ->
    RevTestRuns = lists:reverse(TestRuns),
    {RowRes, Cells} =
        gen_cells(HistoryLogDir, Test, RevTestRuns, RevSplitIds,
                  HtmlFile, MultiBranch, Select, Suppress,
                  HostMap, [], no_data),
    case is_success_res(RowRes) of
        true when Suppress =:= suppress_any_success ->
            false;
        _ ->
            IoList =
                [
                 "    <tr>\n",
                 html_td(Test, RowRes, "left", ""),
                 [C#cell.iolist || C <- Cells],
                 "    </tr>\n"
                ],
            Row =
                #row{res = RowRes,
                     test = Test,
                     cells = Cells,
                     iolist = IoList},
            {true, Row}
    end.

gen_cells(HistoryLogDir, Test, [R1, R2 | TestRuns], SplitIds,
          HtmlFile, MultiBranch, Select, Suppress,
          HostMap, Cells, RowRes)
  when R1#run.id =:= R2#run.id ->
    %% Skip duplicate run
    gen_cells(HistoryLogDir, Test, [R2 | TestRuns], SplitIds,
              HtmlFile, MultiBranch, Select, Suppress,
              HostMap, Cells, RowRes);
gen_cells(HistoryLogDir, Test, TestRuns, [{Id, _} | SplitIds],
          HtmlFile, MultiBranch, Select, Suppress,
          HostMap, Cells, RowRes) ->
    if
        Id =:= (hd(TestRuns))#run.id ->
            [Run | RestRuns] = TestRuns,
            {Cell, NewHostMap} =
                gen_cell(HistoryLogDir, Test, Run,
                         HtmlFile, MultiBranch, HostMap),
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
    gen_cells(HistoryLogDir, Test, RestRuns, SplitIds,
              HtmlFile, MultiBranch, Select, Suppress,
              NewHostMap, [Cell | Cells], NewRowRes);
gen_cells(_HistoryLogDir, _Test, [], [],
          _HtmlFile, _MultiBranch, _Select, _Suppress,
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
        warning   -> true;
        _         -> false
    end.

is_multi(List) ->
    case List of
        []    -> false;
        [_]   -> false;
        [_|_] -> true
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

gen_cell(HistoryLogDir, Test, Run, _HtmlFile, MultiBranch, HostMap) ->
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
                            {ok, Cwd} = file:get_cwd(),
                            NewLogDir = Run#run.new_log_dir,
                            TmpLog = filename:join([Cwd, NewLogDir, OldLog]),
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

parse_summary_logs(Source, HistoryLogDir,
                   Threshold, Newest, Acc, Err, WWW, Opts) ->
    RelFile = Source#source.file,
    Base = filename:basename(?b2l(RelFile)),
    RelDir = "",
    if
        Base =:= ?HISTORY_LOG ->
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
                    parse_summary_files(Source, HistoryLogDir, RelDir, Files,
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
            parse_summary_files(Source, HistoryLogDir, RelDir, [Base],
                                Threshold, Newest,
                                Acc, Err, WWW, Opts);
        true ->
            search_summary_dirs(Source, HistoryLogDir, RelDir,
                                Threshold, Newest,
                                Acc, Err, WWW, Opts)
    end.

search_summary_dirs(Source, HistoryLogDir, RelDir, Threshold,
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
                            parse_summary_files(Source, HistoryLogDir,
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
                                search_summary_dirs(Source, HistoryLogDir,
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

parse_summary_files(Source, HistoryLogDir, RelDir, [Base | Bases],
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
            parse_summary_files(Source, HistoryLogDir, RelDir, Bases,
                                Threshold, Newest2,
                                Acc2, Err2, NewWWW, Opts);
        LastMod ->
            io:format("=", []), % Use cached run
            Newest2 = newest_datetime(LastMod, Newest),
            parse_summary_files(Source, HistoryLogDir, RelDir, Bases,
                                Threshold, Newest2,
                                Acc, Err, WWW, Opts)
    end;
parse_summary_files(_Source, _HistoryLogDir, _RelDir, [],
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
