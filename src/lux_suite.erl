%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2012 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_suite).

-export([run/2]).

-include("lux.hrl").
-include_lib("kernel/include/file.hrl").

-record(rstate,
        {files                      :: [string()],
         mode = execute             :: list | doc | validate | execute | doc,
         skip_skip = false          :: boolean(),
         config_dir                 :: string(),
         file_pattern = "^[^\\\.].*\\\.lux" ++ [$$] :: string(),
         log_fd                     :: file:io_device(),
         log_dir                    :: file:filename(),
         summary_log                :: string(),
         config_name                :: string(),
         config_file                :: string(),
         suite = "unknown"          :: string(),
         start_time                 :: {non_neg_integer(),
                                        non_neg_integer(),
                                        non_neg_integer()},
         run                        :: string(),
         extend_run = false         :: boolean(),
         revision = ""              :: string(),
         html = enable              :: enable | success | skip | warning |
                                       fail | error | disable,
         warnings = []              :: [{warning, string(),
                                         string(), string()}],
         internal_opts = []         :: [{atom(), term()}], % Internal opts
         user_opts = []             :: [{atom(), term()}], % Command line opts
         file_opts = []             :: [{atom(), term()}], % Script opts
         config_opts = []             :: [{atom(), term()}], % Arch spec opts
         default_opts = []          :: [{atom(), term()}], % Default opts
         builtin_dict = lux_utils:builtin_dict()
                                    :: [string()], % ["name=val"]
         system_dict = lux_utils:system_dict()
                                    :: [string()] % ["name=val"]
        }).

run([], _Opts) ->
    FileErr = lux_log:safe_format(undefined,
                                  "ERROR: Mandatory script files are missing\n",
                                  []),
    {error, "", FileErr};
run(Files, Opts) when is_list(Files) ->
    case parse_ropts(Opts, #rstate{files = Files}) of
        {ok, R} when R#rstate.mode =:= list; R#rstate.mode =:= doc ->
            R2 = R#rstate{log_fd = undefined, summary_log = undefined},
            {_ConfigData, R3} = parse_config(R2),
            {R4, Summary, Results} =
                run_suites(R3, R3#rstate.files, success, []),
            write_results(R4, Summary, Results);
        {ok, R} ->
            LogDir = R#rstate.log_dir,
            SummaryLog = filename:join([LogDir, "lux_summary.log"]),
            case filelib:ensure_dir(SummaryLog) of
                ok ->
                    do_run(R, SummaryLog);
                {error, FileReason} ->
                    FileErr =
                        lux_log:safe_format(undefined,
                                    "ERROR: Failed to create log directory:"
                                    " ~s -> ~s\n",
                                    [LogDir, file:format_error(FileReason)]),
                    {error, LogDir, FileErr}
            end;
        {error, {badarg, Name, Val}} ->
            ArgErr =
                lux_log:safe_format(undefined,
                                    "ERROR: ~p is an illegal argument (~p)\n",
                                    [Name, Val]),
            {error, hd(Files), ArgErr};
        {error, File, ArgErr} ->
            {error, File, ArgErr}
    end.

do_run(R, SummaryLog) ->
    case lux_log:open_summary_log(SummaryLog, R#rstate.extend_run) of
        {ok, Exists, SummaryFd} ->
            TimerRef = start_timer(R),
            try
                R2 = R#rstate{log_fd = SummaryFd, summary_log = SummaryLog},
                {ConfigData, R3} = parse_config(R2),
                HtmlPrio = lux_utils:summary_prio(R2#rstate.html),
                InitialSummary = success,
                SummaryPrio0 = lux_utils:summary_prio(InitialSummary),
                InitialRes =
                    case Exists of
                        true ->
                            TmpLog = SummaryLog++".tmp",
                            case lux_log:parse_summary_log(TmpLog) of
                                {ok, _, Groups, _, _, _} ->
                                    initial_results(Groups);
                                {error, _} ->
                                    []
                            end;
                        false ->
                            LogDir = filename:dirname(SummaryLog),
                            ConfigLog = filename:join([LogDir,
                                                       "lux_config.log"]),
                            lux_log:write_config_log(ConfigLog, ConfigData),
                            lux_log:write_results(SummaryLog, skip, [], []),
                            %% Generate initial html log
                            if
                                SummaryPrio0 >= HtmlPrio,
                                R3#rstate.mode =/= list ->
                                    annotate_summary_log(R3, skip, []);
                                true ->
                                    ok
                            end,
                            []
                    end,
                {R5, Summary, Results} =
                    run_suites(R3, R3#rstate.files, InitialSummary, InitialRes),
                print_results(R5, Summary, Results),
                _ = write_results(R5, Summary, Results),
                lux_log:close_summary_log(SummaryFd, SummaryLog),
                SummaryPrio = lux_utils:summary_prio(Summary),
                if
                    SummaryPrio >= HtmlPrio, R5#rstate.mode =/= list ->
                        case lux_html:annotate_log(false, SummaryLog) of
                            ok ->
                                io:format("\nfile://~s\n",
                                          [SummaryLog ++ ".html"]),
                                {ok, Summary, SummaryLog, Results};
                            {error, _File, _ReasonStr} = Error ->
                                Error
                        end;
                    true ->
                        {ok, Summary, SummaryLog, Results}
                end
            catch
                throw:{error, File, ReasonStr} ->
                    lux_log:close_summary_tmp_log(SummaryFd),
                    {error, File, ReasonStr};
                Class:Reason ->
                    lux_log:close_summary_tmp_log(SummaryFd),
                    ReasonStr =
                        lists:flatten(io_lib:format("~p:~p ~p",
                                                    [Class,
                                                     Reason,
                                                     erlang:get_stacktrace()])),
                    {error, SummaryLog, ReasonStr}
            after
                cancel_timer(TimerRef)
            end;
        {error, FileReason} ->
            FileErr =
                lux_log:safe_format(undefined,
                                    "ERROR: Failed to open logfile:"
                                    " ~s -> ~s\n",
                                    [SummaryLog,
                                     file:format_error(FileReason)]),
            {error, SummaryLog, FileErr}
    end.

initial_results(Groups) ->
    Fun =
        fun(Script, {result, Res}) ->
                case Res of
                    success ->
                        {ok, Script, Res, "0", []};
                    skip ->
                        {ok, Script, Res, "0", []};
                    {error_line, RawLineNo, Reason} ->
                        {error, Script, RawLineNo, Reason};
                    {error, [Reason]} ->
                        case binary:split(Reason, <<": ">>, [global]) of
                            [_, <<"Syntax error at line ", N/binary>>, _] ->
                                {error, Script, binary_to_list(N), Reason};
                            _ ->
                                {error, Script, "0", Reason}
                        end;
                  {error, Reason} ->
                        {error, Script, "0", Reason};
                    {fail, _Script, RawLineNo, _Expected, _Actual, _Details} ->
                        {ok, Script, fail, binary_to_list(RawLineNo), []}
                end
        end,
    [Fun(Script, Res) ||
        {test_group, _Group, Cases} <- Groups,
        {test_case, Script, _Log, _Doc, _HtmlLog, Res} <- Cases].

run_suites(R, [SuiteFile | SuiteFiles], Summary, Results) ->
    Mode = R#rstate.mode,
    case list_files(R, SuiteFile) of
        {ok, CaseFiles} when Mode =:= list; Mode =:= doc ->
            %% [io:format("~s\n", [C]) || C <- CaseFiles],
            {R2, Summary2, Results2} =
                run_cases(Mode, R, SuiteFile, CaseFiles, Summary, Results),
            run_suites(R2, SuiteFiles, Summary2, Results2);
        {ok, CaseFiles} ->
            {R2, Summary2, Results2} =
                run_cases(Mode, R, SuiteFile, CaseFiles, Summary, Results),
            run_suites(R2, SuiteFiles, Summary2, Results2);
        {error, _Reason} when Mode =:= list ->
            run_suites(R, SuiteFiles, Summary, Results);
        {error, Reason} ->
            double_rlog(R, "\n~s~s\n",
                        [?TAG("test case"), SuiteFile]),
            ListErr = double_rlog(R, "~s~s: ~s\n",
                                  [?TAG("error"),
                                   SuiteFile, file:format_error(Reason)]),
            Results2 = [{error, SuiteFile, ListErr} | Results],
            run_suites(R, SuiteFiles, error, Results2)
    end;
run_suites(R, [], Summary, Results) ->
    {R, Summary, lists:reverse(Results)}.

parse_ropts([{Name, Val} = NameVal | T], R) ->
    case Name of
        %% suite options
        file_pattern when is_list(Val) ->
            parse_ropts(T, R#rstate{file_pattern = Val});
        config_dir when is_list(Val) ->
            parse_ropts(T, R#rstate{config_dir = filename:absname(Val)});
        config_name when is_list(Val) ->
            parse_ropts(T, R#rstate{config_name = Val});
        suite when is_list(Val) ->
            parse_ropts(T, R#rstate{suite = Val});
        run when is_list(Val) ->
            parse_ropts(T, R#rstate{run = Val});
        extend_run when Val =:= true; Val =:= false ->
            parse_ropts(T, R#rstate{extend_run = Val});
        revision when is_list(Val) ->
            parse_ropts(T, R#rstate{revision = Val});
        skip_skip when Val =:= true; Val =:= false ->
            parse_ropts(T, R#rstate{skip_skip = Val});
        mode when Val =:= list; Val =:= doc;
                  Val =:= validate; Val =:= execute ->
            parse_ropts(T, R#rstate{mode = Val});
        html when Val =:= enable; Val =:= success;
                  Val =:= skip; Val =:= warning;
                  Val =:= fail; Val =:= error;
                  Val =:= disable ->
            parse_ropts(T, R#rstate{html = Val});

        %% case options
        _ ->
            UserOpts = [NameVal | R#rstate.user_opts],
            parse_ropts(T, R#rstate{user_opts = UserOpts})
    end;
parse_ropts([], R) ->
    Now = erlang:now(),
    UniqStr = uniq_str(Now),
    UniqRun = "run_" ++ UniqStr,
    Run =
        case R#rstate.run of
            undefined -> UniqRun;
            UserRun   -> UserRun
        end,
    UserLogDir = pick_val(log_dir, R, undefined),
    RelLogDir =
        case UserLogDir of
            undefined ->
                filename:join(["lux_logs", UniqRun]);
            LogDir ->
                LogDir
        end,
    AbsLogDir0 = filename:absname(RelLogDir),
    ParentDir0 = filename:dirname(AbsLogDir0),
    Link0 = filename:join([ParentDir0, "latest_run"]),
    AbsLogDir =
        if
            UserLogDir =:= undefined, R#rstate.extend_run ->
                case file:read_link(Link0) of
                    {ok, LinkTo} ->
                        %% Reuse old log dir
                        filename:absname(filename:join([ParentDir0, LinkTo]));
                    {error, _} ->
                        AbsLogDir0
                end;
            true ->
                AbsLogDir0
        end,
    ParentDir = filename:dirname(AbsLogDir),
    Link = filename:join([ParentDir, "latest_run"]),
    case filelib:ensure_dir(AbsLogDir) of
        ok ->
            Base = filename:basename(AbsLogDir),
            _ = file:delete(Link),
            _ = file:make_symlink(Base, Link),
            RelFiles = R#rstate.files,
            AbsFiles = [filename:absname(F) || F <- RelFiles],
            UserOpts = lists:reverse(R#rstate.user_opts),
            UserOpts2 = merge_opts([{log_dir, AbsLogDir}], UserOpts),
            R2 = R#rstate{start_time = Now,
                          run = Run,
                          log_dir = AbsLogDir,
                          files = AbsFiles,
                          user_opts = UserOpts2},
            TagFiles = [{config_dir, R2#rstate.config_dir} |
                        [{file, F} || F <- RelFiles]],
            try
                lists:foreach(fun check_file/1, TagFiles),
                {ok, R2}
            catch
                throw:{error, File, Reason} ->
                    {error, File, Reason}
            end;
        {error, FileReason} ->
            {error, AbsLogDir, file:format_error(FileReason)}
    end.

check_file({Tag, File}) ->
    case Tag of
        config_dir when File =:= undefined ->
            ok;
        config_dir ->
            case filelib:is_dir(File) of
                true ->
                    ok;
                false ->
                    BinErr =
                        lux_log:safe_format(undefined,
                                            "ERROR: ~p ~s: ~s \n",
                                            [Tag,
                                             File,
                                             file:format_error(enoent)]),
                    throw({error, File, BinErr})
            end;
        file ->
            case filelib:is_file(File) of
                true ->
                    ok;
                false ->
                    BinErr =
                        lux_log:safe_format(undefined,
                                            "ERROR: ~s: ~s \n",
                                            [File,
                                             file:format_error(enoent)]),
                    throw({error, File, BinErr})
            end
    end.

uniq_str(Now) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:now_to_datetime(Now),
    lists:concat([Year, "_", r2(Month), "_", r2(Day), "_",
                  r2(Hour), "_", r2(Min), "_", r2(Sec)]).

r2(Int) when is_integer(Int) ->
    r2(integer_to_list(Int));
r2(String) when is_list(String) ->
    string:right(String, 2, $0).

list_files(R, File) ->
    case file:read_file_info(File) of
        {ok, #file_info{type = directory}} ->
            Fun = fun(F, Acc) -> [F | Acc] end,
            RegExp = R#rstate.file_pattern,
            Files = lux_utils:fold_files(File, RegExp, true, Fun, []),
            {ok, lists:sort(Files)};
        {ok, _} ->
            {ok, [File]};
        {error, Reason} ->
            {error, Reason}
    end.

run_cases(Mode, R, SuiteFile, [Script | Scripts], OldSummary, Results) ->
    case parse_script(R#rstate{warnings = []}, SuiteFile, Script) of
        {ok, R2, Script2, Commands, Opts} ->
            {SkipNames, SkipUnlessNames} =
                case R2#rstate.skip_skip of
                    true ->
                        {[], []};
                    false ->
                        {list_matching_variables(R2, skip, false),
                         list_matching_variables(R2, skip_unless, true)}
                end,
            RequireNames =
                case Mode of
                    doc -> [];
                    _   -> list_matching_variables(R2, require, true)
                end,
            NewWarnings = R2#rstate.warnings,
            AllWarnings = R#rstate.warnings ++ NewWarnings,
            case Mode of
                list when SkipNames =/= []; SkipUnlessNames =/= [] ->
                    run_cases(Mode, R, SuiteFile, Scripts, OldSummary, Results);
                list ->
                    io:format("~s\n", [Script]),
                    run_cases(Mode, R, SuiteFile, Scripts, OldSummary, Results);
                _ when SkipNames =/= []; SkipUnlessNames =/= [] ->
                    double_rlog(R2, "\n~s~s\n",
                                [?TAG("test case"), Script]),
                    case SkipNames of
                        [SkipName | _] ->
                            double_rlog(R2, "~sSKIP as variable ~s is set\n",
                                        [?TAG("result"), SkipName]);
                        [] ->
                            double_rlog(R2,
                                        "~sSKIP as variable ~s is not set\n",
                                        [?TAG("result"), hd(SkipUnlessNames)])
                    end,
                    Summary = skip,
                    NewSummary = lux_utils:summary(OldSummary, Summary),
                    Res = {ok, Script2, Summary, "0", []},
                    Results2 = [Res | Results],
                    run_cases(Mode, R#rstate{warnings = AllWarnings},
                              SuiteFile, Scripts, NewSummary, Results2);
                _ when RequireNames =/= [] ->
                    double_rlog(R2, "\n~s~s\n",
                                [?TAG("test case"), Script]),
                    double_rlog(R2,
                                "~sFAIL as required variable ~s is not set\n",
                                [?TAG("result"),
                                 hd(RequireNames)]),
                    Summary = fail,
                    NewSummary = lux_utils:summary(OldSummary, Summary),
                    Res = {ok, Script2, Summary, "0", []},
                    Results2 = [Res | Results],
                    run_cases(Mode, R#rstate{warnings = AllWarnings},
                              SuiteFile, Scripts, NewSummary, Results2);
                doc ->
                    Docs = extract_doc(Script2, Commands),
                    {ok, Cwd} = file:get_cwd(),
                    io:format("~s:\n",
                              [lux_utils:drop_prefix(Cwd, Script)]),
                    [io:format("~s~s\n", [lists:duplicate(Level, $\t), Str]) ||
                        #cmd{arg = {Level, Str}} <- Docs],
                    case NewWarnings of
                        [] ->
                            NewSummary = OldSummary,
                            Results2 = Results;
                        _ ->
                            Summary = warning,
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            Results2 = [{Summary, Script2, NewWarnings} |
                                        Results]
                    end,
                    run_cases(Mode, R#rstate{warnings = AllWarnings},
                              SuiteFile, Scripts, NewSummary, Results2);
                validate ->
                    double_rlog(R2, "\n~s~s\n",
                                [?TAG("test case"), Script]),
                    case NewWarnings of
                        [] ->
                            Summary = success,
                            NewSummary = OldSummary,
                            Results2 = Results;
                        _ ->
                            Summary = warning,
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            Results2 = [{NewSummary, Script2, NewWarnings} |
                                        Results]
                    end,
                    double_rlog(R2, "~s~s\n",
                                [?TAG("result"),
                                 string:to_upper(atom_to_list(Summary))]),
                    run_cases(Mode, R#rstate{warnings = AllWarnings},
                              SuiteFile, Scripts, NewSummary, Results2);
                execute ->
                    double_rlog(R2, "\n~s~s\n",
                                [?TAG("test case"), Script]),
                    Res = lux:interpret_commands(Script2, Commands, Opts),
                    case Res of
                        {ok, _, CaseLogDir, Summary, FullLineNo, Events} ->
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            Res2 = {ok, Script, Summary, FullLineNo, Events},
                            NewResults = [Res2 | Results];
                        {error, _, CaseLogDir, _, _} ->
                            Summary = error,
                            NewSummary = lux_utils:summary(OldSummary, Summary),
                            NewResults = [Res | Results]
                    end,
                    HtmlPrio = lux_utils:summary_prio(R2#rstate.html),
                    SummaryPrio = lux_utils:summary_prio(NewSummary),
                    R3 = R2#rstate{warnings = AllWarnings},
                    if
                        SummaryPrio >= HtmlPrio ->
                            Base = filename:basename(Script),
                            EventLog = filename:join([CaseLogDir,
                                                      Base ++ ".event.log"]),
                            lux_html:annotate_log(false, EventLog),
                            annotate_summary_log(R3, NewSummary, NewResults);
                        true ->
                            ignore
                    end,
                    run_cases(Mode, R3,
                              SuiteFile, Scripts, NewSummary, NewResults)
            end;
        {error, _R2, _ErrorStack, _ErrorBin} when Mode =:= list ->
            io:format("~s\n", [Script]),
            run_cases(Mode, R, SuiteFile, Scripts, OldSummary, Results);
        {error, _R2, ErrorStack, ErrorBin} when Mode =:= doc ->
            {MainFile, _FullLineNo, ErrorBin2} =
                parse_error(ErrorStack, ErrorBin),
            io:format("~s:\n\tERROR: ~s: ~s\n",
                      [Script, MainFile, ErrorBin2]),
            run_cases(Mode, R, SuiteFile, Scripts, OldSummary, Results);
        {error, R2, ErrorStack, ErrorBin} ->
            {MainFile, FullLineNo, ErrorBin2} =
                parse_error(ErrorStack, ErrorBin),
            double_rlog(R2, "\n~s~s\n",
                        [?TAG("test case"), Script]),
            double_rlog(R2, "~sERROR ~s: ~s\n",
                        [?TAG("result"), MainFile, ErrorBin2]),
            NewWarnings = R2#rstate.warnings,
            AllWarnings = R#rstate.warnings ++ NewWarnings,
            Summary = error,
            NewSummary = lux_utils:summary(OldSummary, Summary),
            Results2 = [{error, MainFile, FullLineNo, ErrorBin2} | Results],
            run_cases(Mode, R#rstate{warnings = AllWarnings},
                      SuiteFile, Scripts, NewSummary, Results2)
    end;
run_cases(_Mode, R, _SuiteFile, [], Summary, Results) ->
    {R, Summary, Results}.

annotate_summary_log(R, NewSummary, NewResults) ->
    file:sync(R#rstate.log_fd), % Flush summary log
    _ = write_results(R, NewSummary, NewResults),
    SummaryLog = R#rstate.summary_log,
    TmpLog = SummaryLog ++ ".tmp",
    lux_html:annotate_log(false, TmpLog),
    file:rename(TmpLog++".html",SummaryLog++".html").

list_matching_variables(R, Tag, DoNegate) ->
    Fun =
        fun(NameVal) ->
                Bool = test_variable(R, NameVal),
                case DoNegate of
                    true  -> not Bool;
                    false -> Bool
                end
        end,
    NameVals = pick_vals(Tag, R),
    lists:filter(Fun, NameVals).

test_variable(R, NameVal) ->
    Pred = fun(Char) -> Char =/= $= end,
    {Name, OptVal} = lists:splitwith(Pred, NameVal),
    UnExpanded = [$$ | Name],
    try
        Expanded = expand_vars(R, UnExpanded, error),
        case OptVal of
            [$= | Val] when Val =:= Expanded ->
                %% Test on value. Possible empty.
                true;
            [] when Expanded =/= UnExpanded ->
                %% Test on existence
                true;
            _ ->
                %% No match
                false
        end
    catch
        throw:{no_such_var, _} ->
            false
    end.


extract_doc(File, Cmds) ->
    Fun = fun(Cmd, _RevFile, _InclStack, Acc) ->
                  case Cmd of
                      #cmd{type = doc} ->
                          [Cmd | Acc];
                      _ ->
                          Acc
                  end
          end,
    lists:reverse(lux_utils:foldl_cmds(Fun, [], File, [], Cmds)).

write_results(#rstate{mode=Mode, summary_log=SummaryLog},
              Summary, Results)
  when Mode =:= list; Mode =:= doc ->
    {ok, Summary, SummaryLog, Results};
write_results(#rstate{summary_log=SummaryLog, warnings=Warnings},
              Summary, Results) when is_list(SummaryLog) ->
    lux_log:write_results(SummaryLog, Summary, Results, Warnings),
    {ok, Summary, SummaryLog, Results}.

print_results(#rstate{warnings=Warnings}, Summary, Results) ->
    lux_log:print_results({false,standard_io}, Summary, Results, Warnings).

parse_script(R, SuiteFile, Script) ->
    case lux:parse_file(Script, []) of
        {ok, Script2, Commands, FileOpts} ->
            FileOpts2 = merge_opts(FileOpts, R#rstate.file_opts),
            R2 = R#rstate{internal_opts=[],
                          file_opts = FileOpts2},
            LogDir = log_dir(R2, SuiteFile, Script2),
            LogFd = R#rstate.log_fd,
            LogFun = fun(Bin) -> lux_log:safe_write(LogFd, Bin) end,
            InternalOpts = [{log_dir, LogDir},
                            {log_fun, LogFun},
                            {log_fd,  LogFd}],
            UserOpts = R2#rstate.user_opts,
            Opts = lists:foldl(fun(New, Acc) -> merge_opts(New, Acc) end,
                               [],
                               [
                                R2#rstate.default_opts,
                                R2#rstate.config_opts,
                                FileOpts2,
                                UserOpts,
                                InternalOpts
                               ]),
            R3 = R2#rstate{user_opts = UserOpts,
                           file_opts = FileOpts2,
                           internal_opts = InternalOpts},
            {ok, R3, Script2, Commands, Opts};
        {error, ErrorStack, ErrorBin} ->
            {error, R, ErrorStack, ErrorBin}
    end.

parse_config(R) ->
    %% Default opts
    DefaultBase = "luxcfg",
    DefaultDir = filename:absname(code:lib_dir(?APPLICATION, priv)),
    DefaultFile = filename:join([DefaultDir, DefaultBase]),
    DefaultOpts = parse_config_file(R, DefaultFile),
    R2 = R#rstate{default_opts = DefaultOpts},

    %% Config dir
    case pick_val(config_dir, R2, R#rstate.config_dir) of
        undefined -> ConfigDir = code:lib_dir(?APPLICATION, priv);
        ConfigDir -> ok
    end,
    check_file({config_dir, ConfigDir}),

    %% Arch spec opts
    ActualConfigName = config_name(),
    {ConfigName, ConfigFile} =
        config_file(ConfigDir, R2#rstate.config_name, ActualConfigName),
    ConfigOpts = parse_config_file(R2, ConfigFile),
    R3 = R2#rstate{config_name = ConfigName,
                   config_dir = ConfigDir,
                   config_file = ConfigFile,
                   config_opts = ConfigOpts},
    ConfigData =
        builtins(R, ActualConfigName) ++
        [{'default file', DefaultFile}] ++ DefaultOpts ++
        [{'config file', ConfigFile}] ++ ConfigOpts,
    {ConfigData, R3}.

builtins(R, ActualConfigName) ->
    {ok, Cwd} = file:get_cwd(),
    [
     {'start time', lux_utils:now_to_string(R#rstate.start_time)},
     {hostname, hostname()},
     {architecture, ActualConfigName},
     {'system info', sys_info()},
     {suite, R#rstate.suite},
     {run, R#rstate.run},
     {revision, R#rstate.revision},
     {workdir, Cwd},
     {'config name', R#rstate.config_name},
     {config_dir, R#rstate.config_dir}
    ].

config_name() ->
    case string:tokens(os:cmd("uname -sm; echo $?"), "\n ") of
        [Kernel, Machine, CmdStatus] when CmdStatus =:= "0" ->
            Kernel ++ "-" ++ Machine;
        _Bad ->
            erlang:system_info(system_architecture)
    end.

parse_config_file(R, ConfigFile) ->
    Key = config_dir,
    case lux:parse_file(ConfigFile, []) of
        {ok, _File, _Commands, Opts} ->
            Opts2 =
                case lists:keyfind(Key, 1, Opts) of
                    false ->
                        Opts;
                    {_, Dir} ->
                        Top = filename:dirname(ConfigFile),
                        Dir2 = filename:absname(Dir, Top),
                        lists:keystore(Key, 1, Opts, {Key, Dir2})
                end,
            lists:keydelete(log_dir, 1, Opts2);
        {error, ErrorStack, ErrorBin} ->
            Enoent = list_to_binary(file:format_error(enoent)),
            if
                ErrorBin =:= Enoent ->
                    ok;
                true ->
                    {MainFile, FullLineNo, ErrorBin2} =
                        parse_error(ErrorStack, ErrorBin),
                    double_rlog(R, "~s~s: ~s: ~s\n",
                                [?TAG("error"), MainFile,
                                 ErrorBin2, FullLineNo])
            end,
            []
    end.

parse_error(ErrorStack, ErrorBin) ->
    {MainFile,_} = hd(ErrorStack),
    {ErrorFile, _} = lists:last(ErrorStack),
    FullLineNo = lux_utils:full_lineno(ErrorStack),
    if
        ErrorFile =:= MainFile ->
            {MainFile, FullLineNo, ErrorBin};
        true ->
            FileBin = list_to_binary(ErrorFile),
            ErrorBin2 = <<FileBin/binary, ": ", ErrorBin/binary>>,
            {MainFile, FullLineNo, ErrorBin2}
    end.

config_file(ConfigDir, UserConfigName, ActualConfigName) ->
    Ext = ".luxcfg",
    case UserConfigName of
        undefined ->
            Host = hostname(),
            HostFile = filename:join([ConfigDir, Host ++ Ext]),
            case filelib:is_regular(HostFile) of
                true  -> {Host, HostFile};
                false -> config_file2(ConfigDir, ActualConfigName, Ext)
            end;
        _ ->
            config_file2(ConfigDir, UserConfigName, Ext)
    end.

config_file2(ConfigDir, ConfigName, Ext) ->
    File = filename:join([ConfigDir, ConfigName ++ Ext]),
    case filelib:is_regular(File) of
        true ->
            {ConfigName, File};
        false ->
            DefaultFile = filename:join([ConfigDir, "luxcfg"]),
            {ConfigName, DefaultFile}
    end.

sys_info() ->
    chop_newline(os:cmd("uname -a")).

chop_newline(Line) ->
    case lists:reverse(Line) of
        [$\n | RevLine] -> lists:reverse(RevLine);
        _            -> Line
    end.

hostname() ->
    case inet:gethostname() of
        {ok, Host} -> Host;
        _          -> "localhost"
    end.

log_dir(R, SuiteFile, Script) ->
    LogDir = pick_val(log_dir, R, undefined),
    case Script =:= SuiteFile of
        true ->
            LogDir;
        false ->
            Suffix = lux_utils:drop_prefix(SuiteFile, Script),
            case filename:dirname(Suffix) of
                "."    -> LogDir;
                SubDir -> filename:join([LogDir, SubDir])
            end
    end.

%% warning(R, false, _File, _FullLineNo, _Format, _Args) ->
%%     R;
%% warning(R, true, File, FullLineNo, Format, Args) ->
%%     Warning = lists:flatten(io_lib:format(Format, Args)),
%%     rlog(R, "~s~s", [?TAG("warning"), Warning]),
%%     R#rstate{warnings = [{warning, File, FullLineNo, Warning} |
%%     R#rstate.warnings]}.

double_rlog(#rstate{log_fd = Fd}, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    case Fd of
        undefined -> list_to_binary(IoList);
        _         -> lux_log:double_write(Fd, IoList)
    end.

%% rlog(#rstate{log_fd = Fd}, Format, Args) ->
%%     IoList = io_lib:format(Format, Args),
%%     case Fd of
%%        undefined -> list_to_binary(IoList);
%%        _         -> lux_log:safe_write(Fd, IoList)
%%    end.

start_timer(R) ->
    SuiteTimeout = pick_val(suite_timeout, R, infinity),
    Msg = {suite_timeout, SuiteTimeout},
    Multiplier = pick_val(multiplier, R, 1000),
    case lux_utils:multiply(SuiteTimeout, Multiplier) of
        infinity   -> {infinity, Msg};
        NewTimeout -> {erlang:send_after(NewTimeout, self(), Msg), Msg}
    end.

cancel_timer({Ref, Msg}) ->
    case Ref of
        infinity ->
            ok;
        _ ->
            case erlang:cancel_timer(Ref) of
                false ->
                    receive
                        Msg -> ok
                    after 0 ->
                            ok
                    end;
                TimeLeft when is_integer(TimeLeft) ->
                    ok
            end
    end.

expand_vars(R, String, MissingVar) ->
    Dict = pick_vals(var, R),
    Dicts = [Dict, R#rstate.builtin_dict, R#rstate.system_dict],
    lux_utils:expand_vars(Dicts, String, MissingVar).

pick_val(Tag, R, Default) ->
    Dicts = opts_dicts(R),
    multi_key_find(Tag, 1, Dicts, Default).

multi_key_find(Tag, Pos, [Dict|Dicts], Default) ->
    case lists:keyfind(Tag, 1, Dict) of
        false ->
            multi_key_find(Tag, Pos, Dicts, Default);
        {_, Val} ->
            Val
    end;
multi_key_find(_Tag, _Pos, [], Default) ->
    Default.

pick_vals(Tag, R) ->
    Fun = fun(Dict) ->
                  case lists:keyfind(Tag, 1, Dict) of
                      false     -> [];
                      {_, Vals} -> Vals
                  end
          end,
    Dicts = opts_dicts(R),
    lists:flatmap(Fun, Dicts).

opts_dicts(#rstate{internal_opts = I,
                   user_opts = U,
                   file_opts = F,
                   config_opts = C,
                   default_opts = D}) ->
    [I, U, F, C, D].

merge_opts([{Key, Val} | KeyVals], Acc) ->
    case lists:keyfind(Key, 1, Acc) of
        false ->
            %% Insert new val
            merge_opts(KeyVals, [{Key, Val} | Acc]);
        {_, _AccVal} when Key =/= var,
                          Key =/= skip,
                          Key =/= require,
                          Key =/= shell_args ->
            %% Replace old val
            Acc2 = lists:keystore(Key, 1, Acc, {Key, Val}),
            merge_opts(KeyVals, Acc2);
        {_, AccVal} ->
            %% Prepend new val to old val
            Val2 = Val ++ AccVal,
            Acc2 = lists:keystore(Key, 1, Acc, {Key, Val2}),
            merge_opts(KeyVals, Acc2)
     end;
merge_opts([], Acc) ->
    lists:reverse(Acc).
