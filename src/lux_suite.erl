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
         config_dir                 :: string(),
         file_pattern = ".+\\\.lux" ++ [$$] :: string(),
         log_fd                     :: file:io_device(),
         summary_log                :: string(),
         config_name                :: string(),
         config_file                :: string(),
         suite = "unknown"          :: string(),
         start_time                 :: {non_neg_integer(),
                                        non_neg_integer(),
                                        non_neg_integer()},
         run                        :: string(),
         revision = ""              :: string(),
         html = enable              :: enable | success | skip | warning |
                                       fail | error | disable,
         warnings = []              :: [{warning, string(),
                                         string(), string()}],
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
    FileErr = safe_format(undefined,
                          "ERROR: Mandatory script files are missing\n",
                          []),
    {error, "", FileErr};
run(Files, Opts) when is_list(Files) ->
    case parse_ropts(Opts, #rstate{files = Files}) of
        {ok, R, _LogDir} when R#rstate.mode =:= list; R#rstate.mode =:= doc ->
            R2 = R#rstate{log_fd = undefined, summary_log = undefined},
            R3 = parse_config(R2),
            {R4, Summary, Results} =
                run_suites(R3, R3#rstate.files, success, []),
            print_results(R4, Summary, Results);
        {ok, R, LogDir} ->
            SummaryLog = filename:join([LogDir, "lux_summary.log"]),
            case filelib:ensure_dir(SummaryLog) of
                ok ->
                    do_run(R, SummaryLog);
                {error, FileReason} ->
                    FileErr =
                        safe_format(undefined,
                                    "ERROR: Failed to create log directory:"
                                    " ~s -> ~s\n",
                                    [LogDir, file:format_error(FileReason)]),
                    {error, LogDir, FileErr}
            end;
        {error, {badarg, Name, Val}} ->
            ArgErr =
                safe_format(undefined,
                            "ERROR: ~p is an illegal argument (~p)\n",
                            [Name, Val]),
            {error, hd(Files), ArgErr};
        {error, File, ArgErr} ->
            {error, File, ArgErr}
    end.

do_run(R, SummaryLog) ->
    TmpSummaryLog = SummaryLog ++ ".tmp",
    case file:open(TmpSummaryLog, [write]) of
        {ok, Fd} ->
            TimerRef = start_timer(R),
            try
                R2 = R#rstate{log_fd = Fd, summary_log = SummaryLog},
                double_log(R2, "~s~s\n",
                           [?TAG("summary log"),
                            SummaryLog]),
                R4 = parse_config(R2),
                {R5, Summary, Results} =
                    run_suites(R4, R4#rstate.files, success, []),
                print_results(R5, Summary, Results),
                file:close(Fd),
                ok = file:rename(TmpSummaryLog, SummaryLog),

                SummaryLog = R5#rstate.summary_log,
                HtmlPrio = lux_utils:summary_prio(R5#rstate.html),
                SummaryPrio = lux_utils:summary_prio(Summary),
                if
                    SummaryPrio >= HtmlPrio, R5#rstate.mode =/= list ->
                        case lux_html:annotate_log(SummaryLog) of
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
                    file:close(Fd),
                    {error, File, ReasonStr};
                Class:Reason ->
                    file:close(Fd),
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
                safe_format(undefined,
                            "ERROR: Failed to open logfile:"
                            " ~s -> ~s\n",
                            [TmpSummaryLog,
                             file:format_error(FileReason)]),
            {error, SummaryLog, FileErr}
    end.

run_suites(R, [SuiteFile | SuiteFiles], Summary, Results) ->
    Mode = R#rstate.mode,
    case list_files(R, SuiteFile) of
        {ok, CaseFiles} when Mode =:= list; Mode =:= doc ->
            %% [io:format("~s\n", [C]) || C <- CaseFiles],
            {R2, Summary2, Results2} =
                run_cases(Mode, R, SuiteFile, CaseFiles, Summary, Results),
            run_suites(R2, SuiteFiles, Summary2, Results2);
        {ok, CaseFiles} ->
            log(R, "\n~s~s\n",
                [?TAG("test group begin"), SuiteFile]),
            {R2, Summary2, Results2} =
                run_cases(Mode, R, SuiteFile, CaseFiles, Summary, Results),
            log(R2, "\n~s~s\n",
                [?TAG("test group end"), SuiteFile]),
            run_suites(R2, SuiteFiles, Summary2, Results2);
        {error, _Reason} when Mode =:= list ->
            run_suites(R, SuiteFiles, Summary, Results);
        {error, Reason} ->
            log(R, "\n~s~s\n",
                [?TAG("test group begin"), SuiteFile]),
            double_log(R, "\n~s~s\n",
                       [?TAG("test case"), SuiteFile]),
            ListErr = double_log(R, "~s~s: ~s\n",
                                 [?TAG("error"),
                                  SuiteFile, file:format_error(Reason)]),
            Results2 = [{error, SuiteFile, ListErr} | Results],
            log(R, "\n~s~s\n",
                [?TAG("test group end"), SuiteFile]),
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
        revision when is_list(Val) ->
            parse_ropts(T, R#rstate{revision = Val});
        mode when Val =:= list; Val =:= doc;
                  Val =:= validate; Val =:= execute ->
            parse_ropts(T, R#rstate{mode = Val});
        html when Val =:= enable; Val =:= success;
                  Val =:= skip; Val =:= warning;
                  Val =:= fail; Val =:= error;
                  Val =:= disable ->
            parse_ropts(T, R#rstate{html = Val});

        %% lux options
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
    RelLogDir =
        case pick_val(log_dir, R, undefined) of
            undefined ->
                Dir = "lux_logs",
                Link = filename:join([Dir, "latest_run"]),
                _ = file:delete(Link),
                _ = file:make_dir(Dir),
                _ = file:make_symlink(UniqRun, Link),
                filename:join([Dir, UniqRun]);
            LogDir ->
                LogDir
        end,
    RelFiles = R#rstate.files,
    AbsFiles = [filename:absname(F) || F <- RelFiles],
    AbsLogDir = filename:absname(RelLogDir),
    UserOpts = lists:reverse(R#rstate.user_opts),
    UserOpts2 = merge_opts([{log_dir, AbsLogDir}], UserOpts),
    R2 = R#rstate{start_time = Now,
                  run = Run,
                  files = AbsFiles,
                  user_opts = UserOpts2},
    TagFiles = [{config_dir, R2#rstate.config_dir} |
                [{file, F} || F <- RelFiles]],
    try
        lists:foreach(fun check_file/1, TagFiles),
        {ok, R2, AbsLogDir}
    catch
        throw:{error, File, Reason} ->
            {error, File, Reason}
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
                        safe_format(undefined,
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
                        safe_format(undefined,
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
            Pat = R#rstate.file_pattern,
            Files = lux_utils:fold_files(File, Pat, true, Fun, []),
            {ok, lists:sort(Files)};
        {ok, _} ->
            {ok, [File]};
        {error, Reason} ->
            {error, Reason}
    end.

run_cases(Mode, R, SuiteFile, [Script | Scripts], OldSummary, Results) ->
    case parse_script(R#rstate{warnings = []}, SuiteFile, Script) of
        {ok, R2, Script2, Commands, Opts} ->
            SkipNames = list_skip_variables(R2),
            SkipUnlessNames = list_skip_unless_variables(R2),
            RequireNames = list_require_variables(R2),
            NewWarnings = R2#rstate.warnings,
            AllWarnings = R#rstate.warnings ++ NewWarnings,
            case Mode of
                list when SkipNames =/= []->
                    run_cases(Mode, R, SuiteFile, Scripts, OldSummary, Results);
                list ->
                    io:format("~s\n", [Script]),
                    run_cases(Mode, R, SuiteFile, Scripts, OldSummary, Results);
                _ when SkipNames =/= []; SkipUnlessNames =/= [] ->
                    double_log(R2, "\n~s~s\n",
                               [?TAG("test case"), Script]),
                    case SkipNames of
                        [SkipName | _] ->
                            double_log(R2, "~sSKIP as variable ~s is set\n",
                                       [?TAG("result"), SkipName]);
                        [] ->
                            double_log(R2, "~sSKIP as variable ~s is not set\n",
                                       [?TAG("result"), hd(SkipUnlessNames)])
                    end,
                    NewSummary = skip,
                    Summary = lux_utils:summary(OldSummary, NewSummary),
                    Res = {ok, Script2, NewSummary, "0", []},
                    Results2 = [Res | Results],
                    run_cases(Mode, R#rstate{warnings = AllWarnings},
                              SuiteFile, Scripts, Summary, Results2);
              _ when RequireNames =/= [] ->
                    double_log(R2, "\n~s~s\n",
                               [?TAG("test case"), Script]),
                    double_log(R2,
                               "~sFAIL as required variable ~s is not set\n",
                               [?TAG("result"),
                                hd(RequireNames)]),
                    NewSummary = fail,
                    Summary = lux_utils:summary(OldSummary, NewSummary),
                    Res = {ok, Script2, NewSummary, "0", []},
                    Results2 = [Res | Results],
                    run_cases(Mode, R#rstate{warnings = AllWarnings},
                              SuiteFile, Scripts, Summary, Results2);
               doc ->
                    Docs = extract_doc(Script2, Commands),
                    {ok, Cwd} = file:get_cwd(),
                    io:format("~s:\n",
                              [lux_utils:drop_prefix(Cwd, Script)]),
                    [io:format("~s~s\n", [lists:duplicate(Level, $\t), Str]) ||
                        #cmd{arg = {Level, Str}} <- Docs],
                    case NewWarnings of
                        [] ->
                            Summary = OldSummary,
                            Results2 = Results;
                        _ ->
                            NewSummary = warning,
                            Summary = lux_utils:summary(OldSummary,
                                                             NewSummary),
                            Results2 = [{NewSummary, Script2, NewWarnings} |
                                        Results]
                    end,
                    run_cases(Mode, R#rstate{warnings = AllWarnings},
                              SuiteFile, Scripts, Summary, Results2);
               validate ->
                    double_log(R2, "\n~s~s\n",
                               [?TAG("test case"), Script]),
                    case NewWarnings of
                        [] ->
                            Summary = OldSummary,
                            Results2 = Results;
                        _ ->
                            NewSummary = warning,
                            Summary = lux_utils:summary(OldSummary,
                                                             NewSummary),
                            Results2 = [{NewSummary, Script2, NewWarnings} |
                                        Results]
                    end,
                    double_log(R2, "~s~s\n",
                               [?TAG("result"),
                                string:to_upper(atom_to_list(Summary))]),
                    run_cases(Mode, R#rstate{warnings = AllWarnings},
                              SuiteFile, Scripts, Summary, Results2);
                execute ->
                    double_log(R2, "\n~s~s\n",
                               [?TAG("test case"), Script]),
                    Res = lux:interpret_commands(Script2, Commands, Opts),
                    case Res of
                        {ok, _, NewSummary, FullLineNo, Events} ->
                            Summary = lux_utils:summary(OldSummary,
                                                             NewSummary),
                            Res2 = {ok, Script, NewSummary, FullLineNo, Events},
                            Results2 = [Res2 | Results];
                        {error, _, _, _} ->
                            NewSummary = error,
                            Summary = lux_utils:summary(OldSummary,
                                                             NewSummary),
                            Results2 = [Res | Results]
                    end,
                    run_cases(Mode, R#rstate{warnings = AllWarnings},
                              SuiteFile, Scripts, Summary, Results2)
            end;
        {error, _R2, _File2, _FullLineNo, _Error2} when Mode =:= list ->
            io:format("~s\n", [Script]),
            run_cases(Mode, R, SuiteFile, Scripts, OldSummary, Results);
        {error, R2, File2, FullLineNo, Error2} ->
            double_log(R2, "\n~s~s\n",
                       [?TAG("test case"), Script]),
            double_log(R2, "~sERROR ~s: ~s\n",
                       [?TAG("result"), File2, Error2]),
            NewWarnings = R2#rstate.warnings,
            AllWarnings = R#rstate.warnings ++ NewWarnings,
            NewSummary = error,
            Summary = lux_utils:summary(OldSummary, NewSummary),
            Results2 = [{error, File2, FullLineNo, Error2} | Results],
            run_cases(Mode, R#rstate{warnings = AllWarnings},
                     SuiteFile, Scripts, Summary, Results2)
    end;
run_cases(_Mode, R, _SuiteFile, [], Summary, Results) ->
    {R, Summary, Results}.

list_skip_variables(R) ->
    Fun = fun(Name) ->
                  UnExpanded = [$$ | Name],
                  Expanded = expand_vars(R, UnExpanded, keep),
                  Expanded =/= UnExpanded
          end,
    Names = pick_vals(skip, R, []),
    lists:filter(Fun, Names).

list_skip_unless_variables(R) ->
    Fun = fun(Name) ->
                  UnExpanded = [$$ | Name],
                  try expand_vars(R, UnExpanded, error) of
                      _ ->
                          false
                  catch
                      throw:{no_such_var, _} ->
                          true
                  end
          end,
    Names = pick_vals(skip_unless, R, []),
    lists:filter(Fun, Names).

list_require_variables(R) ->
    Fun = fun(Name) ->
                  UnExpanded = [$$ | Name],
                  Expanded = expand_vars(R, UnExpanded, keep),
                  Expanded =:= UnExpanded
          end,
    Names = pick_vals(require, R, []),
    lists:filter(Fun, Names).

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

print_results(R, Summary, Results) when R#rstate.mode == list;
                                        R#rstate.mode == doc ->
    {ok, Summary, R#rstate.summary_log, Results};
print_results(R, Summary, Results) ->
    %% Display most important results last
    io:nl(),
    log(R, "\n", []),
    SuccessScripts =
        [Script || {ok, Script, success, _FullLineNo, _Events} <- Results],
    double_log(R, "~s~p\n",
               [?TAG("successful"),
                length(SuccessScripts)]),
    print_skip(R, Results),
    print_warning(R),
    print_fail(R, Results),
    print_error(R, Results),
    double_log(R, "~s~s\n",
               [?TAG("summary"),
                [string:to_upper(Char) ||
                    Char <- atom_to_list(Summary)]]).

print_error(R, Results) ->
    case [{Script, FullLineNo} ||
             {error, Script, FullLineNo, _String} <- Results] of
        [] ->
            ok;
        ErrorScripts ->
            double_log(R, "~s~p\n",
                       [?TAG("errors"),
                        length(ErrorScripts)]),
            [double_log(R, "\t~s:~s\n", [F, L]) ||
                {F, L} <- ErrorScripts]
    end.

print_fail(R, Results) ->
    case [{Script, FullLineNo} ||
             {ok, Script, fail, FullLineNo, _Events} <- Results] of
        [] ->
            ok;
        FailScripts ->
            double_log(R, "~s~p\n",
                       [?TAG("failed"),
                        length(FailScripts)]),
            [double_log(R, "\t~s:~s\n", [F, L]) || {F, L} <- FailScripts]
    end.

print_skip(R, Results) ->
    case [{Script, FullLineNo} ||
             {ok, Script, skip, FullLineNo, _Events} <- Results] of
        [] ->
            ok;
        SkipScripts ->
            double_log(R, "~s~p\n",
                       [?TAG("skipped"),
                        length(SkipScripts)]),
            [double_log(R, "\t~s:~s\n", [F, L]) || {F, L} <- SkipScripts]
    end.


print_warning(R) ->
    case [{Script, FullLineNo} ||
             {warning, Script, FullLineNo, _String} <- R#rstate.warnings] of
        [] ->
            ok;
        WarnScripts ->
            double_log(R, "~s~p\n",
                       [?TAG("warnings"),
                        length(WarnScripts)]),
            [double_log(R, "\t~s:~s\n", [F, L]) || {F, L} <- WarnScripts]
    end.

parse_script(R, SuiteFile, Script) ->
    case lux:parse_file(Script, []) of
        {ok, Script2, Commands, FileOpts} ->
            FileOpts2 = merge_opts(FileOpts, R#rstate.file_opts),
            FileR = R#rstate{file_opts = FileOpts2},
            LogDir = log_dir(R, SuiteFile, Script),
            LogFun = fun(Bin) -> safe_write(R#rstate.log_fd, Bin) end,
            Mandatory = [{log_dir, LogDir}, {log_fun, LogFun}],
            UserOpts = merge_opts(Mandatory, FileR#rstate.user_opts),
            Opts = lists:foldl(fun(New, Acc) -> merge_opts(New, Acc) end,
                               [],
                               [
                                FileR#rstate.default_opts,
                                FileR#rstate.config_opts,
                                FileOpts2,
                                UserOpts
                               ]),
            R3 = FileR#rstate{user_opts = UserOpts,
                              file_opts = FileOpts2},
            {ok, R3, Script2, Commands, Opts};
        {error, Script2, FullLineNo, Bin} ->
            {error, R, Script2, FullLineNo, Bin}
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

    %% Log
    log_builtins(R3, ActualConfigName),
    log(R3, "~s~s\n", [?TAG("default file"), DefaultFile]),
    [log(R3, "~s~p\n",
         [?TAG(Tag), Val]) || {Tag, Val} <- DefaultOpts],
    log(R3, "~s~s\n",
        [?TAG("config file"), ConfigFile]),
    [log(R3, "~s~p\n",
         [?TAG(Tag), Val]) || {Tag, Val} <- ConfigOpts],
    R3.

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
        {error, Script, _FullLineNo, Bin} ->
            Enoent =  list_to_binary(file:format_error(enoent)),
            if
                Bin =:= Enoent ->
                    ok;
                true ->
                    double_log(R, "~s~s: ~s\n",
                               [?TAG("error"), Script, Bin])
            end,
            []
    end.

log_builtins(R, ActualConfigName) ->
    log(R, "\n~s~s\n",
        [?TAG("start time"),
         lux_utils:now_to_string(R#rstate.start_time)]),
    log(R, "~s~s\n",
        [?TAG("hostname"), hostname()]),
    log(R, "~s~s\n",
        [?TAG("architecture"), ActualConfigName]),
    log(R, "~s~s\n",
        [?TAG("system info"), sys_info()]),
    log(R, "~s~s\n",
        [?TAG("suite"), R#rstate.suite]),
    log(R, "~s~s\n",
        [?TAG("run"), R#rstate.run]),
    log(R, "~s~s\n",
        [?TAG("revision"), R#rstate.revision]),
    {ok, Cwd} = file:get_cwd(),
    log(R, "~s~s\n",
        [?TAG("workdir"), Cwd]),
    log(R, "~s~s\n",
        [?TAG("config name"), R#rstate.config_name]),
    log(R, "~s~s\n",
        [?TAG("config_dir"), R#rstate.config_dir]).


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
%%     log(R, "~s~s", [?TAG("warning"), Warning]),
%%     R#rstate{warnings = [{warning, File, FullLineNo, Warning} |
%%     R#rstate.warnings]}.

double_log(#rstate{log_fd = Fd}, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    case Fd of
        undefined ->
            list_to_binary(IoList);
        _ ->
            double_write(Fd, IoList)
    end.

double_write(Fd, IoList) ->
    Bin = safe_write(Fd, IoList),
    safe_write(undefined, Bin).

log(#rstate{log_fd = Fd}, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    case Fd of
       undefined ->
           list_to_binary(IoList);
       _ ->
           safe_write(Fd, IoList)
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

start_timer(R) ->
    SuiteTimeout = pick_val(suite_timeout, R, infinity),
    Msg = {suite_timeout, SuiteTimeout},
    Multiplier = pick_val(multiplier, R, 1000),
    case lux_utils:multiply(SuiteTimeout, Multiplier) of
        infinity ->
            {infinity, Msg};
        NewTimeout ->
            {erlang:send_after(NewTimeout, self(), Msg), Msg}
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
    Dict = pick_vals(var, R, []),
    Dicts = [Dict, R#rstate.builtin_dict, R#rstate.system_dict],
    lux_utils:expand_vars(Dicts, String, MissingVar).

pick_val(Tag,
         #rstate{user_opts = U,
                 file_opts = F,
                 config_opts = C,
                 default_opts = D},
         Builtin) ->
    case lists:keyfind(Tag, 1, U) of
        false ->
            case lists:keyfind(Tag, 1, F) of
                false ->
                    case lists:keyfind(Tag, 1, C) of
                        false ->
                            case lists:keyfind(Tag, 1, D) of
                                false ->
                                    Builtin;
                                {_, Default} ->
                                    Default
                            end;
                        {_, Config} ->
                            Config
                    end;
                {_, File} ->
                    File
            end;
        {_, User} ->
            User
    end.

pick_vals(Tag,
          #rstate{user_opts = U,
                  file_opts = F,
                  config_opts = C,
                  default_opts = D},
          Builtin) ->
    case lists:keyfind(Tag, 1, U) of
        false     -> User = [];
        {_, User} -> ok
    end,
    case lists:keyfind(Tag, 1, F) of
        false     -> File = [];
        {_, File} -> ok
    end,
    case lists:keyfind(Tag, 1, C) of
        false -> Config = [];
        {_, Config} -> ok
    end,
    case lists:keyfind(Tag, 1, D) of
        false -> Default = [];
        {_, Default} -> ok
    end,
    User ++ File ++ Config ++ Default ++ Builtin.

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
