%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2022 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_main).

-export([
         unsafe_main/1
        ]).

-include("lux.hrl").

-spec unsafe_main([string()]) -> non_neg_integer().
unsafe_main(OrigArgs) ->
    ThisEscript = require_escript(),
    LuxAppDir = require_app(undefined, ?APPLICATION),
    OpModes = op_modes(),
    Args  = expand_flags(OrigArgs, OpModes),
    Opts = lux_args:getopts(Args, LuxAppDir, ThisEscript),
    %% io:format("Args: ~p\n", [Opts]),
    dispatch(Opts, LuxAppDir, OrigArgs).

op_modes() ->
    [
     "--version",
     "--help",

     %% HTML
     "--annotate",
     "--history",

     %% Merge logs for multiple runs
     "--merge",

     %% Trace and debug
     "--suite_trace",
     "--event_trace",
     "--display_trace",
     "--internal_debug",

     %% Product management
     "--gen_markdown",
     "--pre_markdown",
     "--install",
     "--reltool",
     "--xref"
    ].

expand_flags(Args, OpModes) ->
    case Args of
        [First | _] ->
            case lists:member(First, OpModes) of
                true  -> Args; % Ignore additional flags for aux commands
                false -> do_expand_flags(Args)
            end;
        [] ->
            do_expand_flags(Args)
    end.

do_expand_flags(Args) ->
    env_to_args("LUX_FLAGS") ++ env_to_args("LUX_SYSTEM_FLAGS") ++ Args.

env_to_args(Var) ->
    case os:getenv(Var) of
        false -> [];
        Flags -> string:tokens(Flags, " ")
    end.

dispatch([{files, Files}, {prev_log_dir, PrevLogDir} | Opts],
         _LuxAppDir, OrigArgs) ->
    run(Opts, Files, PrevLogDir, OrigArgs);
dispatch(Opts, LuxAppDir, OrigArgs) ->
    OpModes = op_modes(),
    case select_op(Opts, OpModes) of
        {Op, Args} ->
            do_dispatch(Op, Opts, LuxAppDir, Args, OrigArgs);
        undefined ->
            case add_defaults(Opts) of
                {ok, [{files, Files}, {prev_log_dir, PrevLogDir} | Opts2]} ->
                    run(Opts2, Files, PrevLogDir, OrigArgs);
                {error, Format, Args} ->
                    io:format(Format, Args),
                    1
            end
    end.

do_dispatch(Op, Opts, LuxAppDir, Args, OrigArgs) ->
    RA = fun(AppName) -> require_app(Op, AppName) end,
    MA = fun(AppName, Reason) -> missing_app(Op, AppName, Reason) end,
    case {Op, Args} of
        {"--version", [_]} ->
            io:format("~s\n", [lux_utils:version()]),
            0;

        {"--annotate", [LogFile]} ->
            annotate(LogFile, Opts);
        {"--history", [LogDir]} ->
            history(LogDir, Opts);
        {"--merge", [LogDir]} ->
            merge(LogDir, Opts);

        {"--suite_trace", [_]} ->
            suite_trace(Op, LuxAppDir, Opts, OrigArgs);
        {"--event_trace", [_]} ->
            event_trace(Op, LuxAppDir, Opts, OrigArgs);
        {"--display_trace", [File]} ->
            display_trace(Op, LuxAppDir, File, Opts);
        {"--internal_debug", [_]} ->
            internal_debug(Op, OrigArgs);


        {"--gen_markdown", [File]} ->
            res_to_file_exit(lux_debug:gen_markdown(File));
        {"--pre_markdown", [File]} ->
            res_to_file_exit(lux_product:pre_markdown(LuxAppDir, File));
        {"--install", [InstallDir]} ->
            require_app(Op, reltool),
            ThisEscript = require_escript(),
            Res = lux_product:install(LuxAppDir, InstallDir, Opts,
                                      ThisEscript, RA, MA),
            res_to_file_exit(Res);
        {"--reltool", [_]} ->
            require_app(Op, reltool),
            ThisEscript = require_escript(),
            Res = lux_product:reltool(LuxAppDir, Opts, ThisEscript, RA, MA),
            res_to_file_exit(Res);
        {"--xref", [_]} ->
            require_app(Op, reltool),
            require_app(Op, tools),
            ThisEscript = require_escript(),
            Res = lux_product:xref(LuxAppDir, Opts, ThisEscript, RA, MA),
            res_to_file_exit(Res);

        _BadArgs ->
            lux_args:usage(LuxAppDir)
    end.

select_op(Opts, [Key | Keys]) ->
    case proplists:get_value(Key, Opts) of
        [] ->
            select_op(Opts, Keys);
        Vals ->
            {Key, Vals}
    end;
select_op(_Opts, []) ->
    undefined.

add_defaults(Opts) ->
    case lists:keymember(files, 1, Opts) of
        true ->
            Opts;
        false ->
            {Files, Opts2} = lux_args:translate_opts(Opts),
            do_add_defaults(Files, Opts2)
    end.

do_add_defaults(Files, Opts) ->
    UserRun = proplists:get_value(run, Opts),
    UserLogDir = proplists:get_value(log_dir, Opts),
    UserExtendRun = proplists:get_value(extend_run, Opts),
    case UserExtendRun of
        undefined -> ExtendRun = false;
        ExtendRun -> ok
    end,
    StartTime = lux_utils:timestamp(),
    UniqStr = uniq_str(StartTime),
    UniqRun = "run_" ++ UniqStr,
    Run =
        case UserRun of
            undefined -> UniqRun;
            UserRun   -> UserRun
        end,
    case UserLogDir of
        undefined -> RelLogDir = filename:join(["lux_logs", UniqRun]);
        RelLogDir -> ok
    end,
    AbsLogDir0 = lux_utils:normalize_filename(RelLogDir),
    ParentDir0 = filename:dirname(AbsLogDir0),
    Link0 = filename:join([ParentDir0, "latest_run"]),
    PrevLogDir =
        case file:read_link(Link0) of
            {ok, LinkTo} ->
                %% Reuse old log dir
                LinkTo2 = filename:join([ParentDir0, LinkTo]),
                lux_utils:normalize_filename(LinkTo2);
            {error, _} ->
                undefined
        end,
    AbsLogDir =
        if
            UserLogDir =:= undefined, ExtendRun ->
                if
                    PrevLogDir =:= undefined ->
                        AbsLogDir0;
                    true ->
                        PrevLogDir
                end;
            true ->
                AbsLogDir0
        end,
    LogBase = "lux_summary.log",
    SummaryLog = filename:join([AbsLogDir, LogBase]),
    MoreOpts =
        [
         {start_time, StartTime},
         {run, Run},
         {log_dir, AbsLogDir}
        ],
    Replace = fun({Tag, _Val} = Opt, Acc) ->
                      lists:keystore(Tag, 1, Acc, Opt)
              end,
    Opts2 = lists:foldl(Replace, Opts, MoreOpts),
    Opts3 = [{files, Files}, {prev_log_dir, PrevLogDir} | Opts2],
    case lists:keyfind(mode, 1, Opts) of
        {_, M} when M =:= list     orelse
                    M =:= list_dir orelse
                    M =:= doc      orelse
                    M =:= dump     orelse
                    M =:= expand ->
            {ok, Opts3};
        _M ->
            case opt_ensure_dir(ExtendRun, SummaryLog) of
                ok ->
                    opt_create_latest_link(UserLogDir, AbsLogDir),
                    {ok, Opts3};
                summary_log_exists ->
                    {error,
                     "ERROR: Summary log file already exists: ~s\n",
                     [SummaryLog]};
                {error, FileReason} ->
                    {error,
                     "ERROR: Failed to create log directory: ~s -> ~s\n",
                     [AbsLogDir, file:format_error(FileReason)]}
            end
    end.

uniq_str({_MegaSecs, _Secs, MicroSecs} = Now) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:now_to_universal_time(Now),
    lists:concat([Year, "_", r2(Month), "_", r2(Day), "_",
                  r2(Hour), "_", r2(Min), "_", r2(Sec), "_",
                  integer_to_list(MicroSecs)]).

r2(Int) when is_integer(Int) ->
    r2(integer_to_list(Int));
r2(String) when is_list(String) ->
    string:right(String, 2, $0).

opt_ensure_dir(ExtendRun, SummaryLog) ->
    case not ExtendRun andalso filelib:is_dir(SummaryLog) of
        true  -> summary_log_exists;
        false -> filelib:ensure_dir(SummaryLog)
    end.

opt_create_latest_link(undefined, AbsLogDir) ->
    ParentDir = filename:dirname(AbsLogDir),
    Link = filename:join([ParentDir, "latest_run"]),
    Base = filename:basename(AbsLogDir),
    _ = file:delete(Link),
    _ = file:make_symlink(Base, Link),
    ok;
opt_create_latest_link(_UserLogDir, _AbsLogDir) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Erlang trace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite_trace(MainOp, LuxAppDir, Opts, OrigArgs) ->
    require_app(MainOp, runtime_tools),
    case add_defaults(Opts) of
        {ok, Opts2} ->
            LogDir = proplists:get_value(log_dir, Opts2),
            TraceFile0 = filename:join([LogDir, "lux_suite"]),
            {ok, TraceFile} =
                lux_trace:start_suite_trace(TraceFile0, [?ESCRIPT_MOD]),
            io:format("WARNING: Internal tracing of suite started,"
                      " see file ~s\n",
                      [TraceFile]),
            Key = "--suite_trace",
            NewOpts = lists:keyreplace(Key, 1, Opts2, {Key, []}),
            Res = dispatch(NewOpts, LuxAppDir, OrigArgs),
            lux_trace:stop_trace(),
            Res;
        {error, Format, Args} ->
            io:format(Format, Args),
            1
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Event trace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

event_trace(MainOp, LuxAppDir, Opts, OrigArgs) ->
    require_app(MainOp, runtime_tools),
    require_app(MainOp, et),
    require_app(MainOp, wx),
    EtOpts =
        [
         {title,"LUX"},
         {trace_global,true},
         {trace_pattern,{lux,max}},
         {hide_unknown, true},
         {max_actors,10}
        ],
    case et_viewer:start(EtOpts) of
        {ok, ViewerPid} ->
            ViewerRef = erlang:monitor(process, ViewerPid),
            Key = "--event_trace",
            NewOpts = lists:keyreplace(Key, 1, Opts, {Key, []}),
            dispatch(NewOpts, LuxAppDir, OrigArgs),
            receive
                {'DOWN', ViewerRef, _Type, _Object, shutdown} ->
                    ok;
                {'DOWN', ViewerRef, _Type, _Object, Reason} ->
                    io:format("Event tracer exit: ~p\n", [Reason])
            end,
            0;
        {error, Reason} ->
            io:format("Event tracer error: ~p\n", [Reason]),
            1
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Display trace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_trace(MainOp, LuxAppDir, TraceFile, Opts) ->
    require_app(MainOp, runtime_tools),
    FilterFile = filter_trace_file(LuxAppDir, Opts),
    lux_trace:display(TraceFile, FilterFile).

filter_trace_file(LuxAppDir, Opts) ->
    case lists:keyfind("--filter_trace", 1, Opts) of
        {_, []} ->
            filename:join([LuxAppDir, "priv", "filter_trace"]);
        {_, FilterFile} ->
            lists:last(FilterFile)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Erlang debug
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

internal_debug(MainOp, Args) ->
    ThisEscript = require_escript(),
    require_app(MainOp, debugger),
    require_app(MainOp, wx),
    {ok, [{shebang, Shebang0} | _Rest]} = escript:extract(ThisEscript, []),
    case Shebang0 of
        default -> Shebang = "/usr/bin/env escript";
        Shebang -> ok
    end,
    FlatArgs = lists:flatten([[" ", A] || A <- Args,
                                          A =/= "-D",
                                          A =/= "--internal_debug"]),
    %% Use "escript -d" to enable debugging of this escript. Break at main/1.
    DebugCmd = Shebang ++ " -d " ++ ThisEscript ++ FlatArgs,
    io:format("~s\n", [DebugCmd]),
    PortOpts = [binary, stream, use_stdio, stderr_to_stdout, eof],
    Port = open_port({spawn, DebugCmd}, PortOpts),
    spawn_link(fun() -> forward_stdin(Port, 1) end),
    forward_stdout(Port).

forward_stdin(Port, N) ->
    case io:get_line("") of
        eof when N =:= 1 ->
            %% Closed already at startup
            exit(normal);
        eof ->
            io:format("\nEOF: stdin closed\n", []),
            exit(normal);
        {error,terminated} ->
            exit(normal);
        Data ->
            %% forward to port
            true = port_command(Port, Data),
            forward_stdin(Port, N+2)
    end.

forward_stdout(Port) ->
    receive
        {Port, {data, Data}} ->
            io:format("~s", [Data]),
            forward_stdout(Port);
        {Port, eof} ->
            port_close(Port),
            0;
        Unexpected ->
            io:format("Got something unexpected from port: ~p\n",
                      [Unexpected]),
            forward_stdout(Port)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HTML
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annotate(LogFile0, Opts) ->
    {_Files, Opts2} = lux_args:translate_opts(Opts),
    LogFile = filename:absname(LogFile0),
    io:format("Annotating logs ~s...", [LogFile]),
    case lux_suite:annotate_log(true, LogFile, Opts2) of
        ok ->
            io:format("...ok\n", []),
            io:format("\nfile://~s\n", [LogFile ++ ".html"]),
            0;
        {error, File, ReasonStr} ->
            io:format("...ERROR\n\t~p: ~s\n", [File, ReasonStr]),
            1
    end.

history(RelLogDir, Opts) ->
    {LogDirs0, Opts2} = lux_args:translate_opts(Opts),
    case LogDirs0 of
        []    -> LogDirs = [RelLogDir];
        LogDirs -> ok
    end,
    case safe_history(LogDirs, RelLogDir, Opts2) of
        {ok, RelHtmlFile} ->
            io:format("...ok\n", []),
            AbsHtmlFile = filename:absname(RelHtmlFile),
            io:format("\nfile://~s\n", [AbsHtmlFile]),
            0;
        {error, File, ReasonStr} ->
            io:format("...ERROR\n\t~s: ~s\n", [File, ReasonStr]),
            1
    end.

safe_history(LogDirs, RelLogDir, Opts) ->
    case lux_html_history:generate(LogDirs, RelLogDir, Opts) of
        {ok, BinRelHtmlFile} ->
            RelHtmlFile = binary_to_list(BinRelHtmlFile),
            case lux_html_parse:validate_html(RelHtmlFile, Opts) of
                ok ->
                    {ok, RelHtmlFile};
                {error, File, Reason} ->
                    {error, File, Reason}
            end;
        {error, File, Reason} ->
            {error, File, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Merge logs for multiple runs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge(RelLogDir, Opts) ->
    {LogDirs0, _Opts2} = lux_args:translate_opts(Opts),
    case LogDirs0 of
        []    -> LogDirs = [RelLogDir];
        LogDirs -> ok
    end,
    case lux_suite:merge_logs(LogDirs, RelLogDir) of
        {ok, RelHtmlFile} ->
            io:format("...ok\n", []),
            AbsHtmlFile = filename:absname(RelHtmlFile),
            io:format("\nfile://~s\n", [AbsHtmlFile]),
            0;
        {error, File, ReasonStr} ->
            io:format("...ERROR\n\t~s: ~s\n", [File, ReasonStr]),
            1
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Run tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(Opts, Files, PrevLogDir, OrigArgs) ->
    case do_run(Files, Opts, PrevLogDir, OrigArgs) of
        ok -> % HTML
            Summary = success,
            ok;
        {run_ok, Summary, _SummaryLog, _Results} -> % LUX
            ok;
        {run_error, undefined, no_input_files} ->
            Summary = error;
        {run_error, File, ReasonBin} ->
            io:format("\n\nFATAL ERROR: ~s:\n\t~s\n", [File, ReasonBin]),
            Summary = error
    end,
    exit_code(Summary).

do_run(Files, Opts, PrevLogDir, OrigArgs) ->
    case is_html(Files) of
        true ->
            lux_html_parse:validate_html(hd(Files), Opts);
        false ->
            ThisEscript = require_escript(),
            lux_suite:run(Files, Opts, PrevLogDir, [ThisEscript | OrigArgs])
    end.

is_html(Files) ->
    lists:any(fun(F) -> lists:suffix(".html", F) end, Files).

exit_code(Summary) ->
    case Summary of
        success -> 0;
        skip    -> 0;
        warning -> 0;
        fail    -> 1;
        error   -> 1
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generic application handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

require_escript() ->
    escript:script_name().

fatal_error(RetCode, Format, Args) ->
    throw({fatal_error, RetCode, Format, Args}).

res_to_file_exit(Res) ->
    case Res of
        ok ->
            0;
        {error, ReasonStr} ->
            io:format("ERROR: ~s\n", [ReasonStr]),
            1;
        {error, File, ReasonStr} ->
            io:format("ERROR: ~s: ~s\n", [File, ReasonStr]),
            1
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Original code. There is a copy in bin/lux as well.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

require_app(MainOp, AppName) ->
    case code:lib_dir(AppName) of
        {error, Reason} ->
            missing_app(MainOp, AppName, Reason);
        AppDir ->
            case application:load(AppName) of
                ok ->
                    AppDir;
                {error, {already_loaded,AppName}}->
                    AppDir;
                {error, Reason} ->
                    missing_app(MainOp, AppName, Reason)
            end
    end.

missing_app(undefined, AppName, Reason) ->
    fatal_error(4, "The application '~p' is missing.\n\t~p\n",
                [AppName, Reason]);
missing_app(MainOp, AppName, Reason) ->
    fatal_error(4,
                "The application '~p' is required for the ~p option.\n\t~p\n",
                [AppName, MainOp, Reason]).
