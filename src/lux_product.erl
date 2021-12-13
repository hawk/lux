%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2021 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_product).

-export([
         pre_markdown/2,
         install/6,
         reltool/5,
         xref/5
        ]).

-include("lux.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Markdown
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pre_markdown(LuxAppDir, ToFile) ->
    FromFile = ToFile ++ ".src",
    AbsFileName0 = filename:absname(ToFile),
    AbsFileName = lux_utils:normalize_filename(AbsFileName0),
    RelFileName = lux_utils:drop_prefix(LuxAppDir, AbsFileName),
    RelFileDir = filename:dirname(RelFileName),
    {ok, Cwd} = file:get_cwd(),
    RelWorkDir = lux_utils:drop_prefix(LuxAppDir, Cwd),
    case file:read_file(FromFile) of
        {ok, Bin} ->
            try
                Expand =
                    fun(L, {Files, Lines}) ->
                            case markdown_line(RelWorkDir, L) of
                                {keep, Line} ->
                                    {Files, [Line | Lines]};
                                {expand, InclFile, InclLines} ->
                                    RelInclFile =
                                        relpath(RelFileDir, InclFile),
                                    {[RelInclFile | Files],
                                     [InclLines | Lines]};
                                {expand, EvalLines} ->
                                    {Files,
                                     [EvalLines | Lines]}
                            end
                    end,
                Lines = lux_utils:split_lines(Bin),
                {InclFiles, RevNewLines} =
                    lists:foldl(Expand, {[], []}, Lines),
                DepsIoList =
                    [
                     ToFile,
                     ": ",
                     join(" ", [FromFile | lists:reverse(InclFiles)])
                    ],
                DepsFile = ToFile ++ ".d",
                %% Make dependency file
                case file:write_file(DepsFile, DepsIoList) of
                    ok ->
                        ok;
                    {error, FileReason} ->
                        throw({error,
                               "~s: ~s", [DepsFile,
                                          file:format_error(FileReason)]})
                end,
                IoList = lux_utils:expand_lines(lists:reverse(RevNewLines)),
                %% Markdown file
                case file:write_file(ToFile, IoList) of
                    ok ->
                        ok;
                    {error, FileReason2} ->
                        throw({error,
                               "~s: ~s", [ToFile,
                                          file:format_error(FileReason2)]})
                end
            catch
                throw:{error, Format, Args} ->
                    ReasonStr = lists:flatten(io_lib:format(Format, Args)),
                    {error, FromFile, ReasonStr}
            end;
        {error, FileReason} ->
            {error, FromFile, file:format_error(FileReason)}
    end.

join(_Sep, []) ->
    [];
join(Sep, [H|T]) ->
 [H | join2(Sep, T)].

join2(_Sep, []) ->
    [];
join2(Sep, [H | T]) ->
    [Sep , H | join2(Sep, T)].

relpath(".", Path) ->
    Path;
relpath(Prefix, Path) ->
    UpDir = filename:join([".." || _ <- filename:split(Prefix)]),
    filename:join([UpDir, Path]).

markdown_line(RelWorkDir, Line) ->
    Incomplete = {error, "Incomplete include statement", []},
    case string:strip(binary_to_list(Line), left) of
        "#include " ++ InclFile ->
            case string:strip(InclFile, both) of
                "<" ->
                    throw(Incomplete);
                "<" ++ InclFile2 ->
                    case lists:reverse(InclFile2) of
                        ">" ++ RevInclFile ->
                            InclFile3 = lists:reverse(RevInclFile),
                            markup_include(InclFile3);
                        _ ->
                            throw(Incomplete)
                    end;
                _ ->
                    throw(Incomplete)
            end;
        "#eval-include " ++ Cmd0 ->
            Cmd = string:strip(Cmd0),
            FullCmd =
                case relpath(RelWorkDir, "") of
                    "" -> Cmd;
                    UpDir  -> "cd " ++ UpDir ++ " && " ++ Cmd
                end,
            io:format("\t~s\n", [FullCmd]),
            {Output, RetCode} = lux_utils:cmd(FullCmd),
            io:format("\techo $?\n\t~s\n", [RetCode]),
            Prompt = ".../lux> ",
            Prefix = ">     ",
            {expand,
             [
              "Evaluate `", Cmd, "`\n\n",
              Prefix, Prompt, Cmd, "\n",
              lux_utils:expand_lines([[Prefix, L] || L <- Output]),
              "\n",
              Prefix, Prompt, "echo $?\n",
              Prefix, RetCode, "\n"
             ]};
        "#eval-silent " ++ Cmd0 ->
            Cmd = string:strip(Cmd0),
            FullCmd =
                case relpath(RelWorkDir, "") of
                    "" -> Cmd;
                    UpDir  -> "cd " ++ UpDir ++ " && " ++ Cmd
                end,
            io:format("\t~s\n", [FullCmd]),
            {_Output, RetCode} = lux_utils:cmd(FullCmd),
            io:format("\techo $?\n\t~s\n", [RetCode]),
            {expand,
             [
              "Evaluate `", Cmd, "`\n"
             ]};
        _ ->
            {keep, Line}
    end.

markup_include(RelFile) ->
    AbsFile = filename:join(["..", RelFile]),
    case file:read_file(AbsFile) of
        {ok, Bin} ->
            Lines = lux_utils:split_lines(binary_to_list(Bin)),
            RelFile2 = filename:join(["...", "lux", RelFile]),
            {expand,
             RelFile,
             [
              "Snippet from the enclosed `", RelFile2, "` file:\n",
              "\n",
              lux_utils:expand_lines([[">     ", L] || L <- Lines])
             ]};
        {error, FileReason} ->
            RelFile2 = filename:join(["..", "lux", RelFile]),
            throw({error, "~s: ~s", [RelFile2, file:format_error(FileReason)]})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Stand-alone installation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install(LuxAppDir, InstallDir, Opts, ThisEscript, RA, MA) ->
    {RootDir, ReltoolOpts} =
        reltool_opts(LuxAppDir, Opts, ThisEscript, RA, MA),
    InstallProf = reltool_profile(Opts),
    io:format("Installing ~p as a ~p system... ~s\n",
              [?APPLICATION, InstallProf, InstallDir]),
    case reltool:start_server(ReltoolOpts) of
        {ok, ServerPid} ->
            case reltool:get_status(ServerPid) of
                {ok, Warnings} ->
                    [io:format("WARNING: ~s\n", [W]) || W <- Warnings],
                    do_install(ServerPid, InstallDir, RootDir);
                {error, StatusReasonStr} ->
                    io:format("ERROR: ~s\n", [StatusReasonStr]),
                    1
            end;
        {error, StartReasonStr} ->
            {error, StartReasonStr}
    end.

do_install(ServerPid, InstallDir, RootDir) ->
    GetSpecRes = reltool:get_target_spec(ServerPid),
    _StopRes = reltool:stop(ServerPid),
    case GetSpecRes of
        {ok, Spec} ->
            case InstallDir of
                "" ->
                    io:format("Spec: ~p.\n", [Spec]),
                    timer:sleep(timer:seconds(1)),
                    ok;
                _ ->
                    reltool:eval_target_spec(Spec, RootDir, InstallDir)
            end;
        {error, SpecReasonStr} ->
            {error, SpecReasonStr}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reltool
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reltool(LuxAppDir, Opts, ThisEscript, RA, MA) ->
    {_RootDir, ReltoolOpts} =
        reltool_opts(LuxAppDir, Opts, ThisEscript, RA, MA),
    io:format("Starting reltool...\n", []),
    process_flag(trap_exit, true),
    case reltool:start(ReltoolOpts) of
        {ok, Pid} ->
            link(Pid),
            receive
                {'EXIT', Pid, Reason} ->
                    io:format("reltool exit: ~p\n", [Reason])
            end,
            ok;
        {error, ReasonStr} ->
            {error, ReasonStr}
    end.

reltool_opts(LuxAppDir, Opts, ThisEscript, RA, MA) ->
    %% Include some extra files beside the default files
    AppFilters =
        [{incl_app_filters, ["^LICENSE", "^lux.html",
                             "ebin", "^priv", "^examples.*",
                             "^emacs.*", "^vim.*"]},
         {excl_archive_filters, ["^LICENSE", "^lux.html",
                                 "^priv", "^examples.*",
                                 "^emacs.*", "^vim.*"]},
         {excl_app_filters, [".*empty$"]}],
    HasAppLibDir = app_has_feature(reltool, "0.6", mandatory, RA),
    LuxApps =
        case HasAppLibDir of
            true ->
                %% New reltool can handle lib_dir in app
                [{app, ?APPLICATION, [{lib_dir, LuxAppDir} | AppFilters]}];
            false ->
                %% Old reltool
                [{app, ?APPLICATION, AppFilters},
                 {lib_dirs, [filename:dirname(LuxAppDir)]}]
        end,
    HasRelaxedExclusion = app_has_feature(reltool, "0.6.4", mandatory, RA),
    {IsCross, RootDir} = root_dir(Opts),
    {AppCondsWx, AppCondsCross} =
        case {IsCross, HasRelaxedExclusion} of
            {true, false} ->
                {[],
                 []};
            _ ->
                %% New reltool can handle excluded non-existent apps
                {[{wx, exclude}],
                 [{hipe, exclude}, {reltool, exclude}]}
        end,
    AppCondsCommon = [{erts, include}, {crypto, exclude}, {tools, exclude}],
    AppConds = AppCondsCommon ++ AppCondsCross ++ AppCondsWx,
    {_, InstallApps} = lists:keyfind("--install_app", 1, Opts),
    InstallApps2 = [{?l2a(A), include} || A <- InstallApps],
    AppConds2 = [{A, C} || {A, C} <- AppConds,
                           not lists:keymember(A, 1, InstallApps2)],
    AppConds3 = AppConds2 ++ InstallApps2,
    ExtraApps =
        filter_apps(AppConds3, IsCross, HasRelaxedExclusion, MA),
    Common =
        [{root_dir, RootDir},
         {debug_info, strip},
         {escript, ThisEscript, [{incl_cond, include}]}],
    Profile =
        case reltool_profile(Opts) of
            standalone ->
                [{profile, standalone},
                 {excl_sys_filters,
                  {add,
                   ["^bin/(epmd|start.boot)(|\\.exe)" ++ [$$],
                    "^erts.*/bin/(epmd|heart|ct_run)(|\\.exe)"  ++ [$$]]}}];
            InstallProf ->
                [{profile, InstallProf}]
        end,
    {RootDir, [{sys, Common ++ LuxApps ++ ExtraApps ++ Profile}]}.

reltool_profile(Opts) ->
    case lists:keyfind("--install_profile", 1, Opts) of
        {_, []}      -> standalone;
        {_, Profile} -> lists:last(Profile)
    end.

app_has_feature(AppName, LowestVersion, Require, RA) ->
    LoadedApps = application:loaded_applications(),
    case lists:keyfind(AppName, 1, LoadedApps) of
        {_Name, _Slogan, Version} when Version >= LowestVersion ->
            true;
        {_Name, _Slogan, _Version} ->
            false;
        false when Require =:= optional ->
            false;
        false when Require =:= mandatory ->
            RA(AppName), % Halt upon failure
            app_has_feature(AppName, LowestVersion, Require, RA)
    end.

filter_apps(AppConds, _IsCross, true, _MA) ->
    [{app, AppName, [{incl_cond,InclCond}]} || {AppName,InclCond} <- AppConds];
filter_apps(AppConds, _IsCross, false, MA) ->
    lists:zf(fun({AppName, InclCond}) ->
                     case application:load(AppName) of
                         ok ->
                             {true, {app, AppName, [{incl_cond, InclCond}]}};
                         {error, {already_loaded,AppName}}->
                             {true, {app, AppName, [{incl_cond, InclCond}]}};
                         {error, Reason} when InclCond =:= include ->
                             MA(AppName, Reason);
                         {error, _} ->
                             false
                     end
             end,
             AppConds).

root_dir(Opts) ->
    case lists:keyfind("--root_dir", 1, Opts) of
        {_, []}        -> {false, code:root_dir()};
        {_, [RootDir]} -> {true, RootDir}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Xref
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xref(LuxAppDir, Opts, ThisEscript, RA, MA) ->
    {_RootDir, ReltoolOpts} =
        reltool_opts(LuxAppDir, Opts, ThisEscript, RA, MA),
    process_flag(trap_exit, true),
    case reltool:start_server(ReltoolOpts) of
        {ok, ServerPid} ->
            case reltool:get_status(ServerPid) of
                {ok, _Warnings} ->
                    %% [io:format("WARNING: ~s\n", [W]) || W <- Warnings],
                    {ok, {sys, Sys}} =
                        reltool:get_config(ServerPid, true, true),
                    _StopRes = reltool:stop(ServerPid),
                    try
                        do_xref(Sys)
                    catch
                        _Class:Reason ->
                            Str = lists:flatten(io_lib:format("~p", [Reason])),
                            {error, Str}
                    end;
                {error, StatusReasonStr} ->
                    {error, StatusReasonStr}
            end;
        {error, StartReasonStr} ->
            {error, StartReasonStr}
    end.

do_xref(Sys) ->
    {_, ErtsApps} = lists:keyfind(erts, 1, Sys),
    ExtendedSys = Sys ++ ErtsApps,
    AppEbins =  [{A, filename:join([D, "ebin"])} ||
                    {app, A, O}  <- ExtendedSys,
                    {lib_dir, D} <- O],
    Fun = fun(A, M) ->
                  {_, Ebin} = lists:keyfind(A, 1, AppEbins),
                  {A, M, filename:join([Ebin, atom_to_list(M) ++ ".beam"])}
          end,
    ModFiles =  [Fun(A, M) || {app, A, O} <- ExtendedSys,
                              {mod, M, _} <- O],
    Xref = ?ESCRIPT_MOD,
    {ok, _} = xref:start(Xref),
    %% ok = xref:set_library_path(Xref, LibDirs),
    Defaults = [{warnings,false}, {verbose,false}, {builtins,true}],
    ok = xref:set_default(Xref, Defaults),
    {ok, Cwd} = file:get_cwd(),
    Add =
        fun({_App, _Mod, AbsFile}, AccRes) ->
                RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
                case xref:add_module(Xref, RelFile) of
                    {ok, _} ->
                        AccRes;
                    {error, Callback, Error} ->
                        Chars = lists:flatten(Callback:format_error(Error)),
                        Reason = [C || C <- Chars, C =/= $" ],
                        {error, RelFile, Reason}
                end
        end,
    Res = ok,
    {Res2, EscriptModFile} = xref_add_escript(Sys, Add, Res),
    Res3 = lists:foldl(Add, Res2, ModFiles),
    ModFiles2 = [EscriptModFile | ModFiles],
    Res4 = undefined_function_calls(Xref, ModFiles2, Res3),
    xref:stop(Xref),
    Res4.

xref_add_escript(Sys, Add, OldRes) ->
    {_, EscriptFile, _} = lists:keyfind(escript, 1, Sys),
    {ok, Sections} = escript:extract(EscriptFile, [compile_source]),
    {_, EscriptBeam} = lists:keyfind(source, 1, Sections),
    TmpEscriptFile = atom_to_list(?ESCRIPT_MOD) ++ ".beam",
    EscriptModFile = {?APPLICATION, ?ESCRIPT_MOD, TmpEscriptFile},
    case file:write_file(TmpEscriptFile, EscriptBeam) of
        ok ->
            NewRes = Add(EscriptModFile, OldRes),
            file:delete(TmpEscriptFile),
            {NewRes, EscriptModFile};
        {error, FileReason} ->
            ReasonStr = file:format_error(FileReason),
            {{error, TmpEscriptFile, ReasonStr}, EscriptModFile}
    end.

undefined_function_calls(Xref, ModFiles, OldRes) ->
    {ok, MFAs} = xref:analyze(Xref, undefined_function_calls),
    Fun =
        fun({FromMFA, ToMFA}, AccRes) ->
                case ignore_call(FromMFA, ToMFA, ModFiles) of
                    true ->
                        AccRes;
                    false ->
                        io:format("ERROR: ~s calls undefined function ~s\n",
                                  [mfa(FromMFA), mfa(ToMFA)]),
                        {error, "There are undefined functions"}
                end
        end,
    lists:foldl(Fun, OldRes, MFAs).

ignore_call(FromMFA, ToMFA, ModFiles) ->
    FromMod = element(1, FromMFA),
     FromApp = which_app(FromMod, ModFiles),
    if
        FromMod =:= ?ESCRIPT_MOD ->
            ToMod = element(1, ToMFA),
            lists:member(ToMod, [make]);
        FromMod =:= ?MODULE ->
            ToMod = element(1, ToMFA),
            lists:member(ToMod, [reltool, xref]);
        FromApp =:= ?APPLICATION ->
            false;
        true ->
            true
    end.

which_app(Mod, ModFiles) ->
    {Mod, {App, _, _}, _, _} =
        {Mod, lists:keyfind(Mod, 2, ModFiles), ?ESCRIPT_MOD, ?LINE},
    App.

mfa({M, F, A}) ->
  io_lib:format("~s:~s/~p", [M, F, A]).
