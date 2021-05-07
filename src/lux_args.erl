%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2021 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_args).

-export([
         usage/1,
         specs/0,
         getopts/3,
         translate_opts/1
        ]).

-include("lux.hrl").

-define(FILES, "--").

-spec specs() -> [spec()].
specs() ->
    [
     {"-h",                   ["--help=usage"]},
     {"-r",                   ["--rerun=fail"]},
     {"-c",                   ["--progress=compact"]},
     {"-v",                   ["--progress=verbose"]},
     {"-V",                   ["--progress=etrace"]},
     {"-VV",                  ["--progress=ctrace"]},
     {"-t",                   ["--progress=silent", "--tap=stdout"]},
     {"-d",                   ["--debug"]},
     {"-D",                   ["--internal_debug"]},
     {"-E",                   ["--event_trace"]},
     {"-T",                   ["--suite_trace"]},
     {?FILES,                 "",          string,                 mandatory},
     {"--help",               usage,       {enum, help},           optional},
     {"--version",            false,       boolean,                none},
     {"--gen_markdown",       undefined,   string,                 mandatory},
     {"--pre_markdown",       undefined,   string,                 mandatory},
     {"--internal_debug",     false,       boolean,                none},
     {"--event_trace",        false,       boolean,                none},
     {"--suite_trace",        false,       boolean,                none},
     {"--display_trace",      undefined,   string,                 mandatory},
     {"--filter_trace",       undefined,   string,                 mandatory},
     {"--install",            "",          string,                 optional},
     {"--install_app",        undefined,   string,                 mandatory},
     {"--install_profile",    standalone,  {enum, profile},        mandatory},
     {"--reltool",            false,       boolean,                none},
     {"--xref",               false,       boolean,                none},
     {"--annotate",           false,       string,                 mandatory},
     {"--history",            false,       string,                 mandatory},
     {"--rerun",              enable,      {enum, prio},           mandatory},
     {"--html",               enable,      {enum, html},           mandatory},
     {"--debug",              false,       boolean,                none},
     {"--debug_file",         undefined,   string,                 mandatory},
     {"--skip",               undefined,   string,                 mandatory},
     {"--skip_unless",        undefined,   string,                 mandatory},
     {"--skip_unstable",      false,       boolean,                none},
     {"--skip_skip",          false,       boolean,                none},
     {"--fail_when_warning",  false,       boolean,                none},
     {"--require",            undefined,   string,                 mandatory},
     {"--case_prefix",        undefined,   string,                 mandatory},
     {"--log_dir",            undefined,   string,                 mandatory},
     {"--config_dir",         undefined,   string,                 mandatory},
     {"--config_name",        undefined,   string,                 mandatory},
     {"--suite",              undefined,   string,                 mandatory},
     {"--run",                undefined,   string,                 mandatory},
     {"--extend_run",         false,       boolean,                none},
     {"--revision",           undefined,   string,                 mandatory},
     {"--hostname",           undefined,   string,                 mandatory},
     {"--mode",               execute,     {enum, mode},           mandatory},
     {"--doc",                infinity,    {integer, 0, infinity}, optional},
     {"--progress",           brief,       {enum, progress},       mandatory},
     {"--multiplier",         1000,        {integer, 0, infinity}, mandatory},
     {"--suite_timeout",      infinity,    {integer, 0, infinity}, mandatory},
     {"--case_timeout",       5*60*1000,   {integer, 0, infinity}, mandatory},
     {"--flush_timeout",      1000,        {integer, 0, infinity}, mandatory},
     {"--poll_timeout",       100,         {integer, -1, infinity},mandatory},
     {"--timeout",            10*1000,     {integer, 0, infinity}, mandatory},
     {"--cleanup_timeout",    100*1000,    {integer, 0, infinity}, mandatory},
     {"--risky_threshold",    0.85,        {float, 0.0, infinity}, mandatory},
     {"--sloppy_threshold",   0.000000001, {float, 0.0, infinity}, mandatory},
     {"--newshell"     ,      false,       boolean,                none},
     {"--shell_wrapper",      "",          string,                 optional},
     {"--shell_wrapper_mode", silent,      {enum, wrapper_mode},   mandatory},
     {"--shell_cmd",          "/bin/sh",   string,                 mandatory},
     {"--shell_args",         ["-i"],      string,                 mandatory},
     {"--shell_prompt_cmd",   "export PS1=SH-PROMPT:",
      string,                 mandatory},
     {"--shell_prompt_regexp","^SH-PROMPT:",
      string,                 mandatory},
     {"--post_cleanup_cmd",   undefined,   string,                 optional},
     {"--file_pattern", ".*\.lux" ++ [$$], string,                 mandatory},
     {"--root_dir",           undefined,   string,                 mandatory},
     {"--var",                undefined,   string,                 mandatory},
     {"--tap",                undefined,   string,                 mandatory},
     {"--junit",              false,       boolean,                none}
    ].

enum(profile) ->
    [standalone, development, embedded];
enum(mode) ->
    [list, list_dir, doc, validate, dump, expand, execute];
enum(wrapper_mode) ->
    [silent, debug, trace];
enum(prio) ->
    [enable, success, skip, warning, fail, error, disable];
enum(html) ->
    [validate | enum(prio)];
enum(progress) ->
    [silent, summary, brief, doc, compact, verbose, etrace, ctrace];
enum(help) ->
    [usage, info, app_dir].

enum_usage(Enum) ->
    Atoms = enum(Enum),
    Strings = [atom_to_list(A) || A <- Atoms],
    "enum(" ++ string:join(Strings, "|") ++ ")".

enum_value(Enum, Val) when is_list(Val) ->
    enum_value(Enum, ?l2a(Val));
enum_value(Enum, Val) when is_atom(Val) ->
    Vals = enum(Enum),
    case lists:member(Val, Vals) of
        true  -> Val;
        false -> badarg(Val, Enum)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Usage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

usage(LuxAppDir) ->
    do_usage(LuxAppDir),
    1.

do_usage(LuxAppDir) ->
    Specs0 = specs(),
    Specs = [{"--make", false, boolean, none}] ++ Specs0,
    io:format("usage: ~p [OPTION]... [FILE]...\n", [?ESCRIPT_MOD]),
    P = fun(Atom) -> [H | T] = atom_to_list(Atom), [string:to_upper(H) | T] end,
    PrettyType = fun(Type) ->
                         case Type of
                             {enum, Enum}    -> enum_usage(Enum);
                             {T, _Min, _Max} -> P(T);
                             T               -> P(T)
                         end
                 end,
    Pretty = fun(Spec) ->
                     case Spec of
                         {Short, Opts} -> % alias
                             Long = lists:flatten([[O, " "] || O <- Opts]),
                             io:format("\t~s short for ~s\n", [Short, Long]);
                         {Name, _Default, Type, mandatory} ->
                             io:format("\t~s ~s\n", [Name, PrettyType(Type)]);
                         {Name, _Default, Type, optional} ->
                             io:format("\t~s [~s]\n", [Name, PrettyType(Type)]);
                         {Name, _Default, _Type, none} ->
                             io:format("\t~s\n", [Name])
                     end
             end,
    [Pretty(S) || S <- Specs, element(1, S) =/= ?FILES],
    info_doc(LuxAppDir).

info_doc(LuxAppDir) ->
    DefaultDir = filename:join([LuxAppDir, "priv", "luxcfg"]),
    io:format("\nThe site default configuration file is located here:"
              "\n\n\t ~s\n",
              [DefaultDir]),
    ExampDir = filename:join([LuxAppDir, "examples"]),
    case filelib:is_dir(ExampDir) of
        true ->
            io:format("\nThe examples are installed here:"
                      "\n\n\t ~s\n",
                      [ExampDir]);
        false ->
            io:format("\nThere are no examples installed.\n", [])
    end,
    io:format("\nRead the full documentation of Lux (version ~p)"
              " in the browser:"
              "\n\n\t~s\n",
              [lux_utils:version(), doc_url(LuxAppDir)]).

doc_url(AppDir) ->
    UsersGuide = filename:join([AppDir, "lux.html"]),
    "file://" ++ filename:absname(UsersGuide).

translate_opts([
     {?FILES,                 Files},
     {"--help",               _Help},
     {"--version",            _Version},
     {"--gen_markdown",       _GenMarkdown},
     {"--pre_markdown",       _PreMarkdown},
     {"--internal_debug",     _InternalDebug},
     {"--event_trace",        _EventTrace},
     {"--suite_trace",        _SuiteTrace},
     {"--display_trace",      _DisplayTrace},
     {"--filter_trace",       _FilterTrace},
     {"--install",            _InstallDir},
     {"--install_app",        _InstallApp},
     {"--install_profile",    _InstallProfile},
     {"--reltool",            _Reltool},
     {"--xref",               _Xref},
     {"--annotate",           _LogFile},
     {"--history",            _LogDir},
     {"--rerun",              ReRun},
     {"--html",               Html},
     {"--debug",              Debug},
     {"--debug_file",         DebugFile},
     {"--skip",               Skip},
     {"--skip_unless",        SkipUnless},
     {"--skip_unstable",      SkipUnstable},
     {"--skip_skip",          SkipSkip},
     {"--fail_when_warning",  FailWhenWarning},
     {"--require",            Require},
     {"--case_prefix",        CasePrefix},
     {"--log_dir",            LogDir},
     {"--config_dir",         ConfigDir},
     {"--config_name",        ConfigName},
     {"--suite",              Suite},
     {"--run",                Run},
     {"--extend_run",         ExtendRun},
     {"--revision",           Revision},
     {"--hostname",           Hostname},
     {"--mode",               Mode},
     {"--doc",                Doc},
     {"--progress",           Progress},
     {"--multiplier",         Multiplier},
     {"--suite_timeout",      SuiteTimeout},
     {"--case_timeout",       CaseTimeout},
     {"--flush_timeout",      FlushTimeout},
     {"--poll_timeout",       PollTimeout},
     {"--timeout",            ExpectTimeout},
     {"--cleanup_timeout",    CleanupTimeout},
     {"--risky_threshold",    RiskyThreshold},
     {"--sloppy_threshold",   SloppyThreshold},
     {"--newshell",           NewShell},
     {"--shell_wrapper",      ShellWrapper},
     {"--shell_wrapper_mode", ShellWrapperMode},
     {"--shell_cmd",          ShellCmd},
     {"--shell_args",         ShellArgs},
     {"--shell_prompt_cmd",   ShellPromptCmd},
     {"--shell_prompt_regexp",ShellPromptRegexp},
     {"--post_cleanup_cmd",   PostCleanupCmd},
     {"--file_pattern",       FilePattern},
     {"--root_dir",           _RootDir},
     {"--var",                Var},
     {"--tap",                Tap},
     {"--junit",              Junit}
    ]) ->
    Mode2 =
        if
            Doc =/= [] -> [doc];
            true       -> Mode
        end,
    Opts =
        [
         {debug,              Debug},
         {debug_file,         DebugFile},
         {progress,           Progress},
         {rerun,              ReRun},
         {html,               Html},
         {case_timeout,       CaseTimeout},
         {flush_timeout,      FlushTimeout},
         {poll_timeout,       PollTimeout},
         {timeout,            ExpectTimeout},
         {cleanup_timeout,    CleanupTimeout},
         {risky_threshold,    RiskyThreshold},
         {sloppy_threshold,   SloppyThreshold},
         {newshell,           NewShell},
         {shell_wrapper,      ShellWrapper},
         {shell_wrapper_mode, ShellWrapperMode},
         {shell_cmd,          ShellCmd},
         {shell_args,         ShellArgs},
         {shell_prompt_cmd,   ShellPromptCmd},
         {shell_prompt_regexp,ShellPromptRegexp},
         {post_cleanup_cmd,   PostCleanupCmd},
         {multiplier,         Multiplier},
         {suite_timeout,      SuiteTimeout},
         {log_dir,            LogDir},
         {skip,               Skip},
         {skip_unless,        SkipUnless},
         {skip_unstable,      SkipUnstable},
         {skip_skip,          SkipSkip},
         {fail_when_warning,  FailWhenWarning},
         {require,            Require},
         {case_prefix,        CasePrefix},
         {var,                Var},
         {mode,               Mode2},
         {doc,                Doc},
         {config_dir,         ConfigDir},
         {config_name,        ConfigName},
         {suite,              Suite},
         {run,                Run},
         {extend_run,         ExtendRun},
         {revision,           Revision},
         {hostname,           Hostname},
         {file_pattern,       FilePattern},
         {tap,                Tap},
         {junit,              Junit}
        ],
    {Files, lux_suite:args_to_opts(Opts, suite_style, [])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generic option handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type opt()      :: string() .
-type name()     :: string().
-type default()  :: term().
-type type()     :: string | binary |
                    atom | existing_atom |
                    integer | {integer, min(), max()} |
                    float | {float, min(), max()} |
                    boolean | validate().
-type min()      :: integer().
-type max()      :: integer() | infinity.
-type validate() :: fun((opt() | default()) -> val()).
-type presence() :: none | optional | mandatory.
-type spec()     :: {name(), [opt()]} | % alias
                    {name(), default(), type(), presence()}.
-type val()      :: term().
-spec getopts([opt()], string(), string()) -> [[val()]].

%% Parse options and validate their values.
%% All options in the specs are returned and they keep that order.
getopts(Args, LuxAppDir, ThisEscript) ->
    EscriptBase = filename:basename(ThisEscript),
    try
        Specs = specs(),
        Opts = do_getopts(Args, Specs, []),
        expand_opts(Opts, Specs)
    catch
        throw:{badarg, Bad} ->
            io:format("~s: ~p is an illegal argument.\n\n",
                      [EscriptBase, Bad]),
            ExitCode = usage(LuxAppDir),
            safe_halt(ExitCode);
        throw:{badarg, Bad, Type} ->
            io:format("~s: ~p is an illegal argument. ~p expected.\n\n",
                      [EscriptBase, Bad, Type]),
            ExitCode = usage(LuxAppDir),
            safe_halt(ExitCode);
        throw:{badarg, Name, Bad, Type} ->
            io:format("~s: ~p is an illegal value of argument ~s."
                      " ~p expected.\n\n",
                      [EscriptBase, Bad, Name, Type]),
            ExitCode = usage(LuxAppDir),
            safe_halt(ExitCode)
    end.

safe_halt(RetCode) ->
    throw({safe_halt, RetCode}).

do_getopts([], _Specs, Acc) ->
    lists:reverse(Acc);
do_getopts(Args, Specs, Acc) ->
    {MoreArgs, Name, Val, Type, Presence} = getopt(Args, Specs),
    Val2 = validate(Name, Val, Type, Presence),
    do_getopts(MoreArgs, Specs, [{Name, Val2} | Acc]).

getopt([?FILES | Args], _Specs) ->
    {[], ?FILES, Args, string, optional};
getopt([Arg | Args], Specs) when hd(Arg) =:= $- ->
    {Val, Spec} = find_spec(Arg, Specs),
    case Spec of
        {_Short, Long} ->
            getopt(Long ++ Args, Specs);
        {Name, Default, Type, Presence} ->
            case {Presence, Val, Args} of
                {none, undefined, _} when Type =:= boolean ->
                    %% boolean - no value, return opposite of default
                    {Args, Name, not Default, Type, Presence};
                {none, "true", _} when Type =:= boolean ->
                    %% boolean - got name=true
                    {Args, Name, true, Type, Presence};
                {none, "false", _} when Type =:= boolean ->
                    %% boolean - got name=false
                    {Args, Name, false, Type, Presence};
                {none, _, _} ->
                    %% boolean - got -name=val
                    badarg(Arg, Type);
                {mandatory, undefined, []} ->
                    %% val required - no more options
                    badarg(Arg, Type);
                {mandatory, undefined, ["-" ++ _NextArg | _NextArgs]} ->
                    %% val required - next option is not a val
                    badarg(Arg, Type);
                {mandatory, undefined, [NextVal | NextArgs]} ->
                    %% val required - return separate val
                    {NextArgs, Name, NextVal, Type, Presence};
                {mandatory, _, _} ->
                    %% val required - got name=val
                    {Args, Name, Val, Type, Presence};
                {optional, undefined, []} ->
                    %% optional val - return default val
                    {Args, Name, Default, Type, Presence};
                {optional, undefined, ["-" ++ _NextArg | _NextArgs]} ->
                    %% optional val - return default val
                    {Args, Name, Default, Type, Presence};
                {optional, undefined, [NextVal | NextArgs]} ->
                    %% optional val - got name=val
                    {NextArgs, Name, NextVal, Type, Presence};
                {optional, _, _} ->
                    %% optional val - return default val
                    {Args, Name, Val, Type, Presence}
            end
    end;
getopt(Args, _Specs) ->
    {[], ?FILES, Args, string, optional}.

expand_opts(Opts, Specs) ->
    Fun = fun({_Short, _Long}, Acc) -> % alias
                  Acc;
             ({Name, _Default, _Type, _Presence}, Acc) ->
                  Vals = proplists:get_all_values(Name, Opts),
                  Vals2 =
                      if
                          Name =:= ?FILES ->
                              lists:append(Vals);
                          true ->
                              Vals
                      end,
                     [{Name, Vals2} | Acc]
          end,
    lists:reverse(lists:foldl(Fun, [], Specs)).

validate(Name, Val, Type, Presence) ->
    try
        case Type of
            string when is_list(Val) ->
                Val;
            binary ->
                ?l2b(Val);
            atom ->
                ?l2a(Val);
            existing_atom ->
                list_to_existing_atom(Val);
            integer ->
                list_to_integer(Val);
            {integer, _Min, infinity} when Val =:= "infinity" ->
                infinity;
            {integer, Min, infinity} when Val >= Min ->
                list_to_integer(Val);
            {integer, Min, Max} when Val >= Min, Val =< Max ->
                list_to_integer(Val);
            float ->
                list_to_float(Val);
            {float, _Min, infinity} when Val =:= "infinity" ->
                infinity;
            {float, Min, infinity} when Val >= Min ->
                list_to_float(Val);
            {float, Min, Max} when Val >= Min, Val =< Max ->
                list_to_float(Val);
            boolean when Val =:= true; Val =:= "true" ->
                true;
            boolean when Val =:= false; Val =:= "false" ->
                false;
            {enum, Enum} ->
                enum_value(Enum, Val);
            _ when Val =:= undefined, Presence =/= mandatory ->
                Val
        end
    catch
        _Class:_Reason ->
            badarg(Name, Val, Type)
    end.

find_spec(Opt, Specs) ->
    {Name, Val} =
        case lists:prefix("--", Opt) of
            true ->
                %% --name=val
                Eq = $=,
                case lists:splitwith(fun(Char) -> Char =/= Eq end, Opt) of
                    {N, [Eq | V]} -> {N, V};
                    {N, []}       -> {N, undefined}
                end;
            false ->
                {Opt, undefined}
        end,
    %% io:format("Option ~p: ~p -> ~p\n~p\n", [Opt, Name, Val, Specs]),
    case lists:keyfind(Name, 1, Specs) of
        false -> badarg(Opt);
        Spec  -> {Val, Spec}
    end.

badarg(Val) ->
    do_throw({badarg, Val}).

badarg(Val, {enum, Enum}) ->
    do_throw({badarg, Val, enum_usage(Enum)});
badarg(Val, Type) ->
    do_throw({badarg, Val, Type}).

badarg(Name, Val, {enum, Enum}) ->
    do_throw({badarg, Name, Val, enum_usage(Enum)});
badarg(Name, Val, Type) ->
    do_throw({badarg, Name, Val, Type}).

do_throw(Reason) ->
    throw(Reason).
