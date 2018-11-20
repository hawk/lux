%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2018 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_debug).

-export([
         start_link/1,
         format/2,
         eval_cmd/4,
         cmd_attach/3,
         check_breakpoint/2,
         gen_markdown/1
        ]).

-include("lux.hrl").
-include_lib("kernel/include/file.hrl").

-record(dstate,
        {n_cmds          :: non_neg_integer(),
         mode            :: background | foreground,
         interpreter_pid :: pid(),
         shell_pid       :: undfined | pid(),
         shell_name      :: undfined | string(),
         prev_cmd        :: string(),
         cmd_state       :: term()}).

-type debug_type() :: 'integer' |
                      {'integer', integer(), integer() | infinity} |
                      'string' |
                      'lineno'.
-record(debug_param,
        {name     :: string(),
         type     :: debug_type(),
         presence :: [mandatory | optional],
         help     :: string()}).

-type debug_fun() :: fun((#istate{}, [term()], term()) -> {term(), #istate{}}).

-record(debug_cmd,
        {name     :: string(),
         params   :: [debug_type()],
         help     :: string(),
         callback :: debug_fun()}).

format(Fmt, Args) ->
    io:format(Fmt, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read commands from stdin and communicate with interpreter

start_link(DebugFile) ->
    Parent = self(),
    spawn_link(fun() -> init(Parent, DebugFile) end).

init(Ipid, DebugFile) ->
    DefaultCmd = "help",
    Dstate = #dstate{n_cmds = 1,
                     mode = background,
                     interpreter_pid=Ipid,
                     prev_cmd = DefaultCmd,
                     cmd_state=undefined},
    case DebugFile of
        undefined ->
            loop(Dstate);
        DebugFile ->
            LoadCmd = "load " ++ DebugFile,
            NewDstate = call(Dstate, LoadCmd),
            loop(NewDstate#dstate{prev_cmd=DefaultCmd})
    end.

loop(#dstate{mode=Mode} = Dstate) ->
    N = Dstate#dstate.n_cmds,
    case io:get_line("") of
        eof when N =:= 1 ->
            %% Closed already at startup
            exit(normal);
        eof ->
            catch format("\nEOF: stdin closed\n", []),
            exit(normal);
        {error, Reason} ->
            ReasonStr = file:format_error(Reason),
            catch format("\nERROR: ~s\n", [ReasonStr]),
            exit(Reason);
        "\"\"\"\n" when Mode =:= foreground->
            %% Found """. Exit foreground mode
            Name = Dstate#dstate.shell_name,
            NewDstate = call(Dstate, "shell " ++ Name),
            loop(NewDstate);
        Data when Mode =:= foreground->
            Bin = ?l2b(Data),
            Dpid = Dstate#dstate.interpreter_pid,
            Dstate#dstate.shell_pid ! {debug_shell, Dpid, {send,Bin}},
            loop(Dstate);
        Data when Mode =:= background ->
            [$\n | Rest] = lists:reverse(Data),
            ChoppedCmdStr = lists:reverse(Rest),
            CmdStr =
                case string:tokens(ChoppedCmdStr, " ") of
                    [] when ChoppedCmdStr =:= "" ->
                        %% Repeat previous command
                        Dstate#dstate.prev_cmd;
                    _ ->
                        %% Execute new command
                        ChoppedCmdStr
                end,
            Dstate2 = call(Dstate, CmdStr),
            loop(Dstate2#dstate{n_cmds=N+1, prev_cmd=CmdStr})
    end.

call(Dstate0, Cmd) when is_list(Cmd) -> % ; is_function(Cmd, 2) ->
    %% format("DEBUG: ~p\n", [CmdStr]),
    Ipid = Dstate0#dstate.interpreter_pid,
    Dstate = flush_replies(Dstate0, Ipid),
    Dstate#dstate.interpreter_pid !
        {debug_call, self(), Cmd, Dstate#dstate.cmd_state},
    wait_for_reply(Dstate, Ipid, 5000).


flush_replies(Dstate, Ipid) ->
    case wait_for_reply(Dstate, Ipid, 0) of
        flushed ->
            Dstate;
        NewDstate ->
            flush_replies(NewDstate, Ipid)
    end.

wait_for_reply(Dstate, Ipid, Timeout) ->
    receive
        {debug_reply, Ipid, CmdStr, NewCmdState, Dshell} ->
            case Dshell of
                undefined ->
                    ShellName = undefined,
                    Mode = background,
                    ShellPid = undefined;
                #debug_shell{name=ShellName,
                             mode=Mode,
                             pid=ShellPid} ->
                    ok
            end,
            Dstate#dstate{n_cmds=Dstate#dstate.n_cmds+1,
                          mode=Mode,
                          shell_pid=ShellPid,
                          shell_name=ShellName,
                          prev_cmd=CmdStr,
                          cmd_state=NewCmdState}
    after Timeout ->
            if
                Timeout =:= 0 ->
                    flushed;
                is_integer(Timeout) ->
                    %% Display process info for interpreter and its children
                    Dpid = self(),
                    format("\n<LUX WARNING>"
                           " Debugger timed out after ~p milli seconds.\n",
                           [Timeout]),
                    format("Debugger pid: ~p\n", [Dpid]),
                    format("Interpreter pid: ~p\n", [Ipid]),
                    Item = [current_stacktrace, messages],
                    Show =
                        fun(Pid) ->
                                Info = process_info(Pid, Item),
                                format("Proc info for ~p:\n\t~p\n", [Pid, Info])
                        end,
                    Pids = [P || P <- processes(), P > Ipid],
                    AllPids = [Ipid | Pids],
                    lists:foreach(Show, AllPids),
                    wait_for_reply(Dstate, Ipid, infinity)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse and evaluate one command

eval_cmd(I, Dpid, CmdStr, CmdState) when Dpid =:= I#istate.debug_pid ->
    case I#istate.debug_shell of
        undefined ->
            {CmdState2, I2} = do_eval_cmd(I, CmdStr, CmdState),
            Dshell = I2#istate.debug_shell,
            Dpid ! {debug_reply, self(), CmdStr, CmdState2, Dshell},
            I2;
        #debug_shell{name=Name, mode=Mode, pid=ShellPid} ->
            case CmdStr of
                [$! | Rest] when Mode =:= background ->
                    io:format("\nSend data to shell ~p.\n", [Name]),
                    Bin = ?l2b([Rest, "\n"]),
                    ShellPid ! {debug_shell, Dpid, {send,Bin}},
                    Dshell = I#istate.debug_shell,
                    Dpid ! {debug_reply, self(), CmdStr, CmdState, Dshell},
                    I;
                [$~ | Rest] when Mode =:= background ->
                    io:format("\nSend data to shell ~p.\n", [Name]),
                    Bin = ?l2b(Rest),
                    ShellPid ! {debug_shell, Dpid, {send,Bin}},
                    Dshell = I#istate.debug_shell,
                    Dpid ! {debug_reply, self(), CmdStr, CmdState, Dshell},
                    I;
                "?" when Mode =:= background ->
                    io:format("\nReset output buffer for shell ~p.\n", [Name]),
                    ShellPid ! {debug_shell, Dpid, {send,reset}},
                    Dshell = I#istate.debug_shell,
                    Dpid ! {debug_reply, self(), CmdStr, CmdState, Dshell},
                    I;
                "=" when Mode =:= background ->
                    ShellPid ! {debug_shell, Dpid, display_stdout},
                    Dshell = I#istate.debug_shell,
                    Dpid ! {debug_reply, self(), CmdStr, CmdState, Dshell},
                    I;
                CmdStr2 ->
                    {CmdState2, I2} = do_eval_cmd(I, CmdStr2, CmdState),
                    Dshell = I2#istate.debug_shell,
                    Dpid ! {debug_reply, self(), CmdStr2, CmdState2, Dshell},
                    I2
            end
    end.

do_eval_cmd(I, CmdStr, CmdState) when is_list(CmdStr) ->
    case string:tokens(CmdStr, " ") of
        [] ->
            %% Ignore empty command
            {CmdState, I};
        [CmdName | Args] ->
            case select(I#istate.debug_shell, CmdName) of
                {ok, #debug_cmd{name = _Name,
                                params = Params,
                                callback = Fun}} ->
                    case parse_params(I, Params, Args, [], []) of
                        {ok, Args2} ->
                            %% format("Eval: ~s ~p\n", [_Name, Args2]),
                            Fun(I, Args2, CmdState);
                        {error, ReasonStr} ->
                            format("\nERROR: ~s: ~s\n",
                                      [CmdName, ReasonStr]),
                            {CmdState, I}
                    end;
                {error, ReasonStr} ->
                    format("\nERROR: ~s\n", [ReasonStr]),
                    {CmdState, I}
            end
    end;
do_eval_cmd(I, Cmd, CmdState) when is_function(Cmd, 2) ->
    Cmd(CmdState, I).

select(DS, CmdStr) ->
    NamedCmds = [{C#debug_cmd.name, C} || C <- cmds()],
    do_select(DS, CmdStr, NamedCmds, CmdStr, NamedCmds).

do_select(DS, [H | T], NamedCmds, Orig, Prev) ->
    case select_first(H, NamedCmds, []) of
        [] ->
            {error,  ambiguous(DS, Orig, Prev)};
        [{_, Cmd}] ->
            case lists:prefix(Orig, Cmd#debug_cmd.name) of
                true ->
                    {ok, Cmd};
                false ->
                    {error,  ambiguous(DS, Orig, Prev)}
            end;
        Ambiguous ->
            do_select(DS, T, Ambiguous, Orig, NamedCmds)
    end;
do_select(_DS, [], [{_, Cmd}], _Orig, _Prev) ->
    {ok, Cmd};
do_select(DS, [], [], Orig, Prev) ->
    {error, ambiguous(DS, Orig, Prev)};
do_select(DS, [], Ambiguous, Orig, _Prev) ->
    {error,  ambiguous(DS, Orig, Ambiguous)}.

select_first(Char, [{[Char | Chars], Cmd} | NamedCmds], Acc) ->
    select_first(Char, NamedCmds, [{Chars, Cmd} | Acc]);
select_first(Char, [{_Chars, _Cmd} | NamedCmds], Acc) ->
    select_first(Char, NamedCmds, Acc);
select_first(_Char, [], Acc) ->
    lists:reverse(Acc).

ambiguous(DS, Orig, NamedCmds) ->
    Longest = longest(NamedCmds),
    DeepList = lists:map(fun(NC) -> format_cmd_list(NC, Longest) end,
                         NamedCmds),
    OptSubCmds =
        case DS of
            undefined -> [];
            _         -> ["\n", format_shell_sub_cmds()]
        end,
    lists:flatten(["Available commands: ", Orig, "\n",
                   "-------------------\n",
                   DeepList,
                   OptSubCmds]).

format_cmd_list({_, #debug_cmd{name = Name, help = Help}}, Longest) ->
    format_cmd_list({Name, Help}, Longest);
format_cmd_list({Name, Help}, Longest) when is_list(Name), is_list(Help) ->
    Slogan = lists:takewhile(fun(Char) -> Char =/= $\n end, Help),
    ["* ", string:left(Name, Longest, $\ ), " - ", Slogan, "\n"].

parse_params(I, [#debug_param{name = Name,type = Type} | Params],
             [Arg | Args], Acc, _Bad) ->

    try
        Val = parse_param(I, Type, Arg),
        parse_params(I, Params, Args, [{Name, Val} | Acc], [])
    catch
        error:_ ->
            Type2 =
                if
                    is_atom(Type) -> ?a2l(Type);
                    true          -> ?a2l(element(1, Type))
                end,
            ReasonStr = lists:flatten(["Bad type of parameter ", Name,
                                       ". Expected ", Type2]),
            {error, ReasonStr}
    end;
parse_params(_I, Params, [], Acc, Bad) ->
    case [P || P <- Params, P#debug_param.presence =/= optional] of
        [] ->
            {ok, lists:reverse(Acc)};
        Mandatory ->
            All = Bad ++ Mandatory,
            Longest = longest(All),
            DeepList = ["Missing parameter.\nPossible parameters:\n",
                        [pretty_param(P, Longest) || P <- All]],
            {error, lists:flatten(DeepList)}
    end;
parse_params(_I, [], [Arg | _], _Acc, Bad) ->
    Longest = longest(Bad),
    DeepList = ["Value ", Arg, " has the wrong type.\nPossible parameters:\n",
                [pretty_param(P, Longest) || P <- Bad]],
    {error, lists:flatten(DeepList)}.

parse_param(I, Type, Val) ->
    case Type of
        lineno when is_list(Val), Val =/= [] ->
            parse_lineno(I, Val);
        string when is_list(Val) ->
            Val;
        {enum, List} when is_list(List) ->
            [Single] = [Elem || Elem <- List, lists:prefix(Val, Elem)],
            Single;
        binary ->
            ?l2b(Val);
        atom ->
            list_to_atom(Val);
        existing_atom ->
            list_to_existing_atom(Val);
        integer ->
            list_to_integer(Val);
        {integer, _Min, infinity} when Val =:= "infinity" ->
            999999;
        {integer, Min, infinity} ->
            Int = list_to_integer(Val),
            if
                Int >= Min -> Int
            end;
        {integer, Min, Max} ->
            Int = list_to_integer(Val),
            if
                Int >= Min, Int =< Max -> Int
            end;
        float ->
            list_to_float(Val);
        {float, _Min, infinity} when Val =:= "infinity" ->
            999999.0;
        {float, Min, infinity} ->
            Float = list_to_float(Val),
            if
                Float >= Min -> Float
            end;
        {float, Min, Max} ->
            Float = list_to_float(Val),
            if
                Float >= Min, Float =< Max -> Float
            end;
        boolean when Val =:= true; Val =:= "true" ->
            true;
        boolean when Val =:= false; Val =:= "false" ->
            false
    end.

parse_lineno(I, Val) ->
    %% break_pos()   :: static() | dynamic()
    %% static_pos()  :: {[string()], non_neg_integer()}
    %% dynamic_pos() :: [non_neg_integer()]
    StaticType = {integer, 1, infinity},
    case split(Val, ":") of
        [Int] when hd(Int) >= $1, hd(Int) =< $9 ->
            %% Static - current:int
            {lux_utils:filename_split(I#istate.file),
             parse_param(I, StaticType, Int)};
        [File] when hd(File) < $1; hd(File) > $9 ->
            %% Static - file:1
            {lux_utils:filename_split(File), 1};
        ["", Int] ->
            %% Static - main:int
            {lux_utils:filename_split(I#istate.orig_file),
             parse_param(I, StaticType, Int)};
        [File, Int] when hd(File) < $1; hd(File) > $9 ->
            %% Static file:int
            {lux_utils:filename_split(File),
             parse_param(I, StaticType, Int)};
        Dynamic ->
            %% integer [ ":" integer ] +
            DynamicType = integer,
            Parse =
                fun(Int) ->
                        parse_param(I, DynamicType, Int)
                end,
            lists:reverse(lists:map(Parse, Dynamic))
    end.

split(Str, Seps) when is_list(Str), is_list(Seps) ->
    [?b2l(Token) ||
        Token <- binary:split(?l2b(Str), ?l2b(Seps), [global])].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Specification of all available debugger commands

cmds() ->
    [
     #debug_cmd{name = "attach",
                params = [],
                help = "attach to script and pause its execution",
                callback = fun cmd_attach/3},
     #debug_cmd{name = "break",
                params = [#debug_param{name = "lineno",
                                       type = lineno,
                                       presence = optional,
                                       help = "lineno in source file"},
                          #debug_param{name = "duration",
                                       type = {enum, ["normal",
                                                      "temporary",
                                                      "delete",
                                                      "skip"]},
                                       presence = optional,
                                       help = "controls the duration of "
                                       "the breakpoint"}],
                help = "set, delete and list breakpoints\n\n"
                "When a breakpoint is set it may either be normal (default)\n"
                "or temporary. The difference between them is that normal\n"
                "breakpoints remains after the break has been triggered,\n"
                "while temporary breakpoints are automatically deleted when\n"
                "they have been triggered once. delete is used to immediately\n"
                "remove the breakpoint.\n\n"
                "Without parameters, all breakpoints are listed.\n",
                callback = fun cmd_break/3},
     #debug_cmd{name = "continue",
                params = [#debug_param{name = "lineno",
                                       type = lineno,
                                       presence = optional,
                                       help = "run to temporary breakpoint"
                                       " at lineno"}],
                help = "continue script execution",
                callback = fun cmd_continue/3},
     #debug_cmd{name = "help",
                params = [#debug_param{name = "command",
                                       type = string,
                                       presence = optional,
                                       help = "debugger command"}],
                help = "display description of a command",
                callback = fun cmd_help/3},
     #debug_cmd{name = "list",
                params = [#debug_param{name = "n_lines",
                                       type = {integer, 1, infinity},
                                       presence = optional,
                                       help = "number of lines"},
                          #debug_param{name = "lineno",
                                       type = lineno,
                                       presence = optional,
                                       help = "start listing at lineno"}],
                help = "list script source\n\n"
                "If no \"lineno\" is given, the listing will start from the\n"
                "current line or from the latest given \"lineno\" if no other\n"
                "commands have been given in between.",
                callback = fun cmd_list/3},
     #debug_cmd{name = "load",
                params = [#debug_param{name = "file",
                                       type = string,
                                       presence = optional,
                                       help = "file name. Default is "
                                       "\"./lux.debug\"."}],
                help = "load file with debug commands",
                callback = fun cmd_load/3},
     #debug_cmd{name = "next",
                params = [],
                help = "execute next command\n"
                "A multi-line command counts as one command.",
                callback = fun cmd_next/3},
     #debug_cmd{name = "progress",
                params = [#debug_param{name = "level",
                                       type = {enum, ["silent",
                                                      "summary", "brief",
                                                      "doc",
                                                      "compact", "verbose",
                                                      "etrace", "ctrace"]},
                                       presence = optional,
                                       help = "verbosity level. "
                                       "Toggle between brief and "
                                       "verbose by default."}],
                help = "set verbosity level of progress",
                callback = fun cmd_progress/3},
     #debug_cmd{name = "quit",
                params = [#debug_param{name = "scope",
                                       type = {enum, ["case","suite"]},
                                       presence = optional,
                                       help = "scope of exit"}],
                help = "quit a single test case or the entire test suite\n"
                       "in a controlled manner. Runs cleanup if applicable.",
                callback = fun cmd_quit/3},
     #debug_cmd{name = "save",
                params = [#debug_param{name = "file",
                                       type = string,
                                       presence = optional,
                                       help = "file name. Default is "
                                       "\"lux.debug\"."}],
                help = "save debug state to file",
                callback = fun cmd_save/3},
     #debug_cmd{name = "skip",
                params = [#debug_param{name = "lineno",
                                       type = lineno,
                                       presence = optional,
                                       help = "lineno in source file"}],
                help = "skip execution of one or more commands\n"
                "Skip until given lineno is reached.",
                callback = fun cmd_skip/3},
     #debug_cmd{name = "shell",
                params = [#debug_param{name = "name",
                                       type = string,
                                       presence = optional,
                                       help = "name of shell"},
                          #debug_param{name = "mode",
                                       type = {enum, ["background",
                                                      "foreground"]},
                                       presence = optional,
                                       help = "mode of operation"}],

                help = "connect to a shell\n\n"
                "With no argument, the names of the shells will be listed.\n"
                "In the listing the active shell is preceeded by an arrow\n"
                "and zombie shells with an star. Repeating the command\n"
                "will disconnect the shell. Repeat again to connect...\n"
                "\n"
                "Once a shell is connected, its stdout will be tapped and\n"
                "subsequent output will be printed out, beginning with the\n"
                "buffered (non-processed) data.\n"
                "\n"
                "Data can also be sent to the stdin of the shell. In\n"
                "foreground mode all entered text is sent as is to the\n"
                "shell. The foreground mode is exited with a single \"\"\"\"\n"
                "line. In background mode (default), the debugger responds\n"
                "to normal debugger commands as well as a few special\n"
                "commands which only is available in background mode:\n"
                "\n"
                ++ format_shell_sub_cmds(),
                callback = fun cmd_shell/3},
     #debug_cmd{name = "tail",
                params = [#debug_param{name = "index",
                                       type = {integer, 1, infinity},
                                       presence = optional,
                                       help = "log number"},
                          #debug_param{name = "format",
                                       type = {enum, ["compact", "verbose"]},
                                       presence = optional,
                                       help = "display format"},
                          #debug_param{name = "n_lines",
                                       type = {integer, 1, infinity},
                                       presence = optional,
                                       help = "fixed number of lines"}],

                help = "display log files\n\n"
                "With no argument, the names of the log files will be listed.\n"
                "Each one is preceeded by its index number and optionally a\n"
                "star. The star means that the log has been updated since the\n"
                "previous status check. Use the index to display a particular\n"
                "log. Such as \"t 5\" for the event log. Press enter to\n"
                "display more lines. n_lines can be used to override that\n"
                "behavior andonly display a fixed number of lines regardless\n"
                "of the command is repeated or not.",
                callback = fun cmd_tail/3},
     #debug_cmd{name = "TRACE",
                params = [#debug_param{name = "action",
                                       type = {enum, ["START", "STOP"]},
                                       presence = optional,
                                       help = "Trace action"},
                          #debug_param{name = "mode",
                                       type = {enum, ["CASE","SUITE","EVENT"]},
                                       presence = optional,
                                       help = "Trace mode"}],
                help = "start or stop internal tracing\n"
                "Default is to display the trace mode (none|case|suite|event).",
                callback = fun cmd_trace/3}
    ].


format_shell_sub_cmds() ->
    NamedCmds = shell_sub_cmds(),
    Longest = longest(NamedCmds),
    DeepList = lists:map(fun(NC) -> format_cmd_list(NC, Longest) end,
                         NamedCmds),
    lists:flatten(["Sub commands for \"shell\":\n",
                   "-------------------------\n",
                   DeepList]).

shell_sub_cmds() ->
    [
     {"!", "sends text with a trailing newline"},
     {"~", "sends text without a trailing newline"},
     {"=", "displays current output buffer"},
     {"?", "empties the output buffer"}
    ].

intro_help() ->
    "Debugger for Lux scripts\n"
    "========================\n"
    "When `lux` is started with the `--debug` option, the debugger\n"
    "will attach to the script before its execution has started. An\n"
    "optional file with saved commands may be processed at this stage.\n"
    "The debugger can also be attached to the script in the middle of\n"
    "the execution by entering the command \"attach\" (or an abbreviation\n"
    "of the command) and pressing the enter key.\n"
    "\n"
    "Several parameters has a lineno as parameter see `help lineno`.\n"
    "\n"
    "Blank command lines implies that the previous command is repeated.\n"
    "If no command has been entered yet, the command `help` is assumed.\n"
    "\n"
    "Commands may be abbreviated. Use the help command (for example\n"
    "`help help` (or `h h` for short) to get more detailed descriptions\n"
    "of the commands.\n\n".

param_help() ->
    "Available parameters:\n"
    "---------------------\n"
    "* lineno - lineno in source file\n\n".

lineno_help() ->
    "lineno parameter\n"
    "----------------\n"
    "Several commands has a lineno as parameter. It is a string which\n"
    "is divided in several components. The components are separated\n"
    "with a colon and are used to refer to line numbers in include\n"
    "files, macros and loops. The first component is a bit special.\n"
    "It may be a file name or a line number. The file name may be\n"
    "abbreviated.\n"
    "\n"
    "Assume that there is a file called main, which includes a file\n"
    "called outer at line 4 and the file outer includes a file called\n"
    "inner at line 12.\n"
    "\n"
    "Here are a few examples of how lineno can be used:\n"
    "\n"
    "* 3       - line 3 in current file\n"
    "* main    - line 1 in file main\n"
    "* m:3     - line 3 in file main\n"
    "* :3      - line 3 in file main\n"
    "* inner   - line 1 in file inner\n"
    "* outer   - line 1 in file outer\n"
    "* o:12    - line 12 in file outer\n"
    "* 4:12:6  - line 6 in file inner if it is included\n"
    "            on line 12 in outer and outer is included\n"
    "            on line 4 in main.\n\n".

gen_markdown(File) ->
    Intro = intro_help(),
    {error, Ambiguous} = select(undefined, ""),
    Params = param_help(),
    LineNo = lineno_help(),
    Cmds = [["\n", pretty_cmd(Cmd)] ||
               Cmd <- lists:keysort(#debug_cmd.name, cmds())],
    file:write_file(File,
                    [Intro, "\n",
                     Ambiguous, "\n",
                     Params, "\n",
                     LineNo, "\n",
                     Cmds]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_attach(I, _, CmdState) ->
    CurrentFullLineNo = current_full_lineno(I),
    ListOpts =
        case CmdState of
            {attach, next} ->
                BreakPos = full_lineno_to_static_break_pos(CurrentFullLineNo),
                [{"n_lines", 1}, {"lineno", BreakPos}];
            {attach, skip} ->
                BreakPos = full_lineno_to_static_break_pos(CurrentFullLineNo),
                [{"n_lines", 1}, {"lineno", BreakPos}];
            _ ->
                CurrentPos = full_lineno_to_static_break_pos(CurrentFullLineNo),
                format("\nBreak at \"~s\"\n",
                       [pretty_break_pos(CurrentPos)]),
                [#cmd_pos{rev_file = RevFile,
                          lineno = LineNo,
                          type = CmdType} | CmdStack] = CurrentFullLineNo,
                {LineNo2, CmdType2} =
                    if
                        LineNo > 3 ->
                            TmpLineNo = LineNo-2,
                            case lookup_cmd(RevFile, TmpLineNo, I) of
                                false -> {LineNo, CmdType};
                                Cmd   -> {TmpLineNo, Cmd#cmd.type}
                            end;
                        true->
                            {LineNo, CmdType}
                    end,
                CmdPos2 = #cmd_pos{rev_file = RevFile,
                                  lineno = LineNo2,
                                  type = CmdType2},
                FullLineNo = [CmdPos2 | CmdStack],
                BreakPos = full_lineno_to_static_break_pos(FullLineNo),
                [{"n_lines", 10}, {"lineno", BreakPos}]
        end,
    case opt_block(I) of
        {false, I2} -> {undefined, I2};
        {true, I2}  -> cmd_list(I2, ListOpts, CmdState)
    end.

opt_block(I) ->
    if
        I#istate.blocked ->
            {false, I};
        true ->
            lists:foreach(fun(#shell{pid = Pid}) -> Pid ! {block, self()} end,
                          I#istate.shells),
            {true, I#istate{blocked = true,
                            has_been_blocked = true,
                            want_more = false,
                            old_want_more = I#istate.want_more}}
    end.

lookup_cmd(File,
           LineNo,
           #istate{orig_file = OrigFile, orig_commands = OrigCmds}) ->
    Fun = fun(#cmd{lineno = L} = Cmd, F, _CmdStack, Acc) ->
                  if
                      F =:= File, L =:= LineNo -> Cmd;
                      true                     -> Acc
                  end
          end,
    lux_utils:foldl_cmds(Fun, false, OrigFile, [], OrigCmds, static).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_break(I, Args, _CmdState) ->
    I2 =
        case Args of
            [{"lineno", BreakPos}, {"duration", Duration}] ->
                case Duration of
                    "temporary"  ->
                        add_break(I, BreakPos, temporary, false);
                    "next"  ->
                        add_break(I, BreakPos, next, true);
                    "skip"  ->
                        add_break(I, BreakPos, skip, true);
                    "normal"  ->
                        add_break(I, BreakPos, enabled, false);
                    "delete"  ->
                        Breaks = I#istate.breakpoints,
                        PrettyPos = pretty_break_pos(BreakPos),
                        case lists:keyfind(BreakPos, #break.pos, Breaks) of
                            false ->
                                format("\nNo breakpoint at: ~s\n",
                                       [PrettyPos]),
                                I;
                            Break ->
                                format("\nDelete breakpoint at: ~s\n",
                                       [PrettyPos]),
                                Breaks2 = delete_break(Break, Breaks),
                                I#istate{breakpoints = Breaks2}
                        end
                end;
            [{"lineno", BreakPos}] ->
                %% Normal breakpoint
                add_break(I, BreakPos, enabled, false);
            [] ->
                %% List Breakpoints
                Print =
                    fun(#break{pos = BreakPos, type = BreakType}) ->
                            Pretty = pretty_break_pos(BreakPos),
                            PrettyType =
                                case BreakType of
                                    temporary -> "\ttemporary";
                                    next      -> "\tnext";
                                    skip      -> "\tskip";
                                    enabled   -> "";
                                    disabled  -> ""
                                end,
                            format("  ~s~s\n", [Pretty, PrettyType])
                    end,
                case I#istate.commands of
                    [#cmd{} | _] ->
                        CurrentFullLineNo = current_full_lineno(I),
                        BreakPos =
                            full_lineno_to_static_break_pos(CurrentFullLineNo),
                        format("\nCurrent line: ~s\n",
                               [pretty_break_pos(BreakPos)]);
                    [] ->
                        ok
                end,
                case I#istate.breakpoints of
                    [] ->
                        format("\nNo breakpoints.\n", []);
                    Breaks ->
                        format("\nBreakpoints:\n", []),
                        lists:foreach(Print, Breaks)
                end,
                I
        end,
    {undefined, I2}.

add_break(I, BreakPos, BreakType, Invert) ->
    %% Search for matching command
    case break_to_full_lineno(I, BreakPos, rest) of
        false ->
            PrettyBreakPos = pretty_break_pos(BreakPos),
            format("\nERROR: No such lineno: ~p\n", [PrettyBreakPos]),
            I;
        FullLineNo ->
            NewBreakPos =
                case break_to_depth(I, BreakPos) of
                    static       -> full_lineno_to_static_break_pos(FullLineNo);
                    {dynamic, _} -> BreakPos
                end,
            PrettyBreakPos = pretty_break_pos(NewBreakPos),
            case BreakType of
                temporary ->
                    format("\nSet temporary breakpoint at \"~s\"\n",
                           [PrettyBreakPos]);
                next ->
                    ok;
                skip when not Invert ->
                    format("\nSkipping commands up to \"~s\"\n",
                           [PrettyBreakPos]);
                skip when Invert ->
                    format("\nSkip command at \"~s\"\n",
                           [PrettyBreakPos]);
                _ ->
                    format("\nSet breakpoint at \"~s\"\n",
                           [PrettyBreakPos])
            end,
            NewBreak = #break{pos = NewBreakPos,
                              invert = Invert,
                              type = BreakType},
            Breaks = replace_break(NewBreak, I#istate.breakpoints),
            I#istate{breakpoints = Breaks}
    end.

replace_break(NewBreak, Breaks) ->
    Breaks2 = delete_break(NewBreak, Breaks),
    lists:keysort(#break.pos, [NewBreak | Breaks2]).

delete_break(Break, Breaks) ->
    lists:keydelete(Break#break.pos, #break.pos, Breaks).

check_breakpoint(I, LineNo) ->
    Breaks = I#istate.breakpoints,
    case lookup_break(I, LineNo, Breaks) of
        false ->
            case lists:keymember(skip, #break.type, Breaks) of
                true  -> {skip, I}; % At least one skip breakpoint is active
                false -> {dispatch, I}
            end;
        Break ->
            BreakType = Break#break.type,
            CmdState = {attach, BreakType},
            case BreakType of
                temporary ->
                    %% Temporary breakpoint - remove it
                    {_, I2} = cmd_attach(I, [], CmdState),
                    Breaks2 = delete_break(Break, Breaks),
                    {wait, I2#istate{breakpoints = Breaks2}};
                next ->
                    %% Temporary breakpoint - remove it
                    {_, I2} = cmd_attach(I, [], CmdState),
                    Breaks2 = delete_break(Break, Breaks),
                    {wait, I2#istate{breakpoints = Breaks2}};
                skip ->
                    %% Temporary breakpoint - remove it
                    {_, I2} = cmd_attach(I, [], CmdState),
                    Breaks2 = delete_break(Break, Breaks),
                    {wait, I2#istate{breakpoints = Breaks2}};
                enabled ->
                    %% Normal breakpoint
                    %% Disable it to not get stuck when we continue
                    {_, I2} = cmd_attach(I, [], CmdState),
                    NewBreak = Break#break{type = disabled},
                    Breaks2 = replace_break(NewBreak, Breaks),
                    {wait, I2#istate{breakpoints = Breaks2}};
                disabled ->
                    %% Normal breakpoint
                    %% Enable it again for later reuse
                    NewBreak = Break#break{type = enabled},
                    Breaks2 = replace_break(NewBreak, Breaks),
                    {dispatch, I#istate{breakpoints = Breaks2}}
            end
    end.

lookup_break(I, LineNo, Breaks) when is_integer(LineNo) ->
    CurrentPos = current(I),
    FullLineNo = [CurrentPos#cmd_pos{lineno = LineNo} | I#istate.cmd_stack],
    do_lookup_break(FullLineNo, Breaks).

do_lookup_break(FullLineNo, [Break | Breaks]) ->
    Invert = Break#break.invert,
    IsMatch = match_break(FullLineNo, Break#break.pos, exact),
    if
        Invert, not IsMatch -> Break; % Invert test
        not Invert, IsMatch -> Break;
        true                -> do_lookup_break(FullLineNo, Breaks)
    end;
do_lookup_break(_FullLineNo, []) ->
    false.

match_break([#cmd_pos{rev_file = CurrRevFile, lineno = CurrLineNo} | _],
            {BreakRevFile, _BreakLineNo},
            _Prec)
  when CurrLineNo =:= 0 ->
    match_break_file(CurrRevFile, BreakRevFile);
match_break([#cmd_pos{rev_file = CurrRevFile, lineno = CurrLineNo} | _],
            {BreakRevFile, BreakLineNo},
            _Prec)
  when CurrLineNo =:= BreakLineNo ->
    match_break_file(CurrRevFile, BreakRevFile);
match_break(FullLineNo, Dynamic, Prec) when is_list(Dynamic) ->
    match_break_dynamic(FullLineNo, Dynamic, Prec);
match_break(_FullLineNo, _Static, _Prec) ->
    false.

match_break_dynamic([#cmd_pos{lineno = CurrLineNo} | Curr],
                    [_BreakLineNo | Dynamic], Prec = fuzzy)
  when CurrLineNo =:= 0 ->
    match_break_dynamic(Curr, Dynamic, Prec);
match_break_dynamic([{_CurrRevFile, CurrLineNo, _Type} | Curr],
                    [BreakLineNo | Dynamic], Prec)
  when CurrLineNo =:= BreakLineNo ->
    match_break_dynamic(Curr, Dynamic, Prec);
match_break_dynamic([], [], _Prec) ->
    true;
match_break_dynamic(_Curr, _Dynamic, _Prec) ->
    false.

match_break_file([FileComp | RevFile], [BreakComp | BreakFile]) ->
    match_break_comp(FileComp, BreakComp) andalso
        match_break_file(RevFile, BreakFile);
match_break_file(_RevFile, []) ->
    true;
match_break_file([], _BreakFile) ->
    false.

match_break_comp(_FileComp, []) ->
    true;
match_break_comp([Char | FileComp], [Char | BreakComp]) ->
    match_break_comp(FileComp, BreakComp);
match_break_comp(_FileComp, _BreakComp) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_continue(I, Args, CmdState) ->
    Invert = false,
    do_continue(I, Args, CmdState, temporary, Invert).

do_continue(I, Args, _CmdState, BreakType, Invert) ->
    case Args of
        [{"lineno", BreakPos}] ->
            I2 = add_break(I, BreakPos, BreakType, Invert),
            GoAhead = I#istate.breakpoints =/= I2#istate.breakpoints;
        [] ->
            I2 = I,
            GoAhead = true
    end,
    case GoAhead of
        true ->
            case BreakType of
                next ->
                    ok;
                skip ->
                    ok;
                _ ->
                    CurrentFullLineNo = current_full_lineno(I2),
                    BreakPos2 =
                        full_lineno_to_static_break_pos(CurrentFullLineNo),
                    format("\nContinue to run from \"~s\"\n",
                           [pretty_break_pos(BreakPos2)])
            end,
            {_Blocked, I3} = opt_unblock(I2),
            {undefined, I3};
        false ->
            {undefined, I2}
    end.

opt_unblock(I) ->
    if
        not I#istate.blocked ->
            {false, I};
        I#istate.blocked, I#istate.old_want_more =/= undefined ->
            lists:foreach(fun(#shell{pid = P}) -> P ! {unblock, self()} end,
                          I#istate.shells),
            {true, I#istate{blocked = false,
                            want_more = I#istate.old_want_more,
                            old_want_more = undefined}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_help(I, [], CmdState) ->
    {error, Ambiguous} = select(undefined, ""),
    Params = param_help(),
    format("\n~s~s\n~s", [intro_help(), Ambiguous, Params]),
    {CmdState, I};
cmd_help(I, [{_, "lineno"}], CmdState) ->
    format("~s", [lineno_help()]),
    {CmdState, I};
cmd_help(I, [{_, CmdName}], CmdState) ->
    case select(undefined, CmdName) of
        {ok, Cmd} ->
            Pretty = lists:flatten(pretty_cmd(Cmd)),
            format("\n~s\n", [Pretty]),
            {CmdState, I};
        {error, ReasonStr} ->
            format("\nERROR: ~s\n", [ReasonStr]),
            {CmdState, I}
    end.

pretty_cmd(#debug_cmd{name = Name, params = Params, help = Help}) ->
    Longest = longest(Params),
    Fun = fun(#debug_param{name = N, presence = Pres}) ->
                  case Pres of
                      optional  -> [" \\[", N, "\\]"];
                      mandatory -> [" ", N]
                  end
          end,
    Header = lists:flatten([Name, lists:map(Fun, Params)]),
    [Header,
     "\n",
     lists:duplicate(length(Header), "-"),
     "\n\n",
     lux_utils:capitalize(Help),
     "\n\n**Parameters:**  \n\n",
     case Params of
         [] ->
             "* no parameters\n\n";
         _ ->
             [pretty_param(P, Longest) || P <-Params]
     end
    ].

pretty_param(#debug_param{name = Name, type = Type, help = Help}, Longest) ->
    PrettyType =
        case Type of
            binary ->
                "string";
            atom ->
                "string";
            existing_atom ->
                "string";
            {enum, [H | T]} ->
                [
                 "enum(",
                 H, [["|", Elem] || Elem <- T],
                 ")"
                ];
            {Atom, Min, Max} ->
                [
                 ?FF("~p", [Min]),
                 " >= ",
                 ?a2l(Atom),
                 " =< ",
                 ?FF("~p", [Max])
                ];
            Atom ->
                ?a2l(Atom)
        end,
    ["* ", string:left(Name, Longest, $\ ),
     " - ", Help, "; ", PrettyType, "  \n"].

longest(Elems) ->
    longest2(Elems, 0).

longest2([H | T], Longest) ->
    case H of
        {_Name, #debug_cmd{name = Str}} -> ok;
        #debug_cmd{name = Str} -> ok;
        #debug_param{name = Str} -> ok;
        {Str, _Help} when is_list(Str) -> ok;
        Str when is_list(Str) -> ok
    end,
    longest2(T, lists:max([Longest, length(Str)]));
longest2([], Longest) ->
    Longest.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_tail(#istate{suite_log_dir=SuiteLogDir,
                 case_log_dir=CaseLogDir,
                 tail_status=Status} = I,
         [],
         CmdState) ->
    {ok, Cwd} =  file:get_cwd(),
    format("Log files at ~s:\n\n",
           [lux_utils:drop_prefix(Cwd, CaseLogDir)]),
    {I2, Logs} = all_logs(I),
    Print = fun(Abs, Index) ->
                    Rel = lux_utils:drop_prefix(SuiteLogDir, Abs),
                    {Curr, Display} =
                        case file:read_file_info(Abs) of
                            {ok, #file_info{size=Size}} ->
                                {Size, ""};
                            {error, Reason} ->
                                Str = file:format_error(Reason),
                                {Reason, ": " ++ Str}
                        end,
                    Prefix =
                        case lists:keyfind(Abs, 1, Status) of
                            false ->
                                "*";
                            {_, Prev} when Prev =:= Curr ->
                                " ";
                            _ ->
                                "*"
                        end,
                    format("~s~3w ~s~s\n", [Prefix, Index, Rel, Display]),
                    {{Abs, Curr}, Index+1}
            end,
    {Status2, _} = lists:mapfoldl(Print, 1, Logs),
    format("\n", []),
    TailOpts = [{"index", 5}, {"format", "compact"}, {"n_lines", 10}],
    {CmdState2, I3} = cmd_tail(I2, TailOpts, CmdState),
    {CmdState2, I3#istate{tail_status=Status2}};
cmd_tail(I, [{"index", Index} | Rest], CmdState) ->
    case Rest of
        [{"format", Format} | Rest2] ->
            case Rest2 of
                [{"n_lines", UserN}] ->
                    ok;
                [] ->
                    UserN = undefined
            end;
        [] ->
            Format = "compact",
            UserN = undefined
    end,
    {I2, Logs} = all_logs(I),
    case catch lists:nth(Index, Logs) of
        {'EXIT', _} ->
            format("ERROR: ~p is not a valid log index."
                   " Must be within ~p..~p.\n",
                   [Index, 1, length(Logs)]),
            {CmdState, I2};
        LogFile ->
            tail(I2, LogFile, CmdState, Format, UserN)
    end.

all_logs(#istate{orig_file=Script,
                 suite_log_dir=SuiteLogDir,
                 case_log_dir=CaseLogDir,
                 logs=StdLogs} = I) ->
    I2 = lux_interpret:flush_logs(I),
    Split = fun({_Name, Stdin, Stdout}, Acc) ->
                    [Stdout, Stdin | Acc]
            end,
    Logs = lists:reverse(lists:foldl(Split, [], StdLogs)),
    Base = filename:basename(Script),
    EventLog = filename:join([CaseLogDir, Base ++ ?CASE_EVENT_LOG]),
    ConfigLog = filename:join([CaseLogDir, Base ++ ?CASE_CONFIG_LOG]),
    SuiteConfigLog = filename:join([SuiteLogDir, ?SUITE_CONFIG_LOG]),
    SummaryLog = filename:join([SuiteLogDir, ?SUITE_SUMMARY_LOG ++ ".tmp"]),
    ResultLog = filename:join([SuiteLogDir, ?SUITE_RESULT_LOG]),
    {I2, [SuiteConfigLog, SummaryLog, ResultLog, ConfigLog, EventLog | Logs]}.

tail(#istate{suite_log_dir=SuiteLogDir} = I,
     AbsFile, CmdState, Format, UserN) ->
    RelFile = lux_utils:drop_prefix(SuiteLogDir, AbsFile),
    case file:read_file(AbsFile) of
        {ok, Bin} ->
            AllRows = binary:split(Bin, <<"\n">>, [global]),
            Max = length(AllRows),
            N =
                case CmdState of
                    {debug_tail, AbsFile, PrevMin} ->
                        %% Add 10
                        PrevMin + 10;
                    _ when is_integer(UserN) ->
                        %% User specified N
                        UserN;
                    _ ->
                        %% Last 10
                        10
                end,
            Min = lists:max([0, Max - N]),
            TailRows = lists:nthtail(Min, AllRows),
            Actual = length(TailRows),
            format("Last ~p (~p..~p) lines of log file: ~s\n\n",
                   [Actual, Max-Actual+1, Max, RelFile]),
            [tail_format(Format, "~s\n", [Row]) || Row <- TailRows],
            {{debug_tail, AbsFile, N}, I};
        {error, FileReason}->
            FileStr = file:format_error(FileReason),
            format("ERROR: ~s: ~s\n", [RelFile, FileStr]),
            {undefined, I}
    end.

tail_format("compact", Format, Data) ->
    format(Format, Data);
tail_format("verbose", Format, Data) ->
    Str = lists:flatten(?FF(Format, Data)),
    format("~s", [lux_log:dequote(Str)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_list(I, Args, CmdState) ->
    CurrentFullLineNo = current_full_lineno(I),
    case CmdState of
        {debug_list, OldN, OldRevFile, OldLineNo} ->
            PrevRevFile = OldRevFile,
            ok;
        _ ->
            [#cmd_pos{rev_file = OldRevFile,
                      lineno = OldLineNo} | _] = CurrentFullLineNo,
            OldN = 10,
            PrevRevFile = []
    end,
    case Args of
        [{"lineno", BreakPos}] ->
            N = OldN,
            case break_to_full_lineno(I, BreakPos, orig) of
                false ->
                    format("\nERROR: No such lineno: ~p\n",
                           [pretty_break_pos(BreakPos)]),
                    {undefined, I};
                [#cmd_pos{rev_file = RevFile, lineno = First} | _] ->
                    do_list(I, PrevRevFile, RevFile,
                            First, N, CurrentFullLineNo)
            end;
        [{"n_lines", N0}, {"lineno", BreakPos}] ->
            case break_to_full_lineno(I, BreakPos, orig) of
                false ->
                    format("\nERROR: No such lineno: ~p\n",
                           [pretty_break_pos(BreakPos)]),
                    {undefined, I};
                [#cmd_pos{rev_file = RevFile, lineno = First0} | _] ->
                    if
                        N0 < 0 ->
                            N = abs(N0),
                            First = First0 - N,
                            do_list(I, PrevRevFile, RevFile,
                                    First, N, CurrentFullLineNo);
                        true ->
                            N = N0,
                            First = First0,
                            do_list(I, PrevRevFile, RevFile,
                                    First, N, CurrentFullLineNo)
                    end
            end;
        [{"n_lines", N0}] ->
            if
                N0 < 0 ->
                    N = abs(N0),
                    First = OldLineNo - N,
                    do_list(I, PrevRevFile, OldRevFile,
                            First, N, CurrentFullLineNo);
                true ->
                    N = N0,
                    First = OldLineNo,
                    do_list(I, PrevRevFile, OldRevFile,
                            First, N, CurrentFullLineNo)
            end;
        [] ->
            N = OldN,
            First = OldLineNo,
            do_list(I, PrevRevFile, OldRevFile,
                    First, N, CurrentFullLineNo)
    end.

do_list(#istate{orig_file = OrigFile, orig_commands = OrigCmds} = I,
        PrevRevFile, RevFile, First, N,
        [#cmd_pos{rev_file = CurrRevFile, lineno = CurrLineNo} | _]) ->
    Last = First+N-1,
    %% format("List source lines ~p..~p of file ~s\n",
    %%          [First, Last, lux_utils:pretty_filename(RevFile)]),
    if
        PrevRevFile =/= RevFile ->
            format("\nFile ~s:\n", [lux_utils:pretty_filename(RevFile)]);
        true ->
            ignore
    end,
    Print =
        fun(#cmd{type = CmdType, lineno = LineNo, orig = Text}, RF, _IS, Acc) ->
                if
                    RF =:= RevFile, LineNo >= First, LineNo =< Last ->
                        Delim =
                            if
                                RevFile =:= CurrRevFile,
                                LineNo =:= CurrLineNo ->
                                    ">";
                                true ->
                                    ":"
                            end,
                        Pos = {RevFile, LineNo, CmdType},
                        case lists:member(Pos, Acc) of
                            true ->
                                %% Duplicate due to multiple
                                %% inclusions of same file
                                Acc;
                            false ->
                                format("~p~s ~s\n",
                                       [LineNo, Delim, Text]),
                                [Pos | Acc]
                        end;
                    true ->
                        Acc
                end
        end,
    PosList = lux_utils:foldl_cmds(Print, [], OrigFile, [], OrigCmds, static),
    case PosList of
        [] ->
            format("\nWrap file.\n", []),
            {undefined, I};
        _ ->
            {{debug_list, N, RevFile, Last+1}, I}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_load(I, Args, CmdState) ->
    case Args of
        [{"file", File}] ->
            ok;
        [] ->
            File = "lux.debug"
    end,
    case file:read_file(File) of
        {ok, Bin} ->
            format("\nLoad commands from file: ~s\n", [File]),
            Fun = fun(CmdStr, {CS, IS, LineNo}) ->
                          format("~p: ~s\n", [LineNo, CmdStr]),
                          {CS2, IS2} = do_eval_cmd(IS, CmdStr, CS),
                          {CS2, IS2, LineNo+1}
                  end,
            Lines = string:tokens(?b2l(Bin), "\n"),
            {_CmdState, I2, _} = lists:foldl(Fun, {CmdState, I, 1}, Lines),
            {undefined, I2};
        {error, Reason} ->
            format("\nERROR: Cannot read from file ~p: ~s\n",
                   [File, file:format_error(Reason)]),
            {undefined, I}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_next(I, [], CmdState) ->
    CurrentFullLineNo = current_full_lineno(I),
    BreakPos = full_lineno_to_static_break_pos(CurrentFullLineNo),
    Invert = true,
    do_continue(I, [{"lineno", BreakPos}], CmdState, next, Invert).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_progress(I, Args, CmdState) ->
    case Args of
        [{"level", Level0}] ->
            Level = list_to_existing_atom(Level0);
        [] ->
            Level =
                case I#istate.progress of
                    etrace  -> brief;
                    ctrace  -> brief;
                    verbose -> brief;
                    compact -> brief;
                    doc     -> verbose;
                    brief   -> verbose;
                    summary -> verbose;
                    silent  -> verbose
                end
    end,
    lists:foreach(fun(#shell{pid = Pid}) -> Pid ! {progress, self(), Level} end,
                  I#istate.shells),
    {CmdState, I#istate{progress = Level}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_quit(I, Args, _CmdState) ->
    case Args of
        [{"scope", ScopeStr}] ->
            ok;
        [] ->
            ScopeStr = "case"
        end,
    Scope = list_to_existing_atom(ScopeStr),
    format("\nWARNING: Test ~s stopped by user\n", [ScopeStr]),
    {_, I2} = opt_unblock(I),
    InterpreterPid = self(),
    InterpreterPid ! {stopped_by_user, Scope},
    {undefined, I2 }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_save(I, Args, _CmdState) ->
    case Args of
        [{"file", File}] ->
            ok;
        [] ->
            File = "lux.debug"
    end,
    Save =
        fun(#break{pos = BreakPos, type = BreakType}) ->
                [
                 "break ", pretty_break_pos(BreakPos),
                 case BreakType of
                     enabled   -> "";
                     temporary -> " temporary";
                     next      -> " next";
                     skip      -> " skip";
                     disabled  -> ""
                 end,
                 "\n"
                ]
        end,
    IoList = lists:map(Save, I#istate.breakpoints),
    case file:write_file(File, IoList) of
        ok ->
            format("\nSave debugger state to file: ~s\n", [File]);
        {error, Reason} ->
            format("\nERROR: Cannot write to file ~p: ~s\n",
                   [File, file:format_error(Reason)])
    end,
    {undefined, I}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_skip(I, Args, CmdState) ->
    case Args of
        [{"lineno", BreakPos}] ->
            Invert = false;
        [] ->
            CurrentFullLineNo = current_full_lineno(I),
            BreakPos = full_lineno_to_static_break_pos(CurrentFullLineNo),
            Invert = true
    end,
    NewArgs = [{"lineno", BreakPos}],
    do_continue(I, NewArgs, CmdState, skip, Invert).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_shell(I, [], _CmdState) ->
    case all_shells(I) of
        [] ->
            format("No shells started yet.\n", []);
        AllShells ->
            Print = fun(#shell{name = Name, health = Health}) ->
                            N = shell_name(I),
                            Status =
                                if
                                    Name =:= N        -> "->";
                                    Health =:= zombie -> " *";
                                    Health =:= alive  -> "  "
                                end,
                            format("  ~s ~s\n", [Status, Name])
                    end,
            format("Shells:\n", []),
            lists:foreach(Print, AllShells),
            format("\n", [])
    end,
    {undefined, I};
cmd_shell(I, [{"name", Name} | Rest], _CmdState) ->
    AllShells = all_shells(I),
    Filter = fun(S) -> lists:prefix(Name, S#shell.name) end,
    case lists:filter(Filter, AllShells) of
        [] ->
            format("ERROR: ~p does not match any shell name.\n", [Name]),
            {undefined, I};
        [#shell{name = N, health = Health, pid = Pid} | Ambiguous]
          when Ambiguous =:= [];
               N =:= Name -> % Give exact match prio
            case Rest of
                []                -> Mode0 = "background";
                [{"mode", Mode0}] -> ok
            end,
            OldN = shell_name(I),
            if
                OldN =/= undefined ->
                    Old = lists:keyfind(OldN, #shell.name, AllShells),
                    Old#shell.pid ! {debug_shell, self(), disconnect};
                true ->
                    ok
            end,
            DS =
                if
                    Health =:= zombie ->
                        format("ERROR: ~p is a zombie.\n", [Name]),
                        undefined;
                    N =/= OldN ->
                        Mode = list_to_existing_atom(Mode0),
                        Pid ! {debug_shell, self(), {connect, Mode}},
                        #debug_shell{name=N, mode=Mode, pid=Pid};
                    true ->
                        undefined
                end,
            {undefined, I#istate{debug_shell = DS}};
        _Ambiguous ->
            format("ERROR: ~p is an ambiguous shell name.\n", [Name]),
            {undefined, I}
    end.

all_shells(#istate{active_shell=ActiveShell,
                   shells=Shells}) ->
    AllShells =
        case ActiveShell of
            undefined -> Shells;
            #shell{} ->  [ActiveShell | Shells]
        end,
    lists:keysort(#shell.name, AllShells).

shell_name(I) ->
    case I#istate.debug_shell of
        undefined               -> undefined;
        #debug_shell{name=Name} -> Name
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_trace(I, Args, _CmdState) ->
    CmdState =  undefined,
    TraceMode = I#istate.trace_mode,
    case Args of
        [] ->
            format("Internal trace mode: ~p\n", [TraceMode]),
            {CmdState, I};
        [{"action", Action}] ->
            cmd_trace(I, [{"action", Action}, {"mode", "CASE"}], _CmdState);
        [{"action", Action}, {"mode", TraceModeUpper}] ->
            TraceModeLower = string:to_lower(TraceModeUpper),
            TraceMode2 = list_to_atom(TraceModeLower),
            case list_to_atom(Action) of
                'START' when TraceMode =:= none ->
                    LogDir = I#istate.case_log_dir,
                    Base = filename:basename(I#istate.orig_file) ++
                        "." ++ TraceModeLower,
                    TraceLog0 = lux_utils:join(LogDir, Base),
                    FirstTracePid = self(),
                    {ok, TraceLog} =
                        lux_main:start_trace(TraceMode2,
                                             {file, TraceLog0},
                                             FirstTracePid),
                    Base2 = filename:basename(TraceLog),
                    format("\nInternal tracing of test ~s started.\n",
                           [?a2l(TraceMode2)]),
                    elog(I, "trace start (~s)", [Base2]),
                    {CmdState, I#istate{trace_mode = TraceMode2}};
                'STOP' when TraceMode =:= TraceMode2 ->
                    format("\nInternal tracing of test ~s stopped.\n",
                           [?a2l(TraceMode)]),
                    elog(I, "trace stop", []),
                    lux_main:stop_trace(),
                    {CmdState, I#istate{trace_mode = none}};
                _ ->
                    format("\nERROR: Refused to ~p internal tracing of"
                           " test case as test ~s is being traced.\n",
                           [Action, ?a2l(TraceMode)]),
                    elog(I, "trace failed (~s)", [?a2l(TraceMode)]),
                    {CmdState, I}
            end
    end.

elog(I, Format, Args) ->
    lux_interpret:ilog(I, "~s(~p): "++Format++"\n",
                 [I#istate.active_name,
                  (I#istate.latest_cmd)#cmd.lineno | Args]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

current_full_lineno(I) ->
    [current(I) | I#istate.cmd_stack].

current(#istate{file = File,
                call_level = Level,
                latest_cmd = LatestCmd,
                commands = Cmds}) ->
    case Cmds of
        [] when Level =:= 1 ->
            LineNo = 1,
            CmdType = main;
        [] ->
            LineNo = 1,
            CmdType = LatestCmd#cmd.type;
        [#cmd{type = CmdType, lineno = LineNo} | _] ->
            ok
        end,
    RevFile = lux_utils:filename_split(File), % optimize later
    #cmd_pos{rev_file = RevFile, lineno = LineNo, type = CmdType}.

full_lineno_to_static_break_pos(FullLineNo) ->
    #cmd_pos{rev_file = RevFile, lineno = LineNo} = hd(FullLineNo),
    {RevFile, LineNo}.

%% full_lineno_to_dynamic_break_pos(FullLineNo) ->
%%     [LineNo || {_File, LineNo, _Type} <- FullLineNo].

break_to_full_lineno(I, BreakPos, Scope) ->
    case Scope of
        rest ->
            File = I#istate.file,
            Cmds = I#istate.commands;
        orig ->
            File = I#istate.orig_file,
            Cmds = I#istate.orig_commands
    end,
    Collect =
        fun(Cmd, RevFile, CmdStack, Acc) ->
                collect_break(Cmd, RevFile, CmdStack, Acc, BreakPos)
        end,
    Depth = break_to_depth(I, BreakPos),
    case lux_utils:foldl_cmds(Collect, [], File, [], Cmds, Depth) of
        []       -> false;
        Matching -> lists:last(Matching)
    end.

break_to_depth(I, BreakPos) ->
    if
        is_tuple(BreakPos), tuple_size(BreakPos) =:= 2 -> static;
        is_list(BreakPos), BreakPos =/= []             -> {dynamic, I}
    end.

collect_break(#cmd{type = CmdType, lineno = LineNo},
              RevFile, CmdStack, Acc, BreakPos) ->
    CmdPos = #cmd_pos{rev_file = RevFile,
                      lineno = LineNo,
                      type = CmdType},
    FullLineNo = [CmdPos | CmdStack],
    case match_break(FullLineNo, BreakPos, fuzzy) of
        true  -> [FullLineNo | Acc];
        false -> Acc
    end.

pretty_break_pos({RevFile, LineNo}) when is_integer(LineNo) ->
    %% Static
    lists:flatten([lux_utils:pretty_filename(RevFile), ":", ?i2l(LineNo)]);
%% pretty_break_pos([LineNo]) when is_integer(LineNo) ->
%%     %% Dynamic
%%     ?i2l(LineNo);
pretty_break_pos(RevLineNoList) when length(RevLineNoList) > 1 ->
    %% Dynamic
    [LineNo | LineNoList] = lists:reverse(RevLineNoList),
    lists:flatten([?i2l(LineNo), [[":", ?i2l(L)] || L <- LineNoList]]).
