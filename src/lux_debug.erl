%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2012 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_debug).

-export([start_link/1, eval_cmd/4, cmd_attach/3, check_break/2, markdown/0]).

-include("lux.hrl").
-include_lib("kernel/include/file.hrl").

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read commands from stdin and communicate with interpreter

start_link(I) ->
    Parent = self(),
    spawn_link(fun() -> init(I, Parent) end).

init(I, Ipid) ->
    CmdState = undefined,
    NewCmdState =
        case I#istate.debug_file of
            undefined ->
                CmdState;
            DebugFile ->
                LoadCmd = "load " ++ DebugFile,
                call(Ipid, LoadCmd, CmdState)
        end,
    loop(I, Ipid, "help", NewCmdState, 1).

loop(I, Ipid, PrevCmd, CmdState, N) ->
    case io:get_line("") of
        eof when N =:= 1 ->
            %% Closed already at startup
            exit(normal);
        eof ->
            catch io:format("\nEOF: stdin closed\n", []),
            exit(normal);
        {error, Reason} ->
            ReasonStr = file:format_error(Reason),
            catch io:format("\nERROR: ~s\n", [ReasonStr]),
            exit(Reason);
        Cmd0 ->
            [$\n | Rest] = lists:reverse(Cmd0),
            Cmd = lists:reverse(Rest),
            case string:tokens(Cmd, " ") of
                [] when Cmd =:= "" ->
                    %% Repeat previous command
                    NewCmdState = call(Ipid, PrevCmd, CmdState),
                    loop(I, Ipid, PrevCmd, NewCmdState, N+1);
                [] ->
                    %% Ignore empty command
                    loop(I, Ipid, Cmd, CmdState, N+1);
                _ ->
                    %% Execute new command
                    NewCmdState = call(Ipid, Cmd, CmdState),
                    loop(I, Ipid, Cmd, NewCmdState, N+1)
            end
    end.

call(Ipid, Cmd, CmdState) when is_list(Cmd); is_function(Cmd, 2) ->
    %% io:format("DEBUG: ~p\n", [CmdStr]),
    Ipid ! {debug_call, self(), Cmd, CmdState},
    receive
        {debug_call, Ipid, NewCmdState} -> NewCmdState
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse and evaluate one command

eval_cmd(I, Dpid, Cmd, CmdState) ->
    {CmdState2, I2} = do_eval_cmd(I, Cmd, CmdState),
    Dpid ! {debug_call, self(), CmdState2},
    I2.

do_eval_cmd(I, CmdStr, CmdState) when is_list(CmdStr) ->
    [CmdName | Args] = string:tokens(CmdStr, " "),
    case select(CmdName) of
        {ok, #debug_cmd{name = _Name, params = Params, callback = Fun}} ->
            case parse_params(Params, Args, [], []) of
                {ok, Args2} ->
                    %% io:format("Eval: ~s ~p\n", [_Name, Args2]),
                    Fun(I, Args2, CmdState);
                {error, ReasonStr} ->
                    io:format("\nERROR: ~s: ~s", [CmdName, ReasonStr]),
                    {CmdState, I}
            end;
                {error, ReasonStr} ->
            io:format("\nERROR: ~s", [ReasonStr]),
            {CmdState, I}
    end;
do_eval_cmd(I, Cmd, CmdState) when is_function(Cmd, 2) ->
    Cmd(CmdState, I).

select(CmdStr) ->
    NamedCmds = [{C#debug_cmd.name, C} || C <- cmds()],
    do_select(CmdStr, NamedCmds, CmdStr, NamedCmds).

do_select([H | T], NamedCmds, Orig, Prev) ->
    case select_first(H, NamedCmds, []) of
        [] ->
            {error,  ambiguous(Orig, Prev)};
        [{_, Cmd}] ->
            case lists:prefix(Orig, Cmd#debug_cmd.name) of
                true ->
                    {ok, Cmd};
                false ->
                    {error,  ambiguous(Orig, Prev)}
            end;
        Ambiguous ->
            do_select(T, Ambiguous, Orig, NamedCmds)
    end;
do_select([], [{_, Cmd}], _Orig, _Prev) ->
    {ok, Cmd};
do_select([], [], Orig, Prev) ->
    {error, ambiguous(Orig, Prev)};
do_select([], Ambiguous, Orig, _Prev) ->
    {error,  ambiguous(Orig, Ambiguous)}.

select_first(Char, [{[Char | Chars], Cmd} | NamedCmds], Acc) ->
    select_first(Char, NamedCmds, [{Chars, Cmd} | Acc]);
select_first(Char, [{_Chars, _Cmd} | NamedCmds], Acc) ->
    select_first(Char, NamedCmds, Acc);
select_first(_Char, [], Acc) ->
    lists:reverse(Acc).

ambiguous(Orig, NamedCmds) ->
    Longest = longest(NamedCmds),
    Fun = fun({_, #debug_cmd{name = Name, help = Help}}) ->
                  {Slogan, _} =
                      lists:splitwith(fun(Char) -> Char =/= $\n end, Help),
                  ["* ", string:left(Name, Longest, $\ ), " - ", Slogan, "\n"]
          end,
    DeepList = lists:map(Fun, NamedCmds),
    lists:flatten(["Available commands: ", Orig, "\n",
                   "-------------------\n", DeepList]).

parse_params([#debug_param{name = Name,type = Type} | Params],
             [Arg | Args], Acc, _Bad) ->

    try
        Val = parse_param(Type, Arg),
        parse_params(Params, Args, [{Name, Val} | Acc], [])
    catch
        error:_ ->
            Type2 =
                if
                    is_atom(Type) -> atom_to_list(Type);
                    true          -> atom_to_list(element(1, Type))
                end,
            ReasonStr = lists:flatten(["Bad type of parameter ", Name,
                                       ". Expected ", Type2]),
            {error, ReasonStr}
    end;
parse_params(Params, [], Acc, Bad) ->
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
parse_params([], [Arg | _], _Acc, Bad) ->
    Longest = longest(Bad),
    DeepList = ["Value ", Arg, " has the wrong type.\nPossible parameters:\n",
                [pretty_param(P, Longest) || P <- Bad]],
    {error, lists:flatten(DeepList)}.

parse_param(Type, Val) ->
    case Type of
        lineno when is_list(Val) ->
            Parse =
                fun(Raw) ->
                        case string:tokens(Raw, "@") of
                            [LineNo, File] ->
                                {lux_utils:filename_split(File),
                                 parse_param({integer, 1, infinity}, LineNo)};
                            [_] ->
                                try
                                    %% lineno
                                    parse_param({integer, 1, infinity}, Raw)
                                catch
                                    error:_ ->
                                        %% file name
                                        lux_utils:filename_split(Raw)
                                end
                        end
                end,
            X = lists:reverse(lists:map(Parse, string:tokens(Val, ":"))),
            %% io:format("lineno: ~p\n", [X]),
            X;
        string when is_list(Val) ->
            Val;
        {enum, List} when is_list(List) ->
            [Single] = [Elem || Elem <- List, lists:prefix(Val, Elem)],
            Single;
        binary ->
            list_to_binary(Val);
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
                                                      "delete"]},
                                       presence = optional,
                                       help = "controls the duration of "
                                       "the breakpoint"}],
                help = "set, delete and list breakpoints\n\n"
                "When a breakpoint is set it may either be normal (default)\n"
                "or temporary. The difference between them is that normal\n"
                "breakpoints remains after break while temporary breakpoints\n"
                "are automatically deleted when they have been used once.\n\n"
                "delete means that the breakpoint immediately is removed\n"
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
                "star. Star means that the log has been updated since the\n"
                "previous status check. Use the index to display a particular\n"
                "log. Such as \"t 2\" for the event log. Press enter to\n"
                "display more lines. n_lines can be used to override that\n"
                "behavior andonly display a fixed number of lines regardless\n"
                "of the command is repeated or not.",
                callback = fun cmd_tail/3},
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
                                       "\"lux.debug\"."}],
                help = "load file with debug commands",
                callback = fun cmd_load/3},
     #debug_cmd{name = "next",
                params = [#debug_param{name = "n_commands",
                                       type = {integer, 1, infinity},
                                       presence = optional,
                                       help = "number of commands"}],
                help = "execute one or more commands. "
                "A multiline command counts as one command.",
                callback = fun cmd_next/3},
     #debug_cmd{name = "progress",
                params = [#debug_param{name = "level",
                                       type = {enum, ["silent", "brief", "doc",
                                                      "compact", "verbose"]},
                                       presence = optional,
                                       help = "verbosity level. "
                                       "Toggles between brief and "
                                       "verbose by default."}],
                help = "set verbosity level of progress",
                callback = fun cmd_progress/3},
     #debug_cmd{name = "quit",
                params = [],
                help = "exit lux in a controlled manner. "
                "Runs cleanup if applicable. ",
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
                params = [#debug_param{name = "n_commands",
                                       type = {integer, 1, infinity},
                                       presence = optional,
                                       help = "number of commands"}],
                help = "skip execution of one or more commands. "
                "A multiline command counts as one command.",
                callback = fun cmd_skip/3}
    ].

intro_help() ->
    ""
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
        "`help help` to get more detailed descriptions of the commands.\n\n".

lineno_help() ->
    "\n"
        "lineno parameter\n"
        "----------------\n"
        "Several commands has a lineno as parameter. It is a string which\n"
        "is divided in several components. The components are separated\n"
        "with a colon and are used to refer to line numbers in include\n"
        "files and macros. Each component may either be a line number,\n"
        "an (possibly abbreviated) file name or a combination of both\n"
        "separated with an at-sign (int@file).\n"
        "\n"
        "Assume that there is a file called main, which includes a file\n"
        "called outer at line 4 and the file outer includes a file called\n"
        "inner at line 12.\n"
        "\n"
        "Here are a few examples of how lineno can be used:\n"
        "\n"
        "* 3       - line 3 in file main\n"
        "* main    - first line in file main\n"
        "* 3@m     - line 3 in file main\n"
        "* inner   - any line in file inner\n"
        "* outer:i - any line in file inner if it is directly\n"
        "            included from outer\n"
        "* 12@o:i  - any line in file inner if it is directly\n"
        "            included from outer on line 12\n"
        "* 4:12:6  - line 6 in file inner if it is included\n"
        "            on line 12 in outer and outer is included\n"
        "            on line 4 in main.\n\n".

markdown() ->
    Intro = intro_help(),
    {error, Ambiguous} = select(""),
    LineNo = lineno_help(),
    Cmds = lists:flatten([["\n", pretty_cmd(Cmd)] || Cmd <- cmds()]),
    io:format("~s\n", [Intro]),
    io:format("~s\n", [Ambiguous]),
    io:format("~s\n", [LineNo]),
    io:format("~s\n", [Cmds]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_attach(I, _, CmdState) ->
    CurrentFullLineNo = current_full_lineno(I),
    ListOpts =
        case CmdState of
            {attach, next} ->
                BreakPos = full_lineno_to_break_pos(CurrentFullLineNo),
                [{"n_lines", 1}, {"lineno", BreakPos}];
            _ ->
                io:format("\nBreak at ~s\n",
                          [pretty_break_pos(CurrentFullLineNo)]),
                [{RevFile, LineNo} | InclStack] = CurrentFullLineNo,
                LineNo2 =
                    if
                        LineNo > 3 ->
                            TmpLineNo = LineNo-2,
                            case is_valid_lineno(RevFile, TmpLineNo, I) of
                                false -> LineNo;
                                true  -> TmpLineNo
                            end;
                        true->
                            LineNo
                    end,
                FullLineNo = [{RevFile, LineNo2} | InclStack],
                BreakPos2 = full_lineno_to_break_pos(FullLineNo),
                [{"n_lines", 10}, {"lineno", BreakPos2}]
        end,
    case opt_block(I) of
        {false, I2} ->
            {undefined, I2};
        {true, I2} ->
            io:format("\n",[]),
            cmd_list(I2, ListOpts, CmdState)
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

is_valid_lineno(File,
                LineNo,
                #istate{orig_file = OrigFile, orig_commands = OrigCmds}) ->
    Fun = fun(#cmd{lineno = L}, F, _InclStack, Acc) ->
                  if
                      F =:= File, L =:= LineNo -> true;
                      true                     -> Acc
                  end
          end,
    lux_utils:foldl_cmds(Fun, false, OrigFile, [], OrigCmds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_break(I, Args, _CmdState) ->
    I2 =
        case Args of
            [{"lineno", BreakPos}, {"duration", Duration}] ->
                case Duration of
                    "temporary"  ->
                        add_break(I, BreakPos, temporary);
                    "next"  ->
                        add_break(I, BreakPos, next);
                    "normal"  ->
                        add_break(I, BreakPos, enabled);
                    "delete"  ->
                        Breaks = I#istate.breakpoints,
                        PrettyPos = pretty_break_pos(BreakPos),
                        case lists:keyfind(BreakPos, #break.pos, Breaks) of
                            false ->
                                io:format("\nNo breakpoint at: ~s\n",
                                          [PrettyPos]),
                                I;
                            Break ->
                                io:format("\nDelete breakpoint at: ~s\n",
                                          [PrettyPos]),
                                Breaks2 = delete_break(Break, Breaks),
                                I#istate{breakpoints = Breaks2}
                        end
                end;
            [{"lineno", BreakPos}] ->
                %% Normal breakpoint
                add_break(I, BreakPos, enabled);
            [] ->
                %% List Breakpoints
                Print =
                    fun(#break{pos = Pos, type = T}) ->
                            Pretty = pretty_break_pos(Pos),
                            case T of
                                temporary ->
                                    io:format("  ~s\ttemporary\n", [Pretty]);
                                next ->
                                    io:format("  ~s\tnext\n", [Pretty]);
                                enabled ->
                                    io:format("  ~s\n", [Pretty]);
                                disabled ->
                                    io:format("  ~s\n", [Pretty])
                            end
                    end,
                case I#istate.commands of
                    [#cmd{} | _] ->
                        io:format("\nCurrent line: ~s\n",
                                  [pretty_break_pos(current_full_lineno(I))]);
                    [] ->
                        ok
                end,
                case I#istate.breakpoints of
                    [] ->
                        io:format("\nNo breakpoints.\n", []);
                    Breaks ->
                        io:format("\nBreakpoints:\n", []),
                        lists:foreach(Print, Breaks)
                end,
                I
        end,
    {undefined, I2}.

add_break(I, BreakPos, Type) ->
    %% Search for matching command
    case break_to_full_lineno(I, BreakPos) of
        false ->
            PrettyBreakPos = pretty_break_pos(BreakPos),
            io:format("\nERROR: No such lineno: ~p\n", [PrettyBreakPos]),
            I;
        FullLineNo ->
            PrettyBreakPos = pretty_break_pos(FullLineNo),
            case Type of
                temporary ->
                    io:format("\nSet temporary breakpoint at ~s\n",
                              [PrettyBreakPos]);
                next ->
                    ok;
                _ ->
                    io:format("\nSet breakpoint at ~s\n",
                              [PrettyBreakPos])
            end,
            NewBreak = #break{pos = BreakPos, type = Type},
            Breaks = replace_break(NewBreak, I#istate.breakpoints),
            I#istate{breakpoints = Breaks}
    end.

replace_break(NewBreak, Breaks) ->
    Breaks2 = delete_break(NewBreak, Breaks),
    lists:keysort(#break.pos, [NewBreak | Breaks2]).

delete_break(Break, Breaks) ->
    lists:keydelete(Break#break.pos, #break.pos, Breaks).

check_break(I, LineNo) ->
    Breaks = I#istate.breakpoints,
    case lookup_break(I, LineNo, Breaks) of
        false ->
            {true, I};
        Break ->
            Type = Break#break.type,
            CmdState = {attach, Type},
                    case Type of
                        temporary ->
                            %% Temporary breakpoint - remove it
                            {_, I2} = cmd_attach(I, [], CmdState),
                            Breaks2 = delete_break(Break, Breaks),
                            {false, I2#istate{breakpoints = Breaks2}};
                        next ->
                            %% Temporary breakpoint - remove it
                            {_, I2} = cmd_attach(I, [], CmdState),
                            Breaks2 = delete_break(Break, Breaks),
                            {false, I2#istate{breakpoints = Breaks2}};
                        enabled ->
                            %% Normal breakpoint
                            %% Disable it to not get stuck when we continue
                            {_, I2} = cmd_attach(I, [], CmdState),
                            NewBreak = Break#break{type = disabled},
                            Breaks2 = replace_break(NewBreak, Breaks),
                            {false, I2#istate{breakpoints = Breaks2}};
                        disabled ->
                            %% Normal breakpoint
                            %% Enable it again for later reuse
                            NewBreak = Break#break{type = enabled},
                            Breaks2 = replace_break(NewBreak, Breaks),
                            {true, I#istate{breakpoints = Breaks2}}
                    end
    end.

lookup_break(I, LineNo, Breaks) when is_integer(LineNo) ->
    RevFile = lux_utils:filename_split(I#istate.file), %  optimize later
    FullLineNo = [{RevFile, LineNo} | I#istate.incl_stack],
    lookup_break(I, FullLineNo, Breaks);
lookup_break(I, FullLineNo, [Break | Breaks]) when is_list(FullLineNo) ->
    case match_break(FullLineNo, Break#break.pos) of
        true  -> Break;
        false -> lookup_break(I, FullLineNo, Breaks)
    end;
lookup_break(_I, _FullLineNo, []) ->
    false.

%% BreakPos :: [non_neg_integer() | [string()] | {[string(), non_neg_integer()}
match_break([{RevFile, LineNo} | FullLineNo], [BreakPos | Break]) ->
    case match_break2(RevFile, LineNo, BreakPos) of
        true  -> match_break(FullLineNo, Break);
        false -> false
    end;
match_break(_Curr, []) ->
    true;
match_break([], _Break) ->
    false.

match_break2(_RevFile, LineNo, BreakLineNo)
  when is_integer(BreakLineNo) ->
    LineNo =:= BreakLineNo;
match_break2(RevFile, LineNo, {BreakFile, BreakLineNo})
  when is_integer(BreakLineNo) ->
    case LineNo =:= BreakLineNo of
        true  -> match_break2(RevFile, LineNo, BreakFile);
        false -> false
    end;
match_break2([FileComp | RevFile], LineNo, [BreakComp | BreakFile]) ->
    case match_break3(FileComp, BreakComp) of
        true  -> match_break2(RevFile, LineNo, BreakFile);
        false -> false
    end;
match_break2(_RevFile, _LineNo, []) ->
    true;
match_break2([], _LineNo, _BreakFile) ->
    false.

match_break3([FileChar | FileComp], [BreakChar | BreakComp]) ->
    case FileChar =:= BreakChar of
        true  -> match_break3(FileComp, BreakComp);
        false -> false
    end;
match_break3(_FileComp, []) ->
    true;
match_break3([], _BreakComp) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_continue(I, Args, _CmdState) ->
    do_continue(I, Args, _CmdState, temporary).

do_continue(I, Args, _CmdState, Type) ->
    I2 =
        case Args of
            [{"lineno", BreakPos}] ->
                add_break(I, BreakPos, Type);
            [] ->
                I
        end,
    case Type of
        next ->
            ok;
        _ ->
            io:format("\nContinue to run from ~s\n",
                      [pretty_break_pos(current_full_lineno(I))])
    end,
    case opt_unblock(I2) of
        {false, I3} ->
            {undefined, I3};
        {true, I3} ->
            I4 = lux_interpret:opt_dispatch_cmd(I3),
            {undefined, I4}
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
    {error, Ambiguous} = select(""),
    io:format("\n~s~s", [intro_help(), Ambiguous]),
    {CmdState, I};
cmd_help(I, [{_, "lineno"}], CmdState) ->
    io:format("~s", [lineno_help()]),
    {CmdState, I};
cmd_help(I, [{_, CmdName}], CmdState) ->
    case select(CmdName) of
        {ok, Cmd} ->
            Pretty = lists:flatten(pretty_cmd(Cmd)),
            io:format("\n~s\n", [Pretty]),
            {CmdState, I};
        {error, ReasonStr} ->
            io:format("\nERROR: ~s", [ReasonStr]),
            {CmdState, I}
    end.

pretty_cmd(#debug_cmd{name = Name, params = Params, help = Help}) ->
    Longest = longest(Params),
    Fun = fun(#debug_param{name = N, presence = Pres}) ->
                  case Pres of
                      optional  -> [" [", N, "]"];
                      mandatory -> [" ", N]
                  end
          end,
    Header = lists:flatten([Name, lists:map(Fun, Params)]),
    [Header,
     "\n",
     lists:duplicate(length(Header), "-"),
     "\n\n",
     Help,
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
                 io_lib:format("~p", [Min]),
                 " >= ",
                 atom_to_list(Atom),
                 " =< ",
                 io_lib:format("~p", [Max])
                ];
            Atom ->
                atom_to_list(Atom)
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
        Str when is_list(Str) -> ok
    end,
    longest2(T, lists:max([Longest, length(Str)]));
longest2([], Longest) ->
    Longest.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_tail(#istate{log_dir=LogDir, tail_status=Status} = I, [], CmdState) ->
    {ok, Cwd} =  file:get_cwd(),
    io:format("Log files at ~s:\n\n", [lux_utils:drop_prefix(Cwd, LogDir)]),
    Logs = all_logs(I),
    Print = fun(Abs, Index) ->
                    Rel = lux_utils:drop_prefix(LogDir, Abs),
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
                    io:format("~s~3w ~s~s\n", [Prefix, Index, Rel, Display]),
                    {{Abs, Curr}, Index+1}
            end,
    {Status2, _} = lists:mapfoldl(Print, 1, Logs),
    io:format("\n", []),
    TailOpts = [{"index", 2}, {"format", "compact"}, {"n_lines", 10}],
    _ = cmd_tail(I, TailOpts, CmdState),
    {undefined, I#istate{tail_status=Status2}};
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
    Logs = all_logs(I),
    case catch lists:nth(Index, Logs) of
        {'EXIT', _} ->
            io:format("ERROR: ~p is not a valid log index."
                      " Must be within ~p..~p.\n",
                      [Index, 1, length(Logs)]),
            {CmdState, I};
        LogFile ->
            tail(I, LogFile, CmdState, Format, UserN)
    end.

all_logs(#istate{orig_file=Script, log_dir=LogDir, logs=StdLogs} = I) ->
    lux_interpret:flush_logs(I),
    Split = fun({_Name, Stdin, Stdout}, Acc) -> [Stdout, Stdin | Acc] end,
    Logs = lists:reverse(lists:foldl(Split, [], StdLogs)),
    Base = filename:basename(Script),
    EventLog = filename:join([LogDir, Base ++ ".event.log"]),
    ConfigLog = filename:join([LogDir, Base ++ ".config.log"]),
    SuiteConfigLog = filename:join([LogDir, "lux_config.log"]),
    SummaryLog = filename:join([LogDir, "lux_summary.log.tmp"]),
    ResultLog = filename:join([LogDir, "lux_result.log"]),
    [SuiteConfigLog, SummaryLog, ResultLog, ConfigLog, EventLog | Logs].

tail(#istate{log_dir=LogDir} = I, AbsFile, CmdState, Format, UserN) ->
    RelFile = lux_utils:drop_prefix(LogDir, AbsFile),
    case file:read_file(AbsFile) of
        {ok, Bin} ->
            AllRows = binary:split(Bin, <<"\n">>, [global]),
            Max = length(AllRows),
            N =
                case CmdState of
                    _ when is_integer(UserN) ->
                        %% User specified N
                        UserN;
                    {debug_tail, AbsFile, PrevMin} ->
                        %% Add 10
                        PrevMin + 10;
                    _ ->
                        %% Last 10
                        10
                end,
            Min = lists:max([0, Max - N]),
            TailRows = lists:nthtail(Min, AllRows),
            Actual = length(TailRows),
            io:format("Last ~p (~p..~p) lines of log file: ~s\n\n",
                      [Actual, Max-Actual+1, Max, RelFile]),
            [tail_format(Format, "~s\n", [Row]) || Row <- TailRows],
            {{debug_tail, AbsFile, N}, I};
        {error, FileReason}->
            FileStr = file:format_error(FileReason),
            io:format("ERROR: ~s: ~s\n", [RelFile, FileStr]),
            {undefined, I}
    end.

tail_format("compact", Format, Data) ->
    io:format(Format, Data);
tail_format("verbose", Format, Data) ->
    Str = lists:flatten(io_lib:format(Format, Data)),
    io:format(lux_utils:dequote(Str)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_list(I, Args, CmdState) ->
    CurrentFullLineNo = current_full_lineno(I),
    case CmdState of
        {debug_list, OldN, OldRevFile, OldLineNo} ->
            ok;
        _ ->
            [{OldRevFile, OldLineNo} | _] = CurrentFullLineNo,
            OldN = 10
    end,
    case Args of
        [{"lineno", BreakPos}] ->
            N = OldN,
            case break_to_full_lineno(I, BreakPos) of
                false ->
                    io:format("\nERROR: No such lineno: ~p\n",
                              [pretty_break_pos(BreakPos)]),
                    {undefined, I};
                [{RevFile, First} | _] ->
                    do_list(I, RevFile, First, N, CurrentFullLineNo)
            end;
        [{"n_lines", N0}, {"lineno", BreakPos}] ->
            case break_to_full_lineno(I, BreakPos) of
                false ->
                    io:format("\nERROR: No such lineno: ~p\n",
                              [pretty_break_pos(BreakPos)]),
                    {undefined, I};
                [{RevFile, First0} | _] ->
                    if
                        N0 < 0 ->
                            N = abs(N0),
                            First = First0 - N,
                            do_list(I, RevFile, First, N, CurrentFullLineNo);
                        true ->
                            N = N0,
                            First = First0,
                            do_list(I, RevFile, First, N, CurrentFullLineNo)
                    end
            end;
        [{"n_lines", N0}] ->
            if
                N0 < 0 ->
                    N = abs(N0),
                    First = OldLineNo - N,
                    do_list(I, OldRevFile, First, N, CurrentFullLineNo);
                true ->
                    N = N0,
                    First = OldLineNo,
                    do_list(I, OldRevFile, First, N, CurrentFullLineNo)
            end;
        [] ->
            N = OldN,
            First = OldLineNo,
            do_list(I, OldRevFile, First, N, CurrentFullLineNo)
    end.

do_list(I, RevFile, First, N, [{CurrRevFile, CurrLineNo} | _]) ->
    Last = First+N-1,
    %% io:format("List source lines ~p..~p of file ~s\n",
    %%          [First, Last, pretty_file(RevFile)]),
    Print = fun(#cmd{lineno = LineNo, raw = Text}, RF, _IS, Count) ->
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
                            io:format("~p~s ~s\n", [LineNo, Delim, Text]),
                            Count+1;
                        true ->
                            Count
                    end
            end,
    _Count =
        lux_utils:foldl_cmds(Print,
                                  0,
                                  I#istate.orig_file,
                                  [],
                                  I#istate.orig_commands),
    {{debug_list, N, RevFile, Last+1}, I}.

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
            io:format("\nLoad commands from file: ~s\n", [File]),
            Fun = fun(CmdStr, {CS, IS, LineNo}) ->
                          io:format("~p: ~s\n", [LineNo, CmdStr]),
                          {CS2, IS2} = do_eval_cmd(IS, CmdStr, CS),
                          {CS2, IS2, LineNo+1}
                  end,
            Lines = string:tokens(binary_to_list(Bin), "\n"),
            {_CmdState, I2, _} = lists:foldl(Fun, {CmdState, I, 1}, Lines),
            {undefined, I2};
        {error, Reason} ->
            io:format("\nERROR: Cannot read from file ~p: ~s\n",
                      [File, file:format_error(Reason)]),
            {undefined, I}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_next(I, Args, CmdState) ->
    case Args of
        [{"n_commands", N}] ->
            ok;
        [] ->
            N = 1
    end,
    case catch lists:nth(N+1, I#istate.commands) of
        {'EXIT', _} ->
            do_continue(I, [], CmdState, next);
        #cmd{lineno = LineNo} ->
            BreakPos = [{lux_utils:filename_split(I#istate.file), LineNo}],
            do_continue(I, [{"lineno", BreakPos}], CmdState, next)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_progress(I, Args, CmdState) ->
    case Args of
        [{"level", Level0}] ->
            Level = list_to_atom(Level0);
        [] ->
            Level =
                case I#istate.progress of
                    verbose -> brief;
                    compact -> brief;
                    doc     -> verbose;
                    brief   -> verbose;
                    silent  -> verbose
                end
    end,
    lists:foreach(fun(#shell{pid = Pid}) -> Pid ! {progress, self(), Level} end,
                  I#istate.shells),
    {CmdState, I#istate{progress = Level}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_quit(I, _Args, _CmdState) ->
    io:format("\nWARNING: Stopped by user.\n", []),
    {_, I2} = opt_unblock(I),
    InterpreterPid = self(),
    InterpreterPid ! stopped_by_user,
    {undefined, I2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_save(I, Args, _CmdState) ->
    case Args of
        [{"file", File}] ->
            ok;
        [] ->
            File = "lux.debug"
    end,
    Save =
        fun(#break{pos = BreakPos, type = Type}) ->
                [
                 "break ", pretty_break_pos(BreakPos),
                 case Type of
                     enabled   -> "";
                     temporary -> " temporary";
                     next      -> " next";
                     disabled  -> ""
                 end,
                 "\n"
                ]
        end,
    IoList = lists:map(Save, I#istate.breakpoints),
    case file:write_file(File, IoList) of
        ok ->
            io:format("\nSave debugger state to file: ~s\n", [File]);
        {error, Reason} ->
            io:format("\nERROR: Cannot write to file ~p: ~s\n",
                      [File, file:format_error(Reason)])
    end,
    {undefined, I}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_skip(I, Args, _CmdState) ->
    case Args of
        [{"n_commands", N}] ->
            ok;
        [] ->
            N = 1
    end,
    {Skipped, Cmds} = skip_cmds(N, I#istate.commands, []),
    case Skipped of
        [] ->
            io:format("\nNo more lines to skip.\n", []);
        [#cmd{lineno = Single}] ->
            io:format("\nSkipped line ~p.\n", [Single]);
        [Last | Rest] ->
            First = lists:last(Rest),
            io:format("\nSkipped lines ~p..~p.\n", [First, Last])
    end,
    I2 = I#istate{commands = Cmds},
    {undefined, I2}.

skip_cmds(N, [Cmd | Cmds], Skipped) when N > 0 ->
    skip_cmds(N-1, Cmds, [Cmd | Skipped]);
skip_cmds(0, Cmds, Skipped) ->
    {Skipped, Cmds}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

current_full_lineno(I) ->
    LineNo =
        case I#istate.commands of
            [] -> 1;
            [#cmd{lineno = CurrentLineNo} | _] -> CurrentLineNo
        end,
    RevFile = lux_utils:filename_split(I#istate.file),
    [{RevFile, LineNo} | I#istate.incl_stack].

full_lineno_to_break_pos(FullLineNo) ->
    [LineNo || {_File, LineNo} <- FullLineNo].

break_to_full_lineno(I, BreakPos) ->
    Collect = fun(#cmd{lineno = LineNo}, RevFile, InclStack, Acc) ->
                      FullLineNo = [{RevFile, LineNo} | InclStack],
                      case match_break(FullLineNo, BreakPos) of
                          true  -> [FullLineNo | Acc];
                          false -> Acc
                      end
              end,
    Matching = lux_utils:foldl_cmds(Collect,
                                         [],
                                         I#istate.orig_file,
                                         [],
                                         I#istate.orig_commands),
    case catch lists:last(Matching) of
        {'EXIT', _} -> false;
        FullLineNo  -> FullLineNo
    end.

pretty_break_pos(BreakPos) ->
    do_pretty_break_pos(BreakPos, []).

do_pretty_break_pos([LineNo | BreakPos], Acc) when is_integer(LineNo) ->
    do_pretty_break_pos(BreakPos,
                        [integer_to_list(LineNo), opt_delim(Acc) | Acc]);
do_pretty_break_pos([RevFile | BreakPos], Acc) when is_list(RevFile) ->
    do_pretty_break_pos(BreakPos, [pretty_file(RevFile), opt_delim(Acc) | Acc]);
do_pretty_break_pos([{RevFile, LineNo} | BreakPos], Acc)
  when is_list(RevFile), is_integer(LineNo) ->
    do_pretty_break_pos(BreakPos,
                        [[integer_to_list(LineNo), "@", pretty_file(RevFile)],
                         opt_delim(Acc) | Acc]);
do_pretty_break_pos([], Acc) ->
    lists:flatten(lists:reverse(Acc)).

opt_delim(Acc) ->
    case Acc of
        [] -> "";
        _  -> ":"
    end.

pretty_file(RevFile) ->
    filename:join(lists:reverse(RevFile)).
