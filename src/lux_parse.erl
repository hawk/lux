%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2015 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_parse).

-export([parse_file/4]).

-include("lux.hrl").

-record(pstate,
        {file      :: string(),
         mode      :: run_mode(),
         skip_skip :: boolean(),
         dicts     :: [[string()]]}). % ["name=val"][]}).

-define(TAB_LEN, 8).
-define(FF(Format, Args), io_lib:format(Format, Args)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse

parse_file(RelFile, RunMode, SkipSkip, Opts) ->
    try
        File = filename:absname(RelFile),
        DefaultI = lux_interpret:default_istate(File),
        case lux_interpret:parse_iopts(DefaultI, Opts) of
            {ok, I} ->
                Dicts =
                    [I#istate.global_dict,
                     I#istate.builtin_dict,
                     I#istate.system_dict],
                P = #pstate{file = File,
                            mode = RunMode,
                            skip_skip = SkipSkip,
                            dicts = Dicts},
                {_FirstLineNo, _LastLineNo, Cmds} = parse_file2(P),
                garbage_collect(),
                Config = lux_utils:foldl_cmds(fun extract_config/4,
                                              [], File, [], Cmds),
                case parse_config(I, lists:reverse(Config)) of
                    {ok, I2} ->
                        UpdatedOpts = updated_opts(I2, I),
                        %%io:format("PARSE: ~s\n\t~p\n", [RelFile,UpdatedOpts]),
                        {ok, I2#istate.file, Cmds, UpdatedOpts};
                    {error, Reason} ->
                        {error, [{File, 0, main}], Reason}
                end;
            {error, Reason} ->
                {error, [{File, 0, main}], Reason}
        end
    catch
        throw:{error, ErrorStack, ErrorBin} ->
            {error, ErrorStack, ErrorBin};
        throw:{skip, ErrorStack, ErrorBin} ->
            {skip, ErrorStack, ErrorBin}
    end.

extract_config(Cmd, _RevFile, _CmdStack, Acc) ->
    case Cmd of
        #cmd{type = config, arg = {config, Var, Val}} ->
            Name = list_to_atom(Var),
            case lists:keyfind(Name, 1, Acc) of
                false ->
                    [{Name, [Val]} | Acc];
                {_, OldVals} ->
                    [{Name, OldVals ++ [Val]} | Acc]
            end;
        #cmd{} ->
            Acc
    end.

parse_config(I0, Config) ->
    Fun =
        fun({Name, Vals}, {ok, I}) ->
                case lux_interpret:config_type(Name) of
                    {ok, Pos, Types = [{env_list, _}]} ->
                        lux_interpret:set_config_val(Name, Vals, Types, Pos, I);
                    {ok, Pos, Types = [{reset_list, _}]} ->
                        lux_interpret:set_config_val(Name, Vals, Types, Pos, I);
                    {ok, Pos, Types} ->
                        Val = lists:last(Vals),
                        lux_interpret:set_config_val(Name, Val, Types, Pos, I);
                    {error, Reason} ->
                        {error, Reason}
                end;
           (_, {error, Reason}) ->
                {error, Reason}
        end,
    lists:foldl(Fun, {ok, I0}, Config).

updated_opts(I, DefaultI) ->
    Candidates =
        [
         {debug, #istate.debug},
         {skip, #istate.skip},
         {skip_unless, #istate.skip_unless},
         {require, #istate.require},
         {config_dir, #istate.config_dir},
         {progress, #istate.progress},
         {log_dir, #istate.log_dir},
         {log_fun, #istate.log_fun},
         {multiplier, #istate.multiplier},
         {suite_timeout, #istate.suite_timeout},
         {case_timeout,#istate.case_timeout},
         {flush_timeout,#istate.flush_timeout},
         {poll_timeout,#istate.poll_timeout},
         {timeout, #istate.timeout},
         {cleanup_timeout, #istate.cleanup_timeout},
         {shell_wrapper, #istate.shell_wrapper},
         {shell_cmd, #istate.shell_cmd},
         {shell_args, #istate.shell_args},
         {shell_prompt_cmd, #istate.shell_prompt_cmd},
         {shell_prompt_regexp, #istate.shell_prompt_regexp},
         {var, #istate.global_dict},
         {system_env, #istate.system_dict}
        ],
    Filter = fun({Tag, Pos}) ->
                     Old = element(Pos, DefaultI),
                     New = element(Pos, I),
                     case Old =/= New of
                         false -> false;
                         true  -> {true, {Tag, New}}
                     end
             end,
    lists:zf(Filter, Candidates).

parse_file2(P) ->
    case file_open(P) of
        {ok, Fd} ->
            FirstLineNo = 1,
            {eof, RevCmds} = parse(P, Fd, FirstLineNo, []),
            Cmds = lists:reverse(RevCmds),
            %% io:format("Cmds: ~p\n", [Cmds]),
            case Cmds of
                [#cmd{lineno = LastLineNo} | _] -> ok;
                []                              -> LastLineNo = 1
            end,
            {FirstLineNo, LastLineNo, Cmds};
        {error, FileReason} ->
            NewFd = eof,
            parse_error(P, NewFd, 0, file:format_error(FileReason))
    end.

file_open(Lines) when is_list(Lines) ->
    [{read_ahead, Lines}];
file_open(#pstate{file = File, mode = RunMode}) ->
    do_file_open(File, RunMode).

do_file_open( File, RM) when RM =:= validate; RM =:= execute ->
    %% Bulk read file
    case file:read_file(File) of
        {ok, Bin} ->
            Bins = re:split(Bin, <<"\n">>),
            {ok, [{read_ahead, Bins}]};
        {error, FileReason} ->
            {error, FileReason}
    end;
do_file_open(File, RM) when RM =:= doc; RM =:= list ->
    %% Read lines on demand
    case file:open(File, [raw, binary, read_ahead]) of
        {ok, Io} ->
            {ok, [{open_file, Io, chopped}]};
        {error, FileReason} ->
            {error, FileReason}
    end.

file_close([{read_ahead, _Bins} | Rest]) ->
    file_close(Rest);
file_close([{open_file, Io, _} | Rest]) ->
    file:close(Io),
    file_close(Rest);
file_close([]) ->
    eof;
file_close(eof) ->
    eof.

file_push(Fd, []) ->
    Fd;
file_push([{read_ahead, Lines} | Fd], [Line]) ->
    [{read_ahead, [Line | Lines]} | Fd];
file_push(Fd, MoreLines) ->
    [{read_ahead, MoreLines} | Fd].

file_next([{read_ahead, Lines} | Rest]) ->
    case Lines of
        [H | T] ->
            {line, H, [{read_ahead, T} | Rest]};
        [] ->
            file_next(Rest)
    end;
file_next([{open_file, Io, Trailing} | Rest]) ->
    case file:read_line(Io) of
        {ok, Line} ->
            %% Chop newline
            Line2 = re:replace(Line, "\n$", "", [{return, binary}]),
            if
                byte_size(Line) =/= byte_size(Line2) ->
                    %% Trailing newline
                    {line, Line2, [{open_file, Io, newline} | Rest]};
                true ->
                    {line, Line2, [{open_file, Io, chopped} | Rest]}
            end;
        eof when Trailing =:= newline ->
            file:close(Io),
            {line, <<>>, Rest};
        eof when Trailing =:= chopped ->
            file:close(Io),
            file_next(Rest);
        {error, _Reason} ->
            file:close(Io),
            file_next(Rest)
    end;
file_next([]) ->
    eof;
file_next(eof) ->
    eof.

file_takewhile(Fd, Pred, Acc) ->
    case file_next(Fd) of
        {line, Line, NewFd} ->
            case Pred(Line) of
                true  ->
                    file_takewhile(NewFd, Pred, [Line|Acc]);
                {true, NewLine}  ->
                    file_takewhile(NewFd, Pred, [NewLine|Acc]);
                false ->
                    {file_push(NewFd, [Line]), lists:reverse(Acc)}
            end;
        eof = NewFd ->
            {NewFd, lists:reverse(Acc)}
    end.

%% Read until first line without trailing backslash
file_next_wrapper(Fd) ->
    {BackslashFd, Lines} = backslash_takewhile(Fd),
    case file_next(BackslashFd) of
        {line, Line, NewFd} when Lines =:= [] ->
            {line, Line, NewFd, 0};
        {line, Line, NewFd} ->
            ComboLine = iolist_to_binary([Lines, Line]),
            {line, ComboLine, NewFd, length(Lines)};
        eof when Lines =:= [] ->
            eof;
        eof ->
            ComboLine = iolist_to_binary(Lines),
            {line, ComboLine, BackslashFd, length(Lines)}
    end.

backslash_takewhile(Fd) ->
    Pred = fun(Line) -> backslash_filter(Line) end,
    file_takewhile(Fd, Pred, []).

backslash_filter(Line) ->
    Sz = byte_size(Line) - 1,
    case Line of
        <<Chopped:Sz/binary, "\\">> ->
            %% Found trailing backslash
            {true, Chopped};
        _ ->
            false
    end.

parse(P, Fd, LineNo, Tokens) ->
    case file_next_wrapper(Fd) of
        {line, OrigLine, NewFd, Incr} ->
            Line = lux_utils:strip_leading_whitespaces(OrigLine),
            parse_cmd(P, NewFd, Line, LineNo, Incr, OrigLine, Tokens);
        eof = NewFd ->
            {NewFd, Tokens}
    end.

parse_cmd(P, Fd, <<>>, LineNo, Incr, OrigLine, Tokens) ->
    Token = #cmd{type = comment, lineno = LineNo, orig = OrigLine},
    parse(P, Fd, LineNo+Incr+1, [Token | Tokens]);
parse_cmd(P, Fd, Line, LineNo, Incr, OrigLine, Tokens) ->
    RunMode = P#pstate.mode,
    <<Op:8/integer, Data/binary>> = Line,
    Type = parse_oper(P, Fd, Op, LineNo, Line),
    Cmd = #cmd{type = Type, lineno = LineNo, orig = OrigLine},
    if
        Type =:= meta ->
            parse_meta(P, Fd, Data, Cmd, Tokens);
        Type =:= multi_line ->
            parse_multi(P, Fd, Data, Cmd, Tokens);
        RunMode =:= validate; RunMode =:= execute ->
            Cmd2 = parse_single(Op, Cmd, Data),
            parse(P, Fd, LineNo+Incr+1, [Cmd2 | Tokens]);
        RunMode =:= list; RunMode =:= doc ->
            %% Skip command
            parse(P, Fd, LineNo+Incr+1, Tokens)
    end.

parse_oper(P, Fd, Op, LineNo, OrigLine) ->
    case Op of
        $!  -> send_lf;
        $~  -> send;
        $?  -> expect;
        $.  -> expect;
        $-  -> fail;
        $+  -> success;
        $[  -> meta;
        $"  -> multi_line;
        $#  -> comment;
        _   ->
            parse_error(P, Fd, LineNo,
                        ["Syntax error at line ", integer_to_list(LineNo),
                         ": '", OrigLine, "'"])
    end.

parse_single(Op, #cmd{type = Type} = Cmd, Data) ->
    case Type of
        send_lf                    -> Cmd#cmd{arg = Data};
        send                       -> Cmd#cmd{arg = Data};
        expect when Op =:= $.      -> parse_regexp(Cmd, shell_exit);
        expect when Data =:= <<>>  -> parse_regexp(Cmd, reset);
        expect                     -> parse_regexp(Cmd, Data);
        fail when Data =:= <<>>    -> parse_regexp(Cmd, reset);
        fail                       -> parse_regexp(Cmd, Data);
        success when Data =:= <<>> -> parse_regexp(Cmd, reset);
        success                    -> parse_regexp(Cmd, Data);
%%      meta                       -> Cmd;
%%      multi_line                 -> Cmd;
        comment                    -> Cmd
    end.

%% Arg :: shell_exit           |
%%        reset                |
%%        {verbatim, binary()} |
%%        {template, binary()} |
%%        {regexp, binary}     |
%%        {mp, binary(), mp()}   (compliled later)
parse_regexp(Cmd, RegExp) when is_binary(RegExp) ->
    case lux_utils:strip_trailing_whitespaces(RegExp) of
        <<$?:8/integer, $?:8/integer, Stripped/binary>>
          when Cmd#cmd.type =/= fail,
               Cmd#cmd.type =/= success ->
            Cmd#cmd{arg = {verbatim, Stripped}};
        <<$?:8/integer, Stripped/binary>>
          when Cmd#cmd.type =/= fail,
               Cmd#cmd.type =/= success ->
            Cmd#cmd{arg = {template, Stripped}};
        Stripped ->
            Cmd#cmd{arg = {regexp, Stripped}}
    end;
parse_regexp(Cmd, Value) when Value =:= shell_exit;
                              Value =:= reset ->
    Cmd#cmd{arg = Value}.

parse_var(P, Fd, Cmd, Scope, String) ->
    case split_var(String, []) of
        {Var, Val} ->
            Cmd#cmd{type = variable, arg = {Scope, Var, Val}};
        false ->
            LineNo = Cmd#cmd.lineno,
            parse_error(P, Fd, LineNo,
                        ["Syntax error at line ", integer_to_list(LineNo),
                         ": illegal ", atom_to_list(Scope),
                         " variable "," '", String, "'"])
    end.

split_var([$= | Val], Var) ->
    {lists:reverse(Var), Val};
split_var([H | T], Var) ->
    split_var(T, [H | Var]);
split_var([], _Var) ->
    false.

parse_meta(P, Fd, Data, #cmd{lineno = LineNo} = Cmd, Tokens) ->
    ChoppedData = lux_utils:strip_trailing_whitespaces(Data),
    MetaSize = byte_size(ChoppedData) - 1,
    case ChoppedData of
        <<Meta:MetaSize/binary, "]">> ->
            MetaCmd = parse_meta_token(P, Fd, Cmd, Meta, LineNo),
            {NewFd, NewLineNo, NewCmd} =
                case MetaCmd#cmd.type of
                    macro ->
                        parse_body(P, Fd, MetaCmd, "endmacro", LineNo);
                    loop ->
                        parse_body(P, Fd, MetaCmd, "endloop", LineNo);
                    _ ->
                        {Fd, LineNo, MetaCmd}
                end,
            RunMode = P#pstate.mode,
            NewType = NewCmd#cmd.type,
            NewTokens =
                if
                    NewType =:= config ->
                        [NewCmd | Tokens];
                    NewType =:= include  ->
                        [NewCmd | Tokens];
                    NewType =:= doc, P#pstate.mode =/= list ->
                        [NewCmd | Tokens];
                    RunMode =:= list; RunMode =:= doc ->
                        %% Skip command
                        Tokens;
                    RunMode =:= validate; RunMode =:= execute ->
                        [NewCmd | Tokens]
                end,
            parse(P, NewFd, NewLineNo+1, NewTokens);
        _ ->
            parse_error(P, Fd, LineNo,
                        ["Syntax error at line ", integer_to_list(LineNo),
                         ": ']' is expected to be at end of line"])
    end.

parse_body(#pstate{mode = RunMode} = P,
           Fd,
           #cmd{arg = {body, Tag, Name, Items}} = Cmd,
           EndKeyword,
           LineNo) ->
    {ok, MP} = re:compile("^[\s\t]*\\[" ++ EndKeyword ++ "\\]"),
    Pred = fun(L) -> re:run(L, MP, [{capture, none}]) =:= nomatch end,
    {FdAfter, BodyLines0} = file_takewhile(Fd, Pred, []),
    {BodyLines, BodyIncr} = merge_body(BodyLines0, [], [], 0),
    case file_next_wrapper(FdAfter) of
        eof = NewFd ->
            parse_error(P, NewFd, LineNo,
                        ["Syntax error after line ",
                         integer_to_list(LineNo),
                         ": [" ++ EndKeyword ++ "] expected"]);
        {line, _EndMacro, NewFd, Incr}
          when RunMode =:= list; RunMode =:= doc ->
            %% Do not parse body
            BodyLen = length(BodyLines)+BodyIncr,
            LastLineNo = LineNo+Incr+BodyLen+1,
            {NewFd, LastLineNo, Cmd#cmd{arg = undefined}};
        {line, _EndMacro, NewFd, Incr}
          when RunMode =:= validate; RunMode =:= execute ->
            %% Parse body
            BodyLen = length(BodyLines)+BodyIncr,
            FdBody = file_open(BodyLines),
            {eof, RevBodyCmds} = parse(P, FdBody, LineNo+Incr+1, []),
            BodyCmds = lists:reverse(RevBodyCmds),
            LastLineNo = LineNo+Incr+BodyLen+1,
            Arg = {Tag, Name, Items, LineNo, LastLineNo, BodyCmds},
            {NewFd, LastLineNo, Cmd#cmd{arg = Arg}}
    end.

merge_body([Line | Lines], Acc, Pending, Decr) ->
    case backslash_filter(Line) of
        {true, Chopped} ->
            merge_body(Lines, Acc, [Chopped | Pending], Decr+1);
        false ->
            NewLine = iolist_to_binary([lists:reverse(Pending), Line]),
            merge_body(Lines, [NewLine | Acc], [], Decr)
    end;
merge_body([], Acc, [], Decr) ->
    {lists:reverse(Acc), Decr};
merge_body([], Acc, Pending, Decr) ->
    NewLine = iolist_to_binary(lists:reverse(Pending)),
    {lists:reverse([NewLine | Acc]), Decr}.

parse_meta_token(P, Fd, Cmd, Meta, LineNo) ->
    case binary_to_list(Meta) of
        "doc" ++ Text ->
            Text2 =
                case Text of
                    [$\   | _Text] ->
                        "1" ++ Text;
                    [Char | _Text] when Char >= $0, Char =< $9 ->
                        Text;
                    _Text ->
                        "1 " ++Text
                end,
            Pred = fun(Char) -> Char =/= $\  end,
            {LevelStr, Text3} = lists:splitwith(Pred, Text2),
            try
                Level = list_to_integer(LevelStr),
                if Level > 0 -> ok end, % assert
                Doc = list_to_binary(string:strip(Text3)),
                Cmd#cmd{type = doc, arg = {Level, Doc}}
            catch
                error:_ ->
                    parse_error(P, Fd, LineNo,
                                ["Illegal prefix of doc string" ,
                                 Text2, " on line ",
                                 integer_to_list(LineNo)])
            end;
        "cleanup" ++ Name ->
            Cmd#cmd{type = cleanup, arg = string:strip(Name)};
        "shell" ++ Name ->
            Name2 = string:strip(Name),
            Match = re:run(Name2, "\\$\\$", [{capture,none}]),
            case {Name2, Match} of
                {"", _} ->
                    parse_error(P, Fd, LineNo,
                                ?FF("Syntax error at line ~p"
                                    ": missing shell name",
                                    [LineNo]));
                {"lux"++_, _} ->
                    parse_error(P, Fd, LineNo,
                                ?FF("Syntax error at line ~p"
                                    ": ~s is a reserved"
                                    " shell name",
                                    [LineNo, Name2]));
                {"cleanup"++_, _} ->
                    parse_error(P, Fd, LineNo,
                                ?FF("Syntax error at line ~p"
                                    ": ~s is a reserved"
                                    " shell name",
                                    [LineNo, Name2]));
                {_, match} ->
                    parse_error(P, Fd, LineNo,
                                ?FF("Syntax error at line ~p"
                                    ": $$ in shell name",
                                    [LineNo]));
                {_, nomatch} ->
                    Cmd#cmd{type = shell, arg = Name2}
            end;
        "endshell" ->
            Cmd2 = Cmd#cmd{type = expect, orig = <<".">>},
            parse_regexp(Cmd2, shell_exit);
        "config" ++ VarVal ->
            ConfigCmd = parse_var(P, Fd, Cmd, config, string:strip(VarVal)),
            {Scope, Var, Val} = ConfigCmd#cmd.arg,
            Val2 = expand_vars(P, Fd, Val, LineNo),
            ConfigCmd2 = ConfigCmd#cmd{type = config, arg = {Scope, Var, Val2}},
            test_skip(P, Fd, ConfigCmd2);
        "my" ++ VarVal ->
            parse_var(P, Fd, Cmd, my, string:strip(VarVal));
        "local" ++ VarVal ->
            parse_var(P, Fd, Cmd, local, string:strip(VarVal));
        "global" ++ VarVal ->
            parse_var(P, Fd, Cmd, global, string:strip(VarVal));
        "timeout" ++ Time ->
            Cmd#cmd{type = change_timeout, arg = string:strip(Time)};
        "sleep" ++ Time ->
            Cmd#cmd{type = sleep, arg = string:strip(Time)};
        "progress" ++ String ->
            Cmd#cmd{type = progress, arg = string:strip(String)};
        "include" ++ File ->
            File2 = expand_vars(P, Fd, File, LineNo),
            InclFile = filename:absname(string:strip(File2),
                                        filename:dirname(P#pstate.file)),
            try
                {FirstLineNo, LastLineNo, InclCmds} =
                    parse_file2(P#pstate{file=InclFile}),
                Cmd#cmd{type = include,
                        arg = {include,InclFile,FirstLineNo,
                               LastLineNo,InclCmds}}
            catch
                throw:{skip, ErrorStack, Reason} ->
                    %% re-throw
                    parse_error(P, Fd, skip, LineNo, Reason, ErrorStack);
                throw:{error, ErrorStack, Reason} ->
                    %% re-throw
                    parse_error(P, Fd, error, LineNo, Reason, ErrorStack)
            end;
        "macro" ++ Head ->
            case string:tokens(string:strip(Head), " ") of
                [Name | ArgNames] ->
                    Cmd#cmd{type = macro, arg = {body, macro, Name, ArgNames}};
                [] ->
                    parse_error(P, Fd, LineNo,
                                ["Syntax error at line ",
                                 integer_to_list(LineNo),
                                 ": missing macro name"])
            end;
        "invoke" ++ Head ->
            case split_invoke_args(P, Fd, LineNo, Head, normal, [], []) of
                [Name | ArgVals] ->
                    Cmd#cmd{type = invoke, arg = {invoke, Name, ArgVals}};
                [] ->
                    parse_error(P, Fd, LineNo,
                                ["Syntax error at line ",
                                 integer_to_list(LineNo),
                                 ": missing macro name"])
            end;
        "loop" ++ Head ->
            Pred = fun(Char) -> Char =/= $\ end,
            case lists:splitwith(Pred, string:strip(Head)) of
                {Var, Items0} when Var =/= "" ->
                    Items = string:strip(Items0, left),
                    Cmd#cmd{type = loop, arg = {body, loop, Var, Items}};
                _ ->
                    parse_error(P, Fd, LineNo,
                                ["Syntax error at line ",
                                 integer_to_list(LineNo),
                                 ": missing loop variable"])
            end;
        Bad ->
            parse_error(P, Fd, LineNo,
                        ["Syntax error at line ",
                         integer_to_list(LineNo),
                         ": Unknown meta command '",
                         Bad, "'"])
    end.

test_skip(#pstate{mode = RunMode, skip_skip = SkipSkip} = P, Fd,
          #cmd{lineno = LineNo, arg = {config, Var, NameVal}} = Cmd) ->
    case Var of
        "skip" when not SkipSkip ->
            {IsSet, Name} = test_variable(P, NameVal),
            case IsSet of
                false ->
                    Cmd;
                true ->
                    Reason = "SKIP as variable ~s is set",
                    parse_skip(P, Fd, LineNo,?FF(Reason, [Name]))
            end;
        "skip_unless" when not SkipSkip ->
            {IsSet, Name} = test_variable(P, NameVal),
            case IsSet of
                true ->
                    Cmd;
                false ->
                    Reason = "SKIP as variable ~s is not set",
                    parse_skip(P, Fd, LineNo, ?FF(Reason, [Name]))
            end;
        "require" when not SkipSkip, RunMode =:= execute ->
            {IsSet, Name} = test_variable(P, NameVal),
            case IsSet of
                true ->
                    Cmd;
                false ->
                    Reason = "FAIL as required variable ~s is not set",
                    parse_skip(P, Fd, LineNo, ?FF(Reason, [Name]))
            end;
        _ ->
            Cmd
    end.

test_variable(P, VarVal) ->
    case split_var(VarVal, []) of
        {Var, Val} ->
            ok;
        false ->
            Var = VarVal,
            Val = false
    end,
    UnExpanded = [$$ | Var],
    try
        Expanded = lux_utils:expand_vars(P#pstate.dicts, UnExpanded, error),
        %% Variable is set
        if
            Val =:= false ->
                %% Variable exists
                {true, Var};
            Val =:= Expanded ->
                %% Value matches. Possible empty.
                {true, Var};
            true ->
                %% Value does not match
                {false, Var}
        end
    catch
        throw:{no_such_var, _} ->
            %% Variable is not set
            {false, Var}
    end.

expand_vars(P, Fd, Val, LineNo) ->
    try
        lux_utils:expand_vars(P#pstate.dicts, Val, error)
    catch
        throw:{no_such_var, BadVar} ->
            Reason = ["Variable $", BadVar,
                      " is not set on line ",
                      integer_to_list(LineNo)],
            parse_error(P, Fd, LineNo, Reason);
        error:Reason ->
            erlang:error(Reason)
    end.

split_invoke_args(P, Fd, LineNo, [], quoted, Arg, _Args) ->
    parse_error(P, Fd, LineNo,
                ["Syntax error at line ",
                 integer_to_list(LineNo),
                 ": Unterminated quote '",
                 lists:reverse(Arg), "'"]);
split_invoke_args(_P, _ArgsFd, _LineNo, [], normal, [], Args) ->
    lists:reverse(Args);
split_invoke_args(_P, _Fd, _LineNo, [], normal, Arg, Args) ->
    lists:reverse([lists:reverse(Arg) | Args]);
split_invoke_args(P, Fd, LineNo, [H | T], normal = Mode, Arg, Args) ->
    case H of
        $\" -> % quote begin
            split_invoke_args(P, Fd, LineNo, T, quoted, Arg, Args);
        $\  when Arg =:= [] -> % skip space between args
            split_invoke_args(P, Fd, LineNo, T, Mode, Arg, Args);
        $\  when Arg =/= [] -> % first space after arg
            Arg2 = lists:reverse(Arg),
            split_invoke_args(P, Fd, LineNo, T, Mode, [], [Arg2 | Args]);
        $\\ when hd(T) =:= $\\ ; hd(T) =:= $\" -> % escaped char
            split_invoke_args(P, Fd, LineNo, tl(T), Mode, [hd(T) | Arg], Args);
        Char ->
            split_invoke_args(P, Fd, LineNo, T, Mode, [Char | Arg], Args)
    end;
split_invoke_args(P, Fd, LineNo, [H | T], quoted = Mode, Arg, Args) ->
    case H of
        $\" -> % quote end
            Arg2 = lists:reverse(Arg),
            split_invoke_args(P, Fd, LineNo, T, normal, [], [Arg2 | Args]);
        $\\ when hd(T) =:= $\\; hd(T) =:= $\" ->  % escaped char
            split_invoke_args(P, Fd, LineNo, tl(T), Mode, [hd(T) | Arg], Args);
        Char ->
            split_invoke_args(P, Fd, LineNo, T, Mode, [Char | Arg], Args)
    end.

parse_multi(#pstate{mode = RunMode} = P,
            Fd,
            <<$":8/integer, $":8/integer, Chars/binary>>,
            #cmd{lineno = LineNo, orig = OrigLine},
            Tokens) ->
    PrefixLen = count_prefix_len(binary_to_list(OrigLine), 0),
    {RevBefore, FdAfter, RemPrefixLen, MultiIncr} =
        scan_multi(Fd, PrefixLen, [], 0),
    LastLineNo0 = LineNo+MultiIncr+length(RevBefore)+1,
    case file_next_wrapper(FdAfter) of
        eof = NewFd ->
            parse_error(P, NewFd, LastLineNo0,
                        ["Syntax error after line ",
                         integer_to_list(LineNo),
                         ": '\"\"\"' expected"]);
        {line, _, NewFd, Incr} when RemPrefixLen =/= 0 ->
            LastLineNo = LastLineNo0+Incr,
            parse_error(P, NewFd, LastLineNo,
                        ["Syntax error at line ", integer_to_list(LastLineNo),
                         ": multi line block must end in same column as"
                         " it started on line ", integer_to_list(LineNo)]);
        {line, _EndOfMulti, NewFd, Incr}
          when RunMode =:= list; RunMode =:= doc ->
            %% Skip command
            LastLineNo = LastLineNo0+Incr,
            parse(P, NewFd, LastLineNo+1, Tokens);
        {line, _EndOfMulti, Fd2, Incr}
          when RunMode =:= validate; RunMode =:= execute ->
            %% Join all lines with a newline as separator
            Blob =
                case RevBefore of
                    [Single] ->
                        Single;
                    [Last | Other] ->
                        Join =
                            fun(F, L) ->
                                    <<F/binary, <<"\n">>/binary, L/binary>>
                            end,
                        lists:foldl(Join, Last, Other);
                    [] ->
                        <<"">>
                end,
            MultiLine = <<Chars/binary, Blob/binary>>,
            LastLineNo = LastLineNo0+Incr,
            {NewFd, Tokens2} =
                parse_cmd(P, Fd2, MultiLine, LastLineNo, 0, OrigLine, Tokens),
            parse(P, NewFd, LastLineNo+1, Tokens2)
    end;
parse_multi(P, Fd, _, #cmd{lineno = LineNo}, _Tokens) ->
    parse_error(P, Fd, LineNo,
                ["Syntax error at line ", integer_to_list(LineNo),
                 ": '\"\"\"' command expected"]).

count_prefix_len([H | T], N) ->
    case H of
        $\  -> count_prefix_len(T, N+1);
        $\t -> count_prefix_len(T, N+?TAB_LEN);
        $"  -> N
    end.

scan_multi(Fd, PrefixLen, Acc, Incr0) ->
    case file_next_wrapper(Fd) of
        {line, Line, NewFd, Incr} ->
            case scan_single(Line, PrefixLen) of
                {more, Line2} ->
                    scan_multi(NewFd, PrefixLen, [Line2 | Acc], Incr0+Incr);
                {nomore, RemPrefixLen} ->
                    {Acc, file_push(NewFd, [Line]), RemPrefixLen, Incr0+Incr}
            end;
        eof = NewFd->
            {Acc, NewFd, PrefixLen, Incr0}
    end.

scan_single(Line, PrefixLen) ->
    case Line of
        <<"\"\"\"", _Rest/binary>> ->
            {nomore, PrefixLen};
        _ when PrefixLen =:= 0 ->
            {more, Line};
        <<" ", Rest/binary>> ->
            scan_single(Rest, PrefixLen - 1);
        <<"\t", Rest/binary>> ->
            Left = PrefixLen - ?TAB_LEN,
            if
                Left < 0 -> % Too much leading whitespace
                    Spaces = lists:duplicate(abs(Left), $\ ),
                    {more, iolist_to_binary([Spaces, Line])};
                true ->
                    scan_single(Rest, Left)
            end;
        _ ->
            {more, Line}
    end.

parse_skip(File, Fd, LineNo, IoList) ->
    parse_error(File, Fd, skip, LineNo, IoList, []).

parse_error(File, Fd, LineNo, IoList) ->
    parse_error(File, Fd, LineNo, IoList, []).

parse_error(StateOrFile, Fd, LineNo, IoList, Stack) ->
    parse_error(StateOrFile, Fd, error, LineNo, IoList, Stack).

parse_error(StateOrFile, Fd, Tag, LineNo, IoList, Stack) ->
    file_close(Fd),
    File = state_to_file(StateOrFile),
    Context =
        case Stack of
            [] -> main;
            _  -> include
        end,
    throw({Tag, [{File, LineNo, Context} | Stack],
           iolist_to_binary(IoList)}).

state_to_file(File) when is_list(File) -> File;
state_to_file(#pstate{file = File})    -> File;
state_to_file(#istate{file = File})    -> File.
