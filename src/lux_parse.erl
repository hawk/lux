%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2012 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_parse).

-export([parse_file/2]).

-include("lux.hrl").

-record(pstate,
        {file :: string(),
         dict :: [string()]}). % ["name=val"][]}).

-define(TAB_LEN, 8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse

parse_file(RelFile, Opts) ->
    try
        File = filename:absname(RelFile),
        DefaultI = lux_interpret:default_istate(File),
        case lux_interpret:parse_iopts(DefaultI, Opts) of
            {ok, I} ->
                Dict = I#istate.dict ++
                       I#istate.builtin_dict ++
                       I#istate.system_dict,
                P = #pstate{file = File, dict = Dict},
                {_FirstLineNo, _LastLineNo, Cmds} = parse_file2(P),
                Config = lux_utils:foldl_cmds(fun extract_config/4,
                                                   [], File, [], Cmds),
                case parse_config(I, lists:reverse(Config)) of
                    {ok, I2} ->
                        Opts2 = updated_opts(I2, DefaultI),
                        {ok, I2#istate.file, Cmds, Opts2};
                    {error, Reason} ->
                        {error, File, "0", Reason}
                end;
            {error, Reason} ->
                {error, File, "0", Reason}
        end
    catch
        throw:{error, File2, LineNo, ErrorBin} ->
            {error, File2, integer_to_list(LineNo), ErrorBin}
    end.

extract_config(Cmd, _RevFile, _InclStack, Acc) ->
    case Cmd of
        #cmd{type = config, arg = {config, Var, Val}} ->
            Name = list_to_atom(Var),
            case lists:keyfind(Name, 1, Acc) of
                false ->
                    [{Name, [Val]} | Acc];
                {_, OldVals} ->
                    [{Name, [Val | OldVals]} | Acc]
            end;
        #cmd{} ->
            Acc
    end.

parse_config(I, [{Name, Vals} | T]) ->
    Val =
        case lists:member(Name, [skip, require, var, shell_args]) of
            true  -> Vals;
            false -> lists:last(Vals)
        end,
    case lux_interpret:parse_iopt(I, Name, Val) of
        {ok, I2} ->
            parse_config(I2, T);
        {error, Reason} ->
            {error, Reason}
    end;
parse_config(I, []) ->
    {ok, I}.

updated_opts(I, DefaultI) ->
    Candidates =
        [
         {debug, #istate.debug},
         {skip, #istate.skip},
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
         {var, #istate.dict},
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
    case file:read_file(P#pstate.file) of
        {ok, Bin} ->
            Bins = re:split(Bin, <<"\n">>),
            FirstLineNo = 1,
            Commands = parse(P, Bins, FirstLineNo, []),
            case Commands of
                [#cmd{lineno = LastLineNo} | _] -> ok;
                [] -> LastLineNo = 1
            end,
            {FirstLineNo, LastLineNo, lists:reverse(Commands)};
        {error, Reason} ->
            parse_error(P, file:format_error(Reason), 0)
    end.

parse(P, [OrigLine | Lines], LineNo, Tokens) ->
    Line = lux_utils:strip_leading_whitespaces(OrigLine),
    do_parse(P, [Line | Lines], LineNo, OrigLine, Tokens);
parse(_P, [], _LineNo, Tokens) ->
    Tokens.

do_parse(P, [<<>> = Raw | Lines], LineNo, _OrigLine, Tokens) ->
    Token = #cmd{type = comment, lineno = LineNo, raw = Raw},
    parse(P, Lines, LineNo+1, [Token | Tokens]);
do_parse(P,
         [<<Op:8/integer, Bin/binary>> = Raw | Lines],
         LineNo, OrigLine, Tokens) ->
    Type = parse_oper(P, Op, LineNo, Raw),
    Cmd = #cmd{type = Type, lineno = LineNo, raw = Raw},
    Cmd2 =
        case Type of
            send_lf                   -> Cmd#cmd{arg = Bin};
            send                      -> Cmd#cmd{arg = Bin};
            expect when Op =:= $.     -> parse_regexp(Cmd, shell_exit);
            expect when Bin =:= <<>>  -> parse_regexp(Cmd, reset);
            expect                    -> parse_regexp(Cmd, Bin);
            fail when Bin =:= <<>>    -> parse_regexp(Cmd, reset);
            fail                      -> parse_regexp(Cmd, Bin);
            success when Bin =:= <<>> -> parse_regexp(Cmd, reset);
            success                   -> parse_regexp(Cmd, Bin);
            meta                      -> Cmd;
            multi_line                -> Cmd;
            comment                   -> Cmd
        end,
    case Type of
        meta       -> parse_meta(P, Bin, Cmd2, Lines, Tokens);
        multi_line -> parse_multi(P, Bin, Cmd2, Lines, OrigLine, Tokens);
        _          -> parse(P, Lines, LineNo+1, [Cmd2 | Tokens])
    end;
do_parse(_P, [], _LineNo, _OrigLine, Tokens) ->
    Tokens.

parse_oper(P, Op, LineNo, Raw) ->
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
        _   -> parse_error(P,
                           ["Syntax error at line ", integer_to_list(LineNo),
                            ": '", Raw, "'"],
                           LineNo)
    end.

parse_regexp(Cmd, RegExp) when is_binary(RegExp) ->
    Cmd#cmd{arg = lux_utils:strip_trailing_whitespaces(RegExp)};
parse_regexp(Cmd, Value) when is_atom(Value) ->
    Cmd#cmd{arg = Value}.

parse_var(P, Cmd, Scope, String) ->
    Pred = fun(C) -> C =/= $= end,
    case lists:splitwith(Pred, String) of
        {Var, [$= | Val]} ->
            Cmd#cmd{type = variable, arg = {Scope, Var, Val}};
        _ ->
            LineNo = Cmd#cmd.lineno,
            parse_error(P,
                        ["Syntax error at line ", integer_to_list(LineNo),
                         ": illegal ", atom_to_list(Scope),
                         " variable "," '", String, "'"],
                        LineNo)
    end.

parse_meta(P, Bin, #cmd{lineno = LineNo} = Cmd, Lines, Tokens) ->
    [First | MultiLine] = re:split(Bin, <<"\n">>),
    ChoppedBin = lux_utils:strip_trailing_whitespaces(First),
    MetaSize = byte_size(ChoppedBin) - 1,
    case ChoppedBin of
        <<Meta:MetaSize/binary, "]">> ->
            {LineNo2, Token2, Lines2} =
                case parse_meta_token(P, Cmd, Meta, LineNo) of
                    #cmd{type = macro} = Macro ->
                        parse_macro(P, Macro, LineNo, MultiLine, Lines);
                    Token ->
                        {LineNo, Token, Lines}
                end,
            parse(P, Lines2, LineNo2+1, [Token2 | Tokens]);
        _ ->
            parse_error(P,
                        ["Syntax error at line ", integer_to_list(LineNo),
                         ": ']' is expected to be at end of line"],
                        LineNo)
    end.

parse_macro(P,
            #cmd{arg = {pre_macro, Name, ArgNames}} = Cmd,
            LineNo,
            MultiLine,
            Lines) ->
    case MultiLine of
        [] ->
            {ok, MP} = re:compile(<<"^[\s\t]*\\[endmacro\\]">>),
            Pred = fun(L) -> re:run(L, MP, [{capture, none}]) =:= nomatch end,
            {RawBody, After} = lists:splitwith(Pred, Lines),
            BodyLen = length(RawBody),
            case After of
                [] ->
                    parse_error(P,
                                ["Syntax error after line ",
                                 integer_to_list(LineNo),
                                 ": [endmacro] expected"],
                                LineNo);
                [_EndMacro | Lines2] ->
                    Body = lists:reverse(parse(P, RawBody, LineNo+1, [])),
                    LastLineNo = LineNo+BodyLen+1,
                    {LineNo+BodyLen+1,
                     Cmd#cmd{arg = {macro, Name, ArgNames, LineNo,
                                    LastLineNo, Body}},
                     Lines2}
            end;
        _ ->
            Body = lists:reverse(parse(P, MultiLine, LineNo+1, [])),
            LastLineNo = LineNo,
            {LineNo,
             Cmd#cmd{arg = {macro, Name, ArgNames, LineNo, LastLineNo, Body}},
             Lines}
    end.

parse_meta_token(P, Cmd, Meta, LineNo) ->
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
                    parse_error(P,
                                ["Illegal prefix of doc string" ,
                                 Text2,
                                 " on line ",
                                 integer_to_list(LineNo)],
                                LineNo)
            end;
        "cleanup" ++ Name ->
            Cmd#cmd{type = cleanup, arg = string:strip(Name)};
        "shell" ++ Name ->
            Cmd#cmd{type = shell, arg = string:strip(Name)};
        "endshell" ->
            Cmd2 = Cmd#cmd{type = expect, raw = <<".">>},
            parse_regexp(Cmd2, shell_exit);
        "config" ++ VarVal ->
            ConfigCmd = parse_var(P, Cmd, config, string:strip(VarVal)),
            {Scope, Var, Val} = ConfigCmd#cmd.arg,
            try
                MissingVar = keep, % BUGBUG: should be error
                Val2 = lux_utils:expand_vars(P#pstate.dict,
                                                  Val,
                                                  MissingVar),
                ConfigCmd#cmd{type = config, arg = {Scope, Var, Val2}}
            catch
                throw:{no_such_var, BadVar} ->
                    parse_error(P,
                                ["Variable $",
                                 BadVar,
                                 " is not set on line ",
                                 integer_to_list(LineNo)],
                                LineNo);
                error:Reason ->
                    erlang:error(Reason)
            end;
        "my" ++ VarVal ->
            parse_var(P, Cmd, my, string:strip(VarVal));
        "local" ++ VarVal ->
            parse_var(P, Cmd, local, string:strip(VarVal));
        "global" ++ VarVal ->
            parse_var(P, Cmd, global, string:strip(VarVal));
        "timeout" ++ Time ->
            Cmd#cmd{type = change_timeout, arg = string:strip(Time)};
        "sleep" ++ Time ->
            Cmd#cmd{type = sleep, arg = string:strip(Time)};
        "progress" ++ String ->
            Cmd#cmd{type = progress, arg = string:strip(String)};
        "include" ++ File ->
            InclFile = filename:absname(string:strip(File),
                                        filename:dirname(P#pstate.file)),
            {FirstLineNo, LastLineNo, InclCmds} =
                parse_file2(P#pstate{file=InclFile}),
            Cmd#cmd{type = include,
                    arg = {include, InclFile, FirstLineNo, LastLineNo, InclCmds}};
        "macro" ++ Head ->
            case string:tokens(string:strip(Head), " ") of
                [Name | ArgNames] ->
                    Cmd#cmd{type = macro, arg = {pre_macro, Name, ArgNames}};
                [] ->
                    parse_error(P,
                                ["Syntax error at line ",
                                 integer_to_list(LineNo),
                                 ": missing macro name"],
                                LineNo)
            end;
        "invoke" ++ Head ->
            case split_invoke_args(P, LineNo, Head, normal, [], []) of
                [Name | ArgVals] ->
                    Cmd#cmd{type = invoke, arg = {invoke, Name, ArgVals}};
                [] ->
                    parse_error(P,
                                ["Syntax error at line ",
                                 integer_to_list(LineNo),
                                 ": missing macro name"],
                                LineNo)
            end;
        Bad ->
            parse_error(P,
                        ["Syntax error at line ",
                         integer_to_list(LineNo),
                         ": Unknown meta command '",
                         Bad, "'"],
                        LineNo)
    end.

split_invoke_args(P, LineNo, [], quoted, Arg, _Args) ->
    parse_error(P,
                ["Syntax error at line ",
                 integer_to_list(LineNo),
                 ": Unterminated quote '",
                 Arg, "'"],
                LineNo);
split_invoke_args(_P, _LineNo, [], normal, [], Args) ->
    lists:reverse(Args);
split_invoke_args(_P, _LineNo, [], normal, Arg, Args) ->
    lists:reverse([lists:reverse(Arg) | Args]);
split_invoke_args(P, LineNo, [H | T], normal = Mode, Arg, Args) ->
    case H of
        $\" -> % quote begin
            split_invoke_args(P, LineNo, T, quoted, Arg, Args);
        $\  when Arg =:= [] -> % skip space between args
            split_invoke_args(P, LineNo, T, Mode, Arg, Args);
        $\  when Arg =/= [] -> % first space after arg
            Arg2 = lists:reverse(Arg),
            split_invoke_args(P, LineNo, T, Mode, [], [Arg2 | Args]);
        $\\ when hd(T) =:= $\\ ; hd(T) =:= $\" -> % escaped char
            split_invoke_args(P, LineNo, tl(T), Mode, [hd(T) | Arg], Args);
        Char ->
            split_invoke_args(P, LineNo, T, Mode, [Char | Arg], Args)
    end;
split_invoke_args(P, LineNo, [H | T], quoted = Mode, Arg, Args) ->
    case H of
        $\" -> % quote end
            Arg2 = lists:reverse(Arg),
            split_invoke_args(P, LineNo, T, normal, [], [Arg2 | Args]);
        $\\ when hd(T) =:= $\\; hd(T) =:= $\" ->  % escaped char
            split_invoke_args(P, LineNo, tl(T), Mode, [hd(T) | Arg], Args);
        Char ->
            split_invoke_args(P, LineNo, T, Mode, [Char | Arg], Args)
    end.

parse_multi(P, <<$":8/integer, $":8/integer, Rest/binary>>,
            #cmd{lineno = LineNo}, Lines, OrigLine, Tokens) ->
    PrefixLen = count_prefix_len(binary_to_list(OrigLine), 0),
    {RevBefore, After, RemPrefixLen} = scan_multi(Lines, PrefixLen, []),
    LastLineNo = LineNo + length(RevBefore) + 1,
    case After of
        [] ->
            parse_error(P,
                        ["Syntax error after line ",
                         integer_to_list(LineNo),
                         ": '\"\"\"' expected"],
                        LastLineNo);
        _ when RemPrefixLen =/= 0 ->
            parse_error(P,
                        ["Syntax error at line ", integer_to_list(LastLineNo),
                         ": multi line block must end in same column as"
                         " it started on line ", integer_to_list(LineNo)],
                        LastLineNo);
        [_EndOfMulti | Lines2] ->
            %% Join all lines with a newline as separator
            Multi =
                case RevBefore of
                    [Single] ->
                        Single;
                    [Last | Other] ->
                        Join = fun(F, L) ->
                                       <<F/binary, <<"\n">>/binary, L/binary>>
                               end,
                        lists:foldl(Join, Last, Other);
                    [] ->
                        <<"">>
                end,
            Rest2 = lux_utils:strip_leading_whitespaces(Rest),
            {Extra, ExtraLineNo} =
                case Rest2 of
                    <<>> ->
                        parse_error(P,
                                    ["Syntax error at line ",
                                     integer_to_list(LineNo),
                                     ": '\"\"\"' must be followed by a command"],
                                    LineNo);
                    <<"[", _/binary>> ->
                        {[<<Rest2/binary, "\n", Multi/binary>>], LineNo};
                    _ ->
                        {[<<Rest2/binary, Multi/binary>>], LastLineNo}
                end,
            Tokens2 = do_parse(P, Extra, ExtraLineNo, OrigLine, Tokens),
            parse(P, Lines2, LastLineNo+1, Tokens2)
    end;
parse_multi(P, _, #cmd{lineno = LineNo}, _Lines, _OrigLine, _Tokens) ->
    parse_error(P,
                ["Syntax error at line ", integer_to_list(LineNo),
                 ": '\"\"\"' command expected"],
                LineNo).

count_prefix_len([H | T], N) ->
    case H of
        $\  -> count_prefix_len(T, N + 1);
        $\t -> count_prefix_len(T, N + ?TAB_LEN);
        $"  -> N
    end.

scan_multi([Line | Lines] = All, PrefixLen, Acc) ->
    case scan_single(Line, PrefixLen) of
        {more, Line2} ->
            scan_multi(Lines, PrefixLen, [Line2 | Acc]);
        {nomore, RemPrefixLen} ->
            {Acc, All, RemPrefixLen}
    end;
scan_multi([], PrefixLen, Acc) ->
    {Acc, [], PrefixLen}.

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
                    Spaces = list_to_binary(lists:duplicate(abs(Left), $\ )),
                    {more, <<Spaces/binary, Line/binary>>};
                true ->
                    scan_single(Rest, Left)
            end;
        _ ->
            {more, Line}
    end.

parse_error(#pstate{file = File}, IoList, LineNo) ->
    throw({error, File, LineNo, list_to_binary(IoList)});
parse_error(#istate{file = File}, IoList, LineNo) ->
    throw({error, File, LineNo, list_to_binary(IoList)}).
