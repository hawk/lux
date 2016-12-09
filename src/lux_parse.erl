%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2016 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_parse).

-export([parse_file/4]).

-include("lux.hrl").

-record(pstate,
        {
          file       :: string(),
          orig_file  :: string(),
          pos_stack  :: [#cmd_pos{}],
          mode       :: run_mode(),
          skip_skip  :: boolean(),
          multi_vars :: [[string()]], % ["name=val"]
          warnings   :: [binary()]
        }).

-define(TAB_LEN, 8).
-define(FF(Format, Args), io_lib:format(Format, Args)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse

parse_file(RelFile, RunMode, SkipSkip, Opts) ->
    try
        File = lux_utils:normalize(RelFile),
        RevFile = lux_utils:filename_split(File),
        DefaultI = lux_case:default_istate(File),
        case lux_case:parse_iopts(DefaultI, Opts) of
            {ok, I} ->
                MultiVars =
                    [I#istate.global_vars,
                     I#istate.builtin_vars,
                     I#istate.system_vars],
                P = #pstate{file = File,
                            orig_file = File,
                            pos_stack = [],
                            mode = RunMode,
                            skip_skip = SkipSkip,
                            multi_vars = MultiVars,
                            warnings = []},
                test_user_config(P, I),
                {P2, _FirstLineNo, _LastLineNo, Cmds} = parse_file2(P),
                Config = lux_utils:foldl_cmds(fun extract_config/4,
                                              [], File, [], Cmds),
                garbage_collect(),
                case parse_config(I, lists:reverse(Config)) of
                    {ok, I2} ->
                        File2 = I2#istate.file,
                        UpdatedOpts = updated_opts(I2, I),
                        {ok, File2, Cmds, UpdatedOpts, P2#pstate.warnings};
                    {error, Reason} ->
                        {error,
                         [#cmd_pos{rev_file= RevFile, lineno = 0, type = main}],
                         Reason}
                end;
            {error, Reason} ->
                {error,
                 [#cmd_pos{rev_file = RevFile, lineno = 0, type = main}],
                 Reason}
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
                    lists:keyreplace(Name, 1, Acc, {Name, OldVals ++ [Val]})
            end;
        #cmd{} ->
            Acc
    end.

parse_config(I0, Config) ->
    Fun =
        fun({Name, Vals}, {{ok, I}, U}) ->
            case lux_case:config_type(Name) of
                {ok, Pos, Types = [{std_list, _}]} ->
                    lux_case:set_config_vals(Name, Vals, Types, Pos, I, U);
                {ok, Pos, Types = [{reset_list, _}]} ->
                    lux_case:set_config_vals(Name, Vals, Types, Pos, I, U);
                {ok, Pos, Types} ->
                    Val = lists:last(Vals),
                    lux_case:set_config_val(Name, Val, Types, Pos, I, U);
                {error, Reason} ->
                    {{error, Reason}, U}
            end;
           (_, {{error, _Reason}, _U} = Res) ->
                Res
        end,
    Res = lists:foldl(Fun, {{ok, I0}, []}, Config),
    element(1, Res).

updated_opts(I, DefaultI) ->
    Candidates = lux_case:user_config_types(),
    Filter = fun({Tag, Pos, Types}) ->
                     Old = element(Pos, DefaultI),
                     New = element(Pos, I),
                     case Old =/= New of
                         false ->
                             false;
                         true when Types =:= [{std_list, [string]}] ->
                             Diff = New -- Old,
                             {true, {Tag, Diff}};
                         true ->
                             {true, {Tag, New}}
                     end
             end,
    Args = lists:zf(Filter, Candidates),
    lux_suite:args_to_opts(Args, case_style, []).

parse_file2(P) ->
    case file_open(P) of
        {ok, Fd} ->
            FirstLineNo = 1,
            {P2, eof, RevCmds} = parse(P, Fd, FirstLineNo, []),
            Cmds = lists:reverse(RevCmds),
            %% io:format("Cmds: ~p\n", [Cmds]),
            case Cmds of
                [#cmd{lineno = LastLineNo} | _] -> ok;
                []                              -> LastLineNo = 1
            end,
            {P2, FirstLineNo, LastLineNo, Cmds};
        {error, FileReason} ->
            NewFd = eof,
            Reason = iolist_to_binary([lux_utils:drop_prefix(P#pstate.file),
                                       ": ", file:format_error(FileReason)]),
            parse_error(P, NewFd, 0, Reason)
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
do_file_open(File, RM) when RM =:= doc; RM =:= list; RM =:= list_dir ->
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
            NewLine = backslash_chop(Line),
            {line, NewLine, NewFd, 0};
        {line, Line, NewFd} ->
            TmpLine = backslash_chop(Line),
            NewLine = lux_utils:strip_leading_whitespaces(TmpLine),
            ComboLine = backslash_merge(Lines, NewLine),
            {line, ComboLine, NewFd, length(Lines)};
        eof when Lines =:= [] ->
            eof;
        eof ->
            ComboLine = backslash_merge(Lines, <<>>),
            {line, ComboLine, BackslashFd, length(Lines)}
    end.

backslash_takewhile(Fd) ->
    Pred = fun(Line) -> backslash_filter(Line) end,
    file_takewhile(Fd, Pred, []).

backslash_filter(Line) ->
    Sz = byte_size(Line) - 1,
    Sz2 = Sz - 1,
    case Line of
        <<_Chopped:Sz2/binary, "\\\\">> ->
            %% Found two trailing backslashes
            false;
        <<Chopped:Sz/binary, "\\">> ->
            %% Found trailing backslash
            {true, Chopped};
        _ ->
            false
    end.

backslash_chop(Line) ->
    Sz = byte_size(Line) - 1,
    case Line of
        <<Chopped:Sz/binary, "\\">> ->
            Chopped;
        _ ->
            Line
    end.

backslash_merge([First|Rest], Last) ->
    Lines = [lux_utils:strip_leading_whitespaces(Line) || Line <- Rest],
    iolist_to_binary([First,Lines,Last]).

parse(P, Fd, LineNo, Tokens) ->
    case file_next_wrapper(Fd) of
        {line, OrigLine, NewFd, Incr} ->
            Line = lux_utils:strip_leading_whitespaces(OrigLine),
            parse_cmd(P, NewFd, Line, LineNo, Incr, OrigLine, Tokens);
        eof = NewFd ->
            {P, NewFd, Tokens}
    end.

parse_cmd(P, Fd, <<>>, LineNo, Incr, OrigLine, Tokens) ->
    Token = #cmd{type = comment, lineno = LineNo, orig = OrigLine},
    parse(P, Fd, LineNo+Incr+1, [Token | Tokens]);
parse_cmd(P, Fd, Line, LineNo, Incr, OrigLine, Tokens) ->
    RunMode = P#pstate.mode,
    {Type, SubType, UnStripped} = parse_oper(P, Fd, LineNo, Line),
    Cmd = #cmd{lineno = LineNo,
               type = Type,
               arg = SubType,
               orig = OrigLine},
    if
        Type =:= meta ->
            parse_meta(P, Fd, UnStripped, Cmd, Tokens);
        Type =:= multi ->
            parse_multi(P, Fd, UnStripped, Cmd, Tokens);
        RunMode =:= validate; RunMode =:= execute ->
            Cmd2 = parse_single(Cmd, UnStripped),
            parse(P, Fd, LineNo+Incr+1, [Cmd2 | Tokens]);
        RunMode =:= list; RunMode =:= list_dir; RunMode =:= doc ->
            %% Skip command
            parse(P, Fd, LineNo+Incr+1, Tokens)
    end.

parse_oper(P, Fd, LineNo, OrigLine) ->
    case OrigLine of
        <<"!",      D/binary>> -> {send,              lf,        D};
        <<"~",      D/binary>> -> {send,              nolf,      D};
        <<"?++",    D/binary>> -> {expect_add_strict, regexp,    D};
        <<"?+",     D/binary>> -> {expect_add,        regexp,    D};
        <<"???",    D/binary>> -> {expect,            verbatim,  D};
        <<"??",     D/binary>> -> {expect,            template,  D};
        <<"?",      D/binary>> -> {expect,            regexp,    D};
        <<"-",      D/binary>> -> {fail,              regexp,    D};
        <<"+",      D/binary>> -> {success,           regexp,    D};
        <<"@",      D/binary>> -> {break,             regexp,    D};
        <<"[",      D/binary>> -> {meta,              undefined, D};
        <<"\"\"\"", D/binary>> -> {multi,             undefined, D};
        <<"#",      D/binary>> -> {comment,           undefined, D};
        _ ->
            parse_error(P, Fd, LineNo,
                        ["Syntax error at line ", ?i2l(LineNo),
                         ": '", OrigLine, "'"])
    end.

parse_single(#cmd{type = Type, arg = SubType} = Cmd, Data) ->
    case Type of
        send when SubType =:= lf   -> Cmd#cmd{arg = <<Data/binary, "\n">>};
        send when SubType =:= nolf -> Cmd#cmd{arg = Data};
        expect when Data =:= <<>>  -> parse_regexp(Cmd, SubType, reset, single);
        expect                     -> parse_regexp(Cmd, SubType, Data,  multi);
        expect_add                 -> parse_regexp(Cmd, SubType, Data,  multi);
        expect_add_strict          -> parse_regexp(Cmd, SubType, Data,  multi);
        fail when Data =:= <<>>    -> parse_regexp(Cmd, SubType, reset, single);
        fail                       -> parse_regexp(Cmd, SubType, Data,  single);
        success when Data =:= <<>> -> parse_regexp(Cmd, SubType, reset, single);
        success                    -> parse_regexp(Cmd, SubType, Data,  single);
        break when Data =:= <<>>   -> parse_regexp(Cmd, SubType, reset, single);
        break                      -> parse_regexp(Cmd, SubType, Data,  single);
%%      meta                       -> Cmd;
%%      multi                      -> Cmd;
        comment                    -> Cmd
    end.

%% Arg :: reset                               |
%%        {endshell, single,        regexp()} |
%%        {verbatim, regexp_oper(), regexp()} |
%%        {template, regexp_oper(), regexp()} |
%%        {regexp,   regexp_oper(), regexp()  |
%%        {mp,       regexp_oper(), regexp(), mp(), multi()} (compliled later)
%% regexp_oper() :: single | multi | expect_add | expect_add_strict
%% regexp()      :: binary()
%% multi()       :: [{Name::binary(), regexp(), AlternateCmd::#cmd{}}]

parse_regexp(#cmd{type = Type} = Cmd, RegExpType, RegExp, RegExpOper) ->
    if
        RegExp =:= reset, RegExpOper =:= single ->
            Cmd#cmd{arg = reset};
        Type =:= expect_add_strict; Type =:= expect_add ->
            Cmd#cmd{type = expect, arg = {RegExpType, Type, RegExp}};
        true ->
            Cmd#cmd{arg = {RegExpType, RegExpOper, RegExp}}
    end.

parse_var(P, Fd, Cmd, Scope, String) ->
    case lux_utils:split_var(String, []) of
        {Var, Val} ->
            {P, Cmd#cmd{type = variable, arg = {Scope, Var, Val}}};
        false ->
            LineNo = Cmd#cmd.lineno,
            parse_error(P, Fd, LineNo,
                        ["Syntax error at line ", ?i2l(LineNo),
                         ": illegal ", atom_to_list(Scope),
                         " variable "," '", String, "'"])
    end.

parse_meta(P, Fd, UnStripped, #cmd{lineno = LineNo} = Cmd, Tokens) ->
    Stripped = lux_utils:strip_trailing_whitespaces(UnStripped),
    P2 =
        if
            Stripped =/= UnStripped ->
                add_warning(P, Cmd, <<"Trailing whitespaces">>);
           true ->
                P
        end,
    MetaSize = byte_size(Stripped) - 1,
    case Stripped of
        <<Meta:MetaSize/binary, "]">> ->
            {P3, MetaCmd} = parse_meta_token(P2, Fd, Cmd, Meta, LineNo),
            {NewP, NewFd, NewLineNo, NewCmd} =
                case MetaCmd#cmd.type of
                    macro ->
                        parse_body(P3, Fd, MetaCmd, "endmacro", LineNo);
                    loop ->
                        parse_body(P3, Fd, MetaCmd, "endloop", LineNo);
                    _ ->
                        {P3, Fd, LineNo, MetaCmd}
                end,
            RunMode = NewP#pstate.mode,
            NewType = NewCmd#cmd.type,
            NewTokens =
                if
                    NewType =:= config ->
                        [NewCmd | Tokens];
                    NewType =:= include  ->
                        [NewCmd | Tokens];
                    NewType =:= doc,
                    RunMode =/= list,
                    RunMode =/= list_dir ->
                        [NewCmd | Tokens];
                    RunMode =:= list; RunMode =:= list_dir; RunMode =:= doc ->
                        %% Skip command
                        Tokens;
                    RunMode =:= validate; RunMode =:= execute ->
                        [NewCmd | Tokens]
                end,
            parse(NewP, NewFd, NewLineNo+1, NewTokens);
        _ ->
            parse_error(P2, Fd, LineNo,
                        ["Syntax error at line ", ?i2l(LineNo),
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
                         ?i2l(LineNo),
                         ": [" ++ EndKeyword ++ "] expected"]);
        {line, _EndMacro, NewFd, Incr}
          when RunMode =:= list; RunMode =:= list_dir; RunMode =:= doc ->
            %% Do not parse body
            BodyLen = length(BodyLines)+BodyIncr,
            LastLineNo = LineNo+Incr+BodyLen+1,
            {P, NewFd, LastLineNo, Cmd#cmd{arg = undefined}};
        {line, _EndMacro, NewFd, Incr}
          when RunMode =:= validate; RunMode =:= execute ->
            %% Parse body
            BodyLen = length(BodyLines)+BodyIncr,
            FdBody = file_open(BodyLines),
            {P2, eof, RevBodyCmds} = parse(P, FdBody, LineNo+Incr+1, []),
            BodyCmds = lists:reverse(RevBodyCmds),
            LastLineNo = LineNo+Incr+BodyLen+1,
            Arg = {Tag, Name, Items, LineNo, LastLineNo, BodyCmds},
            {P2, NewFd, LastLineNo, Cmd#cmd{arg = Arg}}
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
                        "1 " ++ Text
                end,
            Pred = fun(Char) -> Char =/= $\  end,
            {LevelStr, Text3} = lists:splitwith(Pred, Text2),
            try
                Level = list_to_integer(LevelStr),
                if Level > 0 -> ok end, % assert
                Doc = list_to_binary(string:strip(Text3)),
                {P, Cmd#cmd{type = doc, arg = {Level, Doc}}}
            catch
                error:_ ->
                    parse_error(P, Fd, LineNo,
                                ["Illegal prefix of doc string" ,
                                 Text2, " on line ",
                                 ?i2l(LineNo)])
            end;
        "cleanup" ++ Name ->
            {P, Cmd#cmd{type = cleanup, arg = string:strip(Name)}};
        "shell" ++ Name ->
            Name2 = string:strip(Name),
            Match = re:run(Name2, "\\$\\$", [{capture,none}]),
            case {Name2, Match} of
                %%                 {"", _} ->
                %%                     parse_error(P, Fd, LineNo,
                %%                                 ?FF("Syntax error at line ~p"
                %%                                     ": missing shell name",
                %%                                     [LineNo]));
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
                    {P, Cmd#cmd{type = shell, arg = Name2}}
            end;
        "endshell" ++ Data ->
            case list_to_binary(string:strip(Data)) of
                %% <<>>   -> RegExp = <<"0">>;
                <<>>   -> RegExp = <<".*">>;
                RegExp -> ok
            end,
            {P, Cmd#cmd{type = expect, arg = {endshell, single, RegExp}}};
        "config" ++ VarVal ->
            {P2, ConfigCmd} =
                parse_var(P, Fd, Cmd, config, string:strip(VarVal)),
            {Scope, Var, Val} = ConfigCmd#cmd.arg,
            Val2 = expand_vars(P2, Fd, Val, LineNo),
            ConfigCmd2 = ConfigCmd#cmd{type = config, arg = {Scope, Var, Val2}},
            test_skip(P2, Fd, ConfigCmd2);
        "my" ++ VarVal ->
            parse_var(P, Fd, Cmd, my, string:strip(VarVal));
        "local" ++ VarVal ->
            parse_var(P, Fd, Cmd, local, string:strip(VarVal));
        "global" ++ VarVal ->
            parse_var(P, Fd, Cmd, global, string:strip(VarVal));
        "timeout" ++ Time ->
            {P, Cmd#cmd{type = change_timeout, arg = string:strip(Time)}};
        "sleep" ++ Time ->
            {P, Cmd#cmd{type = sleep, arg = string:strip(Time)}};
        "progress" ++ String ->
            {P, Cmd#cmd{type = progress, arg = string:strip(String)}};
        "include" ++ RelFile ->
            CurrFile = P#pstate.file,
            CurrPosStack = P#pstate.pos_stack,
            Dir = filename:dirname(CurrFile),
            RelFile2 = string:strip(expand_vars(P, Fd, RelFile, LineNo)),
            AbsFile = filename:absname(RelFile2, Dir),
            AbsFile2 = lux_utils:normalize(AbsFile),
            try
                NewPosStack = cmd_pos_stack(P, LineNo),
                {P2, FirstLineNo, LastLineNo, InclCmds} =
                    parse_file2(P#pstate{file = AbsFile2,
                                         pos_stack = NewPosStack}),
                Cmd2 = Cmd#cmd{type = include,
                               arg = {include,AbsFile2,FirstLineNo,
                                      LastLineNo,InclCmds}},
                {P2#pstate{file = CurrFile, pos_stack = CurrPosStack}, Cmd2}
            catch
                throw:{skip, ErrorStack, Reason} ->
                    %% re-throw
                    reparse_error(Fd, skip, ErrorStack, Reason);
                throw:{error, ErrorStack, Reason} ->
                    %% re-throw
                    reparse_error(Fd, error, ErrorStack, Reason)
            end;
        "macro" ++ Head ->
            case string:tokens(string:strip(Head), " ") of
                [Name | ArgNames] ->
                    {P, Cmd#cmd{type = macro,
                                arg  = {body, macro, Name, ArgNames}}};
                [] ->
                    parse_error(P, Fd, LineNo,
                                ["Syntax error at line ",
                                 ?i2l(LineNo),
                                 ": missing macro name"])
            end;
        "invoke" ++ Head ->
            case split_invoke_args(P, Fd, LineNo, Head, normal, [], []) of
                [Name | ArgVals] ->
                    {P, Cmd#cmd{type = invoke, arg = {invoke, Name, ArgVals}}};
                [] ->
                    parse_error(P, Fd, LineNo,
                                ["Syntax error at line ",
                                 ?i2l(LineNo),
                                 ": missing macro name"])
            end;
        "loop" ->
            %% Indefinite loop
            {P, Cmd#cmd{type = loop, arg = {body, loop, forever, undefined}}};
        "loop" ++ Head ->
            Pred = fun(Char) -> Char =/= $\ end,
            case lists:splitwith(Pred, string:strip(Head)) of
                {Var, Items0} when Var =/= "" ->
                    Items = string:strip(Items0, left),
                    {P, Cmd#cmd{type = loop, arg = {body, loop, Var, Items}}};
                _ ->
                    parse_error(P, Fd, LineNo,
                                ["Syntax error at line ",
                                 ?i2l(LineNo),
                                 ": missing loop variable"])
            end;
        Bad ->
            parse_error(P, Fd, LineNo,
                        ["Syntax error at line ",
                         ?i2l(LineNo),
                         ": Unknown meta command '",
                         Bad, "'"])
    end.

test_user_config(P, I) ->
    T =
        fun(Var, NameVal) ->
                Cmd = #cmd{type = config,
                           arg = {config, Var, NameVal},
                           lineno = 0,
                           orig = <<>>},
                test_skip(P, eof, Cmd)
        end,
    lists:foreach(fun(Val) -> T("skip", Val) end, I#istate.skip),
    lists:foreach(fun(Val) -> T("skip_unless", Val) end, I#istate.skip_unless),
    lists:foreach(fun(Val) -> T("require", Val) end, I#istate.require).

test_skip(#pstate{mode = RunMode, skip_skip = SkipSkip} = P, Fd,
          #cmd{lineno = LineNo, arg = {config, Var, NameVal}} = Cmd) ->
    case Var of
        "skip" when not SkipSkip ->
            {IsSet, Name} = test_var(P, NameVal),
            case IsSet of
                false ->
                    {P, Cmd};
                true ->
                    Reason = "SKIP as variable ~s is set",
                    parse_skip(P, Fd, LineNo, ?FF(Reason, [Name]))
            end;
        "skip_unless" when not SkipSkip ->
            {IsSet, Name} = test_var(P, NameVal),
            case IsSet of
                true ->
                    {P, Cmd};
                false ->
                    Reason = "SKIP as variable ~s is not set",
                    parse_skip(P, Fd, LineNo, ?FF(Reason, [Name]))
            end;
        "require" when RunMode =:= execute ->
            {IsSet, Name} = test_var(P, NameVal),
            case IsSet of
                true ->
                    {P, Cmd};
                false ->
                    Reason = "FAIL as required variable ~s is not set",
                    parse_skip(P, Fd, LineNo, ?FF(Reason, [Name]))
            end;
        _ ->
            {P, Cmd}
    end.

test_var(P, VarVal) ->
    lux_utils:test_var(P#pstate.multi_vars, VarVal).

expand_vars(P, Fd, Val, LineNo) ->
    try
        lux_utils:expand_vars(P#pstate.multi_vars, Val, error)
    catch
        throw:{no_such_var, BadVar} ->
            Reason = ["Variable $", BadVar,
                      " is not set on line ",
                      ?i2l(LineNo)],
            parse_error(P, Fd, LineNo, Reason);
        error:Reason ->
            erlang:error(Reason)
    end.

split_invoke_args(P, Fd, LineNo, [], quoted, Arg, _Args) ->
    parse_error(P, Fd, LineNo,
                ["Syntax error at line ",
                 ?i2l(LineNo),
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

parse_multi(P, Fd, <<>>,
            #cmd{lineno = LineNo}, _Tokens) ->
    parse_error(P, Fd, LineNo,
                ["Syntax error at line ", ?i2l(LineNo),
                 ": '\"\"\"' command expected"]);
parse_multi(#pstate{mode = RunMode} = P, Fd, Chars,
            #cmd{lineno = LineNo, orig = OrigLine} = Cmd, Tokens) ->
    PrefixLen = count_prefix_len(binary_to_list(OrigLine), 0),
    {P2, RevBefore, FdAfter, RemPrefixLen, MultiIncr} =
        scan_multi(P, Fd, Cmd, PrefixLen, [], 0),
    LastLineNo0 = LineNo+MultiIncr+length(RevBefore)+1,
    case file_next_wrapper(FdAfter) of
        eof = NewFd ->
            parse_error(P2, NewFd, LastLineNo0,
                        ["Syntax error after line ",
                         ?i2l(LineNo),
                         ": '\"\"\"' expected"]);
        {line, _, NewFd, Incr} when RemPrefixLen =/= 0 ->
            LastLineNo = LastLineNo0+Incr,
            parse_error(P2, NewFd, LastLineNo,
                        ["Syntax error at line ", ?i2l(LastLineNo),
                         ": multi line block must end in same column as"
                         " it started on line ", ?i2l(LineNo)]);
        {line, _EndOfMulti, NewFd, Incr}
          when RunMode =:= list; RunMode =:= list_dir; RunMode =:= doc ->
            %% Skip command
            LastLineNo = LastLineNo0+Incr,
            parse(P2, NewFd, LastLineNo+1, Tokens);
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
            {P3, NewFd, Tokens2} =
                parse_cmd(P2, Fd2, MultiLine, LastLineNo, 0, OrigLine, Tokens),
            parse(P3, NewFd, LastLineNo+1, Tokens2)
    end.

count_prefix_len([H | T], N) ->
    case H of
        $\  -> count_prefix_len(T, N+1);
        $\t -> count_prefix_len(T, N+?TAB_LEN);
        $"  -> N
    end.

scan_multi(P, Fd, Cmd, PrefixLen, Acc, Incr0) ->
    case file_next_wrapper(Fd) of
        {line, Line, NewFd, Incr} ->
            NextIncr = Incr0+Incr,
            case scan_single(P, Fd, Cmd, Line, PrefixLen, Incr0) of
                {more, P2, Line2} ->
                    scan_multi(P2, NewFd, Cmd, PrefixLen,
                               [Line2 | Acc], NextIncr);
                {nomore, P2, RemPrefixLen} ->
                    {P2, Acc, file_push(NewFd,[Line]), RemPrefixLen, NextIncr}
            end;
        eof = NewFd->
            {P, Acc, NewFd, PrefixLen, Incr0}
    end.

scan_single(P, Fd, Cmd, Line, PrefixLen, Incr) ->
    case Line of
        <<"\"\"\"", UnStripped/binary>> ->
            Stripped = lux_utils:strip_trailing_whitespaces(UnStripped),
            ThisLineNo = Cmd#cmd.lineno-Incr+1,
            if
                UnStripped =:= <<>> ->
                    {nomore, P, PrefixLen};
                Stripped =:= <<>> ->
                    Reason = <<"Trailing whitespaces">>,
                    Cmd2 = Cmd#cmd{lineno = ThisLineNo},
                    P2 = add_warning(P, Cmd2, Reason),
                    {nomore, P2, PrefixLen};
                true ->
                    Reason = <<"Trailing garbage">>,
                    parse_error(P, Fd, ThisLineNo, Reason)
            end;
        _ when PrefixLen =:= 0 ->
            {more, P, Line};
        <<" ", Rest/binary>> ->
            scan_single(P, Fd, Cmd, Rest, PrefixLen - 1, Incr);
        <<"\t", Rest/binary>> ->
            Left = PrefixLen - ?TAB_LEN,
            if
                Left < 0 -> % Too much leading whitespace
                    Spaces = lists:duplicate(abs(Left), $\ ),
                    {more, P, iolist_to_binary([Spaces, Line])};
                true ->
                    scan_single(P, Fd, Cmd, Rest, Left, Incr)
            end;
        _ ->
            {more, P, Line}
    end.

parse_skip(P, Fd, LineNo, IoList) ->
    parse_error(P, Fd, skip, LineNo, IoList).

parse_error(P, Fd, LineNo, IoList) ->
    parse_error(P, Fd, error, LineNo, IoList).

parse_error(P, Fd, Tag, LineNo, IoList) ->
    NewPosStack = cmd_pos_stack(P, LineNo),
    NewIoList = iolist_to_binary(IoList),
    reparse_error(Fd, Tag, NewPosStack, NewIoList).

reparse_error(Fd, Tag, PosStack, IoList) ->
    file_close(Fd),
    throw({Tag, PosStack, IoList}).

make_warning(P, Cmd, IoList) ->
    File = P#pstate.orig_file,
    FullLineNo = full_lineno(P, Cmd),
    {warning, File, FullLineNo, iolist_to_binary(IoList)}.

add_warning(P, Cmd, IoList) ->
    Warning = make_warning(P, Cmd, IoList),
    P#pstate{warnings = P#pstate.warnings ++ [Warning]}.

full_lineno(P, #cmd{lineno = LineNo}) ->
    FullStack = cmd_pos_stack(P, LineNo),
    lux_utils:pretty_full_lineno(FullStack).

cmd_pos_stack(P, LineNo) ->
    RevFile = lux_utils:filename_split(P#pstate.file),
    OldPosStack = P#pstate.pos_stack,
    Context =
        case OldPosStack of
            [] -> main;
            _  -> include
        end,
    CmdPos = #cmd_pos{rev_file = RevFile,
                      lineno = LineNo,
                      type = Context},
    [CmdPos | OldPosStack].
