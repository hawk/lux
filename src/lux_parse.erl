%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2020 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_parse).

-export([parse_file/6]).

-include("lux.hrl").

-define(TAB_LEN, 8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse a script file

-spec(parse_file(filename(), run_mode(),
                 boolean(), boolean(), boolean(),
                 opts()) ->
             {ok, filename(), cmds(), opts()} | skip() | error()).

parse_file(RelFile, RunMode, SkipUnstable, SkipSkip, CheckDoc, Opts) ->
    try
        File = lux_utils:normalize_filename(RelFile),
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
                            body_level = 1,
                            mode = RunMode,
                            skip_unstable = SkipUnstable,
                            skip_skip = SkipSkip,
                            multi_vars = MultiVars,
                            warnings = [],
                            top_doc = undefined,
                            newshell = I#istate.newshell},
                test_user_config(P, I),
                {P2, _FirstLineNo, _LastLineNo, RevCmds} = parse_file2(P),
                RevCmds2 = ensure_cleanup(P2, RevCmds),
                Cmds = lists:reverse(RevCmds2),
                Config = lux_utils:foldl_cmds(fun extract_config/4,
                                              [], File, [], Cmds),
                I2 = I#istate{newshell = P2#pstate.newshell},
                case parse_config(I2, lists:reverse(Config)) of
                    {ok, I3} ->
                        garbage_collect(),
                        File2 = I3#istate.file,
                        UpdatedOpts = updated_opts(I3, I),
                        P3 =
                            if
                                CheckDoc, P2#pstate.top_doc =/= 1 ->
                                    add_warning(P2, undefined,
                                                <<"Missing summary doc">>);
                                true ->
                                    P2
                            end,
                        {ok, File2, Cmds, UpdatedOpts, P3#pstate.warnings};
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

ensure_cleanup(P, RevCmds) when P#pstate.has_cleanup ->
    RevCmds;
ensure_cleanup(_P, [] = RevCmds) ->
    RevCmds;
ensure_cleanup(_P, [LastCmd | _] = RevCmds) ->
    NoCleanup = LastCmd#cmd{type = no_cleanup},
    [NoCleanup | RevCmds].

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
            LastLineNo =
                case RevCmds of
                    []            -> FirstLineNo;
                    [LastCmd | _] -> LastCmd#cmd.lineno
                end,
            {P2, FirstLineNo, LastLineNo, RevCmds};
        {error, FileReason} ->
            NewFd = eof,
            Reason = ?l2b([lux_utils:drop_prefix(P#pstate.file),
                           ": ", file:format_error(FileReason)]),
            parse_error(P, NewFd, 0, Reason)
    end.

file_open(Lines) when is_list(Lines) ->
    [{read_ahead, Lines}];
file_open(#pstate{file = File, mode = RunMode}) ->
    do_file_open(File, RunMode).

do_file_open( File, RunMode) when RunMode =:= validate;
                                  RunMode =:= execute ->
    %% Bulk read file
    case file:read_file(File) of
        {ok, Bin} ->
            Bins = re:split(Bin, <<"\n">>),
            {ok, [{read_ahead, Bins}]};
        {error, FileReason} ->
            {error, FileReason}
    end;
do_file_open(File, RunMode) when RunMode =:= list;
                                 RunMode =:= list_dir;
                                 RunMode =:= doc ->
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
    ?l2b([First,Lines,Last]).

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
    NextLineNo = LineNo+Incr+1,
    parse(P, Fd, NextLineNo, [Token | Tokens]);
parse_cmd(P, Fd, Line, LineNo, Incr, OrigLine, Tokens) ->
    RunMode = P#pstate.mode,
    {Type, SubType, UnStripped} = parse_oper(P, Fd, LineNo, Line),
    Cmd = #cmd{lineno = LineNo,
               type = Type,
               arg = SubType,
               orig = OrigLine},
    NextLineNo = LineNo+Incr+1,
    if
        Type =:= meta ->
            parse_meta(P, Fd, Incr, UnStripped, Cmd, Tokens);
        Type =:= multi ->
            parse_multi(P, Fd, Incr, UnStripped, Cmd, Tokens, no_meta);
        RunMode =:= validate;
        RunMode =:= execute ->
            Cmd2 = parse_single(Cmd, UnStripped),
            parse(P, Fd, NextLineNo, [Cmd2 | Tokens]);
        RunMode =:= list;
        RunMode =:= list_dir;
        RunMode =:= doc ->
            %% Skip command
            parse(P, Fd, NextLineNo, Tokens)
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
        send when SubType =:= lf         -> Cmd#cmd{arg = <<Data/binary, "\n">>};
        send when SubType =:= nolf       -> Cmd#cmd{arg = Data};
        expect when Data =:= <<>>        -> regexp(Cmd, SubType, reset, single);
        expect when SubType =:= verbatim -> regexp(Cmd, SubType, Data,  single);
        expect when SubType =:= template -> regexp(Cmd, SubType, Data,  single);
        expect when SubType =:= regexp   -> regexp(Cmd, SubType, Data,  multi);
        expect_add                       -> regexp(Cmd, SubType, Data,  multi);
        expect_add_strict                -> regexp(Cmd, SubType, Data,  multi);
        fail when Data =:= <<>>          -> regexp(Cmd, SubType, reset, single);
        fail                             -> regexp(Cmd, SubType, Data,  single);
        success when Data =:= <<>>       -> regexp(Cmd, SubType, reset, single);
        success                          -> regexp(Cmd, SubType, Data,  single);
        break when Data =:= <<>>         -> regexp(Cmd, SubType, reset, single);
        break                            -> regexp(Cmd, SubType, Data,  single);
%%      meta                             -> Cmd;
%%      multi                            -> Cmd;
        comment                          -> Cmd
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

regexp(#cmd{type = Type} = Cmd, RegExpType, RegExp, RegExpOper) ->
    if
        RegExp =:= reset ->
            RegExpOper = single, % Assert
            Cmd#cmd{arg = reset};
        Type =:= expect_add_strict orelse Type =:= expect_add ->
            RegExpOper = multi, % Assert
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
                         ": Illegal ", ?a2l(Scope),
                         " variable '", String, "'"])
    end.

parse_meta(P, Fd, NextIncr, UnStripped, Cmd, Tokens) ->
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
            parse_single_meta(P2, Fd, NextIncr, Meta, Cmd, Tokens);
        _ ->
            %% BUGBUG: Pre-validate syntax of what we got so far
            parse_multi_meta(P2, Fd, NextIncr, Cmd, Tokens)
    end.

parse_single_meta(P, Fd, NextIncr, Meta, #cmd{lineno = LineNo} = Cmd, Tokens) ->
    {P2, MetaCmd} = parse_meta_token(P, Fd, Cmd, Meta, LineNo),
    {NewP, NewFd, NewLineNo, NewCmd} =
        case MetaCmd#cmd.type of
            macro ->
                parse_body(P2, Fd, NextIncr, MetaCmd, "endmacro");
            loop ->
                parse_body(P2, Fd, NextIncr, MetaCmd, "endloop");
            doc ->
                parse_doc(P2, Fd, NextIncr, MetaCmd);
            _ ->
                {P2, Fd, LineNo+NextIncr, MetaCmd}
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
            RunMode =:= list;
            RunMode =:= list_dir;
            RunMode =:= doc ->
                %% Skip command
                Tokens;
            RunMode =:= validate;
            RunMode =:= execute ->
                [NewCmd | Tokens]
        end,
    parse(NewP, NewFd, NewLineNo+1, NewTokens).

%% [global var=prefix-
%%     """
%%     multi
%%     line
%%     value
%%     """]
%%
%% Interpreted as
%%
%% [global var=prefix-multi\nline\nvalue]
parse_multi_meta(P, Fd, Incr, #cmd{lineno = LineNo} = Cmd, Tokens) ->
    case file_next_wrapper(Fd) of
        {line, OrigLine, NewFd, MultiIncr} ->
            NewIncr = 0,
            NewLineNo = LineNo + Incr + MultiIncr + 1,
            UnStripped = lux_utils:strip_leading_whitespaces(OrigLine),
            Stripped = lux_utils:strip_trailing_whitespaces(UnStripped),
            case Stripped of
                <<"\"\"\"">> ->
                    NewP =
                        if
                            Stripped =/= UnStripped ->
                                add_warning(P, Cmd, <<"Trailing whitespaces">>);
                            true ->
                                P
                        end,
                    MultiCmd = Cmd#cmd{lineno = NewLineNo, orig = OrigLine},
                    parse_multi(NewP, NewFd, NewIncr, UnStripped, MultiCmd,
                                Tokens, Cmd);
                _ ->
                    parse_error(P, NewFd, LineNo,
                                ["Syntax error at line ", ?i2l(LineNo),
                                 ": ']' is expected to be at end of line"])
            end;
        eof = NewFd ->
            {P, NewFd, Tokens}
    end.

parse_doc(P, Fd, NextIncr, #cmd{arg = {_Level, Suffix, <<>>}} = Cmd) ->
    %% Multi line
    EndKeyword = "enddoc" ++ Suffix,
    parse_body(P, Fd, NextIncr, Cmd, EndKeyword);
parse_doc(P, Fd, NextIncr, #cmd{arg = {Level, _Suffix, Doc}} = Cmd) ->
    %% Single line
    {P, Fd, Cmd#cmd.lineno+NextIncr, Cmd#cmd{arg = [{Level, Doc}]}}.

parse_multi_doc(Level, UnStripped) ->
    Stripped = [?l2b(string:strip(?b2l(Line))) || Line <- UnStripped],
    Pred = fun(Line) -> Line =:= <<>>  end,
    case lists:dropwhile(Pred, Stripped) of
        [] ->
            {warning, "Missing summary line", []};
        [Oneliner] ->
            {ok, [{Level, Oneliner}]};
        [Oneliner, <<>>] ->
            {warning, "More documentation lines expected after empty line",
             [{Level, Oneliner}]};
        [Oneliner, <<>> | Rest] ->
            Details = [{Level+1, Line} || Line <- Rest],
            {ok, [{Level, Oneliner} | Details]};
        [Oneliner | _Rest] ->
            {warning, "Empty line expected after summary line",
             [{Level, Oneliner}]}
    end.

parse_body(#pstate{body_level = Level} = P, Fd, NextIncr, Cmd, EndKeyword) ->
    TmpP = P#pstate{body_level = Level+1},
    {NewP, NewFd, LastLineNo, NewCmd} =
        do_parse_body(TmpP, Fd, NextIncr, Cmd, EndKeyword),
    {NewP#pstate{body_level = Level}, NewFd, LastLineNo, NewCmd}.

do_parse_body(#pstate{mode = RunMode} = P,
           Fd,
           NextIncr,
           #cmd{type = Type, arg = Arg, lineno = LineNo} = Cmd,
           EndKeyword) ->
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
          when Type =:= doc ->
            %% Do not parse body
            BodyLen = length(BodyLines)+BodyIncr,
            LastLineNo = LineNo+NextIncr+Incr+BodyLen+1,
            {Level, _Suffix, _EmptyDoc} = Arg,
            case parse_multi_doc(Level, BodyLines) of
                {ok, MultiDoc} ->
                    {P, NewFd, LastLineNo, Cmd#cmd{arg = MultiDoc}};
                {warning, Reason, MultiDoc} ->
                    {add_warning(P, Cmd, Reason),
                     NewFd, LastLineNo, Cmd#cmd{arg = MultiDoc}};
                {error, Reason} ->
                    parse_error(P, NewFd, LineNo,
                                ["Syntax error after line ",
                                 ?i2l(LineNo),
                                 ": ",
                                 Reason])
            end;
        {line, _EndMacro, NewFd, Incr}
          when RunMode =:= list;
               RunMode =:= list_dir;
               RunMode =:= doc ->
            %% Do not parse body
            BodyLen = length(BodyLines)+BodyIncr,
            LastLineNo = LineNo+NextIncr+Incr+BodyLen+1,
            {P, NewFd, LastLineNo, Cmd#cmd{arg = undefined}};
        {line, _EndMacro, NewFd, Incr}
          when RunMode =:= validate;
               RunMode =:= execute ->
            %% Parse body
            BodyLen = length(BodyLines)+BodyIncr,
            FdBody = file_open(BodyLines),
            {P2, eof, RevBodyCmds} = parse(P, FdBody, LineNo+Incr+1, []),
            LastLineNo = LineNo+NextIncr+Incr+BodyLen+1,
            BodyCmds = lists:reverse(RevBodyCmds),
            {body, Tag, Name, Items} = Arg,
            Arg2 = {Tag, Name, Items, LineNo, LastLineNo, BodyCmds},
            {P2, NewFd, LastLineNo, Cmd#cmd{arg = Arg2}}
    end.

merge_body([Line | Lines], Acc, Pending, Decr) ->
    case backslash_filter(Line) of
        {true, Chopped} ->
            merge_body(Lines, Acc, [Chopped | Pending], Decr+1);
        false ->
            NewLine = ?l2b([lists:reverse(Pending), Line]),
            merge_body(Lines, [NewLine | Acc], [], Decr)
    end;
merge_body([], Acc, [], Decr) ->
    {lists:reverse(Acc), Decr};
merge_body([], Acc, Pending, Decr) ->
    NewLine = ?l2b(lists:reverse(Pending)),
    {lists:reverse([NewLine | Acc]), Decr}.

parse_meta_token(P, Fd, Cmd, Meta, LineNo) ->
    case ?b2l(Meta) of
        "doc" ++ Text ->
            parse_meta_doc(P, Fd, Cmd, LineNo, Text);
        "cleanup" ++ _Name when P#pstate.has_cleanup ->
            parse_error(P, Fd, LineNo,
                        ["Syntax error at line ",
                         ?i2l(LineNo),
                         ": only one cleanup allowed"]);
        "cleanup" ++ _Name when P#pstate.body_level > 1 ->
            parse_error(P, Fd, LineNo,
                        ["Syntax error at line ",
                         ?i2l(LineNo),
                         ": cleanup only allowed at top level in main script"]);
        "cleanup" ->
            Name = "",
            {P#pstate{has_cleanup = true},
             Cmd#cmd{type = cleanup, arg = Name}};
        "cleanup " ++ Name ->
            {P#pstate{has_cleanup = true},
             Cmd#cmd{type = cleanup, arg = Name}};
        "shell" ->
            Name = "",
            parse_shell(P, Fd, Cmd, LineNo, Name, shell);
        "shell " ++ Name ->
            parse_shell(P, Fd, Cmd, LineNo, Name, shell);
        "newshell " ++ Name when Name =/= "" ->
            P2 = P#pstate{newshell = true},
            parse_shell(P2, Fd, Cmd, LineNo, Name, newshell);
        "endshell" ->
            RegExp = <<".*">>,
            {P, Cmd#cmd{type = expect, arg = {endshell, single, RegExp}}};
        "endshell " ++ Data ->
            RegExp = ?l2b(Data),
            {P, Cmd#cmd{type = expect, arg = {endshell, single, RegExp}}};
        "config " ++ VarVal ->
            {P2, ConfigCmd} =
                parse_var(P, Fd, Cmd, config, VarVal),
            {Scope, Var, Val} = ConfigCmd#cmd.arg,
            Val2 = expand_vars(P2, Fd, Val, LineNo),
            ConfigCmd2 = ConfigCmd#cmd{type = config, arg = {Scope, Var, Val2}},
            test_skip(P2, Fd, ConfigCmd2);
        "my " ++ VarVal ->
            parse_var(P, Fd, Cmd, my, VarVal);
        "local " ++ VarVal ->
            parse_var(P, Fd, Cmd, local, VarVal);
        "global " ++ VarVal ->
            parse_var(P, Fd, Cmd, global, VarVal);
        "timeout" ->
            Time = "",
            {P, Cmd#cmd{type = change_timeout, arg = Time}};
        "timeout " ++ Time ->
            {P, Cmd#cmd{type = change_timeout, arg = Time}};
        "sleep " ++ Time ->
            {P, Cmd#cmd{type = sleep, arg = Time}};
        "progress " ++ String ->
            {P, Cmd#cmd{type = progress, arg = String}};
        "debug " ++ DbgCmd ->
            {P, Cmd#cmd{type = debug, arg = DbgCmd}};
        "include " ++ RelFile ->
            CurrFile = P#pstate.file,
            CurrPosStack = P#pstate.pos_stack,
            Dir = filename:dirname(CurrFile),
            RelFile2 = expand_vars(P, Fd, RelFile, LineNo),
            AbsFile = filename:absname(RelFile2, Dir),
            AbsFile2 = lux_utils:normalize_filename(AbsFile),
            try
                NewPosStack = cmd_pos_stack(P, LineNo),
                {P2, FirstLineNo, LastLineNo, RevInclCmds} =
                    parse_file2(P#pstate{file = AbsFile2,
                                         pos_stack = NewPosStack}),
                InclCmds = lists:reverse(RevInclCmds),
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
        "macro " ++ Head ->
            case string:tokens(Head, " ") of
                [Name | ArgNames] ->
                    P2 =
                        case lists:member(?SPACE , Name) of
                            true ->
                                add_warning(P, Cmd, ["Macro name \"", Name,
                                                     "\" contains whitespace"]);
                            false ->
                                P
                        end,
                    {P2, Cmd#cmd{type = macro,
                                 arg  = {body, macro, Name, ArgNames}}};
                [] ->
                    parse_error(P, Fd, LineNo,
                                ["Syntax error at line ",
                                 ?i2l(LineNo),
                                 ": missing macro name"])
            end;
        "invoke " ++ Head ->
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
        "loop " ++ Head ->
            Pred = fun(Char) -> Char =/= ?SPACE end,
            case lists:splitwith(Pred, Head) of
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

parse_meta_doc(P, Fd, Cmd, LineNo, Text) ->
    {Suffix, Unstripped} =
        case Text of
            "" ->
                {"", Text};
            " " ++  _ ->
                {"", Text};
            _ ->
                Pred = fun(Char) -> Char =/= ?SPACE  end,
                lists:splitwith(Pred, Text)
        end,
    {P2, Doc} =
        case ?l2b(string:strip(Unstripped)) of
            <<>> when Unstripped =/= ""  ->
                Reason = <<"Missing doc text">>,
                {add_warning(P, Cmd, Reason), ?l2b(Unstripped)};
            Stripped ->
                {P, Stripped}
        end,
    Level =
        try
            if
                Suffix =:= "" ->
                    1;
                true ->
                    Level0 = list_to_integer(Suffix),
                    if Level0 > 0 -> Level0 end % assert
            end
        catch
            error:_ ->
                parse_error(P2, Fd, LineNo,
                            ["Syntax error at line ", ?i2l(LineNo),
                             ": Illegal doc level \"", Suffix, "\""])
        end,
    case P2#pstate.top_doc of
        undefined -> TopDoc = Level;
        TopDoc    -> ok
    end,
    {P2#pstate{top_doc = TopDoc},
     Cmd#cmd{type = doc, arg = {Level, Suffix, Doc}}}.

parse_shell(P, Fd, Cmd, LineNo, Name, Type) ->
    Match = re:run(Name, "\\$\\$", [{capture,none}]),
    case {Name, Match} of
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
                            [LineNo, Name]));
        {"cleanup"++_, _} ->
            parse_error(P, Fd, LineNo,
                        ?FF("Syntax error at line ~p"
                            ": ~s is a reserved"
                            " shell name",
                            [LineNo, Name]));
        {"post_cleanup"++_, _} ->
            parse_error(P, Fd, LineNo,
                        ?FF("Syntax error at line ~p"
                            ": ~s is a reserved"
                            " shell name",
                            [LineNo, Name]));
        {_, match} ->
            parse_error(P, Fd, LineNo,
                        ?FF("Syntax error at line ~p"
                            ": $$ in shell name",
                            [LineNo]));
        {_, nomatch} ->
            {P, Cmd#cmd{type = Type, arg = Name}}
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
    lists:foreach(fun(Val) -> T("unstable", Val) end, I#istate.unstable),
    lists:foreach(fun(Val) -> T("unstable_unless", Val) end, I#istate.unstable_unless),
    lists:foreach(fun(Val) -> T("require", Val) end, I#istate.require).

test_skip(#pstate{mode = RunMode,
                  skip_unstable = SkipUnstable,
                  skip_skip = SkipSkip} = P, Fd,
          #cmd{lineno = LineNo, arg = {config, Var, NameVal}} = Cmd) ->
    case Var of
        "require" when RunMode =:= execute ->
            {IsSet, Name, Val} = test_var(P, NameVal),
            case IsSet of
                true ->
                    {P, Cmd};
                false ->
                    Format = "FAIL as required variable ~s is not set",
                    parse_skip(P, Fd, LineNo, format_val(Format, [Name], Val))
            end;
        "skip" when not SkipSkip ->
            {IsSet, Name, Val} = test_var(P, NameVal),
            case IsSet of
                false ->
                    {P, Cmd};
                true ->
                    Format = "SKIP as variable ~s is set",
                    parse_skip(P, Fd, LineNo, format_val(Format, [Name], Val))
            end;
        "skip_unless" when not SkipSkip ->
            {IsSet, Name, Val} = test_var(P, NameVal),
            case IsSet of
                true ->
                    {P, Cmd};
                false ->
                    Format = "SKIP as variable ~s is not set",
                    parse_skip(P, Fd, LineNo, format_val(Format, [Name], Val))
            end;
        "unstable" when not SkipSkip, SkipUnstable ->
            {IsSet, Name, Val} = test_var(P, NameVal),
            case IsSet of
                false ->
                    {P, Cmd};
                true ->
                    Format = "SKIP UNSTABLE as variable ~s is set",
                    parse_skip(P, Fd, LineNo, format_val(Format, [Name], Val))
            end;
        "unstable_unless" when not SkipSkip, SkipUnstable ->
            {IsSet, Name, Val} = test_var(P, NameVal),
            case IsSet of
                true ->
                    {P, Cmd};
                false ->
                    Format = "SKIP UNSTABLE as variable ~s is not set",
                    parse_skip(P, Fd, LineNo, format_val(Format, [Name], Val))
            end;
        _ ->
            {P, Cmd}
    end.

test_var(P, VarVal) ->
    lux_utils:test_var(P#pstate.multi_vars, VarVal).

format_val(Format, Args, false) ->
    ?FF(Format, Args);
format_val(Format, Args, Val) ->
    ?FF(Format ++ " to ~p", Args ++ [Val]).

expand_vars(P, Fd, Val, LineNo) ->
    try
        lux_utils:expand_vars(P#pstate.multi_vars, Val, error)
    catch
        throw:{no_such_var, BadVar} ->
            Reason = ["Variable ${", BadVar,
                      "} is not set on line ",
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
        ?SPACE  when Arg =:= [] -> % skip space between args
            split_invoke_args(P, Fd, LineNo, T, Mode, Arg, Args);
        ?SPACE  when Arg =/= [] -> % first space after arg
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

parse_multi(P, Fd, _NextIncr, <<>>,
            #cmd{lineno = LineNo}, _Tokens, MetaCmd) ->
    {GoodKeyword, _} = multi_end_keyword(MetaCmd),
    parse_error(P, Fd, LineNo,
                ["Syntax error at line ", ?i2l(LineNo),
                 ": '", GoodKeyword, "' expected"]);
parse_multi(#pstate{mode = RunMode} = P, Fd, NextIncr, Chars,
            #cmd{lineno = LineNo, orig = OrigLine} = Cmd, Tokens, MetaCmd) ->
    PrefixLen = count_prefix_len(?b2l(OrigLine), 0),
    {P2, RevBefore, FdAfter, RemPrefixLen, MultiIncr} =
        scan_multi(P, Fd, Cmd, PrefixLen, [], NextIncr, MetaCmd),
    LastLineNo0 = LineNo+MultiIncr+length(RevBefore)+1,
    case file_next_wrapper(FdAfter) of
        eof = NewFd ->
            {GoodKeyword, _} = multi_end_keyword(MetaCmd),
            parse_error(P2, NewFd, LastLineNo0,
                        ["Syntax error after line ",
                         ?i2l(LineNo),
                         ": '", GoodKeyword, "' expected"]);
        {line, _, NewFd, Incr} when RemPrefixLen =/= 0 ->
            LastLineNo = LastLineNo0+Incr,
            parse_error(P2, NewFd, LastLineNo,
                        ["Syntax error at line ", ?i2l(LastLineNo),
                         ": multi line block must end in same column as"
                         " it started on line ", ?i2l(LineNo)]);
        {line, _EndOfMulti, NewFd, Incr}
          when RunMode =:= list;
               RunMode =:= list_dir;
               RunMode =:= doc ->
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
            LastLineNo = LastLineNo0+Incr,
            MultiLine =
                case MetaCmd of
                    no_meta ->
                        <<Chars/binary, Blob/binary>>;
                    #cmd{orig = MetaChars} ->
                        <<MetaChars/binary, Blob/binary, "]">>
                end,
            {P3, NewFd, Tokens2} =
                parse_cmd(P2, Fd2, MultiLine, LastLineNo, 0, OrigLine, Tokens),
            parse(P3, NewFd, LastLineNo, Tokens2)
    end.

count_prefix_len([H | T], N) ->
    case H of
        ?SPACE -> count_prefix_len(T, N+1);
        $\t    -> count_prefix_len(T, N+?TAB_LEN);
        $"     -> N
    end.

scan_multi(P, Fd, Cmd, PrefixLen, Acc, Incr0, MetaCmd) ->
    case file_next_wrapper(Fd) of
        {line, Line, NewFd, Incr} ->
            NextIncr = Incr0+Incr,
            case scan_single(P, Fd, Cmd, Line, PrefixLen, Incr0, MetaCmd) of
                {more, P2, Line2} ->
                    scan_multi(P2, NewFd, Cmd, PrefixLen,
                               [Line2 | Acc], NextIncr, MetaCmd);
                {nomore, P2, RemPrefixLen} ->
                    {P2, Acc, file_push(NewFd,[Line]), RemPrefixLen, NextIncr}
            end;
        eof = NewFd->
            {P, Acc, NewFd, PrefixLen, Incr0}
    end.

scan_single(P, Fd, Cmd, Line, PrefixLen, Incr, MetaCmd) ->
    {GoodKeyword, BadKeyword} = multi_end_keyword(MetaCmd),
    Sz = byte_size(GoodKeyword),
    case Line of
        <<BadKeyword/binary>> when is_binary(BadKeyword) ->
            ThisLineNo = Cmd#cmd.lineno+Incr,
            parse_error(P, Fd, ThisLineNo,
                        ["Syntax error at line ", ?i2l(ThisLineNo),
                         ": '", GoodKeyword, "' expected.",
                         " Found nested '", BadKeyword, "'."]);
        <<GoodKeyword:Sz/binary, UnStripped/binary>> ->
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
            scan_single(P, Fd, Cmd, Rest, PrefixLen - 1, Incr, MetaCmd);
        <<"\t", Rest/binary>> ->
            Left = PrefixLen - ?TAB_LEN,
            if
                Left < 0 -> % Too much leading whitespace
                    Spaces = lists:duplicate(abs(Left), ?SPACE),
                    {more, P, ?l2b([Spaces, Line])};
                true ->
                    scan_single(P, Fd, Cmd, Rest, Left, Incr, MetaCmd)
            end;
        _ ->
            {more, P, Line}
    end.

multi_end_keyword(MetaCmd) ->
    Base = <<"\"\"\"">>,
    case MetaCmd of
        no_meta -> {Base, false};
        #cmd{}  -> {<<Base/binary, "]">>, Base}
    end.

parse_skip(P, Fd, LineNo, IoList) ->
    parse_error(P, Fd, skip, LineNo, IoList).

parse_error(P, Fd, LineNo, IoList) ->
    parse_error(P, Fd, error, LineNo, IoList).

parse_error(P, Fd, Tag, LineNo, IoList) ->
    %% io:format("\nerror(~p): ~p\n", [?LINE, ?stacktrace()]),
    NewPosStack = cmd_pos_stack(P, LineNo),
    NewIoList = ?l2b(IoList),
    reparse_error(Fd, Tag, NewPosStack, NewIoList).

reparse_error(Fd, Tag, PosStack, IoList) ->
    file_close(Fd),
    throw({Tag, PosStack, IoList}).

make_warning(P, OptCmd, IoList) ->
    File = P#pstate.orig_file,
    FullLineNo = full_lineno(P, OptCmd),
    #warning{file = File, lineno = FullLineNo, details = ?l2b(IoList)}.

add_warning(P, OptCmd, IoList) ->
    Warning = make_warning(P, OptCmd, IoList),
    P#pstate{warnings = P#pstate.warnings ++ [Warning]}.

full_lineno(_P, undefined) ->
    "0";
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
