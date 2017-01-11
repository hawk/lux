%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_utils).
-export([version/0, timestamp/0,
         builtin_vars/0, system_vars/0, expand_vars/3,
         test_var/2, split_var/2,
         summary/2, summary_prio/1,
         multiply/2, drop_prefix/1, drop_prefix/2, normalize/1,
         strip_leading_whitespaces/1, strip_trailing_whitespaces/1,
         normalize_newlines/1, expand_lines/1, split_lines/1, shrink_lines/1,
         to_string/1, capitalize/1, tag_prefix/2,
         progress_write/2, fold_files/5, foldl_cmds/5, foldl_cmds/6,
         pretty_full_lineno/1, pretty_filename/1, filename_split/1,
         now_to_string/1, datetime_to_string/1, verbatim_match/2,
         diff/2, equal/2, diff_iter/3, diff_iter/4, shrink_diff/2,
         cmd/1, cmd_expected/1, perms/1,
         pick_opt/3]).

-include("lux.hrl").

version() ->
    LoadedApps = application:loaded_applications(),
    {_Name, _Slogan, Version} = lists:keyfind(?APPLICATION, 1, LoadedApps),
    Version.

hidden_apply(M, F, A) ->
    Obfuscated = fun() -> M end(),
    apply(Obfuscated, F, A).

timestamp() ->
    try
        hidden_apply(erlang, timestamp, []) % Avoid xref warning
    catch error:undef ->
        hidden_apply(erlang, now, []) % Avoid compiler warning
    end.

builtin_vars() ->
    %% Alphabetic order
    [
     "_BS_="  ++ [8],  % backspace
     "_CR_="  ++ [13]  % carriage return
    ] ++  ctrl_vars() ++
    [
     "_DEL_=" ++ [127], % delete
     "_LF_="  ++ [10],  % line feed
     "_TAB_=" ++ [9]    % tab
    ].

ctrl_vars() -> % From a-z
    %% Alphabetic order
    ["_CTRL_" ++ [Ctrl+64] ++ "_=" ++ [Ctrl] ||  Ctrl <- lists:seq(1,26)].

system_vars() ->
    %% Alphabetic order
    lists:sort(os:getenv()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expand varibles

%% MissingVar = keep | empty | error
expand_vars(MultiVars, String, MissingVar) when is_list(String) ->
    do_expand_vars(MultiVars, normal, String, [], MissingVar);
expand_vars(MultiVars, Bin, MissingVar) when is_binary(Bin) ->
    ?l2b(expand_vars(MultiVars, binary_to_list(Bin), MissingVar)).

do_expand_vars(MultiVars, normal = Mode, [H | T], Acc, MissingVar) ->
    case H of
        $$ ->
            do_expand_vars(MultiVars, {variable, []}, T, Acc, MissingVar);
        _ ->
            do_expand_vars(MultiVars, Mode, T, [H | Acc], MissingVar)
    end;
do_expand_vars(_MultiVars, normal, [], Acc, _MissingVar) ->
    lists:reverse(Acc);
do_expand_vars(MultiVars, {variable, []}, [$$=H | T], Acc, MissingVar) ->
    do_expand_vars(MultiVars, normal, T, [H | Acc], MissingVar);
do_expand_vars(MultiVars, {variable, []}, [${=H | T], Acc, MissingVar) ->
    FailAcc = [H, $$ | Acc],
    case split_name(T, [], FailAcc) of
        {match, Name, FailAcc2, T2} ->
            %% Found a variable name "prefix${var}suffix"
            Acc2 = replace_var(MultiVars, Name, Acc, FailAcc2, MissingVar),
            do_expand_vars(MultiVars, normal, T2, Acc2, MissingVar);
        {nomatch, _, _, []} ->
            %% False positive. Continue to search.
            do_expand_vars(MultiVars, normal, T, FailAcc, MissingVar)
    end;
do_expand_vars(MultiVars, {variable, RevName}, [H | T], Acc, MissingVar) ->
    case is_var(H) of
        true ->
            do_expand_vars(MultiVars, {variable, [H|RevName]}, T,
                           Acc, MissingVar);
        false ->
            %% Found a variable name "prefix$var/suffix"
            Name = lists:reverse(RevName),
            FailAcc = RevName ++ [$$ | Acc],
            Acc2 = replace_var(MultiVars, Name, Acc, FailAcc, MissingVar),
            do_expand_vars(MultiVars, normal, [H | T], Acc2, MissingVar)
    end;
do_expand_vars(MultiVars, {variable, RevName}, [], Acc, MissingVar) ->
    %% Found a variable name "prefix$var"
    Name = lists:reverse(RevName),
    FailAcc = RevName ++ [$$ | Acc],
    Acc2 = replace_var(MultiVars, Name, Acc, FailAcc, MissingVar),
    lists:reverse(Acc2).

split_name([Char | Rest], Name, Fail) ->
    %% Search for first } char
    if
        Char =/= $} ->
            split_name(Rest, [Char | Name], [Char | Fail]);
        true ->
            {match, lists:reverse(Name), [Char | Fail], Rest}
    end;
split_name([] = Rest, Name, Fail) ->
    {nomatch, lists:reverse(Name), Fail, Rest}.

is_var(Char) ->
    if
        Char >= $a, Char =< $z -> true;
        Char >= $A, Char =< $Z -> true;
        Char >= $0, Char =< $9 -> true;
        Char =:= $_            -> true;
        true                   -> false
    end.

replace_var(_MultiVars, "", _Acc, FailAcc, _MissingVar) ->
    %% False positive
    FailAcc;
replace_var(MultiVars, Name, Acc, FailAcc, MissingVar) ->
    do_replace_var(MultiVars, Name, Acc, FailAcc, MissingVar).

do_replace_var([], Name, _Acc, FailAcc, MissingVar) ->
    %% No such var
    case MissingVar of
        keep  -> FailAcc; % keep "$var"
        empty -> "";      % replace with ""
        error -> throw({no_such_var, Name})
    end;
do_replace_var([Vars | MultiVars], Name, Acc, FailAcc, MissingVar) ->
    case lookup_var(Vars, Name) of
        false ->
            do_replace_var(MultiVars, Name, Acc, FailAcc, MissingVar);
        Val ->
            lists:reverse(Val) ++ Acc
    end.

lookup_var([VarVal | VarVals], Name) ->
    case do_lookup_var(VarVal, Name) of
        false -> lookup_var(VarVals, Name);
        Val   -> Val
    end;
lookup_var([], _Name) ->
    false.

do_lookup_var([H|VarVal], [H|Name]) ->
    do_lookup_var(VarVal, Name);
do_lookup_var([$=|Val], []) ->
    Val;
do_lookup_var(_, _) ->
    false.

test_var(Vars, VarVal) ->
    case split_var(VarVal, []) of
        {Var, Val} ->
            ok;
        false ->
            Var = VarVal,
            Val = false
    end,
    UnExpanded = [$$ | Var],
    try
        Expanded = expand_vars(Vars, UnExpanded, error),
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

split_var([$= | Val], Var) ->
    {lists:reverse(Var), Val};
split_var([H | T], Var) ->
    split_var(T, [H | Var]);
split_var([], _Var) ->
    false.

summary(Old, New) ->
    case summary_prio(New) > summary_prio(Old) of
        true  -> New;
        false -> Old
    end.

summary_prio(Summary) ->
    case Summary of
        validate       -> 0;
        enable         -> 1;
        no_data        -> 2;
        success        -> 3;
        none           -> 4;
        skip           -> 5;
        warning        -> 5;
        secondary_fail -> 7;
        fail           -> 8;
        error          -> 9;
        disable        -> 999
    end.

multiply(infinity, _Factor) ->
    infinity;
multiply(Timeout, Factor) ->
    (Timeout * Factor) div 1000.

drop_prefix(File) ->
    {ok, Cwd} = file:get_cwd(),
    lux_utils:drop_prefix(Cwd, File).

drop_prefix(Prefix, File) when is_binary(Prefix) ->
    drop_prefix(binary_to_list(Prefix), File);
drop_prefix(Prefix, File) when is_binary(File) ->
    ?l2b(drop_prefix(Prefix, binary_to_list(File)));
drop_prefix(Prefix, File) when is_list(Prefix), is_list(File) ->
    SplitPrefix = filename:split(Prefix),
    SplitFile = filename:split(File),
    do_drop_prefix(SplitPrefix, SplitFile, SplitFile).

do_drop_prefix([H | Prefix], [H | File], OrigFile) ->
    do_drop_prefix(Prefix, File, OrigFile);
do_drop_prefix([], [], _OrigFile) ->
    ".";
do_drop_prefix([], Rest, _OrigFile) ->
    filename:join(Rest);
do_drop_prefix(_Prefix, _Rest, OrigFile) ->
    filename:join(OrigFile).

normalize(File) when is_binary(File) ->
    ?l2b(normalize(binary_to_list(File)));
normalize(File) ->
    do_normalize(filename:split(filename:absname(File)), []).

do_normalize([H|T], Acc) ->
    Acc2 =
        case H of
            "."                  -> Acc;
            ".." when Acc =:= [] -> Acc;
            ".."                 -> tl(Acc);
            _                    -> [H|Acc]
        end,
    do_normalize(T, Acc2);
do_normalize([], Acc) ->
    filename:join(lists:reverse(Acc)).

strip_leading_whitespaces(Bin) when is_binary(Bin) ->
    re:replace(Bin, "^[\s\t]+", "", [{return, binary}]).

strip_trailing_whitespaces(Bin) when is_binary(Bin) ->
    re:replace(Bin, "[\s\t]+$", "", [{return, binary}]).

to_string(Atom) when is_atom(Atom) ->
    to_string(atom_to_list(Atom));
to_string(Bin) when is_binary(Bin) ->
    to_string(binary_to_list(Bin));
to_string([H | T]) when is_integer(H) ->
    [$$ | Chars] = io_lib:write_char(H),
    case Chars of
        [$\\, $s] -> " " ++ to_string(T);
        [$\\, $t] -> "\t" ++ to_string(T);
        _         -> Chars ++ to_string(T)
    end;
to_string([H | T]) ->
    to_string(H) ++ to_string(T);
to_string([]) ->
    [].

capitalize([H | T]) ->
    [string:to_upper(H) | T];
capitalize([] = L) ->
    L.

progress_write(Progress, String) ->
    case Progress of
        silent  -> ok;
        summary -> ok;
        brief   -> io:format("~s", [String]);
        doc     -> io:format("~s", [String]);
        compact -> ok;
        verbose -> ok
    end.

tag_prefix(Tag, Width) when is_atom(Tag) ->
    tag_prefix(atom_to_list(Tag), Width);
tag_prefix(Tag, Width) when is_binary(Tag) ->
    tag_prefix(binary_to_list(Tag), Width);
tag_prefix(Tag, Width) ->
    string:left(Tag, Width-2) ++ ": ".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fold files - same as filelib:fold_files/5 but it does not follow symlinks

-include_lib("kernel/include/file.hrl").

-spec fold_files(file:name(), string(), boolean(), fun((_,_) -> _), _) -> _.
fold_files(Dir, RegExp, Recursive, Fun, Acc) ->
    {ok, RegExp2} = re:compile(RegExp,[unicode]),
    do_fold_files(Dir, RegExp2, Recursive, Fun, Acc, true).

do_fold_files(File, RegExp, Recursive, Fun, Acc, IsTopLevel) ->
    case file:read_link_info(File) of
        {ok, #file_info{type = Type}} ->
            case Type of
                directory when IsTopLevel; Recursive->
                    Dir = File,
                    case file:list_dir(Dir) of
                        {ok, Files} ->
                            SubFun =
                                fun(F, A) ->
                                        do_fold_files(F,
                                                      RegExp,
                                                      Recursive,
                                                      Fun,
                                                      A,
                                                      false)
                                end,
                            SubFiles = [filename:join([Dir, F]) || F <- Files],
                            lists:foldl(SubFun, Acc, SubFiles);
                        {error, _Reason} ->
                            Acc
                    end;
                directory ->
                    Acc;
                _ -> % device | regular | symlink | other
                    Base = filename:basename(File),
                    case re:run(Base, RegExp, [{capture,none}]) of
                        match   -> Fun(File, Acc);
                        nomatch -> Acc
                    end
            end;
        {error, _Reason} ->
            Acc
    end.

%% Iterate over commands
foldl_cmds(Fun, Acc, File, CmdStack, Cmds) ->
    foldl_cmds(Fun, Acc, File, CmdStack, Cmds, include).

%% Depth :: main         - iterate only over main script file
%%        | include      - do also iterate over include files
%%        | static       - do also iterate over loops and macros
%%        | {dynamic, I} - do also iterate over macros invokations
foldl_cmds(Fun, Acc, File, CmdStack, Cmds, Depth) when is_atom(Depth) ->
    foldl_cmds(Fun, Acc, File, CmdStack, Cmds, {Depth, undefined});
foldl_cmds(Fun, Acc, File, CmdStack, Cmds, {Depth, OptI})
  when Depth =:= main; Depth =:= include; Depth =:= static; Depth =:= dynamic ->
    File2 = lux_utils:drop_prefix(File),
    RevFile = lux_utils:filename_split(File2),
    do_foldl_cmds(Fun, Acc, File2, RevFile, CmdStack, Cmds, {Depth, OptI}).

do_foldl_cmds(Fun, Acc, File, RevFile, CmdStack,
              [#cmd{type = Type, lineno = LineNo, arg = Arg} = Cmd | Cmds],
              {Depth, OptI} = FullDepth) ->
    CmdPos = #cmd_pos{rev_file = RevFile, lineno = LineNo, type = Type},
    SubFun =
        fun(SubFile, SubCmds, SubStack) ->
                SubAcc = Fun(Cmd, RevFile, SubStack, Acc),
                foldl_cmds(Fun, SubAcc, SubFile, SubStack, SubCmds, FullDepth)
        end,
    Acc2 =
        case Type of
            include when Depth =:= include;
                         Depth =:= static;
                         Depth =:= dynamic ->
                {include, SubFile, _FirstLineNo, _LastFileNo, SubCmds} = Arg,
                SubFun(SubFile, SubCmds, [CmdPos | CmdStack]);
            macro when Depth =:= static;
                       Depth =:= dynamic ->
                {macro, _Name, _ArgNames, _FirstLineNo, _LastLineNo, Body} =
                    Arg,
                SubFun(File, Body, [CmdPos | CmdStack]);
            loop when Depth =:= static;
                      Depth =:= dynamic ->
                {loop, _Name, _ItemStr, _FirstLineNo, _LastLineNo, Body} = Arg,
                LoopPos = #cmd_pos{rev_file = RevFile,
                                   lineno = LineNo,
                                   type = iteration},
                SubStack = [LoopPos, CmdPos | CmdStack],
                SubFun(File, Body, SubStack);
            invoke when Depth =:= dynamic ->
                case lux_interpret:lookup_macro(OptI, Cmd) of
                    {ok, _NewCmd, [#macro{file = MacroFile, cmd = MacroCmd}]} ->
                        #cmd{arg = MacroArg} = MacroCmd,
                        {macro, _Name, _ArgNames,
                         _FirstLineNo, _LastLineNo, Body} =
                            MacroArg,
                        SubFun(MacroFile, Body, [CmdPos | CmdStack]);
                _NoMatch ->
                        %% Ignore non-existent macro
                        Acc
                end;
            _ ->
                Fun(Cmd, RevFile, CmdStack, Acc)
        end,
    do_foldl_cmds(Fun, Acc2, File, RevFile, CmdStack, Cmds, FullDepth);
do_foldl_cmds(_Fun, Acc, _File, _RevFile, _CmdStack, [], {_Depth, _OptI}) ->
    Acc.

pretty_full_lineno(FullStack) ->
    Pick = fun(#cmd_pos{lineno=L}) when is_integer(L) -> L;
              (L)                  when is_integer(L) -> L
           end,
    FullStack2 = lists:dropwhile(fun(FL) -> Pick(FL) < 0 end, FullStack),
    [FileLine | Incl] = lists:reverse(FullStack2),
    LineNo = Pick(FileLine),
    LineNoSuffix = [[":", ?i2l(Pick(FL))] || FL <- Incl],
    lists:flatten([?i2l(LineNo), LineNoSuffix]).

pretty_filename(RevFile) ->
    filename:join(lists:reverse(RevFile)).

filename_split(FileName) ->
    FileName2 = drop_prefix(FileName),
    lists:reverse(filename:split(FileName2)).

now_to_string({_Mega, _Secs, Micros} = Now) ->
    DateTime = calendar:now_to_local_time(Now),
    datetime_to_string(DateTime, [".", p(Micros, 6)]).

datetime_to_string(DateTime) ->
    datetime_to_string(DateTime, []).

datetime_to_string({{Year, Month, Day}, {Hour, Min, Sec}}, Decimals) ->
    lists:concat([Year, "-", p(Month), "-", p(Day), " ",
                  p(Hour), ":", p(Min), ":", p(Sec)] ++ Decimals).

p(Int) ->
    p(Int, 2).

p(Int, Len) ->
    string:right(?i2l(Int), Len, $0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verbatim match
verbatim_match(<<>>, _Expected) ->
    nomatch;
verbatim_match(_Actual, <<>>) ->
    nomatch;
verbatim_match(Actual, Expected) ->
    verbatim_search(Actual, Expected, Expected, 0).

verbatim_normalize(<<"\r\n", Rest/binary>>) ->
    {1, <<"\n", Rest/binary>>};
verbatim_normalize(<<"\n\r", Rest/binary>>) ->
    {1, <<"\n", Rest/binary>>};
verbatim_normalize(<<"\r", Rest/binary>>) ->
    {0, <<"\n", Rest/binary>>};
verbatim_normalize(Rest) ->
    {0, Rest}.

verbatim_search(Actual, Expected, Orig, Pos) ->
    {Add, Actual2} = verbatim_normalize(Actual),
    {_Sub, Expected2} = verbatim_normalize(Expected),
    verbatim_search2(Actual2, Expected2, Orig, Pos+Add).

verbatim_search2(<<Match:1/binary, Actual/binary>>,
                 <<Match:1/binary, Expected/binary>>,
                 Orig,
                 Pos) ->
    %% First match
    verbatim_collect(Actual, Expected, Orig, Pos, Pos, 1);
verbatim_search2(<<_A:1/binary, Actual/binary>>,
                 Expected,
                 Orig,
                 Pos) ->
    %% No match while searching - reset expr
    verbatim_search(Actual, Expected, Orig, Pos+1);
verbatim_search2(_Actual,
                 _Expected,
                 _Orig,
                 _Pos) ->
    nomatch.

verbatim_collect(Actual, Expected, Orig, Base, Pos, Len) ->
    {Add, Actual2} = verbatim_normalize(Actual),
    {_Sub, Expected2} = verbatim_normalize(Expected),
    verbatim_collect2(Actual2, Expected2, Orig, Base, Pos+Add, Len+Add).

verbatim_collect2(<<Match:1/binary, Actual/binary>>,
                 <<Match:1/binary, Expected/binary>>,
                 Orig,
                 Base,
                 Pos,
                 Len) ->
    %% Match
    verbatim_collect(Actual, Expected, Orig, Base, Pos+1, Len+1);
verbatim_collect2(<<_A:1/binary, Actual/binary>>,
                 <<_E:1/binary, _/binary>>,
                 Orig,
                 _Base,
                 Pos,
                 _Len) ->
    %% No match
    verbatim_search(Actual, Orig, Orig, Pos+1);
verbatim_collect2(_Actual, <<>>, _Orig, Base, _Pos, Len) ->
    %% Match completed
    {match, [{Base, Len}]};
verbatim_collect2(_Actual, _Expected, _Orig, _Base, _Pos, _Len) ->
    %% No match
    nomatch.

normalize_newlines(IoList) ->
    normalize_newlines(IoList, <<"\\\\R">>).

normalize_newlines(IoList, To) ->
    re:replace(IoList, <<"(\r\n|\r|\n)">>, To, [global, {return, binary}]).

expand_lines([] = Line) ->
    Line;
expand_lines([_] = Line) ->
    Line;
expand_lines([Line | Lines]) ->
    [Line, "\n", expand_lines(Lines)].

split_lines(IoList) ->
    split_lines(IoList, <<"\\\\R">>).

split_lines(IoList, To) ->
    Normalized = normalize_newlines(IoList, To),
    binary:split(Normalized, To, [global]).

shrink_lines(Lines) ->
    case Lines of
        [H1, H2, H3 | HT] ->
            case lists:reverse(HT) of
                [T1, T2, T3, _T4, _T5, _T6 | TT] ->
                    Len = ?l2b(?i2l(length(TT)+3)),
                    [H1, H2, H3,
                     <<"... ", Len/binary," lines not shown...">>,
                     T3, T2, T1];
                _ ->
                    Lines
            end;
        _ ->
            Lines
    end.

cmd(Cmd) ->
    Output = os:cmd(Cmd++"; echo $?"),
    Tokens = string:tokens(Output, "\n"),
    [CmdStatus | Rest] = lists:reverse(Tokens),
    {lists:reverse(Rest), CmdStatus}.

cmd_expected(Cmd) ->
    case Cmd of
        #cmd{type = expect, arg = {endshell, _RegexpOper, Expected, _MP}} ->
            ok;
        #cmd{type = expect, arg = {verbatim, _RegexpOper, Expected}} ->
            ok;
        #cmd{type = expect, arg = {template, _RegexpOper, Expected}} ->
            ok;
        #cmd{type = expect, arg = {regexp, _RegexpOper, Expected}} ->
            ok;
        #cmd{type = expect, arg = {mp, _RegexpOper, Expected, _MP, _Multi}} ->
            ok;
        #cmd{} ->
            Expected = <<"">>
    end,
    Expected.

%% Generate all permutations of the elements in a list
perms([])->
    [[]];
perms(L) ->
    [[H|T] || H <- L,
              T <- perms(L--[H])].

pick_opt(Tag, [{Tag, NewVal} | Opts], _OldVal) ->
    pick_opt(Tag, Opts, NewVal);
pick_opt(Tag, [{_Tag, _Val} | Opts], Val) ->
    pick_opt(Tag, Opts, Val);
pick_opt(_Tag, [], Val) ->
    Val.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Diff

diff(Old, New) ->
    Equal =
        fun(O, N) ->
                case lux_utils:equal(O, N) of
                    match   -> true;
                    nomatch -> false
                end
        end,
    lux_diff:compare2(Old, New, Equal).

equal(Expected, Expected) ->
    match;
equal(Expected0, Actual) when is_binary(Expected0); is_list(Expected0) ->
    Expected = normalize_regexp(Expected0),
    try
        re:run(Actual, Expected,[{capture, none}, notempty])
    catch _:_ ->
            nomatch
    end.

normalize_regexp(<<Prefix:1/binary, _/binary>> = RegExp)
  when Prefix =/= <<"^">> ->
    normalize_regexp(<<"^", RegExp/binary>>);
normalize_regexp(RegExp) when is_binary(RegExp) ->
    Size = byte_size(RegExp)-1,
    case RegExp of
        <<_:Size/binary, "$">> ->
            RegExp;
        _ ->
            normalize_regexp(<<RegExp/binary, "$">>)
    end;
normalize_regexp([Prefix|RegExp])
  when Prefix =/= $^ ->
    normalize_regexp([$^|RegExp]);
normalize_regexp(RegExp) when is_list(RegExp) ->
    case lists:last(RegExp) of
        $\$ ->
            RegExp;
        _ ->
            normalize_regexp(RegExp++"$")
    end.

-type elem() :: binary() | char().
-type op() :: {common,[elem()]} |
              {del, [elem()]} |
              {add,[elem()]} |
              {replace, Del :: [elem()], Add :: [elem()]} |
              {nested, Del :: [elem()], Add :: [elem()], NestedAcc :: acc()}.
-type context() :: first | middle | last. % Single diff implies 'last'
-type mode() :: flat | deep | nested.
-type acc() :: term().
-type callback() :: fun((op(), mode(), context(), acc()) -> acc()).
-spec diff_iter([binary()], [binary()], mode(), callback()) -> acc().
-type diff() :: lux_diff:compact_diff().
diff_iter(Old, New, Mode, Fun) when Mode =:= flat; Mode =:= deep ->
    Diff = diff(Old, New),
    diff_iter(Diff, Mode, Fun).

-spec diff_iter(diff(), mode(), callback()) -> acc().
diff_iter(Diff, Mode, Fun) ->
    InitialAcc = [],
    diff_iter_loop(Diff, Mode, Fun, InitialAcc).

diff_iter_loop([H|T], Mode, Fun, Acc) ->
    Context = context(Acc, T),
    case H of
        Common when is_list(Common) ->
            NewAcc = Fun({common,Common}, Mode, Context, Acc),
            diff_iter_loop(T, Mode, Fun, NewAcc);
        {'-', Del} when element(1, hd(T)) =:= '+' ->
            Add = element(2, hd(T)),
            diff_iter_loop([{'!',Del,Add} | tl(T)], Mode, Fun, Acc);
        {'-', Del} ->
            NewAcc = Fun({del,Del}, Mode, Context, Acc),
            diff_iter_loop(T, Mode, Fun, NewAcc);
        {'+', Add} when element(1, hd(T)) =:= '-' ->
            Del = element(2, hd(T)),
            diff_iter_loop([{'!',Del,Add} | tl(T)], Mode, Fun, Acc);
        {'+', Add} ->
            NewAcc = Fun({add,Add}, Mode, Context, Acc),
            diff_iter_loop(T, Mode, Fun, NewAcc);
        {'!', Del, Add} when Mode =:= deep ->
            DelChars = ?b2l(?l2b(expand_lines(Del))),
            AddChars = ?b2l(?l2b(expand_lines(Add))),
            NestedDiff = lux_diff:compare(DelChars, AddChars),
            NestedAcc = diff_iter(NestedDiff, nested, Fun),
            DeepAcc = Fun({nested,Del,Add,NestedAcc}, Mode, Context, Acc),
            diff_iter_loop(T, Mode, Fun, DeepAcc);
        {'!', Del, Add} when Mode =:= flat;
                             Mode =:= nested ->
            NewAcc = Fun({replace,Del,Add}, Mode, Context, Acc),
            diff_iter_loop(T, Mode, Fun, NewAcc)
    end;
diff_iter_loop([], _Mode, _Fun, Acc) ->
    Acc.

context(_Acc, []) ->
    last;
context([], _Tail) ->
    first;
context(_Aacc, _Tail) ->
    middle.

shrink_diff(Old, New) when is_binary(Old), is_binary(New) ->
    ToIoList =
        fun ({Sign, Bin}) ->
                Prefix =
                    case Sign of
                        '+' -> "+ ";
                        '-' -> "- ";
                        '=' -> "  "
                    end,
                [Prefix, Bin, "\n"]
        end,
    Diff = diff(split_lines(Old, <<"\n">>),
                split_lines(New, <<"\n">>)),
    ShrinkedDiff = shrink(Diff, []),
    Expanded = lux_diff:split_diff(ShrinkedDiff),
    iolist_to_binary(lists:map(ToIoList, Expanded)).

shrink([Common | T], Acc) when is_list(Common) ->
    Shrinked = shrink_lines(Common),
    shrink(T, [Shrinked | Acc]);
shrink([Other | T], Acc) ->
    shrink(T, [Other | Acc]);
shrink([], Acc) ->
    lists:reverse(Acc).
