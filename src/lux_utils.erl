%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2014 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_utils).
-compile(export_all).
-export([builtin_dict/0, system_dict/0, expand_vars/3,
         summary/2, summary_prio/1,
         multiply/2, drop_prefix/1, drop_prefix/2,
         strip_leading_whitespaces/1, strip_trailing_whitespaces/1,
         normalize_newlines/1, expand_lines/1,
         to_string/1, tag_prefix/2,
         progress_write/2, fold_files/5, foldl_cmds/5, foldl_cmds/6,
         pretty_full_lineno/1, filename_split/1, dequote/1,
         now_to_string/1, datetime_to_string/1, verbatim_match/2,
         diff/2,
         cmd/2, chop_newline/1]).

-include("lux.hrl").

builtin_dict() ->
    %% Alphabetic order
    [
     "_BS_="  ++ [8],  % backspace
     "_CR_="  ++ [13]  % carriage return
    ] ++  ctrl_dict() ++
    [
     "_DEL_=" ++ [127], % delete
     "_LF_="  ++ [10],  % line feed
     "_TAB_=" ++ [9]   % tab
    ].

ctrl_dict() -> % From a-z
    %% Alphabetic order
    ["_CTRL_" ++ [Ctrl+64] ++ "_=" ++ [Ctrl] ||  Ctrl <- lists:seq(1,26)].

system_dict() ->
    %% Alphabetic order
    lists:sort(os:getenv()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expand varibles

%% MissingVar = keep | empty | error
expand_vars(Dicts, String, MissingVar) when is_list(String) ->
    do_expand_vars(Dicts, normal, String, [], MissingVar);
expand_vars(Dicts, Bin, MissingVar) when is_binary(Bin) ->
    list_to_binary(expand_vars(Dicts, binary_to_list(Bin), MissingVar)).

do_expand_vars(Dicts, normal = Mode, [H | T], Acc, MissingVar) ->
    case H of
        $$ ->
            do_expand_vars(Dicts, {variable, []}, T, Acc, MissingVar);
        _ ->
            do_expand_vars(Dicts, Mode, T, [H | Acc], MissingVar)
    end;
do_expand_vars(_Dicts, normal, [], Acc, _MissingVar) ->
    lists:reverse(Acc);
do_expand_vars(Dicts, {variable, []}, [$$=H | T], Acc, MissingVar) ->
    do_expand_vars(Dicts, normal, T, [H | Acc], MissingVar);
do_expand_vars(Dicts, {variable, []}, [${=H | T], Acc, MissingVar) ->
    FailAcc = [H, $$ | Acc],
    case split_name(T, [], FailAcc) of
        {match, Name, FailAcc2, T2} ->
            %% Found a variable name "prefix${var}suffix"
            Acc2 = replace_var(Dicts, Name, Acc, FailAcc2, MissingVar),
            do_expand_vars(Dicts, normal, T2, Acc2, MissingVar);
        {nomatch, _, _, []} ->
            %% False positive. Continue to search.
            do_expand_vars(Dicts, normal, T, FailAcc, MissingVar)
    end;
do_expand_vars(Dicts, {variable, RevName}, [H | T], Acc, MissingVar) ->
    case is_var(H) of
        true ->
            do_expand_vars(Dicts, {variable, [H|RevName]}, T, Acc, MissingVar);
        false ->
            %% Found a variable name "prefix$var/suffix"
            Name = lists:reverse(RevName),
            FailAcc = RevName ++ [$$ | Acc],
            Acc2 = replace_var(Dicts, Name, Acc, FailAcc, MissingVar),
            do_expand_vars(Dicts, normal, [H | T], Acc2, MissingVar)
    end;
do_expand_vars(Dicts, {variable, RevName}, [], Acc, MissingVar) ->
    %% Found a variable name "prefix$var"
    Name = lists:reverse(RevName),
    FailAcc = RevName ++ [$$ | Acc],
    Acc2 = replace_var(Dicts, Name, Acc, FailAcc, MissingVar),
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

replace_var(_Dicts, "", _Acc, FailAcc, _MissingVar) ->
    %% False positive
    FailAcc;
replace_var(Dicts, Name, Acc, FailAcc, MissingVar) ->
    do_replace_var(Dicts, Name, Acc, FailAcc, MissingVar).

do_replace_var([], Name, _Acc, FailAcc, MissingVar) ->
    %% No such var
    case MissingVar of
        keep  -> FailAcc; % keep "$var"
        empty -> "";      % replace with ""
        error -> throw({no_such_var, Name})
    end;
do_replace_var([Dict | Dicts], Name, Acc, FailAcc, MissingVar) ->
    case lookup_var(Dict, Name) of
        false ->
            do_replace_var(Dicts, Name, Acc, FailAcc, MissingVar);
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

summary(Old, New) ->
    case summary_prio(New) > summary_prio(Old) of
        true  -> New;
        false -> Old
    end.

summary_prio(Summary) ->
    case Summary of
        enable         -> 0;
        no_data        -> 1;
        success        -> 2;
        none           -> 3;
        skip           -> 4;
        warning        -> 5;
        secondary_fail -> 6;
        fail           -> 7;
        error          -> 8;
        disable        -> 999
    end.

multiply(infinity, _Factor) ->
    infinity;
multiply(Timeout, Factor) ->
    (Timeout * Factor) div 1000.

drop_prefix(File) ->
    {ok, Cwd} = file:get_cwd(),
    lux_utils:drop_prefix(Cwd, File).

drop_prefix(Prefix, File) ->
    case do_drop_prefix(filename:split(Prefix),
                        filename:split(File)) of
        [] ->
            File;
        Suffix ->
            filename:join(Suffix)
    end.

do_drop_prefix([H | Prefix], [H | File]) ->
    do_drop_prefix(Prefix, File);
do_drop_prefix(_, File) ->
    File.

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

dequote(" expect " ++ _ = L) ->
    re:replace(L, <<"\\\\\\\\R">>, <<"\n    ">>, [global, {return, list}]);
dequote([$\"|T]) ->
    [$\"|dequote1(T)];
dequote([H|T]) ->
    [H|dequote(T)];
dequote([]) ->
    [].

dequote1([$\\,$\\|T]) ->
    [$\\|dequote1(T)];
dequote1([$\\,$r,$\\,$n|T]) ->
    "\n    " ++ dequote1(T);
dequote1([$\\,$n|T]) ->
    "\n    " ++ dequote1(T);
dequote1([H|T]) ->
    [H|dequote1(T)];
dequote1([]) ->
    [].

progress_write(Progress, String) ->
    case Progress of
        silent  -> ok;
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
    Pos = {RevFile, LineNo, Type},
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
                SubFun(SubFile, SubCmds, [Pos | CmdStack]);
            macro when Depth =:= static;
                       Depth =:= dynamic ->
                {macro, _Name, _ArgNames, _FirstLineNo, _LastLineNo, Body} =
                    Arg,
                SubFun(File, Body, [Pos | CmdStack]);
            loop when Depth =:= static;
                      Depth =:= dynamic ->
                {loop, _Name, _ItemStr, _FirstLineNo, _LastLineNo, Body} = Arg,
                SubStack = [{RevFile, 0, iteration}, Pos | CmdStack],
                SubFun(File, Body, SubStack);
            invoke when Depth =:= dynamic ->
                case lux_interpret:lookup_macro(OptI, Cmd) of
                    {ok, _NewCmd, [#macro{file = MacroFile, cmd = MacroCmd}]} ->
                        #cmd{arg = MacroArg} = MacroCmd,
                        {macro, _Name, _ArgNames,
                         _FirstLineNo, _LastLineNo, Body} =
                            MacroArg,
                        SubFun(MacroFile, Body, [Pos | CmdStack]);
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
    Pick = fun({_F, L,_T}) when is_integer(L) -> L;
              (L)          when is_integer(L) -> L
           end,
    FullStack2 = lists:dropwhile(fun(FL) -> Pick(FL) < 0 end, FullStack),
    [FileLine | Incl] = lists:reverse(FullStack2),
    LineNo = Pick(FileLine),
    LineNoSuffix = [[":", integer_to_list(Pick(FL))] || FL <- Incl],
    lists:flatten([integer_to_list(LineNo), LineNoSuffix]).

filename_split(FileName) ->
    FileName2 = drop_prefix(FileName),
    lists:reverse(filename:split(FileName2)).

now_to_string(Now) ->
    DateTime = calendar:now_to_local_time(Now),
    datetime_to_string(DateTime).

datetime_to_string({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    lists:concat([Year, "-", p(Month), "-", p(Day), " ",
                  p(Hour), ":", p(Min), ":", p(Sec)]).

p(Int) when Int >= 0, Int < 10 ->
    [$0 | integer_to_list(Int)];
p(Int) when Int < 100 ->
    integer_to_list(Int).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Diff

%% diff_files(OldFile, NewFile) ->
%%     {OldFile, {ok, OldBin}} = {OldFile, file:read_file(OldFile)},
%%     {NewFile, {ok, NewBin}} = {NewFile, file:read_file(NewFile)},
%%     diff(split_lines(OldBin), split_lines(NewBin)).
%%
%% split_lines(<<"">>) ->
%%     [];
%% split_lines(Bin) when is_binary(Bin) ->
%%     Opts = [global],
%%     Bin2 = normalize_regexp(Bin),
%%     binary:split(Bin2, <<"\\\\R">>, Opts).

diff(OldBins, NewBins) ->
    Old = numerate_lines(OldBins),
    New = numerate_lines(NewBins),
    Patch = diff(New, Old, [], 0),
    merge(Patch, Old).

numerate_lines(List) ->
    numerate_lines(List, 1, []).

numerate_lines([H|T], N, Acc) ->
    numerate_lines(T, N+1, [{N,H}|Acc]);
numerate_lines([], _N, Acc) ->
    lists:reverse(Acc).

-type patch() :: {From :: non_neg_integer(),
                  To   :: non_neg_integer()} |
                  binary().

-type diff() :: {common,[binary()]} |
                {insert,[binary()]} |
                {delete,[binary()]} |
                {replace,[binary()],[binary()]}.

-spec diff(New :: [{non_neg_integer(),binary()}],
           Old :: [{non_neg_integer(),binary()}],
           [patch()],
           Min :: non_neg_integer()) ->
          [patch()].

diff([], _, Patch,_Min) ->
    lists:reverse(Patch);
diff([{_,Line}|T]=New, Old, Patch, Min) ->
    case match(New, Old, Min) of
        {yes, From, To, Rest} ->
            diff(Rest, Old, [{From,To}|Patch], To);
        no ->
            diff(T, Old, [Line|Patch], Min)
    end.

match([{_,NewElem}|NewT]=New, [{From,OldElem}|OldT], Min) when From > Min ->
    case equal(OldElem, NewElem) of
        match   -> extend_match(NewT, OldT, From, From);
        nomatch -> match(New, OldT, Min)
    end;
match(New, [_|T], Min) ->
    match(New, T, Min);
match(_New, [], _Min) ->
    no.

extend_match([{_,NewElem}|NewT]=New, [{To,OldElem}|OldT], From, PrevTo) ->
    case equal(OldElem, NewElem) of
        match   -> extend_match(NewT, OldT, From, To);
        nomatch -> {yes, From, PrevTo, New}
    end;
extend_match(New, _, From, To) ->
    {yes, From, To, New}.

equal(Expected, Expected) ->
    match;
equal(<<"">>, _Actual) ->
    nomatch;
equal("", _Actual) ->
    nomatch;
equal(Expected0, Actual) when is_binary(Expected0); is_list(Expected0) ->
    Expected = normalize_regexp(Expected0),
    try
        re:run(Actual, Expected,[{capture, none}, notempty])
    catch _:_ ->
            nomatch
    end;
equal(_Expected, _Actual) ->
    nomatch.

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

normalize_newlines(IoList) ->
    re:replace(IoList, <<"(\r\n|\r|\n)">>, <<"\\\\R">>,
               [global, {return, binary}]).

expand_lines([]) ->
    [];
expand_lines([Line]) ->
    Line;
expand_lines([Line | Lines]) ->
    [Line, "\n", expand_lines(Lines)].

-spec merge([patch()], Old  :: [{non_neg_integer(),binary()}]) ->
          [diff()].

merge(Patch, Old) ->
    merge(Patch, Old, 1, []).

merge([{From,To}|Patch], Old, Next, Acc) ->
    {Next2, Common} = get_lines(From, To, Old, []),
    Acc2 =
        if
            From > Next ->
                %% Add missing lines
                {_, Delete} = get_lines(Next, From-1, Old, []),
                add({common,Common}, add({delete,Delete}, Acc));
            true ->
                add({common,Common},Acc)
        end,
    merge(Patch, Old, Next2, Acc2);
merge([Insert|Patch], Old, Next, Acc) ->
    merge(Patch, Old, Next, add({insert,[Insert]},Acc));
merge([], Old, Next, Acc) ->
    %% Add missing lines in the end
    Fun = fun({N,L}, A) when N >= Next -> [L|A];
             (_,A)                    -> A
          end,
    Acc2 =
        case lists:foldl(Fun, [], Old) of
            []     -> Acc;
            Delete -> add({delete,lists:reverse(Delete)}, Acc)
        end,
    lists:reverse(Acc2).

get_lines(_, To, [{To,Line}|_Rest], Acc) ->
    {To+1, lists:reverse([Line|Acc])};
get_lines(From, To, [{From,Line}|Rest], Acc) ->
    get_lines(From+1, To, Rest, [Line|Acc]);
get_lines(From, To, [_|Rest], Acc) ->
    get_lines(From, To, Rest, Acc).

add({insert,Curr}, [{insert,Prev}|Merge]) ->
    [{insert,Prev++Curr}|Merge];
add({delete,Delete}, [{insert,Insert}|Merge]) ->
    [{replace,Insert,Delete}|Merge];
add(Curr, Merge) ->
    [Curr|Merge].

cmd(Cmd, Default) ->
    Output = os:cmd(Cmd++"; echo $?"),
    rsplit(Output, "\n", Default).

rsplit(Line, Seps, Default) ->
    {After, Sep, Before} = split(lists:reverse(Line), Seps, Default),
    {lists:reverse(Before), Sep, lists:reverse(After)}.

split(Line, Seps, Default) ->
    Pred = fun(C) -> not lists:member(C, Seps) end,
    case lists:splitwith(Pred, Line) of
        {Before, ""}          -> {Before, "", Default};
        {Before, [Sep|After]} -> {Before, Sep, After}
    end.

chop_newline(Line) ->
    {Before, _Sep, After} = rsplit(Line, "\n", ""),
    case After of
        "" -> Before;
        _  -> Line
    end.
