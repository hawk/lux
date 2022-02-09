%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2022 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_utils).
-export([version/0, timestamp/0, has_timestamp/0,
         builtin_vars/0, system_vars/0, expand_vars/3,
         test_var/2, split_var/2,
         summary/2, summary_prio/1,
         validate_timeout/2, send_after/4, multiply/2, cancel_timer/1,
         drop_prefix/1, drop_prefix/2,
         replace/2,
         normalize_filename/1, quote_newlines/1,
         normalize_newlines/1, normalize_match_regexp/1,
         strip_leading_whitespaces/1, strip_trailing_whitespaces/1,
         expand_lines/1, split_lines/1, shrink_lines/1,
         to_string/1, capitalize/1, tag_prefix/2,
         progress_write/2, fold_files/5, foldl_cmds/5, foldl_cmds/6,
         full_lineno/3, pretty_full_lineno/1, pretty_stack/2,
         cmd_pos/2, cmd_pos/4,
         pretty_filename/1, filename_split/1,
         now_to_string/1, datetime_to_string/1, verbatim_match/2,
         diff/3, equal/3, diff_iter/4, diff_iter/5, shrink_diff/3,
         cmd/1, cmd_expected/1, perms/1,
         pick_opt/3, split/2, join/2,
         is_url/1, start_app/1, stop_app/1,
         user_prefix/0, real_hostname/0, make_warning/3,
         multi_member/2, summary_log_candidates/0]).

-include("lux.hrl").

version() ->
    LoadedApps = application:loaded_applications(),
    case lists:keyfind(?APPLICATION, 1, LoadedApps) of
        {_Name, _Slogan, Version} ->
            Version;
        false ->
            "unknown"
    end.

timestamp() ->
    case has_timestamp() of
        true  -> hidden_apply(erlang, timestamp, []); % Avoid xref warning
        false -> hidden_apply(erlang, now, [])        % Avoid compiler warning
    end.

has_timestamp() ->
    erlang:function_exported(erlang, timestamp, 0).

hidden_apply(M, F, A) ->
    Obfuscated = fun() -> M end(),
    apply(Obfuscated, F, A).

builtin_vars() ->
    %% Alphabetic order
    ascii_vars() ++
    [
     "_BS_="  ++ [8],  % backspace
     "_CR_="  ++ [13]  % carriage return
    ] ++
    ctrl_vars() ++
    [
     "_DEL_=" ++ [127], % delete
     "_ESC_=" ++ [27],  % escape
     "_LF_="  ++ [10],  % line feed
     "_TAB_=" ++ [9]    % tab
    ].

ascii_vars() -> % From decimal 0 to 127
    %% Alphabetic order
    ["_ASCII_" ++ ?i2l(Dec) ++ "_=" ++ [Dec] ||  Dec <- lists:seq(0,127)].

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
    ?l2b(expand_vars(MultiVars, ?b2l(Bin), MissingVar)).

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
                {true, Var, Val};
            Val =:= Expanded ->
                %% Value matches. Possible empty.
                {true, Var, Val};
            true ->
                %% Value does not match
                {false, Var, Val}
        end
    catch
        throw:{no_such_var, _} ->
            %% Variable is not set
            {false, Var, Val}
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
        no_branch      -> 3;
        success        -> 4;
        none           -> 5;
        skip           -> 6;
        warning        -> 7;
        secondary_fail -> 8;
        fail           -> 9;
        error          -> 10;
        disable        -> 999
    end.

validate_timeout(Timeout, Multiplier) ->
    try
        Ref = send_after(Timeout, Multiplier, fake_pid, validate_timeout),
        cancel_timer(Ref),
        true
    catch
        error:badarg ->
            false
    end.

send_after(Timeout, Multiplier, Pid, Msg) ->
    case multiply(Timeout, Multiplier) of
        infinity ->
            NewTimeout = infinity,
            AlmostInfinity = timer:hours(1000),
            Ref = erlang:send_after(AlmostInfinity, fake_pid, Msg);
        NewTimeout ->
            Ref = erlang:send_after(NewTimeout, Pid, Msg)
    end,
    #timer_ref{ref = Ref, timeout = NewTimeout, send_to = Pid, msg = Msg}.

cancel_timer(#timer_ref{ref = Ref, timeout = T, send_to = Pid, msg = Msg}) ->
    case erlang:cancel_timer(Ref) of
        false when Pid =:= self() ->
            receive
                Msg ->
                    0
            after 0 ->
                    0
            end;
        false ->
            0;
        TimeLeft when is_integer(TimeLeft) ->
            case T of
                infinity -> infinity;
                _        -> TimeLeft
            end
    end.

multiply(_Timeout, infinity)    -> infinity;
multiply(infinity, _Multiplier) -> infinity;
multiply(Timeout, Multiplier)   -> (Timeout*Multiplier) div ?ONE_SEC.

drop_prefix(File) ->
    {ok, Cwd} = file:get_cwd(),
    drop_prefix(Cwd, File).

drop_prefix(Prefix, File) when is_binary(Prefix), is_binary(File) ->
    list_to_binary(drop_prefix(binary_to_list(Prefix), binary_to_list(File)));
drop_prefix(Prefix, File) when is_list(Prefix), is_list(File) ->
    SplitPrefix = filename:split(Prefix),
    SplitFile = filename:split(File),
    do_drop_prefix(SplitPrefix, SplitFile, SplitPrefix, File).

do_drop_prefix([H | Prefix], [H | File], OrigPrefix, OrigFile) ->
    do_drop_prefix(Prefix, File, OrigPrefix, OrigFile);
do_drop_prefix([], [], _OrigPrefix, _OrigFile) ->
    ".";
do_drop_prefix([], Rest, _OrigPrefix, _OrigFile) ->
    filename:join(Rest);
do_drop_prefix(DownPrefix, Rest, OrigPrefix, _OrigFile)
  when DownPrefix =/= OrigPrefix ->
    UpPrefix = lists:duplicate(length(DownPrefix), ".."),
    filename:join(UpPrefix ++ Rest);
do_drop_prefix(_DownPrefix, _Rest, _OrigPrefix, OrigFile) ->
    OrigFile.

normalize_filename(File) when is_binary(File) ->
    list_to_binary(normalize_filename(binary_to_list(File)));
normalize_filename(File) when is_list(File) ->
    Delim = "://",
    case split(File, Delim) of
        {Prefix, Rel} ->
            Delim2 = Delim,
            Abs = Rel;
        false ->
            Prefix = "",
            Delim2 = "",
            Abs = filename:absname(File)
    end,
    File2 = do_normalize_filename(filename:split(Abs), []),
    Prefix ++ Delim2 ++ File2.

do_normalize_filename([H|T], Acc) ->
    Acc2 =
        case H of
            "."                  -> Acc;
            ".." when Acc =:= [] -> Acc;
            ".."                 -> tl(Acc);
            _                    -> [H|Acc]
        end,
    do_normalize_filename(T, Acc2);
do_normalize_filename([], Acc) ->
    filename:join(lists:reverse(Acc)).

split(File, Delim) when is_binary(File), is_binary(Delim) ->
    case split(binary_to_list(File), binary_to_list(Delim)) of
        false ->
            false;
        {Before, After} ->
            {list_to_binary(Before), list_to_binary(After)}
    end;
split(File, Delim) when is_list(File), is_list(Delim) ->
    split2(File, Delim, Delim, [], 0).

split2([H|T], [H|DT], OrigDelim, Acc, N) ->
    %% Partial match delim
    split2(T, DT, OrigDelim, [H|Acc], N+1);
split2(Rest, [], _OrigDelim, Acc, N) ->
    %% Full match delim
    {lists:reverse(lists:nthtail(N, Acc)), Rest};
split2([H|T], _Delim, OrigDelim, Acc, _N) ->
    %% Reset delim
    split2(T, OrigDelim, OrigDelim, [H|Acc], 0);
split2([], _Delim, _OrigDelim, _Acc, _N) ->
    false.

join(Dir, <<"/", _/binary>> = File) when is_binary(Dir) ->
    File;
join(Dir, File) when is_binary(Dir), is_binary(File) ->
    ?l2b(join(?b2l(Dir), ?b2l(File)));
join(Dir, []) when is_list(Dir) ->
    Dir;
join(Dir, File) when is_list(Dir), hd(File) =:= $/ ->
    File;
join(Dir, File) when is_list(Dir), is_list(File) ->
    Delim = "://",
    case split(File, Delim) of
        {_Prefix, _Rel} ->
            File;
        false ->
            join2(Dir, File, Dir ++ "/" ++ File)
    end.

join2(".", _File, Default) ->
    Default;
join2(Dir, File, Default) ->
    case File of
        "../" ++ Rest ->
            Parent = filename:dirname(Dir),
            join2(Parent, Rest, Default);
        ".." ->
            filename:dirname(Dir);
        "./" ++ Rest ->
            join2(Dir, Rest, Default);
        "." ->
            Dir;
        _ ->
            Dir++"/"++File
    end.

is_url(File) when is_binary(File) ->
    is_url(?b2l(File));
is_url("") ->
    true;
is_url(File) ->
    Delim = "://",
    case split(File, Delim) of
        {_Prefix, _Rel} ->
            true;
        false ->
            false
    end.

strip_leading_whitespaces(Bin) when is_binary(Bin) ->
    re:replace(Bin, "^[\s\t]+", "", [{return, binary}]).

strip_trailing_whitespaces(Bin) when is_binary(Bin) ->
    re:replace(Bin, "[\s\t]+$", "", [{return, binary}]).

to_string(Atom) when is_atom(Atom) ->
    to_string(?a2l(Atom));
to_string(Bin) when is_binary(Bin) ->
    to_string(?b2l(Bin));
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
        verbose -> ok;
        etrace  -> ok;
        ctrace  -> ok
    end.

tag_prefix(Tag, Width) when is_atom(Tag) ->
    tag_prefix(?a2l(Tag), Width);
tag_prefix(Tag, Width) when is_binary(Tag) ->
    tag_prefix(?b2l(Tag), Width);
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
                directory when Recursive orelse IsTopLevel ->
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
foldl_cmds(Fun, Acc, File, PosStack, Cmds) ->
    foldl_cmds(Fun, Acc, File, PosStack, Cmds, include).

%% Depth :: main         - iterate only over main script file
%%        | include      - do also iterate over include files
%%        | static       - do also iterate over loops and macros
%%        | {dynamic, I} - do also iterate over macros invokations
foldl_cmds(Fun, Acc, File, PosStack, Cmds, Depth) when is_atom(Depth) ->
    foldl_cmds(Fun, Acc, File, PosStack, Cmds, {Depth, undefined});
foldl_cmds(Fun, Acc, File, PosStack, Cmds, {Depth, OptI})
  when Depth =:= main; Depth =:= include; Depth =:= static; Depth =:= dynamic ->
    File2 = drop_prefix(File),
    RevFile = filename_split(File2),
    do_foldl_cmds(Fun, Acc, File2, RevFile, PosStack, Cmds, {Depth, OptI}).

do_foldl_cmds(Fun, Acc, File, RevFile, PosStack, [Cmd | Cmds], FullDepth) ->
    #cmd{type = Type, lineno = LineNo, arg = Arg} = Cmd,
    {Depth, OptI} = FullDepth,
    CmdPos = lux_utils:cmd_pos(File, Cmd),
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
                SubFun(SubFile, SubCmds, [CmdPos | PosStack]);
            macro when Depth =:= static;
                       Depth =:= dynamic ->
                {macro, _Name, _ArgNames, _FirstLineNo, _LastLineNo, Body} =
                    Arg,
                SubFun(File, Body, [CmdPos | PosStack]);
            loop when Depth =:= static;
                      Depth =:= dynamic ->
                {loop, _Name, _ItemStr, _FirstLineNo, _LastLineNo, Body} = Arg,
                LoopPos = #cmd_pos{rev_file = RevFile,
                                   lineno = LineNo,
                                   type = iteration},
                SubStack = [LoopPos, CmdPos | PosStack],
                SubFun(File, Body, SubStack);
            invoke when Depth =:= dynamic ->
                case lux_interpret:lookup_macro(OptI, Cmd) of
                    {ok, _NewCmd, [#macro{file = MacroFile, cmd = MacroCmd}]} ->
                        #cmd{arg = MacroArg} = MacroCmd,
                        {macro, _Name, _ArgNames,
                         _FirstLineNo, _LastLineNo, Body} =
                            MacroArg,
                        SubFun(MacroFile, Body, [CmdPos | PosStack]);
                _NoMatch ->
                        %% Ignore non-existent macro
                        Acc
                end;
            _ ->
                Fun(Cmd, RevFile, PosStack, Acc)
        end,
    do_foldl_cmds(Fun, Acc2, File, RevFile, PosStack, Cmds, FullDepth);
do_foldl_cmds(_Fun, Acc, _File, _RevFile, _PosStack, [], FullDepth) ->
    {_Depth, _OptI} = FullDepth,
    Acc.

full_lineno(File, Cmd, PosStack) ->
    CmdPos = cmd_pos(File, Cmd),
    FullStack = [CmdPos | PosStack],
    pretty_full_lineno(FullStack).

pretty_full_lineno(FullStack) ->
    Pick = fun(#cmd_pos{lineno=L}) when is_integer(L) -> L;
              (L)                  when is_integer(L) -> L
           end,
    FullStack2 = lists:dropwhile(fun(FL) -> Pick(FL) < 0 end, FullStack),
    [FileLine | Incl] = lists:reverse(FullStack2),
    LineNo = Pick(FileLine),
    LineNoSuffix = [[":", ?i2l(Pick(FL))] || FL <- Incl],
    lists:flatten([?i2l(LineNo), LineNoSuffix]).

cmd_pos(File, #cmd{lineno = LineNo, type = Type, arg = Arg}) ->
    RevFile = filename_split(File),
    cmd_pos(RevFile, LineNo, Type, Arg).

cmd_pos(RevFile, LineNo, Type, Name) when is_list(Name) ->
    P = #cmd_pos{rev_file = RevFile, lineno = LineNo, type = Type, name = Name},
    %% io:format("\nNEW POS ~p\n\t~p\n", [P, ?stacktrace()]),
    P;
cmd_pos(RevFile, LineNo, Type, Arg) ->
    case Arg of
        {macro, Name, _ArgNames, _FirstLineNo, _LastLineNo, _Body}
          when Type =:= macro ->
            ok;
        {body, macro, Name, _ArgNames}
          when Type =:= macro ->
            ok;
        {include, Name, _FirstLineNo, _LastLineNo, _InclCmds}
          when Type =:= include ->
            ok;
        _ ->
            Name = ""
    end,
    cmd_pos(RevFile, LineNo, Type, Name).

pretty_stack(OrigFile, FullStack) ->
    Dir = filename:dirname(OrigFile),
    Pretty = fun(#cmd_pos{rev_file = _RevFile, lineno=L}) when L < 0 ->
                     false;
                (#cmd_pos{rev_file = RevFile, lineno=L} = CmdPos) ->
                     RelFile = drop_prefix(Dir, pretty_filename(RevFile)),
                     {true, {RelFile ++ ":" ++ ?i2l(L), CmdPos}}
             end,
    lists:zf(Pretty, FullStack).

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

verbatim_search(Actual, Expected, Orig, Pos) ->
    {Add, Actual2} = verbatim_normalize(Actual),
    {_Sub, Expected2} = verbatim_normalize(Expected),
    verbatim_search2(Actual2, Expected2, Orig, Pos+Add).

verbatim_search2(<<Match:1/binary, Actual/binary>>,
                 <<Match:1/binary, Expected/binary>>,
                 Orig,
                 Pos) ->
    %% First char is matching - try to collect consecutive chars
%io:format("SEARCH MATCH ~p ~p\n", [Match, Pos]),
    verbatim_collect(Actual, Expected, Orig, Pos, Pos+1, 1);
verbatim_search2(<<_A:1/binary, Actual/binary>>,
                 Expected,
                 Orig,
                 Pos) ->
    %% Char does not match - try next char
%io:format("SEARCH NOMATCH ~p ~p\n", [_A, Pos+1]),
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
%io:format("COLLECT MATCH ~p ~p (~p)\n", [Match, Base, Pos+1]),
    verbatim_collect(Actual, Expected, Orig, Base, Pos+1, Len+1);
verbatim_collect2(<<_A:1/binary, Actual/binary>>,
                  <<_E:1/binary, _/binary>>,
                  Orig,
                  _Base,
                  Pos,
                  _Len) ->
    %% No match
%io:format("COLLECT NOMATCH ~p ~p (~p)\n", [_A, _Base, Pos+1]),
    verbatim_search(Actual, Orig, Orig, Pos+1);
verbatim_collect2(_Actual, <<>>, _Orig, Base, _Pos, Len) ->
    %% Match completed
%io:format("COLLECT COMPLETE ~p (~p)\n", [Base, _Pos]),
    {match, [{Base, Len}]};
verbatim_collect2(<<>>, _Expected, _Orig, _Base, _Pos, _Len) ->
    %% No match
    nomatch.

verbatim_normalize(<<"\r\n", Rest/binary>>) ->
    {1, <<"\n", Rest/binary>>};
verbatim_normalize(<<"\r", Rest/binary>>) ->
    {0, <<"\n", Rest/binary>>};
verbatim_normalize(Rest) ->
    {0, Rest}.

expand_lines([] = Line) ->
    Line;
expand_lines([_] = Line) ->
    Line;
expand_lines([Line | Lines]) ->
    [Line, "\n", expand_lines(Lines)].

quote_newlines(IoList) ->
    Replace = fun({From, To}, Acc) ->
                      re:replace(Acc, From, To, [global, {return, binary}])
              end,
    Map = [{<<"\r">>, <<"\\\\r">>},
           {<<"\n">>, <<"\\\\n">>}],
    lists:foldl(Replace, IoList, Map).

split_lines(IoList) ->
    Normalized = normalize_newlines(IoList),
    binary:split(Normalized, <<"\n">>, [global]).

normalize_newlines(Bin) when is_binary(Bin) ->
    replace(Bin, [{crlf, <<"\n">>}]);
normalize_newlines(IoList) ->
    normalize_newlines(?l2b(IoList)).

normalize_match_regexp(Bin) when is_binary(Bin) ->
    replace(Bin, [{crlf, <<"\\R">>}]);
normalize_match_regexp(IoList) ->
    normalize_match_regexp(?l2b(IoList)).

replace(Bin, [Transform|Rest]) when is_binary(Bin) ->
    Bin2 =
        case Transform of
            List when is_list(List) ->
                replace(Bin, List);
            Fun when is_function(Fun, 1) ->
                Fun(Bin);
            crcr ->
                replace(Bin, [{<<"\r+">>, <<"\r">>} | Rest]);
            crlf ->
                replace(Bin, [{crlf, <<"\n">>} | Rest]);
            {crlf, To} ->
                From = [<<"\\R">>, <<"\r\n">>, <<"\r">>, <<"\n">>],
                replace(Bin, [crcr | [{F, To} || F <- From]]);
            quoted_crcr ->
                replace(Bin, [{<<"(\\r)+">>, <<"\\r">>} | Rest]);
            quoted_crlf ->
                replace(Bin, [{quoted_crlf, <<"\\R">>} | Rest]);
            {quoted_crlf, To} ->
                From = [<<"\\\\R">>, <<"\\r\\n">>, <<"\\r">>, <<"\\n">>],
                replace(Bin, [quoted_crcr | [{F, To} || F <- From]]);
            {From, To} ->
                binary:replace(Bin, From, To, [global])
        end,
    replace(Bin2, Rest);
replace(Bin, []) when is_binary(Bin) ->
    Bin.

shrink_lines(Lines) ->
    case Lines of
        [H1, H2, H3 | HT] ->
            case lists:reverse(HT) of
                [T1, T2, T3, _T4, _T5, _T6 | TT] ->
                    Len = ?l2b(?i2l(length(TT)+3)),
                    [H1, H2, H3,
                     <<"... ", Len/binary," common lines not shown...">>,
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
    ExpectTag =
        case Cmd of
            #cmd{type=expect, arg={endshell,_RegexpOper,Expected,_MP}} ->
                ?EXPECTED_RE;
            #cmd{type=expect, arg={verbatim,_RegexpOper,Expected}} ->
                ?EXPECTED_EQ;
            #cmd{type=expect, arg={template,_RegexpOper,Expected}} ->
                ?EXPECTED_EQ;
            #cmd{type=expect, arg={regexp,_RegexpOper,Expected}} ->
                ?EXPECTED_RE;
            #cmd{type=expect, arg={mp,_RegexpOper,Expected,_MP,_Multi}} ->
                ?EXPECTED_RE;
            #cmd{} ->
                Expected = <<"">>,
                ?EXPECTED_RE
        end,
    {ExpectTag, Expected}.

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

diff(ExpectedTag, Old, New) ->
    Equal =
        fun(O, N) ->
                case equal(ExpectedTag, O, N) of
                    match   -> true;
                    nomatch -> false
                end
        end,
    lux_diff:compare(Old, New, Equal).

equal(ExpectedTag, Expected, Expected)
  when ExpectedTag =:= ?EXPECTED_EQ;
       ExpectedTag =:= ?EXPECTED_OLD ->
    match;
equal(ExpectedTag, Expected0, Actual)
  when ExpectedTag =:= ?EXPECTED_RE;
       ExpectedTag =:= ?EXPECTED_OLD ->
    Expected = normalize_diff_regexp(Expected0),
    try
        re:run(Actual, Expected,[{capture, none}, notempty])
    catch _:_ ->
            nomatch
    end;
equal(ExpectedTag, _Expected, _Actual)
  when ExpectedTag =:= ?EXPECTED_EQ ->
    nomatch.

normalize_diff_regexp(<<Prefix:1/binary, _/binary>> = RegExp)
  when Prefix =/= <<"^">> ->
    normalize_diff_regexp(<<"^", RegExp/binary>>);
normalize_diff_regexp(RegExp) when is_binary(RegExp) ->
    Size = byte_size(RegExp)-1,
    case RegExp of
        <<_:Size/binary, "$">> ->
            RegExp;
        _ ->
            normalize_diff_regexp(<<RegExp/binary, "$">>)
    end;
normalize_diff_regexp([Prefix|RegExp])
  when Prefix =/= $^ ->
    normalize_diff_regexp([$^|RegExp]);
normalize_diff_regexp(RegExp) when is_list(RegExp) ->
    case lists:last(RegExp) of
        $\$ ->
            RegExp;
        _ ->
            normalize_diff_regexp(RegExp++"$")
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
-spec diff_iter(expected_tag(), [binary()], [binary()],
                mode(), callback()) -> acc().
-type diff() :: lux_diff:compact_diff().
diff_iter(ExpectedTag, Old, New, Mode, Fun)
  when Mode =:= flat; Mode =:= deep ->
    Diff = diff(ExpectedTag, Old, New),
    diff_iter(ExpectedTag, Diff, Mode, Fun).

-spec diff_iter(expected_tag(), diff(), mode(), callback()) -> acc().
diff_iter(ExpectedTag, Diff, Mode, Fun) ->
    InitialAcc = [],
    diff_iter_loop(ExpectedTag, Diff, Mode, Fun, InitialAcc).

-define(MAX_DIFF, 10000).
diff_iter_loop(ExpectedTag, [H|T], Mode, Fun, Acc) ->
    Context = context(Acc, T),
    case H of
        Common when is_list(Common) ->
            NewAcc = Fun({common,Common}, Mode, Context, Acc),
            diff_iter_loop(ExpectedTag, T, Mode, Fun, NewAcc);
        {'-', Del} when element(1, hd(T)) =:= '+' ->
            Add = element(2, hd(T)),
            diff_iter_loop(ExpectedTag, [{'!',Del,Add} | tl(T)],
                           Mode, Fun, Acc);
        {'-', Del} ->
            NewAcc = Fun({del,Del}, Mode, Context, Acc),
            diff_iter_loop(ExpectedTag, T, Mode, Fun, NewAcc);
        {'+', Add} when element(1, hd(T)) =:= '-' ->
            Del = element(2, hd(T)),
            diff_iter_loop(ExpectedTag, [{'!',Del,Add} | tl(T)],
                           Mode, Fun, Acc);
        {'+', Add} ->
            NewAcc = Fun({add,Add}, Mode, Context, Acc),
            diff_iter_loop(ExpectedTag, T, Mode, Fun, NewAcc);
        {'!', Del, Add} when Mode =:= deep,
                             length(Del) < ?MAX_DIFF,
                             length(Add) < ?MAX_DIFF ->
            DelChars = ?b2l(?l2b(expand_lines(Del))),
            AddChars = ?b2l(?l2b(expand_lines(Add))),
            DefaultMatch = lux_diff:default_match(),
            NestedDiff = lux_diff:compare(DelChars, AddChars, DefaultMatch),
            NestedAcc = diff_iter(?EXPECTED_EQ, NestedDiff, nested, Fun),
            DeepAcc = Fun({nested,Del,Add,NestedAcc}, Mode, Context, Acc),
            diff_iter_loop(ExpectedTag, T, Mode, Fun, DeepAcc);
        {'!', Del, Add} when Mode =:= deep ->
            NewMode = flat,
            NewAcc = Fun({replace,Del,Add}, NewMode, Context, Acc),
            diff_iter_loop(ExpectedTag, T, Mode, Fun, NewAcc);
        {'!', Del, Add} when Mode =:= flat;
                             Mode =:= nested ->
            NewAcc = Fun({replace,Del,Add}, Mode, Context, Acc),
            diff_iter_loop(ExpectedTag, T, Mode, Fun, NewAcc)
    end;
diff_iter_loop(_ExpectedTag, [], _Mode, _Fun, Acc) ->
    Acc.

context(_Acc, []) ->
    last;
context([], _Tail) ->
    first;
context(_Aacc, _Tail) ->
    middle.

shrink_diff(ExpectedTag, Old, New) when is_binary(Old), is_binary(New) ->
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
    Diff = diff(ExpectedTag, split_lines(Old), split_lines(New)),
    ShrinkedDiff = shrink(Diff, []),
    Expanded = lux_diff:split_diff(ShrinkedDiff),
    ?l2b(lists:map(ToIoList, Expanded)).

shrink([Common | T], Acc) when is_list(Common) ->
    Shrinked = shrink_lines(Common),
    shrink(T, [Shrinked | Acc]);
shrink([Other | T], Acc) ->
    shrink(T, [Other | Acc]);
shrink([], Acc) ->
    lists:reverse(Acc).

start_app(App) ->
    case application:start(App) of
        ok ->
            {true, fun() -> application:stop(App) end};
        {error,{already_started,App}} ->
            {true, fun() -> ok end};
        {error,_Reason} ->
            {false, fun() -> ok end}
    end.

stop_app({N, WWW}) when is_integer(N) ->
    stop_app(WWW);
stop_app(WWW) ->
    if
        WWW =:= undefined ->
            ok;
        WWW =:= false ->
            ok;
        is_function(WWW, 0) ->
            %% WWW(), % Get rid of progress printouts
            ok
    end.

user_prefix() ->
    case os:getenv("USER") of
        false -> "";
        ""    -> "";
        User  -> User ++ "@"
    end.

real_hostname() ->
    case inet:gethostname() of
        {ok, Host} -> Host;
        _          -> "localhost"
    end.

make_warning(File, FullLineNo, Reason) ->
    #warning{file = File,
             lineno = FullLineNo,
             reason = ?l2b(Reason)}.

multi_member([H | T], List) ->
    case lists:member(H, List) of
        true  -> {true, H};
        false -> multi_member(T, List)
    end;
multi_member([], _List) ->
    false.

summary_log_candidates() ->
    [
     "lux.skip",
     ?SUITE_SUMMARY_LOG,
     ?SUITE_SUMMARY_LOG ++ ".tmp",
     "qmscript.skip",
     "qmscript_summary.log",
     "qmscript_summary.log.tmp"
    ].
