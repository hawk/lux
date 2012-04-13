%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2012 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_utils).

-export([builtin_dict/0, system_dict/0, expand_vars/3,
         summary/2, summary_prio/1,
         multiply/2, drop_prefix/1, drop_prefix/2,
         strip_leading_whitespaces/1, strip_trailing_whitespaces/1,
         to_string/1, safe_format/5, safe_write/4, tag_prefix/1,
         progress_write/2, fold_files/5, foldl_cmds/5,
         full_lineno/1, filename_split/1, dequote/1,
         now_to_string/1, datetime_to_string/1]).

-include("lux.hrl").

builtin_dict() ->
    [
     "_CTRL_C_=" ++ [3],  % end of text (etx)
     "_CTRL_D_=" ++ [4],  % end of transmission (eot)
     "_BS_="     ++ [8],  % backspace
     "_TAB_="    ++ [9],  % tab
     "_LF_="     ++ [10], % line feed
     "_CR_="     ++ [13], % carriage return
     "_CTRL_Z_=" ++ [26], % substitute (sub)
     "_DEL_="    ++ [127] % delete
    ].

system_dict() ->
    os:getenv().

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
            do_expand_vars(Dicts, {variable, [H | RevName]}, T, Acc, MissingVar);
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
        enable   -> 0;
        no_data  -> 1;
        success  -> 2;
        skip     -> 3;
        warning  -> 4;
        fail     -> 5;
        error    -> 6;
        disable  -> 999
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

safe_format(Progress, LogFun, Fd, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    safe_write(Progress, LogFun, Fd, IoList).

safe_write(Progress, LogFun, Fd, IoList) when is_list(IoList) ->
    safe_write(Progress, LogFun, Fd, list_to_binary(IoList));
safe_write(Progress, LogFun, Fd0, Bin) when is_binary(Bin) ->
    case Fd0 of
        undefined  ->
            Fd = Fd0,
            Verbose = false;
        {Verbose, Fd} ->
            ok
    end,
    case Progress of
        silent ->
            ok;
        brief ->
            ok;
        doc ->
            ok;
        compact when Verbose ->
            try
                io:format("~s", [binary_to_list(Bin)])
            catch
                _:CReason ->
                    exit({safe_write, verbose, Bin, CReason})
            end;
        compact ->
            ok;
        verbose when Verbose ->
            try
                io:format("~s", [dequote(binary_to_list(Bin))])
            catch
                _:VReason ->
                    exit({safe_write, verbose, Bin, VReason})
            end;
        verbose ->
            ok
    end,
    case Fd of
        undefined ->
            try
                case LogFun(Bin) of
                    <<_/binary>> ->
                        ok;
                    BadRes ->
                        exit({safe_write, log_fun, Bin, BadRes})
                end
            catch
                _:LReason ->
                    exit({safe_write, log_fun, Bin, LReason})
            end;
        _ ->
            try file:write(Fd, Bin) of
                ok ->
                    ok;
                {error, FReason} ->
                    Str = file:format_error(FReason),
                    io:format("\nfile write failed: ~s\n", [Str]),
                    exit({safe_write, file, Fd, Bin, {error, FReason}})
            catch
                _:WReason ->
                    exit({safe_write, file, Bin, WReason})
            end
    end,
    Bin.

dequote(" expect " ++ _ = L) ->
    L;
dequote([$\"|T]) ->
    [$\"|dequote1(T)];
dequote([H|T]) ->
    [H|dequote(T)];
dequote([]) ->
    [].

dequote1([$\\,$\\|T]) ->
    [$\\|dequote1(T)];
dequote1([$\\,$r|T]) ->
    dequote1(T);
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

tag_prefix(Tag) when is_atom(Tag) ->
    tag_prefix(atom_to_list(Tag));
tag_prefix(Tag) ->
    string:left(Tag, 18) ++ ": ".

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
                    case re:run(File, RegExp, [{capture,none}]) of
                        match  ->
                            Fun(File, Acc);
                        nomatch ->
                            Acc
                    end
            end;
        {error, _Reason} ->
            Acc
    end.

foldl_cmds(Fun, Acc, File, InclStack, Cmds) ->
    File2 = lux_utils:drop_prefix(File),
    RevFile = lux_utils:filename_split(File2),
    do_foldl_cmds(Fun, Acc, RevFile, InclStack, Cmds).

do_foldl_cmds(Fun, Acc, RevFile, InclStack, [Cmd | Cmds]) ->
    Acc2 =
        case Cmd of
            #cmd{type = include,
                 lineno = LineNo,
                 arg = {include, SubFile, _FirstLineNo,
                        _LastFileNo, SubCmds}} ->
                SubAcc = Fun(Cmd, RevFile, InclStack, Acc),
                foldl_cmds(Fun,
                           SubAcc,
                           SubFile,
                           [{RevFile, LineNo} | InclStack],
                           SubCmds);
            #cmd{} ->
                Fun(Cmd, RevFile, InclStack, Acc)
        end,
    do_foldl_cmds(Fun, Acc2, RevFile, InclStack, Cmds);
do_foldl_cmds(_Fun, Acc, _RevFile, _InclStack, []) ->
    Acc.

full_lineno([{_FileComps, LineNo} | InclStack]) ->
    LineNoPrefix = [[integer_to_list(No), ":"] ||
                       {_F, No} <- lists:reverse(InclStack)],
    lists:flatten([LineNoPrefix, integer_to_list(LineNo)]).

filename_split(FileName) ->
    FileName2 = drop_prefix(FileName),
    lists:reverse(filename:split(FileName2)).

now_to_string(Now) ->
    DateTime = calendar:now_to_local_time(Now),
    datetime_to_string(DateTime).

datetime_to_string({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    lists:concat([Year, "-", p(Month), "-", p(Day), " ",
                  p(Hour), ":", p(Min), ":", p(Sec)]).

p(Int) when Int >= 0, Int < 10 -> [$0 | integer_to_list(Int)];
p(Int) when Int < 100 -> integer_to_list(Int).
