%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2016 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_html_parse).

-export([validate_file/1, parse_file/1, format_results/1,
         validate_html/1, validate_html/2]).

-include_lib("xmerl/include/xmerl.hrl").

validate_html(HtmlFile, Opts) ->
    case lists:keyfind(html, 1, Opts) of
        {html, validate} ->
            validate_html(HtmlFile);
        _Other ->
            ok
    end.

validate_html(HtmlFile) ->
    ValRes = validate_file(HtmlFile),
    case format_results(ValRes) of
        [] ->
            ok;
        Reasons ->
            io:format("\n", []),
            [io:format("~s\n", [R]) || R <- Reasons]
    end,
    Errors = [VR || VR <- ValRes, element(1, VR) =/= ok],
    case Errors of
        [] -> ok;
        _  -> {error, HtmlFile, "Not valid HTML"}
    end.

-type val_res() ::
        {ok, File::file:filename(), html | no_html} |
        {link_error, File::file:filename(),
         Line::non_neg_integer(), Col::non_neg_integer(),
         Reason::string()} |
        {syntax_error, File::file:filename(),
         Line::non_neg_integer(), Col::non_neg_integer(),
         Reason::string()}.

-spec validate_file(File::file:filename()) -> [val_res()].

validate_file(File) ->
    Res = deep_parse_files([File], []),
    %% io:format("\nLINKS: ~s\n\t~p\n", [File, Res]),
    Enoent = file:format_error(enoent),
    Fun = fun(E, A) -> validate_links(E, Res, Enoent, A) end,
    lists:reverse(lists:foldl(Fun, [], Res)).

validate_links({ok, Abs, Refs, Type}, Orig, EnoEnt, Acc) ->
    Fun = fun(E, A) -> do_validate_links(E, Abs, Orig, EnoEnt, A) end,
    case lists:foldl(Fun, [], Refs) of
        []  -> [{ok, Abs, Type} | Acc];
        New -> New ++ Acc
    end;
validate_links({error, Abs, Line, Col, Reason}, _Orig, _EnoEnt, Acc) ->
    [{syntax_error, Abs, Line, Col, Reason} | Acc].

do_validate_links({link, External, Internal}, Abs, Orig, EnoEnt, Acc) ->
    AbsDir = filename:dirname(Abs),
    AbsExternal =
        case External of
            "" -> Abs;
            _  -> filename:join([AbsDir, External])
        end,
    case lists:keyfind(AbsExternal, 2, Orig) of
        {ok, _File, _Refs, _Type} when hd(External) =:= $/, Internal =:= "" ->
            Reason = External,
            [{link_error, Abs, 0, 0, Reason} | Acc];
        {ok, _File, _Refs, _Type} when hd(External) =:= $/ ->
            Reason = External ++ "#" ++ Internal,
            [{link_error, Abs, 0, 0, Reason} | Acc];
        {ok, _File, _Refs, _Type} when Internal =:= "" ->
            Acc;
        {ok, _File, Refs, _Type} ->
            case [A || {anchor, A} <- Refs, A =:= Internal] of
                [] ->
                    Reason = External ++ "#" ++ Internal,
                    [{link_error, Abs, 0, 0, Reason} | Acc];
                _Anchors ->
                    Acc
            end;
        {error, _File, _Line, _Col, Reason} when Internal =:= "",
                                                 Reason =/= EnoEnt ->
            Acc;
        {error, _File, _Line, _Col, _Reason} when Internal =:= "" ->
            Reason = External,
            [{link_error, Abs, 0, 0, Reason} | Acc];
        {error, _File, _Line, _Col, _Reason} ->
            Reason = External ++ "#" ++ Internal,
            [{link_error, Abs, 0, 0, Reason} | Acc]
    end;
do_validate_links({anchor, _Name}, _Abs, _Orig, _EnoEnt, Acc) ->
    Acc.

deep_parse_files([Rel|Rest], Acc) ->
    Abs = filename:absname(Rel),
    case lists:keymember(Abs, 2, Acc) of
        true ->
            %% Already parsed
            deep_parse_files(Rest, Acc);
        false ->
            case parse_file(Abs) of
                {ok, Simple, Type} ->
                    Refs = to_links(Simple),
                    Dir = filename:dirname(Abs),
                    AbsName =
                        fun(F) ->
                                case F of
                                    "" -> Abs;
                                    _  -> filename:absname(F, Dir)
                                end
                        end,
                    More = [AbsName(E) || {link, E, _I} <- Refs],
                    deep_parse_files(Rest++More, [{ok, Abs, Refs, Type} | Acc]);
                {error, Line, Col, Reason} ->
                    Acc2 = [{error, Abs, Line, Col, Reason} | Acc],
                    deep_parse_files(Rest, Acc2)
            end
    end;
deep_parse_files([], Acc) ->
    lists:reverse(Acc).

to_links(Simple) ->
    Fun = fun({href, Name}, _Parents, Acc) ->
                  Pred = fun(Char) -> Char =/= $# end,
                  case lists:splitwith(Pred, Name) of
                      {External, [$# | Internal]} -> ok;
                      {External, Internal}        -> ok
                  end,
                  [{link, External, Internal} | Acc];
             ({name, Name}, _Parents, Acc) ->
                  [{anchor, Name} | Acc];
             (_Other, _Parents, Acc) ->
                  Acc
          end,
    iterate(Fun, Simple, []).

parse_file(File) ->
    case lists:suffix(".html", File) of
        false ->
            case filelib:is_file(File) of
                false ->
                    Enoent = file:format_error(enoent),
                    {error, 0, 0, Enoent};
                true ->
                    {ok, [], no_html}
            end;
        true ->
            parse_html_file(File)
    end.

parse_html_file(File) ->
    try
        {Internal, _Rest} = xmerl_scan:file(File, [{quiet,true}]),
        %% io:format("\nInternal: ~p\n", [Internal]),
        case Internal of
            error ->
                Enoent = file:format_error(enoent),
                {error, 0, 0, Enoent};
            _ ->
                Simple = to_simple(Internal),
                %% io:format("\nSimple: ~p\n", [Simple]),
                {ok, Simple, html}
        end
    catch
        exit:{fatal, {Reason, {file,_BadFile}, {line,Line}, {col,Col}}} ->
            IoList = io_lib:format("~p", [Reason]),
            {error, Line, Col, lists:flatten(IoList)}
    end.

to_simple(List) when is_list(List) ->
    Simple = fun(Elem, Acc) ->
                     case to_simple(Elem) of
                         []       -> Acc;
                         Stripped -> [Stripped | Acc]
                     end
             end,
    lists:reverse(lists:foldl(Simple, [], List));
to_simple(#xmlElement{name       = Tag,
                      attributes = Attrs,
                      content    = Content}) ->
    {Tag,
     lists:flatten(to_simple(Attrs)),
     lists:flatten(to_simple(Content))};
to_simple(#xmlAttribute{name  = Tag,
                        value = Value}) ->
    {Tag, Value};
to_simple(#xmlText{value = Text}) ->
    case strip(Text) of
        ""       -> [];
        Stripped -> {text, Stripped}
    end;
to_simple(#xmlComment{value = _Text}) ->
    "".

%% Strips off leading and trailing white spaces
strip([]) ->
    [];
strip([Char | Text]) when Char==$\s; Char==$\n; Char==$\t ->
    strip(Text);
strip(Text) ->
    strip(Text,[],[]).
strip([Char | Text], TAcc, SAcc) when Char==$\s; Char==$\n; Char==$\t ->
    strip(Text, TAcc, [Char | SAcc]);
strip([Char |Text], TAcc, SAcc) ->
    strip(Text, [Char | SAcc ++ TAcc], []);
strip([], TAcc, _SAcc) ->
    lists:reverse(TAcc).

%% Deep iteration of the simple form
iterate(Fun, Simple, Acc) ->
    iterate2(Fun, Simple, [],Acc).

iterate2(Fun, [H | T], Parents, Acc) ->
    NewAcc = iterate2(Fun, H, Parents, Acc),
    iterate2(Fun, T, Parents, NewAcc);
iterate2(_Fun, [], _Parents, Acc) ->
    Acc;
iterate2(Fun, {Tag, Attrs, Content}, Parents, Acc) ->
    Self = [Tag | Parents],
    NewAcc = iterate2(Fun, Attrs, Self, Acc),
    iterate2(Fun, Content, Self, NewAcc);
iterate2(Fun, {_Tag, _Value} = Attr, Parents, Acc) ->
    Fun(Attr, Parents, Acc).

-spec format_results(Results::[val_res()]) -> [string()].

format_results(Results) ->
    {ok, Cwd} = file:get_cwd(),
    format_results(Results, Cwd, []).

format_results([H|T], Cwd, Acc) ->
    Acc2 =
        case lists:flatten(format_result(H, Cwd)) of
            []     -> Acc;
            Reason -> [Reason|Acc]
        end,
    format_results(T, Cwd, Acc2);
format_results([], _Cwd, Acc) ->
    lists:reverse(Acc).

format_result(Res, Cwd) ->
    case Res of
        {ok, _AbsFile, html} ->
            %% RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
            %% ["HTML OK:    ", RelFile];
            [];
        {ok, _AbsFile, no_html} ->
            %% RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
            %% ["EXISTS:     ", RelFile];
            [];
        {link_error, AbsFile, Line, Col, Reason} ->
            RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
            Reason2 = "Bad link: " ++ Reason,
            ["HTML LUX ERROR: ", RelFile, opt_pos(Line, Col), Reason2];
        {syntax_error, AbsFile, Line, Col, Reason} ->
            RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
            ["HTML LUX ERROR: ", RelFile, opt_pos(Line, Col), Reason]
    end.

opt_pos(0, _Col) ->
    ": ";
opt_pos(Line, 0) ->
    [":", integer_to_list(Line), ": "];
opt_pos(Line, Col) ->
    [":", integer_to_list(Line), ": column ", integer_to_list(Col), ": "].
