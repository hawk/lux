%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2020 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_html_parse).

-export([format_results/2, validate_html/2, parse_files/3]).

-include_lib("lux.hrl").
-include_lib("xmerl/include/xmerl.hrl").

validate_html(HtmlFile, Opts) when is_list(HtmlFile) ->
    case lists:keyfind(html, 1, Opts) of
        {html, validate} ->
            io:format("\nValidate ~p...\n", [HtmlFile]),
            {ValRes, NewWWW} = do_validate_html(HtmlFile, undefined),
            lux_utils:stop_app(NewWWW),
            ValRes;
        _Other ->
            ok
    end.

do_validate_html(HtmlFile, WWW) ->
    {ValRes, NewWWW} = validate_file(HtmlFile, WWW),
    case format_results(ValRes, NewWWW) of
        [] ->
            ok;
        Reasons ->
            io:format("\n", []),
            [io:format("~s\n", [R]) || R <- Reasons]
    end,
    Errors = [VR || VR <- ValRes,
                    element(1, VR) =/= ok,
                    element(1, VR) =/= link_warning],
    Res =
        case Errors of
            [] -> ok;
            _  -> {error, HtmlFile, "Not valid HTML"}
        end,
    {Res, NewWWW}.

-type val_res() ::
        {ok, File::file:filename(), html | no_html} |
        {link_warning, File::file:filename(),
         Line::non_neg_integer(), Col::non_neg_integer(),
         Reason::string()} |
        {link_error, File::file:filename(),
         Line::non_neg_integer(), Col::non_neg_integer(),
         Reason::string()} |
        {syntax_error, File::file:filename(),
         Line::non_neg_integer(), Col::non_neg_integer(),
         Reason::string()}.

validate_file(File, WWW) ->
    {Res, NewWWW} = parse_files(deep, File, WWW),
    %% io:format("\nLINKS: ~s\n\t~p\n", [File, Res]),
    Enoent = file:format_error(enoent),
    Fun = fun(E, A) -> validate_links(E, Res, Enoent, A) end,
    {lists:reverse(lists:foldl(Fun, [], Res)), NewWWW}.

validate_links({ok, Abs, Refs, Type}, Orig, EnoEnt, Acc) ->
    Fun = fun(E, A) -> do_validate_links(E, Abs, Orig, EnoEnt, A) end,
    case lists:foldl(Fun, [], Refs) of
        []  -> [{ok, Abs, Type} | Acc];
        New -> New ++ Acc
    end;
validate_links({error, Abs, Line, Col, Reason}, _Orig, _EnoEnt, Acc) ->
    [{syntax_error, Abs, Line, Col, Reason} | Acc].

do_validate_links({link, External, Internal}, Abs, Orig, EnoEnt, Acc) ->
    {Type, AbsExternal} = link_type({target, Abs, External}),
    case Type of
        remote ->
            Reason = External,
            [{link_warning, Abs, 0, 0, Reason} | Acc];
        local ->
            case lists:keyfind(AbsExternal, 2, Orig) of
                {ok, _File, _Refs, _Type} when hd(External) =:= $/,
                                               Internal =:= "" ->
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
            end
    end;
do_validate_links({anchor, _Name}, _Abs, _Orig, _EnoEnt, Acc) ->
    Acc.

parse_files(Mode, Rel, WWW) ->
    do_parse_files(Mode, [{top, Rel}], WWW, []).

do_parse_files(Mode, [Rel|Rest], WWW, Acc) ->
    {_LinkType, Abs} = link_type(Rel),
    case lists:keymember(Abs, 2, Acc) of
        true ->
            %% Already parsed
            do_parse_files(Mode, Rest, WWW, Acc);
        false ->
            {ParseRes, NewWWW} = parse_file(Abs, lux_utils:is_url(Abs), WWW),
            case ParseRes of
                {ok, Simple, Type} ->
                    Refs = to_links(Simple),
                    NewAcc = [{ok, Abs, Refs, Type} | Acc],
                    Next =
                        case Mode of
                            deep    ->
                                More = [{target, Abs, E} ||
                                           {link, E, _I} <- Refs],
                                Rest++More;
                            shallow ->
                                Rest
                        end,
                    do_parse_files(Mode, Next, NewWWW, NewAcc);
                {error, Line, Col, Reason} ->
                    NewAcc = [{error, Abs, Line, Col, Reason} | Acc],
                    do_parse_files(Mode, Rest, NewWWW, NewAcc)
            end
    end;
do_parse_files(_Mode, [], WWW, Acc) ->
    {lists:reverse(Acc), WWW}.

link_type({target, Source, ""}) when is_list(Source) ->
    {local, Source};
link_type({target, Source, Target}) when is_list(Source), is_list(Target) ->
    IsUrl = lux_utils:is_url(Target),
    if
        IsUrl ->
            {remote, Target};
        true ->
            Dir = filename:dirname(Source),
            {local, lux_utils:join(Dir, Target)}
    end;
link_type({top, Source}) ->
    Type =
        case lux_utils:is_url(Source) of
            true ->  remote;
            false -> local
        end,
    {Type, lux_utils:normalize_filename(Source)}.

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

parse_file(URL, false, WWW) ->
    {do_parse_file(URL), WWW};
parse_file(URL, true, false = WWW) ->
    {do_parse_file(URL), WWW};
parse_file(URL, true = IsUrl, undefined) ->
    case lux_utils:start_app(inets) of
        {true, StopFun} ->
            parse_file(URL, IsUrl, StopFun);
        {false, _StopFun} ->
            parse_file(URL, IsUrl, false)
    end;
parse_file(URL, true, StopFun = WWW) when is_function(StopFun, 0) ->
    io:format(":", []),
    Res =
        case httpc:request(URL) of
            {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
                case lists:suffix(".html", URL) of
                    true ->
                        ParseFun =
                            fun(Opts) -> xmerl_scan:string(Body, Opts) end,
                        parse_html(ParseFun);
                    false ->
                        {ok, [], no_html}
                end;
            {ok, {{_Version, _Code, ReasonPhrase}, _Headers, _Body}} ->
                {error, 0, 0, ReasonPhrase};
            {error, Reason} ->
                String = lists:flatten(?FF("~p", [Reason])),
                {error, 0, 0, String}
        end,
    {Res, WWW}.

do_parse_file(File) ->
    case lists:suffix(".html", File) of
        true ->
            ParseFun = fun(Opts) -> xmerl_scan:file(File, Opts) end,
            parse_html(ParseFun);
        false ->
            case filelib:is_file(File) of
                false ->
                    Enoent = file:format_error(enoent),
                    {error, 0, 0, Enoent};
                true ->
                    {ok, [], no_html}
            end
    end.

parse_html(ParseFun) ->
    try
        {Internal, _Rest} = ParseFun([{quiet,true}]),
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
            IoList = ?FF("~p", [Reason]),
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

-spec format_results(Results::[val_res()], _WWW) -> [string()].

format_results(Results, WWW) ->
    {ok, Cwd} = file:get_cwd(),
    format_results(Results, WWW, Cwd, []).

format_results([H|T], WWW, Cwd, Acc) ->
    Acc2 =
        case lists:flatten(format_result(H, WWW, Cwd)) of
            []     -> Acc;
            Reason -> [Reason|Acc]
        end,
    format_results(T, WWW, Cwd, Acc2);
format_results([], _WWW, _Cwd, Acc) ->
    lists:reverse(Acc).

format_result(Res, WWW, Cwd) ->
    case Res of
        {ok, _AbsFile, html} ->
            %% RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
            %% ["HTML OK:    ", RelFile];
            [];
        {ok, _AbsFile, no_html} ->
            %% RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
            %% ["EXISTS:     ", RelFile];
            [];
        {ok, _AbsFile, _Links, html} ->
            %% RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
            %% ["HTML OK:    ", RelFile];
            [];
        {link_warning, _AbsFile, _Line, _Col, _Reason}
          when is_function(WWW,0) ->
            %% Ignore link warning when we have www access
            [];
        {link_warning, AbsFile, Line, Col, Reason}
        when WWW =:= undefined; WWW =:= false ->
            RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
            Reason2 = "Remote link: " ++ Reason,
            ["HTML LUX WARNING: ", RelFile, opt_pos(Line, Col), Reason2];
        {link_error, AbsFile, Line, Col, Reason} ->
            RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
            Reason2 = "Bad link: " ++ Reason,
            ["HTML LUX ERROR: ", RelFile, opt_pos(Line, Col), Reason2];
        {syntax_error, AbsFile, Line, Col, Reason} ->
            RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
            ["HTML LUX ERROR: ", RelFile, opt_pos(Line, Col), Reason];
        {error, AbsFile, Line, Col, Reason} ->
            RelFile = lux_utils:drop_prefix(Cwd, AbsFile),
            ["HTML LUX ERROR: ", RelFile, opt_pos(Line, Col), Reason]
    end.

opt_pos(0, _Col) ->
    ": ";
opt_pos(Line, 0) ->
    [":", ?i2l(Line), ": "];
opt_pos(Line, Col) ->
    [":", ?i2l(Line), ": column ", ?i2l(Col), ": "].
