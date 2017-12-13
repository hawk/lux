%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_html_utils).

-export([
         keysplit/2,
         split_timers/1,
         safe_write_file/2,
         html_href/2, html_href/3, html_href/4, html_href/5,
         html_anchor/2, html_anchor/4,
         html_quote/1,
         html_header/1,
         html_footer/0
        ]).

-include("lux.hrl").


%% Collect list of tuples and group them according to their tag
%%
%% The internal ordering is kept:
%%
%%   keysplit(1, [{3,3},{3,1},{3,2},{1,1},{1,2},{2,2},{2,1},{1,3}]).
%%   -> [{3,[{3,3},{3,1},{3,2}]},
%%       {1,[{1,1},{1,2},{1,3}]},
%%       {2,[{2,2},{2,1}]}]

keysplit(Pos, List) ->
    do_keysplit(Pos, List, []).

do_keysplit(Pos, [H | T], Acc) ->
    Tag = element(Pos, H),
    case lists:keyfind(Tag, 1, Acc) of
        false ->
            NewAcc = [{Tag, [H]} | Acc],
            do_keysplit(Pos, T, NewAcc);
        {Tag, Old} ->
            NewAcc = lists:keyreplace(Tag, 1, Acc, {Tag, [H|Old]}),
            do_keysplit(Pos, T, NewAcc)
    end;
do_keysplit(_Pos, [], Acc) ->
    lists:reverse([{Tag, lists:reverse(List)} || {Tag, List} <- Acc]).

split_timers(Timers) ->
    ShellTimers = strip_timers(Timers, []),
    ShellSplit = do_split_timers(#timer.shell, ShellTimers),
    MacroTimers = expand_macros(ShellTimers),
    MacroSplit = do_split_timers(#timer.macro, MacroTimers),
    {MacroSplit, ShellSplit}.

do_split_timers(Pos, Timers) ->
    SplitTimers = keysplit(Pos, Timers),
    Calc = fun({Tag, List}) ->
                   Sum =
                       lists:sum([opt_time(T#timer.elapsed_time) || T <- List]),
                   {Tag, Sum, List}
           end,
    SplitSums = lists:reverse(lists:keysort(2, lists:map(Calc, SplitTimers))),
    Total = lists:sum([Sum || {_Tag, Sum, _List} <- SplitSums]),
    {Total, SplitSums}.

opt_time(Time) ->
    case Time of
        undefined -> 0;
        _         -> Time
    end.

%% Strip startup of shells and failures
strip_timers([H | T], Acc) ->
    Skip =
        H#timer.send_lineno =:= H#timer.match_lineno andalso
        not lists:keymember(H#timer.shell, #timer.shell, Acc) andalso
        H#timer.status =:= matched,
    Acc2 =
        case Skip of
            true  -> Acc;
            false -> [H | Acc]
        end,
    strip_timers(T, Acc2);
strip_timers([], Acc) ->
    lists:reverse(Acc).

expand_macros(Timers) ->
    Expand = fun(#timer{callstack=C} = T) ->
                    Macros = binary:split(C, <<"->">>, [global]),
                    [T#timer{macro = M} || M <- Macros]
             end,
    lists:flatmap(Expand, Timers).

safe_write_file(File, IoList) when is_binary(File) ->
    safe_write_file(?b2l(File), IoList);
safe_write_file(File, IoList) when is_list(File) ->
    Res = filelib:ensure_dir(File),
    if
        Res =:= ok; Res =:= {error, eexist} ->
            TmpFile = File ++ ".tmp",
            case file:write_file(TmpFile, IoList, [raw]) of
                ok ->
                    case file:rename(TmpFile, File) of
                        ok ->
                            {ok, File};
                        {error, FileReason} ->
                            {error, File, file:format_error(FileReason)}
                    end;
                {error, FileReason} ->
                    {error, TmpFile, file:format_error(FileReason)}
            end;
        true ->
            {error, FileReason} = Res,
            {error, filename:dirname(File), file:format_error(FileReason)}
    end.

html_href("a", "", Protocol, Name, Label) ->
    ["\n",html_href(Protocol, Name, Label)];
html_href(Tag, Prefix, Protocol, Name, Label) when Tag =/= "" ->
    [
     "\n<", Tag, ">",
     Prefix, html_href(Protocol, Name, Label),
     "</", Tag, ">\n"
    ].

html_href(Protocol, Name, Label, "") when Protocol =:= "" -> % Temporary assert
    %% AbsHref = ?b2l(?l2b([Protocol, Name])),
    %% case chop_root(AbsHref) of
    %%     AbsHref ->
    %%         io:format("ABS HREF ~p\n\t~p\n", [AbsHref, ?stacktrace()]);
    %%     _RelHref ->
    %%         ok
    %% end,
    [
     "<a href=\"", Protocol, html_quote(Name), "\">", Label, "</a>"
    ];
html_href(Protocol, Name, Label, Type) when Protocol =:= "" ->
    [
     "<a href=\"", Protocol, html_quote(Name), "\""
     " type=\"", Type, "\">", Label, "</a>"
    ].

html_href(Name, Label) ->
    html_href("", Name, Label).

html_href(Protocol, Name, Label) ->
    html_href(Protocol, Name, Label, "").

html_anchor(Tag, Prefix, Name, Label) ->
    [
     "<", Tag, ">", Prefix, html_anchor(Name, Label), "</", Tag, ">"
    ].

html_anchor(Name, Label) ->
    [
     "<a name=\"", Name, "\">", html_quote(Label), "</a>"
    ].

html_quote(IoList) ->
    lux_utils:replace(?l2b(IoList),
                      [
                       fun(B) -> ?l2b(lists:map(fun safe_ctrl/1, ?b2l(B))) end,
                       fun(B) -> safe_latin1(B, []) end,
                       {<<"&">>, <<"&amp;">>},
                       {<<"<">>, <<"&lt;">>},
                       {<<">">>, <<"&gt;">>},
                       {<<"\"">>, <<"&quot;">>}
                      ]).

safe_ctrl(Char) ->
    if
        Char < 32, Char =/= $\t, Char =/= $\n, Char =/= $\r ->
            ["<ctrl char \\", ?i2l(Char), " - see plain lux log for details>"];
        true ->
            Char
    end.

safe_latin1(<<>>, Acc) ->
    ?l2b(lists:reverse(Acc));
safe_latin1(Bin, Acc) ->
    case unicode:characters_to_binary(Bin, utf8, latin1) of
        {error, Good, _Rest} ->
            BadSz = ?i2l(byte_size(Bin) - byte_size(Good)),
            Reason = ["<illegal char(s) - skipped ", BadSz,
                      " bytes - see plain lux log for details>"],
            safe_latin1(<<>>, [Reason, Good | Acc]);
        {incomplete, Good, _Rest} ->
            BadSz = ?i2l(byte_size(Bin) - byte_size(Good)),
            Reason = ["<incomplete char - skipped ", BadSz,
                      " bytes - see plain lux log for details>"],
            safe_latin1(<<>>, [Reason, Good | Acc]);
        Good ->
            safe_latin1(<<>>, [Good | Acc])
    end.

html_header(Title) ->
    [
     <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" "
       "\"http://www.w3.org/TR/html4/strict.dtd\">\n">>,
     <<"<html>\n">>,
     <<"<head>\n">>,
     html_style(),
     <<"<title>">>, Title, <<"</title>\n">>,
     <<"</head>\n\n">>,
     <<"<body>\n">>
    ].

html_footer() ->
    <<"\n</body>\n</html>\n">>.

html_style() ->
<<"
<style>
  body {
        color: #000000;
        background-color: white
  }

  div {
        overflow: auto;
        padding: 2px;
        border: 1px solid #b00;
        margin-left: 2%;
        margin-bottom: 2px;
        margin-top: 2px;
        color: #000000;
        background-color: #FFFFE0
  }

  div.event {
        font-weight: normal;
  }

  div.result {
  }

  div.config {
  }

  div.code {
        font-weight: bold;
        overflow: visible;
        padding: 0px;
        border: 0px;
        margin-left: 0%;
        margin-bottom: 0px;
        margin-top: 0px;
        color: #000000;
        background-color: white
  }

  div.send {
        background-color: #FFEC8B;
  }

  div.recv {
        background-color: #E9967A;
  }

  div.match {
        background-color: #FFFFE0
  }

  div.expect {
        background-color: #FFFFE0
  }

  div.case {
        background-color: #D3D3D3
  }

  td.fail {
        background-color: #CC3333
  }

  td.secondary_fail {
        background-color: #F26C4F
  }

  td.skip {
        background-color: #FFFFE0
  }

  td.warning {
        background-color: #FFF380
  }

  td.none {
        background-color: #80FF80
  }

  td.success {
        background-color: #00A651
  }

  td.no_data {
        background-color: #FFFFFF
  }
  </style>

">>.
