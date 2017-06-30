%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_junit).

-export([write_report/2]).

-include("lux.hrl").

write_report(LogFile, _Opts) ->
    File = lux_utils:normalize_filename(LogFile),
    case lux_log:parse_summary_log(File) of
        {ok, _Result, Groups, _ConfigSection, _FileInfo, _EventLogs} ->
            Content = testsuites(Groups),
            Filename = File ++ ".junit.xml",
            file:write_file(Filename, Content);
        {error, _File, _Reason} = Error ->
            Error
    end.

testsuites(Groups) ->
    ["<?xml version=\"1.0\" encoding=\"utf-8\"?>",
     "<testsuites>",
     lists:map(fun testsuite/1, Groups),
     "</testsuites>"].

testsuite({test_group, _, Cases}) ->
    Tests = erlang:integer_to_binary(erlang:length(Cases)),
    ["<testsuite tests=\"", Tests, "\">",
    lists:map(fun testcase/1, Cases),
     "</testsuite>"].

testcase({test_case, Filename, _, _, _, Result}) ->
    ["<testcase ",
     "classname=\"", classname(Filename), "\" ",
     "name=\"", name(Filename), "\">",
     body(Result),
     "</testcase>"].

classname(Filename) ->
    case string:join(filename:split(filename:dirname(Filename)), ".") of
        [$/, $. | ClassName] -> ClassName;
        ClassName            -> ClassName
    end.

name(Filename) ->
    filename:basename(Filename).

body({result, {_, LineNo, Expected, Actual, Details}}) ->
    ["<failure type=\"NoMatch\">",
     "<![CDATA[",
     "\nLine number: ", LineNo,
     "\nExpected: ", Expected,
     "\nActual: ", Actual,
     "\nDetails:\n", join(Details, "\n"),
     "]]>",
     "</failure>"];
body(_) ->
    [].

join([], _) ->
    [];
join([H | T], Separator) ->
    JoinFun = fun(Element) ->
                      [Separator, Element]
              end,
    [H | lists:flatmap(JoinFun, T)].
