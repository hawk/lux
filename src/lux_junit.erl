%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_junit).

-export([write_report/3]).

-include("lux.hrl").

write_report(LogFile, RunDir, _Opts) ->
    File = lux_utils:normalize_filename(LogFile),
    case lux_log:parse_summary_log(File) of
        {ok, _Result, Groups, _ConfigSection, _FileInfo, _EventLogs} ->
            Content = testsuites(Groups, RunDir),
            Filename = File ++ ".junit.xml",
            file:write_file(Filename, Content);
        {error, _File, _Reason} = Error ->
            Error
    end.

testsuites(Groups, RunDir) ->
    TestsuiteFun = fun(Group) ->
                           testsuite(Group, RunDir)
                   end,
    ["<?xml version=\"1.0\" encoding=\"utf-8\"?>",
     "<testsuites>",
     lists:flatmap(TestsuiteFun, Groups),
     "</testsuites>"].

testsuite({test_group, _, Cases}, RunDir) ->
    TestcaseFun = fun(Testcase) ->
                          testcase(Testcase, RunDir)
                  end,
    Tests = erlang:integer_to_binary(erlang:length(Cases)),
    ["<testsuite tests=\"", Tests, "\">",
    lists:flatmap(TestcaseFun, Cases),
     "</testsuite>"];
testsuite(_, _) ->
    [].

testcase({result_case, Filename, Reason, _Details}, RunDir) ->
    ["<testcase ",
     "classname=\"", classname(Filename, RunDir), "\" ",
     "name=\"", name(Filename), "\">",
     "<system-out>", Reason, "</system-out>",
     "</testcase>"];
testcase({test_case, Filename, _, _, _, Result}, RunDir) ->
    ["<testcase ",
     "classname=\"", classname(Filename, RunDir), "\" ",
     "name=\"", name(Filename), "\">",
     body(Result),
     "</testcase>"];
testcase(_, _) ->
    [].

classname(Filename, RunDir) ->
    DirName = lux_utils:drop_prefix(RunDir, filename:dirname(Filename)),
    case string:join(filename:split(DirName), ".") of
        "."                  -> "lux";
        [$/, $. | ClassName] -> "lux." ++ ClassName;
        ClassName            -> "lux." ++ ClassName
    end.

name(Filename) ->
    filename:basename(Filename).

body({result, skip}) ->
    ["<skipped/>"];
body({result, {_, LineNo, Expected, Actual, Details}}) ->
    ["<failure type=\"NoMatch\">",
     "<![CDATA[",
     "\nLine number: ", LineNo,
     "\nExpected: ", Expected,
     "\nActual: ", Actual,
     "\nDetails:\n", join(Details, "\n"),
     "]]>",
     "</failure>"];
body({error, Reason}) ->
    ["<error message=\"error\" type=\"general_error\">",
     "<![CDATA[",
     join(Reason, "\n"),
     "]]>",
     "</error>"];
body({error_line, LineNo, Reason}) ->
    ["<error message=\"error\" type=\"general_error\">",
     "<![CDATA[",
     "Error at line ", LineNo,
     join(Reason, "\n"),
     "]]>",
     "</error>"];
body(_) ->
    [].

join([], _) ->
    [];
join([H | T], Separator) ->
    JoinFun = fun(Element) ->
                      [Separator, Element]
              end,
    [H | lists:flatmap(JoinFun, T)].
