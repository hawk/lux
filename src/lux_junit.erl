%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2024 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_junit).

-export([write_report/3]).

-include("lux.hrl").

-define(INDENT, "    ").

write_report(SummaryLog, RunDir, _Opts) ->
    File = lux_utils:normalize_filename(SummaryLog),
    WWW = undefined,
    {ParseRes, NewWWW} = lux_log:parse_summary_log(File, WWW),
    Res =
        case ParseRes of
            {ok, _Result, Cases, _ConfigSection, _FileInfo, _EventLogs} ->
                Content = testsuites(Cases, RunDir, ""),
                Dir = filename:dirname(File),
                JunitFile = filename:join([Dir, "lux_junit.xml"]),
                file:write_file(JunitFile, Content);
            {error, _File, _Reason} = Error ->
            Error
        end,
    lux_utils:stop_app(NewWWW),
    Res.

testsuites(Cases, RunDir, Indent) ->
    [
     Indent, "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
     "\n",
     Indent, "<testsuites>\n",
     testsuite(Cases, RunDir, [Indent, ?INDENT]),
     Indent, "</testsuites>\n"
    ].

testsuite([], _RunDir, _Indent) ->
    [];
testsuite(Cases, RunDir, Indent) ->
    TestcaseFun = fun(Testcase) ->
                          testcase(Testcase, RunDir, [Indent, ?INDENT])
                  end,
    Tests = ?i2b(length(Cases)),
    [
     Indent, "<testsuite tests=\"", Tests, "\">\n",
     lists:flatmap(TestcaseFun, Cases),
     Indent, "</testsuite>\n"
    ].

testcase({result_case, Filename, Reason, _Details}, RunDir, Indent) ->
    [
     Indent, "<testcase ",
     "classname=\"", classname(Filename, RunDir), "\" ",
     "name=\"", name(Filename), "\">\n",
     Indent, ?INDENT, "<system-out>", Reason, "</system-out>\n",
     Indent, "</testcase>\n"
    ];
testcase({test_case, Filename, _, _, _, Result}, RunDir, Indent) ->
    [
     Indent, "<testcase ",
     "classname=\"", classname(Filename, RunDir), "\" ",
     "name=\"", name(Filename), "\">\n",
     body(Result, [Indent, ?INDENT]),
     Indent, "</testcase>\n"
    ].

classname(Filename, RunDir) ->
    DirName = lux_utils:drop_prefix(RunDir, filename:dirname(Filename)),
    case string:join(filename:split(DirName), ".") of
        "."                  -> "lux";
        [$/, $. | ClassName] -> "lux." ++ ClassName;
        ClassName            -> "lux." ++ ClassName
    end.

name(Filename) ->
    filename:basename(Filename).

body({warnings_and_result, _Warnings, skip}, Indent) ->
    [Indent, "<skipped/>\n"];
body({warnings_and_result,
      _Warnings,
      {_Res, LineNo, _Shell, _ExpectedTag, Expected, Actual, Details}},
     _Indent) ->
    [
     "<failure type=\"NoMatch\">\n",
     "<![CDATA[",
     "\nLine number: ", LineNo,
     "\nExpected: ", Expected,
     "\nActual: ", Actual,
     "\nDetails:\n", join(Details, "\n"),
     "]]>", "</failure>\n"
    ];
body({warnings_and_result, _Warnings, Res}, Indent) ->
    [Indent, "<!-- ", ?a2l(Res), " -->\n"];
body({error, Reason}, _Indent) ->
    [
     "<error message=\"error\" type=\"general_error\">\n",
     "<![CDATA[",
     join(Reason, "\n"),
     "]]>", "</error>\n"];
body({error_line, LineNo, Reason}, _Indent) ->
    [
     "<error message=\"error\" type=\"general_error\">\n",
     "<![CDATA[",
     "Error at line ", LineNo,
     join(Reason, "\n"),
     "]]>", "</error>\n"
    ].

join([], _) ->
    [];
join([H | T], Separator) ->
    JoinFun = fun(Element) ->
                      [Separator, Element]
              end,
    [H | lists:flatmap(JoinFun, T)].
