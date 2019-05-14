Introduction
============

Lux (LUcid eXpect scripting) is a test automation framework with
Expect style execution of commands. See [Expect][] for more info about
the origin.

With Lux it is possible to

* simplify automated testing
* control interactive programs by sending textual input to them and
  using [regular expression][]s to ensure that their output matches the
  expectations
* perform detailed post mortem analyzis of test suite results
* interactively debug and trace single test cases
* get editor support for editing scripts by using the [Emacs][] mode

The tool is written in [Erlang/OTP][] and requires its runtime
environment.

See the file **lux.html** for the full documentation or view it online
on [GitHub](https://github.com/hawk/lux/blob/master/doc/lux.md).

A sample script
---------------

Here is an example of a test script. It starts couple of concurrent
shells, sends text to them with the `!` command and matches expected
output with `?`.

Snippet from the enclosed `.../lux/examples/intro.lux` file:

>     [doc Test of single and multi line regular expressions]
>     
>     # Assign a global variable which is accessible in all shells
>     [global file=removeme.txt]
>     
>     # Start a shell
>     [shell single]
>         # Send text to the active shell
>         !echo foo
>         # Match output from the active shell
>         # The terminal echoes all input and here we match on the echoed input
>         ?echo foo
>     
>     # Start yet another shell (and make it the active one)
>     [shell multi]
>         # Create a file where bar and baz happens to be indented
>         # Variables are
>         !echo "foo"      > $file
>         !echo "    bar" >> $file
>         !echo "  baz"   >> $file
>         !echo "fum"     >> $file
>     
>         # Single line matches
>         !cat $file
>         ?foo
>         ?bar
>         # Don't bother of matching baz. All output between bar and fum is skipped.
>         ?fum
>         # Match the predefined shell prompt
>         ?SH-PROMPT:
>     
>         # Multi line match. The first double quote char defines the first
>         # column of the regexp. The indentation of bar and baz is significant.
>         !cat $file
>         """?
>         foo
>             bar
>           baz
>         fum
>         SH-PROMPT:
>         """
>     
>     # Switch back to the first shell
>     [shell single]
>         # Match the actual output from the echo command
>         ?^foo
>     
>     # Cleanup side effects. The cleanup section is always executed,
>     # regardless of the script succeeds or fails
>     [cleanup]
>         !rm -f $file
>         ?SH-PROMPT:
>         # Match command exit status. Observe the double dollar sign which
>         # escapes the dollar sign, implying "echo ==$$?==" to be sent to
>         # the shell.
>         !echo ==$$?==
>         ?^==0==
>     

How to run the script
---------------------

Run a single script like this:

Evaluate `lux examples/intro.lux`

>     .../lux> lux examples/intro.lux
>     summary log       : /Users/hmattsso/dev/tailf/lux/lux_logs/run_2019_05_14_21_30_44_568831/lux_summary.log
>     test case         : examples/intro.lux
>     progress          : ..:..:..:.:..:.:.:.....:..:..:..:..:.:..:.:..:.:.:..:.:..:.....:..:.:.:....c......:.:.:..:.:..:..:.:..:..:..:.
>     result            : SUCCESS
>     successful        : 1
>     summary           : SUCCESS
>     file:///Users/hmattsso/dev/tailf/lux/lux_logs/run_2019_05_14_21_30_44_568831/lux_summary.log.html
>     .../lux> echo $?
>     0


In this run we got a (brief) progress report of the test case on
stdout and a link to a summary log containing (lots of) details.

How to assemble the history of multiple runs
--------------------------------------------

In a nightly build environment it might be difficult to pinpoint when
a certain test case/suite started to fail. This process is greatly
simplified by running `lux` with the `--history` option as it will
assemble all test results as a timeline (interleaved with change-set
identities if provided with `--revision`).

Evaluate `lux --revision svn_4711 --run jenkins_17 examples`


Evaluate `lux --revision svn_4712 --run jenkins_20 examples/intro.lux`


Evaluate `lux --revision svn_4712 --run jenkins_20 examples/fail.lux`


Evaluate `lux --revision svn_4715 --run jenkins_22 examples`


Evaluate `lux --history .`

>     .../lux> lux --history .
>     Assembling history of logs from...
>     	........................ss...................
>     INTERNAL LUX ERROR in ./tutorial/chatty/test/intro/lux_logs/run_2019_05_14_17_35_39_52759/lux_summary.log
>     function_clause
>     [{lux_log,split_result,
>               [[<<"successful        : 0\nerrors            : 1\n\ta_simple_server.lux:0 - INTERNAL LUX ERROR: {'EXIT',\n                        {{case_clause,no_shell},\n                         [{lux_debug,wait_for_reply,3,\n                              [{file,\"lux_debug.erl\"},{line,134}]},\n                          {lux_debug,init,2,\n                              [{file,\"lux_debug.erl\"},{line,71}]}]}}">>,
>                 <<"summary           : ERROR\n">>]],
>               [{file,"lux_log.erl"},{line,226}]},
>      {lux_log,parse_summary_result,2,[{file,"lux_log.erl"},{line,501}]},
>      {lux_log,do_parse_summary_log,3,[{file,"lux_log.erl"},{line,135}]},
>      {lux_log,try_parse_summary_log,2,[{file,"lux_log.erl"},{line,118}]},
>      {lux_log,parse_summary_log,2,[{file,"lux_log.erl"},{line,101}]},
>      {lux_html_history,parse_summary_files,7,
>                        [{file,"lux_html_history.erl"},{line,941}]},
>      {lists,foldl,3,[{file,"lists.erl"},{line,1263}]},
>      {lux_html_history,collect_branch,5,
>                        [{file,"lux_html_history.erl"},{line,99}]}]
>     ..s....ss.....................s...ss...........................
>     Analyzed 98 test runs (1 errors)...ok
>     file:///Users/hmattsso/dev/tailf/lux/lux_history.html
>     .../lux> echo $?
>     0

