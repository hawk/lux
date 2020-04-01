Lux - LUcid eXpect scripting
============================

Version 2.2.3 - 2020-04-01

* [Introduction](#../README)
* [Concepts](#main_concepts)
* [Tutorial](#../tutorial/README)
* [Script syntax](#script_syntax)
* [Command line options](#cmd_line_opts)
* [Configuration parameters](#config_params)
* [Logs](#logs)
* [Debugger for Lux scripts](#debug_cmds)
* [Examples](#examples)
* [Installation](#../INSTALL)
* [Original author](#../AUTHORS)
* [References](#references)

<a name="../README"/>

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

<a name="main_concepts"/>

Concepts
--------

A Lux script may succeed or fail, meaning that the system under
test is either conforming to or diverging from the expected
behavior. A Lux script may also end with an **error**, if the
Lux engine encountered some problem, and could not determine
whether the system under test conforms or not. A syntax error in
the Lux script is an error, not a failure.

A Lux script consists of a sequence of instructions, mostly `send`
and `expect` operations, read in sequence from top to bottom. The test
case **succeed**s if all statements in the script are executed (or if an
optional success criteria matched). The test case **fail**s if there
is an `expect` operation not matching within a given time (or if an
optional failure criteria matches).

Each test case should not depend on other test cases being executed
before (for example preparing something) or after (to clean up).
The **cleanup** procedure is an integral part of each test case.

Each `send` and `expect` operation is directed to one particular shell
(the **active shell**). Input is sent to the **stdin** stream of the
shell. The **stdout** and **stderr** streams of the shell are combined
to one single output stream. A Lux script can start and control
many concurrent shells, but at any time point only one is the active
shell which can evaulate new Lux statements.

It is possible to reference **variables** in the `send`, `expect`,
`my`, `local`, `global` and `config` statements, using the standard
shell `$var` or `${var}` notation. Note that the substitution is
performed by the Lux engine and not by the process running within
the shell. The variables are initially set to all **environment
variables**. But new settings can be added by using `[my var=val]`,
`[local var=val]` and `[global var=val]`. Each such variable setting
overrides existing settings of the same variable.

If no variable substitution should take place, the dollar sign must be
*escape*d with yet another dollar sign (`$`). For example, if the
actual value of an environment variable named `var` should be read
from the Bourne shell, the `$var` string cannot be substituted to
another value by the Lux engine. The string `$var` must be sent
literally to the shell even if the Lux engine happens to have
have a variable registered with that name. In order to achieve this
the `$var` must be escaped as `$$var`.

The [regular expression][]s in `expect` statements may contain
**sub-patterns**. The values matching captured sub-patterns may be
accessed in the following variable assignments. For example if the
statement `?begin (.*) middle (.*) end` matches the actual shell output
`begin abc middle def end`. The captured sub-patterns can be accessed
as numbered variables: `[local foo=the vals are $1 and $2]` which will
assign the variable `foo` to the value "`the vals are abc and def`".
As in any language [regular expression][]s contains keywords. If the
output of a shell contains such a keyword and we want to match that,
the keyword must be escaped with a backslash. Example of such keywords
are any of the characters `^$.?+*()[]{}|`.

The variable substitution mechanism makes it also possible to reuse
(parts of) generic scripts in several test cases. The (main) script
for these test cases may assign different values to variables and then
include a generic script that makes use of these variables.

See the documentation about [regular expression][]s in Erlang for
details about the regular expression dialect used in Lux. It is
an extended subset of [PCRE][PCRE].
<a name="../tutorial/README"/>

Tutorial
========

Installation
------------

Read the file **.../lux/tutorial/INSTALL.md** or view it online on
[GitHub](https://github.com/hawk/lux/blob/euc/tutorial/INSTALL.md)
about how to do a install LUX, build and test the chatty app.

>     cd .../lux/tutorial/chatty
>     make build

How do we test a simple chat server?
------------------------------------

Imagine a scenario where we start a server and two clients. The
clients cannot connect until the server is up and running. When text
is entered in one client it must be verified that it is displayed in
the other client(s). You can start the system with these commands,
using three different shells:

>     cd chatty/test/intro
>     erl -pa ../../../chatty/ebin -noshell -sname mytopic -s chatty server
>     erl -pa ../../../chatty/ebin -noshell -sname cons    -s chatty client mytopic
>     erl -pa ../../../chatty/ebin -noshell -sname hawk    -s chatty client mytopic

Walkthru the test cases and emphasize on their differences
----------------------------------------------------------

Now when you are familiar with the system, how would you write an
automated test case for it? That is a stable test without race
conditions.

Walkthru these test cases below and emphasize on their differences.
Hopefully the test code is self-explanatory.

Evaluate `cd tutorial/chatty/test/intro && lux .`

>     .../lux> cd tutorial/chatty/test/intro && lux .
>     summary log       : /Users/hmattsso/dev/lux/tutorial/chatty/test/intro/lux_logs/run_2019_06_11_15_10_04_314531/lux_summary.log
>     test case         : a_simple_server.lux
>     progress          : ..:...:.:..:..:.:..:....:.:..:.:.:...:.:..:..:....
>     result            : SUCCESS
>     test case         : async_startup_fail.lux
>     progress          : ..:..:.:..:..:.:..:.:.:.:....:.:.:..:.:...:.:..:..:.:.Will fail due to startup race cond.:.:.:..:.:.:.:.:.:.25????25..
>     result            : FAIL at 25 in shell hawk
>     expected*
>     	Trying to join the mytopic chat room...
>     	Welcome to the chat room mytopic!a!!
>     	Enter text and press enter. Exit chat with \^d.
>     	
>     	hawk>
>     actual match_timeout
>     	erl -pa ../../../chatty/ebin -sname hawk -noshell -s chatty client myt opic
>     	Trying to join the mytopic chat room...
>     	<ERROR> Failed to join 'mytopic@HMATTSSO-M-N1P1'. Is the server started?
>     	{"init terminating in do_boot",shutdown}
>     	init terminating in do_boot (shutdown)
>     	SH-PROMPT:
>     diff
>     	+ erl -pa ../../../chatty/ebin -sname hawk -noshell -s chatty client myt 
>     	+ opic
>     	  Trying to join the mytopic chat room...
>     	- Welcome to the chat room mytopic!a!!
>     	- Enter text and press enter. Exit chat with \^d.
>     	- 
>     	- hawk>
>     	+ <ERROR> Failed to join 'mytopic@HMATTSSO-M-N1P1'. Is the server started?
>     	+ {"init terminating in do_boot",shutdown}
>     	+ init terminating in do_boot (shutdown)
>     	+ SH-PROMPT:
>     	
>     test case         : sync_startup.lux
>     progress          : ..:..:.:..:..:..:.:.:.:..:.:.:.:....:..:..:..:.:..:.:....:..:.:..:..:.:..:.:.:.:....:..:..:..:.:..:.:.:.:.::......:..::.....:............
>     result            : SUCCESS
>     test case         : sync_startup_cleanup.lux
>     progress          : ()..:..:.:..:...:..:.:.:.:..:.:.:.:....:..:.:..:..:..:.:....:..:..:...:.:.:.:.:.:....:..:..:...:.:.:..:.:....:.:.:.::..c..........:..:..:.(.:..:.:.)(.:..:.:.)((..:.:.:.:.:.:.:.:.:.:.:.:.)(.:.:..:..))((.:..:.:.:.:.:.:.:.:.:.)(.:..:..))
>     result            : SUCCESS
>     successful        : 3
>     failed            : 1
>     	async_startup_fail.lux:25 - match_timeout
>     summary           : FAIL
>     file:///Users/hmattsso/dev/lux/tutorial/chatty/test/intro/lux_logs/run_2019_06_11_15_10_04_314531/lux_summary.log.html
>     .../lux> echo $?
>     1


Snippet from the enclosed `.../lux/tutorial/chatty/test/intro/a_simple_server.lux` file:

>     [doc Demo a simple single shell test case]
>     
>     # Start a shell
>     [shell server]
>         # Send text to the active shell
>         !erl -sname server -pa ../../../chatty/ebin
>         # Match output from the active shell
>         ?Erlang/OTP
>         ?Eshell
>         ?> 
>     
>         !chatty:server().
>         ?Starting server
>         ?> 
>     
>         !halt(3).
>         ?SH-PROMPT:
>     
>         !echo "===$?==="
>         ?===3===
>         ?SH-PROMPT:
>     

Snippet from the enclosed `.../lux/tutorial/chatty/test/intro/async_startup_fail.lux` file:

>     [doc Demo too fast startup]
>     
>     # Assign a global variable which is accessible in all shells
>     [global topic=mytopic]
>     [global ebin=../../../chatty/ebin]
>     
>     [shell server]
>         !erl -pa $ebin -sname $topic -s chatty server
>         ?Starting server
>     
>     [shell hawk]
>         !export ERL_CRASH_DUMP_BYTES=0
>         ?SH-PROMPT:
>         !erl -pa $ebin -sname hawk -noshell -s chatty client $topic
>     
>         [progress Will fail due to startup race cond]
>         # Multi line match. The first double quote char defines the first
>         # column of the regexp.
>         """?
>         Trying to join the $topic chat room...
>         Welcome to the chat room $topic!a!!
>         Enter text and press enter. Exit chat with \^d.
>     
>         hawk>
>         """
>     

Snippet from the enclosed `.../lux/tutorial/chatty/test/intro/sync_startup.lux` file:

>     [doc Demo start sync]
>     
>     [global topic=mytopic]
>     [global ebin=../../../chatty/ebin]
>     
>     [shell server]
>         !erl -pa $ebin -sname $topic -s chatty server
>         ?Starting server
>     
>         # Match sub-expressions
>         ?Trying to open log file (.*)\.\.\.ok.
>         [global logfile=$1]
>     
>     # Start another shell
>     [shell server-log]
>         # Match in log file
>         !tail -F $logfile
>         ?Server started
>     
>     [shell hawk]
>         !erl -pa $ebin -sname hawk -noshell -s chatty client $topic
>         # Match with variable
>         """?
>         Trying to join the $topic chat room...
>         Welcome to the chat room $topic!!!
>         Enter text and press enter. Exit chat with \^d.
>     
>         hawk>
>         """
>     
>     [shell cons]
>         !erl -pa $ebin -sname cons -noshell -s chatty client $topic
>         # Verbatim match
>         """??
>         Trying to join the $topic chat room...
>         Welcome to the chat room $topic!!!
>         Enter text and press enter. Exit chat with ^d.
>     
>         cons>
>         """
>     
>     # Switch active shell
>     [shell hawk]
>         ?cons: Client joined
>         !ping
>         ?hawk>
>     
>     [shell server-log]
>         ?Client hawk said ping
>     
>     [shell cons]
>         ?hawk: ping
>     

Snippet from the enclosed `.../lux/tutorial/chatty/test/intro/sync_startup_cleanup.lux` file:

>     [doc Demo cleanup]
>     
>     [include ../../../support/luxinc/macros.luxinc]
>     
>     [global topic=mytopic]
>     [global ebin=../../../chatty/ebin]
>     
>     [shell server]
>         # Set fail pattern for shell
>         -[Ee][Rr][Rr][Oo][Rr]]
>         !erl -pa $ebin -sname $topic -s chatty server
>         ?Starting server
>     
>         ?Trying to open log file (.*)\.\.\.ok.
>         [global logfile=$1]
>     
>     [shell server-log]
>         !tail -F $logfile
>         ?Server started
>     
>     [shell hawk]
>         !erl -pa $ebin -sname hawk -noshell -s chatty client $topic
>         """??
>         Trying to join the $topic chat room...
>         Welcome to the chat room $topic!!!
>         Enter text and press enter. Exit chat with ^d.
>     
>         hawk>
>         """
>     
>     [shell cons]
>         # Use interactive Erlang shell
>         !erl -pa $ebin -sname cons
>         ?Erlang/OTP
>         ?Eshell
>         ?cons@
>         !chatty:client(['${topic}']).
>         """??
>         Trying to join the $topic chat room...
>         Welcome to the chat room $topic!!!
>         Enter text and press enter. Exit chat with ^d.
>     
>         cons>
>         """
>     
>     [cleanup]
>         # Kill lingering processes
>         [invoke eval_any "pkill -f beam.*chatty.*client"]
>         [invoke eval_any "pkill -f beam.*chatty.*server"]
>     
>         # Save log file
>         [invoke eval "mkdir -p ${LUX_EXTRA_LOGS}"]
>         [invoke eval "cp $logfile ${LUX_EXTRA_LOGS}/"]
>     

Post mortem analysis
--------------------

Walkthru the different logs from the latest test run. They are found
at `lux_logs/latest_run`. With this command you get a list of all logs:

Evaluate `cd tutorial/chatty/test/intro && ls -ld lux_logs/latest_run`

>     .../lux> cd tutorial/chatty/test/intro && ls -ld lux_logs/latest_run
>     lrwxr-xr-x 1 hmattsso staff 30 Jun 11 17:10 lux_logs/latest_run -> run_2019_06_11_15_10_04_314531
>     .../lux> echo $?
>     0


Evaluate `cd tutorial/chatty/test/intro && find -L lux_logs/latest_run`

>     .../lux> cd tutorial/chatty/test/intro && find -L lux_logs/latest_run
>     lux_logs/latest_run
>     lux_logs/latest_run/sync_startup.lux.event.log
>     lux_logs/latest_run/sync_startup.lux.event.log.html
>     lux_logs/latest_run/sync_startup_cleanup.lux.cleanup.stdin.log
>     lux_logs/latest_run/a_simple_server.lux.server.stdout.log
>     lux_logs/latest_run/sync_startup.lux.hawk.stdout.log
>     lux_logs/latest_run/async_startup_fail.lux.hawk.stdin.log
>     lux_logs/latest_run/async_startup_fail.lux.event.log.html
>     lux_logs/latest_run/async_startup_fail.lux.hawk.stdout.log
>     lux_logs/latest_run/async_startup_fail.lux.event.log.csv
>     lux_logs/latest_run/sync_startup_cleanup.lux.server.stdout.log
>     lux_logs/latest_run/sync_startup.lux.hawk.stdin.log
>     lux_logs/latest_run/lux_summary.log.html
>     lux_logs/latest_run/async_startup_fail.lux.config.log
>     lux_logs/latest_run/sync_startup_cleanup.lux.cons.stdin.log
>     lux_logs/latest_run/sync_startup_cleanup.lux.server-log.stdout.log
>     lux_logs/latest_run/sync_startup_cleanup.lux.extra.logs
>     lux_logs/latest_run/sync_startup_cleanup.lux.extra.logs/chatty_mytopic.log
>     lux_logs/latest_run/sync_startup.lux.orig
>     lux_logs/latest_run/sync_startup_cleanup.lux.event.log.csv
>     lux_logs/latest_run/sync_startup_cleanup.lux.cleanup.stdout.log
>     lux_logs/latest_run/sync_startup_cleanup.lux.event.log
>     lux_logs/latest_run/sync_startup.lux.config.log
>     lux_logs/latest_run/sync_startup_cleanup.lux.hawk.stdout.log
>     lux_logs/latest_run/sync_startup.lux.server-log.stdin.log
>     lux_logs/latest_run/a_simple_server.lux.event.log.html
>     lux_logs/latest_run/async_startup_fail.lux.server.stdout.log
>     lux_logs/latest_run/sync_startup.lux.server.stdin.log
>     lux_logs/latest_run/a_simple_server.lux.orig
>     lux_logs/latest_run/async_startup_fail.lux.event.log
>     lux_logs/latest_run/sync_startup_cleanup.lux.orig
>     lux_logs/latest_run/sync_startup_cleanup.lux.hawk.stdin.log
>     lux_logs/latest_run/sync_startup_cleanup.lux.server.stdin.log
>     lux_logs/latest_run/sync_startup_cleanup.lux.server-log.stdin.log
>     lux_logs/latest_run/a_simple_server.lux.event.log.csv
>     lux_logs/latest_run/sync_startup_cleanup.lux.config.log
>     lux_logs/latest_run/Users
>     lux_logs/latest_run/Users/hmattsso
>     lux_logs/latest_run/Users/hmattsso/dev
>     lux_logs/latest_run/Users/hmattsso/dev/lux
>     lux_logs/latest_run/Users/hmattsso/dev/lux/tutorial
>     lux_logs/latest_run/Users/hmattsso/dev/lux/tutorial/support
>     lux_logs/latest_run/Users/hmattsso/dev/lux/tutorial/support/luxinc
>     lux_logs/latest_run/Users/hmattsso/dev/lux/tutorial/support/luxinc/macros.luxinc.orig
>     lux_logs/latest_run/lux_config.log
>     lux_logs/latest_run/async_startup_fail.lux.server.stdin.log
>     lux_logs/latest_run/sync_startup.lux.cons.stdout.log
>     lux_logs/latest_run/sync_startup.lux.server-log.stdout.log
>     lux_logs/latest_run/sync_startup_cleanup.lux.event.log.html
>     lux_logs/latest_run/lux_result.log
>     lux_logs/latest_run/async_startup_fail.lux.orig
>     lux_logs/latest_run/lux_summary.log
>     lux_logs/latest_run/sync_startup.lux.server.stdout.log
>     lux_logs/latest_run/a_simple_server.lux.config.log
>     lux_logs/latest_run/sync_startup.lux.event.log.csv
>     lux_logs/latest_run/sync_startup_cleanup.lux.cons.stdout.log
>     lux_logs/latest_run/a_simple_server.lux.event.log
>     lux_logs/latest_run/a_simple_server.lux.server.stdin.log
>     lux_logs/latest_run/lux.tap
>     lux_logs/latest_run/sync_startup.lux.cons.stdin.log
>     .../lux> echo $?
>     0


Some logs are common for all test cases in a test suite:

  - Summary log - a summary of the outcome of the test suite
  - Config log - actual configuration for the run
  - Annotated summary log (HTML) - pretty printed asummary log

while others are per test case:

  - Event log - a trace of internal lux events
  - Extra logs - user defined logs/files worth to save after the run
  - Config log - test case specific configuration
  - Statistics - low level info about actual duration of timers
  - TAP log - summary log on TAP format
  - JUnit log - summary log on JUnit format
  - Annotated event log (HTML) - pretty printed event log with links to other logs

and yet some are per shell in the test case:

  - Shell stdin log(s) - bytes sent to stdin of the shell
  - Shell stdout log(s) - bytes received from stdout (and stderr) of the shell

Debugging
---------

There are various ways of debugging test cases. The simplest way is to
use the `--progress=verbose` flag or `-v` for short:

>     lux -v a_simple_server.lux

Evaluate `cd tutorial/chatty/test/intro && lux -v a_simple_server.lux`

>     .../lux> cd tutorial/chatty/test/intro && lux -v a_simple_server.lux
>     summary log       : /Users/hmattsso/dev/lux/tutorial/chatty/test/intro/lux_logs/run_2019_06_11_15_10_28_2162/lux_summary.log
>     test case         : a_simple_server.lux
>     event log         : 0.3
>     /Users/hmattsso/dev/lux/tutorial/chatty/test/intro/a_simple_server.lux
>     lux(0): start_time "2019-06-11 17:10:28.110469"
>     lux(1): doc "Demo a simple single shell test case"
>     server(4): start "/Users/hmattsso/dev/lux/priv/bin/runpty /bin/sh -i"
>     server(4): expected* ".+"
>     server(4): timer started (10 seconds * 1.000 multiplier)
>     server(4): recv "\e[?1034hsh-3.2$ "
>     server(4): timer canceled (after 7104 micro seconds)
>     server(4): match "\e[?1034hsh-3.2$ "
>     server(4): send "export PS1=SH-PROMPT:
>         "
>     server(4): recv "expo"
>     server(4): recv "rt PS1=SH-PROMPT:
>         "
>     server(4): expected* "^SH-PROMPT:"
>     server(4): timer started (10 seconds * 1.000 multiplier)
>     server(4): recv "SH-PROMPT:"
>     server(4): timer canceled (after 133 micro seconds)
>     server(4): skip "export PS1=SH-PROMPT:
>         "
>     server(4): match "SH-PROMPT:"
>     server(6): send "erl -sname server -pa ../../../chatty/ebin
>         "
>     server(6): recv "erl -"
>     server(6): recv "sname server -pa ../../../chatty"
>     server(8): expected* "Erlang/OTP"
>     server(8): timer started (10 seconds * 1.000 multiplier)
>     server(8): recv "/ebin
>         "
>     server(8): recv "Erlang/OTP 21 [erts-10.3.5] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]
>         
>         "
>     server(8): timer canceled (after 108215 micro seconds)
>     server(8): skip "erl -sname server -pa ../../../chatty/ebin
>         "
>     server(8): match "Erlang/OTP"
>     server(9): expected* "Eshell"
>     server(9): timer started (10 seconds * 1.000 multiplier)
>     server(9): recv "Eshell V10.3.5  (abort with ^G)
>         "
>     server(9): timer canceled (after 117009 micro seconds)
>     server(9): skip " 21 [erts-10.3.5] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]
>         
>         "
>     server(9): match "Eshell"
>     server(9): recv "(server@HMATTSSO-M-N1P1)1> "
>     server(10): expected* "> "
>     server(10): timer started (10 seconds * 1.000 multiplier)
>     server(10): timer canceled (after 8 micro seconds)
>     server(10): skip " V10.3.5  (abort with ^G)
>         (server@HMATTSSO-M-N1P1)1"
>     server(10): match "> "
>     server(12): send "chatty:server().
>         "
>     server(13): expected* "Starting server"
>     server(13): timer started (10 seconds * 1.000 multiplier)
>     server(13): recv "chatty:server().
>         "
>     server(13): recv "Starting server server...
>         "
>     server(13): timer canceled (after 21878 micro seconds)
>     server(13): skip "chatty:server().
>         "
>     server(13): match "Starting server"
>     server(14): expected* "> "
>     server(14): timer started (10 seconds * 1.000 multiplier)
>     server(14): recv "Trying to open log file chatty_server.log..."
>     server(14): recv "ok.
>         <0.86.0>"
>     server(14): recv "
>         (server@HMATTSSO-M-N1P1)2> "
>     server(14): timer canceled (after 3002721 micro seconds)
>     server(14): skip " server...
>         Trying to open log file chatty_server.log...ok.
>         <0.86.0>
>         (server@HMATTSSO-M-N1P1)2"
>     server(14): match "> "
>     server(16): send "halt(3).
>         "
>     server(17): expected* "SH-PROMPT:"
>     server(17): timer started (10 seconds * 1.000 multiplier)
>     server(17): recv "halt(3).
>         "
>     server(17): recv "SH-PROMPT:"
>     server(17): timer canceled (after 4958 micro seconds)
>     server(17): skip "halt(3).
>         "
>     server(17): match "SH-PROMPT:"
>     server(19): send "echo "===$?==="
>         "
>     server(19): recv "echo ""
>     server(20): expected* "===3==="
>     server(20): timer started (10 seconds * 1.000 multiplier)
>     server(20): recv "===$?==="
>         ===3===
>         SH-PROMPT:"
>     server(20): timer canceled (after 190 micro seconds)
>     server(20): skip "echo "===$?==="
>         "
>     server(20): match "===3==="
>     server(21): expected* "SH-PROMPT:"
>     server(21): timer started (10 seconds * 1.000 multiplier)
>     server(21): timer canceled (after 9 micro seconds)
>     server(21): skip "
>         "
>     server(21): match "SH-PROMPT:"
>     server(22): no cleanup
>     server(22): inactivate zombify
>     server(22): end of script
>     server(22): stop success
>     lux(0): end_time "2019-06-11 17:10:31.407339"
>     result            : SUCCESS
>     successful        : 1
>     summary           : SUCCESS
>     file:///Users/hmattsso/dev/lux/tutorial/chatty/test/intro/lux_logs/run_2019_06_11_15_10_28_2162/lux_summary.log.html
>     .../lux> echo $?
>     0


The shell stdin log is also quite useful when trying to reproduce a
run of a test case.

  - Start multiple terminalks and create shells manually
  - Copy and paste from stdin logs to the shells

Evaluate `cd tutorial/chatty/test/intro && cat lux_logs/latest_run/a_simple_server.lux.server.stdin.log`

>     .../lux> cd tutorial/chatty/test/intro && cat lux_logs/latest_run/a_simple_server.lux.server.stdin.log
>     export PS1=SH-PROMPT:
>     erl -sname server -pa ../../../chatty/ebin
>     chatty:server().
>     halt(3).
>     echo "===$?==="
>     .../lux> echo $?
>     0


Evaluate `cd tutorial/chatty/test/intro && cat lux_logs/latest_run/a_simple_server.lux.server.stdout.log`

>     .../lux> cd tutorial/chatty/test/intro && cat lux_logs/latest_run/a_simple_server.lux.server.stdout.log
>     [?1034hsh-3.2$ export PS1=SH-PROMPT:
>     SH-PROMPT:erl -sname server -pa ../../../chatty/ebin
>     Erlang/OTP 21 [erts-10.3.5] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]
>     
>     Eshell V10.3.5  (abort with ^G)
>     (server@HMATTSSO-M-N1P1)1> chatty:server().
>     Starting server server...
>     Trying to open log file chatty_server.log...ok.
>     <0.86.0>
>     (server@HMATTSSO-M-N1P1)2> halt(3).
>     SH-PROMPT:echo "===$?==="
>     ===3===
>     .../lux> echo $?
>     SH-PROMPT:0


Lux has a built-in debugger. It is always present, ready to read
commands from stdin. For example the command `tail` or `t` for short
can be used to interactively display the various logs while the test
is running. By default it displays the last 10 lines of the event
log. So when the test program is waiting for output you can use `tail`
to see what is going on.

>     lux --timeout=30000 async_startup.lux
>     t

Just pressing enter without any command will simply repeat the
previous command. Some commands behaves slightly different when they
are repeated. The `tail` command for example displays more and more
for each time. It displays 10 lines, 20, lines, 30 lines, ...

The test script may also be attached before line 1 with `--debug` or
`-d` for short:

>     lux -d a_simple_server.lux

There you can explore the available commands using the built-in `help`
command. Try these commands out and see what happens:

Evaluate `cd tutorial/chatty/test/intro && lux a_simple_server.delux`


Snippet from the enclosed `.../lux/tutorial/chatty/test/intro/lux_logs/latest_run/a_simple_server.delux.debug.stdin.log` file:

>     export PS1=SH-PROMPT:
>     rm -rf tmp_logs
>     lux -d --log_dir=tmp_logs a_simple_server.lux
>     c 15
>     shell server
>     !im().
>     ?
>     n
>     t
>     help quit
>     c
>     

Snippet from the enclosed `.../lux/tutorial/chatty/test/intro/lux_logs/latest_run/a_simple_server.delux.debug.stdout.log` file:

>     [?1034hsh-3.2$ export PS1=SH-PROMPT:
>     SH-PROMPT:rm -rf tmp_logs
>     SH-PROMPT:lux -d --log_dir=tmp_logs a_simple_server.lux
>     summary log       : /Users/hmattsso/dev/lux/tutorial/chatty/test/intro/tmp_logs/lux_summary.log
>     
>     test case         : a_simple_server.lux
>     progress          : 
>     Break at "a_simple_server.lux:1"
>     
>     File a_simple_server.lux:
>     1> [doc Demo a simple single shell test case]
>     2: 
>     3: # Start a shell
>     4: [shell server]
>     5:     # Send text to the active shell
>     6:     !erl -sname server -pa ../../../chatty/ebin
>     7:     # Match output from the active shell
>     8:     ?Erlang/OTP
>     9:     ?Eshell
>     10:     ?> 
>     
>     Debugger for lux. Try help or continue.
>     c 15
>     
>     Set temporary breakpoint at "a_simple_server.lux:15"
>     
>     Continue to run from "a_simple_server.lux:1"
>     ..:..:.:..:..:.:..:.:..:.:....:.:..14?:?:?:?
>     Break at "a_simple_server.lux:15"
>     
>     File a_simple_server.lux:
>     13:     ?Starting server
>     14:     ?> 
>     15> 
>     16:     !halt(3).
>     17:     ?SH-PROMPT:
>     18: 
>     19:     !echo "===$?==="
>     20:     ?===3===
>     21:     ?SH-PROMPT:
>     22: 
>     22: 
>     shell server
>     
>     Connect to shell "server" in background mode.
>     !im().
>     
>     Send data to shell "server".
>     
>     server(send): im().
>     server(send): 
>     
>     server(recv): im().
>     server(recv): <0.89.0>
>     server(recv): (server@HMATTSSO-M-N1P1)3> 
>     ?
>     
>     Reset output buffer for shell "server".
>     n
>     
>     File a_simple_server.lux:
>     16>     !halt(3).
>     t
>     Log files at tmp_logs/.:
>     
>     *  1 lux_config.log
>     *  2 lux_summary.log.tmp
>     *  3 lux_result.log
>     *  4 a_simple_server.lux.config.log
>     *  5 a_simple_server.lux.event.log
>     *  6 a_simple_server.lux.server.stdin.log
>     *  7 a_simple_server.lux.server.stdout.log
>     
>     Last 10 (54..63) lines of log file: a_simple_server.lux.event.log
>     
>     server(14): recv "Trying to open log file chatty_server.log..."
>     server(14): recv "ok.\r\n"
>     server(14): recv "<0.86.0>\r\n(server@HMATTSSO-M-N1P1)2> "
>     server(14): timer canceled (after 3003945 micro seconds)
>     server(14): skip " server...\r\nTrying to open log file chatty_server.log...ok.\r\n<0.86.0>\r\n(server@HMATTSSO-M-N1P1)2"
>     server(14): match "> "
>     server(14): send "im().\n"
>     server(14): recv "im().\r\n<0.89.0>\r\n(server@HMATTSSO-M-N1P1)3> "
>     server(14): output reset
>     
>     help quit
>     
>     quit \[scope\]
>     --------------
>     
>     Quit a single test case or the entire test suite
>     in a controlled manner. Runs cleanup if applicable.
>     
>     **Parameters:**  
>     
>     * scope - scope of exit; enum(case|suite)  
>     
>     c
>     
>     Continue to run from "a_simple_server.lux:16"
>     
>     server(send): halt(3).
>     server(send): 
>     
>     server(recv): halt(3).
>     server(recv): 
>     
>     server(recv): SH-PROMPT:
>     
>     server(send): echo "===$?==="
>     server(send): 
>     
>     server(recv): echo
>     
>     server(recv):  "===$?==="
>     server(recv): ===3===
>     server(recv): 
>     
>     server(recv): SH-PROMPT:
>     .
>     Cleanup. Turn existing shells into zombies.
>     
>     Disconnect from shell "server".
>     .
>     result            : SUCCESS
>     

Infra-structure support
-----------------------

In a hetrogenous test environment with various types of machines
possibly with different architectures and hardware, it may be
necessary to have machine dependent configuration settings. This can
be achieved by using `.luxcfg` files. Look in the
`lux_logs/latest_run/lux_config.log` file to figure out the
architecture and name your `.luxcfg` file accordingly. It is also
possible to have a host specific configuration file or rely on the
default configuration in the file named `luxcfg`.

Typical things that may vary from machine to machine is shell settings
and test cases which only should be run on certain architectures. If
some machine is very slow the `multiplier` can be set to something
else than 1000 which is the default. The match timeout (in seconds) is
multiplied with this setting to compute the actual timeout to get
milli seconds which is used internally.

Here you can find a couple of architecture specific examples:

Evaluate `cd tutorial && find support/luxcfg`

>     .../lux> cd tutorial && find support/luxcfg
>     support/luxcfg
>     support/luxcfg/luxcfg
>     support/luxcfg/NetBSD-macppc.luxcfg
>     support/luxcfg/SunOS-i86pc.luxcfg
>     .../lux> echo $?
>     0


Here are some examples of how test cases can be skipped or marked as
unstable when architecture or host specific variables are set (or not
set).

Snippet from the enclosed `.../lux/tutorial/chatty/test/infra/skip.lux` file:

>     [doc Demonstrate an unstable test which is skipped]
>     
>     [config skip=SKIP_SUNOS]
>     
>     [shell date]
>         -[2-9]
>         !date +%S
>         ?SH-PROMPT
>     

Snippet from the enclosed `.../lux/tutorial/chatty/test/infra/unstable.lux` file:

>     [doc Demonstrate an unstable test which is run but do not clutter the results]
>     
>     [config unstable_unless=TEST_DEVELOP]
>     
>     [shell date]
>         -[2-4]
>         !date +%S
>         ?SH-PROMPT
>     

For more complex test cases there may be a need to have a build step
before running the test case(s). One way of solving this is to use
`lux --mode=list_dir` to find the directories which contain `.lux`
files, and simply run make on those directories. A simple example of
this can be found in this makefile:

Snippet from the enclosed `.../lux/tutorial/chatty/test/Makefile` file:

>     LUXDIRS=$(filter-out .,$(shell lux --mode=list_dir *))
>     
>     .PHONY: all build test history clean info
>     
>     all: build test
>     
>     build:
>     	@for d in $(LUXDIRS); do \
>     	   if test -f $$d/Makefile ; then \
>     	      (cd $$d && $(MAKE) $@) ; \
>     	   fi; \
>     	done
>     
>     test:
>     	lux .
>     
>     history:
>     	lux --history . .
>     
>     clean:
>     	rm -rf lux_logs lux_history* *~
>     	@for d in $(LUXDIRS); do \
>     	   if test -f $$d/Makefile ; then \
>     	      (cd $$d && $(MAKE) $@) ; \
>     	   fi; \
>     	done
>     info:
>     	@echo "LUXDIRS=$(LUXDIRS)"
>     

History of test run results

Snippet from the enclosed `.../lux/tutorial/chatty/test/infra/Makefile` file:

>     .PHONY: all build test history clean history_demo info
>     
>     TIMESTAMP=$(shell date +"%F_%T")
>     GITHASH=$(shell git rev-parse --verify --short HEAD)
>     TOPDIR=$(shell pwd | sed -e 's/tutorial.*/tutorial/')
>     LUXOPTS=\
>     	--config_dir=$(TOPDIR)/support/luxcfg \
>     	--revision=$(TIMESTAMP)_$(GITHASH)
>     
>     all: build test
>     
>     build:
>     
>     test:
>     	lux $(LUX_OPTS) .
>     
>     history:
>     	lux --history . .
>     
>     clean:
>     	rm -rf lux_logs lux_history* chatty_*.log erl_crash.dump *~
>     
>     history_demo:
>     	for i in 0 1 2 3 4 5 6 7 8 9; do \
>     		rev="2019-05-14_21:5$$i:04_$(GITHASH)$$i" ; \
>     		opts="--revision=$$rev --suite=demo --config_dir=$(TOPDIR)/support/luxcfg" ; \
>     		lux $$opts --hostname=sunny --config_name=SunOS-i86pc .; \
>     		lux $$opts --hostname=netty --config_name=NetBSD-macppc .; \
>     		sleep 1; \
>     	done
>     
>     info:
>     	@echo "TOPDIR=$(TOPDIR)"
>     	@echo "TIMESTAMP=$(TIMESTAMP)"
>     	@echo "GITHASH=$(GITHASH)"
>     	@echo "LUXOPTS=$(LUXOPTS)"
>     

Evaluate `rm -rf tutorial/chatty/test/infra/lux_logs`


Evaluate `cd tutorial/chatty/test/infra && make history_demo`


Evaluate `cd tutorial/chatty/test/infra && lux --history history_logs lux_logs`

>     .../lux> cd tutorial/chatty/test/infra && lux --history history_logs lux_logs
>     Assembling history of logs from...
>     	lux_logs....................
>     Analyzed 20 test runs (0 errors)...ok
>     file:///Users/hmattsso/dev/lux/tutorial/chatty/test/infra/history_logs/lux_history.html
>     .../lux> echo $?
>     0


Walkthru history_logs/lux_history.html

    - Overview
    - Per architecture
    - Per host
    - Still failing test cases

Jenkins

  - Automated tests
  - Display Jenkins test results as LUX history for non-LUX tests

More concepts
-------------

  - Fail pattern
  - Loops
    - Foreach
    - Break pattern
  - Macros
  - Variable scope
    - Environment
      - Initial values
      - Require
      - Local within one shell
      - Global for all shells
      - Statement block (my)
      - Sub expression
  - Regexp match vs verbatim match
  - Match on permutations
  - Shell config
    - Pseudo terminal (PTY)
    - Normalized prompt
    - Using other types of shells
  - Use the power of various interactive languages
  - Using LUX as an all purpose scripting language

Implementation
--------------

Why is Erlang a good fit? Primary due to its

  - Concurrency
  - Port programs
  - Built-in regular expresssions (re)
  - Timers

Lux is written as an escript which can be installed as stand-alone
(including the Erlang runtime). Reltool is used for this.

The test cases in a suite are executed in sequence where a new
interpreter process is started for each test script. The script is
interpreted statement for statement.

When a new Lux shell is to be started a new process is spawned. That
process runs the Bourne shell as a port program and acts as a man in
the middle between the interpreter and the port program.

In fact it is not that simple. To make the Bourne shell believe it is
executed in an interactive terminal there is actually one more man in
the middle. The `runpty` is a small C program which manipulates the
terminal settings for the pseudo TTY. When it has done that and setup
sockets between the parent and child process it will fork the Bourne
shell.

Input data strings from the script is sent as is to the stdin of the
port. The terminal is setup to echo the input to stdout.

The stderr is redirected to stdout. The terminal will normalise the
output from the Bourne shell (stdout and stderr) to make each end of
line a carriage return followed by a line feed. The output from the
port is buffered.

When the script expects a regexp to match the buffer a timer is
started. And the buffer is matched against the regexp when the buffer
is updated. If the buffer does not match the regexp when the timer
times out the script will fail.

If the test script has a cleanup section, the cleanup is run as yet
another Lux shell.

Lessons learned
---------------

  - Expect like testing requires a different mindset (find sync points in streams of data)
  - Testability is a vital property of products, observability
  - Effective post mortem analysis of test runs is a big time saver
  - Test cases (as well as test tools) does also needs to be debugged

More info
---------

  - Download from https://github.com/hawk/lux (Apache license)

  - See the file **../lux.html** for the full documentation or view it online
    on [GitHub](https://github.com/hawk/lux/blob/euc/doc/lux.md).

Maintenance of LUX itself
-------------------------

  - Run LUX in Erlang debugger
  - Use Erlang trace
    - Interactive display
    - Display filtered Erlang trace
  - Use Event Tracer
  - Use xref
  - Use reltool
  - Install as stand-alone incl Erlang runtime
  - Documentation
    - Markdown
    - Generated from example runs
    - Generated from built-in debugger help
    - Generated from .md.src files
  - Test of LUX itself

Tail-f/Cisco
------------

  - Widely used for testing of Tail-f products
  - Automated test environment using Jenkins
    - ~4500 Lux test cases per run
    - distributed over ~150 Docker containers
  - ConfD
    - Device configuration
    - Model driven configuration management framework for a network element
    - Render northbound interfaces such as CLI, Netconf, SNMP, Rest, RestConf
    - Tracable internal interfaces
  - NSO
    - Orchestrator for a massive number of (hetrogenous) network elements
    - Same standardised northbound interfaces as Confd
    - Standard interfaces southbound combined with
    - 100+ adaptors for network elements lacking standard interfaces
<a name="script_syntax"/>

Script syntax
=============

The Lux script syntax is as follows. The **first non whitespace**
character on each line determines how it will be processed. Lines
beginning with a `#` are comments. It is recommended to use
indentation and comments to make the scripts more readable. The **Lux
mode for [Emacs][]** (`lux/emacs/lux-mode.el`) is quite useful as it
simplifies the indentation and makes scripts more easy to read with
coloring for different types of language constructs.

Lines beginning with `"""Char` are **multi-line quotes**. The quote
ends with the next line beginning with `"""`. The opening quote and
closing quote must be in the same column of the script. The char right
after the first `"""` determines how the multi-line quote will be
interpreted. The char is interpreted as a statement just like any of
the single line statement characters (so it can be e.g. `?`, `!`, `~`,
`#`, `-`, etc).

When multi-line quotes are indented the leading whitespaces are
stripped from the quoted lines, up to but not including the column
of the double quote character, or to the first non-whitespace
character, whichever occurs first. In this process, a tab character
is treated as 8 space characters.

A backslash at end of line implies line continuation and not a
newline. This is syntactic sugar which makes it possible to split
a long line into several shorter ones. Leading whitespaces on the
following line are ignored. If the intention is to keep the backslash
at the end of the line, this can be achieved with two backslashes.

Interacting with a shell
------------------------

**#String**  
Inline style comment. The `#` must be the first non-whitespace
character on the line.

**!String**  

A `send` operation. Sends a `String` to the `stdin` of the active
shell. Adds a `LF` (line feed) at the end of the string. `String`
may contain references to variables using `$Var` or `${Var}`.

**~String**  
Same as `!String`, but it does NOT add a `LF` at the end.

**???Verbatim**  
An `expect` operation which waits for a given sequence of characters
to appear on the shell output (either `stdout` or `stderr`). All
characters in the `Verbatim` string are matched literally. This means
that even characters like `\` (backslash), `$` (dollar) etc. are
matched explicitly.

If no matching output does appear within the timeout period, the test
case is considered as failed. See the `--timeout` option. See also the
`--flush_timeout` and `--poll_timeout` configuration parameters about
customizing the `?` behavior.

**??Template**  
Like `?Verbatim`, but variables are also substituted.

**?Regexp**  
Like `??Template`, but matches a [regular expression][] after the
variable substitution. If the shell output is expected to contain a
regexp keyword, such as `^$.?+*()[]{}|`, the keyword must be escaped
with a backslash.

**?**  
Flush the output streams (`stdout`, `stderr`). Already received output
is discarded. Avoid this (mis)feature. At a first look it seems more
useful than it is. It often causes unexpected race patterns.

**?+Regexp**  
Like `?Regexp`, but has no immediate effect. It is used when the
order of the output is undeterministic. Assume a case where the
strings A, B and C occurs in the output but the order of them is
unknown. Then we need to match all permutations of the strings.
Such as ABC, ACB, BAC, BCA, CAB and CBA. It can be achieved by
the relatively simple regexp `?(ABC)|(ACB)|(BAC)|(BCA)|CAB)|(CBA)`.
But with larger regexps, possibly spanning multiple lines, it
can be quite complex to just write the regexps. Performing the
post mortem analyzis to determine which sub-pattern that is
matching which part of the output will be even worse. In the
following example `?+` is used to register a sub-pattern and `?`
evaluates the permutations of all sub-patterns (including the one
specified with `?).

    ?+A
    ?+B
    ?C

will render matching of all permutatations of A, B and C. Note the
usage of `?`. `?+` is always used in conjunction with `?`. Never `??`
nor `???`. It is the `?` command which triggers the actual regexp
match.

**-**  
**-Regexp**  
Sets the failure pattern for a shell to a regular expression (see
[regular expression][]). It is typically used to match error
messages. If the given `Regexp` matches, the test case is considered
to have failed (no further processing of the script will be performed
besides the `cleanup`). If no `Regexp` is given, the failure pattern
is reset (cleared).

The failure pattern is primarily searched for when the script
explicitly is expecting some output. That is when a command like `?`,
`??` or `???` is evaluated. It is also searched for when a shell
cannot produce more output, for example when a shell exits or when
there are no more commands to evaluate.

**+**  
**+Regexp**  
Sets the success pattern for a shell to a regular expression (see
[regular expression][]). If the given `Regexp` matches, the test case
is considered to be a success (no further processing of the script
will be performed besides the `cleanup`). If no `Regexp` is given, the
success pattern is reset (cleared).

The success pattern is primarily searched for when the script
explicitly is expecting some output. That is when a command like `?`,
`??` or `???` is evaluated. It is also searched for when a shell
cannot produce more output, for example when a shell exits or when
there are no more commands to evaluate.

**@**  
**@Regexp**  
Sets a loop break pattern for a shell to a regular expression (see
[regular expression][]). This statement is only valid in loops. It
is typically used to match output from a poll like command which is
executed over and over again and after a while the command causes some
output that will match the break pattern.  When the given `Regexp`
matches, the loop (and all nested loops) is immediately exited and the
execution continues with the first statement after the loop.

The break pattern is only searched for when the script explicitly is
expecting some output. That is when a command like `?`, `??` or `???`
is evaluated. It may be a prompt or whatever, indicating that the poll
like command has produced all output that may match the break pattern.

A loop with a break pattern can only exit by a successful match of the
break pattern. If the loop exits anyway it will cause the test case to
fail. Unless the loop break pattern is reset (cleared).

**\[endshell\]**  
**\[endshell Regexp\]**
An `expect` operation like `?`, but it waits for the `stdout` stream
of the shell to be closed. This means the shell has terminated. The
`Regexp` may optionally be used to match on the exit status from the
ehell, such as `[endshell ^0$]`.

### Meta statements ###

**\[**  
Indicates the beginning of a meta statement. Meta statements are ended
on the same line with a `]`.

**\[newshell Name\]**  
Creates a new shell named `Name`.

By default a `/bin/sh` shell (Bourne shell) is started. See
the `--shell_wrapper`, `--shell_cmd` and `--shell_arg` configuration
parameters. The current working directory of a newly started shell is
the same as the dirname of the script file. The **environment
variable** `LUX_SHELLNAME` is set to `Name`. The shell prompt variable
`PS1` is set to `SH-PROMPT:` and the first printout of the prompt is
automatically matched in an expect like manner in order to ensure that
the shell is ready for input. The `Name` may contain variables. Shell
names beginning with `lux`, `cleanup` and `post_cleanup` are reserved
for internal purposes. The **environment variable** `LUX_START_REASON`
is initially set to `normal`. See also `[cleanup]`.

**\[shell\]**  
**\[shell Name\]**  
Switches to the named shell, to make it active. If `Name` is omitted,
the active shell is deactivated. This implies no shell to be activated.

If `--newshell` mode is not activated, the command may also be used
to create a new shell named `Name`. See the configuration parameter
`--newshell`.

**\[cleanup\]**  
is the cleanup marker. If the script is prematurely aborted due to
a failure (or due to a matching success pattern) the remaining
statements in the file are normally skipped. But if the there is a
cleanup marker after the failing line (and this is the only
cleanup marker), the lines after the cleanup marker will also be
run in order to enable a controlled cleanup of leftovers. Such as
killing processes, removing files etc. When the cleanup marker is
evaluated, the running shells will be set into a non accessible mode
(**zombie mode**) and their failure and success patterns will be
reset (cleared). This means that output received by zombie shells
during the cleanup is not matched against failure or success
patterns. A brand new shell (called something beginning with
`cleanup`) will also be started. If the cleanup code causes a failure
the remaining statements (on that level) will be skipped.

The **environment variable** `LUX_START_REASON` is set to `normal`
in most shells, but if the cleanup is run due to premature failure or
premature success it will be set to `fail` or `success` respectively.
This can for example be used if you want to save the contents of
error logs, core dumps etc. in case of failure. Textual logs can
simply be written to `stdout` in order to be easily accessible in
the post mortem analyzis. For the purpose of saving binary files
the **environment variable** `LUX_EXTRA_LOGS` may be used. It
refers to a log directory name unique for each test case. The
directory is however not automatically created. It must be created
by you in the test script if you want to use it. If you have created
the directory, it will turn up as a link in the annotated event log.

**\[include FileName\]**  
Includes and runs the specified script at this point. The `FileName`
is relative to the currently executing script, unless given as an
absolute path. `.luxinc` is preferred as file extension. Variables in
`FileName` are expanded during parsing of the script, before execution
of the script.

**\[macro MacroName ArgName1 ArgName2 ...\]**  
  ...  
**\[endmacro\]**  
Declare a macro. The body of the macro consists of all lines up to the
next `[endmacro]` line. The scope of the arguments are local within
the macro. The arguments can be accessed via their names as normal
variables, such as `$ArgName1`. `[my Var=Value]` can be used to assign
temporary variables only valid within the macro. If a macro switches
to another shell it is good practice to switch back to the calling
shell before the end of the macro. One way of doing this is to get the
name of the active shell from the **environment variable**
`LUX_SHELLNAME` with `[my old=$LUX_SHELLNAME]` and later switch back
to the shell with `[shell $old]`.

**\[invoke MacroName ArgVal1 ArgVal ...\]**  
Invoke a macro. The arguments are separated with spaces. Arguments
can be quoted with the double quote (`"`) character. Double quotes
and backslashes (`\`) must be escaped with a backslash.

**\[loop Var Item1 Item2 ...\]**  
  ...  
**\[endloop\]**  
Declare a loop. The body of the loop consists of all lines up to the
next `[endloop]` line. The commands within the loop are repeated for
each item. For each iteration the loop variable `Var` is set to the
value of the current `Item`. The scope of the loop variable is the
same as a macro variable (defined with `my`).

The `Item` list may contain variables and these are expanded before
the first iteration.  Items in the expanded list are separated with
spaces. For example `[loop color blue red green]` or
`[loop color blue $more]` where `more` is set to `"red green"`.

When iterating over a set of consecutive integers, such as
`[loop iter 4 5 6 7 8 9]`, this can be written as a range expression,
like `[loop iter 4..9]`. By default the increment is 1. A custom
increment can also be set with the construct `from..to..incr`, such as
`[loop iter 4..9..2]`. This would be the same as `[loop iter 4 6
8]`. `[loop iter 9..4..2]` would be the same as `[loop iter 9 7 5]`.

In the logs the iteration counter is represented as a negative line
number. For example "8:-2:10" would mean line 10 in the second loop
iteration where the loop starts at line 8.

###Variables###

**\[local Var=Value\]**  
assigns a value to a variable that is local to the current
shell. `Value` may contain references to variables using `$Var`,
`${Var}` or `$N`, where `N` is an integer. `$N` refers to a captured
substring from the most recent `expect` operation. Subsequent `send`
operations may refer to this new variable in the same manner as
environment variables. In order to prevent variable substitutions and
keep a `$Var` string literally it must be escaped as `$$Var`. For
example this is needed when "true" environment variables needs to be
read. In order to read a variable like `$?` it must be written as
`$$?`.

**\[global Var=Value\]**  
assigns a value to a global variable. Works like `[local]`, but the
variable setting is propagated to all shells. Global variables may be
set before even if no shell is active.

**\[my Var=Value\]**  
assigns a value to a variable with a very limited scope. Works like
`[global]`, but can only be set and used in a macro or loop. The
variable setting is only valid within the macro that assigns the
variable.

###Multi-line values in variables###

    [local Var=Multi\nLine\nValue]

can be written as

    [local Var=
        """
        Multi
        Line
        Value
        """]

###Built-in variables###

    _BS_        - backspace       (ASCII 8)
    _TAB_       - horizontal tab  (ASCII 9)
    _LF_        - line feed       (ASCII 10)
    _CR_        - carriage return (ASCII 13)
     _ESC_      - escape          (ASCII 27)
    _DEL_       - delete          (ASCII 127)
    _CTRL_A_    - control a       (ASCII 1)
    ...
    _CTRL_Z_    - control z       (ASCII 26)
    _ASCII_0    - null            (ASCII 0)
    ...
    _ASCII_127_ - delete          (ASCII 127)
    N           - where N is an integer refering to a captured substring

###Built-in environment variables###

    LUX_SHELLNAME       - name of active Lux shell
    LUX_START_REASON    - reason for starting a shell (normal|fail|success)
    LUX_TIMEOUT         - value of match timeout in the active Lux shell
    LUX_FAIL_PATTERN    - value of fail pattern in the active Lux shell
    LUX_SUCCESS_PATTERN - value of success pattern in the active Lux shell
    PS1                 - shell prompt variable set by Lux

###Miscellaneous statements###

**\[doc String\]**  
**\[docN String\]**  
A test case slogan displayed in the summary log. It is also possible
to document parts of a test case by specifying a documentation level
`N`. In that case the doc statement should look like `[docN String]`
where `N` is an integer. `doc2` would mean that the documentation is on
level 2. Doc strings can be extracted from the scripts and written to
stdout with the`--mode=doc` and `--doc=N` command line options. It
gives a quick overview of the test cases and can be seen as a poor
mans test spec.

The first `[doc]` documentation string in a script is a bit special as
it is regarded as a one line summary of the script. With `--doc=0`
only the oneline summary lines are displayed.

**\[doc\]**  
  ...  
**\[enddoc\]**  
Multi-line documentation, typically to be used first in the script.
The first line is regarded as a one line summary (on level 1) and
the remaining lines on next level (2). Third line must be preceded
by empty line.

>     [doc]
>     One line summary
>
>     Details
>     More details
>     Yet more details
>     [enddoc]

would have been the same as

>     [doc One line summary]
>     [doc2 Details]
>     [doc2 More details]
>     [doc2 Yet more details]

**\[timeout\]**  
**\[timeout Seconds\]**  
The script expects the shell output to match given
[regular expression][]s. But the output must be received within a
given time limit. The `timeout` command sets the timeout for the
current shell to the given number of seconds multiplied with a
configurated factor. By default the multiplier is `1000`. For example,
by setting the `--multiplier` parameter to `2000` all timeouts will be
doubled. The resulting timeout value affects how long time `expect`
operations will wait before reporting failure. If the time is omitted
like `[timeout]`, the timeout is reset to the default timeout
specified with the `--timeout` configuration parameter. The timeout
value `infinity` means infinity.

**\[sleep Seconds\]**  
waits given number of seconds before proceeding in the script. No
`multiplier` factor is applied. The `sleep` command should be avoided
if possible. It absolutely not intended to be used for solving race
conditions. Find out some way to synchronize the test case properly
instead.

**\[progress String\]**  
Displays `String` on the `stdout` stream together with the rest of the
progress info. May contain newlines.

**\[config Var=Value\]**  
assigns a value to a [configuration parameter](#config_params). The
assignment takes place during parsing of the script file. The
configuration parameters in **architecture specific files** can be
overridden by **command line options**. For example `[config
timeout=2000]` can be overridden with `--timeout=4000`.  Explicit
`[config Var=Value]` settings in scripts takes however precedence over
settings in architecture specific files and command line options. See
the section *Configuration parameters* about valid configuration
parameters. Some config parameters can have multiple values, such as
`skip` and `require`. See their respective descriptions. See also the
configuration parameter `--config_dir` about the location of the
architecture specific files.
<a name="cmd_line_opts"/>

Command line options
====================

Normal execution mode for Lux is to execute test suites and most of
the command line options affects the execution in different ways.
There are however a few auxiliary options that can be used to make
Lux perform other tasks.

* --help
* --version
* --reltool
* --xref
* --install
* --make
* --markdown
* --annotate
* --history
* --mode
* --doc

Script execution
----------------

    lux [--mode Mode] [ConfigParam]... [File]...

Exit status is 0 if all test cases are successful and 1 otherwise.

See the section [Configuration parameters](#config_params) about
script execution.

Release management
------------------

    lux --help
    lux --version
    lux --reltool [--root_dir RootDir]
    lux --xref
    lux --install [InstallDir] [--root_dir RootDir]
    lux --make
    lux --markdown

**--help**  
Displays a brief list of all command line arguments, as well as a URL
to the full documentation.

**-h**  
A shortcut for `--help`.

**--version**  
Prints out the actual Lux version

**--reltool**  
Starts the graphical tool [Reltool][] which enables inspection of
internal Lux application dependencies. It is disabled in the
standalone installation.

**--xref**  
Perform cross reference checks of Lux itself in order to find calls to
undefined functions.

**--install \[InstallDir\]**  
See [installation](#../INSTALL). Installs the Lux application as a
standalone application in the `InstallDir` directory. `InstallDir`
must exist. If `InstallDir` is omitted only a dry run is performed. A
standalone installation is self-contained and contains a minimal
Erlang runtime system. It is however not neccessary to install Lux as
standalone. If Erlang already is installed on the system, Lux can make
use of that runtime environment. But sometimes it is useful to avoid
that dependency.
    
**--root\_dir `RootDir`**  
Directs [Reltool][] to use an alternate Erlang root directory instead
of the one currently being used. Affects `--install` and `--reltool`.

**--make**  
Performs a simplified build only relying on a pre-installed Erlang/OTP
system. To be used with care on obscure platforms. See
[installation](#../INSTALL).
    
**--markdown**  
Generates documentation for the Lux debugger on [Markdown][] format.
This is used internally by doc/Makefile.
    
Log management
--------------

    lux --annotate LogFile
    lux --history TargetLogDir [SourceLogFile]...

**--annotate LogFile**  
Transforms textual log files into HTML format and annotates Lux script
code with log events. The generated HTML file will get the same name
as `LogFile` but with a `.html` extension added. See also the
[configuration parameter](#config_params) `--html`.

**--history TargetLogDir [SourceLogFile]...**  
Generates an HTML file which summarizes the history of all test runs.
The history file will be generated on the `TargetLogDir` directory and
is named `lux_history.html`. Its behavior can be customized by using
the `--suite`, `--run`, `--revision` and `--hostname`
[configuration parameters](#config_params).

The history file generation is done by analyzing `lux_summary.log`
files. A `SourceLogFile`s may either be an already existing
`lux_history.html` file or a directory. When `SourceLogFile` is a
directory all subdirectories not containing a `lux.skip` file will be
searched for `lux_summary.log` files.

`SourceLogFile` may also be a `lux_history.html` file, in that case
the `lux_summary.log` files are extracted from the history file. This
can be used for the purpose of merging existing history files. The
`SourceLogFile` may either be a local filename or an URL. If it is an
URL both the history file and the summary log files are fetched over
the network. The resulting history file will then contain URL's,
implying that it may relocated without getting dangling links.

The `SourceLogFile` may be prefixed with a suite name, like
`SuitePrefix::SourceLogFile`. I that case the `SuitePrefix` will
override the `--suite` parameter setting from the original run. This
may be useful when a suite has been reused and thus run several
times. For example when there are several versions of the system under
test:

    lux --history . debug::PathToDebugLogDir release::PathToReleaseLogDir
<a name="config_params"/>

Configuration parameters
========================

    lux [--mode Mode] [ConfigParam]... [File]...

Exit status is 0 if all test cases are successful and 1 otherwise.

Configuration parameters can be given as command line options, as
`[config Var=Value]` statements in a script or in a **architecture
specific file**.

An `architecture specific file` is a file with configuration
statements only valid for a certain architecture/platform/system.
The syntax of such a file is the same a normal Lux script, but only
configuration settings (`[config Var=Value]`) are extracted. See
also the configuration parameters `--config_name` and `--config_dir`.
The file extension is `.luxcfg`.

When a test suite (one or more test cases) is to be evaluated, the Lux
engine will determine the software/hardware signature of the system to
construct the name of a architecture specific file. If a file with
that name exists, the architecture specific configuration settings
will be extracted and used as base for the entire test suite. These
settings can however be overridden by command line options and
`[config var=val]` statements in each test case file.

The Lux engine evaluates one or more Lux files. Lux files has normally
`.lux` as extension. See the configuration parameter `--file_pattern`.
If a directory is given as input, all `.lux` files in that directory
and its sub directories are evaluated. The given files (files or
directories) are called test suites and the derived files (actual Lux
scripts) are called test cases. The configuration parameter `--skip`
can be used to conditionally skip test cases.

Test case control
-----------------

**--mode Mode**  
Mode can be one of :

* `execute`  - evaluate the test cases. This is default.
* `validate` - parse all script files and configuration files and
               report syntax errors and warnings.
* `list`     - display a list of all (non-skipped) test cases.
               One file per line.
* `list_dir` - display a list of all directories with non-skipped
               test cases. One directory per line.
* `doc`      - extract all `[doc]` and `[docN]` strings and display
               them on a simple format which is as follows.
               First the script file name is printed on an own line
               ending with a colon, followed by all doc strings, each
               one on a separate line. The doc strings are indented
               with a tab char for each doc level. See [docN].

**--doc Level**  
Implies `--mode=doc`. Restricts how many documentation levels which
should be displayed. `--doc=1` only shows documentation on level 1,
`--doc=2` shows documentation both on level 1 and 2.

The first `[doc]` documentation string in a script is a bit special as
it is regarded as a one line summary of the script. With `--doc=0`
only the oneline summary lines are displayed.

**--rerun Result**  
Rerun old test cases. The test case candidates are found by parsing
old log summary files. If any `File` is explicitly given on command
line these files are interpreted as log directories possibly
containing summary log files. If no `File` is given the log directory
referred to by the `latest_run` link is used.

For each found test case its result must have the same outcome or
higher (worse) than `Result`.`Result` is an enum whose names and
relative values are as follows:

    enable < success < skip < warning < fail < error < disable

For example `--rerun=fail` implies that all old test cases whose
outcome is fail or error will be rerun.

Default is `disabled`, which means that this behavior is disabled.

**-r**  
A shortcut for `--rerun=fail`.

**--file\_pattern**  
Specify file pattern for scripts to be executed when a directory is
given. Defaults to `.*.lux$`.

**--var**  
Overrides environment variable settings. Each entry must be of the
form `var=value`.

**--config_name ConfigName**  
Normally Lux figures out which system software/hardware it runs on,
but it can explicitly be overridden with the `ConfigName` option. The
`ConfigName` is used to read system architecture specific configuration
parameters from a file named `ConfigName.luxcfg`. By default `ConfigName`
 is obtained from `uname -sm` where `ConfigName` is set to `Kernel-Machine`.
This behavior can be overridden by adding a file named after the name of
the host (`hostname.luxcfg`) on the `ConfigDir` directory. 

**--config\_dir ConfigDir**  
A directory where architecture specific connfiguration files may
reside. The format of the architecture specific files a subset of the
script format. Only `[config var=value]` statements are extracted from
the architecture specific file. The config settings in the
architecture specific file may be overridden by config settings in the
script files. Config settings in script files may be overridden by
command line options. Architecture specific files are by default
located in the subdirectory called `priv` in the `Lux` application.

**--hostname Hostname**  
The `Hostname` overrides the hostname obtained from the operating
system. It may be useful when testing config settings of other
machines or faking the hostname in a test environment with multiple
equivalent slaves.

**--require Var**  
**--require Var=Value**  
Require the given variable to be set. The script will fail if
the variable not is set. This option can be used multiple times,
which means that all given Vars are required to be set.
Typically require is used to test on presence of environment
variables. `--require` is intended to be used as `[config require=Var]`
or `[config require=Var=Value]` statements within scripts. The
construction **Var=Value** is little more restrictive as it
requires the variable to be set to a certain value.

**--skip Var**  
**--skip Var=Value**  
Skip execution of the script if the given variable is set. This
option can be used multiple times, which means that it suffices
that one of the given `Var`s is set in order to skip the test
case. Typically `--skip` is used to test on presence of environment
variables. `--skip` is intended to be used as `[config skip=Var]`
or `[config skip=Var=Value]` statements within scripts. The
construction **Var=Value** is little more restrictive as it requires
that the variable is set to a certain value.

**--skip\_unless Var**  
**--skip\_unless Var=Value**  
Skip execution of the script if the given variable NOT is set. This
option can be used multiple times, which means that it suffices
that one of the given `Var`s NOT is set in order to skip the test
case. Typically `--skip_unless` is used to test on absence of
environment variables. `--skip_unless` is intended to be used as
`[config skip_unless=Var]` or `[config skip_unless=Var=Value]`
statements within scripts. The construction **Var=Val** is little more
restrictive as it requires that the variable is set to a certain
value.

**--unstable Var**  
**--unstable Var=Value**  
Mark a test case as unstable if the given variable is set. This
implies failures to be reported as warnings.The option can be used
multiple times, which means that it suffices that one of the given
`Var`s is set in order to mark the test case as unstable. Typically
`--unstable` is used to test on presence of environment
variables. `--unstable` is intended to be used as `[config
unstable=Var]` or `[config unstable=Var=Value]` statements within
scripts. The construction **Var=Value** is little more restrictive as
it requires that the variable is set to a certain value.

**--unstable\_unless Var**  
**--unstable\_unless Var=Value**  
Mark a test case as unstable if the given variable NOT is set. This
implies failures to be reported as warnings. The option can be used
multiple times, which means that it suffices that one of the given
`Var`s NOT is set in order to mark the test case as unstable.
Typically `--unstable_unless` is used to test on absence of
environment variables. `--unstable_unless` is intended to be used as
`[config unstable_unless=Var]` or `[config unstable_unless=Var=Value]`
statements within scripts. The construction **Var=Val** is little more
restrictive as it requires that the variable is set to a certain
value.

**--skip\_unstable**  
**--skip\_unstable=true**  
Skip unstable test cases. See `--unstable` and `--unstable_unless`.

**--skip\_skip**  
**--skip\_skip=true**  
Forces Lux to not care about `--skip` and `--skip_unless` settings.
Overrides `--skip_unstable`.

Log control
-----------

**--log\_dir LogDir**  
A directory where log files will be written. Default is `./lux_logs`.

**--html Html**  
The `Html` option controls whether the logs should be converted to
HTML or not. It is an enum denoting the outcome of the tests.
If the actual outcome is the same or higher than `Html` then the
logs will be converted. The possible outcome and their relative
values are as follows:

    validate < enable < success < skip < warning < fail < error < disable

Default is `enable`. `validate` behaves as `enable` but will also
perform validation of the generated HTML files. The logs can also be
converted to HTML manually later by using the command line option
`--annotate`.

**--tap LogFile**  
A file where [TAP][TAP] events should be written. The file names
`stdout` and `stderr` are specially handled. They causes the log events
to be written to standard output respective standard error. Multiple
"files" can be given. A log file named lux.tap will always be generated,
regardless of this option.

**-t**  
A shortcut for `--progress=silent --tap=stdout`.

**--junit**  
Generate a JUnit test report for the test run that can be used for example
by Jenkins to show test result using the JUnit plugin. The generated test
report will be named `lux_junit.xml`.

**--case_prefix CasePrefix**  
A prefix string which is prepended to the script names in the user
friendly log files (TAP and HTML). With this the log files can provide
the context for the test case(s), such as subsystem or test suite.

Timeouts
--------

**--timeout Timeout**  
The script expects the shell output to match given
[regular expression][]s. But the output must be received within a
given time limit. The `Timeout` specifies how long it will wait before
the script fails. The `Timeout` defaults to `10000` milli seconds
(`10` seconds). This `Timeout` can be overridden by the statement
`[timeout Timeout]` in the script itself.

**--cleanup\_timeout CleanupTimeout**  
When the script reaches the `[cleanup]` marker, the ordinary
`Timeout` will be set to `CleanupTimeout`. The `CleanupTimeout`
defaults to `100000` milli seconds (`100` seconds).

**--multiplier Multiplier**  
In order to be able to run the tests on very slow hardware,
the `Multiplier` can be used. Each time a timer is initiated
(except sleep) its value is multiplied with the `Multiplier`
value. `Multiplier` is an integer and defaults to `1000`. For
example, by setting the `Multiplier` to `2000` all timeouts will
be doubled. `--multiplier` is intended to be set in architecture
specific files to provide different settings on different systems.

**--suite\_timeout SuiteTimeout**  
If the duration of the execution exceeds the `SuiteTimeout`, it
is aborted. The `SuiteTimeout` defaults to `infinity`, but can
be any positive integer value in the unit of milli seconds.

**--case\_timeout CaseTimeout**  
If the the duration of a single test case exceeds the
`CaseTimeout`, it is aborted. It can be any positive integer
value in the unit of milli seconds or `infinity`. The default
is `300000` (5 minutes).

**--flush\_timeout FlushTimeout**  
An experimental timeout setting.
All output from a shell is buffered and matched against
[regular expression][]s. It can however explicitly be flushed by
the script. When this is done, the engine first waits a while
before it discards the output. How long it waits is controlled
by `FlushTimeout`. It defaults to `0`. If you want to experiment
with it, `1000` milli seconds (1 second) can be a resonable value.

**--poll\_timeout PollTimeout**  
An experimental timeout setting.
When the Lux engine receives output from a shell it will
wait in `PollTimeout` milli seconds for more output before it
tries to match it against any [regular expression][]s. It defaults
to `0`. If you want to experiment with it, `100` milli seconds
(1/10 second) can be a resonable value.

History control
---------------

**--suite Suite**  

The Suite is used for bookkeeping a name which later is used for
printing out the history of test runs. See the
 [command line option](#cmd_line_opts) `--history`.

**--run RunId**  
The `RunId` is used for bookkeeping a name which later is used for
printing out the history of test runs. See the
 [command line option](#cmd_line_opts) `--history`.

**--extend\_run**  
**--extend\_run=true**  
Combines two runs into one. The summary log of an earlier run is
extended with the outcome of the new run. `--log_dir` can be given
explicitly. If not, the symbolic `latest_run` link is used to find
a suitable log directory.

**--revision Revision**  
The `Revision` is used for bookkeeping a repository revision
(changeset) which later is used for printing out the history of test
runs. See the [command line option](#cmd_line_opts) `--history`.

**--hostname Hostname**  
The `Hostname`overrides the hostnames extracted from the log files.
It may for example be useful in a test environment where the test
runs are distributed over multiple equivalent slaves. See the
[command line option](#cmd_line_opts) `--history`.

**--html validate**
Performs validation of the generated HTML files.

<a name="debugging"/>

Debugging and tracing
---------------------

**--progress ProgressLevel**  
`ProgressLevel` can be one of `silent`, `summary`, `brief`, `doc`,
`compact` and `verbose`. It defaults to `brief` which means that
single characters are printed to stdout. `doc` is like `brief` but in
this mode doc strings are also printed on stdout. `compact` means that
an event trace is printed on stdout. It is the same event trace that
is written to the `event log`. verbose contains the same info as
compact but is more readable (the newlines are expanded). `summary`
means that no progress is printed. `silent` means that nothing is
printed. The `brief` characters have the following meanings:

       . - a new row in the script is being interpreted
       : - output is being received from a shell
       c - the normal cleanup marker
       C - the cleanup marker during premature termination
       z - is printed out each second while sleeping
       W - is printed out when a dynamic warning is issued
       ( - beginning of a macro, loop or an include file
       ) - end of a macro, loop or an include file
       ? - waiting for shell output. Preceded with lineno.

`[progress String]` can also be used to display progress info.

The `ProgressLevel` can also interactively be changed via the debugger.

**-c**  
A shortcut for `--progress=compact`.

**-v**  
A shortcut for `--progress=verbose`.

**-t**  
A shortcut for `--progress=silent --tap=stdout`.

**--debug**  
The debugger is always available (even without this flag) and waiting
for input on the `stdin` stream. With the `--debug` flag the debugger
is attached to the script before the first line is executed and
waiting for input. The command `attach` (`a` for short) attaches the
debugger to the script and pauses its execution. The command
`progress` (`p` for short) toggles the verbosity level between `brief`
and `verbose`. Use the debugger command `help` to get more info about
the available commands. See also the section [debugging and tracing](#debugging).

**-d**  
A shortcut for `--debug`.

**--debug\_file SavedFile**  
Loads the commands in the `SavedFile` before the first line in the
script is executed. See the debugger command `save` and `load` for
more info. The format of the `SavedFile` is very simple and may be
manually edited. For example `break` and `continue` may be convenient
commands to add to such a file.

Miscellaneous
-------------

**--newshell**  
In `--newshell` mode shells are created with the `[newshell Name]` command
and making another shell active is done with `[shell Name]`. That is the
`[shell Name]` command cannot be used to create new shells in newhell mode.

**--shell\_cmd Cmd**  
**--shell\_args Arg**  
These parameters controls which program that will be started when a
script starts a shell. By default **`/bin/sh -i`** is started as
`--shell_cmd` and `--shell_args` defaults to `/bin/sh` and `-i`
respectively. `--shell_args` is a bit special in how this parameter
is treated by Lux.

**--shell\_prompt\_cmd PromptCmd**  
**--shell\_prompt\_regexp PromptRegExp**  
When Lux starts a shell the prompt is set to **`SH-PROMPT:`** by
default. In Bourne shell, which is the default shell, the variable
`PS1` is used to set the prompt. This is obtained by using the command
`export PS1=SH-PROMPT:` followed by an explicit match of the prompt
using the regexp `^SH-PROMPT:`. This behavior can be overridden by
using `--shell_prompt_cmd` and `--shell_prompt_regexp` respectively
when using more exotic shells, such as the Bourne Again shell:

     [config shell_cmd=/bin/bash]
     [config shell_prompt_cmd=unset PROMPT_COMMAND; export PS1=SH-PROMPT:]

**--shell\_wrapper**  
**--shell\_wrapper \[Executable\]**  
In order to get the terminal settings to work properly in advanced
interactive cases such as tab completion etc., the shell needs to be
executed in a **pseudo terminal**. This is accomplished by using a
wrapper program setting up the terminal correctly. The arguments to
the wrapper program is the name of the shell program with its
arguments (for example `/bin/sh -i`, see also see `--shell_cmd` and
`--shell_args`). The wrapper is expected to first configure the
terminal and then start the shell.

The built-in executable `lux/priv/runpty` will be used by default as
shell wrapper (if it has been built properly).

It is also possible to use no shell wrapper at all by omitting the
`Executable` value (or simply set it to the empty string "").

**--post\_commit\_cmd**  
**--post\_commit\_cmd \[CleanupCmd\]**  
Enable centrally defined cleanup code to be run after those test cases
where the "normal" cleanup fails. The purpose of this is to make it
possible to report and possibly undo unwanted side effects which the
cleanup code have failed to handle. A workaround for sloppy written
test cases which may make a test suite more stable.

When a test case fails in its `cleanup`, a shell named `post_commit`
will be started, the `CleanupCmd` string will be sent to the shell and
the shell prompt will be waited for.

**--line\_term Chars**  
Specify the character sequence added to the end of lines sent to
a shell. It defaults to `\n`.
<a name="logs"/>

Logs
====

Lux will create a new directory for each test run. By default the log
files are generated under `./lux_logs/run_yyyy_mm_dd_hh_mm_ss_mmmmmm`
where `run_yyyy_mm_dd_hh_mm_ss_mmmmmm` is a unique directory name
generated from the current time. A symbolic link called
`./lux_logs/latest_run` will also be created. It refers to the newly
created log directory for the latest run. If the [configuration
parameter](#config_params) `--log_dir LogDir` is set, the given path
will be used instead and no symbolic link will be created.

Each test run will result in the following log files:

* **lux_summary.log** which contains information about the outcome of
  each test case and paths to test case logs.
* **lux_config.log** which contains all configuration settings common
    for the test suite.
* **lux_result.log** which contains a condensed summary of the outcome.
* **lux.tap** is a [TAP][TAP] compliant log file.

For each test case several logs are written:

* **$CASE.event.log** contains every internal event. Such as which
  statements that has been executed, output from shells etc.  This is
  the main source for information for detailed information about the
  outcome of a test case.
* **$CASE.config.log** contains configuration settings for the test case.
* **$CASE.$SHELL.stdin.log** contains the raw input to the
  shells. There is one such log per shell.
* **$CASE.$SHELL.stdout.log** contains the raw output from the
  shells. There is one such log per shell.
* **$CASE.orig** is a copy of the test script.

The summary log, result log, event logs and config logs are by default
processed and converted to HTML in order to make them easier to
read. This can be controlled with the `--html` [configuration
parameter](#config_params). The html logs are called
`lux_summary.log.html` and `$CASE.event.log.html` respectively.

The outcome of multiple test runs can be assembled in a **history
log**. This log is very useful when Lux is used in a daily build
environment and some test cases suddenly starts to fail. By using the
time line in the history log it can be possible to determine which
checkin to the repository that introduced the first failure. See the
`--history` [command line option](#cmd_line_opts). Its behavior can be
customized by using the `--suite`, `--run` and `--revision`
[configuration parameters](#config_params).
<a name="debug_cmds"/>

Debugger for Lux scripts
========================
When `lux` is started with the `--debug` option, the debugger
will attach to the script before its execution has started. An
optional file with saved commands may be processed at this stage.
The debugger can also be attached to the script in the middle of
the execution by entering the command "attach" (or an abbreviation
of the command) and pressing the enter key.

Several parameters has a lineno as parameter see `help lineno`.

Blank command lines implies that the previous command is repeated.
If no command has been entered yet, the command `help` is assumed.

Commands may be abbreviated. Use the help command (for example
`help help` (or `h h` for short) to get more detailed descriptions
of the commands.


Available commands: 
-------------------
* attach   - attach to script and pause its execution
* break    - set, delete and list breakpoints
* continue - continue script execution
* help     - display description of a command
* list     - list script source
* load     - load file with debug commands
* next     - execute next command
* progress - set verbosity level of progress
* quit     - quit a single test case or the entire test suite
* save     - save debug state to file
* skip     - skip execution of one or more commands
* shell    - connect to a shell
* tail     - display log files
* TRACE    - start or stop internal tracing

Available parameters:
---------------------
* lineno - lineno in source file


lineno parameter
----------------
Several commands has a lineno as parameter. It is a string which
is divided in several components. The components are separated
with a colon and are used to refer to line numbers in include
files, macros and loops. The first component is a bit special.
It may be a file name or a line number. The file name may be
abbreviated.

Assume that there is a file called main, which includes a file
called outer at line 4 and the file outer includes a file called
inner at line 12.

Here are a few examples of how lineno can be used:

* 3       - line 3 in current file
* main    - line 1 in file main
* m:3     - line 3 in file main
* :3      - line 3 in file main
* inner   - line 1 in file inner
* outer   - line 1 in file outer
* o:12    - line 12 in file outer
* 4:12:6  - line 6 in file inner if it is included
            on line 12 in outer and outer is included
            on line 4 in main.



TRACE \[action\] \[mode\]
-------------------------

Start or stop internal tracing
Default is to display the trace mode (none|case|suite|event).

**Parameters:**  

* action - Trace action; enum(START|STOP)  
* mode   - Trace mode; enum(CASE|SUITE|EVENT)  

attach
------

Attach to script and pause its execution

**Parameters:**  

* no parameters


break \[lineno\] \[duration\]
-----------------------------

Set, delete and list breakpoints

When a breakpoint is set it may either be normal (default)
or temporary. The difference between them is that normal
breakpoints remains after the break has been triggered,
while temporary breakpoints are automatically deleted when
they have been triggered once. delete is used to immediately
remove the breakpoint.

Without parameters, all breakpoints are listed.


**Parameters:**  

* lineno   - lineno in source file; lineno  
* duration - controls the duration of the breakpoint; enum(normal|temporary|delete|skip)  

continue \[lineno\]
-------------------

Continue script execution

**Parameters:**  

* lineno - run to temporary breakpoint at lineno; lineno  

help \[command\]
----------------

Display description of a command

**Parameters:**  

* command - debugger command; string  

list \[n_lines\] \[lineno\]
---------------------------

List script source

If no "lineno" is given, the listing will start from the
current line or from the latest given "lineno" if no other
commands have been given in between.

**Parameters:**  

* n_lines - number of lines; 1 >= integer =< infinity  
* lineno  - start listing at lineno; lineno  

load \[file\]
-------------

Load file with debug commands

**Parameters:**  

* file - file name. Default is "./lux.debug".; string  

next
----

Execute next command
A multi-line command counts as one command.

**Parameters:**  

* no parameters


progress \[level\]
------------------

Set verbosity level of progress

**Parameters:**  

* level - verbosity level. Toggle between brief and verbose by default.; enum(silent|summary|brief|doc|compact|verbose|etrace|ctrace)  

quit \[scope\]
--------------

Quit a single test case or the entire test suite
in a controlled manner. Runs cleanup if applicable.

**Parameters:**  

* scope - scope of exit; enum(case|suite)  

save \[file\]
-------------

Save debug state to file

**Parameters:**  

* file - file name. Default is "lux.debug".; string  

shell \[name\] \[mode\]
-----------------------

Connect to a shell

With no argument, the names of the shells will be listed.
In the listing the active shell is preceeded by an arrow
and zombie shells with an star. Repeating the command
will disconnect the shell. Repeat again to connect...

Once a shell is connected, its stdout will be tapped and
subsequent output will be printed out, beginning with the
buffered (non-processed) data.

Data can also be sent to the stdin of the shell. In
foreground mode all entered text is sent as is to the
shell. The foreground mode is exited with a single """"
line. In background mode (default), the debugger responds
to normal debugger commands as well as a few special
commands which only is available in background mode:

Sub commands for "shell":
-------------------------
* ! - sends text with a trailing newline
* ~ - sends text without a trailing newline
* = - displays current output buffer
* ? - empties the output buffer


**Parameters:**  

* name - name of shell; string  
* mode - mode of operation; enum(background|foreground)  

skip \[lineno\]
---------------

Skip execution of one or more commands
Skip until given lineno is reached.

**Parameters:**  

* lineno - lineno in source file; lineno  

tail \[index\] \[format\] \[n_lines\]
-------------------------------------

Display log files

With no argument, the names of the log files will be listed.
Each one is preceeded by its index number and optionally a
star. The star means that the log has been updated since the
previous status check. Use the index to display a particular
log. Such as "t 5" for the event log. Press enter to
display more lines. n_lines can be used to override that
behavior andonly display a fixed number of lines regardless
of the command is repeated or not.

**Parameters:**  

* index   - log number; 1 >= integer =< infinity  
* format  - display format; enum(compact|verbose)  
* n_lines - fixed number of lines; 1 >= integer =< infinity  
<a name="examples"/>

Examples
========

A successful test case
----------------------

Here is an example of a test script. It starts couple of concurrent
shells, sends text to them with the `!` command and matches expected
output with `?`. The send and match operations are always performed in
context of the active shell.

Match operations search for a given pattern in the output stream. Once
a match is found, the preceding characters are skipped. There are a
few flavors of match operations. They may be single-line or
multi-line.  Evaluate regular expressions or verbatim. Variables can
be expanded or not. When using regular expressions variables can be
bound to parts of the output and used later in the test case.

There are different variable scopes. They may be accessible from all
shells (global), only accessible within the shell where they were set
(local), only accessible with in their lexical scope (my) such as
within a macro, loop etc.

When a test case has side effects, such as creating files, start
programs etc., the test case should have a `[cleanup]` section where
the side effect is reversed. Otherwise subsequent test cases may
fail. The cleanup section is always executed, regardless of the script
succeeds or fails.

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

A failing test case
-------------------

Test cases are executed until they succeed or fail. The script is
aborted at the first failure. Which may occur when the expected output
not has matched within the given timeout or when the failure pattern has
matched. A failure pattern is local to the given shell and will cause
abort when it matches.

The (match) timeout may explicitly be set within the script, in a
configuration file or given as a command line parameter. If the tests
are run both on fast machines and slow machines, it may be hard to set
an optimal timeout length. Then it may be appropriate to multiply the
timeout with different factors depending on the machine capabilities.
The config parameter called multiplier is used for this purpose. As
all configuration parameters, it can be given a host specific setting
or a architecture specific setting. Or even overridden on command
line.

As a test case may fail early or late in its execution the cleanup
code must cope with this. For example if a test case which normally
creates a file or starts a program fails before that point it must be
written in a manner so it does not fail during the cleanup.

Lux collects data in various logs. Such as an event log, one log per
shell for stdin and stdout etc. But sometimes this is not enough. For
example logs from the SUT (System Under Test). Such logs should be
stored under $LUX_EXTRA_LOGS. The $LUX_EXTRA_LOGS is a built-in
variable containing a suitable directory path. The SUT can either be
configured to store its logs there or the logs may be copied during
cleanup. In order to only copy the logs at failure the cleanup code
may test on the variable $LUX_START_REASON which also is built-in. It
is set to "fail" if the (cleanup) shell was started after a failure
has encountered. These extra logs are located under lux log directory
and it can (also) be reached via an HTML link from the annotated event
log.

Snippet from the enclosed `.../lux/examples/fail.lux` file:

>     [doc Demonstrate a failure]
>     
>     [global fail_pattern=[Ee][Rr][Rr][Oo][Rr]]
>     [global eprompt=\d+>\s]
>     
>     [doc2 Provoke a failure to get something interesting in the logs]
>     
>     [shell calculator]
>         -$fail_pattern|SH-PROMPT:
>         !erl
>     
>         # Multi-line expect
>         """?
>         Eshell.*
>         $eprompt
>         """
>     
>         # Multi-line send
>         """!
>         2+3.
>         6+7.
>         """
>     
>         # Ignore output between 5 and 13
>         ?5
>         ?13
>     
>         # Shorten the match timeout as we deliberately will demo a fail
>         [timeout 2]
>         !5+13.
>         # Next line will fail
>         ?19
>     
>     [cleanup]
>         # Save logs at fail
>         ~if [ "$LUX_START_REASON" = "fail" ]; then
>         ~  mkdir -p $LUX_EXTRA_LOGS/erl;
>         ~  cp -r ./logs/* $LUX_EXTRA_LOGS/erl;
>         !fi; true
>         ?SH-PROMPT:
>     

Warnings and avoiding failures
------------------------------

At startup lux imports all environment variables as global lux
variables. making them accessible vith `$var` syntax. Variables may
also be set in architecture or host specific configuration files. This
may be useful when certain test cases only can be run on some hosts
due to missing libraries, lack of memory etc.

Test cases may be skipped by testing of a certain variable is set at
all `[config skip=VAR]` or is set to a certain value `[config
skip=VAR=val]`.  The inverse is also possible, by using `[config
skip_unless=VAR]` and `[config skip_unless=VAR=val]` respectively.

During development when some test cases are unstable it is possible to
classify those as unstable. This implies that they are run but instead
of reporting a failed test case as failure it is reported as a
warning.

Here are few examples of a few config settings which may be useful in
a hetrogenous lab environment:

One config file `.../lux_config/Linux-i686.luxcfg`

>     [config var=MAKE=make]
>     [config var=USE_VALGRIND=true]
>     [config multiplier=1000]

and another `.../lux_config/SunOS-sun4u.luxcfg`

>     [config var=MAKE=gmake]
>     [config var=TEST_SUNOS=true]
>     [config var=SKIP_JAVA=true]
>     [config var=USE_VALGRIND=false]
>     [config multiplier=3000]

Snippet from the enclosed `.../lux/examples/require.lux` file:

>     [doc Demonstrate an error]
>     
>     [config require=MAKE]
>     
>     [shell setup]
>         !$MAKE start
>     
>     
>     [cleanup]
>         !$MAKE stop
>     

Snippet from the enclosed `.../lux/examples/warning.lux` file:

>     [doc Demonstrate a warning]
>     
>     [global foo=bar]   
>     

Snippet from the enclosed `.../lux/examples/skip.lux` file:

>     [doc Demonstrate a skipped test]
>     
>     [doc2 Show examples of config settings]
>     
>     [config skip=SKIP_JAVA]
>     [config skip_unless=TEST_SUNOS]
>     
>     [shell foo]
>         ?bar
>     

Snippet from the enclosed `.../lux/examples/unstable.lux` file:

>     [doc Demonstrate an unstable test]
>     
>     [config unstable_unless=TEST_DEVELOP]
>     
>     [shell foo]
>         [timeout 1]
>         ?bar
>     

Here follow the output from the enclosed example test suite under
`.../lux/examples`.

Evaluate `lux examples`

>     .../lux> lux examples
>     summary log       : /Users/hmattsso/dev/lux/lux_logs/run_2020_04_01_14_48_34_924562/lux_summary.log
>     test case         : examples/calc.lux
>     progress          : ..:..:.:..:...:..:.:....:..:.:..:..(....:..:.:.:.:...)(.:.:...)...:..:.:..:..(.:.:..:..)..(.:.:..:..)(....:.:..:...)(.:..:..)..(.:.:...)......:.:..........
>     result            : SUCCESS
>     test case         : examples/fail.lux
>     progress          : ..:..:.:..:...:..:.:.:...:.:.:.:....:.:...32C..:..:.:..:..:.:..:..:.:..:.:..:.:.:.:.:.:.:.:.:.:.:.:.:.
>     result            : FAIL at 32 in shell calculator
>     expected*
>     	19
>     actual match_timeout
>     	
>     	3> 5+13.
>     	18
>     	4> 
>     diff
>     	- 19
>     	+ 
>     	+ 3> 5+13.
>     	+ 18
>     	+ 4> 
>     	
>     test case         : examples/intro.lux
>     progress          : ..:..:.:..:..:.:.:.....:..:..:..:.:..:..:.:..:.:.:..:.:......:..:.:.:....c......:.:.:..:..:..:.:..:..:.:..
>     result            : SUCCESS
>     test case         : examples/loop.lux
>     progress          : ..:..:.:..:.((.:.:..:.)(.:..:.)(.:..:.))((.:.:..:.)(..:.:.)(.:..:.)(.:..:.)(.:..:.))((.:..:.)(.:..:.)(.:.:..:.)(.:..:.)(.:..:.)(.:..:.)(.:.:..:.)(.:..:.))...:..:.:..:..:.:..:..:.:..:...:..:.:..:.((.i=1..:.:..:.:..z)(z..i=2..:..:.:.:..z)(z..i=3..:..:.:.:..z)(:.z..i=4..:..:.:.):).c........:..:.:..:..:..:.:.
>     result            : SUCCESS
>     test case         : examples/loop_fail.lux
>     progress          : ..:..:.:..:.((.i=1..:.:...z)(z..i=2..:.:..:..z)(z..i=3..:..:..z))+5
>     result            : FAIL at 5 in shell break
>     expected*
>     	
>     actual Loop ended without match of break pattern "THIS WILL NEVER MATCH"
>     	
>     diff
>     	  
>     	
>     test case         : examples/require.lux
>     result            : FAIL as required variable MAKE is not set
>     test case         : examples/skip.lux
>     result            : SKIP as variable TEST_SUNOS is not set
>     test case         : examples/unstable.lux
>     progress          : ..:..:.:..:....7
>     warning           : 8: Fail but UNSTABLE as variable TEST_DEVELOP is not set
>     result            : WARNING at 7 in shell foo
>     expected*
>     	bar
>     actual match_timeout
>     	
>     diff
>     	- bar
>     	+ 
>     	
>     test case         : examples/warning.lux
>     progress          : W
>     warning           : 3: Trailing whitespaces
>     result            : WARNING
>     successful        : 3
>     skipped           : 1
>     	examples/skip.lux:6
>     warnings          : 2
>     	examples/unstable.lux:8 - Fail but UNSTABLE as variable TEST_DEVELOP is not set
>     	examples/warning.lux:3 - Trailing whitespaces
>     failed            : 3
>     	examples/fail.lux:32 - match_timeout
>     	examples/loop_fail.lux:5 - Loop ended without match of break pattern "THIS WILL NEVER MATCH"
>     	examples/require.lux:3 - FAIL as required variable MAKE is not set
>     summary           : FAIL
>     file:///Users/hmattsso/dev/lux/lux_logs/run_2020_04_01_14_48_34_924562/lux_summary.log.html
>     .../lux> echo $?
>     1

<a name="../INSTALL"/>

Installation
============

On MacOS, Lux can be installed with

>     brew tap hawk/homebrew-hawk
>     brew install lux

It will install Erlang and whatever else Lux needs.

Debian/Ubuntu

>     sudo apt-get install erlang erlang-dev
>     wget https://github.com/hawk/lux/releases/download/lux-2.1/lux_2.1-1.deb
>     sudo dpkg -i lux_2.1-1.deb

Arch Linux

>     mkdir tmp
>     cd tmp
>     wget https://github.com/hawk/lux/releases/download/lux-2.1/PKGBUILD
>     makepkg -si

TLDR;

Prerequisites
-------------

The following software is required:

* On BSD based systems, GNU Make is required.

* The tool **Lux** is implemented with **[Erlang/OTP][]** and its
  runtime system must be installed in order to build the tool. Install
  `Erlang/OTP` from [source][Erlang/OTP] or use [pre-built packages][]:

>     brew install erlang

* or

>     sudo apt-get install erlang erlang-dev

* By installing the `erlang` package most of the Erlang apps needed by
  Lux will be installed automatically. But on some systems there are
  additional Erlang packages which may be needed when using more
  exotic features, such as debugging and developing Lux itself:

  - `--internal_debug` requires `debugger`+`wx`
  - `--suite_trace`    requires `runtime_tools`
  - `--event_trace`    requires `runtime_tools`+`et`+`wx`
  - `--reltool`        requires `reltool`

* Building Lux using `--make` requires `tools`. (Avoid this kind of build.)

* Installation of Lux as standalone (using `--install`) requires `reltool`.

* Testing of Lux itself requires `tools`.

* `--history` require may `inets`. But only when the logs are referred
  to by using URL's. Using local log files does not require `inets`.

* The documentation is pre-built. Re-generation of the documentation
  requires **[Markdown][]**.

Instructions
------------

Lux can be `downloaded` from GitHub with

>     git clone git@github.com:hawk/lux.git
>     cd lux

Lux is built with

>     autoconf
>     ./configure
>     make

When this is done you have a system which can run Lux with

>     bin/lux <SOME PARAMS>

But you may also install Lux somewhere by using

>     make install

By default (that is when ./configure has been invoked without
parameters), Lux will be installed under /usr/local. It is effectively
the same as invoking

>     ./configure --prefix=/usr/local --exec_prefix=/usr/local --bindir=/usr/local/bin --sysconfdir=/usr/local/etc

`make install` does also accept various parameters which overrides the
ones given to `./configure`. Such as

>     make prefix=/usr/local exec_prefix=/usr/local bindir=/usr/local/bin sysconfdir=/usr/local/etc install

and those parameters may be combined with

>     make DESTDIR=/my/staging/area install

Standalone installation
-----------------------

When building Lux an Erlang/OTP system must be available.

By default that Erlang/OTP system is also used when running Lux.

But it is possible to perform an `standalone installation` of Lux
where the Lux installation is bundled with Erlang/OTP. This means that
you may in fact uninstall the Erlang/OTP system used for building Lux
and still be able to run Lux as it is self-contained with its own
Erlang/OTP runtime system. A standalone installation is performed with

>     mkdir -p <TARGETDIR>
>     bin/lux --install <TARGETDIR>

The installed standalone system may be re-located if needed.

Obscure platforms
-----------------

On "obscure platforms" which have `Erlang/OTP` but lacks `autotools`,
`make` etc. it may still possible to build Lux with

>     bin/lux --make

Re-build the documentation
--------------------------

Simply do

>     cd doc
>     make
>     open ../lux.html
<a name="../AUTHORS"/>

Original author
--------------

* Håkan Mattsson

Contributors
------------

* Jan Lindblad (implemented a predecessor to Lux, called Qmscript, as a Python plugin to QMTest)
* Sebastian Strollo (runpty)
* Martin Björklund (Emacs mode)
* Johan Bevemyr (LCS diff)
<a name="references"/>

References
==========

1. [Lux - LUcid eXpect scripting][Lux]  
2. [Expect homepage][Expect]  
3. [Erlang programming language][Erlang/OTP]  
4. [Pre-built Erlang packages][pre-built packages]  
5. [Erlang style regular expressions (re)][regular expression]  
6. [PCRE - Perl Compatible Regular Expressions][PCRE]  
7. [Erlang release management tool][Reltool]  
8. [Markdown][Markdown]  
9. [TAP - Test Anything Protocol][TAP]  

[Expect]:             http://www.nist.gov/el/msid/expect.cfm
                      "Expect homepage"
[Erlang/OTP]:         http://www.erlang.org/
                      "Erlang programming language"
[pre-built packages]:    https://www.erlang-solutions.com/downloads/download-erlang-otp
                      "Pre-built packages at Erlang Solutions"

[regular expression]: http://www.erlang.org/doc/man/re.html#regexp_syntax
                      "Erlang style regular expressions (re)"
[PCRE]:               http://www.pcre.org/
                      "PCRE - Perl Compatible Regular Expressions"
[Emacs]:              http://www.gnu.org/software/emacs
                      "Emacs text editor"
[Lux]:                https://github.com/hawk/lux
                      "LUcid eXpect scripting"
[Markdown]:           http://www.daringfireball.net/projects/markdown
                      "Markdown"
[Reltool]:            http://www.erlang.org/doc/apps/reltool/index.html
                      "Erlang release management tool"
[TAP]:                http://testanything.org/
                      "Test Anything Protocol"
