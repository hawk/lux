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
>     summary log       : /Users/hmattsso/dev/tailf/lux/tutorial/chatty/test/intro/lux_logs/run_2019_05_14_21_31_12_97352/lux_summary.log
>     test case         : a_simple_server.lux
>     progress          : ..:..:.:..:..:.:..:.:..:.:....:.:..:.:.:.:...:.:..:.:..:....
>     result            : SUCCESS
>     test case         : async_startup_fail.lux
>     progress          : ..:..:.:..:..:..:.:.:.:.:....:.:.:..:..:..:.:..:..:.:.Will fail due to startup race cond.:.:.:..:.:.:.:.:.:.:.25????25..
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
>     progress          : ..:..:.:..:..:..:.:.:.:..:.:.:.:....:..:..:..:..:.:....:..:..:..:.:..:.:.:.:.:....:..:.:..:..:.:..:.:.:.:.::......:..::.....:............
>     result            : SUCCESS
>     test case         : sync_startup_cleanup.lux
>     progress          : ()..:..:..:...:..:.:.:.:.:..:.:.:.:....:..:.:..:..:..:.:....:..:.:..:..:..:.:.:.:.:....:..:.:..:..:..:.:.:..:.:....:.:.:.::..c..........:..:.:..:.(.:.:..:.:.)(.:..:.:.)((.:..:.:.:.:.:.:.:.:.:.:.:.:.)(.:..:.:..))((.:..:.:.:.:.:.:.:.:.:.:.:.:.:.)(.:..:.:..))
>     result            : SUCCESS
>     successful        : 3
>     failed            : 1
>     	async_startup_fail.lux:25 - match_timeout
>     summary           : FAIL
>     file:///Users/hmattsso/dev/tailf/lux/tutorial/chatty/test/intro/lux_logs/run_2019_05_14_21_31_12_97352/lux_summary.log.html
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
>     lrwxr-xr-x 1 hmattsso staff 29 May 14 23:31 lux_logs/latest_run -> run_2019_05_14_21_31_12_97352
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
>     lux_logs/latest_run/Users/hmattsso/dev/tailf
>     lux_logs/latest_run/Users/hmattsso/dev/tailf/lux
>     lux_logs/latest_run/Users/hmattsso/dev/tailf/lux/tutorial
>     lux_logs/latest_run/Users/hmattsso/dev/tailf/lux/tutorial/support
>     lux_logs/latest_run/Users/hmattsso/dev/tailf/lux/tutorial/support/luxinc
>     lux_logs/latest_run/Users/hmattsso/dev/tailf/lux/tutorial/support/luxinc/macros.luxinc.orig
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

  - Summary log
  - Config log
  - Annotated summary log (HTML)

while others are per test case:

  - Event log
  - Extra logs
  - Config log
  - Statistics
  - TAP log
  - JUnit log
  - Annotated event log (HTML)

and yet some are per shell in the test case:

  - Shell stdin log(s)
  - Shell stdout log(s)

Debugging
---------

There are various ways of debugging test cases. The simplest way is to
use the `--progress=verbose` flag or `-v` for short:

>     lux -v a_simple_server.lux

Evaluate `cd tutorial/chatty/test/intro && lux -v a_simple_server.lux`

>     .../lux> cd tutorial/chatty/test/intro && lux -v a_simple_server.lux
>     summary log       : /Users/hmattsso/dev/tailf/lux/tutorial/chatty/test/intro/lux_logs/run_2019_05_14_21_31_36_62641/lux_summary.log
>     test case         : a_simple_server.lux
>     event log         : 0.3
>     /Users/hmattsso/dev/tailf/lux/tutorial/chatty/test/intro/a_simple_server.lux
>     lux(0): start_time "2019-05-14 23:31:36.150101"
>     lux(1): doc "Demo a simple single shell test case"
>     server(4): start "/Users/hmattsso/dev/tailf/lux/priv/bin/runpty /bin/sh -i"
>     server(4): expected* ".+"
>     server(4): timer started (10 seconds * 1.000 multiplier)
>     server(4): recv "\e[?1034hsh-3.2$ "
>     server(4): timer canceled (after 12137 micro seconds)
>     server(4): match "\e[?1034hsh-3.2$ "
>     server(4): send "export PS1=SH-PROMPT:
>         "
>     server(4): recv "export"
>     server(4): expected* "^SH-PROMPT:"
>     server(4): timer started (10 seconds * 1.000 multiplier)
>     server(4): recv " PS1=SH-PROMPT:
>         SH-PROMPT:"
>     server(4): timer canceled (after 140 micro seconds)
>     server(4): skip "export PS1=SH-PROMPT:
>         "
>     server(4): match "SH-PROMPT:"
>     server(6): send "erl -sname server -pa ../../../chatty/ebin
>         "
>     server(6): recv "erl"
>     server(6): recv " -sname server -pa ../."
>     server(8): expected* "Erlang/OTP"
>     server(8): timer started (10 seconds * 1.000 multiplier)
>     server(8): recv "./../chatty/ebin
>         "
>     server(8): recv "Erlang/OTP 21 [erts-10.3.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]
>         
>         "
>     server(8): timer canceled (after 163510 micro seconds)
>     server(8): skip "erl -sname server -pa ../../../chatty/ebin
>         "
>     server(8): match "Erlang/OTP"
>     server(9): expected* "Eshell"
>     server(9): timer started (10 seconds * 1.000 multiplier)
>     server(9): recv "Eshell V10.3.2  (abort with ^G)
>         "
>     server(9): timer canceled (after 119186 micro seconds)
>     server(9): skip " 21 [erts-10.3.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]
>         
>         "
>     server(9): match "Eshell"
>     server(9): recv "(server@HMATTSSO-M-N1P1)1> "
>     server(10): expected* "> "
>     server(10): timer started (10 seconds * 1.000 multiplier)
>     server(10): timer canceled (after 11 micro seconds)
>     server(10): skip " V10.3.2  (abort with ^G)
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
>     server(13): timer canceled (after 24800 micro seconds)
>     server(13): skip "chatty:server().
>         "
>     server(13): match "Starting server"
>     server(14): expected* "> "
>     server(14): timer started (10 seconds * 1.000 multiplier)
>     server(14): recv "Trying to open log file chatty_server.log..."
>     server(14): recv "ok.
>         "
>     server(14): recv "<0.86.0>
>         (server@HMATTSSO-M-N1P1)2> "
>     server(14): timer canceled (after 3003625 micro seconds)
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
>     server(17): timer canceled (after 5795 micro seconds)
>     server(17): skip "halt(3).
>         "
>     server(17): match "SH-PROMPT:"
>     server(19): send "echo "===$?==="
>         "
>     server(19): recv "echo "===$?=="
>     server(20): expected* "===3==="
>     server(20): timer started (10 seconds * 1.000 multiplier)
>     server(20): recv "="
>         ===3===
>         SH-PROMPT:"
>     server(20): timer canceled (after 178 micro seconds)
>     server(20): skip "echo "===$?==="
>         "
>     server(20): match "===3==="
>     server(21): expected* "SH-PROMPT:"
>     server(21): timer started (10 seconds * 1.000 multiplier)
>     server(21): timer canceled (after 7 micro seconds)
>     server(21): skip "
>         "
>     server(21): match "SH-PROMPT:"
>     server(22): no cleanup
>     server(22): inactivate zombify
>     server(22): end of script
>     server(22): stop success
>     lux(0): end_time "2019-05-14 23:31:39.519734"
>     result            : SUCCESS
>     successful        : 1
>     summary           : SUCCESS
>     file:///Users/hmattsso/dev/tailf/lux/tutorial/chatty/test/intro/lux_logs/run_2019_05_14_21_31_36_62641/lux_summary.log.html
>     .../lux> echo $?
>     0


The shell stdin log is also quite useful when trying to reproduce a
run of a test case.

  - Start multiple terminalks and create shells manually
  - Copy and paste from stdin logs to the shells

Evaluate `cd tutorial/chatty/test/intro && cat lux_logs/latest_run/a_simple_server.lux.eshell.stdin.log`

>     .../lux> cd tutorial/chatty/test/intro && cat lux_logs/latest_run/a_simple_server.lux.eshell.stdin.log
>     cat: lux_logs/latest_run/a_simple_server.lux.eshell.stdin.log: No such file or directory
>     .../lux> echo $?
>     1


Evaluate `cd tutorial/chatty/test/intro && cat lux_logs/latest_run/a_simple_server.lux.eshell.stdout.log`

>     .../lux> cd tutorial/chatty/test/intro && cat lux_logs/latest_run/a_simple_server.lux.eshell.stdout.log
>     cat: lux_logs/latest_run/a_simple_server.lux.eshell.stdout.log: No such file or directory
>     .../lux> echo $?
>     1


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
>     lux -d --log_dir=tmp_logs a_simple_server.lux
>     

Snippet from the enclosed `.../lux/tutorial/chatty/test/intro/lux_logs/latest_run/a_simple_server.delux.debug.stdout.log` file:

>     [?1034hsh-3.2$ export PS1=SH-PROMPT:
>     SH-PROMPT:lux -d --log_dir=tmp_logs a_simple_server.lux
>     ERROR: Failed to open logfile: /Users/hmattsso/dev/tailf/lux/tutorial/chatty/test/intro/tmp_logs/lux_summary.log -> file already exists
>     
>     
>     FATAL ERROR: /Users/hmattsso/dev/tailf/lux/tutorial/chatty/test/intro/tmp_logs/lux_summary.log:
>     	ERROR: Failed to open logfile: /Users/hmattsso/dev/tailf/lux/tutorial/chatty/test/intro/tmp_logs/lux_summary.log -> file already exists
>     
>     SH-PROMPT:

>     c 15
>     shell server
>     !im().
>     n
>     t
>     help quit
>     c

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
>     file:///Users/hmattsso/dev/tailf/lux/tutorial/chatty/test/infra/history_logs/lux_history.html
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
