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
>     summary log       : /Users/hmattsso/dev/lux/tutorial/chatty/test/intro/lux_logs/run_2022_06_27_19_40_56_509994/lux_summary.log
>     test case         : a_simple_server.lux
>     progress          : ..:...:.:.:.:..:.:..:.:.:.:.:..:.:....:.:.:..:.:.:..:..:.:..:.:..:.:....
>     result            : SUCCESS
>     test case         : async_startup_fail.lux
>     progress          : ..:...:.:.:.:...:.:.:.:.:.:.:.:.:.:....:.:.:..:.:..:.:..:..:.:.:..:.:.Will fail due to startup race cond.:.:.:..:.:.:.:.:.:.:.:.:.:.:.25????25..
>     result            : FAIL at line 25 in shell hawk
>     expected*
>     	Trying to join the mytopic chat room...
>     	Welcome to the chat room mytopic!a!!
>     	Enter text and press enter. Exit chat with \^d.
>     	
>     	hawk>
>     actual match_timeout
>     	erl -pa ../../../chatty/ebin -sname hawk -noshell -s chatty client myt opic
>     	Trying to join the mytopic chat room...
>     	<ERROR> Failed to join 'mytopic@HMATTSSO-M-74JD'. Is the server started?
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
>     	+ <ERROR> Failed to join 'mytopic@HMATTSSO-M-74JD'. Is the server started?
>     	+ {"init terminating in do_boot",shutdown}
>     	+ init terminating in do_boot (shutdown)
>     	+ SH-PROMPT:
>     	
>     test case         : sync_startup.lux
>     progress          : ..:...:.:.:.:..:..:.:.:.:.:.:.:.:.:.:.:.:.:..:.:.:.:....:..:..:.:..:..:.:.:....:.:..:..:.:..:.:..:.:.:.:.:.:.:.:.:.:.:.:....:...:.:.:..:.:..:.:.:.:.:.:.:.:.:.:.::......:.:.:.....:............
>     result            : SUCCESS
>     test case         : sync_startup_cleanup.lux
>     progress          : ()..:...:.:.:.:.:....:.:.:.:.:.:.:.:.:.:..:.:.:.:....:..:.:..:.:..:..:.:.:.:....:...:.:.:.:...:.:.:.:.:.:.:.:.:.:.:.:.:.:.:....:..:..:.:.:...:.:.:.:.:.:..:.:....:.:.:.::..c..........:..:..:.:.(..:.:.:.:.:.)(..:.:.:.:.)((.:..:.:.:.:.:.:.:.:.:.:.:.:.:.)(.:.:..:..))((..:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.)(.:.:..:..))
>     result            : SUCCESS
>     successful        : 3
>     failed            : 1
>     	async_startup_fail.lux:25 - match_timeout
>     summary           : FAIL
>     file:///Users/hmattsso/dev/lux/tutorial/chatty/test/intro/lux_logs/run_2022_06_27_19_40_56_509994/lux_summary.log.html
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
>     lrwxr-xr-x 1 hmattsso staff 30 Jun 27 21:40 lux_logs/latest_run -> run_2022_06_27_19_40_56_509994
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
>     summary log       : /Users/hmattsso/dev/lux/tutorial/chatty/test/intro/lux_logs/run_2022_06_27_19_41_21_142972/lux_summary.log
>     test case         : a_simple_server.lux
>     event log         : 0.8
>     /Users/hmattsso/dev/lux/tutorial/chatty/test/intro/a_simple_server.lux
>     21:41:21.276605 lux(0): start_time "2022-06-27 21:41:21.229784"
>     21:41:21.276924 lux(0): suite_timeout infinity
>     21:41:21.277050 lux(0): case_timeout 300000000 micros left (300 seconds * 1.000 multiplier)
>     21:41:21.279648 lux(1): doc "Demo a simple single shell test case"
>     21:41:21.284303 server(4): start "/Users/hmattsso/dev/lux/priv/bin/runpty /bin/sh -i"
>     21:41:21.285051 server(4): expected* ".+"
>     21:41:21.285051 server(4): timer started (10 seconds * 1.000 multiplier)
>     21:41:21.295985 server(4): recv "\e[?1034hsh-3.2$ "
>     21:41:21.296145 server(4): timer canceled (after 10925 microseconds)
>     21:41:21.296239 server(4): match "\e[?1034hsh-3.2$ "
>     21:41:21.296239 server(4): rest ""
>     21:41:21.296511 server(4): send "export PS1=SH-PROMPT:
>         "
>     21:41:21.296715 server(4): expected* "^SH-PROMPT:"
>     21:41:21.296715 server(4): timer started (10 seconds * 1.000 multiplier)
>     21:41:21.296907 server(4): recv "export PS1"
>     21:41:21.297045 server(4): recv "=SH-PROM"
>     21:41:21.297188 server(4): recv "PT:
>         "
>     21:41:21.297317 server(4): recv "SH-PROMPT:"
>     21:41:21.297423 server(4): timer canceled (after 571 microseconds)
>     21:41:21.297501 server(4): skip "export PS1=SH-PROMPT:
>         "
>     21:41:21.297501 server(4): match "SH-PROMPT:"
>     21:41:21.297501 server(4): rest ""
>     21:41:21.297771 server(6): send "erl -sname server -pa ../../../chatty/ebin
>         "
>     21:41:21.297990 server(6): recv "e"
>     21:41:21.298092 server(6): recv "rl"
>     21:41:21.298191 server(8): expected* "Erlang/OTP"
>     21:41:21.298191 server(8): timer started (10 seconds * 1.000 multiplier)
>     21:41:21.298331 server(8): recv " -sname server -pa "
>     21:41:21.298438 server(8): recv "../../"
>     21:41:21.298523 server(8): recv "../ch"
>     21:41:21.298593 server(8): recv "atty"
>     21:41:21.298685 server(8): recv "/ebin"
>     21:41:21.298769 server(8): recv "
>         "
>     21:41:21.574667 server(8): recv "Erlang/OTP 24 [erts-12.3.1] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [jit] [dtrace]
>         
>         "
>     21:41:21.574789 server(8): timer canceled (after 276496 microseconds)
>     21:41:21.574870 server(8): skip "erl -sname server -pa ../../../chatty/ebin
>         "
>     21:41:21.574870 server(8): match "Erlang/OTP"
>     21:41:21.574870 server(8): rest " 24 [erts-12.3.1] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [jit] [dtrace]
>         
>         "
>     21:41:21.575228 server(9): expected* "Eshell"
>     21:41:21.575228 server(9): timer started (10 seconds * 1.000 multiplier)
>     21:41:21.696138 server(9): recv "Eshell V12.3.1  (abort with ^G)
>         (server@HMATTSSO-M-74JD)1> "
>     21:41:21.696316 server(9): timer canceled (after 120975 microseconds)
>     21:41:21.696480 server(9): skip " 24 [erts-12.3.1] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [jit] [dtrace]
>         
>         "
>     21:41:21.696480 server(9): match "Eshell"
>     21:41:21.696480 server(9): rest " V12.3.1  (abort with ^G)
>         (server@HMATTSSO-M-74JD)1> "
>     21:41:21.696850 server(10): expected* "> "
>     21:41:21.696850 server(10): timer started (10 seconds * 1.000 multiplier)
>     21:41:21.697041 server(10): timer canceled (after 8 microseconds)
>     21:41:21.697146 server(10): skip " V12.3.1  (abort with ^G)
>         (server@HMATTSSO-M-74JD)1"
>     21:41:21.697146 server(10): match "> "
>     21:41:21.697146 server(10): rest ""
>     21:41:21.697509 server(12): send "chatty:server().
>         "
>     21:41:21.697713 server(13): expected* "Starting server"
>     21:41:21.697713 server(13): timer started (10 seconds * 1.000 multiplier)
>     21:41:21.719395 server(13): recv "cha"
>     21:41:21.719581 server(13): recv "tty:server().
>         "
>     21:41:21.725196 server(13): recv "Starting server server...
>         "
>     21:41:21.725304 server(13): timer canceled (after 27456 microseconds)
>     21:41:21.725423 server(13): skip "chatty:server().
>         "
>     21:41:21.725423 server(13): match "Starting server"
>     21:41:21.725423 server(13): rest " server...
>         "
>     21:41:21.725737 server(14): expected* "> "
>     21:41:21.725737 server(14): timer started (10 seconds * 1.000 multiplier)
>     21:41:24.726659 server(14): recv "Trying to open log file chatty_server.log..."
>     21:41:24.731218 server(14): recv "ok.
>         "
>     21:41:24.731442 server(14): recv "<0.87.0>
>         (server@HMATTSSO-M-74JD)2> "
>     21:41:24.731635 server(14): timer canceled (after 3005772 microseconds)
>     21:41:24.731790 server(14): skip " server...
>         Trying to open log file chatty_server.log...ok.
>         <0.87.0>
>         (server@HMATTSSO-M-74JD)2"
>     21:41:24.731790 server(14): match "> "
>     21:41:24.731790 server(14): rest ""
>     21:41:24.732272 server(16): send "halt(3).
>         "
>     21:41:24.732629 server(17): expected* "SH-PROMPT:"
>     21:41:24.732629 server(17): timer started (10 seconds * 1.000 multiplier)
>     21:41:24.732921 server(17): recv "halt(3).
>         "
>     21:41:24.737803 server(17): recv "SH-PROMPT:"
>     21:41:24.737974 server(17): timer canceled (after 5137 microseconds)
>     21:41:24.738088 server(17): skip "halt(3).
>         "
>     21:41:24.738088 server(17): match "SH-PROMPT:"
>     21:41:24.738088 server(17): rest ""
>     21:41:24.738467 server(19): send "echo "===$?==="
>         "
>     21:41:24.738768 server(19): recv "ec"
>     21:41:24.738987 server(19): recv "ho "===$?==="
>     21:41:24.739133 server(20): expected* "===3==="
>     21:41:24.739133 server(20): timer started (10 seconds * 1.000 multiplier)
>     21:41:24.739344 server(20): recv ""
>         ===3===
>         SH-PROMPT:"
>     21:41:24.739453 server(20): timer canceled (after 160 microseconds)
>     21:41:24.739541 server(20): skip "echo "===$?==="
>         "
>     21:41:24.739541 server(20): match "===3==="
>     21:41:24.739541 server(20): rest "
>         SH-PROMPT:"
>     21:41:24.739753 server(21): expected* "SH-PROMPT:"
>     21:41:24.739753 server(21): timer started (10 seconds * 1.000 multiplier)
>     21:41:24.739865 server(21): timer canceled (after 8 microseconds)
>     21:41:24.739939 server(21): skip "
>         "
>     21:41:24.739939 server(21): match "SH-PROMPT:"
>     21:41:24.739939 server(21): rest ""
>     21:41:24.740129 server(22): no_cleanup
>     21:41:24.750479 server(22): inactivate after zombify
>     21:41:24.761906 server(22): stop shutdown
>     21:41:24.761906 server(22): where "22"
>     21:41:24.761906 server(22): stack "a_simple_server.lux:22" no_cleanup 
>     21:41:24.762584 lux(0): case_timeout 296514000 micros left (296 seconds * 1.000 multiplier)
>     21:41:24.762834 lux(0): suite_timeout infinity
>     21:41:24.763034 lux(0): end_time "2022-06-27 21:41:24.763022"
>     result            : SUCCESS
>     successful        : 1
>     summary           : SUCCESS
>     file:///Users/hmattsso/dev/lux/tutorial/chatty/test/intro/lux_logs/run_2022_06_27_19_41_21_142972/lux_summary.log.html
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
>     Erlang/OTP 24 [erts-12.3.1] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [jit] [dtrace]
>     
>     Eshell V12.3.1  (abort with ^G)
>     (server@HMATTSSO-M-74JD)1> chatty:server().
>     Starting server server...
>     Trying to open log file chatty_server.log...ok.
>     <0.87.0>
>     (server@HMATTSSO-M-74JD)2> halt(3).
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
>     ..:..:.:..:..:.:..:.:.:..:.:....:.:.:..:.:.:.
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
>     shell server
>     22: 
>     
>     Connect to shell "server" in background mode.
>     !im().
>     
>     Send data to shell "server".
>     
>     server(recv): im().
>     server(recv): 
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
>     Last 10 (65..74) lines of log file: a_simple_server.lux.event.log
>     
>     21:41:29.433328 server(14): recv "ok.\r\n<0.87.0>\r\n"
>     21:41:29.433638 server(14): recv "(server@HMATTSSO-M-74JD)2> "
>     21:41:29.433835 server(14): timer canceled (after 3004863 microseconds)
>     21:41:29.433956 server(14): skip " server...\r\nTrying to open log file chatty_server.log...ok.\r\n<0.87.0>\r\n(server@HMATTSSO-M-74JD)2"
>     21:41:29.433956 server(14): match "> "
>     21:41:29.433956 server(14): rest ""
>     21:41:29.441252 server(14): send "im().\n"
>     21:41:29.942268 server(14): recv "im().\r\n"
>     21:41:29.954063 server(14): reset "7 bytes wasted"
>     21:41:29.954063 server(14): output reset 7 bytes
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
>     server(recv): <0.90.0>
>     server(recv): 
>     
>     server(recv): (server@HMATTSSO-M-74JD)3> halt(3).
>     server(recv): 
>     
>     server(recv): SH-PROMPT:
>     
>     server(recv): e
>     
>     server(recv): cho "===$?==="
>     server(recv): ===3===
>     server(recv): SH-PROMPT:
>     .
>     Cleanup. Turn existing shells into zombies.
>     
>     Disconnect from shell "server".
>     .
>     result            : SUCCESS
>     
>     successful        : 1
>     summary           : SUCCESS
>     
>     file:///Users/hmattsso/dev/lux/tutorial/chatty/test/intro/tmp_logs/lux_summary.log.html
>     SH-PROMPT:

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
milliseconds which is used internally.

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

>     [doc Demonstrate a skipped test]
>     
>     [config skip=PATH]
>     

Snippet from the enclosed `.../lux/tutorial/chatty/test/infra/unstable.lux` file:

>     [doc Demonstrate an unstable test which is run but do not clutter the results]
>     
>     [config unstable_unless=TEST_DEVELOP]
>     
>     [shell date]
>         # Ensure a quick fail if it fails
>         [timeout 2]
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
>     all build test history:
>     	@for d in $(LUXDIRS); do \
>     	   if test -f $$d/Makefile ; then \
>     	      $(MAKE) -C $$d $@ || exit $?; \
>     	   fi; \
>     	done
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

>     .PHONY: all build test history clean info
>     
>     TIMESTAMP=$(shell date +"%F_%T")
>     GITHASH=$(shell git rev-parse --verify --short HEAD)
>     TOPDIR=$(shell pwd | sed -e 's/tutorial.*/tutorial/')
>     
>     all: build test
>     
>     build:
>     
>     test:
>     	lux .
>     
>     history:
>     	lux --history . lux_logs
>     	@echo
>     	@echo open lux_history.html
>     
>     clean:
>     	rm -rf lux_logs history_demo*_logs erl_crash.dump *~
>     
>     info:
>     	@echo "TOPDIR=$(TOPDIR)"
>     	@echo "TIMESTAMP=$(TIMESTAMP)"
>     	@echo "GITHASH=$(GITHASH)"
>     
>     ############################################################
>     # Internal history demo targets
>     ############################################################
>     
>     .PHONY: history_demo history_demo_single history_demo_multi_host history_demo_multi_branch history_demo_success history_demo_warning history_demo_empty
>     
>     history_demo: history_demo_single history_demo_multi_host history_demo_multi_branch history_demo_success history_demo_warning history_demo_empty
>     	ls -1 history_demo_*/lux_history.html | sed -e 's/^/open /g'
>     
>     history_demo_single:
>     	rm -rf ${@}; \
>     	for i in 1 2 3 4 5 6 7 8 9; do \
>     	  opts="--revision=$(TIMESTAMP)_$$i_$(GITHASH) --suite=demo --config_dir=$(TOPDIR)/support/luxcfg"; \
>     	  lux $$opts --hostname=sunny  --config_name=SunOS-i86pc  --log_dir=${@}/run_logs/run_sunny_$$i .; \
>     	done; \
>     	lux --history ${@} ${@}/run_logs
>     
>     history_demo_multi_host:
>     	rm -rf ${@}; \
>     	for i in 1 2 3 4 5 6 7 8 9; do \
>     	  opts="--revision=$(TIMESTAMP)_$$i_$(GITHASH) --suite=demo --config_dir=$(TOPDIR)/support/luxcfg"; \
>     	  lux $$opts --hostname=sunny  --config_name=SunOS-i86pc   --log_dir=${@}/run_logs/run_sunny_$$i  .; \
>     	  lux $$opts --hostname=cloudy --config_name=SunOS-i86pc   --log_dir=${@}/run_logs/run_cloudy_$$i .; \
>     	  lux $$opts --hostname=netty  --config_name=NetBSD-macppc --log_dir=${@}/run_logs/run_netty_$$i  .; \
>     	done; \
>     	lux --history ${@} ${@}/run_logs
>     
>     history_demo_multi_branch:
>     	rm -rf ${@}; \
>     	branches="chatty-1.0 chatty-2.0"; \
>     	histargs=${@}; \
>     	for b in $$branches; do \
>     	  for i in 1 2 3 4 5 6 7 8 9; do \
>     	    opts="--revision=$(TIMESTAMP)_$$i_$(GITHASH) --suite=demo --config_dir=$(TOPDIR)/support/luxcfg"; \
>     	    lux $$opts --hostname=sunny  --config_name=SunOS-i86pc    --log_dir=${@}/run_logs/$$b/run_sunny_$$i  .; \
>     	    lux $$opts --hostname=cloudy --config_name=SunOS-i86pc    --log_dir=${@}/run_logs/$$b/run_cloudy_$$i .; \
>     	    lux $$opts --hostname=netty  --config_name=NetBSD-macppc  --log_dir=${@}/run_logs/$$b/run_netty_$$i  .; \
>     	  done; \
>     	  histargs="$$histargs $$b:::${@}/run_logs/$$b"; \
>     	done; \
>     	lux --history $$histargs
>     
>     history_demo_success:
>     	rm -rf ${@}; \
>     	for i in 1 2 3 4 5 6 7 8 9; do \
>     	  opts="--revision=$(TIMESTAMP)_$$i_$(GITHASH) --suite=demo --config_dir=$(TOPDIR)/support/luxcfg"; \
>     	  lux $$opts --hostname=sunny  --config_name=SunOS-i86pc  --log_dir=${@}/run_logs/run_sunny_$$i success.lux; \
>     	done; \
>     	lux --history ${@} ${@}/run_logs
>     
>     history_demo_warning:
>     	rm -rf ${@}; \
>     	for i in 1 2 3 4 5 6 7 8 9; do \
>     	  opts="--revision=$(TIMESTAMP)_$$i_$(GITHASH) --suite=demo --config_dir=$(TOPDIR)/support/luxcfg"; \
>     	  lux $$opts --hostname=sunny  --config_name=SunOS-i86pc  --log_dir=${@}/run_logs/run_sunny_$$i success.lux warning.lux; \
>     	done; \
>     	lux --history ${@} ${@}/run_logs
>     
>     history_demo_empty:
>     	rm -rf ${@}; \
>     	lux --history ${@} ${@}/run_logs
>     

Evaluate `cd tutorial/chatty/test/infra && make history_demo_multi_host`

Evaluate `cd tutorial/chatty/test/infra && rm -f history_demo_multi_host/lux_history*`

Evaluate `cd tutorial/chatty/test/infra && lux --history history_demo_multi_host history_demo_multi_host/run_logs`

>     .../lux> cd tutorial/chatty/test/infra && lux --history history_demo_multi_host history_demo_multi_host/run_logs
>     Cwd: /Users/hmattsso/dev/lux/tutorial/chatty/test/infra
>     Invoke: /Users/hmattsso/dev/lux/bin/lux --history history_demo_multi_host history_demo_multi_host/run_logs
>     Assembling history of logs from...
>     	history_demo_multi_host/run_logs ...........................
>     Wrote 2499 bytes in run cache to file history_demo_multi_host/lux_history.cache
>     Analyzed 27 test runs with 135 test cases (0 errors)...ok
>     file:///Users/hmattsso/dev/lux/tutorial/chatty/test/infra/history_demo_multi_host/lux_history.html
>     .../lux> echo $?
>     0


Walkthru history_demo_multi_host/lux_history.html

- Overview
    - Per architecture (config)
    - Per host
    - Still failing test cases

Evaluate `cd tutorial/chatty/test/infra && make history_demo_multi_branch`


Walkthru history_demo_multi_branch/lux_history.html

    - Compare branches

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
