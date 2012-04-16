Introduction
============

Lux (LUcid eXpect scripting) provides scripting with an
`Expect`-style execution of commands in a powerful but simple
fashion. See [Expect][] for more info about the origin.

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

Here is an example of script `(lux/examples/intro.lux)`
which starts a couple of concurrent shells and sends text to them with
the `!` command and matches expected output with `?`.

>     [doc Test of single and multi line regular expressions]
> 
>     # A global variable is accessible in all shells
>     [global file=removeme.txt]
> 
>     [shell single]
>         # The terminal echoes all input
>         !echo foo
>         ?foo
> 
>     [shell multi]
>         # bar is indented 4 characters
>         !echo "foo"      > $file
>         !echo "    bar" >> $file
>         !echo "fum"     >> $file
>         !cat $file
> 
>         # The first double quote char defines the
>         # first column of the multi line regexp
>         """?
>         foo
>             bar
>         fum
>         """
> 
>     # Let single be the active shell again
>     [shell single]
>         ?^foo
> 
>     # Cleanup is executed regardless of the script succeeds or fails
>     [cleanup]
>         # Match of command exit status. Observe the double dollar sign.
>         !rm -f $file
>         !echo ==$$?==
>         ?^==0==

How to run the script
---------------------

Run a single script like this:

>     /home/hm> lux lux/examples/intro.lux
>
>     summary log       : /home/hm/lux_logs/run_2012_03_22_12_44_42/lux_summary.log
> 
>     test case         : /home/hm/lux/examples/intro.lux
>     progress          : ..:..:....:.....:...:...:..:..:..:.....c......:...:...:..:....
>     result            : SUCCESS
> 
>     successful        : 1
>     summary           : SUCCESS
>
>     file:///home/hm/lux_logs/run_2012_03_22_12_44_42/lux_summary.log.html
>
>     /home/hm> 

In this run we got a (brief) progress report of the test case on
stdout and a link to a summary log containing (lots of) details.

How to assemble the history of multiple runs
--------------------------------------------

In a nightly build environment it might be difficult to pinpoint when
a certain test case/suite started to fail. This process is greatly
simplified by running `lux` with the `--history` option as it will
assemble all test results as a timeline (interleaved with changeset
identities if provided with `--revision`).

>     /home/hm> lux --revision svn_4711 --run jenkins_17 lux/examples
>     /home/hm> lux --revision svn_4712 --run jenkins_20 lux/examples/intro.lux
>     /home/hm> lux --revision svn_4712 --run jenkins_20 lux/examples/fail.lux
>     /home/hm> lux --revision svn_4715 --run jenkins_22 lux/examples
>     /home/hm> lux --history .
>     Assembling history of logs in /home/hm.......4 test runs...ok
>
>     file:///home/hm/lux_history.html

