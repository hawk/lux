Lux - LUcid eXpect scripting
============================

Version 1.15.2 - 2017-03-06

* [Introduction](#../README)
* [Concepts](#main_concepts)
* [Script syntax](#script_syntax)
* [Command line options](#cmd_line_opts)
* [Configuration parameters](#config_params)
* [Logs](#logs)
* [Debugger for Lux scripts](#debug_cmds)
* [Examples](#examples)
* [Installation](#../INSTALL)
* [Original author:](#../AUTHORS)
* [References](#references)

<a name="../README"/>

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

Here is an example of a test script `(lux/examples/intro.lux)`. It
starts couple of concurrent shells and sends text to them with the
`!` command and matches expected output with `?`.

>     [doc Test of single and multi line regular expressions]
>
>     # A global variable is accessible in all shells
>     [global file=removeme.txt]
>
>     [shell single]
>         # The terminal echoes all input
>         !echo foo
>         ?echo foo
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
>     # Cleanup is always executed, regardless if the script succeeds or fails
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

Lines beginning with `"""Char` are **multi line quotes**. The quote
ends with the next line beginning with `"""`. The opening quote and
closing quote must be in the same column of the script. The char right
after the first `"""` determines how the multi line quote will be
interpreted. The char is interpreted as a statement just like any of
the single line statement characters (so it can be e.g. `?`, `!`, `~`,
`#`, `-`, etc).

When multi line quotes are indented the leading whitespaces are
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
shell. Adds a `LF` at the end of the string. `String` may contain
references to variables using `$Var` or `${Var}`.

**~String**  
Same as `!String`, but it does NOT add a `LF` at the end.

**?**  
Flush the output streams (`stdout`, `stderr`). Already received output
is discarded. Avoid this (mis)feature. At a first look it seems more
useful than it is. It often causes unexpected race patterns.

**?Regexp**  
An `expect` operation which waits for a string matching a [regular
expression][] to appear on the shell output (either `stdout` or
`stderr`). If no matching output does appear within the timeout
period, the test case is considered as failed. See the `--timeout`
option. See also the `--flush_timeout` and `--poll_timeout`
configuration parameters about customizing the `?` behavior.

**??Template**  
Like `?Regexp`, but is more restricted as all regular expression
keywords are ignored. Variables are still substituted.

**???Verbatim**  
Like `??Template`, but is more restricted as no variables are substituted.
That is the string is matched as is.

**?+Regexp**  
Like `?Regexp`, but has no immediate effect. It is used when the
order of the output is undeterministic. Assume a case where the
strings A, B and C occurs in the output but the order of them is
unknown. Then we need to match all permutations of the strings.
Such as ABC, ACB, BAC, BCA, CAB and CBA. It can be achieved by
the relatively simple regexp `?(ABC)|(ACB)|(BAC)|(BCA)|CAB)|(CBA)`.
But with larger regexps, possibly spanning multiple lines, it
can be quite complex to just write the regexps. Performing the
post mortem analyzis to determine which subpattern that is
matching which part of the output will be even worse. In the
following example `?+` is used to register a subpattern and `?`
evaluates the permutations of all subpatterns (including the one
specified with `?).

    ?+A
    ?+B
    ?C

will render matching of all permutatations of A, B and C. Note
the usage of `?`. `?+` is always used together with `?`. It is
`?` which triggers the evaluation.

**-**  
**-Regexp**  
Sets a failure pattern to a regular expression (see [regular
expression][]). It is typically used to match error messages. If the
given `Regexp` ever matches, the test case is considered to have
failed (no further processing of the script will be performed besides
the `cleanup`). If no `Regexp` is given, the old failure pattern is
reset (cleared).

In the active shell, the `Regexp` is tried on the output preceding
each successful match of expect expressions. The characters up to, but
not including, the (successful) match are tried against the failure
pattern. In non-active shells the `Regexp` is tried when the shell
produces new output.

**+**  
**+Regexp**  
Sets a success pattern to a regular expression (see [regular
expression][]). If the given `Regexp` ever matches, the test case is
considered a success (no further processing of the script will be
performed besides the `cleanup`). If no `Regexp` is given, the old
success pattern is reset (cleared).

In the active shell, the `Regexp` is tried on the output preceding
each successful match of expect expressions. The characters up to, but
not including, the (successful) match are tried against the success
pattern. In non-active shells the `Regexp` is tried when the shell
produces new output.

**@**  
**@Regexp**  
Sets a loop break pattern to a regular expression (see [regular
expression][]). This statement is only valid in loops. It is typically
used to match output from a poll like command. When the given `Regexp`
matches, the loop is immediately exited. The execution continues with
the first statement after the loop. The test case fails if the loop is
exited before the break pattern is matched. If no `Regexp` is given,
the break failure pattern is reset (cleared).

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

**\[shell\]**  
**\[shell Name\]**  
Switches to the named shell, to make it active. In case there is no
such shell started yet, a new shell named `Name` is created.  If
`Name` is missing, the active shell is deactivated. This implies
no shell to be activated.

By default a `/bin/sh` shell (Bourne shell) is started. See
the `--shell_wrapper`, `--shell_cmd` and `--shell_arg` configuration
parameters. The current working directory of a newly started shell is
the same as the dirname of the script file. The **environment
variable** `LUX_SHELLNAME` is set to `Name`. The shell prompt variable
`PS1` is set to `SH-PROMPT:` and the first printout of the prompt is
automatically matched in an expect like manner in order to ensure that
the shell is ready for input. The `Name` may contain variables. Shell
names beginning with `lux` and `cleanup` are reserved for internal
purposes. The **environment variable** `LUX_START_REASON` is
initially set to `normal`. See also `[cleanup]`.

**\[cleanup\]**  
is the cleanup marker. If the script is prematurely aborted due to
failure (or due to a matching success pattern) the remaining
statements in the file are normally skipped. But if the there is a
cleanup marker after the failing line (and this is the only
cleanup marker), the lines after the cleanup marker will also be
run in order to enable a controlled cleanup of leftovers. Such as
killing processes, removing files etc. When the cleanup marker is
evaluated, the running shells will be set into a non accessible mode
(**zombie mode**) and their failure and success patterns will be
reset (cleared). A brand new shell (called something beginning with
`cleanup`) will also be started. If the cleanup code causes a failure
the remaining statements (on that level) will be skipped.

Cleanup code in included files will always be run, even if the failure
occurred in the included file. This means that each file can take care
of its own failures. This does also apply on nested include files. On
the topmost level the automatically started shell will be called
`cleanup`, on the next level it is called `cleanup2`, on next level
`cleanup3` etc.

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
absolute path. `.luxinc` is preferred as file extension. If the included
file contains a `[cleanup]` marker, the statements after that will be
evaluated in order to clean up unwanted side effects. Variables in
`FileName` are expanded.

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
to the shell with `[shell $old]`. If the macro file contains a
`[cleanup]` marker, the statements after that will be evaluated in
order to clean up unwanted side effects.

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
same as a macro variable (defined with `my`). The `Item` list may
contain variables and these are expanded before the first iteration.
Items in the expanded list are separated with spaces. For example
`[loop colors blue red green]`. When iterating over a set of
consecutive integers, such as `[loop iter 4 5 6 7 8 9]`, this can be
written as a range expression, like `[loop iter 4..9]`. In the logs
the iteration counter is represented as a negative line number. For
example "8:-2:10" would mean line 10 in the second loop iteration
where the loop starts at line 8. By default the increment is 1. A
custom increment can also be set with the construct `from..to..incr`,
such as `[loop iter 4..9..2]`. This would be the same as `[loop iter 4
6 8]`. `[loop iter 9..4..2]` would be the same as `[loop iter 9 7 5]`.

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

###Builtin variables###

    _BS_      - backspace       (ASCII 8)
    _TAB_     - tab             (ASCII 9)
    _LF_      - line feed       (ASCII (10)
    _CR_      - carriage return (ASCII 13)
    _DEL_     - delete          (ASCII 127)
    _CTRL_A_  - control a       (ASCII 1)
    ...
    _CTRL_Z_  - control z       (ASCII 26)
    N         - where N is an integer refering to a captured substring

###Builtin environment variables###

    LUX_SHELLNAME    - name of active Lux shell
    LUX_START_REASON - reason for starting a shell (normal|fail|success)
    PS1              - shell prompt variable set by Lux

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
Multi line documentation, typically to be used first in the script.
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
    lux --history LogDir

**--annotate LogFile**  
Transforms textual log files into HTML format and annotates Lux script
code with log events. The generated HTML file will get the same name
as `LogFile` but with a `.html` extension added. See also the
[configuration parameter](#config_params) `--html`.

**--history LogDir**  
Generates an HTML file which summarizes the history of all test runs,
by analyzing the `lux_summary.log` files located under `LogDir`. All
sub directories not containing a `lux.skip` file will be searched. The
file will be generated on the `LogDir` directory and is called
`lux_history.html`. Its behavior can be customized by using the
`--suite`, `--run`, `--revision` and `--hostname`
[configuration parameters](#config_params).
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

**--skip\_skip**  
**--skip\_skip=true**  
Forces Lux to not care about `--skip` and `--skip_unless` settings.

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
`stdout` and `stdin` are specially handled. They causes the log events
to be written to standard output respective standard error. Multiple
"files" can be given.

**-t**  
A shortcut for `--progress=silent --tap=stdout`.

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
when using more exotic shells.

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

The builtin executable `lux/priv/runpty` will be used by default as
shell wrapper (if it has been built properly).

It is also possible to use no shell wrapper at all by omitting the
`Executable` value (or simply set it to the empty string "").

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
* tail     - display log files
* list     - list script source
* load     - load file with debug commands
* next     - execute next command. A multi-line command counts as one command.
* progress - set verbosity level of progress
* quit     - quit a single test case or the entire test suite in a controlled manner. Runs cleanup if applicable.
* save     - save debug state to file
* skip     - skip execution of one or more commands. A multi-line command counts as one command.


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
* duration - controls the duration of the breakpoint; enum(normal|temporary|delete)  

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

Execute next command. A multi-line command counts as one command.

**Parameters:**  

* no parameters


progress \[level\]
------------------

Set verbosity level of progress

**Parameters:**  

* level - verbosity level. Toggle between brief and verbose by default.; enum(silent|summary|brief|doc|compact|verbose)  

quit \[scope\]
--------------

Quit a single test case or the entire test suite in a controlled manner. Runs cleanup if applicable.

**Parameters:**  

* scope - scope of exit; enum(case|suite)  

save \[file\]
-------------

Save debug state to file

**Parameters:**  

* file - file name. Default is "lux.debug".; string  

skip \[n_commands\]
-------------------

Skip execution of one or more commands. A multi-line command counts as one command.

**Parameters:**  

* n_commands - number of commands; 1 >= integer =< infinity  

tail \[index\] \[format\] \[n_lines\]
-------------------------------------

Display log files

With no argument, the names of the log files will be listed.
Each one is preceeded by its index number and optionally a
star. Star means that the log has been updated since the
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

To be written...<a name="../INSTALL"/>

Installation
============

Prerequisites
-------------

The following software is required:

* The tool **Lux** is implemented with **[Erlang/OTP][]** and its
  runtime system must be installed in order to build the tool. Install
  `Erlang/OTP` from [source][Erlang/OTP] or use [pre-built packages][]:
>     sudo apt-get install erlang

  Once `Lux` has been installed, it will be self-contained and does
  not need a separate `Erlang/OTP` runtime system any more.

* The documentation is pre-built. Re-generation of the documentation
  requires **[Markdown][]**.

Instructions
------------

If you have cloned the source from `github.com` and want to build the
tool using `configure` and `make` there is no `configure` script. Then
you need to create it with

>     autoconf

Vanilla configure, build and install with

>     ./configure
>     make
>     make install

This will imply that **Lux** will be installed on `/usr/local/lux` and
that custom architecture configuration will be read from
`/usr/local/lux/lib/lux-$(VSN)/priv`.

Install on specific directory `/foo/bar` with

>     ./configure
>     make
>     DESTDIR=/foo/bar make install

alternatively

>     ./configure --prefix=/foo/bar
>     make
>     make install

Install on directory `/foo/bar` and read custom architecture
configuration from `/etc/lux` with

>     ./configure
>     make
>     DESTDIR=/foo/bar ETCDIR=/etc/lux make install

alternatively

>     ./configure --prefix=/foo/bar --sysconfdir=/etc/lux
>     make
>     make install

Obscure platforms
-----------------

On "obscure platforms" which have `Erlang/OTP` but lacks
`autotools`, make etc. it is still possible to build with

>     bin/lux --make

and install with

>     bin/lux --install DestDir

The given InstallDir will contain the lux tool as well as a stripped
Erlang runtime system. It is possible to move the entire standalone
system from InstallDir to another location without any
re-installation.

The standalone tool can be started with

>     DestDir/bin/lux

Re-build the documentation
--------------------------

Simply do

>     cd doc
>     make
>     open ../lux.html
<a name="../AUTHORS"/>

Original author:

* Håkan Mattsson

Contributors:

* Jan Lindblad (implemented a predecessor to Lux, called Qmscript, as a Python plugin to QMTest)
* Sebastian Strollo (runpty)
* Martin Björklund (Emacs mode)
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
                      "Prebuilt packages at Erlang Solutions"

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
