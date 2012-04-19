Configuration parameters
========================

    lux [--mode Mode] [ConfigParam]... [File]...

Exit status is 0 if all test cases are successful and 1 otherwise.

Configuration parameters can be given as command line options, as
`[config Var=Value]` statements in a script or in a **architecture
specific file**.

An `architecture specific file` is a file with configuration
statements that only are valid for a certain
architecture/platform/system. The format of such a file is a subset of
a normal Lux script. Only configuration settings (`[config Var=Value]`).
See also the configuration parameters `--config_name` and `--config_dir`.

When a test suite (one or more test cases) is to be evaluated, the Lux
engine will determine the software/hardware signature of the system to
construct the name of a architecture specifiv file. If such a file
exists, its configuration settings will be extracted and used as base
for the entire test suite. These settings can however be overridden by
command line options and configuration settings in each test case.

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

* `execute`  - evaluates the test cases. This is default.
* `validate` - parse all script files and configuration files and
               report syntax errors and warnings.
* `list`     - display a list of all test cases. One filename on
               each line.
* `doc`      - extract all `[doc]` strings and display them on a
               simple format. First the main file name is printed on
               an own line ending with a colon, followed by all doc
               strings, one on each line. The doc strings are indented
               with a tab char for each doc level.

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
is obtained from `uname -sm` where `ConfigName is set to `Kernel-Machine`.
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
      
**--skip Var**  
Skip execution of the script if the given variable is set. This
option can be used multiple times, which means that it suffices
that one of the given `Var`s is set in order to skip the test
case. Typically `--skip` is used to test on presence of environment
variables. `--skip` is intended to be used as `[config skip=Var]`
statements within scripts.

**--require Var**  
Require that the given variable is set. The script will fail if
the variable not is set. This option can be used multiple times,
which means that all given Vars are required to be set.
Typically require is used to test on presence of environment
variables. `--require` is intended to be used as `[config require=Var]`
statements within scripts.

Log control
-----------

**--log\_dir LogDir**  
A directory where log files will be written. Default is `./lux_logs`.

**--html Html**  
The `Html` option controls whether the logs should be converted to
HTML or not. It is an enum that denotes the outcome of the tests.
If the actual outcome is the same or higher than `Html` then the
logs will be converted. The possible outcome and their relative
values are as follows:

    success < skip < warning < fail < error < never

The logs can be converted to HTML manually by using the command line
option `--annotate`.

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
All output from a shell is buffered and matched against
[regular expression][]s. It can however explicitly be flushed by
the script. When this is done, the engine first waits a while
before it discards the output. How long it waits is controlled
by `FlushTimeout`. It defaults to `0`. If you want to experiment
with it, `1000` milli seconds (1 second) can be a resonable value.

**--poll\_timeout PollTimeout**  
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

**--revision Revision**  
The `Revision` is used for bookkeeping a repository revision
(changeset) which later is used for printing out the history of test
runs. See the [command line option](#cmd_line_opts) `--history`.

Debugging and tracing
---------------------

**--progress ProgressLevel**  
`ProgressLevel` can be one of `silent`, `brief`, `doc`, `compact` and
`verbose`. It defaults to `brief` which means that single characters
are printed to stdout. `doc` is like `brief` but in this mode doc
strings are also printed on stdout. `compact` means that an event
trace is printed on stdout. It is the same event trace that is written
to the `event log`. verbose contains the same info as compact but is
more readable (the newlines are expanded). `silent` means that no
progress at all is printed. The `brief` characters have the following
meanings:
   
       . - a new row in the script is being interpreted
       : - output is being received from a shell
       c - the cleanup marker
       z - is printed out each second while sleeping
       ( - beginning of a macro or an include file
       ) - end of a macro or an include file
       ? - preceded with line number of a potential failure

In addition to these characters, line number are printed when there is
a failure or a potential failure.

`[progress String]` can also be used to display progress info.

The `ProgressLevel` can also interactively be changed via the debugger.

**--debug**  
The debugger is always available (even without this flag) and waiting
for input on the `stdin` stream. With the `--debug` flag the debugger
is attached to the script before the first line is executed and
waiting for input. The command `attach` (`a` for short) attaches the
debugger to the script and pauses its execution. The command
`progress` (`p` for short) toggles the verbosity level between `brief`
and `verbose`. Use the debugger command `help` to get more info about
the available commands. See also the section [debugging and tracing](#debugging).

**--debug\_file SavedFile**  
Loads the commands in the `SavedFile` before the first line in the
script is executed. See the debugger command `save` and `load` for
more info. The format of the `SavedFile` is very simple and may be
manually edited. For example `break` and `continue` may be convenient
commands to add to such a file.

Miscellaneous
-------------

**--shell\_cmd Cmd**  
**--shell\_arg Arg**  

These parameters controls which program that will be started when a
script starts a shell. By default **`/bin/sh -i`** is started as
`--shell_cmd` and `--shell_arg` defaults to `/bin/sh` and `-i`
respectively.

**--shell\_wrapper \[Executable\]**  

In order to get the terminal settings to work properly in advanced
interactive cases such as tab completion etc., the shell needs to be
executed in a **pseudo terminal**. This can be accomplished by using a
wrapper program that sets up the terminal correctly. The wrapper
program takes the name of the shell program with arguments (by default
`/bin/sh -i`) as argument and is expected to first configure the
terminal and then start the shell.

The `lux/priv/runpty` is an `Executable` that handles such terminal
settings and it will be used by default (if it has been built
properly). It is however possible to use a custom wrapper program by
using the `--shell_wrapper` parameter.

It is also possible to use no shell wrapper at all by omitting the
`Executable` value (or simply set it to the empty string "").

**--line\_term Chars**  
Specify the character sequence added to the end of lines sent to
a shell. It defaults to `\n`.
