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
* `list`     - display a list of all (non-skipped) test cases.
               One file per line.
* `list_dir` - display a list of all directories with non-skipped
               test cases. One directory per line.
* `doc`      - extract all `[doc]` strings and display them on a
               simple format. First the main file name is printed on
               an own line ending with a colon, followed by all doc
               strings, one on each line. The doc strings are indented
               with a tab char for each doc level.

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

**--hostname Hostname**  
The `Hostname`overrides the hostname obtained from the operating
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
case. Typically `--skip` is used to test on presence of environment
variables. `--skip_unless` is intended to be used as
`[config skip_unless=Var]` or `[config skip_unless=Var=Value]`
 statements within scripts. The construction **Var=Val** is little
more restrictive as it requires that the variable is set to a certain
value.

**--skip\_skip**  
**--skip\_skip=true**  
Forces Lux to not care about `--skip` and `--skip_unless` settings.

**--require Var**  
**--require Var=Value**  
Require that the given variable is set. The script will fail if
the variable not is set. This option can be used multiple times,
which means that all given Vars are required to be set.
Typically require is used to test on presence of environment
variables. `--require` is intended to be used as `[config require=Var]`
or `[config require=Var=Value]` statements within scripts. The
construction **Var=Value** is little more restrictive as it
requires that the variable is set to a certain value.

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

    enable < success < skip < warning < fail < error < disable

Default is `enable`. The logs can be converted to HTML manually by
using the command line option `--annotate`.

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
       c - the cleanup marker
       z - is printed out each second while sleeping
       ( - beginning of a macro or an include file
       ) - end of a macro or an include file
       ? - waiting for shell output. Can be preceded with lineno of potential error.

In addition to these characters, line number are printed when there is
a failure or a potential failure.

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
using `--shell_prompt_cmd` and `--shell_prompt_regex` respectively
when using more exotic shells.

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
