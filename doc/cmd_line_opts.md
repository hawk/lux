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
