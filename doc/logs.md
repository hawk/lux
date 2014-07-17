Logs
====

Lux will create a new directory for each test run. By default the log
files are generated under `./lux_logs/run_yyyy_mm_dd_hh_mm_ss` where
`run_yyyy_mm_dd_hh_mm_ss` is a unique directory name generated from
the current time. A symbolic link called `./lux_logs/latest_run` will
also be created. It refers to the newly created log directory for the
latest run. If the [configuration parameter](#config_params)
`--log_dir LogDir` is set, the given path will be used instead and no
symbolic link will be created.

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
