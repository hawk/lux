Logs
====

Each test run will result in a **summary log** which contains
information about the outcome of each test case and paths to test case
logs. By default the log files are generated under `./lux_logs`.
See the [configuration parameter](#config_params) `--log_dir LogDir`.

For each test case several logs are written:

* **event log** contains every internal event. Such as which
  statements that has been executed, output from shells etc.  This is
  the main source for information about detailed information about the
  outcome of a test case.
* **config log** contains all configuration settings.
* **stdin log(s)** contains the raw input to the shells. There is one
  such log per shell.
* **stdout log(s)** contains the raw output from the shells. There is
  one such log per shell.

The summary log, event logs and config logs are by default processed
and converted to HTML in order to make them easier to read. This can
be controlled with the `--html` [configuration parameter](#config_params).

The outcome of multiple test runs can be assembled in a **history
log**. This log is very useful when Lux is used in a daily build
environment and some test cases suddenly starts to fail. By using the
time line in the history log it can be possible to determine which
checkin to the repository that introduced the first failure. See the
`--history` [command line option](#cmd_line_opts). Its behavior can be
customized by using the `--suite`, `--run` and `--revision`
[configuration parameters](#config_params).