%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux).

-export([run/3, parse_file/5, interpret_commands/6, annotate_log/2, history/3]).
-export([trace_me/4, trace_me/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types

-include("lux.hrl").

-type filename() :: string().
-type dirname()  :: string().
-type opts()     :: [{atom(), term()}].
-type cmds()     :: [#cmd{}].
-type warnings() :: [{warning,string(),string(),string()}].
-type summary()  :: success | skip | warning | fail | error.
-type lineno()   :: string().
-type warning()  :: {warning, filename(), lineno(), string()}.
-type skip()     :: {skip, filename(), string()}.
-type error()    :: {error, filename(), string()}.
-type no_input() :: {error, undefined, no_input_files}.
-type result()   :: {ok, filename(), summary(), lineno(), [warning()]}.
-type run_mode() :: list | list_dir | doc | validate | execute.

-export_type([run_mode/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Run a test suite

-spec(run(filename(), opts(), [string()]) ->
             {ok, summary(), filename(), [result()]} | error() | no_input()).

run(File, Opts, OrigArgs) ->
    lux_suite:run(File, Opts, OrigArgs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse a script file

-spec(parse_file(filename(), run_mode(), boolean(), boolean(), opts()) ->
             {ok, filename(), cmds(), opts()} | skip() | error()).

parse_file(File, RunMode, SkipSkip, CheckDoc, Opts) ->
    lux_parse:parse_file(File, RunMode, SkipSkip, CheckDoc, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interpret parsed script

-spec(interpret_commands(filename(),
                         cmds(),
                         warnings(),
                         {non_neg_integer(),
                          non_neg_integer(),
                          non_neg_integer()},
                         opts(),
                         [{atom(), term()}]) ->
             [{ok, summary(), filename(), [result()]} | error()]).

interpret_commands(File, Cmds, Warnings, StartTime, Opts, Opaque) ->
    lux_case:interpret_commands(File, Cmds, Warnings, StartTime, Opts, Opaque).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Annotate log file(s)

-spec(annotate_log(filename(), opts()) ->
             ok | error()).

annotate_log(LogFile, Opts) ->
    lux_suite:annotate_log(true, LogFile, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Assemble result history

-spec(history(dirname(), filename(), opts()) ->
             ok | error()).

history(LogDir, HtmlFile, Opts) ->
    case lux_html_history:generate(LogDir, HtmlFile, Opts) of
        {ok, HtmlFile} ->
            lux_html_parse:validate_html(HtmlFile, Opts);
        {error, File, Reason} ->
            {error, File, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Enable simplified tracing and viewing it as a sequence chart
%% See et:trace_me/5

trace_me(DetailLevel, FromTo, Label, Contents) ->
    %% N.B External call
    ?MODULE:trace_me(DetailLevel, FromTo, FromTo, Label, Contents).

trace_me(_DetailLevel, _From, _To, _Label, _Contents) ->
    hopefully_traced.
