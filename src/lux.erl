%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2015 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux).

-export([run/2, parse_file/4, interpret_commands/3, annotate_log/2, history/3]).
-export([trace_me/4, trace_me/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types

-include("lux.hrl").

-type filename() :: string().
-type dirname()  :: string().
-type opts()     :: [{atom(), term()}].
-type cmds()     :: [#cmd{}].
-type summary()  :: success | skip | warning | fail | error.
-type lineno()   :: string().
-type warning()  :: {warning, filename(), lineno(), string()}.
-type skip()     :: {skip, filename(), string()}.
-type error()    :: {error, filename(), string()}.
-type result()   :: {ok, filename(), summary(), lineno(), [warning()]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Run a test suite

-spec(run(filename(), opts()) ->
             {ok, summary(), filename(), [result()]} | error()).

run(File, Opts) ->
    lux_suite:run(File, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse a script file

-spec(parse_file(filename(), run_mode(), boolean(), opts()) ->
             {ok, filename(), cmds(), opts()} | skip() | error()).

parse_file(File, RunMode, SkipSkip, Opts) ->
    lux_parse:parse_file(File, RunMode, SkipSkip,Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interpret parsed script

-spec(interpret_commands(filename(), cmds(), opts()) ->
             [{ok, summary(), filename(), [result()]} | error()]).

interpret_commands(File, Cmds, Opts) ->
    lux_interpret:interpret_commands(File, Cmds, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Annotate log file(s)

-spec(annotate_log(filename(), opts()) ->
             ok | error()).

annotate_log(LogFile, Opts) ->
    lux_html:annotate_log(true, LogFile, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Assemble result history

-spec(history(dirname(), filename(), opts()) ->
             ok | error()).

history(LogDir, HtmlFile, Opts) ->
    lux_html:history(LogDir, HtmlFile, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Enable simplified tracing and viewing it as a sequence chart
%% See et:trace_me/5

trace_me(DetailLevel, FromTo, Label, Contents) ->
    %% N.B External call
    ?MODULE:trace_me(DetailLevel, FromTo, FromTo, Label, Contents).

trace_me(_DetailLevel, _From, _To, _Label, _Contents) ->
    hopefully_traced.
