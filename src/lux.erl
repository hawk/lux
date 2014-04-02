%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2012 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux).

-export([run/2, parse_file/2, interpret_commands/3, annotate_log/1, history/2]).
-export([test/1]).

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

-spec(parse_file(filename(), opts()) ->
             {ok, filename(), cmds(), opts()} | error()).

parse_file(File, Opts) ->
    lux_parse:parse_file(File, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interpret parsed script

-spec(interpret_commands(filename(), cmds(), opts()) ->
             [{ok, summary(), filename(), [result()]} | error()]).

interpret_commands(File, Commands, Opts) ->
    lux_interpret:interpret_commands(File, Commands, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Annotate log file(s)

-spec(annotate_log(filename()) ->
             ok | error()).

annotate_log(LogFile) ->
    lux_html:annotate_log(true, LogFile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Assemble result history

-spec(history(dirname(), filename()) ->
             ok | error()).

history(LogDir, HtmlFile) ->
    lux_html:history(LogDir, HtmlFile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simple test which parses all .lux files

-spec(test(dirname()) ->
             [{ok, filename(), cmds(), opts()} | error()]).

test(Dir) ->
    Fun = fun(File, Acc) -> [lux_parse:parse_file(File, []) | Acc] end,
    filelib:fold_files(Dir, ".*\\" ++ ".lux" ++ [$$], true, Fun, []).
