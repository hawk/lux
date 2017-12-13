%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defines

-define(b2l(B), binary_to_list(B)).
-define(l2b(L), iolist_to_binary(L)).
-define(i2l(I), integer_to_list(I)).
-define(stacktrace(), try throw(1) catch _:_ -> erlang:get_stacktrace() end).
-define(APPLICATION, lux).
-define(TAG_WIDTH, 20).
-define(TAG(Tag), lux_utils:tag_prefix(Tag, ?TAG_WIDTH)).
-define(dmore, 10).
-define(RE_COMPILE_OPTS, [{newline,anycrlf}, multiline]).
-define(RE_RUN_OPTS,     [{newline,anycrlf}, notempty]).
-define(fail_pattern_matched, "fail pattern matched ").
-define(success_pattern_matched, "success pattern matched ").

-record(cmd,
        {lineno :: non_neg_integer(),
         type   :: atom(),
         arg    :: term(),
         orig   :: binary()}).

-record(shell,
        {name   :: string(),
         pid    :: pid(),
         ref    :: reference(),
         health :: alive | zombie,
         vars   :: [string()]}). % ["name=val"]

-record(cmd_pos,
        {rev_file   :: [string()],
         lineno     :: non_neg_integer(),
         type       :: atom()}).

-record(result,
        {outcome       :: fail | success | shutdown,
         name          :: string(),
         latest_cmd    :: #cmd{},
         cmd_stack     :: [{string(), non_neg_integer(), atom()}],
         expected_tag  :: 'expected=' | 'expected*',
         expected      :: binary() | atom(),
         extra         :: undefined | atom() | binary(),
         actual        :: binary() | atom(),
         rest          :: binary() | atom(),
         events        :: [{non_neg_integer(),
                            atom(),
                            binary() | atom() | string()}]}).

-record(break,
        {pos            :: {string(), non_neg_integer()} | [non_neg_integer()],
         invert = false :: boolean(),
         type           :: temporary  | next | skip | enabled | disabled}).

-record(macro,
        {name :: string(),
         file :: string(),
         cmd  :: #cmd{}}).

-record(loop,
        {mode :: iterate | break | #cmd{},
         cmd  :: #cmd{}}).

-record(debug_shell,
        {name :: string(),
         mode :: background | foreground,
         pid  :: pid()}).

-record(istate,
        {file                       :: string(),
         orig_file                  :: string(),
         mode = running             :: running | cleanup | stopping,
         warnings                   :: [{warning,string(),string(),string()}],
         loop_stack = []            :: [#loop{}],
         cleanup_reason = normal    :: fail | success | normal,
         debug = false              :: boolean(),
         debug_file                 :: string(),
         skip = []                  :: [string()],
         skip_unless = []           :: [string()],
         unstable = []              :: [string()],
         unstable_unless = []       :: [string()],
         require = []               :: [string()],
         case_prefix = ""           :: string(),
         config_dir = undefined     :: undefined | string(),
         progress = brief           :: silent | summary | brief |
                                       doc | compact | verbose,
         suite_log_dir = "lux_logs" :: string(),
         case_log_dir               :: string(),
         log_fun                    :: function(),
         config_log_fd              :: {true, file:io_device()},
         event_log_fd               :: {true, file:io_device()},
         summary_log_fd             :: file:io_device(),
         logs = []                  :: [{string(), string(), string()}],
         tail_status = []           :: [{string(), string()}],
         multiplier = 1000          :: non_neg_integer(),
         suite_timeout = infinity   :: non_neg_integer() | infinity,
         case_timeout = 5*60*1000   :: non_neg_integer() | infinity,
         flush_timeout = 0          :: non_neg_integer(),
         poll_timeout = 0           :: non_neg_integer(), % 100
         default_timeout = 10*1000  :: non_neg_integer() | infinity,
         cleanup_timeout = 100*1000 :: non_neg_integer() | infinity,
         shell_wrapper              :: undefined | string(),
         shell_cmd = "/bin/sh"      :: string(),
         shell_args = ["-i"]        :: [string()],
         shell_prompt_cmd = "export PS1=SH-PROMPT:" :: string(),
         shell_prompt_regexp = "^SH-PROMPT:" :: string(),
         call_level= 1              :: non_neg_integer(),
         results = []               :: [#result{} | {'EXIT', term()}],
         active_shell               :: undefined | #shell{},
         active_name = "lux"        :: undefined | string(),
         shells = []                :: [#shell{}],
         debug_shell                :: undefined | #debug_shell{},
         blocked                    :: boolean(),
         has_been_blocked           :: boolean(),
         want_more                  :: boolean(),
         old_want_more              :: boolean(),
         debug_level = 0            :: non_neg_integer(),
         breakpoints = []           :: [#break{}],
         commands                   :: [#cmd{}],
         orig_commands              :: [#cmd{}],
         macros = []                :: [#macro{}],
         cmd_stack = []             :: [{string(), non_neg_integer(), atom()}],
         submatch_vars = []         :: [string()],   % ["name=val"]
         macro_vars = []            :: [string()],   % ["name=val"]
         global_vars = []           :: [string()],   % ["name=val"]
         builtin_vars               :: [string()],   % ["name=val"]
         system_vars                :: [string()],   % ["name=val"]
         latest_cmd = #cmd{type = comment, lineno = 0, orig = <<>>}
                                    :: #cmd{},
         stopped_by_user            :: undefined | 'case' | suite}).


-record(run,
        {test         :: binary(),              % [prefix "::"] suite [":" case]
         result       :: success | warning | skip | fail,
         id           :: binary(),              % --run
         log          :: file:filename(),       % file rel to summary log dir
         start_time   :: binary(),
         branch       :: undefined | string(),
         hostname     :: binary(),              % $HOSTNAME or --hostname
         config_name  :: binary(),              % --config
         run_dir      :: file:filename(),       % current dir during run
         run_log_dir  :: file:dirname(),        % dir where logs was created
         new_log_dir  :: file:dirname(),        % top dir for new logs
         repos_rev    :: binary(),              % --revision
         details      :: [#run{}]}).            % list of cases

-record(source,
        {branch       :: undefined | string(),
         suite_prefix :: undefined | string(),
         file         :: file:filename(),       % relative to cwd
         dir          :: file:dirname(),        % relative to cwd
         orig         :: file:filename()}).

-record(event,
        {lineno  :: non_neg_integer(),
         shell   :: binary(),
         op      :: binary(),
         data    :: [binary()]}).

-record(body,
        {invoke_lineno :: integer(),
         first_lineno  :: non_neg_integer(),
         last_lineno   :: non_neg_integer(),
         file          :: file:filename(),
         events        :: [#event{}]}).

-record(timer,
        {match_lineno :: [integer()], % Reversed stack of lineno
         match_data   :: [binary()],
         send_lineno  :: [integer()], % Reversed stack of lineno
         send_data    :: [binary()],
         shell        :: binary(),                        % Name
         callstack    :: binary(),                        % Name->Name->Name
         macro        :: binary(),                        % Name
         max_time     :: infinity | non_neg_integer(),    % Micros
         status       :: expected | started | matched | failed,
         elapsed_time :: undefined | non_neg_integer()}). % Micros

 -define(DEFAULT_LOG, <<"unknown">>).
 -define(DEFAULT_HOSTNAME, <<"unknown">>).
 -define(DEFAULT_CONFIG_NAME, <<"unknown">>).
 -define(DEFAULT_SUITE, <<"unknown">>).
 -define(DEFAULT_RUN, <<"unknown">>).
 -define(DEFAULT_REV, <<"">>).
 -define(DEFAULT_TIME, <<"yyyy-mm-dd hh:mm:ss">>).
