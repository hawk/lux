%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2015 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defines

-define(stack(), try throw(1) catch _:_ -> erlang:get_stacktrace() end).
-define(APPLICATION, lux).
-define(TAG_WIDTH, 20).
-define(TAG(Tag), lux_utils:tag_prefix(Tag, ?TAG_WIDTH)).

-define(dmore, 10).

-record(cmd,
        {lineno :: non_neg_integer(),
         type   :: atom(),
         arg    :: term(),
         raw    :: binary()}).

-record(shell,
        {name   :: string(),
         pid    :: pid(),
         ref    :: reference(),
         health :: alive | zombie}).

-record(result,
        {outcome       :: fail | success | shutdown,
         name          :: string(),
         latest_cmd    :: #cmd{},
         cmd_stack     :: [{string(), non_neg_integer(), atom()}],
         expected      :: binary() | atom(),
         extra         :: undefined | atom() | binary(),
         actual        :: binary() | atom(),
         rest          :: binary() | atom(),
         events        :: [{non_neg_integer(),
                            atom(),
                            binary() | atom() | string()}]}).

-record(break,
        {pos  :: {string(), non_neg_integer()} | [non_neg_integer()],
         type :: temporary | next | enabled | disabled}).

-record(macro,
        {name :: string(),
         file :: string(),
         cmd  :: #cmd{}}).

-record(istate,
        {file                       :: string(),
         orig_file                  :: string(),
         mode = running             :: running | cleanup | stopping,
         loop_stack = []            :: [continue|break],
         cleanup_reason = normal    :: fail | success | normal,
         debug = false              :: boolean(),
         debug_file                 :: string(),
         skip = []                  :: [string()],
         skip_unless = []           :: [string()],
         require = []               :: [string()],
         config_dir = undefined     :: undefined | string(),
         progress = brief           :: silent | brief | doc | compact | verbose,
         log_dir = "lux_logs"       :: string(),
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
         timeout = 10*1000          :: non_neg_integer() | infinity,
         cleanup_timeout = 100*1000 :: non_neg_integer() | infinity,
         shell_wrapper              :: undefined | string(),
         shell_cmd = "/bin/sh"      :: string(),
         shell_args = ["-i"]        :: [string()],
         shell_prompt_cmd = "export PS1=SH-PROMPT:" :: string(),
         shell_prompt_regexp = "^SH-PROMPT:" :: string(),
         call_level= 1              :: non_neg_integer(),
         results = []               :: [#result{} | {'EXIT', term()}],
         active_pid                 :: undefined | pid(),
         active_name = "lux"        :: undefined | string(),
         blocked                    :: boolean(),
         has_been_blocked           :: boolean(),
         want_more                  :: boolean(),
         old_want_more              :: boolean(),
         debug_level = 0            :: non_neg_integer(),
         breakpoints = []           :: [#break{}],
         shells = []                :: [#shell{}],
         commands                   :: [#cmd{}],
         orig_commands              :: [#cmd{}],
         macros = []                :: [#macro{}],
         latest_cmd = #cmd{type = comment, lineno = 0, raw = <<>>}
                                    :: #cmd{},
         cmd_stack = []             :: [{string(), non_neg_integer(), atom()}],
         macro_dict = []            :: [string()],   % ["name=val"]
         dict = []                  :: [string()],   % ["name=val"]
         builtin_dict               :: [string()],   % ["name=val"]
         system_dict                :: [string()]}). % ["name=val"]

-record(run,
        {id,
         test,
         result,
         log,
         start_time,
         hostname,
         config_name,
         run_dir,
         repos_rev,
         details}).

 -define(DEFAULT_LOG, <<"unknown">>).
 -define(DEFAULT_HOSTNAME, <<"unknown">>).
 -define(DEFAULT_CONFIG_NAME, <<"unknown">>).
 -define(DEFAULT_SUITE, <<"unknown">>).
 -define(DEFAULT_RUN, <<"unknown">>).
 -define(DEFAULT_REV, <<"">>).
 -define(DEFAULT_TIME, <<"yyyy-mm-dd hh:mm:ss">>).
