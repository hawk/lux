%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2021 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defines

-define(TRACE_ME(DetailLevel, From, To, Label, Contents),
        lux:trace_me(DetailLevel, From, To, Label,
                     [{efile,?FILE,?LINE} | Contents])).
-define(TRACE_ME2(DetailLevel, FromTo, Label, Contents),
        ?TRACE_ME(DetailLevel, FromTo, FromTo, Label, Contents)).

-define(FF(Format, Args), io_lib:format(Format, Args)).
-define(b2l(B), binary_to_list(B)).
-define(b2i(B), binary_to_integer(B)).
-define(l2a(L), list_to_atom(L)).
-define(l2b(L), iolist_to_binary(L)).
-define(i2b(I), integer_to_binary(I)).
-define(i2l(I), integer_to_list(I)).
-define(a2l(A), atom_to_list(A)).
-define(a2b(A), ?l2b(?a2l(A))).
-define(APPLICATION, lux).
-define(ESCRIPT_MOD, lux_escript).
-define(TAG_WIDTH, 20).
-define(TAG(Tag), lux_utils:tag_prefix(Tag, ?TAG_WIDTH)).
-define(dmore, 10).
-define(RE_COMPILE_OPTS, [{newline,anycrlf}, multiline]).
-define(RE_RUN_OPTS,     [{newline,anycrlf}, notempty]).
-define(fail_pattern_matched, "fail pattern matched ").
-define(success_pattern_matched, "success pattern matched ").
-define(loop_break_matched, "loop break pattern matched ").
-define(loop_break_mismatch,"Loop ended without match of break pattern ").
-define(DEFAULT_RISKY_THRESHOLD, 0.85).
-define(DEFAULT_SLOPPY_THRESHOLD, 0.000000001).
-define(HISTORY_LOG_BASE, "lux_history").
-define(HTML_EXT, ".html").
-define(SUITE_SUMMARY_LOG, "lux_summary.log").
-define(SUITE_CONFIG_LOG, "lux_config.log").
-define(SUITE_RESULT_LOG, "lux_result.log").
-define(CASE_EVENT_LOG, ".event.log").
-define(CASE_CONFIG_LOG, ".config.log").
-define(CASE_EXTRA_LOGS, ".extra.logs").
-define(CASE_TAP_LOG, "lux.tap").

-define(DEFAULT_LOG, <<"unknown">>).
-define(DEFAULT_HOSTNAME, <<"no_host">>).
-define(DEFAULT_CONFIG_NAME, <<"no_config">>).
-define(DEFAULT_SUITE, <<"no_suite">>).
-define(DEFAULT_CASE, <<"no_case">>).
-define(DEFAULT_RUN, <<"no_runid">>).
-define(DEFAULT_REV, <<"">>).
-define(DEFAULT_TIME,     <<"yyyy-mm-dd hh:mm:ss">>).
-define(DEFAULT_TIME_STR,   "yyyy-mm-dd hh:mm:ss").
-define(ONE_SEC, 1000).
-define(ONE_MIN, (?ONE_SEC*60)).
-define(ONE_SEC_MICROS, (?ONE_SEC*1000)).

-define(EXPECTED_EQ, 'expected=').
-define(EXPECTED_RE, 'expected*').
-define(EXPECTED_OLD, 'expected').
-define(SPACE, $\ ).

-ifdef(OTP_RELEASE).
    -define(stacktrace(),
            fun() -> try throw(1) catch _:_:StAcK -> StAcK end end()).
    -define(CATCH_STACKTRACE(Class, Reason, Stacktrace),
            Class:Reason:Stacktrace ->
           ).
-else.
    -define(stacktrace(),
            try throw(1) catch _:_ -> erlang:get_stacktrace() end).
    -define(CATCH_STACKTRACE(Class, Reason, Stacktrace),
            Class:Reason ->
                Stacktrace = erlang:get_stacktrace(),
           ).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Records

-record(timer_ref,
        {ref     :: reference(),
         timeout :: infinity | non_neg_integer(),
         send_to :: pid(),
         msg     :: term()}).

-record(cmd,
        {lineno :: non_neg_integer(),
         type   :: atom(),
         arg    :: term(),
         orig   :: binary()}).

-record(shell,
        {name            :: string(),
         pid             :: pid(),
         ref             :: reference(),
         health          :: alive | zombie,
         vars            :: [string()],                     % ["name=val"]
         match_timeout   :: infinity  | non_neg_integer(),
         fail_pattern    :: undefined | binary(),
         success_pattern :: undefined | binary()}).

-record(cmd_pos,
        {rev_file   :: [string()],
         lineno     :: non_neg_integer(),
         type       :: atom(),
         name       :: string()}).

-record(warning,
        {file    :: filename(),
         lineno  :: lineno(),
         reason  :: binary()}).

-record(result,
        {outcome       :: fail | success | shutdown,
         mode          :: running | cleanup | stopping,
         shell_name    :: string(),
         latest_cmd    :: #cmd{},
         pos_stack     :: [#cmd_pos{}],
         expected_tag  :: expected_tag(),
         expected      :: binary() | atom(),
         extra         :: undefined | atom() | binary(),
         actual        :: binary() | atom(),
         rest          :: binary() | atom(),
         warnings      :: [#warning{}]}).

-record(break,
        {pos            :: {string(), non_neg_integer()} | [non_neg_integer()],
         invert = false :: boolean(),
         type           :: temporary  | next | skip | enabled | disabled}).

-record(macro,
        {name   :: string(),
         file   :: string(),
         lineno :: non_neg_integer(),
         cmd    :: #cmd{}}).

-record(loop,
        {mode :: iterate | break | #cmd{},
         cmd  :: #cmd{}}).

-record(debug_shell,
        {name :: string(),
         mode :: background | foreground,
         pid  :: pid()}).

-record(istate,
        {top_pid                    :: pid(),
         file                       :: string(),
         orig_file                  :: string(),
         mode = running             :: running | cleanup | stopping,
         fail_when_warning = false  :: boolean(),
         warnings                   :: [#warning{}],
         loop_stack = []            :: [#loop{}],
         cleanup_reason = normal    :: fail | success | normal,
         debug = false              :: boolean(),
         debug_file                 :: string(),
         debug_pid                  :: pid(),
         trace_mode                 :: none | suite | 'case' | event | progress,
         skip = []                  :: [string()],
         skip_unless = []           :: [string()],
         unstable = []              :: [string()],
         unstable_unless = []       :: [string()],
         skip_skip = false          :: boolean(),
         require = []               :: [string()],
         case_prefix = ""           :: string(),
         config_dir = undefined     :: undefined | string(),
         progress = brief           :: silent | summary | brief |
                                       doc | compact | verbose |
                                       etrace | ctrace,
         suite_log_dir = "lux_logs" :: string(),
         case_log_dir               :: string(),
         log_fun                    :: function(),
         config_log_fd              :: {true, file:io_device()},
         event_log_fd               :: {true, file:io_device()},
         summary_log_fd             :: file:io_device(),
         logs = []                  :: [{string(), string(), string()}],
         tail_status = []           :: [{string(), string()}],
         start_time                 :: erlang:timestamp(),
         emit_timestamp = false     :: boolean(),
         multiplier = ?ONE_SEC          :: non_neg_integer(),
         suite_timeout = infinity   :: non_neg_integer() | infinity,
         suite_timer_ref            :: #timer_ref{},
         case_timeout = 5*?ONE_MIN  :: non_neg_integer() | infinity,
         case_timer_ref             :: #timer_ref{},
         flush_timeout = 10         :: non_neg_integer(),
         poll_timeout = 0           :: non_neg_integer(), % Should be 100
         default_timeout = 10*?ONE_SEC  :: non_neg_integer() | infinity,
         cleanup_timeout = 100*?ONE_SEC :: non_neg_integer() | infinity,
         risky_threshold  = ?DEFAULT_RISKY_THRESHOLD  :: float() | infinity,
         sloppy_threshold = ?DEFAULT_SLOPPY_THRESHOLD :: float() | infinity,
         newshell = false           :: boolean(),
         shell_wrapper              :: undefined | string(),
         shell_cmd = "/bin/sh"      :: string(),
         shell_args = ["-i"]        :: [string()],
         shell_prompt_cmd = "export PS1=SH-PROMPT:" :: string(),
         shell_prompt_regexp = "^SH-PROMPT:" :: string(),
         post_cleanup_cmd           :: undefined | string(),
         call_level= 1              :: non_neg_integer(),
         results = []               :: [#result{} | {'EXIT', term()}],
         active_shell = no_shell    :: no_shell | #shell{},
         active_name = "lux"        :: undefined | string(),
         shells = []                :: [#shell{}],
         debug_shell = no_shell     :: no_shell | #debug_shell{},
         blocked                    :: boolean(),
         has_been_blocked           :: boolean(),
         want_more                  :: boolean(),
         old_want_more              :: boolean(),
         debug_level = 0            :: non_neg_integer(),
         breakpoints = []           :: [#break{}],
         commands                   :: [#cmd{}],
         orig_commands              :: [#cmd{}],
         macros = []                :: [#macro{}],
         pos_stack = []             :: [#cmd_pos{}],
         submatch_vars = []         :: [string()],   % ["name=val"]
         macro_vars = []            :: [string()],   % ["name=val"]
         global_vars = []           :: [string()],   % ["name=val"]
         builtin_vars               :: [string()],   % ["name=val"]
         system_vars                :: [string()],   % ["name=val"]
         latest_cmd = #cmd{type = comment, lineno = 0, orig = <<>>}
                                    :: #cmd{},
         stopped_by_user            :: undefined | 'case' | suite,
         escript_mod                :: atom()}).

-record(run,
        {test = ?DEFAULT_SUITE
                       :: binary(),             % [prefix "::"] suite [":" case]
         result = fail :: success | warning | skip | fail,
         warnings = undefined :: undefined | [binary()],
         id = ?DEFAULT_RUN
                      :: binary(),              % --run
         log = ?DEFAULT_LOG
                      :: binary(),              % file rel to summary log dir
         start_time = ?DEFAULT_TIME
                      :: binary(),
         branch       :: undefined | string(),
         hostname = ?DEFAULT_HOSTNAME
                      :: binary(),              % $HOSTNAME or --hostname
         config_name = ?DEFAULT_CONFIG_NAME
                      :: binary(),              % --config
         run_dir      :: binary(),              % current dir during run
         run_log_dir  :: binary(),              % rel dir where logs was created
         new_log_dir  :: binary(),              % rel top dir for new logs
         repos_rev = ?DEFAULT_REV
                      :: binary(),              % --revision
         runs = []    :: [#run{}]}).            % list of cases

-record(source,
        {branch       :: undefined | binary(),
         suite_prefix :: undefined | binary(),
         file         :: binary(),              % relative to cwd
         dir          :: binary(),              % relative to cwd
         orig         :: binary()}).

-record(event,
        {lineno    :: non_neg_integer(),
         shell     :: binary(),
         op        :: binary(),
         timestamp :: no_timestamp | binary(),
         quote     :: quote | plain,
         data      :: [binary()]}).

-record(body,
        {invoke_lineno :: integer(),
         first_lineno  :: non_neg_integer(),
         last_lineno   :: non_neg_integer(),
         file          :: file:filename(),
         events        :: [#event{}]}).

-record(timer,
        {send_lineno  :: [integer()],                 % Reversed stack of lineno
         send_data    :: [binary()],
         match_lineno :: [integer()],                 % Reversed stack of lineno
         match_data   :: [binary()],
         shell        :: binary(),                        % Name
         callstack    :: binary(),                        % Name->Name->Name
         macro        :: binary(),                        % Name
         max_time     :: infinity | non_neg_integer(),    % Micros
         status       :: expected | started | matched | failed,
         elapsed_time :: undefined | non_neg_integer()}). % Micros

-record(pattern,
        {cmd       :: #cmd{},
         pos_stack :: [#cmd_pos{}]}).

-record(cstate,
        {orig_file               :: string(),
         parent                  :: pid(),
         name                    :: string(),
         debug = disconnect      :: connect | disconnect,
         latest_cmd              :: #cmd{},
         pos_stack = []          :: [#cmd_pos{}],
         wait_for_expect         :: undefined | pid(),
         mode = resume           :: resume | suspend | stop,
         start_reason            :: fail | success | normal,
         progress                :: silent | summary | brief |
                                    doc | compact | verbose |
                                    etrace | ctrace,
         log_fun                 :: function(),
         log_prefix              :: string(),
         event_log_fd            :: {true, file:io_device()},
         stdin_log_fd            :: {false, file:io_device()},
         stdout_log_fd           :: {false, file:io_device()},
         emit_timestamp          :: boolean(),
         multiplier              :: non_neg_integer(),
         suite_timeout           :: non_neg_integer() | infinity,
         case_timeout            :: non_neg_integer() | infinity,
         flush_timeout           :: non_neg_integer(),
         poll_timeout            :: non_neg_integer(),
         match_timeout           :: non_neg_integer() | infinity,
         timer_ref               :: undefined | #timer_ref{},
         timer_started_at        :: undefined | erlang:timestamp(),
         wakeup_ref              :: undefined | #timer_ref{},
         risky_threshold         :: float() | infinity,
         sloppy_threshold        :: float() | infinity,
         shell_wrapper           :: undefined | string(),
         shell_cmd               :: string(),
         shell_args              :: [string()],
         shell_prompt_cmd        :: string(),
         shell_prompt_regexp     :: string(),
         port                    :: port(),
         waiting = false         :: boolean(),
         fail                    :: undefined | #pattern{},
         success                 :: undefined | #pattern{},
         loop_stack = []         :: [#loop{}],
         expected                :: undefined | #cmd{},
         pre_expected = []       :: [#cmd{}],
         actual = <<>>           :: binary(),
         state_changed = false   :: boolean(),
         timed_out = false       :: boolean(),
         idle_count = 0          :: non_neg_integer(),
         no_more_output = false  :: boolean(),
         exit_status             :: non_neg_integer(),
         posix_code              :: file:posix(),
         got_endshell = false    :: boolean(),
         debug_level = 0         :: non_neg_integer(),
         warnings = []           :: [#warning{}]}).

-record(rstate,
        {files                      :: [string()],
         orig_files                 :: [string()],
         orig_args                  :: [string()],
         prev_log_dir               :: undefined | string(),
         mode = execute             :: lux:run_mode(),
         skip_unstable = false      :: boolean(),
         skip_skip = false          :: boolean(),
         progress = brief           :: silent | summary | brief |
                                       doc | compact | verbose |
                                       etrace | ctrace,
         config_dir                 :: string(),
         file_pattern = "^[^\\\.].*\\\.lux" ++ [$$] :: string(),
         case_prefix = ""           :: string(),
         log_fd                     :: file:io_device(),
         log_dir                    :: file:filename(),
         summary_log                :: string(),
         config_name                :: string(),
         config_file                :: string(),
         suite = ?b2l(?DEFAULT_SUITE) :: string(),
         start_time                 :: erlang:timestamp(),
         suite_timer_ref            :: #timer_ref{},
         run                        :: string(),
         extend_run = false         :: boolean(),
         revision = ""              :: string(),
         hostname = lux_utils:real_hostname() :: string(),
         rerun = disable            :: enable | success | skip | warning |
                                       fail | error | disable,
         html = enable              :: validate |
                                       enable | success | skip | warning |
                                       fail | error | disable,
         warnings = []              :: [#warning{}],
         internal_args = []         :: [{atom(), term()}], % Internal opts
         user_args = []             :: [{atom(), term()}], % Command line opts
         file_args = []             :: [{atom(), term()}], % Script opts
         config_args = []           :: [{atom(), term()}], % Arch spec opts
         common_args = []           :: [{atom(), term()}], % Non-arch spec opts
         default_args = []          :: [{atom(), term()}], % Default opts
         builtin_vars = lux_utils:builtin_vars()
                                    :: [string()], % ["name=val"]
         system_vars = lux_utils:system_vars()
                                    :: [string()], % ["name=val"]
         tap_opts = []              :: [string()],
         tap                        :: term(), % #tap{}
         junit = false              :: boolean()
        }).

-record(pstate,
        {file           :: string(),
         orig_file      :: string(),
         pos_stack      :: [#cmd_pos{}],
         body_level     :: non_neg_integer(),
         mode           :: lux:run_mode(),
         skip_unstable  :: boolean(),
         skip_skip      :: boolean(),
         multi_vars     :: [[string()]], % ["name=val"]
         warnings       :: [binary()],
         has_cleanup = false :: boolean(),
         top_doc        :: undefined | non_neg_integer(),
         newshell       :: boolean()
        }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types

-type filename()     :: string().
-type dirname()      :: string().
-type opts()         :: [{atom(), term()}].
-type cmds()         :: [#cmd{}].
-type summary()      :: success | skip | warning | fail | error.
-type lineno()       :: string().
-type skip()         :: {skip, filename(), string()}.
-type error()        :: {error, filename(), string()}.
-type no_input()     :: {error, undefined, no_input_files}.
-type result()       :: {ok, filename(), summary(), lineno(), [#warning{}]}.
-type run_mode()     :: list | list_dir | doc |
                        validate | dump | expand | execute.
-type expected_tag() :: ?EXPECTED_EQ | ?EXPECTED_RE.
-type suppress()     :: suppress_any_success | suppress_none.
-type select()       :: select_worst | select_latest.
