%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2014 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_shell).

-export([start_monitor/3]).

-include("lux.hrl").

-define(end_of_script(C),
        C#cstate.no_more_input =:= true andalso
        C#cstate.timer =:= undefined andalso
        C#cstate.expected =:= undefined).

-record(pattern,
        {cmd       :: #cmd{},
         cmd_stack :: [{string(), non_neg_integer(), atom()}]}).

-record(cstate,
        {orig_file               :: string(),
         parent                  :: pid(),
         name                    :: string(),
         latest_cmd              :: #cmd{},
         cmd_stack = []          :: [{string(), non_neg_integer(), atom()}],
         wait_for_expect         :: undefined | pid(),
         mode = resume           :: resume | suspend,
         start_reason            :: fail | success | normal,
         progress                :: silent | brief | doc | compact | verbose,
         log_fun                 :: function(),
         log_prefix              :: string(),
         event_log_fd            :: {true, file:io_device()},
         stdin_log_fd            :: {false, file:io_device()},
         stdout_log_fd           :: {false, file:io_device()},
         multiplier              :: non_neg_integer(),
         poll_timeout            :: non_neg_integer(),
         flush_timeout           :: non_neg_integer(),
         timeout                 :: non_neg_integer() | infinity,
         shell_wrapper           :: undefined | string(),
         shell_cmd               :: string(),
         shell_args              :: [string()],
         shell_prompt_cmd        :: string(),
         shell_prompt_regexp     :: string(),
         port                    :: port(),
         waiting = false         :: boolean(),
         fail                    :: undefined | #pattern{},
         success                 :: undefined | #pattern{},
         expected                :: undefined | #cmd{},
         actual = <<>>           :: binary(),
         state_changed = false   :: boolean(),
         timed_out = false       :: boolean(),
         idle_count = 0          :: non_neg_integer(),
         no_more_input = false   :: boolean(),
         no_more_output = false  :: boolean(),
         timer                   :: undefined | infinity | reference(),
         timer_started_at        :: undefined | {non_neg_integer(),
                                                 non_neg_integer(),
                                                 non_neg_integer()},
         submatches = []         :: [binary()],
         debug_level = 0         :: non_neg_integer(),
         events = []             :: [tuple()],
         macro_dict              :: [string()],   % ["name=val"]
         dict                    :: [string()],   % ["name=val"]
         builtin_dict            :: [string()],   % ["name=val"]
         system_dict             :: [string()]}). % ["name=val"]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client

start_monitor(I, Cmd, Name) ->
    OrigFile = I#istate.orig_file,
    Self = self(),
    Base = filename:basename(OrigFile),
    Prefix = filename:join([I#istate.log_dir, Base ++ "." ++ Name]),
    C = #cstate{orig_file = OrigFile,
                parent = Self,
                name = Name,
                start_reason = I#istate.cleanup_reason,
                latest_cmd = Cmd,
                cmd_stack = I#istate.cmd_stack,
                progress = I#istate.progress,
                log_fun = I#istate.log_fun,
                event_log_fd = I#istate.event_log_fd,
                log_prefix = Prefix,
                multiplier = I#istate.multiplier,
                poll_timeout = I#istate.poll_timeout,
                flush_timeout = I#istate.flush_timeout,
                timeout = I#istate.timeout,
                shell_wrapper = I#istate.shell_wrapper,
                shell_cmd = I#istate.shell_cmd,
                shell_args = I#istate.shell_args,
                shell_prompt_cmd = I#istate.shell_prompt_cmd,
                shell_prompt_regexp = I#istate.shell_prompt_regexp,
                debug_level = I#istate.debug_level,
                macro_dict = I#istate.macro_dict,
                dict = I#istate.dict,
                builtin_dict = I#istate.builtin_dict,
                system_dict = I#istate.system_dict},
    {Pid, Ref} = spawn_monitor(fun() -> init(C) end),
    Shell = #shell{name = Name, pid = Pid, ref = Ref, health = alive},
    I2 = I#istate{active_pid = Pid,
                  active_name = Name,
                  shells = [Shell | I#istate.shells]},
    receive
        {started, Pid, Logs} ->
            {ok, I2#istate{logs = I2#istate.logs ++ [Logs]}};
        {'DOWN', _, process, Pid, Reason} ->
            {error, I2, Pid, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server

send_reply(C, To, Msg) ->
    TraceFrom = C#cstate.name,
    TraceTo =
        if
            To =:= C#cstate.parent -> 'case';
            true                   -> To
        end,
    lux:trace_me(50, TraceFrom, TraceTo, element(1, Msg), [Msg]),
    To ! Msg.

init(C) when is_record(C, cstate) ->
    erlang:monitor(process, C#cstate.parent),
    process_flag(trap_exit, true),
    Name = C#cstate.name,
    lux:trace_me(50, 'case', Name, 'spawn', []),
    {InFile, InFd} = open_logfile(C, "stdin"),
    {OutFile, OutFd} = open_logfile(C, "stdout"),
    C2 = C#cstate{stdin_log_fd = {false, InFd},
                  stdout_log_fd = {false, OutFd}},
    {Exec, Args} = choose_exec(C2),
    FlatExec = lists:flatten([Exec, [[" ", A] || A <- Args]]),
    Events = save_event(C2, start, FlatExec),
    StartReason = atom_to_list(C#cstate.start_reason),
    PortEnv = [{"LUX_SHELLNAME", Name},
               {"LUX_START_REASON", StartReason}],
    WorkDir = filename:dirname(C#cstate.orig_file),
    Opts = [binary, stream, use_stdio, stderr_to_stdout,
            {args, Args}, {cd, WorkDir}, {env, PortEnv}],
    try
        Port = open_port({spawn_executable, Exec}, Opts),
        Dict = ["LUX_SHELLNAME=" ++ Name,
                "LUX_START_REASON=" ++ StartReason],
        C3 = C2#cstate{port = Port,
                       events = Events,
                       dict = Dict ++ C2#cstate.dict},

        Parent = C3#cstate.parent,
        erlang:monitor(process, Parent),
        send_reply(C3, Parent, {started, self(), {Name, InFile, OutFile}}),
        try
            shell_loop(C3, C3)
        catch
            error:LoopReason ->
                LoopBinErr = list_to_binary("Internal lux error: " ++
                                                file:format_error(LoopReason)),
                io:format("~s\n~p\n", [LoopBinErr, erlang:get_stacktrace()]),
                stop(C2, error, LoopBinErr)
        end
    catch
        error:InitReason ->
            InitBinErr = list_to_binary(FlatExec ++ ": " ++
                                            file:format_error(InitReason)),
            io:format("~s\n~p\n", [InitBinErr, erlang:get_stacktrace()]),
            stop(C2, error, InitBinErr)
    end.

open_logfile(C, Slogan) ->
    LogFile = C#cstate.log_prefix ++ "." ++ Slogan ++ ".log",
    case file:open(LogFile, [write, raw]) of
        {ok, Fd} ->
            {LogFile, Fd};
        {error, FileReason} ->
            String = file:format_error(FileReason),
            BinErr = list_to_binary(["Failed to open logfile: ", LogFile,
                                     " -> ", String]),
            io:format("~s\n~p\n", [BinErr, erlang:get_stacktrace()]),
            stop(C, error, BinErr)
    end.

choose_exec(C) ->
    case C#cstate.shell_wrapper of
        undefined -> {C#cstate.shell_cmd, C#cstate.shell_args};
        Wrapper   -> {Wrapper, [C#cstate.shell_cmd | C#cstate.shell_args]}
    end.

shell_loop(C, OrigC) ->
    {C2, ProgressStr} = progress_token(C),
    lux_utils:progress_write(C2#cstate.progress, ProgressStr),
    C3 = expect_more(C2),
    C4 = shell_wait_for_event(C3, OrigC),
    shell_loop(C4, OrigC).

progress_token(#cstate{idle_count = IdleCount} = C) ->
    case IdleCount of
        0 ->
            {C, "."};
        1 ->
            {C#cstate{idle_count = IdleCount+1},
             integer_to_list((C#cstate.latest_cmd)#cmd.lineno) ++ "?"};
        _ ->
            {C, "?"}
    end.

shell_wait_for_event(#cstate{name = _Name} = C, OrigC) ->
    Timeout = timeout(C),
    receive
        {block, From} ->
            %% io:format("\nBLOCK ~s\n", [C#cstate.name]),
            block(C, From, OrigC);
        {sync, From, When} ->
            C2 = opt_sync_reply(C, From, When),
            shell_wait_for_event(C2, OrigC);
        {switch_cmd, From, _When, NewCmd, CmdStack, Fun} ->
            C2 = switch_cmd(C, From, NewCmd, CmdStack, Fun),
            shell_wait_for_event(C2, OrigC);
        {change_mode, From, Mode, Cmd, CmdStack}
          when Mode =:= resume; Mode =:= suspend ->
            C2 = change_mode(C, From, Mode, Cmd, CmdStack),
            expect_more(C2);
        {progress, _From, Level} ->
            shell_wait_for_event(C#cstate{progress = Level}, OrigC);
        {expand_vars, From, Bin, MissingVar} ->
            expand_vars_and_reply(C, From, Bin, MissingVar);
        {eval, From, Cmd} ->
            dlog(C, ?dmore, "eval (got ~p)", [Cmd#cmd.type]),
            assert_eval(C, Cmd, From),
            shell_eval(C, Cmd, OrigC);
        {global, _From, VarVal} ->
            clog(C, global, "\"~s\"", [VarVal]),
            C#cstate{dict = [VarVal | C#cstate.dict]};
        {macro_dict, _From, MacroDict} ->
            %% clog(C, macro_dict, "\"~s\"", [MacroDict]),
            C#cstate{macro_dict = MacroDict};
        {shutdown = Data, _From} ->
            stop(C, shutdown, Data);
        {relax = Data, _From} ->
            stop(C, relax, Data);
        {end_of_script, _From} ->
            clog(C, 'end', "of script", []),
            dlog(C, ?dmore,"mode=resume (end_of_script)", []),
            C#cstate{no_more_input = true, mode = resume};
        {Port, {data, Data}} when Port =:= C#cstate.port ->
            Progress = C#cstate.progress,
            lux_utils:progress_write(Progress, ":"),
            %% Read all available data
            NewData = flush_port(C, C#cstate.poll_timeout, Data),
            lux_log:safe_write(Progress,
                               C#cstate.log_fun,
                               C#cstate.stdout_log_fd,
                               NewData),
            OldData = C#cstate.actual,
            C#cstate{state_changed = true,
                     actual = <<OldData/binary, NewData/binary>>,
                     events = save_event(C, recv, NewData)};
        timeout ->
            C#cstate{state_changed = true,
                     timed_out = true,
                     events = save_event(C, recv, timeout)};
        {wakeup, Secs} ->
            clog(C, wake, "up (~p seconds)", [Secs]),
            dlog(C, ?dmore,"mode=resume (wakeup)", []),
            C#cstate{mode = resume};
        {'EXIT', Port, _PosixCode} when Port =:= C#cstate.port ->
            C#cstate{state_changed = true,
                     no_more_output = true,
                     events = save_event(C, recv, shell_exit)};
        {'DOWN', _, process, Pid, Reason} ->
            if
                Pid =:= C#cstate.parent ->
                    Waste = flush_port(C,
                                       C#cstate.flush_timeout,
                                       C#cstate.actual),
                    clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
                    close_and_exit(C,
                                   {error, internal},
                                   {internal_error, interpreter_died,
                                    C#cstate.latest_cmd, Reason});
                true ->
                    %% Ignore
                    C
            end;
        Unexpected ->
            lux:trace_me(70, C#cstate.name, internal_error,
                         [{shell_got, Unexpected}]),
            exit({shell_got, Unexpected})
    after multiply(C, Timeout) ->
            C#cstate{idle_count = C#cstate.idle_count + 1}
    end.

timeout(C) ->
    IdleThreshold = timer:seconds(3),
    if
        C#cstate.expected =:= undefined ->
            infinity;
        C#cstate.timer =/= undefined ->
            IdleThreshold;
        true ->
            IdleThreshold
    end.

expand_vars_and_reply(C, From, Bin, MissingVar) ->
    Res =
        try
            {ok, expand_vars(C, Bin, MissingVar)}
        catch
            throw:{no_such_var, BadName} ->
                {no_such_var, BadName}
        end,
    send_reply(C, From, {expand_vars, self(), Res}),
    C.

switch_cmd(C, From, NewCmd, CmdStack, Fun) ->
    Fun(),
    send_reply(C, From, {switch_cmd_ack, self()}),
    C#cstate{latest_cmd = NewCmd, cmd_stack = CmdStack}.

change_mode(C, From, Mode, Cmd, CmdStack) ->
    Reply = {change_mode_ack, self()},
    case Mode of
        suspend ->
            clog(C, Mode, "", []),
            dlog(C, ?dmore,"mode=suspend waiting=false (~p)",
                 [Cmd#cmd.type]),
            send_reply(C, From, Reply),
            C#cstate{mode = Mode,
                     waiting = false};
        resume ->
            NewC =
                C#cstate{mode = Mode,
                         waiting = false,
                         latest_cmd = Cmd,
                         cmd_stack = CmdStack},
            dlog(NewC, ?dmore, "mode=resume waiting=false (~p)",
                 [Cmd#cmd.type]),
            clog(NewC, Mode, "(idle since line ~p)",
                 [C#cstate.latest_cmd#cmd.lineno]),
            send_reply(NewC, From, Reply),
            NewC
    end.

block(C, From, OrigC) ->
    lux:trace_me(40, C#cstate.name, block, []),
    receive
        {unblock, From} ->
            lux:trace_me(40, C#cstate.name, unblock, []),
            %% io:format("\nUNBLOCK ~s\n", [C#cstate.name]),
            shell_wait_for_event(C, OrigC);
        {expand_vars, From, Bin, MissingVar} ->
            C2 = expand_vars_and_reply(C, From, Bin, MissingVar),
            block(C2, From, OrigC);
        {sync, From, When} ->
            C2 = opt_sync_reply(C, From, When),
            block(C2, From, OrigC);
        {switch_cmd, From, _When, NewCmd, CmdStack, Fun} ->
            C2 = switch_cmd(C, From, NewCmd, CmdStack, Fun),
            block(C2, From, OrigC);
        {'DOWN', _, process, Pid, Reason} when Pid =:= C#cstate.parent ->
            Waste = flush_port(C,
                               C#cstate.flush_timeout,
                               C#cstate.actual),
            clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
            close_and_exit(C,
                           {error, internal},
                           {internal_error, interpreter_died,
                            C#cstate.latest_cmd, Reason});
        {'EXIT', Port, _PosixCode} when Port =:= C#cstate.port ->
            C2 = C#cstate{state_changed = true,
                          no_more_output = true,
                          events = save_event(C, recv, shell_exit)},
            shell_wait_for_event(C2, OrigC);
        {shutdown = Data, _From} ->
            stop(C, shutdown, Data);
        {relax = Data, _From} ->
            stop(C, relax, Data)
    end.

opt_sync_reply(C, From, When) when C#cstate.wait_for_expect =:= undefined ->
    case When of
        flush ->
            flush_logs(C),
            sync_reply(C, From);
        immediate ->
            sync_reply(C, From);
        wait_for_expect when C#cstate.expected =:= undefined ->
            sync_reply(C, From);
        wait_for_expect ->
            C#cstate{wait_for_expect = From}
    end.

opt_late_sync_reply(#cstate{wait_for_expect = From} = C) ->
    if
        is_pid(From) ->
            sync_reply(C, From);
        From =:= undefined ->
            C
    end.

sync_reply(C, From) ->
    send_reply(C, From, {sync_ack, self()}),
    C#cstate{wait_for_expect = undefined}.

assert_eval(C, Cmd, From) when From =/= C#cstate.parent ->
    %% Assert - sender must be parent
    Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
    close_and_exit(C,
                   {error, internal},
                   {internal_error, invalid_sender,
                    Cmd, process_info(From)});
assert_eval(C, Cmd, From) when C#cstate.no_more_input ->
    Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
    close_and_exit(C,
                   {error, internal},
                   {internal_error, no_more_input,
                    Cmd, process_info(From)});
assert_eval(C, _Cmd, _From) when C#cstate.expected =:= undefined ->
    ok;
assert_eval(_C, Cmd, _From) when Cmd#cmd.type =:= variable;
                                 Cmd#cmd.type =:= cleanup ->
    ok;
%% assert_eval(C, Cmd, _From) when is_record(C#cstate.expected, cmd),
%%                                 Cmd#cmd.type =/= expect,
%%                                 Cmd#cmd.type =/= send_lf,
%%                                 Cmd#cmd.type =/= send,
%%                                 Cmd#cmd.type =/= fail,
%%                                 Cmd#cmd.type =/= success,
%%                                 Cmd#cmd.type =/= sleep,
%%                                 Cmd#cmd.type =/= progress,
%%                                 Cmd#cmd.type =/= change_timeout ->
%%         ok;
assert_eval(C, Cmd, _From) ->
    Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
    close_and_exit(C,
                   {error, internal},
                   {internal_error, invalid_type,
                    Cmd, C#cstate.expected, Cmd#cmd.type}).

shell_eval(#cstate{name = Name} = C0,
           #cmd{type = Type, arg = Arg} = Cmd, OrigC) ->
    C = C0#cstate{latest_cmd = Cmd, waiting = false},
    dlog(C, ?dmore,"waiting=false (eval ~p)", [Cmd#cmd.type]),
    case Type of
        variable ->
            try
                {Scope, Var, Val} = Arg,
                Val2 = expand_vars(C, Val, error),
                VarVal = lists:flatten([Var, $=, Val2]),
                clog(C, Scope, "\"~s\"", [ VarVal]),
                case Scope of
                    my ->
                        send_reply(C, C#cstate.parent, {my,self(),Name,VarVal}),
                        C#cstate{macro_dict = [VarVal | C#cstate.macro_dict]};
                    local ->
                        C#cstate{dict = [VarVal | C#cstate.dict]};
                    global ->
                        send_reply(C, C#cstate.parent,
                                   {global,self(), Name, VarVal}),
                        C#cstate{dict = [VarVal | C#cstate.dict]}
                end
            catch
                throw:{no_such_var, BadName} ->
                    BinErr = list_to_binary(["Variable $", BadName,
                                             " is not set"]),
                    %% io:format("~s\n~p\n", [BinErr, erlang:get_stacktrace()]),
                    stop(C, error, BinErr)
            end;
        send_lf when is_binary(Arg) ->
            send_to_port(C, <<Arg/binary, "\n">>);
        send when is_binary(Arg) ->
            send_to_port(C, Arg);
        expect when Arg =:= shell_exit ->
            clog(C, expect, "~p", [Arg]),
            C2 = start_timer(C),
            dlog(C2, ?dmore, "expected=regexp (shell_exit)", []),
            C2#cstate{state_changed = true,
                      expected = Cmd};
        expect when Arg =:= reset ->
            %% Reset output buffer
            C2 = cancel_timer(C),
            Waste = flush_port(C2, C2#cstate.flush_timeout, C#cstate.actual),
            clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
            clog(C, output, "reset", []),
            C2#cstate{state_changed = true,
                      actual = <<>>,
                      events = save_event(C, recv, Waste)};
        expect ->
            {C2, Cmd2, RegExp2} = compile_regexp(C, Arg),
            clog(C2, expect, "\"~s\"", [lux_utils:to_string(RegExp2)]),
            C3 = start_timer(C2),
            dlog(C3, ?dmore, "expected=regexp (expect)", []),
            C3#cstate{state_changed = true,
                      expected = Cmd2};
        fail ->
            {C2, Cmd2, RegExp2} = compile_regexp(C, Arg),
            clog(C2, fail, "pattern ~p", [lux_utils:to_string(RegExp2)]),
            Pattern = #pattern{cmd = Cmd2,
                               cmd_stack = C2#cstate.cmd_stack},
            C2#cstate{state_changed = true,
                      fail = Pattern};
        success ->
            {C2, Cmd2, RegExp2} = compile_regexp(C, Arg),
            clog(C2, success, "pattern ~p",
                 [lux_utils:to_string(RegExp2)]),
            Pattern = #pattern{cmd = Cmd2,
                               cmd_stack = C2#cstate.cmd_stack},
            C2#cstate{state_changed = true,
                           success = Pattern};
        sleep ->
            Secs = parse_int(C, Arg, Cmd),
            clog(C, sleep, "(~p seconds)", [Secs]),
            Progress = C#cstate.progress,
            Self = self(),
            WakeUp = {wakeup, Secs},
            Sleeper = spawn_link(fun() ->
                                         sleep_walker(Progress, Self, WakeUp)
                                 end),
            erlang:send_after(timer:seconds(Secs), Sleeper, WakeUp),
            dlog(C, ?dmore,"mode=suspend (sleep)", []),
            C#cstate{mode = suspend};
        progress when is_list(Arg) ->
            try
                String = expand_vars(C, Arg, error),
                clog(C, progress, "\"~s\"", [String]),
                io:format("~s", [String]),
                C
            catch
                throw:{no_such_var, BadName} ->
                    BinErr = list_to_binary(["Variable $", BadName,
                                             " is not set"]),
                    stop(C, error, BinErr)
            end;
        change_timeout ->
            Millis =
                case Arg of
                    "" ->
                        OrigC#cstate.timeout;
                    "infinity" ->
                        infinity;
                    SecsStr ->
                        Secs = parse_int(C, SecsStr, Cmd),
                        timer:seconds(Secs)
                end,
            case Millis of
                infinity ->
                    clog(C, change, "expect timeout to infinity", []);
                _ ->
                    clog(C, change, "expect timeout to ~p seconds",
                         [Millis div timer:seconds(1)])
            end,
            C#cstate{timeout = Millis};
        cleanup ->
            C2 = cancel_timer(C),
            case C#cstate.fail of
                undefined -> ok;
                _         -> clog(C2, fail, "pattern reset", [])
            end,
            case C#cstate.success of
                undefined  -> ok;
                _          -> clog(C2, success, "pattern reset", [])
            end,
            C3 = C2#cstate{expected = undefined,
                           fail = undefined,
                           success = undefined},
            dlog(C3, ?dmore, "expected=undefined (cleanup)", []),
            opt_late_sync_reply(C3);
        Unexpected ->
            Err = io_lib:format("[shell ~s] got cmd with type ~p ~p\n",
                                [Name, Unexpected, Arg]),
            stop(C, error, list_to_binary(Err))
    end.

send_to_port(C, RawData) ->
    try
        Data = expand_vars(C, RawData, error),
        lux_log:safe_write(C#cstate.progress,
                           C#cstate.log_fun,
                           C#cstate.stdin_log_fd,
                           Data),
        C2 = C#cstate{events = save_event(C, send, Data)},
        try
            true = port_command(C2#cstate.port, Data),
            C2
        catch
            error:_Reason ->
                receive
                    {'EXIT', Port, _PosixCode}
                      when Port =:= C2#cstate.port ->
                        C2#cstate{state_changed = true,
                                  no_more_output = true,
                                  events = save_event(C2, recv, shell_exit)}
                after 0 ->
                        C2
                end
        end
    catch
        throw:{no_such_var, BadName} ->
            VarErr = list_to_binary(["Variable $", BadName,
                                     " is not set"]),
            stop(C, error, VarErr)
    end.

parse_int(C, Chars, Cmd) ->
    Chars2 = expand_vars(C, Chars, error),
    try
        list_to_integer(Chars2)
    catch
        error:_ ->
            BinErr = list_to_binary(["Syntax error at line ",
                                     integer_to_list(Cmd#cmd.lineno),
                                     ": '", Chars2, "' integer expected"]),
            stop(C, error, BinErr)
    end.

sleep_walker(Progress, ReplyTo, WakeUp) ->
    %% Snore each second
    lux_utils:progress_write(Progress, "z"),
    receive
        WakeUp ->
            ReplyTo ! WakeUp,
            unlink(ReplyTo)
    after 1000 ->
            sleep_walker(Progress, ReplyTo, WakeUp)
    end.

want_more(#cstate{mode = Mode, expected = Expected, waiting = Waiting}) ->
    Mode =:= resume andalso Expected =:= undefined andalso not Waiting.

expect_more(C) ->
    C2 = expect(C),
    HasEval =
        if
            C2#cstate.debug_level > 0 ->
                Msgs = element(2, process_info(self(), messages)),
                lists:keymember(eval, 1, Msgs);
            true ->
                undefined
        end,
    case want_more(C2) of
        true when HasEval ->
            dlog(C2, ?dmore, "messages=~p", [HasEval]),
            C2;
        true ->
            dlog(C2, ?dmore, "messages=~p", [HasEval]),
            dlog(C2, ?dmore, "waiting=true (~p) send more",
                 [C2#cstate.latest_cmd#cmd.type]),
            send_reply(C2, C2#cstate.parent, {more, self(), C2#cstate.name}),
            C2#cstate{waiting = true};
        false ->
            C2
    end.

expect(#cstate{} = C) when ?end_of_script(C) ->
    %% Normal end of script
    stop(C, success, end_of_script);
expect(#cstate{state_changed = false} = C) ->
    %% Nothing has changed
    C;
expect(#cstate{state_changed = true,
               no_more_output = NoMoreOutput,
               expected = Expected,
               actual = Actual,
               timed_out = TimedOut} = C0) ->
    %% Something has changed
    TmpC = match_success_pattern(C0#cstate{state_changed = false}, Actual),
    C = match_fail_pattern(TmpC, Actual),
    case Expected of
        undefined ->
            %% Nothing to wait for
            cancel_timer(C);
        #cmd{arg = Arg} ->
            if
                TimedOut ->
                    %% timeout - waited enough for more input
                    Earlier = C#cstate.timer_started_at,
                    clog(C, timer, "fail (~p seconds)",
                         [timer:now_diff(erlang:now(), Earlier) div 1000000]),
                    stop(C, fail, timeout);
                NoMoreOutput, Arg =:= shell_exit ->
                    %% Successful match of end of file (port program closed)
                    C2 = cancel_timer(C),
                    clog(C2, match, "\"shell_exit\"", []),
                    stop(C, shutdown, shell_exit);
                NoMoreOutput ->
                    %% Got end of file while waiting for more data
                    stop(C, fail, shell_exit);
                Arg =:= shell_exit ->
                    %% Still waiting for end of file
                    C;
                true ->
                    %% Waiting for more data
                    try_match(C, Actual)
            end
    end.

try_match(C, Actual) ->
    case match(Actual, C#cstate.expected) of
        {match, Matches}  ->
            %% Successful match
            C2 = cancel_timer(C),
            {Actual2, SubMatches} =
                split_submatches(C2, Matches, Actual, ""),
            case C2#cstate.no_more_input of
                true ->
                    %% End of input
                    stop(C2, success, end_of_script);
                false ->
                    C3 = C2#cstate{expected = undefined,
                                   actual = Actual2,
                                   submatches = SubMatches},
                    dlog(C3, ?dmore,
                         "expected=undefined (waiting)",
                         []),
                    opt_late_sync_reply(C3)
            end;
        nomatch ->
            %% Main pattern does not match
            %% Wait for more input
            C
    end.

match(Actual, #pattern{cmd = Cmd}) -> % success or fail pattern
    match(Actual, Cmd);
match(_Actual, undefined) ->
    nomatch;
match(Actual, #cmd{type = Type, arg = Arg}) ->
    if
        Type =:= expect; Type =:= fail; Type =:= success ->
            case Arg of
                {verbatim, Expected} ->
                    %% io:format("\n"
                    %%           "verbatim_match(~p,\n"
                    %%           "               ~p,\n"
                    %%           "               ~p).\n",
                    %%           [Actual, Expected, []]),
                    lux_utils:verbatim_match(Actual, Expected);
                {mp, _RegExp, MP} ->
                    Opts = [{newline,any},notempty,{capture,all,index}],
                    %% io:format("\n"
                    %%           "re:run(~p,\n"
                    %%           "       ~p,\n"
                    %%           "       ~p).\n",
                    %%           [Actual, _RegExp, Opts]),
                    re:run(Actual, MP, Opts)
            end
    end.

match_fail_pattern(C, Actual) ->
    case match(Actual, C#cstate.fail) of
        {match, Matches} ->
            C2 = cancel_timer(C),
            C3 = prepare_stop(C2, Actual, Matches, "fail pattern matched "),
            stop(C3, fail, fail_pattern_matched);
        nomatch ->
            C
    end.

match_success_pattern(C, Actual) ->
    case match(Actual, C#cstate.success) of
        {match, Matches} ->
            C2 = cancel_timer(C),
            C3 = prepare_stop(C2, Actual, Matches, "success pattern matched "),
            stop(C3, success, success_pattern_matched);
        nomatch ->
            C
    end.

split_submatches(C, [{First, TotLen} | Matches], Actual, Context) ->
    {Consumed, Rest} = split_binary(Actual, First+TotLen),
    {Skip, Match} = split_binary(Consumed, First),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Skip)]),
    clog(C, match, "~s\"~s\"", [Context, lux_utils:to_string(Match)]),
    Extract =
        fun(PosLen) ->
                case PosLen of
                    {-1,0} -> nosubmatch;
                    _      -> binary:part(Actual, PosLen)
                end
        end,
    SubBins = lists:map(Extract, Matches),
    {Rest, SubBins}.

prepare_stop(C, Actual, [{First, TotLen} | _], Context) ->
    {Skip, Rest} = split_binary(Actual, First),
    {Match, _Actual2} = split_binary(Rest, TotLen),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Skip)]),
    clog(C, match, "~s\"~s\"", [Context, lux_utils:to_string(Match)]),
    C2 = C#cstate{expected = undefined,
                  actual = Match,
                  submatches = []},
    dlog(C2, ?dmore, "expected=undefined (prepare_stop)", []),
    opt_late_sync_reply(C2).

flush_port(#cstate{port = Port} = C, Timeout, Acc) ->
    case do_flush_port(Port, 0, 0, Acc) of
        {0, Acc2} when Timeout =/= 0 ->
            {_N, Acc3} = do_flush_port(Port, multiply(C, Timeout), 0, Acc2),
            Acc3;
        {_N, Acc2} ->
            Acc2
    end.

do_flush_port(Port, Timeout, N, Acc) ->
    receive
        {Port, {data, Data}} ->
            do_flush_port(Port, Timeout, N+1, <<Acc/binary, Data/binary>>)
    after Timeout ->
            {N, Acc}
    end.

start_timer(#cstate{timer = undefined, timeout = infinity} = C) ->
    clog(C, timer, "start (infinity)", []),
    C#cstate{timer = infinity, timer_started_at = erlang:now()};
start_timer(#cstate{timer = undefined} = C) ->
    Seconds = C#cstate.timeout div timer:seconds(1),
    Multiplier = C#cstate.multiplier / 1000,
    clog(C, timer, "start (~p seconds * ~.3f)", [Seconds, Multiplier]),
    Timer = safe_send_after(C, C#cstate.timeout, self(), timeout),
    C#cstate{timer = Timer, timer_started_at = erlang:now()};
start_timer(#cstate{} = C) ->
    clog(C, timer, "keep", []),
    C.

cancel_timer(#cstate{timer = undefined} = C) ->
    C;
cancel_timer(#cstate{timer = Timer, timer_started_at = Earlier} = C) ->
    clog(C, timer, "cancel (~p seconds)",
        [timer:now_diff(erlang:now(), Earlier) div 1000000]),
    case Timer of
        infinity -> ok;
        _        -> erlang:cancel_timer(Timer)
    end,
    C#cstate{idle_count = 0,
             timer = undefined,
             timer_started_at = undefined}.

compile_regexp(C, reset) ->
    {C, undefined, reset};
compile_regexp(C, shell_exit) ->
    {C, undefined, shell_exit};
compile_regexp(C, {verbatim, Verbatim}) ->
    {C, C#cstate.latest_cmd, Verbatim};
compile_regexp(C, {mp, RegExp, _MP}) ->
    {C, C#cstate.latest_cmd, RegExp};
compile_regexp(C, {template, Template}) ->
    Verbatim = expand_vars(C, Template, error),
    patch_latest(C, {verbatim, Verbatim}, Verbatim);
compile_regexp(C, {regexp, RegExp}) ->
    try
        RegExp2 = expand_vars(C, RegExp, error),
        RegExp3 = lux_utils:normalize_newlines(RegExp2),
        %% io:format("REGEXP: ~p ~p\n", [RegExp, RegExp3]),
        Opts = [multiline, {newline, anycrlf}],
        case re:compile(RegExp3, Opts) of
            {ok, MP2} ->
                patch_latest(C, {mp, RegExp3, MP2}, RegExp3);
            {error, {Reason, _Pos}} ->
                BinErr = list_to_binary(["Syntax error: ", Reason,
                                         " in regexp '", RegExp3, "'"]),
                stop(C, error, BinErr)
        end
    catch
        throw:{no_such_var, BadName} ->
            BinErr2 = list_to_binary(["Syntax error: Variable $",
                                      BadName, " is not set in regexp '",
                                      RegExp, "'"]),
            stop(C, error, BinErr2)
    end.

patch_latest(C, NewArg, Expect) ->
    Cmd = C#cstate.latest_cmd,
    Cmd2 = Cmd#cmd{arg = NewArg},
    {C#cstate{latest_cmd = Cmd2}, Cmd2, Expect}.

stop(C, Outcome, Actual) when is_binary(Actual); is_atom(Actual) ->
    Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
    clog(C, stop, "~p", [Outcome]),
    Cmd = C#cstate.latest_cmd,
    Expected = cmd_expected(Cmd),
    {NewOutcome, Extra} = prepare_outcome(C, Outcome, Actual),
    Res = #result{outcome = NewOutcome,
                  name = C#cstate.name,
                  latest_cmd = Cmd,
                  cmd_stack = C#cstate.cmd_stack,
                  expected = Expected,
                  extra = Extra,
                  actual = Actual,
                  rest = C#cstate.actual,
                  events = lists:reverse(C#cstate.events)},
    lux:trace_me(40, C#cstate.name, Outcome, [{actual, Actual}, Res]),
    C2 = opt_late_sync_reply(C#cstate{expected = undefined}),
    C3 = close_logs(C2),
    send_reply(C3, C3#cstate.parent, {stop, self(), Res}),
    if
        Outcome =:= shutdown ->
            close_and_exit(C3, Outcome, Res);
        Outcome =:= error ->
            close_and_exit(C3, {error, Actual}, Res);
        true ->
            %% Wait for potential cleanup to be run
            %% before we close the port
            wait_for_down(C3, Res)
    end.

prepare_outcome(C, Outcome, Actual) ->
    if
        Outcome =:= fail, Actual =:= fail_pattern_matched ->
            NewOutcome = Outcome,
            Fail = C#cstate.fail,
            FailCmd = Fail#pattern.cmd,
            Extra = element(2, FailCmd#cmd.arg),
            clog(C, pattern, "\"~p\"", [lux_utils:to_string(Extra)]);
        Outcome =:= success, Actual =:= success_pattern_matched ->
            NewOutcome = Outcome,
            Success = C#cstate.success,
            SuccessCmd = Success#pattern.cmd,
            Extra = element(2, SuccessCmd#cmd.arg),
            clog(C, pattern, "\"~p\"", [lux_utils:to_string(Extra)]);
        Outcome =:= error ->
            NewOutcome = fail,
            Extra = Actual;
        Outcome =:= shutdown ->
            NewOutcome = Outcome,
            Extra = undefined;
        Outcome =:= relax ->
            NewOutcome = Outcome,
            Extra = undefined;
        true ->
            NewOutcome = Outcome,
            Extra = undefined
    end,
    {NewOutcome, Extra}.

cmd_expected(Cmd) ->
    case Cmd of
        #cmd{type = expect, arg = {verbatim, Expected}} ->
            ok;
        #cmd{type = expect, arg = {mp, Expected, _MP}} ->
            ok;
        #cmd{} ->
            Expected = <<"">>
    end,
    Expected.

close_and_exit(C, Reason, #result{}) ->
    lux:trace_me(40, C#cstate.name, close_and_exit, []),
    catch port_close(C#cstate.port),
    exit(Reason);
close_and_exit(C, Reason, Error) when element(1, Error) =:= internal_error ->
    Cmd = element(3, Error),
    Why = element(2, Error),
    Res = #result{outcome = error,
                  name = C#cstate.name,
                  latest_cmd = Cmd,
                  cmd_stack = C#cstate.cmd_stack,
                  expected = cmd_expected(Cmd),
                  extra = undefined,
                  actual = internal_error,
                  rest = C#cstate.actual,
                  events = lists:reverse(C#cstate.events)},
    io:format("\nSHELL INTERNAL ERROR: ~p\n\t~p\n\t~p\n\t~p\n",
              [Reason, Error, Res, erlang:get_stacktrace()]),
    clog(C, internal_error, "\"~p@~p\"", [Why, Cmd#cmd.lineno]),
    C2 = opt_late_sync_reply(C#cstate{expected = undefined}),
    C3 = close_logs(C2),
    send_reply(C3, C3#cstate.parent, {stop, self(), Res}),
    close_and_exit(C3, Reason, Res).

close_logs(#cstate{stdin_log_fd = InFd, stdout_log_fd = OutFd} = C) ->
    Waste = flush_port(C, C#cstate.flush_timeout, C#cstate.actual),
    clog(C, skip, "\"~s\"", [lux_utils:to_string(Waste)]),
    catch file:close(element(2, InFd)),
    catch file:close(element(2, OutFd)),
    C#cstate{log_fun = closed,
             event_log_fd = closed, % Leave the log open for other processes
             stdin_log_fd = closed,
             stdout_log_fd = closed}.

flush_logs(#cstate{event_log_fd = {_,EventFd},
                   stdin_log_fd = {_,InFd},
                   stdout_log_fd = {_,OutFd}}) ->
    file:sync(EventFd),
    file:sync(InFd),
    file:sync(OutFd),
    ok;
flush_logs(#cstate{event_log_fd = closed,
                   stdin_log_fd = closed,
                   stdout_log_fd = closed}) ->
    ok.

wait_for_down(C, Res) ->
    lux:trace_me(40, C#cstate.name, wait_for_down, []),
    receive
        {sync, From, When} ->
            C2 = opt_sync_reply(C, From, When),
            wait_for_down(C2, Res);
        {switch_cmd, From, _When, NewCmd, CmdStack, Fun} ->
            C2 = switch_cmd(C, From, NewCmd, CmdStack, Fun),
            wait_for_down(C2, Res);
        {change_mode, From, Mode, Cmd, CmdStack}
          when Mode =:= resume; Mode =:= suspend ->
            C2 = change_mode(C, From, Mode, Cmd, CmdStack),
            wait_for_down(C2, Res);
        {'DOWN', _, process, Pid, Reason} when Pid =:= C#cstate.parent ->
            close_and_exit(C, Reason, Res);
        {'EXIT', Port, _PosixCode} when Port =:= C#cstate.port ->
            C2 = C#cstate{state_changed = true,
                          no_more_output = true,
                          events = save_event(C, recv, shell_exit)},
            wait_for_down(C2, Res)
    end.

save_event(#cstate{latest_cmd = _Cmd, events = Events} = C, Op, Data) ->
    clog(C, Op, "\"~s\"", [lux_utils:to_string(Data)]),
    %% [{Cmd#cmd.lineno, Op, Data} | Events].
    Events.

safe_send_after(State, Timeout, Pid, Msg) ->
    case multiply(State, Timeout) of
        infinity   -> infinity;
        NewTimeout -> erlang:send_after(NewTimeout, Pid, Msg)
    end.

multiply(#cstate{multiplier = Factor}, Timeout) ->
    case Timeout of
        infinity ->
            infinity;
        _ ->
            lux_utils:multiply(Timeout, Factor)
    end.

clog(#cstate{event_log_fd = closed},
     _Op, _Format, _Args) ->
    ok;
clog(#cstate{progress = Progress,
             log_fun = LogFun,
             event_log_fd = Fd,
             name = Shell,
             latest_cmd = Cmd},
     Op, Format, Args) ->
    E = {event, Cmd#cmd.lineno, Shell, Op, Format, Args},
    lux_log:write_event(Progress, LogFun, Fd, E).

dlog(C, Level, Format, Args) when C#cstate.debug_level >= Level ->
    clog(C, debug, Format, Args);
dlog(_C, _Level, _Format, _Args) ->
    ok.

expand_vars(#cstate{submatches   = SubMatches,
                    macro_dict   = MacroDict,
                    dict         = Dict,
                    builtin_dict = BuiltinDict,
                    system_dict  = SystemDict},
            Bin,
            MissingVar) ->
    Fun = fun(nosubmatch, {N, Acc}) ->
                  {N+1, Acc}; % Omit $N as its value is undefined
             (Sub, {N, Acc}) ->
                  List = [integer_to_list(N), $=, binary_to_list(Sub)],
                  {N+1, [lists:flatten(List) | Acc]}
          end,
    {_,SubDict} = lists:foldl(Fun, {1, []}, SubMatches),
    Dicts = [SubDict, MacroDict, Dict, BuiltinDict, SystemDict],
    lux_utils:expand_vars(Dicts, Bin, MissingVar).
