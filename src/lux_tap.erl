%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2015 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_tap).

-export([open/1, close/1]).
-export([plan/3, test/4, diag/2, bail_out/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TAP - Test Anything Protocol
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tap()         :: ( [version()] plan() line()+ )
%%                | ( [version()] line()+ plan() )
%% version()     :: "TAP version" integer()
%% plan ()       :: integer() ".." integer() [directive()]
%% line()        :: test() | diag() | bail()
%% diag()        :: "#" string()
%% test()        :: outcome() [description()] [directive()]
%% outcome()     :: "ok" | "not ok"
%% description() :: string()
%% directive()   :: "#" ( todo() | skip() )
%% todo()        :: "TODO" string()
%% skip()        :: "SKIP" string()
%% bail()        :: "Bail out!" string()
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(tap,
        {
          receivers = [] :: [stdout|stderr|{fd, file:filename(), file:fd()}]
        }).

open(Opts) ->
    open(Opts, #tap{}).

open([Opt|Opts], TAP) ->
    if
        Opt =:= stdout; Opt =:= "stdout" ->
            Recvs = [stdout | TAP#tap.receivers],
            open(Opts, TAP#tap{receivers = Recvs});
        Opt =:= stderr; Opt =:= "stderr" ->
            Recvs = [stderr | TAP#tap.receivers],
            open(Opts, TAP#tap{receivers = Recvs});
        is_list(Opt) ->
            File = Opt,
            case file:open(File, [write, raw]) of
                {ok, Fd} ->
                    Recvs = [{fd, File, Fd} | TAP#tap.receivers],
                    open(Opts, TAP#tap{receivers = Recvs});
                {error, Reason} ->
                    file_wrapper(File, {error, Reason})
            end
    end;
open([], TAP) ->
    {ok, TAP}.

plan(TAP, N, "") when N =:= 0 ->
    return_worst([
                  %% write(TAP, ["TAP version 13"]),
                  write(TAP, ["1..", N, dir("SKIP all test cases")])
                 ]);
plan(TAP, N, Dir) when is_integer(N), N >= 0 ->
    return_worst([
                  %% write(TAP, ["TAP version 13"]),
                  write(TAP, ["1..", N, dir(Dir)])
                 ]).

test(TAP, not_ok, Descr, Dir) ->
    test(TAP, 'not ok', Descr, Dir);
test(TAP, Outcome, Descr, Dir)
  when Outcome =:= ok; Outcome == 'not ok' ->
    write(TAP, [Outcome, descr(Descr), dir(Dir)]).

diag(TAP, Descr) when Descr =:= "\n" ->
    write(TAP, []);
diag(TAP, Descr) when Descr =/= "" ->
    write(TAP, ["# ", Descr]).

bail_out(TAP, Descr) ->
    return_worst([
                  write(TAP, ["Bail out!", descr(Descr)]),
                  close(TAP)
                 ]).

descr(Descr) ->
    case Descr of
        "" -> Descr;
        _  -> " " ++ Descr
    end.

dir(Dir) ->
    case Dir of
        ""          -> Dir;
        "SKIP" ++ _ -> " # " ++ Dir;
        "TODO" ++ _ -> " # " ++ Dir
        %%_           -> " # " ++ Dir % Accept anyway
    end.

write(TAP, Things) ->
    String = lists:concat(Things) ++ "\n",
    Write =
        fun(R) ->
                case R of
                    stdout ->
                        io:format(standard_io, "~s", [String]);
                    stderr ->
                        io:format(standard_error, "~s", [String]);
                    {fd, File, Fd} ->
                        file_wrapper(File, file:write(Fd, String))
                end
            end,
    return_worst(lists:map(Write, TAP#tap.receivers)).

close(TAP) ->
    Close =
        fun(R) ->
                case R of
                    stdout ->
                        ok;
                    stderr ->
                        ok;
                    {fd, File, Fd} ->
                        file_wrapper(File, file:close(Fd))
                end
        end,
    return_worst(lists:map(Close, TAP#tap.receivers)).

return_worst([]) ->
    ok;
return_worst([H|T]) ->
    return_worst(T, H).

return_worst([H|T], Worst) ->
    case Worst of
        {error, _Reason} ->
            Worst;
        _ ->
            return_worst(T, H)
    end;
return_worst([], Worst) ->
    Worst.

file_wrapper(File, Res) ->
    case Res of
        {error, Reason} ->
            {error, File ++ ": " ++ file:format_error(Reason)};
        Other ->
            Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% These are behaviors desired in all TAP parsers:
%%
%% - Should work on the TAP as a stream (ie. as each line is received)
%%   rather than wait until all the TAP is received.
%% - The TAP source should be pluggable (ie. don't assume its always
%%   coming from a Perl program).
%% - The TAP display should be pluggable.
%% - Should be able to gracefully handle future upgrades to TAP.
%% - Should be forward compatible.
%% - Ignore unknown directives.
%% - Ignore any unparsable lines.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
