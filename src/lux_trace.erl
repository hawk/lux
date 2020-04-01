%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2020 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_trace).

-export([
         display/2
        ]).

-include("lux.hrl").

-define(WILDCARD, '_').

-type field_action() ::
          keep |         % Keep as is
          annotate |     % Annotate field value with its name
          field  |       % Field name
          value |        % Field value
          skip.          % Skip field
-type record_action() ::
          record |       % Record name
          field_action().
-type call_action() ::
          deep |         % Follow call stack
          shallow |      % Keep call but do not recurse
          skip.          % Skip call
-type field_id() :: atom() | non_neg_integer().
-type field_filter() :: {field_id(), field_action()}.
-type data_filter() :: {'record', atom(), [field_filter()], record_action()}.
-type call_filter() ::
          {'call', {M::?WILDCARD|atom(),
                    F::?WILDCARD|'fun'|atom(),
                    A::?WILDCARD|non_neg_integer()},
                    call_action(),
                    record_action()}.

-record(level,
        {pid    :: pid(),
         indent :: non_neg_integer(),
         max    :: infinity | non_neg_integer()
        }).

-record(state,
        {reply_to     :: pid(),
         levels       :: [#level{}],
         call_filters :: [call_filter()],
         data_filters :: [data_filter()]
        }).

display(TraceFile, FilterFile) ->
    case filelib:is_regular(TraceFile) of
        true ->
            case file:consult(FilterFile) of
                {ok, AllFilters} ->
                    CallFilters =
                        [F || {call, {MF,FF,AF}, CallAction, DataAction} =
                                  F <- AllFilters,
                                       is_atom(MF), is_atom(FF), is_atom(AF),
                                       is_atom(CallAction),
                                       is_atom(DataAction)],
                    DataFilters =
                        [F || {record, Name, FieldFilters, RecAction} =
                                  F <- AllFilters,
                                       is_atom(Name),
                                       is_list(FieldFilters),
                                       is_atom(RecAction)],
                    io:format("Read ~p trace filters from:\n"
                              "\t--filter_trace=~s\n",
                              [length(AllFilters), FilterFile]),
                    io:format("Applying call filters:\n\t~p\n",
                              [CallFilters]),
                    io:format("Applying data filters:\n\t~p\n",
                              [DataFilters]),
                    BadF = (AllFilters -- CallFilters) -- DataFilters,
                    io:format("Ignore bad filters:\n\t~p\n", [BadF]),
                    io:format("\n", []),
                    S = #state{reply_to = self(),
                               levels = [],
                               call_filters = CallFilters,
                               data_filters = DataFilters},
                    start_trace_client(TraceFile, S);
                {error, FileReason} when is_atom(FileReason) ->
                    StrReason = file:format_error(FileReason),
                    io:format("ERROR: ~s: ~s\n", [FilterFile, StrReason]),
                    1;
                {error, {Line, Mod, Term}} ->
                    StrReason = file:format_error({Line, Mod, Term}),
                    io:format("ERROR: ~s: ~s\n", [FilterFile, StrReason]),
                    1
            end;
        false ->
            io:format("ERROR: ~s: no such file\n", [TraceFile]),
            1
    end.

start_trace_client(TraceFile, S) ->
    HandlerSpec = {fun trace_client/2, S},
    ClientPid = dbg:trace_client(file, TraceFile, HandlerSpec),
    link(ClientPid),
    receive
        end_of_trace ->
            0;
        {'EXIT', Reason} ->
            io:format("ERROR: ~s: ~p\n", [TraceFile, Reason]),
            1
    end.

trace_client(Trace = end_of_trace, #state{reply_to = ReplyTo}) ->
    ReplyTo ! Trace;
trace_client(Trace, S) when tuple_size(Trace) >= 3 ->
    {Action, NewTrace} = filter(Trace, S),
    Pid = element(2, Trace),
    Type = element(3, Trace),
    {NewAction, Indent, NewS} = update_level(Pid, Type, S, Action),
    case NewAction of
        keep ->
            Prefix = lists:duplicate(Indent, $ ),
            io:format("~s~p.\n", [Prefix, NewTrace]);
        skip ->
            ok
    end,
    NewS.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filter

filter(Trace, S) when is_tuple(Trace),
                      element(1, Trace) =:= trace_ts ->
    [trace_ts | Rest] = tuple_to_list(Trace),
    Rest2 = lists:reverse(tl(lists:reverse(Rest))),
    filter(list_to_tuple([trace | Rest2]), S);
filter({trace, Pid, Type = call, {M,F,A} = MFA}, S) ->
    {CallAction, [NewA]} = filter_call(MFA, [A], S),
    {CallAction, {trace, Pid, Type, {M,F,NewA}}};
filter({trace, Pid, Type = return_from, {M,F,A} = MFA, RetVal}, S) ->
    {CallAction, [NewA, NewRetVal]} = filter_call(MFA, [A, RetVal], S),
    {CallAction, {trace, Pid, Type, {M,F,NewA}, NewRetVal}};
filter({trace, Pid, Type = exception_from, {M,F,A} = MFA, {Class,Reason}}, S) ->
    {CallAction, [NewA, NewReason]} = filter_call(MFA, [A, Reason], S),
    {CallAction, {trace, Pid, Type, {M,F,NewA}, {Class,NewReason}}};
filter(Trace, S) when element(1, Trace) =:= trace ->
    TopFs = S#state.data_filters,
    TopAct = keep,
    Top = {TopFs, TopAct},
    NewTrace = filter_data(Trace, TopFs, TopAct, Top),
    {keep, NewTrace}.

filter_call(MFA, Vals, S) ->
    {CallAction, DataAction} =
        lookup_call_filter(MFA, S#state.call_filters, deep, keep),
    TopFs = S#state.data_filters,
    TopAct = DataAction,
    Top = {TopFs, TopAct},
    NewVals = [filter_data(V, TopFs, TopAct, Top) || V <- Vals],
    {CallAction, NewVals}.

filter_data(Rec, Filters, RecDefault, Top) when is_tuple(Rec) ->
    Name = element(1, Rec),
    {FieldFilters, RecAction} =
        lookup_record_filter(Name, Filters, RecDefault),
    filter_record(Name, Rec, FieldFilters, RecAction, Top);
filter_data(List, Filters, Default, Top) when is_list(List) ->
    [filter_data(Elem, Filters, Default, Top) || Elem <- List];
filter_data(Other, _Filters, _Default, _Top) ->
    Other.

filter_record(_Name, Rec, [], keep, _Top) ->
    Rec; % Keep as is
filter_record(Name, _Rec, [], record, _Top) when is_atom(Name) ->
    {Name}; % Keep only name
filter_record(_Name, Rec, [], record, Top) ->
    filter_record(tuple, Rec, [], value, Top);
filter_record(Name, Rec, Filters, Default, Top) ->
    Vals = tuple_to_list(Rec),
    case fields(Name) of
        false ->
            Keys = lists:seq(1, tuple_size(Rec)),
            Filtered = filter_fields(tuple, Keys, Vals, Filters, Default, Top),
            list_to_tuple(Filtered);
        Keys ->
            Filtered =
                filter_fields(Name, Keys, tl(Vals), Filters, Default, Top),
            list_to_tuple([Name | Filtered])
    end.

filter_fields(Name, [Key|Keys], [Val|Vals], Filters, Default, Top) ->
    Action = lookup_field_filter(Key, Filters, Default),
    filter_fields2(Name, Key, Keys, Val, Vals, Action, Filters, Default, Top);
filter_fields(_Name, [], [], _Filters, _Default, _Top) ->
    [].

filter_fields2(Name, Key, Keys, Val, Vals, Action, Filters, Default, Top) ->
    case Action of
        annotate ->                              % Both field name and value
            {TopFs, TopAct} = Top,
            Val2 = filter_data(Val, TopFs, TopAct, Top),
            [{Key,'=', Val2} |
             filter_fields(Name, Keys, Vals, Filters, Default, Top)];
        field ->                                 % Only field name
            [Key | filter_fields(Name, Keys, Vals, Filters, Default, Top)];
        value ->                                 % Keep value as is
            {TopFs, TopAct} = Top,
            Val2 = filter_data(Val, TopFs, TopAct, Top),
            [Val2 | filter_fields(Name, Keys, Vals, Filters, Default, Top)];
        skip ->                                  % Skip field
            filter_fields(Name, Keys, Vals, Filters, Default, Top)
    end.

lookup(Key, Pos, List, Default) ->
    case lists:keyfind(Key, Pos, List) of
        false  -> Default;
        KeyVal -> KeyVal
    end.

lookup_record_filter(Name, DataFilters, RecDefault) ->
    case lookup(Name, 2, DataFilters, false) of
        false when Name =:= ?WILDCARD ->
            {[], RecDefault};
        false ->
            lookup_record_filter(?WILDCARD, DataFilters, RecDefault);
        {record, Name, FieldFilters, RecAction}
          when is_atom(Name), is_list(FieldFilters), is_atom(RecAction) ->
            {FieldFilters, RecAction};
        BadF ->
            io:format("\n ERROR: Illegal field filter: ~p\n", [BadF]),
            exit(illegal_record_filter)
    end.

lookup_field_filter(Id, FieldFilters, Default) ->
    case lookup(Id, 1, FieldFilters, false) of
        false when  Id =:= ?WILDCARD ->
            Default;
        false ->
            lookup_field_filter(?WILDCARD, FieldFilters, Default);
        {Id, Action} when is_atom(Id), is_atom(Action) ->
            Action;
        {Id, Action} when is_integer(Id), is_atom(Action) ->
            Action;
        BadF ->
            io:format("\n ERROR: Illegal field filter: ~p\n", [BadF]),
            exit(illegal_field_filter)
    end.

lookup_call_filter({M,F,A} = MFA,
                   [{call, {MF,FF,AF}, CallAction, DataAction} | Fs],
                   CallDefault,
                   DataDefault)
  when is_atom(MF), is_atom(FF), is_atom(AF),
       is_atom(CallAction), is_atom(DataAction) ->
    if (M =:= MF orelse MF =:= ?WILDCARD) andalso
       (F =:= FF orelse FF =:= ?WILDCARD) andalso
       (A =:= AF orelse AF =:= ?WILDCARD) ->
            {CallAction, DataAction};
       true ->
            lookup_call_filter(MFA, Fs, CallDefault, DataDefault)
    end;
lookup_call_filter({_,_,_}, [BadF | _], _CallDefault, _DataDefault) ->
    io:format("\n ERROR: Illegal call filter: ~p\n", [BadF]),
    exit(illegal_call_filter);
lookup_call_filter({_,_,_}, [], CallDefault, DataDefault) ->
    {CallDefault, DataDefault}.

lookup_level(Pid, #state{levels = Levels}) ->
    Default = #level{pid = Pid, indent = 0, max = infinity},
    lookup(Pid, #level.pid, Levels, Default).

update_level(Pid, Type, S, Action) ->
    L = #level{indent = Indent, max = Max} = lookup_level(Pid, S),
    NewIndent = update_indent(Type, Indent),
    {NewAction, NewMax} = update_max(Max, Indent, NewIndent, Action),
    NewL = L#level{indent = NewIndent, max = NewMax},
    Levels = S#state.levels,
    NewLevels = lists:keystore(Pid, #level.pid, Levels, NewL),
    NewS = S#state{levels = NewLevels},
    {NewAction, Indent, NewS}.

update_indent(Type, Indent) ->
    case Type of
        call           -> Indent + 1;
        return_from    -> Indent - 1;
        exception_from -> Indent - 1;
        _Other         -> Indent
    end.

update_max(Max, Indent, NewIndent, Action) ->
    if
        Max =/= infinity, Indent >= Max ->
            %% Max level already reached
            if
                NewIndent < Max ->
                    %% Remove limit
                    {skip, infinity};
                true ->
                    %% Keep limit
                    {skip, Max}
            end;
        true ->
            %% Below max
            case Action of
                keep ->
                    %% Keep potential limit
                    {keep, Max};
                deep ->
                    %% Keep potential limit
                    {keep, Max};
                shallow ->
                    %% Set new limit
                    {keep, NewIndent};
                skip ->
                    %% Set new limit
                    {skip, Indent}
            end
    end.

fields(Name) ->
    case Name of
        istate      -> record_info(fields, istate);
        cstate      -> record_info(fields, cstate);
        rstate      -> record_info(fields, rstate);
        pstate      -> record_info(fields, pstate);
        pattern     -> record_info(fields, pattern);
        timer       -> record_info(fields, timer);
        cmd         -> record_info(fields, cmd);
        shell       -> record_info(fields, shell);
        cmd_pos     -> record_info(fields, cmd_pos);
        warning     -> record_info(fields, warning);
        result      -> record_info(fields, result);
        break       -> record_info(fields, break);
        macro       -> record_info(fields, macro);
        loop        -> record_info(fields, loop);
        debug_shell -> record_info(fields, debug_shell);
        run         -> record_info(fields, run);
        source      -> record_info(fields, source);
        event       -> record_info(fields, event);
        body        -> record_info(fields, body);
        _           -> false
    end.
