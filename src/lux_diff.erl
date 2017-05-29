%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2009-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implements E. W. Myers diff algorithm (finding the Longest Common
%% Subsequence) described in his paper "An O(ND) Difference Algorithm
%% and Its Variations" found at http://www.xmailserver.org/diff2.pdf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_diff).

-export([
         compare/2, compare2/3,
         split_diff/1,
         apply_verbose_diff/2, apply_compact_diff/2,
         test/0, test2/2
        ]).

-type elem() :: term().
-type elem_list() :: [elem()].
-type compact_diff() ::
        [ {'+', elem_list()} | {'-', elem_list()} |  elem_list() ].
-type verbose_diff() :: [ {'+', elem()}   | {'-', elem()}   | {'=', elem()} ].
-type match_fun()    :: fun((A :: elem(), B :: elem()) -> boolean()) .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% We can reduce the problem to only diffing the parts that occurs in
%% both lists. The other elements can be merged in afterwards. This
%% greatly speeds up the case when only a few elements are the same.

-spec(compare(A::elem_list(), B::elem_list()) -> compact_diff()).
compare(A, A) ->
    [A];
compare(A, B) ->
    ASame = A -- (A -- B),
    BSame = B -- (B -- A),
    Fun = default_match(),
    CompactDiff = compare2(ASame, BSame, Fun),
    merge_unique(A, B, CompactDiff, []).

merge_unique([], [], [], Acc) ->
    merge_cleanup(Acc, []);
merge_unique(As, Bs, [{'-', _Dels=[D]}|Es], Acc) ->
    {ADels, NewAs} = grab_until(As, D, [], add),
    merge_unique(NewAs, Bs, Es, [{'-',ADels}|Acc]);
merge_unique(As, Bs, [{'-', _Dels=[D|Drest]}|Es], Acc) ->
    {ADels, NewAs} = grab_until(As, D, [], add),
    merge_unique(NewAs, Bs, [{'-',Drest}|Es], [{'-',ADels}|Acc]);
merge_unique(As, Bs, [{'+', _Adds=[A]}|Es], Acc) ->
    {BAdds, NewBs} = grab_until(Bs, A, [], add),
    merge_unique(As, NewBs, Es, [{'+',BAdds}|Acc]);
merge_unique(As, Bs, [{'+', _Adds=[A|Arest]}|Es], Acc) ->
    {BAdds, NewBs} = grab_until(Bs, A, [], add),
    merge_unique(As, NewBs, [{'+',Arest}|Es], [{'+',BAdds}|Acc]);
merge_unique(As, Bs, [[K]|Es], Acc) ->
    {ADels, NewAs} = grab_until(As, K, [], nadd),
    {BAdds, NewBs} = grab_until(Bs, K, [], nadd),
    merge_unique(NewAs, NewBs, Es, [[K],{'+',BAdds},{'-',ADels}|Acc]);
merge_unique(As, Bs, [[K|Krest]|Es], Acc) ->
    {ADels, NewAs} = grab_until(As, K, [], nadd),
    {BAdds, NewBs} = grab_until(Bs, K, [], nadd),
    merge_unique(NewAs, NewBs, [Krest|Es], [[K],{'+',BAdds},{'-',ADels}|Acc]);
merge_unique(As, Bs, [], Acc) ->
    NewAcc = [{'+',Bs},{'-',As}|Acc],
    merge_cleanup(NewAcc, []).

grab_until([X|Xs], X, Acc, add) ->
    {lists:reverse([X|Acc]), Xs};
grab_until([X|Xs], X, Acc, _) ->
    {lists:reverse(Acc), Xs};
grab_until([X|Xs], Y, Acc, Add) ->
    grab_until(Xs, Y, [X|Acc], Add).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calcuate shortest edit list to go from A to B

-spec(compare2(A::elem_list(), B::elem_list(), Fun::match_fun()) ->
             compact_diff()).
compare2(A, B, Fun) ->
    DataA = list_to_tuple(A),
    DataB = list_to_tuple(B),
    DownVector = ets:new(down, [private]),
    UpVector   = ets:new(up, [private]),
    try
        Diff = lcs(DataA, 0, size(DataA),
                   DataB, 0, size(DataB),
                   DownVector, UpVector,
                   Fun, []),
        merge_cleanup(Diff,[])
    after
        ets:delete(DownVector),
        ets:delete(UpVector)
    end.

merge_cleanup([], Acc) ->
    Acc;
merge_cleanup([{Sign, P}|Rest], Acc) ->
    merge_cleanup_sign(Sign, Rest, Acc, P);
merge_cleanup([Keep|Rest], Acc) ->
    merge_cleanup_keep(Rest, Acc, Keep).

merge_cleanup_sign(_Sign, Rest, Acc, []) ->
    %% Empty sublist
    merge_cleanup(Rest, Acc);
merge_cleanup_sign(Sign, [{Sign,Add}|Rest], Acc, AddAcc) ->
    %% Merge same sign
    merge_cleanup_sign(Sign, Rest, Acc, Add++AddAcc);
merge_cleanup_sign('-', [{'+',Add}|Rest], Acc, AddAcc) ->
    %% Reorder consecutive signs - always keep same order
    merge_cleanup_sign('+', [{'-',AddAcc}|Rest], Acc, Add);
merge_cleanup_sign(Sign1, [{Sign2,Add}|Rest], Acc, AddAcc)
  when Add =:= AddAcc, Sign1 =/= Sign2 ->
    %% Merge add and delete of same sublist
    merge_cleanup_keep(Rest, Acc, AddAcc);
merge_cleanup_sign(Sign1, [{Sign2,Add2}, {Sign1,Add1}|Rest], Acc, AddAcc)
  when Sign1 =/= Sign2 ->
    %% Merge add and delete of for Add+Del+Add or Del+Add+Del
    merge_cleanup_sign(Sign1,  [{Sign2,Add2}|Rest], Acc, Add1++AddAcc);
merge_cleanup_sign(Sign, Rest, Acc, AddAcc) ->
    %% Add to acc
    merge_cleanup(Rest, [{Sign, AddAcc}|Acc]).

merge_cleanup_keep(Rest, Acc, []) ->
    %% Empty sublist
    merge_cleanup(Rest, Acc);
merge_cleanup_keep([], Acc, KeepAcc) ->
    [KeepAcc|Acc];
merge_cleanup_keep([{_,[]}|Es], Acc, KeepAcc) ->
    merge_cleanup_keep(Es, Acc, KeepAcc);
merge_cleanup_keep(Es=[{_,_}|_], [Prev|Acc], KeepAcc) when is_list(Prev) ->
    %% Merge subsequent common sublists
    merge_cleanup(Es, [KeepAcc++Prev|Acc]);
merge_cleanup_keep(Es=[{_,_}|_], Acc, KeepAcc) ->
    %% Add to acc
    merge_cleanup(Es, [KeepAcc|Acc]);
merge_cleanup_keep([Keep|Rest], Acc, KeepAcc) ->
    %% Merge subsequent common sublists
    merge_cleanup_keep(Rest, Acc, Keep++KeepAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Longest Common Subsequence

lcs(DataA, LowerA0, UpperA0,
    DataB, LowerB0, UpperB0,
    DownVector, UpVector,
    Fun, Acc0) ->
    %% Skip equal lines at start
    {LowerA, LowerB, StartKeep} =
        whilex(
          fun({LA, LB, StartAcc}) ->
                  if
                      LA < UpperA0, LB < UpperB0 ->
                          DA = element(LA+1, DataA),
                          DB = element(LB+1, DataB),
                          case Fun(DA, DB) of
                              true ->
                                  {LA+1, LB+1, [DA|StartAcc]};
                              false ->
                                  {false, {LA, LB, lists:reverse(StartAcc)}}
                          end;
                      true ->
                          {false, {LA, LB, lists:reverse(StartAcc)}}
                  end
          end, {LowerA0, LowerB0, []}),
    %% Skip equal lines at end
    {UpperA, UpperB, EndKeep} =
        whilex(
          fun({UA, UB, EndAcc}) ->
                  if
                      LowerA < UA, LowerB < UB ->
                          DA = element(UA, DataA),
                          DB = element(UB, DataB),
                          case Fun(DA, DB) of
                              true ->
                                  {UA-1, UB-1, [DA|EndAcc]};
                              false ->
                                  {false, {UA, UB, EndAcc}}
                          end;
                      true ->
                          {false, {UA, UB, EndAcc}}
                  end
          end, {UpperA0, UpperB0, []}),

    if
        LowerA =:= UpperA, LowerB < UpperB ->
            %% Mark as inserted
            Inserted = [element(I, DataB) || I <- lists:seq(LowerB+1, UpperB)],
            acc_add(EndKeep,acc_add({'+',Inserted},acc_add(StartKeep,Acc0)));
        LowerA =:= UpperA ->
            acc_add(StartKeep++EndKeep, Acc0);
        LowerB =:= UpperB, LowerA < UpperA ->
            %% Mark as deleted
            Deleted = [element(I, DataA) || I <- lists:seq(LowerA+1, UpperA)],
            acc_add(EndKeep,acc_add({'-',Deleted},acc_add(StartKeep,Acc0)));
        LowerB =:= UpperB ->
            acc_add(StartKeep++EndKeep, Acc0);
        true ->
            {SX, SY} = sms(DataA, LowerA, UpperA,
                           DataB, LowerB, UpperB,
                           DownVector, UpVector,
                           Fun),
            Acc1 = lcs(DataA, LowerA, SX,
                       DataB, LowerB, SY,
                       DownVector, UpVector,
                       Fun, acc_add(StartKeep,Acc0)),
            Acc2 = lcs(DataA, SX, UpperA,
                       DataB, SY, UpperB,
                       DownVector, UpVector,
                       Fun, Acc1),
            acc_add(EndKeep, Acc2)
    end.

acc_add([],Acc) ->
    Acc;
acc_add({_Sign,[]}, Acc) ->
    Acc;
acc_add(E, Acc) ->
    [E|Acc].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Find Shortest Middle Sname

sms(DataA, LowerA, UpperA,
    DataB, LowerB, UpperB,
    DownVector, UpVector,
    Fun) ->
    DownK = LowerA - LowerB,
    UpK = UpperA - UpperB,
    %%
    Delta = (UpperA-LowerA) - (UpperB-LowerB),
    OddDelta = ((Delta band 1) /= 0),
    %%
    MaxD = ((UpperA - LowerA + UpperB - LowerB) div 2) + 1,
    %%
    vset(DownVector, DownK+1, LowerA),
    vset(UpVector, UpK-1, UpperA),
    %%
    sms_d(0, MaxD,
          DataA, LowerA, UpperA,
          DataB, LowerB, UpperB,
          DownVector, UpVector, DownK, UpK,
          Delta, OddDelta, Fun).

%% D loop
sms_d(D, MaxD,
      _DataA, _LowerA, _UpperA,
      _DataB, _LowerB, _UpperB,
      _DownVector,_UpVector,_DownK,_UpK,
      _Delta, _OddDelta, _Fun) when D > MaxD ->
    throw(error);
sms_d(D, MaxD,
      DataA, LowerA, UpperA,
      DataB, LowerB, UpperB,
      DownVector, UpVector, DownK, UpK,
      Delta, OddDelta, Fun) ->
    case sms_d_k_f(DownK-D, D,
                   DataA, LowerA, UpperA,
                   DataB, LowerB, UpperB,
                   DownVector, UpVector, DownK, UpK,
                   Delta, OddDelta, Fun) of
        not_found ->
            case sms_d_k_r(UpK-D, D,
                           DataA, LowerA, UpperA,
                           DataB, LowerB, UpperB,
                           DownVector, UpVector, DownK, UpK,
                           Delta, OddDelta, Fun) of
                not_found ->
                    sms_d(D+1, MaxD,
                          DataA, LowerA, UpperA,
                          DataB, LowerB, UpperB,
                          DownVector, UpVector, DownK, UpK,
                          Delta, OddDelta, Fun);
                Point -> Point
            end;
        Point -> Point
    end.

%% Forward snake
sms_d_k_f(K, D,
          _DataA, _LowerA, _UpperA,
          _DataB, _LowerB, _UpperB,
          _DownVector, _UpVector, DownK, _UpK,
          _Delta, _OddDelta, _Fun)
  when K > (DownK+D) ->
    not_found;
sms_d_k_f(K, D,
          DataA, LowerA, UpperA,
          DataB, LowerB, UpperB,
          DownVector, UpVector, DownK, UpK,
          Delta, OddDelta, Fun) ->
    if
        K =:= (DownK - D) ->
            X = vget(DownVector, K+1); %% Down
        true ->
            Right = vget(DownVector, K-1)+1,
            Down = vget(DownVector, K+1),
            if
                (K < (DownK+D)) and (Down >= Right) ->
                    X = Down; %% Down
                true ->
                    X = Right %% Right
            end
    end,
    Y = X - K,
    %% Walk diagonal
    {NewX, _NewY} =
        whilex(fun({XX, YY}) ->
                       if
                           XX < UpperA, YY < UpperB ->
                               DA = element(XX+1, DataA),
                               DB = element(YY+1, DataB),
                               case Fun(DA, DB) of
                                   true ->
                                       {XX+1, YY+1};
                                   false ->
                                       {false, {XX, YY}}
                               end;
                           true ->
                               {false, {XX, YY}}
                       end
               end, {X, Y}),

    vset(DownVector, K, NewX),
    %% Overlap?
    if
        OddDelta, (UpK-D) < K, K < (UpK+D) ->
            UpVK = vget(UpVector, K),
            DownVK = vget(DownVector, K),
            if
                UpVK =< DownVK ->
                    {DownVK, DownVK-K};
                true ->
                    sms_d_k_f(K+2, D,
                              DataA, LowerA, UpperA,
                              DataB, LowerB, UpperB,
                              DownVector, UpVector, DownK, UpK,
                              Delta, OddDelta, Fun)
            end;
        true ->
            sms_d_k_f(K+2, D,
                      DataA, LowerA, UpperA,
                      DataB, LowerB, UpperB,
                      DownVector, UpVector, DownK, UpK,
                      Delta, OddDelta, Fun)
    end.

%% Backward snake
sms_d_k_r(K, D,
          _DataA, _LowerA, _UpperA,
          _DataB, _LowerB, _UpperB,
          _DownVector, _UpVector, _DownK, UpK,
          _Delta, _OddDelta, _Fun)
  when K > (UpK+D) ->
    not_found;
sms_d_k_r(K, D,
          DataA, LowerA, UpperA,
          DataB, LowerB, UpperB,
          DownVector, UpVector, DownK, UpK,
          Delta, OddDelta, Fun) ->
    if
        K =:= (UpK+D) ->
            X = vget(UpVector, K-1); %% Up
        true ->
            Up = vget(UpVector, K-1),
            Left = vget(UpVector, K+1)-1,
            if
                K > (UpK-D), Up < Left ->
                    X = Up;
                true ->
                    X = Left
            end
    end,
    Y = X - K,
    %% Walk diagonal
    {NewX, _NewY} =
        whilex(fun({XX, YY}) ->
                       if
                           XX > LowerA, YY > LowerB ->
                               DA = element(XX, DataA),
                               DB = element(YY, DataB),
                               case Fun(DA, DB) of
                                   true ->
                                       {XX-1, YY-1};
                                   false ->
                                       {false, {XX, YY}}
                               end;
                           true ->
                               {false, {XX, YY}}
                       end
               end, {X, Y}),

    vset(UpVector, K, NewX),
    %% Overlap?
    if
        not(OddDelta), (DownK-D) =< K, K =< (DownK+D) ->
            UpVK = vget(UpVector, K),
            DownVK = vget(DownVector, K),
            if
                UpVK =< DownVK ->
                    {DownVK, DownVK-K};
                true ->
                    sms_d_k_r(K+2, D,
                              DataA, LowerA, UpperA,
                              DataB, LowerB, UpperB,
                              DownVector, UpVector, DownK, UpK,
                              Delta, OddDelta, Fun)
            end;
        true ->
            sms_d_k_r(K+2, D,
                      DataA, LowerA, UpperA,
                      DataB, LowerB, UpperB,
                      DownVector, UpVector, DownK, UpK,
                      Delta, OddDelta, Fun)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expand compact diff into long diff

-spec(split_diff(compact_diff()) -> verbose_diff()).
split_diff(Diff) ->
    split_diff(Diff, []).

split_diff([], Acc) ->
    lists:reverse(Acc);
split_diff([{_Sign, []}|Rest], Acc) ->
    split_diff(Rest, Acc);
split_diff([{Sign, [H|T]}|Rest], Acc) ->
    split_diff([{Sign, T}|Rest], [{Sign, H}|Acc]);
split_diff([[]|Rest], Acc) ->
    split_diff(Rest, Acc);
split_diff([[H|T]|Rest], Acc) ->
    split_diff([T|Rest], [{'=',H}|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Apply diff to list

-spec(apply_verbose_diff(A::elem_list(), Diff::verbose_diff()) ->
             elem_list()).
apply_verbose_diff(A, Diff) ->
    apply_verbose_diff(A, Diff, []).

apply_verbose_diff([], [], Acc) ->
    lists:reverse(Acc);
apply_verbose_diff(A, [{'+',C}|Rest], Acc) ->
    apply_verbose_diff(A, Rest, [C|Acc]);
apply_verbose_diff([A|As], [{'-',A}|Rest], Acc) ->
    apply_verbose_diff(As, Rest, Acc);
apply_verbose_diff([A|As], [{'=',A}|Rest], Acc) ->
    apply_verbose_diff(As, Rest, [A|Acc]).

-spec(apply_compact_diff(A::elem_list(), Diff::compact_diff()) ->
             elem_list()).
apply_compact_diff(A, Diff) ->
    apply_compact_diff(A, Diff, []).

apply_compact_diff([], [], Acc) ->
    lists:reverse(Acc);
apply_compact_diff(A, [{'+',Add}|Rest], Acc) when is_list(Add) ->
    apply_compact_diff(A, Rest, lists:reverse(Add, Acc));
apply_compact_diff(A, [{'-',Del}|Rest], Acc) when is_list(Del) ->
    ARest = lists:nthtail(length(Del), A),
    apply_compact_diff(ARest, Rest, Acc);
apply_compact_diff(A, [Keep|Rest], Acc) when is_list(Keep) ->
    ARest = lists:nthtail(length(Keep), A),
    apply_compact_diff(ARest, Rest, lists:reverse(Keep,Acc)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utils

whilex(Fun, Acc) ->
    case Fun(Acc) of
        {false, Res} ->
            Res;
        Acc2 ->
            whilex(Fun, Acc2)
    end.

vget(T, I) ->
    case ets:lookup(T, I) of
        [{_,V}|_]  -> V;
        _ -> false
    end.

vset(T, I, V) ->
    ets:insert(T, {I,V}).

-spec(default_match() -> match_fun()).
default_match() ->
    fun(A, B) -> A =:= B end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Self test

test() ->
    %% Special cases
    test2([],[]),
    test2([a,b,c],[a,b,c]),
    rand:seed(exsplus),
    Repetitions = 10000,
    MaxListLen = 100,
    ElemVariation = 100,
    test(Repetitions, MaxListLen, ElemVariation).

test(0, _Max, _Var) ->
    ok;
test(N, Max, Var) ->
    %% Create two random strings
    Populate =
        fun() ->
                Size = rand:uniform(Max)-1,
                if
                    Size > 0 ->
                        [rand:uniform(Var) || _ <- lists:seq(1,Size)];
                    true ->
                        []
                end
        end,
    A = Populate(),
    B = Populate(),
    case catch test2(A, B) of
        {ok, Time1, Time2} when (N rem 10) =:= 0 ->
            C = A -- (A -- B),
            D = B -- (B -- A),
            %% io:format("A=~p.\n", [A]),
            %% io:format("B=~p.\n", [B]),
            %% io:format("C=~p.\n", [C]),
            %% io:format("D=~p.\n", [D]),
            io:format("test ~p"
                      "\tA=~p"
                      "\tB=~p"
                      "\tC=~p"
                      "\tD=~p"
                      "\t~p"
                      "\t/ ~p"
                      "\t= ~p\ttimes speedup\n",
                      [N, length(A), length(B), length(C), length(D),
                       Time1, Time2,
                       case Time2 of 0 -> 0; _ -> Time1 div Time2 end]);
        {ok, _Time1, _Time2} ->
            ok;
        {'EXIT', Reason} = Exit ->
            io:format("Test ~p failed for:\n"
                      "  ~p:test2(\n"
                      "      ~p\n,"
                      "      ~p).\n"
                      "  %% ~p\n", [N,?MODULE,A,B,Exit]),
            timer:sleep(500),
            exit(Reason)
    end,
    test(N-1, Max, Var).

test2(A, B) ->
    Fun = default_match(),
    erlang:garbage_collect(),
    {Time2,CompactDiff1} = timer:tc(fun() -> compare(A ,B) end),
    erlang:garbage_collect(),
    {Time1,CompactDiff2} = timer:tc(fun() -> compare2(A, B, Fun) end),
    try
        [] = validate_diff(CompactDiff1, []),
        [] = validate_diff(CompactDiff2, []),
        VerboseComp3 = split_diff(CompactDiff1),
        B = apply_compact_diff(A, CompactDiff1),
        B = apply_compact_diff(A, CompactDiff2),
        B = apply_verbose_diff(A, VerboseComp3),
        {ok, Time1, Time2}
    catch
        error:Reason ->
            EST = erlang:get_stacktrace(),
            io:format("Test failed for:\n"
                      "  ~p:test2(\n"
                      "      ~p\n,"
                      "      ~p).\n",
                      [?MODULE,A,B]),
            io:format("ERROR:\n"
                      "  ~p\n"
                      "  ~p\n"
                      "  1: ~p\n"
                      "  2: ~p\n",
                      [Reason, EST, CompactDiff1, CompactDiff2]),
            timer:sleep(500),
            {error, Reason}
    end.

validate_diff([{S,_}=A, {S,_}=B | Rest], Acc) ->
    validate_diff([B | Rest], [{double_sign, A, B} | Acc]);
validate_diff([A, B | Rest], Acc)
  when is_list(A), is_list(B) ->
    validate_diff([B | Rest], [{double_share, A, B} | Acc]);
validate_diff([A, B, C | Rest], Acc)
  when is_tuple(A), is_tuple(B), is_tuple(C) ->
    validate_diff([B, C | Rest], [{triple_sign, A, B, C} | Acc]);
validate_diff([_A | Rest], Acc) ->
    validate_diff(Rest, Acc);
validate_diff([], Acc) ->
    Acc.
