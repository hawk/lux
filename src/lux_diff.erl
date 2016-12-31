%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2009-2017 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implements E. W. Myers diff altorithm described in his paper
%% "An O(ND) Difference Algorithm and Its Variations" found at
%% http://www.xmailserver.org/diff2.pdf

-module(lux_diff).

-export([compare/2, split_diff/1, apply_diff/2, test/0]).

%% @spec([term()],[term()]) -> [{'+',[term()]} | {'-', [term()]} | [term()] ]
%% We can reduce the problem to only diffing the parts that occurs
%% in both lists. The other elements can be merged in afterwards.
%% This greatly speeds up the case when only a few elements are the same.
compare(A, A) ->
    [A];
compare(A, B) ->
    ASame = A -- (A -- B),
    BSame = B -- (B -- A),
    Edit = compare2(ASame, BSame),
    merge_unique(A, B, Edit, []).

%% @spec(Diff) -> SplitDiff
%%   Diff      = [ {'+', [term()]} | {'-', [term()]}, [term()] ]
%%   SplitDiff = [ {'+', term()} | {'-', term()}, {'=', term()} ]
split_diff(Diff) ->
    split_diff(Diff, []).

%% @spec([term()],[term()]) -> [{'+',[term()]} | {'-', [term()]} | [term()] ]
%% calcuate shortest edit list to go from A to B
compare2(A, B) ->
    DataA = list_to_tuple(A),
    DataB = list_to_tuple(B),
    DownVector = ets:new(down, [private]),
    UpVector   = ets:new(up, [private]),
    try
        Diff = lcs(DataA, 0, size(DataA), DataB, 0, size(DataB),
                   DownVector, UpVector, []),
        merge_cleanup(Diff,[])
    after
        ets:delete(DownVector),
        ets:delete(UpVector)
    end.

%% @spec([term()], Diff) -> [term()]
%% Diff=[{'+',[term()]} | {'-', [term()]} | [term()] ]
%% Apply diff to list
apply_diff(A, Diff) ->
    apply_diff(A, Diff, []).

apply_diff([], [], Acc) ->
    lists:reverse(Acc);
apply_diff(A, [{'+',Add}|Rest], Acc) when is_list(Add) ->
    apply_diff(A, Rest, lists:reverse(Add, Acc));
apply_diff(A, [{'+',C}|Rest], Acc) ->
    apply_diff(A, Rest, [C|Acc]);
apply_diff(A, [{'-',Del}|Rest], Acc) when is_list(Del) ->
%    AStart = lists:sublist(A,length(Del)),
%    AStart == Del,
    ARest = lists:nthtail(length(Del), A),
    apply_diff(ARest, Rest, Acc);
apply_diff([A|As], [{'-',A}|Rest], Acc) ->
    apply_diff(As, Rest, Acc);
apply_diff([A|As], [{'=',A}|Rest], Acc) ->
    apply_diff(As, Rest, [A|Acc]);
apply_diff(A, [Keep|Rest], Acc) when is_list(Keep) ->
%    AStart = lists:sublist(A,length(Keep)),
%    AStart == Keep,
    ARest = lists:nthtail(length(Keep), A),
    apply_diff(ARest, Rest, lists:reverse(Keep,Acc)).

test() ->
    %% special cases,
    test([],[]),
    test([a,b,c],[a,b,c]),
    random:seed(),
    test(100000).

test(0) ->
    ok;
test(N) ->
    %% create two random strings
    SizeA = random:uniform(100)-1,
    if SizeA > 0 ->
            A = [random:uniform(100) || _ <- lists:seq(1,SizeA)];
       true ->
            A = []
    end,
    SizeB = random:uniform(100)-1,
    if SizeB > 0 ->
            B = [random:uniform(100) || _ <- lists:seq(1,SizeB)];
       true ->
            B = []
    end,
    case catch test(A, B) of
        ok ->
            ok;
        _Error ->
            io:format("test failed for:\n~p\n~p\n", [A,B]),
            throw(test_failed)
    end,
    if (N rem 100) == 0 ->
            io:format("test ~p\n", [N]);
       true ->
            ok
    end,
    test(N-1).

test(A,B) ->
    Comp1 = compare2(A,B),
    Comp2 = compare(A,B),
    Comp3 = split_diff(Comp2),
    B = apply_diff(A, Comp1),
    B = apply_diff(A, Comp2),
    B = apply_diff(A, Comp3),
    ok.

%%

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

merge_cleanup([], Acc) ->
    Acc;
merge_cleanup([{Sign, P}|Rest], Acc) ->
    merge_cleanup_sign(Sign, Rest, Acc, P);
merge_cleanup([Keep|Rest], Acc) ->
    merge_cleanup_keep(Rest, Acc, Keep).

merge_cleanup_sign(_Sign, [], Acc, []) ->
    Acc;
merge_cleanup_sign(Sign, [], Acc, AddAcc) ->
    [{Sign,AddAcc}|Acc];
merge_cleanup_sign(Sign, [{_, []}|Rest], Acc, AddAcc) ->
    merge_cleanup_sign(Sign, Rest, Acc, AddAcc);
merge_cleanup_sign(Sign, [{Sign, Add}|Rest], Acc, AddAcc) ->
    merge_cleanup_sign(Sign, Rest, Acc, Add++AddAcc);
merge_cleanup_sign(_Sign, Rest, Acc, []) ->
    merge_cleanup(Rest, Acc);
merge_cleanup_sign(Sign, Rest, Acc, AddAcc) ->
    merge_cleanup(Rest, [{Sign, AddAcc}|Acc]).

%% not liked by dialyzer :-(
%% merge_cleanup_keep([], Acc, []) ->
%%     Acc;
merge_cleanup_keep([], Acc, KeepAcc) ->
    [KeepAcc|Acc];
merge_cleanup_keep([{_,[]}|Es], Acc, KeepAcc) ->
    merge_cleanup_keep(Es, Acc, KeepAcc);
merge_cleanup_keep(Es=[{_,_}|_], Acc, []) ->
    merge_cleanup(Es, Acc);
merge_cleanup_keep(Es=[{_,_}|_], Acc, KeepAcc) ->
    merge_cleanup(Es, [KeepAcc|Acc]);
merge_cleanup_keep([K|Rest], Acc, KeepAcc) ->
    merge_cleanup_keep(Rest, Acc, K++KeepAcc).

lcs(DataA, LowerA0, UpperA0, DataB, LowerB0, UpperB0, DownVector, UpVector,
    Acc0) ->
    %% skip equal lines at start
    {LowerA, LowerB, StartKeep} =
        whilex(
          fun({LA, LB, StartAcc}) ->
                  if LA < UpperA0, LB < UpperB0 ->
                          DA = element(LA+1, DataA),
                          DB = element(LB+1, DataB),
                          if DA == DB ->
                                  {LA+1, LB+1, [DA|StartAcc]};
                             true ->
                                  {false, {LA, LB, lists:reverse(StartAcc)}}
                          end;
                     true ->
                          {false, {LA, LB, lists:reverse(StartAcc)}}
                  end
          end, {LowerA0, LowerB0, []}),
    %% skip equal lines at end
    {UpperA, UpperB, EndKeep} =
        whilex(
          fun({UA, UB, EndAcc}) ->
                  if LowerA < UA, LowerB < UB ->
                          DA = element(UA, DataA),
                          DB = element(UB, DataB),
                          if DA == DB ->
                                  {UA-1, UB-1, [DA|EndAcc]};
                             true ->
                                  {false, {UA, UB, EndAcc}}
                          end;
                     true ->
                          {false, {UA, UB, EndAcc}}
                  end
          end, {UpperA0, UpperB0, []}),

    if LowerA == UpperA, LowerB < UpperB ->
            %% mark as inserted
            Inserted = [element(I, DataB) || I <- lists:seq(LowerB+1, UpperB)],
            acc_add(EndKeep,acc_add({'+',Inserted},acc_add(StartKeep,Acc0)));
       LowerA == UpperA ->
            acc_add(StartKeep++EndKeep, Acc0);
       LowerB == UpperB, LowerA < UpperA ->
            %% mark as deleted
            Deleted = [element(I, DataA) || I <- lists:seq(LowerA+1, UpperA)],
            acc_add(EndKeep,acc_add({'-',Deleted},acc_add(StartKeep,Acc0)));
       LowerB == UpperB ->
            acc_add(StartKeep++EndKeep, Acc0);
       true ->
            {SX, SY} = sms(DataA, LowerA, UpperA, DataB, LowerB, UpperB,
                           DownVector, UpVector),
            Acc1 = lcs(DataA, LowerA, SX, DataB, LowerB, SY, DownVector,
                       UpVector, acc_add(StartKeep,Acc0)),
            Acc2 = lcs(DataA, SX, UpperA, DataB, SY, UpperB, DownVector,
                       UpVector, Acc1),
            acc_add(EndKeep, Acc2)
    end.

acc_add([],Acc) ->
    Acc;
acc_add({_Sign,[]}, Acc) ->
    Acc;
acc_add(E, Acc) ->
    [E|Acc].

%% find shortest middle sname
sms(DataA, LowerA, UpperA, DataB, LowerB, UpperB, DownVector, UpVector) ->
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
    %% D loop
    sms_d(0, MaxD, DataA, LowerA, UpperA, DataB, LowerB, UpperB, DownVector,
          UpVector, DownK, UpK, Delta, OddDelta).

sms_d(D, MaxD, _DataA, _LowerA, _UpperA, _DataB, _LowerB, _UpperB, _DownVector,
      _UpVector, _DownK, _UpK, _Delta, _OddDelta) when D > MaxD ->
    throw(error);
sms_d(D, MaxD, DataA, LowerA, UpperA, DataB, LowerB, UpperB, DownVector,
      UpVector, DownK, UpK, Delta, OddDelta) ->
    case sms_d_k_f(DownK-D, D, DataA, LowerA, UpperA, DataB, LowerB, UpperB,
                   DownVector, UpVector, DownK, UpK, Delta, OddDelta) of
        not_found ->
            case sms_d_k_r(UpK-D, D, DataA, LowerA, UpperA, DataB, LowerB,
                           UpperB, DownVector, UpVector, DownK, UpK,
                           Delta, OddDelta) of
                not_found ->
                    sms_d(D+1, MaxD, DataA, LowerA, UpperA, DataB, LowerB,
                          UpperB, DownVector, UpVector, DownK, UpK, Delta,
                          OddDelta);
                Point -> Point
            end;
        Point -> Point
    end.

%% Forward snake
sms_d_k_f(K, D, _DataA, _LowerA, _UpperA, _DataB, _LowerB, _UpperB,
          _DownVector, _UpVector, DownK, _UpK, _Delta, _OddDelta)
  when K > (DownK+D) ->
    not_found;
sms_d_k_f(K, D, DataA, LowerA, UpperA, DataB, LowerB, UpperB,
          DownVector, UpVector, DownK, UpK, Delta, OddDelta) ->
    if K == (DownK - D) ->
            X = vget(DownVector, K+1); %% Down
       true ->
            Right = vget(DownVector, K-1)+1,
            Down = vget(DownVector, K+1),
            if (K < (DownK+D)) and (Down >= Right) ->
                    X = Down; %% Down
               true ->
                    X = Right %% right
            end
    end,
    Y = X - K,
    %% walk diagonal
    {NewX, _NewY} =
        whilex(fun({XX, YY}) ->
                       if XX < UpperA, YY < UpperB ->
                               DA = element(XX+1, DataA),
                               DB = element(YY+1, DataB),
                               if DA == DB ->
                                       {XX+1, YY+1};
                                  true ->
                                       {false, {XX, YY}}
                               end;
                          true ->
                               {false, {XX, YY}}
                       end
               end, {X, Y}),

    vset(DownVector, K, NewX),
    %% overlap?
    if OddDelta, (UpK-D) < K, K < (UpK+D) ->
            UpVK = vget(UpVector, K),
            DownVK = vget(DownVector, K),
            if UpVK =< DownVK ->
                    {DownVK, DownVK-K};
               true ->
                    sms_d_k_f(K+2, D, DataA, LowerA, UpperA, DataB,
                              LowerB, UpperB, DownVector, UpVector,
                              DownK, UpK, Delta, OddDelta)
            end;
       true ->
            sms_d_k_f(K+2, D, DataA, LowerA, UpperA, DataB,
                      LowerB, UpperB, DownVector, UpVector,
                      DownK, UpK, Delta, OddDelta)
    end.

%% Backward snake
sms_d_k_r(K, D, _DataA, _LowerA, _UpperA, _DataB, _LowerB, _UpperB,
          _DownVector, _UpVector, _DownK, UpK, _Delta, _OddDelta)
  when K > (UpK+D) ->
    not_found;
sms_d_k_r(K, D, DataA, LowerA, UpperA, DataB, LowerB, UpperB,
          DownVector, UpVector, DownK, UpK, Delta, OddDelta) ->
    if K == (UpK+D) ->
            X = vget(UpVector, K-1); %% up
       true ->
            Up = vget(UpVector, K-1),
            Left = vget(UpVector, K+1)-1,
            if K > (UpK-D), Up < Left ->
                    X = Up;
               true ->
                    X = Left
            end
    end,
    Y = X - K,
    %% walk diagonal
    {NewX, _NewY} =
        whilex(fun({XX, YY}) ->
                       if XX > LowerA, YY > LowerB ->
                               DA = element(XX, DataA),
                               DB = element(YY, DataB),
                               if DA == DB ->
                                       {XX-1, YY-1};
                                  true ->
                                       {false, {XX, YY}}
                               end;
                          true ->
                               {false, {XX, YY}}
                       end
               end, {X, Y}),

    vset(UpVector, K, NewX),
    %% overlap?
    if not(OddDelta), (DownK-D) =< K, K =< (DownK+D) ->
            UpVK = vget(UpVector, K),
            DownVK = vget(DownVector, K),
            if UpVK =< DownVK ->
                    {DownVK, DownVK-K};
               true ->
                    sms_d_k_r(K+2, D, DataA, LowerA, UpperA, DataB, LowerB,
                              UpperB, DownVector, UpVector, DownK, UpK, Delta,
                              OddDelta)
            end;
       true ->
            sms_d_k_r(K+2, D, DataA, LowerA, UpperA, DataB, LowerB,
                      UpperB, DownVector, UpVector, DownK, UpK, Delta,
                      OddDelta)
    end.

whilex(F, Acc) ->
    case F(Acc) of
        {false, Res} ->
            Res;
        Acc2 ->
            whilex(F, Acc2)
    end.

vget(T, I) ->
    case ets:lookup(T, I) of
        [{_,V}|_]  -> V;
        _ -> false
    end.

vset(T, I, V) ->
    ets:insert(T, {I,V}).




