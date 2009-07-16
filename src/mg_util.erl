-module(mg_util).

-export([combinations/2, is_mutation/2]).

-define(POINT, $$).

combinations(Seq, Degree) when Degree > 1 ->
  L = lists:foldr(fun (Elt, Acc) -> combinations(Elt, 1) ++ Acc end,
                  [],
                  combinations(Seq, Degree - 1)),
  sets:to_list(sets:from_list(L));
combinations(Seq, 1) ->
  F = fun (Pos, Acc) ->
          Head = lists:sublist(Seq, Pos - 1),
          Tail = lists:nthtail(Pos, Seq),
          [Head ++ [?POINT] ++ Tail | Acc]
      end,
  lists:filter(fun(X) when X =:= Seq -> false;
                  (_) -> true
               end,
               lists:foldr(F, [], lists:seq(1, length(Seq)))).

is_mutation(Seq1, {_, Seq2}) when Seq1 =:= Seq2 ->
  false;
is_mutation(Seq1, {_, Seq2}) when length(Seq1) =/= length(Seq2) ->
  false;
is_mutation(Seq1, {Key, Seq2}) ->
  only_differs_at_points(Seq1, Key, Seq2).

only_differs_at_points([], [], []) ->
  true;
only_differs_at_points([C|_], [?POINT|_], [C|_]) ->
  false;
only_differs_at_points([_|As], [?POINT|Ks], [_|Bs]) ->
  only_differs_at_points(As, Ks, Bs);
only_differs_at_points([C|As], [_|Ks], [C|Bs]) ->
  only_differs_at_points(As, Ks, Bs);
only_differs_at_points(_,_,_) ->
  false.
