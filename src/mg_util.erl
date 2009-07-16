-module(mg_util).

-export([combinations/2]).

combinations(Seq, Degree) when Degree > 1 ->
  L = lists:foldr(fun (Elt, Acc) -> combinations(Elt, 1) ++ Acc end,
                  [],
                  combinations(Seq, Degree - 1)),
  sets:to_list(sets:from_list(L));
combinations(Seq, 1) ->
  F = fun (Pos, Acc) ->
          Head = lists:sublist(Seq, Pos - 1),
          Tail = lists:nthtail(Pos, Seq),
          [Head ++ "$" ++ Tail | Acc]
      end,
  lists:filter(fun(X) when X =:= Seq -> false;
                  (_) -> true
               end,
               lists:foldr(F, [], lists:seq(1, length(Seq)))).
