-module(and_constraint).

-export([propagate/2]).

propagate(Cons, P0) ->
  case propagate_and(Cons, P0, []) of
    {[S], P} -> {S, P};
    false -> {false, P0};
    {[], P} -> {true, P};
    {NewList, P} -> {{and_constraint, NewList}, P}
  end.

propagate_and([], P, Done) ->
  {Done, P};
propagate_and([H0|T], P0, Done) ->
  case propagate(H0, P0) of
    {true, P} -> propagate_andlist(T, P, Done);
    {false, P} -> false; %% need to ensure we don't destory state
    {H, P} -> propagate_andlist(T, P, [H|Done])
  end.
