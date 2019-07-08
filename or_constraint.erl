-module(or_constraint).

-export([propagate/2]).

propagate(Cons, P0) ->
  case propagate_or(Cons, P0, []) of
    [S] -> {S, P0};
    [] -> {true, P0};
    NewList -> {{or_constraint, NewList}, P0}
  end.

propagate_or([], P, Done) ->
  Done;
propagate_or([H0|T], P0, Done) ->
  case propagate(H0, P0) of
    {true, P} -> propagate_andlist(T, P, Done);
    {H, P} -> propagate_andlist(T, P, [H|Done])
  end.
