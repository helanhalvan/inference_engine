-module(sat3_solver).

-export([run/0]).

run(P = #{constraint := true}) -> P;

run(P = #{
       constraint := Con
      }) ->
  {Con1, P1} = propagate(Con, P)
  run(P1#{constraint := Con1}).

propagate(true, P) ->
  {true, P};
propagate(false, P) ->
  {false, P};
propagate({MODULE, State0}, P0) ->
  MODULE:propagate(State, P0).
