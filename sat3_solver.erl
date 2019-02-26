-module(sat3_solver).

-export([bench/0]).
-compile(export_all).

bench() ->
  {T,_}=timer:tc(fun() -> run(10) end),
  T/math:pow(10,6).

run(0)->ok;
run(N)->
  run_file("./instances/75_vars/uf75-0"++integer_to_list(N)++".cnf", N),
  run(N-1).

run_file(Name, N)->
  {ok, B} = file:read_file(Name),
  F = binary_to_list(B),
  L = string:split(F, "\n", all),
  GL = lists:nthtail(8, L),
  NL = lists:map(
  fun(S) ->
    case length(S)>4 of
      true ->
    NLS = string:split(S, " ", all),
    NL = lists:map(fun([]) -> 0 ;
                      (A)-> erlang:list_to_integer(A) end, NLS),
    NL;
     false ->
       [0]
     end
  end, GL),
  LL = to_tuples(NL, []),
  PL = preprocess(LL),
  P = {[],PL},
  {D,[]}=solve(P),
  case assign(D,P) of
    {_,[]} -> io:write({N});
    A -> io:write({'problem_instnace',A,D})
  end.

solve(P) ->
  V = variables(P),
  V2 = lists:nthtail(length(V) div 2, V),
  solve_with_assumption(V2,P).

solve_with_assumption([],P) ->
  solve_without_assumption(P);
solve_with_assumption(V,P) ->
  P2 = assign(V,P),
  P3 = inference(P2),
  case solvable_or_done(P3) of
    done -> P3;
    false -> [_|T] = V,
             solve_with_assumption(T, P);
    true -> case solve(P3) of
                false -> [_|T] = V,
                         solve_with_assumption(T, P);
                Res -> Res
            end
  end.

solve_without_assumption(P) ->
  P2 = inference(P),
  case solvable_or_done(P2) of
    done -> P2;
    false -> false;
    true -> [H|_] = variables(P2),
            %io:write({hi,H,l(P2)}),
            %timer:sleep(1000),
            P3 = assign([H],P),
            case solve_without_assumption(P3) of
              false -> solve_without_assumption(assign([-H], P2));
              Res -> Res
            end
  end.

l(false)->
  false;
l({_,List}=P)->
  {length(variables(P)),length(List)}.

to_tuples([], Acc) -> Acc;
to_tuples([L|T], Acc) ->
  L2=lists:filter(fun(0)->false;
                  (_)->true end,L),
  case L2 of
    [] -> to_tuples(T, Acc);
    _ -> to_tuples(T, [lists:reverse(lists:usort(L2))|Acc])
  end.

preprocess(TL) ->
  lists:map(fun(T) -> lists:filter(fun(A)->not(lists:member(A*-1,T)) end,T) end, TL).

inference({_,TL}=P) ->
  Singles = lists:flatten(lists:filter(fun(T) -> length(T) == 1 end, TL)),
  Symbols = symbols(P),
  %% iff A is present but -A is not, assigning A to true is better then false
  Not_bad = lists:filter(fun(A)->not(lists:any(fun(V)->V == -A end, Symbols)) end, Symbols),
  A = lists:usort(Singles ++ Not_bad),
  case A of
    [] -> P;
    _ -> inference(assign(A, P))
  end.

symbols({_,TL}) ->
  lists:usort(lists:flatten(TL)).
variables(P) ->
  lists:filter(fun(A)->A>0 end, symbols(P)).

assign([], A) -> A;
%assigned list of symbols
assign(S,{P,TL})->
  NOTS= lists:map(fun(D)-> -D end, S),
  %% drop false symbols
  TL1 = lists:map(fun(T) -> lists:filter(fun(A)->not(lists:member(A,NOTS)) end,T) end, TL),
  %% drop true expressions
  TL2 = lists:filter(fun(T)-> not(lists:any(fun(A)-> lists:member(A,S) end,T)) end, TL1),
  {P++S,TL2}.

solvable_or_done({_,[]}) -> done;
solvable_or_done({_,TL}) ->
  lists:all(fun([]) -> false;
               (_) -> true end, TL).
