
-module(calculus).
-export([derivative/2]).
-include_lib("eunit/include/eunit.hrl").

derivative(Function, Point) ->
  Delta = 1.0e-10,
  (Function(Point + Delta) - Function(Point)) / Delta.

derivative_test_() ->
  F1 = fun(X) -> X * X end,
  [?_assert(derivative(F1, 3) =:= 6.000000496442226),
   ?_assert(derivative(fun(X) -> 3 * X * X + 2 * X + 1 end, 5) =:= 32.00000264769187),
   ?_assert(derivative(fun math:sin/1, 0) =:= 1.0)
  ].

