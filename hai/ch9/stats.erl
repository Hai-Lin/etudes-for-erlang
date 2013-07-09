
-module(stats).
-export([minimum/1, range/1, maximum/1, mean/1, stdv/1]).
-include_lib("eunit/include/eunit.hrl").

range(Numbers) -> [ minimum(Numbers), maximum(Numbers) ].

maximum(NumberList) ->
  try maximum(NumberList, hd(NumberList))
  catch
    error:Error -> {error, Error}
  end.

maximum([], Max) -> Max;

maximum([H | T], Max) -> 
  if H > Max -> maximum(T, H);
    true -> maximum(T, Max)
  end.

minimum(NumberList) ->
  try minimum(NumberList, hd(NumberList))
  catch
    error:Error -> {error, Error}
  end.

minimum([], Minimum) -> Minimum;

minimum([H | T], Minimum) ->
  if H < Minimum -> minimum(T, H);
    true -> minimum(T, Minimum)
  end.

mean(NumberList) ->
  try
    Sum = lists:foldl(fun sum/2, 0, NumberList),
    Sum / length(NumberList)
  catch
    error:Error -> {error, Error}
  end.

sum(Value, Accumulator) ->
  Value + Accumulator.

sum_and_square_sum(NumberList) ->
  SumAndSquareSum = fun(Value, [H , T]) -> [Value + H, Value * Value + T] end,
  lists:foldl(SumAndSquareSum, [0, 0], NumberList).

stdv(NumberList) ->
  try
    [Sum, SquareSum] = sum_and_square_sum(NumberList),
    Length = length(NumberList),
    math:sqrt((Length * SquareSum - Sum * Sum) / (Length * (Length - 1)))
  catch
    error:Error -> {error, Error}
  end.


minimum_test_() ->
  [?_assert(minimum([19, 3, 4, 0, 100, -30, -0.5]) =:= -30),
   ?_assert(minimum([19, 3, 4, 0, -100, -30, -0.5]) =:= -100)
  ].

maximum_test_() ->
  [?_assert(maximum([19, 3, 4, 0, 100, -30, -0.5]) =:= 100),
   ?_assert(maximum([19, 3, 4, 0, -100, -30, -0.5]) =:= 19)
  ].

range_test_() ->
  [?_assert(range([19, 3, 4, 0, 100, -30, -0.5]) =:= [-30,100]),
   ?_assert(range([19, 3, 4, 0, -100, -30, -0.5]) =:= [-100, 19])
  ].

mean_test_() ->
  [?_assert(mean([1, 2, 3]) =:= 2.0),
   ?_assert(mean([1, 0, 3]) =:= 1.3333333333333333),
   ?_assert(mean([3, 3, 3]) =:= 3.0)
  ].

sum_test_() ->
  [?_assert(sum(0,1) =:= 1),
   ?_assert(sum(2,1) =:= 3),
   ?_assert(sum(-1,1) =:= 0),
   ?_assert(sum(-1.5,1) =:= -0.5)
  ].

sum_and_square_sum_test_() ->
  [?_assert(sum_and_square_sum([1, 2, 3]) =:= [6, 14]),
   ?_assert(sum_and_square_sum([-1, 2, 3]) =:= [4, 14]),
   ?_assert(sum_and_square_sum([0, 0, 0]) =:= [0, 0])
  ].

stdv_test_() ->
  [?_assert(stdv([7, 2, 9]) =:= 3.605551275463989)].
