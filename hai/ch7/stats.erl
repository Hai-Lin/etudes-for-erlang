
-module(stats).
-export([minimum/1, range/1, maximum/1, mean/1]).
-include_lib("eunit/include/eunit.hrl").

range(Numbers) -> [ minimum(Numbers), maximum(Numbers) ].

maximum([]) -> false;
maximum([H | T]) -> maximum([H | T], H).

maximum([], Max) -> Max;

maximum([H | T], Max) -> 
	if H > Max -> maximum(T, H);
		true -> maximum(T, Max)
	end.

minimum([]) -> false;
minimum([H | T]) -> minimum(T, H).

minimum([], Minimum) -> Minimum;

minimum([H | T], Minimum) ->
	if H < Minimum -> minimum(T, H);
		true -> minimum(T, Minimum)
	end.

mean(NumberList) ->
	Sum = fun(Value, Accumulator) -> Value + Accumulator end,
	SumNumber = lists:foldl(Sum, 0, NumberList),
	SumNumber / length(NumberList).



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
