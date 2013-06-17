
-module(stats).
-export([minimum/1, range/1, maximum/1]).

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
