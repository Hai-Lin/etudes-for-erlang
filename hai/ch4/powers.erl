

-module(powers).
-export([raise/2, nth_root/2]).
-include_lib("eunit/include/eunit.hrl").

raise(_, Exponent) when Exponent == 0 ->
		1;

raise(Base, Exponent) when Exponent < 0 ->
		1.0 / raise(Base, 0 - Exponent);

raise(Base, Exponent) when Exponent > 0 ->
		raise(Base, Exponent, 1).

raise(_, Exponent, Acc) when Exponent == 0 ->
		Acc;

raise(Base, Exponent, Acc)  ->
		raise(Base, Exponent - 1, Acc * Base).


raise_test_() ->
		[?_assert(raise(5, 1) =:= 5),
				?_assert(raise(2, 3) =:= 8),
				?_assert(raise(1.2, 3) =:= 1.728),
				?_assert(raise(2, 0) =:= 1),
				?_assert(raise(2, -3) =:= 0.125)
		].


nth_root(Base, Root) -> nth_root(Base, Root, Base / 2.0).

nth_root(Base, Root, Approximation) ->
		io:format("Current guess is ~p~n", [Approximation]),
		F = raise(Approximation, Root) - Base,
		Fprime = Approximation * raise(Approximation, Root - 1),
		Next = Approximation - F / Fprime,
		Diff = abs(Next - Approximation),
		if Diff < 1.0e-8
				-> Next;
				true -> nth_root(Base, Root, Next)
		end.

nth_root_test_() ->
		[?_assert(nth_root(27, 3) =:= 3.0)].
