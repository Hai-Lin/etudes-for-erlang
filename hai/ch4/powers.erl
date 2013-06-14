

-module(powers).
-export([raise/2]).
-include_lib("eunit/include/eunit.hrl").

raise(Base, Exponent) when Exponent == 0 ->
		1;

raise(Base, Exponent) when Exponent == 1 ->
		Base;

raise(Base, Exponent) when Exponent > 0 ->
		Base * raise(Base, Exponent - 1);

raise(Base, Exponent) when Exponent < 0 ->
		1.0 / raise(Base, 0 - Exponent).

raise_test_() ->
		[?_assert(raise(5, 1) =:= 5),
				?_assert(raise(2, 3) =:= 8),
				?_assert(raise(1.2, 3) =:= 1.728),
				?_assert(raise(2, 0) =:= 1),
				?_assert(raise(2, -3) =:= 0.125)
		].
