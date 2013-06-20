
-module(dijkstra).
-export([gcd/2]).
-include_lib("eunit/include/eunit.hrl").


gcd(Number1, Number2) ->
	if Number1 == Number2 -> Number1;
		Number1 > Number2 -> gcd(Number1 - Number2, Number2);
		true -> gcd(Number1, Number2 - Number1)
	end.

gcd_test_() ->
	[?_assert(gcd(12, 8) =:= 4),
		?_assert(gcd(14, 21) =:= 7),
		?_assert(gcd(125, 46) =:= 1),
		?_assert(gcd(120, 36) =:= 12)
	].
