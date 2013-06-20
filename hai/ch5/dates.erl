

-module(dates).
-export([data_parts/1]).
-include_lib("eunit/include/eunit.hrl").

data_parts(Date) ->
	[Year, Month, Day] = re:split(Date, "-", [{return, list}]),
	[element(1,string:to_integer(Year)),
		element(1,string:to_integer(Month)),
		element(1,string:to_integer(Day))].

data_parts_test_() ->
	[?_assert(data_parts("1982-06-29") =:= [1982,06,29]),
		?_assert(data_parts("1998-07-29") =:= [1998,07,29])
	].
