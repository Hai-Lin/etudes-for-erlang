

-module(dates).
-export([data_parts/1]).
-include_lib("eunit/include/eunit.hrl").

data_parts(Date) ->
	[Year, Month, Day] = re:split(Date, "-", [{return, list}]),
	[element(1,string:to_interger(Year)),
		element(1,string:to_interger(Month)),
		element(1,string:to_interger(Day))].

data_parts_test() ->
	[?_assert(data_parts("1982-06-29") =:= [1982,06,29]),
		?_assert(data_parts("1998-07-29") =:= [1998,07,29])
	].
