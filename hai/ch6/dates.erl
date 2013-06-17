

-module(dates).
-export([data_parts/1, is_leap_year/1]).
-include_lib("eunit/include/eunit.hrl").

data_parts(Date) ->
	[Year, Month, Day] = re:split(Date, "-", [{return, list}]),
	[element(1,string:to_interger(Year)),
		element(1,string:to_interger(Month)),
		element(1,string:to_interger(Day))].

is_leap_year(Year) ->
	(Year rem 4 == 0 andalso Year rem 100 /= 0)
	orelse (Year rem 400 == 0).

data_parts

data_parts_test() ->
	[?_assert(data_parts("1982-06-29") =:= [1982,06,29]),
		?_assert(data_parts("1998-07-29") =:= [1998,07,29])
	].

is_leap_year_test() ->
	[?_assert(is_leap_year(2000) =:= true), 
		?_assert(is_leap_year(2004) =:= true), 
		?_assert(is_leap_year(2005) =:= false)
	].
