

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

get_month_days(Year) ->
	case is_leap_year(Year) of
		false -> [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
		true -> [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	end.


get_month_days_test() ->
	[?_assert(get_month_days(2000) =:= [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]),
		?_assert(get_month_days(2001) =:= [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])
	].


data_parts_test() ->
	[?_assert(data_parts("1982-06-29") =:= [1982,06,29]),
		?_assert(data_parts("1998-07-29") =:= [1998,07,29])
	].

is_leap_year_test() ->
	[?_assert(is_leap_year(2000) =:= true), 
		?_assert(is_leap_year(2004) =:= true), 
		?_assert(is_leap_year(2005) =:= false)
	].
