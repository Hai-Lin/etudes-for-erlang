
-module(dates).
-export([date_parts/1, is_leap_year/1, julian/1]).
-include_lib("eunit/include/eunit.hrl").

date_parts(Date) ->
	[Year, Month, Day] = re:split(Date, "-", [{return, list}]),
	[element(1,string:to_integer(Year)),
		element(1,string:to_integer(Month)),
		element(1,string:to_integer(Day))].

is_leap_year(Year) ->
	(Year rem 4 == 0 andalso Year rem 100 /= 0)
	orelse (Year rem 400 == 0).

get_month_days(Year) ->
	case is_leap_year(Year) of
		false -> [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
		true -> [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	end.

julian(DateString) -> 
	[Year, Month, Day] = date_parts(DateString),
	Days = get_month_days(Year),
	{CountDays, _} = lists:split(Month - 1, Days),
	Day + sum_list(CountDays).

sum_list(NumberList) ->
	lists:foldl(fun sum/2, 0, NumberList).

sum(Value, Accumulator) ->
	Value + Accumulator.

julian_test_() ->
	[?_assert(julian("2012-12-31") =:= 366),
		?_assert(julian("2013-12-31") =:= 365),
		?_assert(julian("2012-02-05") =:= 36),
		?_assert(julian("1900-03-01") =:= 60),
		?_assert(julian("2000-03-01") =:= 61),
		?_assert(julian("2013-01-01") =:= 1)
	].

get_month_days_test_() ->
	[?_assert(get_month_days(2000) =:= [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]),
		?_assert(get_month_days(2001) =:= [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])
	].


date_parts_test_() ->
	[?_assert(date_parts("1982-06-29") =:= [1982,06,29]),
		?_assert(date_parts("1998-07-29") =:= [1998,07,29]),
		?_assert(date_parts("2012-12-31") =:= [2012,12,31])
	].

is_leap_year_test_() ->
	[?_assert(is_leap_year(2000) =:= true), 
		?_assert(is_leap_year(2004) =:= true), 
		?_assert(is_leap_year(2005) =:= false)
	].
