-module(convert).
-export([calculate_time/2, string_to_time/1, string_to_date/1, string_to_number/1]).


calculate_time(StartTime, EndTime) ->
  seconds_to_minutes(calculate_seconds(EndTime) - calculate_seconds(StartTime)).

seconds_to_minutes(Seconds) ->
  (Seconds + 59) div 60.

calculate_seconds({Date, Time}) ->
  calendar:datetime_to_gregorian_seconds({string_to_date(Date), string_to_time(Time)}).

string_to_time(Time) ->
  [Hours, Minutes, Seconds] = re:split(Time, ":", [{return,list}]),
  {string_to_number(Hours), string_to_number(Minutes), string_to_number(Seconds)}.

string_to_date(Date) ->
  [Year, Month, Day] = re:split(Date, "-", [{return,list}]),
  {string_to_number(Year), string_to_number(Month), string_to_number(Day)}.

string_to_number(String) ->
  Number = string:to_float(String),
  case Number of
    {error,_} -> element(1, string:to_integer(String));
    _ -> element(1, Number)
  end.
