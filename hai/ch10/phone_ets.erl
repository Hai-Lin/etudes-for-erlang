
-module(phone_ets).
-export([setup/1, calculate_time_list/0]).
-include("phone_records.hrl").

setup(File) ->
  init_table(),
  setup_ets_table(file:open(File, [read])).

init_table() ->
  case ets:info(phone_calls) of
    undefined -> false;
    _ -> ets:delete(phone_calls)
  end,
  ets:new(phone_calls, [named_table, bag, {keypos, #phone_call.phone_number}]).

setup_ets_table({_, File}) ->
  Line = io:get_line(File, ""),
  if Line /= eof ->
      write_ets(Line),
      setup_ets_table({ok, File});
    true ->
      io:format("Done importing table~n"),
      ets:info(phone_calls)
  end.

write_ets(Line) ->
  [PhoneNumber, StartDate, StartTime, EndDate, EndTime] = re:split(re:replace(Line, "\\s+", "", [global]),",",[{return,list}]),
  ets:insert(phone_calls, #phone_call{phone_number=PhoneNumber, start_date=StartDate, start_time=StartTime, end_date=EndDate, end_time=EndTime}).

calculate_time_list() ->
  [{PhoneNumber, calculate_time({StartDate, StartTime}, {EndDate, EndTime})} ||  {_, PhoneNumber, StartDate, StartTime, EndDate, EndTime} <- ets:tab2list(phone_calls)].

calculate_time(StartTime, EndTime) ->
  calculate_seconds(EndTime) - calculate_seconds(StartTime).

calculate_seconds({Date, Time}) ->
  calendar:datetime_to_gregorian_seconds({parse_date(Date), parse_time(Time)}).

parse_time(Time) ->
  [Hours, Minutes, Seconds] = re:split(Time, ":", [{return,list}]),
  {string_to_number(Hours), string_to_number(Minutes), string_to_number(Seconds)}.

parse_date(Date) ->
  [Year, Month, Day] = re:split(Date, "-", [{return,list}]),
  {string_to_number(Year), string_to_number(Month), string_to_number(Day)}.
  
string_to_number(String) ->
  Number = string:to_float(String),
  case Number of
    {error,_} -> element(1, string:to_integer(String));
    _ -> element(1, Number)
  end.
