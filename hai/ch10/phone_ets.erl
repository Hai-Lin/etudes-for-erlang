
-module(phone_ets).
-export([setup/1, summary/0]).
-include("phone_records.hrl").

setup(File) ->
  init_table(),
  setup_phone_calls_table(file:open(File, [read])),
  calculate_time_list(),
  setup_call_minutes_table(calculate_time_list()).

init_table() ->
  init_call_minutes_table(),
  init_phone_calls_table().

delete_table_if_exist(TableName) ->
  case ets:info(TableName) of
    undefined -> false;
    _ -> ets:delete(TableName)
  end.

init_call_minutes_table() ->
  delete_table_if_exist(call_minutes),
  ets:new(call_minutes, [named_table, {keypos, #call_minute.phone_number}]).

init_phone_calls_table() ->
  delete_table_if_exist(phone_calls),
  ets:new(phone_calls, [named_table, bag, {keypos, #phone_call.phone_number}]).

setup_phone_calls_table({_, File}) ->
  Line = io:get_line(File, ""),
  if Line /= eof ->
      write_phone_calls_table(Line),
      setup_phone_calls_table({ok, File});
    true -> false
  end.

setup_call_minutes_table([]) ->
  io:format("End setup~n");

setup_call_minutes_table([First | Rest]) ->
  write_to_call_minutes_table(First),
  setup_call_minutes_table(Rest).

write_to_call_minutes_table({PhoneNumber, Minutes}) ->
  QueryResult = ets:lookup(call_minutes, PhoneNumber),
  case QueryResult of
    [] -> 
      NewMinutes = 0;
    _ -> 
      {_, _, NewMinutes} = hd(QueryResult)
  end,
  ets:insert(call_minutes, #call_minute{phone_number = PhoneNumber, minutes = Minutes + NewMinutes}).

summary() ->
  [{PhoneNumber, Minutes} ||  {_, PhoneNumber, Minutes} <- ets:tab2list(call_minutes)].

write_phone_calls_table(Line) ->
  [PhoneNumber, StartDate, StartTime, EndDate, EndTime] = re:split(re:replace(Line, "\\s+", "", [global]),",",[{return,list}]),
  ets:insert(phone_calls, #phone_call{phone_number=PhoneNumber, start_date=StartDate, start_time=StartTime, end_date=EndDate, end_time=EndTime}).

calculate_time_list() ->
  [{PhoneNumber, calculate_time({StartDate, StartTime}, {EndDate, EndTime})} ||  {_, PhoneNumber, StartDate, StartTime, EndDate, EndTime} <- ets:tab2list(phone_calls)].

calculate_time(StartTime, EndTime) ->
  seconds_to_minutes(calculate_seconds(EndTime) - calculate_seconds(StartTime)).

seconds_to_minutes(Seconds) ->
  (Seconds + 59) div 60.

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
