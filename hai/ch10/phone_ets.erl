
-module(phone_ets).
-export([setup/1]).
-include("phone_records.hrl").

setup(File) ->
  init_table(),
  setup_ets_table(file:open(File, [read])).
  %catch
    %error:Error -> {error, Error}
  %end.

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
      io:format("EOF, exitting..~n"),
      ets:info(phone_calls)
  end.

write_ets(Line) ->
  [PhoneNumber, StartingDate, StartingTime, EndDate, EndTime] = re:split(Line,",",[{return,list}]),
  ets:insert(phone_calls, #phone_call{phone_number=PhoneNumber, start_date=StartingDate, start_time=StartingTime, end_date=EndDate, end_time=EndTime}).


