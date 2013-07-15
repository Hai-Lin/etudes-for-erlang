
-module(phone_ets).
-export([setup/1]).

setup(File) ->
  try setup_ets_table(file:open(File, [read]))
  catch
    error:Error -> {error, Error}
  end.

setup_ets_table({_, InputFile}) ->
  Line = io:get_line(InputFile, ""),
  if Line /= eof ->
      write_ets(Line),
      setup_ets_table({ok, InputFile});
    true ->
      io:format("EOF, exitting..~n")
  end.

write_ets(Line) ->
  io:format(Line).


