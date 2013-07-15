
-module(phone_ets).
-export([setup/1]).

setup(File) ->
  try Result = file:open(File, [read])
  catch
    error:Error -> {error, Error}
  end.


