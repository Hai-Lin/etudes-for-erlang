
-module(number).
-export([get_number/1]).

get_number(Prompt) ->
  PromptString = "Enter " ++ Prompt ++ " > ",
  Result = io:get_line(PromptString),
  string_to_number(Result).

string_to_number(String) ->
  Number = string:to_float(String),
  case Number of
    {error,_} -> element(1, string:to_integer(String));
    _ -> element(1, Number)
  end.
