
-module(start).
-export([start_game/1]).

start_game(NumberOfCards) when NumberOfCards =< 26 ->
  spawn(game, dealer, [{init, NumberOfCards}]);

start_game(_) ->
  io:format("Start wish any number between 1-26 ~n").
