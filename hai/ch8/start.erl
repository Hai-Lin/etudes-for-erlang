
-module(start).
-export([start_game/1]).

start_game(NumberOfCards) ->
  spawn(game, dealer, [{init, NumberOfCards}]).
