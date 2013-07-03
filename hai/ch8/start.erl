
-module(start).
-export([start_game/0]).

start_game() ->
  spawn(game, dealer, [{init, [], [], [], [],[]}]).
