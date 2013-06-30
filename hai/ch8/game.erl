
-module(game).
-export([player/1, dealer/1]).

dealer({init, _}) ->
  io:format("Dealers init game~n"),
  io:format("Init player1~n"),
  Player1 = spawn(game, player, [[1,2,3,4]]),
  io:format("Init player2~n"),
  Player2 = spawn(game, player, [[5,4,3,2]]),
  dealer({pre_battle, [Player1, Player2]});

dealer({pre_battle, Players}) ->
  io:format("PreBattle start with players: ~p~n",[Players]),
  [Player1, Player2] = Players,
  io:format("Ask player1 ~p for cards~n", [Player1]),
  Player1 ! {self(), 1},
  io:format("Ask player2 ~p for cards~n", [Player2]),
  Player2 ! {self(), 1},
  dealer({await_battle, [Player1, Player2]});

dealer(_) ->
  io:format("Ending...~n").

player(Cards) ->
  receive
    {From, NumbersOfCards} ->
      if length(Cards) >= NumbersOfCards ->
          {GiveAwayCards, LeftCards} = lists:split(NumbersOfCards, Cards),
          From ! {ok, GiveAwayCards},
          io:format("Player ~p start with cards: ~p~n", [self(), LeftCards]),
          player(LeftCards);
        true ->
          io:format("I lose ~n"),
          From ! {"I lose"}
      end
  end.

