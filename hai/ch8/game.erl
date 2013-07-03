-module(game).
-export([player/1, dealer/1]).


dealer({init, _, _, _, _, _}) ->
  io:format("Dealers init game~n"),
  io:format("Init player1~n"),
  Player1 = spawn(game, player, [[1,2,3,4]]),
  io:format("Init player2~n"),
  Player2 = spawn(game, player, [[5,4,3,2]]),
  dealer({pre_battle, [Player1, Player2],[], [], [], 0});

dealer({pre_battle, Players, _, _, _, _}) ->
  io:format("PreBattle start with players: ~p~n",[Players]),
  [Player1, Player2] = Players,
  io:format("Ask player1 ~p for cards~n", [Player1]),
  Player1 ! {self(), 1},
  io:format("Ask player2 ~p for cards~n", [Player2]),
  Player2 ! {self(), 1},
  dealer({await_battle, [Player1, Player2],[], [], [], 0});

dealer({await_battle, Players, Piles, Piles1, Piles2, NumberOfResponds}) ->
  io:format("Await Battle~n"),
  [Player1, Player2] = Players,
  receive
    {ok, PlayerPid, Cards} ->
      NewPiles = lists:append(Piles, Cards),
      case PlayerPid of
        Player1 ->
          io:format("Receive cards from player1~n"),
          case NumberOfResponds of
            0 ->
              io:format("Call dealer await_battle for player2~n"),
              dealer({await_battle, Players, NewPiles, Cards, Piles2, NumberOfResponds + 1});
            1 ->
              io:format("Call dealer check_cards~n"),
              dealer({check_cards, Players, NewPiles, Cards, Piles2, NumberOfResponds + 1})
          end;
        Player2 ->
          io:format("Receive cards from player2~n"),
          case NumberOfResponds of
            0 ->
              io:format("Call dealer await_battle for player1~n"),
              dealer({await_battle, Players, NewPiles, Piles1, Cards, NumberOfResponds + 1});
            1 ->
              io:format("Call dealer check_cards~n"),
              dealer({check_cards, Players, NewPiles, Piles1, Cards, NumberOfResponds + 1})
          end
      end
  end;

dealer(_) ->
  io:format("Ending...~n").

player(Cards) ->
  receive
    {From, NumbersOfCards} ->
      if length(Cards) >= NumbersOfCards ->
          {GiveAwayCards, LeftCards} = lists:split(NumbersOfCards, Cards),
          From ! {ok, self(), GiveAwayCards},
          io:format("Player ~p start with cards: ~p~n", [self(), LeftCards]),
          player(LeftCards);
        true ->
          io:format("I lose ~n"),
          From ! {"I lose"}
      end
  end.

