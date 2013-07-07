-module(game).
-export([player/1, dealer/1]).
-include_lib("eunit/include/eunit.hrl").

dealer({init, NumberOfCards}) ->
  io:format("Dealers init game~n"),
  Cards = cards:make_deck(),
  Cards1 = lists:sublist(Cards, NumberOfCards),
  Cards2 = lists:sublist(Cards, NumberOfCards + 1, NumberOfCards),
  Player1 = spawn_player(Cards1),
  Player2 = spawn_player(Cards2),
  dealer({pre_battle, [Player1, Player2],[], [], [], 0});

dealer({pre_battle, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds}) when NumberOfResponds =< 1->
  io:format("Ask player1 ~p for 1 cards~n", [Player1]),
  Player1 ! {self(),ask_for_cards, 1},
  io:format("Ask player2 ~p for 1 cards~n", [Player2]),
  Player2 ! {self(),ask_for_cards, 1},
  dealer({await_battle, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds});

dealer({pre_battle, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds}) ->
  io:format("War start ~n"),
  io:format("Ask player1 ~p for 3 cards~n", [Player1]),
  Player1 ! {self(),ask_for_cards, 3},
  io:format("Ask player2 ~p for 3 cards~n", [Player2]),
  Player2 ! {self(),ask_for_cards, 3},
  dealer({await_battle, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds});

dealer({await_battle, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds}) ->
  io:format("Await Battle~n"),
  receive
    {ok, PlayerPid, Cards} ->
      case PlayerPid of
        Player1 ->
          io:format("Receive cards from player1~n"),
          case NumberOfResponds of
            0 ->
              io:format("Call dealer await_battle for player2~n"),
              dealer({await_battle, [Player1, Player2], Piles ++ Cards, Cards, Piles2, NumberOfResponds + 1});
            1 ->
              io:format("Call dealer check_cards~n"),
              dealer({check_cards, [Player1, Player2], Piles ++ Cards, Cards, Piles2, NumberOfResponds + 1});
            2 ->
              io:format("Call dealer await_battle(WAR) for player2~n"),
              dealer({await_battle, [Player1, Player2], Piles ++ Cards, Piles1 ++ Cards, Piles2, NumberOfResponds + 1});
            3 ->
              io:format("Call dealer check_cards for WAR~n"),
              dealer({check_cards, [Player1, Player2], Piles ++ Cards, Piles1 ++ Cards, Piles2, NumberOfResponds + 1})
          end;
        Player2 ->
          io:format("Receive cards from player2~n"),
          case NumberOfResponds of
            0 ->
              io:format("Call dealer await_battle for player1~n"),
              dealer({await_battle, [Player1, Player2], Piles ++ Cards, Piles1, Cards, NumberOfResponds + 1});
            1 ->
              io:format("Call dealer check_cards~n"),
              dealer({check_cards, [Player1, Player2], Piles ++ Cards, Piles1, Cards, NumberOfResponds + 1});
            2 ->
              io:format("Call dealer await_battle(WAR) for player1~n"),
              dealer({await_battle, [Player1, Player2], Piles ++ Cards, Piles1, Piles2 ++ Cards, NumberOfResponds + 1});
            3 ->
              io:format("Call dealer check_cards for WAR~n"),
              dealer({check_cards, [Player1, Player2], Piles ++ Cards, Piles1, Piles2 ++ Cards, NumberOfResponds + 1})
          end
      end;
    {no_card, PlayerPid} ->
      if
        PlayerPid =:= Player1 ->
          io:format("Player ~p has no card left, waiting for player ~p~n", [Player1, Player2]),
          dealer({last_round});
        true ->
          io:format("Player ~p has no card left, waiting for player ~p~n", [Player2, Player1]),
          dealer({last_round})
      end
  end;

dealer({check_cards, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds}) ->
  CompareResult = cards:compare(Piles1, Piles2),
  case CompareResult of
    bigger ->
      Player1 ! {give_cards, Piles},
      dealer({pre_battle, [Player1, Player2], [], [], [], 0});
    smaller ->
      Player2 ! {give_cards, Piles},
      dealer({pre_battle, [Player1, Player2], [], [], [], 0});
    equal ->
      dealer({pre_battle, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds})
  end;

dealer({last_round}) ->
  receive
    {ok, PlayerPid, _} ->
      io:format("Player ~p win, game over.~n", [PlayerPid]);
    {no_cards, _} ->
      io:format("Game endding in a draw.~n")
  end;

dealer(_) ->
  io:format("Error, existting...~n").

spawn_player(Cards) ->
  io:format("Init player1~n"),
  Player = spawn(game, player, [Cards]),
  io:format("Player ~p start with cards: ~p~n", [Player, Cards]),
  Player.


player(Cards) ->
  receive
    {From, ask_for_cards, NumbersOfCards} ->
      io:format("Player ~p got request for ~p of cards ~n", [self(), NumbersOfCards]),
      if length(Cards) >= NumbersOfCards ->
          {GiveAwayCards, LeftCards} = lists:split(NumbersOfCards, Cards),
          From ! {ok, self(), GiveAwayCards},
          io:format("Player ~p start with cards: ~p~n", [self(), LeftCards]),
          player(LeftCards);
        true ->
          io:format("Player ~p have no card left ~n", [self()]),
          From ! {no_card, self()}
      end;
    {give_cards, GivenCards} ->
      player(Cards ++ GivenCards)
  end.

