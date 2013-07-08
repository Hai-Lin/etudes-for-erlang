-module(game).
-export([player/1, dealer/1]).
-include_lib("eunit/include/eunit.hrl").

dealer({init, NumberOfCards}) ->
  io:format("Dealer init game~n"),
  Cards = cards:make_deck(),
  Cards1 = lists:sublist(Cards, NumberOfCards),
  Cards2 = lists:sublist(Cards, NumberOfCards + 1, NumberOfCards),
  Player1 = spawn_player(Cards1),
  Player2 = spawn_player(Cards2),
  dealer({pre_battle, [Player1, Player2],[], [], [], 0});

dealer({pre_battle, Players, Piles, Piles1, Piles2, NumberOfResponds}) when NumberOfResponds =< 1->
  ask_for_cards(Players, 1),
  dealer({await_battle, Players, Piles, Piles1, Piles2, NumberOfResponds});

dealer({pre_battle, Players, Piles, Piles1, Piles2, NumberOfResponds}) ->
  io:format("War start ~n"),
  ask_for_cards(Players, 3),
  dealer({await_battle, Players, Piles, Piles1, Piles2, NumberOfResponds});

dealer({await_battle, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds}) ->
  receive
    {ok, PlayerPid, Cards} ->
      await_battle_receive_cards_helper({PlayerPid, Cards, Player1, Player2, Piles, Piles1, Piles2, NumberOfResponds});
    {no_card, PlayerPid} ->
      await_battle_no_card_helper({PlayerPid, Player1, Player2, NumberOfResponds})
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
  io:format("Last round..~n"),
  receive
    {ok, PlayerPid, _} ->
      io:format("Player ~p win, game over.~n", [PlayerPid]);
    {no_card, _} ->
      io:format("Game endding in a draw.~n")
  end;

dealer(_) ->
  io:format("Error, existting...~n").

spawn_player(Cards) ->
  io:format("Init player1~n"),
  Player = spawn(game, player, [Cards]),
  io:format("Player ~p start with cards: ~p~n", [Player, Cards]),
  Player.

ask_for_cards([Player1, Player2], NumberOfCards) ->
  io:format("Dealer ask player ~p for ~p cards~n", [Player1, NumberOfCards]),
  Player1 ! {self(),ask_for_cards, NumberOfCards},
  io:format("Dealer ask player ~p for ~p cards~n", [Player2, NumberOfCards]),
  Player2 ! {self(),ask_for_cards, NumberOfCards}.

await_battle_receive_cards_helper({PlayerPid, Cards, Player1, Player2, Piles, Piles1, Piles2, NumberOfResponds}) ->
  NewPiles = Piles ++ Cards,
  case PlayerPid of
    Player1 ->
      io:format("Dealer receive cards ~p from player ~p~n", [Cards, Player1]),
      NewPiles1 = Piles1 ++ Cards,
      NewPiles2 = Piles2;
    Player2 ->
      io:format("Dealer receive cards ~p from player ~p~n", [Cards, Player2]),
      NewPiles1 = Piles1,
      NewPiles2 = Piles2 ++ Cards
  end,
  if NumberOfResponds rem 2 =:= 0 ->
      dealer({await_battle, [Player1, Player2], NewPiles, NewPiles1, NewPiles2, NumberOfResponds + 1});
    true ->
      dealer({check_cards, [Player1, Player2], NewPiles, NewPiles1, NewPiles2, NumberOfResponds + 1})
  end.

await_battle_no_card_helper({PlayerPid, Player1, Player2, NumberOfResponds}) ->
  if NumberOfResponds rem 2 =:= 0 ->
      last_round_start({PlayerPid, Player1, Player2});
    true ->
      player_win({PlayerPid, Player1, Player2})
  end.

last_round_start({PlayerPid, Player1, Player2}) ->
  case PlayerPid of
    Player1 ->
      io:format("Player ~p has no card left, waiting for player ~p~n", [Player1, Player2]);
    Player2 ->
      io:format("Player ~p has no card left, waiting for player ~p~n", [Player2, Player1])
  end,
  dealer({last_round}).

player_win({PlayerPid, Player1, Player2}) ->
  case PlayerPid of
    Player1 ->
      io:format("Player ~p win, game over.~n", [Player2]);
    Player2 ->
      io:format("Player ~p win, game over.~n", [Player1])
  end.

player(Cards) ->
  receive
    {From, ask_for_cards, NumbersOfCards} ->
      io:format("Player ~p got request for ~p cards ~n", [self(), NumbersOfCards]),
      if length(Cards) >= NumbersOfCards ->
          {GiveAwayCards, LeftCards} = lists:split(NumbersOfCards, Cards),
          From ! {ok, self(), GiveAwayCards},
          io:format("Player ~p give cards: ~p start with cards: ~p~n", [self(), GiveAwayCards, LeftCards]),
          player(LeftCards);
        true ->
          io:format("Player ~p have no card left ~n", [self()]),
          From ! {no_card, self()}
      end;
    {give_cards, GivenCards} ->
      io:format("Player ~p got cards ~p from dealer ~n", [self(), GivenCards]),
      io:format("Player ~p start with cards: ~p~n", [self(), Cards ++ GivenCards]),
      player(Cards ++ GivenCards)
  end.

