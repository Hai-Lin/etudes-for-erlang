-module(game).
-export([player/1, dealer/1]).


dealer({init, _, _, _, _, _}) ->
  io:format("Dealers init game~n"),
  io:format("Init player1~n"),
  Player1 = spawn(game, player, [[1,2,0,1, 1]]),
  io:format("Init player2~n"),
  Player2 = spawn(game, player, [[1,4,3,2, 2]]),
  dealer({pre_battle, [Player1, Player2],[], [], [], 0});

dealer({pre_battle, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds}) when NumberOfResponds =< 1->
  io:format("PreBattle start with players: ~p~n",[[Player1, Player2]]),
  io:format("Ask player1 ~p for cards~n", [Player1]),
  Player1 ! {self(),ask_for_cards, 1},
  io:format("Ask player2 ~p for cards~n", [Player2]),
  Player2 ! {self(),ask_for_cards, 1},
  dealer({await_battle, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds});

dealer({pre_battle, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds}) ->
  io:format("War start with players: ~p~n",[[Player1, Player2]]),
  io:format("Ask player1 ~p for cards~n", [Player1]),
  Player1 ! {self(),ask_for_cards, 3},
  io:format("Ask player2 ~p for cards~n", [Player2]),
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
    {lose, PlayerPid} ->
      io:format("Player ~p is lose, game over ~n", [PlayerPid])
  end;

dealer({check_cards, [Player1, Player2], Piles, Piles1, Piles2, NumberOfResponds}) ->
  CompareResult = compare(Piles1, Piles2),
  case CompareResult of
    bigger ->
      Player1 ! {give_cards, Piles},
      dealer({pre_battle, [Player1,Player2], [], [], [], 0});
    smaller ->
      Player2 ! {give_cards, Piles},
      dealer({pre_battle, [Player1,Player2], [], [], [], 0});
    equal ->
      dealer({pre_battle, [Player1,Player2], Piles, Piles1, Piles2, NumberOfResponds})
  end;

dealer(_) ->
  io:format("Ending...~n").

player(Cards) ->
  receive
    {From, ask_for_cards,NumbersOfCards} ->
      if length(Cards) >= NumbersOfCards ->
          {GiveAwayCards, LeftCards} = lists:split(NumbersOfCards, Cards),
          From ! {ok, self(), GiveAwayCards},
          io:format("Player ~p start with cards: ~p~n", [self(), LeftCards]),
          player(LeftCards);
        true ->
          io:format("I lose ~n"),
          From ! {lose, self()}
      end;
    {give_cards, GivenCards} ->
      player(Cards ++ GivenCards)
  end.

compare(Piles1, Piles2) ->
  io:format("Now compare ~p with ~p ~n", [Piles1, Piles2]),
  LastOfPiles1 = lists:last(Piles1),
  LastOfPiles2 = lists:last(Piles2),
  if LastOfPiles1 == LastOfPiles2 ->
      io:format("~p equals to ~p ~n", [Piles1, Piles2]),
      equal;
    LastOfPiles1 > LastOfPiles2 ->
      io:format("~p is bigger than  ~p ~n", [Piles1, Piles2]),
      bigger;
    LastOfPiles1 < LastOfPiles2 ->
      io:format("~p is smaller than  ~p ~n", [Piles1, Piles2]),
      smaller
  end.

