
-module(game).
-export([player/1]).


player(Cards) ->
  receive
    {From, NumbersOfCards} ->
      if length(Cards) >= NumbersOfCards ->
          {GiveAwayCards, LeftCards} = lists:split(NumbersOfCards, Cards),
          From ! {ok, GiveAwayCards},
          io:format("Play start with cards: ~p~n", [LeftCards]),
          player(LeftCards);
        true ->
          io:format("I lose ~n"),
          From ! {"I lose"}
      end
  end.

