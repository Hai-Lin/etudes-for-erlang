
-module(game).
-export([player/1]).


player(Cards) ->
    receive
      {From, NumbersOfCards} ->
        {GiveAwayCards, LeftCards} = lists:split(NumbersOfCards, Cards),
        From ! {GiveAwayCards},
        io:format("Play start with cards: ~p~n", [LeftCards]),
        player(LeftCards)
    end.
