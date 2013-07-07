
-module(cards).
-export([make_deck/0, show_deck/1, compare/2]).
-include_lib("eunit/include/eunit.hrl").

make_deck() ->
  Deck =  [{Value, Suit} || Value <- ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"],
                            Suit <- ["Clubs", "Diamonds", "Hearts", "Spades"]],
  shuffle(Deck).

show_deck(Deck) ->
  lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).

shuffle(List) -> shuffle(List, []).
shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
  {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
  shuffle(Leading ++ T, [H | Acc]).

compare(Piles1, Piles2) ->
  io:format("Now compare ~p with ~p ~n", [Piles1, Piles2]),
  LastOfPiles1 = lists:last(Piles1),
  LastOfPiles2 = lists:last(Piles2),
  {Value1, _} = LastOfPiles1,
  {Value2, _} = LastOfPiles2,
  if Value1 == Value2 ->
      io:format("~p equals to ~p ~n", [Piles1, Piles2]),
      equal;
    Value1 =:= "A" ->
      io:format("~p is bigger than  ~p ~n", [Piles1, Piles2]),
      bigger;
    Value2 =:= "A" ->
      io:format("~p is smaller than  ~p ~n", [Piles1, Piles2]),
      smaller;
    Value1 > Value2 ->
      io:format("~p is bigger than  ~p ~n", [Piles1, Piles2]),
      bigger;
    Value1 < Value2 ->
      io:format("~p is smaller than  ~p ~n", [Piles1, Piles2]),
      smaller
  end.

compare_test_() ->
  [?_assert(compare([{7,"Clubs"},{9,"Diamonds"}],
                    [{10,"Diamonds"},{10,"Clubs"}]) =:= smaller),
   ?_assert(compare([{7,"Clubs"},{"Q","Diamonds"}],
                    [{10,"Diamonds"},{10,"Clubs"}]) =:= bigger),
   ?_assert(compare([{7,"Clubs"},{"Q","Diamonds"}],
                    [{10,"Diamonds"},{"Q","Clubs"}]) =:= equal),
   ?_assert(compare([{7,"Clubs"},{"Q","Diamonds"}],
                    [{10,"Diamonds"},{"A","Clubs"}]) =:= smaller)
  ].
