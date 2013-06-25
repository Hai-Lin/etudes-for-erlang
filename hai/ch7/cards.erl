
-module(cards).
-export([make_deck/0]).

make_deck() ->
	Deck =  [{Value, Suit} || Value <- ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"],
    Suit <- ["Clubs", "Diamonds", "Hearts", "Spades"]],
	show_deck(Deck).

show_deck(Deck) ->
	lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).
