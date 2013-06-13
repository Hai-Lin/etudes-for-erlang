%% @author Hai Lin <hai.lin.cs@gmail.com>
%% @doc Functions for calculating areas of geometric shapes.

-module(geom).
-export([area/2]).

%% @doc Calculates the area of a rectangle, given the
%% length and width. Returns the product
%% of its arguments.

-spec(area(number(),number()) ->number()).

area(Height, Width) ->
	Height * Width.
