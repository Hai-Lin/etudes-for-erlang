%% @author Hai Lin <hai.lin.cs@gmail.com>
%% @doc Functions for calculating areas of geometric shapes.

-module(geom).
-export([area/1]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Calculates the area of a shape, given the name of the shape
%%  and other two geo parameters. Returns the area, if the shape unrognized, return 0.

-spec(area({atom(),number(),number()}) ->number()).

area({Shape, Dim1, Dim2}) when Dim1 >= 0, Dim2 >=0 ->
	case Shape of 
		ellipse -> 
			math:pi() * Dim1 * Dim2; 
		triangle ->
			(Dim1 * Dim2) / 2;
		rectangle ->
			Dim1 * Dim2;
		true -> 0
	end.
area_test_() ->
	[?_assert(area({ellipse, 7, 3}) =:= 65.97344572538566), 
		?_assert(area({triangle, 7, 3}) =:= 10.5), 
		?_assert(area({rectangle, 7, 3}) =:= 21) 
	].
