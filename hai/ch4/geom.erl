%% @author Hai Lin <hai.lin.cs@gmail.com>
%% @doc Functions for calculating areas of geometric shapes.

-module(geom).
-export([area/1]).

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

