%% @author Hai Lin <hai.lin.cs@gmail.com>
%% @doc Functions for calculating areas of geometric shapes.

-module(geom).
-export([area/1]).

%% @doc Calculates the area of a shape, given the name of the shape
%%  and other two geo parameters. Returns the area, if the shape unrognized, return 0.

-spec(area({atom(),number(),number()}) ->number()).

area({Shape, Dim1, Dim2}) -> area(Shape, Dim1, Dim2).

area(ellipse, Height, Width) when Height >= 0, Width >= 0 ->
	math:pi() * Height * Width; 

area(triangle, Height, Width) when Height >= 0, Width >= 0 ->
	(Height * Width) / 2;

area(rectangle, Height, Width) when Height >= 0, Width >= 0 -> 
		Height * Width;

area(_,_,_) -> 0.
