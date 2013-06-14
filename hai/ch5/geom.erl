%% @author Hai Lin <hai.lin.cs@gmail.com>
%% @doc Functions for calculating areas of geometric shapes.

-module(geom).
-export([ask_area/0]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Calculates the area of a shape, given the name of the shape
%%  and other two geo parameters. Returns the area, if the shape unrognized, return 0.

-spec(area({atom(),number(),number()}) ->number()).


ask_area() ->
		1.


get_number(Prompt) ->
		P =	"Enter " ++ Prompt ++ ">",
		Result = io:get_chars(P,1),
		string_to_number(Result).


string_to_number(String) ->
		Number = string:to_float(String),
		case Number of 
				error -> string:to_integer(Number);
				_ -> Number
		end.


char_to_shape(Char)  ->
		case Char of
				"T" -> shape;
				"t" -> shape;
				"R" -> rectangle;
				"r" -> rectangle;
				"E" -> ellipse;
				"e" -> ellipse;
				_ -> unknow
		end.



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

get_number_test() ->
		[?_assert(get_number("5.5") =:= 5.5),
				?_assert(get_number("5") =:= 5),
				?_assert(get_number("0") =:= 0),
				?_assert(get_number("-1") =:= -1)
		].

char_to_shape_test() ->
		[?_assert(char_to_shape("T") =:= shape),
				?_assert(char_to_shape("t") =:= shape),
				?_assert(char_to_shape("R") =:= rectangle),
				?_assert(char_to_shape("r") =:= rectangle),
				?_assert(char_to_shape("E") =:= ellipse),
				?_assert(char_to_shape("e") =:= ellipse)
		].
