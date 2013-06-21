%% @author Hai Lin <hai.lin.cs@gmail.com>
%% @doc Functions for calculating areas of geometric shapes.

-module(geom).
-export([ask_area/0, get_number/1]).
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


ask_area() ->
	Shape = get_shape(),
	Dimensions = get_dimensions(Shape),
	if 
		Dimensions /= ok ->
			area({Shape, element(1, Dimensions), element(2, Dimensions)});
		true -> ok
	end.

get_shape()  ->
	Char = io:get_chars("R)ectangle, T)riangle, or E)llipse >", 1),
	char_to_shape(Char).

get_number(Prompt) ->
	P =	"Enter " ++ Prompt ++ " > ",
	Result = io:get_line(P),
	io:format(Result),
	string_to_number(Result).

string_to_number(String) ->
	Number = string:to_float(String),
	case Number of 
		{error,_} -> element(1, string:to_integer(String));
		_ -> element(1, Number)
	end.

get_dimensions(Shape) when Shape == triangle ->
	Dimension1 = get_number("width"),
	Dimension2 = get_number("height"),
	{Dimension1, Dimension2}; 

get_dimensions(Shape) when Shape == rectangle ->
	Dimension1 = get_number("base"),
	Dimension2 = get_number("height"),
	{Dimension1, Dimension2}; 

get_dimensions(Shape) when Shape == ellipse ->
	Dimension1 = get_number("axis"),
	Dimension2 = get_number("axis"),
	{Dimension1, Dimension2}; 

get_dimensions(Shape) when Shape == unknown ->
	io:format("Unknown shape").		

char_to_shape(Char)  ->
	case Char of
		"T" -> triangle;
		"t" -> triangle;
		"R" -> rectangle;
		"r" -> rectangle;
		"E" -> ellipse;
		"e" -> ellipse;
		_ -> unknown
	end.


string_to_number_test_() ->
	[?_assert(string_to_number("5.5") =:= 5.5),
		?_assert(string_to_number("5") =:= 5),
		?_assert(string_to_number("0") =:= 0),
		?_assert(string_to_number("-1") =:= -1)
	].

char_to_shape_test_() ->
	[?_assert(char_to_shape("T") =:= triangle),
		?_assert(char_to_shape("t") =:= triangle),
		?_assert(char_to_shape("R") =:= rectangle),
		?_assert(char_to_shape("r") =:= rectangle),
		?_assert(char_to_shape("E") =:= ellipse),
		?_assert(char_to_shape("e") =:= ellipse)
	].
