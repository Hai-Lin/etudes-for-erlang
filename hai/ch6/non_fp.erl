-module(non_fp).
-export([generate_teeth/2, t_teeth/0]).


generate_teeth(IsExist, Possibility) ->
	generate_teeth(IsExist, Possibility, []).

generate_teeth([], _, Results) ->
	lists:reverse(Results);

generate_teeth([$F|T], Possibility, Results) ->
	generate_teeth(T, Possibility, [[0] | Results]);

generate_teeth([$T|T], Possibility, Results)  ->
	generate_teeth(T, Possibility, [ generate_teeth(Possibility) | Results ]).

generate_teeth(Possibility) ->
	RandomNumber = generate_random_number(),
	if 
		RandomNumber < Possibility ->
			BaseDepth = 2;
		true ->
			BaseDepth = 3
	end,
	generate_tooth(BaseDepth, 6, []).

generate_tooth(_, 0, AccumulatedList) ->
	AccumulatedList;

generate_tooth(BaseDepth, LeftNumber, AccumulatedList) ->
	RandonNumber = generate_random_number() * 3,
	if 
		RandonNumber >= 2 ->
			AddNumber = 1;
		RandonNumber >= 1 ->
			AddNumber = 0;
		true -> 
			AddNumber = -1
	end,
	generate_tooth(BaseDepth, LeftNumber - 1, [AddNumber + BaseDepth | AccumulatedList]).

generate_random_number() ->
	random:seed(now()),
	random:uniform().

%% Below are functions copied from suggested solution

t_teeth() ->
	TList = "FTTTTTTTTTTTTTTFTTTTTTTTTTTTTTTT",
	N = generate_teeth(TList, 0.75),
	print_tooth(N).

print_tooth([]) -> io:format("Finished.~n");
print_tooth([H|T]) ->
	io:format("~p~n", [H]),
	print_tooth(T).
