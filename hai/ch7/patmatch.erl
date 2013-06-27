  
-module(patmatch).
-export([older_males/1, older_people/1]).
-include_lib("eunit/include/eunit.hrl").


older_males(People) ->
  [Name || {Name, Gender, Age} <- People, Gender == $M, Age > 40].

older_people(People) ->
  [Name || {Name, Gender, Age} <- People, Gender == $M orelse Age > 40].

older_males_test_() ->
  People = [{"Federico", $M, 22}, {"Kim", $F, 45}, {"Hansa", $F, 30},
            {"Tran", $M, 47}, {"Cathy", $F, 32}, {"Elias", $M, 50}],
  [?_assert(older_males(People) =:= ["Tran", "Elias"])
  ].

older_people_test_() ->
  People = [{"Federico", $M, 22}, {"Kim", $F, 45}, {"Hansa", $F, 30},
            {"Tran", $M, 47}, {"Cathy", $F, 32}, {"Elias", $M, 50}],
  [?_assert(older_people(People) =:= ["Federico", "Kim", "Tran", "Elias"])
  ].
