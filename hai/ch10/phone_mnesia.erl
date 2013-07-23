-module(phone_mnesia).
-export([setup/0, summary/3]).
-include("phone_records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  mnesia_setup:setup().

summary(LastName, FirstName, MiddleName) ->
  {atomic, Results} = mnesia:transaction(
      fun() -> qlc:e(
            qlc:q([{C#customer.phone_number, C#customer.rate, convert:calculate_number_time({P#phone_call.start_date, P#phone_call.start_time}, {P#phone_call.end_date, P#phone_call.end_time})} ||
                P <- mnesia:table(phone_call),
                C <- mnesia:table(customer),
                C#customer.first_name == FirstName,
                C#customer.last_name == LastName,
                C#customer.middle_name == MiddleName,
                P#phone_call.phone_number == C#customer.phone_number])
            )
      end
      ),
  Minutes = lists:foldl(fun total_minutes/2, 0, Results),
  {Number, Rate, _} = hd(Results),
  {Number, Minutes, Minutes * Rate}.

total_minutes({_, _, Minutes}, Accumlator) -> 
  Accumlator + Minutes.


summary_test_() ->
  setup(),
  [?_assert(summary("Smith", "Samuel", "Steven") =:= {"838-555-1099",9,0.9}),
   ?_assert(summary("Nakamura", "Noriko", "") =:= {"213-555-0172",9,1.08})].
