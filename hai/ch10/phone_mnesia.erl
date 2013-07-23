-module(phone_mnesia).
-export([setup/0, summary/3]).
-include("phone_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

setup() ->
  mnesia_setup:setup().

summary(LastName, FirstName, MiddleName) ->
  mnesia:transaction(
    fun() -> qlc:e(
          qlc:q([{C#customer.phone_number, convert:calculate_number_time({P#phone_call.start_date, P#phone_call.start_time}, {P#phone_call.end_date, P#phone_call.end_time}),
                  (convert:calculate_number_time({P#phone_call.start_date, P#phone_call.start_time}, {P#phone_call.end_date, P#phone_call.end_time}))*C#customer.rate} || 
              P <- mnesia:table(phone_call),
              C <- mnesia:table(customer),
              C#customer.first_name == FirstName,
              C#customer.last_name == LastName,
              C#customer.middle_name == MiddleName,
              P#phone_call.phone_number == C#customer.phone_number])
          )
    end
    ).



