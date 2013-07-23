
-module(mnesia_setup).
-export([setup/0]).
-include("phone_records.hrl").

setup() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia_utility:fill_table(phone_call, "call_data.csv", fun add_data_to_phone_table/1, record_info(fields, phone_call), bag),
  mnesia_utility:fill_table(customer, "customer_data.csv", fun add_data_to_customer_table/1, record_info(fields, customer), set).

add_data_to_phone_table([PhoneNumber, StartDate, StartTime, EndDate, EndTime]) ->
  mnesia:write(#phone_call{phone_number=PhoneNumber, start_date=convert:string_to_date(StartDate), start_time=convert:string_to_time(StartTime), end_date=convert:string_to_date(EndDate), end_time=convert:string_to_time(EndTime)}).

add_data_to_customer_table([PhoneNumber, LastName, FirstName, MiddleName, Rate]) ->
  mnesia:write(#customer{phone_number=PhoneNumber, last_name=LastName, first_name=FirstName, middle_name=MiddleName, rate=convert:string_to_number(Rate)}).
