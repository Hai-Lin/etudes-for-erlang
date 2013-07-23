-module(mnesia_utility).
-export([fill_table/5]).
-include("phone_records.hrl").

fill_table(TableName, FileName, AddDataFun, RecordInfo, TableType) ->
  init_table(TableName, RecordInfo, TableType),
  {Status, File} = file:open(FileName, [read]),
  case Status of
    ok ->
  mnesia:transaction(fun() -> read_file(AddDataFun,File) end);
    _ ->
      io:format("Read file ~p error~n", [FileName])
  end.

init_table(TableName, RecordInfo, TableType) ->
  mnesia:delete_table(TableName),
  mnesia:create_table(TableName,[{type, TableType}, {attributes, RecordInfo}]).

read_file(AddDataFun, File) ->
 Content = io:get_line(File, ""),
  case Content of 
    eof ->
      false;
    _ ->
      Items = re:split(re:replace(Content, "\\s+", "", [global]),",",[{return,list}]),
      io:format("~p~n", [Items]),
      AddDataFun(Items),
      read_file(AddDataFun, File)
  end.

