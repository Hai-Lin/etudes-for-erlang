-module(mnesia_utility).
-export([fill_table/5]).

fill_table(TableName, FileName, AddDataFun, RecordInfo, TableType) ->
  init_table(TableName, RecordInfo, TableType),
  mnesia:transaction(read_file(AddDataFun,file:open(FileName, [read]))).

init_table(TableName, RecordInfo, TableType) ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(TableName,[{type, TableType}, {attributes, RecordInfo}]).

read_file(AddDataFun, File) ->
  Content = io:get_line(File, ""),
  case Content of 
    eof ->
      false;
    _ ->
      Items = re:split(re:replace(Content, "\\s+", "", [global]),",",[{return,list}]),
      AddDataFun(Items),
      read_file(AddDataFun, File)
  end.
