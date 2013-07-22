-module(mnesia_utility).
-export([fill_table/5]).


fill_table(TableName, FileName, AddDataFun, RecordInfo, TableType) ->
  init_table(TableName, RecordInfo, TableType),
  mnesia:transaction(AddDataFun(FileName)).


init_table(TableName, RecordInfo, TableType) ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(TableName,[{type, TableType}, {attributes, RecordInfo}]).

