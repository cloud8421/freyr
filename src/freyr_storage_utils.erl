-module(freyr_storage_utils).

-export([create_table/2]).

create_table(TableName, Attributes) ->
  case table_exists(TableName) of
    true -> ok;
    false -> mnesia:create_table(TableName,
                                [{type, ordered_set},
                                 {disc_copies, [node()]},
                                 {attributes, Attributes}])
  end.

table_exists(TableName) ->
  Tables = mnesia:system_info(tables),
  lists:member(TableName, Tables).
