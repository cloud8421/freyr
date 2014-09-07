-module(freyr_storage_test).

-c(freyr_storage).

-include_lib("eunit/include/eunit.hrl").

creates_table_test() ->
  mock_mnesia(),
  {ok, _Pid} = freyr_storage:start_link(),
  ?assert(meck:validate(mnesia)),
  meck:unload(mnesia).

mock_mnesia() ->
  meck:new(mnesia),
  meck:expect(mnesia, system_info, fun(tables) -> [] end),
  meck:expect(mnesia, create_table, fun(_TableName, _Props) -> {atomic, ok} end).
