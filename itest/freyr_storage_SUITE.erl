-module(freyr_storage_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         inserts_reading/1, finds_by_hour/1]).

all() -> [inserts_reading, finds_by_hour].

init_per_suite(Config) ->
  Priv = ?config(priv_dir, Config),
  application:set_env(mnesia, dir, Priv),
  freyr_app:install(),
  mnesia:start(),
  mnesia:wait_for_tables([freyr_readings], 5000),
  Config.

init_per_testcase(_, Config) ->
  {ok, _Pid} = freyr_storage:start_link(),
  Config.

inserts_reading(_Config) ->
  [] = freyr_storage:all(),
  NewReading = {freyr_reading,"ee6ab164-366f-11e4-a7d9-b8e8563a72e8",23.7,
                390,3700,{{2014,9,7},{9,18,29}}},
  ok = freyr_storage:insert(NewReading),
  [NewReading] = freyr_storage:all().

finds_by_hour(_Config) ->
  NewReading = {freyr_reading,"ee6ab164-366f-11e4-a7d9-b8e8563a72e8",23.7,
                390,3700,{{2014,9,7},{9,18,29}}},
  ok = freyr_storage:insert(NewReading),
  [NewReading] = freyr_storage:by_hour(9).

end_per_suite(_Config) ->
  application:stop(mnesia),
  ok.
