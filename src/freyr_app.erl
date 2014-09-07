-module(freyr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, install/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  lager:start(),
  mnesia:start(),
  mnesia:wait_for_tables([freyr_readings], 5000),
  freyr_sup:start_link().

stop(_State) ->
  ok.

install() ->
  ok = mnesia:create_schema([node()]),
  application:start(mnesia),
  freyr_storage:create_table(),
  application:stop(mnesia).
