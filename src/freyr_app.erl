-module(freyr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, install/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  lager:start(),
  application:start(crypto),
  application:start(ranch),
  application:start(cowlib),
  application:start(cowboy),
  mnesia:start(),
  start_cowboy(),
  mnesia:wait_for_tables([freyr_readings], 500),
  freyr_sup:start_link().

stop(_State) ->
  ok.

install() ->
  ok = mnesia:create_schema([node()]),
  application:start(mnesia),
  freyr_readings_storage:create_table(),
  application:stop(mnesia).

start_cowboy() ->
  DispatchSpec = [{'_', [{"/readings/[:reading_id]", freyr_cowboy_handler, []}]}],
  Dispatch = cowboy_router:compile(DispatchSpec),
  {ok, _} = cowboy:start_http(http, 100, [{port, 9000}], [{env, [{dispatch, Dispatch}]}]).
