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
  setup_http_logger(),
  start_cowboy(),
  mnesia:wait_for_tables([freyr_readings], 500),
  freyr_sup:start_link().

stop(_State) ->
  ok.

install() ->
  application:stop(mnesia),
  ok = mnesia:create_schema([node()]),
  application:start(mnesia),
  freyr_reading_store:create_table(),
  freyr_device_store:create_table(),
  application:stop(mnesia).

start_cowboy() ->
  DispatchSpec = [{'_', [{"/devices/[:device_id]/readings/[:reading_id]", freyr_reading_handler, []},
                         {"/devices/[:device_id]", freyr_device_handler, []}]}],
  Dispatch = cowboy_router:compile(DispatchSpec),
  {ok, _} = cowboy:start_http(http, 100, [{port, freyr_settings:http_port()},
                                          {ip, freyr_settings:http_host()}],
                              [
                               {compress, true},
                               {env, [{dispatch, Dispatch}]},
                               {onresponse, fun log/4}
                              ]).

log(Status, _Headers, _Body, Req) ->
  EventPayload = #{request => Req, status => Status},
  gen_event:notify({global, http_logger}, EventPayload),
  Req.

setup_http_logger() ->
  {ok, HttpLogger} = gen_event:start_link({global, http_logger}),
  gen_event:add_handler(HttpLogger, freyr_logger, []).
