-module(freyr_reading_handler).

-export([init/3]).
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2]).
-export([get_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"HEAD">>, <<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

get_json(Req, State) ->
  {DeviceId, Req2} = cowboy_req:binding(device_id, Req),
  Readings = get_readings(DeviceId),
  Serialized = freyr_serializer:serialize(Readings),
  Body = jsx:encode(Serialized),
  {Body, Req2, State}.

get_readings(DeviceId) ->
  freyr_reading_store:by_device(binary_to_list(DeviceId)).
