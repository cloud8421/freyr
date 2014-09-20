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
  case freyr_device:find(binary_to_list(DeviceId)) of
    nil -> {ok, Req3} = cowboy_req:reply(404, [], Req2),
           {halt, Req3, DeviceId};
    _Device -> Readings = get_readings(DeviceId),
               Serialized = freyr_serializer:serialize(Readings),
               Body = json:to_binary(Serialized),
               {Body, Req2, State}
  end.

get_readings(DeviceId) ->
  freyr_reading:by_device(binary_to_list(DeviceId)).
