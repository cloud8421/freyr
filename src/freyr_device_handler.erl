-module(freyr_device_handler).

-export([init/3]).
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2, resource_exists/2]).
-export([get_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"HEAD">>, <<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

resource_exists(Req, _State) ->
  case cowboy_req:binding(device_id, Req) of
    {undefined, Req2} ->
      {true, Req2, undefined};
    {DeviceId, Req2} ->
      {true, Req2, DeviceId}
  end.

get_json(Req, undefined) ->
  Body = jsx:encode([]),
  {Body, Req, undefined};

get_json(Req, DeviceId) ->
  Readings = freyr_reading_store:by_device(binary_to_list(DeviceId)),
  Serialized = freyr_reading_serializer:serialize(Readings),
  Body = jsx:encode(Serialized),
  {Body, Req, DeviceId}.
