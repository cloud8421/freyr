-module(freyr_plant_handler).

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
  {ok, Req2} = cowboy_req:reply(404, [], Req),
  {halt, Req2, undefined};

get_json(Req, DeviceId) ->
  case freyr_device:find(binary_to_list(DeviceId)) of
    nil -> {ok, Req2} = cowboy_req:reply(404, [], Req),
           {halt, Req2, DeviceId};
    _Device -> Plants = get_plants(DeviceId),
               Serialized = freyr_serializer:serialize(Plants),
               Body = jsx:encode(Serialized),
               {Body, Req, DeviceId}
  end.

get_plants(DeviceId) ->
  freyr_plant:by_device(binary_to_list(DeviceId)).
