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
  Body = json:to_binary([]),
  {Body, Req, undefined};

get_json(Req, DeviceId) ->
  case freyr_device:find(binary_to_list(DeviceId)) of
    nil -> {ok, Req2} = cowboy_req:reply(404, [], Req),
           {halt, Req2, DeviceId};
    Device -> Serialized = freyr_serializer:serialize(Device),
              Body = json:to_binary(Serialized),
              {Body, Req, DeviceId}
  end.
