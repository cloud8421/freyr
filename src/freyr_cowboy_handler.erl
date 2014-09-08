-module(freyr_cowboy_handler).

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
  {AllValues, Req2} = cowboy_req:qs_vals(Req),
  Readings = get_data(AllValues),
  Serialized = freyr_reading_serializer:serialize(Readings),
  Body = jsx:encode(Serialized),
  {Body, Req2, State}.

get_data([]) -> freyr_storage:all();
get_data([{<<"hour">>, Hour}]) -> freyr_storage:by_hour(binary_to_integer(Hour));
get_data([{<<"device">>, Device}]) -> freyr_storage:by_device(binary_to_list(Device)).
