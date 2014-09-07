-module(freyr_api).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3, terminate/4]).
-export([get/3]).

init(_Route, _Req, State) ->
  {ok, State}.

get("/readings", _Req, State) ->
  Readings = freyr_storage:all(),
  Serialized = freyr_reading_serializer:serialize(Readings),
  {ok, {json, Serialized}, State}.

terminate(_Reason, _Route, _Req, _State) ->
  ok.
