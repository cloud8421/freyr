-module(mocks).

-export([default_uuid/0, default_timestamp/0, mock_calendar/0, mock_uuid/0,
        mock_forecast_client_request/0]).

-define(DEFAULT_UUID, <<131,178,187,218,53,74,17,228,163,40,184,232,86,58,114,232>>).
-define(DEFAULT_TIMESTAMP, {{2014,9,5},{22,23,18}}).

default_uuid() ->
  uuid:to_string(?DEFAULT_UUID).

default_timestamp() ->
  ?DEFAULT_TIMESTAMP.

mock_uuid() ->
  meck:new(uuid, [passthrough]),
  meck:expect(uuid, uuid1, fun() -> ?DEFAULT_UUID end).

mock_calendar() ->
  meck:new(calendar, [unstick]),
  meck:expect(calendar, universal_time, fun() -> ?DEFAULT_TIMESTAMP end).

mock_forecast_client_request() ->
  meck:new(ibrowse),
  meck:expect(ibrowse, send_req, fun(_Url, [], get) ->
                                     {ok, "200", [], weather_response()}
                                 end).


weather_response() ->
  {ok, BinaryResponse} = file:read_file("../test/fixtures/weather.json"),
  binary_to_list(BinaryResponse).
