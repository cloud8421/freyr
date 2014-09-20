-module(freyr_forecast_client).

-export([fetch/2]).

-define(EXCLUDED_DATA, "alerts,flags").
-define(DATA_FORMAT, "si").

fetch(Lat, Lng) ->
  Url = api_url(Lat, Lng),
  {ok, "200", _Headers, Body} = ibrowse:send_req(Url, [], get),
  parse_response(Body).

api_url(Lat, Lng) ->
  LatString = float_to_list(Lat, [{decimals, 6}]),
  LngString = float_to_list(Lng, [{decimals, 6}]),
  "https://api.forecast.io/forecast/" ++ api_key() ++ "/" ++ LatString ++ "," ++ LngString ++ "?exclude=" ++ ?EXCLUDED_DATA ++ "&units=" ++ ?DATA_FORMAT.

parse_response(Body) ->
  BinaryBody = list_to_binary(Body),
  Parsed = json:from_binary(BinaryBody),
  json:get(<<"/currently">>, Parsed).

api_key() ->
  os:getenv("FORECAST_IO_API_KEY").
