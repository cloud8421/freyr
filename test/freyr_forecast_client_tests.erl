-module(freyr_forecast_client_tests).

-c(freyr_forecast_client).
-c(mocks).

-include_lib("eunit/include/eunit.hrl").

fetches_test() ->
  mocks:mock_forecast_client_request(),
  Lat = 51.5199579,
  Lng = -0.0990549,
  Expected = #{<<"apparentTemperature">> => 16.42,
               <<"cloudCover">> => 0.73,
               <<"dewPoint">> => 14,
               <<"humidity">> => 0.86,
               <<"icon">> => <<"partly-cloudy-night">>,
               <<"nearestStormBearing">> => 6,
               <<"nearestStormDistance">> => 45,
               <<"ozone">> => 274.39,
               <<"precipIntensity">> => 0,
               <<"precipProbability">> => 0,
               <<"pressure">> => 1019.55,
               <<"summary">> => <<"Mostly Cloudy">>,
               <<"temperature">> => 16.42,
               <<"time">> => 1411248932,
               <<"visibility">> => 7,
               <<"windBearing">> => 5,
               <<"windSpeed">> => 2.45},
  Actual = freyr_forecast_client:fetch(Lat, Lng),
  ?assertEqual(Expected, Actual).
