-module(freyr_forecast_client_tests).

-c(freyr_forecast_client).

-include_lib("eunit/include/eunit.hrl").

parses_response_test() ->
  Response = "{\"latitude\":51.519958,\"longitude\":-0.099055,\"timezone\":\"Europe/London\",\"offset\":1,\"currently\":{\"time\":1411229707,\"summary\":\"Mostly Cloudy\",\"icon\":\"partly-cloudy-day\",\"nearestStormDistance\":34,\"nearestStormBearing\":343,\"precipIntensity\":0,\"precipProbability\":0,\"temperature\":17.48,\"apparentTemperature\":17.48,\"dewPoint\":15.25,\"humidity\":0.87,\"windSpeed\":1.63,\"windBearing\":358,\"visibility\":7,\"cloudCover\":0.83,\"pressure\":1017.03,\"ozone\":271.32}}",
  Expected = #{<<"apparentTemperature">> => 17.48,
    <<"cloudCover">> => 0.83,
    <<"dewPoint">> => 15.25,
    <<"humidity">> => 0.87,
    <<"icon">> => <<"partly-cloudy-day">>,
    <<"nearestStormBearing">> => 343,
    <<"nearestStormDistance">> => 34,
    <<"ozone">> => 271.32,
    <<"precipIntensity">> => 0,
    <<"precipProbability">> => 0,
    <<"pressure">> => 1017.03,
    <<"summary">> => <<"Mostly Cloudy">>,
    <<"temperature">> => 17.48,
    <<"time">> => 1411229707,
    <<"visibility">> => 7,
    <<"windBearing">> => 358,
    <<"windSpeed">> => 1.63},
  ?assertEqual(Expected, freyr_forecast_client:parse_response(Response)).
