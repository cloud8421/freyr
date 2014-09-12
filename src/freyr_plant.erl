-module(freyr_plant).

-export([health_status/2]).

-include("freyr_plant.hrl").
-include("freyr_reading.hrl").

-define(TOLERANCE, 20).

health_status(Plant, LastReading) ->
  TemperatureStatus = check_temperature(LastReading#freyr_reading.temperature,
                                        Plant#freyr_plant.optimal_temperature),
  HumidityStatus = check_humidity(LastReading#freyr_reading.moisture,
                                  Plant#freyr_plant.optimal_humidity),
  BrightnessStatus = check_brightness(LastReading#freyr_reading.brightness,
                                      Plant#freyr_plant.optimal_brightness),
  #{humidity => HumidityStatus,
    temperature => TemperatureStatus,
    brightness => BrightnessStatus}.

check_temperature(Actual, Ideal) ->
  check_delta(100.0 - (Actual * 100.0 / Ideal)).

check_humidity(Actual, Ideal) ->
  check_delta(100 - (Actual * 100/ Ideal)).

check_brightness(Actual, Ideal) ->
  check_delta(100 - (Actual * 100/ Ideal)).

check_delta(0.0) -> optimal;
check_delta(Delta) when Delta < 0 andalso Delta >= -?TOLERANCE -> high;
check_delta(Delta) when Delta < -?TOLERANCE -> critical_high;
check_delta(Delta) when Delta > 0 andalso Delta =< ?TOLERANCE -> low;
check_delta(Delta) when Delta > ?TOLERANCE -> critical_low.

