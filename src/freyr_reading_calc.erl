-module(freyr_reading_calc).

-export([averages/1]).

-include("../src/freyr_reading.hrl").

averages(Readings) when is_list(Readings) ->
  TemperatureValues = lists:map(fun(R) -> R#freyr_reading.temperature end, Readings),
  BrightnessValues  = lists:map(fun(R) -> R#freyr_reading.brightness end, Readings),
  MoistureValues    = lists:map(fun(R) -> R#freyr_reading.moisture end, Readings),
  #{average_temperature => average_floats(TemperatureValues),
    average_brightness  => average_integers(BrightnessValues),
    average_moisture    => average_integers(MoistureValues)}.


average_floats(Values) ->
  Total = lists:foldl(fun(V, Sum) -> V + Sum end, 0, Values),
  Total/length(Values).

average_integers(Values) ->
  Total = lists:foldl(fun(V, Sum) -> V + Sum end, 0, Values),
  Total div length(Values).
