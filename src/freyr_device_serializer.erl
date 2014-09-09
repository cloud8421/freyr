-module(freyr_device_serializer).

-export([metadata/1]).

-include("freyr_reading.hrl").

metadata(DeviceId) when is_list(DeviceId) ->
  Readings = [LastReading | _Rest] = freyr_reading_store:by_device(DeviceId),
  Averages = freyr_reading_calc:averages(Readings),
  #{metadata => Averages,
    last_reading => freyr_reading_serializer:serialize(LastReading)}.
