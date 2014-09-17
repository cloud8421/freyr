-module(freyr_device).

-export([find/1, create/1]).

-include("freyr_reading.hrl").
-include("freyr_device_with_metadata.hrl").

find(DeviceId) ->
  case freyr_device_store:by_id(DeviceId) of
    nil -> nil;
    Device -> Metadata = metadata(DeviceId),
              {ok, Averages} = maps:find(averages, Metadata),
              {ok, LastReading} = maps:find(last_reading, Metadata),
              #freyr_device_with_metadata{device=Device,
                                          averages=Averages,
                                          last_reading=LastReading}
  end.

create(NewDevice) ->
  Insertion = fun() ->
                  mnesia:write(NewDevice)
              end,
  mnesia:transaction(Insertion).

metadata(DeviceId) when is_list(DeviceId) ->
  Readings = [LastReading | _Rest] = freyr_reading_store:by_device(DeviceId),
  Averages = freyr_reading_calc:averages(Readings),
  #{averages => Averages,
    last_reading => LastReading}.
