-module(freyr_reading).

-export([find/1, create/1, all/0, by_hour/1, by_device/1,
         last_by_device/1]).
-export([create_table/0]).

-include("freyr_reading.hrl").
-include("freyr_device.hrl").
-include("freyr_device_with_metadata.hrl").

-define(TABLE_NAME, freyr_reading).

find(DeviceId) when is_list(DeviceId) ->
  case do_find(DeviceId) of
    nil -> nil;
    Device -> Metadata = metadata(DeviceId),
              {ok, Averages} = maps:find(averages, Metadata),
              {ok, LastReading} = maps:find(last_reading, Metadata),
              #freyr_device_with_metadata{device=Device,
                                          averages=Averages,
                                          last_reading=LastReading}
  end.

by_hour(Hour) when is_integer(Hour) ->
  Q = fun() ->
          freyr_queries:request({reading, by_hour, Hour})
      end,
  {atomic, Readings} = mnesia:transaction(Q),
  Readings.

by_device(DeviceId) when is_list(DeviceId) ->
  Q = fun() ->
          freyr_queries:request({reading, by_device, DeviceId})
      end,
  {atomic, Readings} = mnesia:transaction(Q),
  Readings.

last_by_device(DeviceId) when is_list(DeviceId) ->
  Q = fun() ->
          freyr_queries:request({reading, last_by_device, DeviceId})
      end,
  {atomic, Reading} = mnesia:transaction(Q),
  Reading.

all() ->
  Q = fun() ->
          freyr_queries:request({reading, all})
      end,
  {atomic, Devices} = mnesia:transaction(Q),
  Devices.

create(NewReading) ->
  Insertion = fun() ->
                  mnesia:write(NewReading)
              end,
  mnesia:transaction(Insertion).

%% Private

metadata(DeviceId) when is_list(DeviceId) ->
  Readings = [LastReading | _Rest] = freyr_reading:by_device(DeviceId),
  Averages = freyr_reading_calc:averages(Readings),
  #{averages => Averages,
    last_reading => LastReading}.

do_find(DeviceId) ->
  Q = fun() ->
          mnesia:read(?TABLE_NAME, DeviceId)
      end,
  case mnesia:transaction(Q) of
    {atomic, [Device]} -> Device;
    {atomic, []} -> nil
  end.

create_table() ->
  Attributes = record_info(fields, ?TABLE_NAME),
  freyr_storage_utils:create_table(?TABLE_NAME, Attributes).
