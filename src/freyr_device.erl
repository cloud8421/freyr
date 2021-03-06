-module(freyr_device).

-export([find/1, create/1, all/0]).
-export([create_table/0]).

-include("freyr_reading.hrl").
-include("freyr_device.hrl").
-include("freyr_device_with_metadata.hrl").

-define(TABLE_NAME, freyr_device).

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

all() ->
  Q = fun() ->
          freyr_queries:request({device, all})
      end,
  {atomic, Devices} = mnesia:transaction(Q),
  Devices.

create(NewDevice) ->
  Insertion = fun() ->
                  mnesia:write(NewDevice)
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
