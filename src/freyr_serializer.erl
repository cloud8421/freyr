-module(freyr_serializer).

-export([serialize/1]).

-include("freyr_reading.hrl").
-include("freyr_device.hrl").

serialize(Readings) when is_list(Readings) ->
  lists:map(fun(Reading) ->
                serialize(Reading)
            end, Readings);
serialize({freyr_reading, Uuid, DeviceId, Temperature, Brigthness, Moisture, Timestamp}) ->
  [{<<"uuid">>, list_to_binary(Uuid)},
   {<<"device_id">>, list_to_binary(DeviceId)},
   {<<"temperature">>, Temperature},
   {<<"brightness">>, Brigthness},
   {<<"moisture">>, Moisture},
   {<<"timestamp">>, list_to_binary(timestamp_to_string(Timestamp))}];
serialize({freyr_device, Uuid, Name, Location, CreatedAt, UpdatedAt}) ->
  [{<<"uuid">>, list_to_binary(Uuid)},
   {<<"name">>, list_to_binary(Name)},
   {<<"location">>, list_to_binary(Location)},
   {<<"created_at">>, list_to_binary(timestamp_to_string(CreatedAt))},
   {<<"updated_at">>, list_to_binary(timestamp_to_string(UpdatedAt))}];

serialize({freyr_device_with_metadata, Device, Averages, Reading}) ->
  #{device=>serialize(Device),
    averages=>Averages,
    last_reading=>serialize(Reading)}.

timestamp_to_string({{Year, Month, Day}, {Hour, Minutes, Seconds}}) ->
  Date = lists:map(fun(X) -> integer_to_list(X) end, [Year, Month, Day]),
  Time = lists:map(fun(X) -> integer_to_list(X) end, [Hour, Minutes, Seconds]),
  string:join(Date, ":") ++ "-" ++ string:join(Time, ":").
