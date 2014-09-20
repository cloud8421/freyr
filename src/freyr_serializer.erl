-module(freyr_serializer).

-export([serialize/1]).

-include("freyr_reading.hrl").
-include("freyr_device.hrl").

serialize(Items) when is_list(Items) ->
  lists:map(fun(Reading) ->
                serialize(Reading)
            end, Items);

serialize({freyr_reading, Uuid, DeviceId, Temperature, Brigthness, Moisture, Timestamp}) ->
  [{<<"uuid">>, list_to_binary(Uuid)},
   {<<"device_id">>, list_to_binary(DeviceId)},
   {<<"temperature">>, Temperature},
   {<<"brightness">>, Brigthness},
   {<<"moisture">>, Moisture},
   {<<"timestamp">>, list_to_binary(timestamp_to_string(Timestamp))}];

serialize({freyr_device, Uuid, Name, Location, Lat, Lng, CreatedAt, UpdatedAt}) ->
  [{<<"uuid">>, list_to_binary(Uuid)},
   {<<"name">>, list_to_binary(Name)},
   {<<"location">>, list_to_binary(Location)},
   {<<"lat">>, Lat},
   {<<"lng">>, Lng},
   {<<"created_at">>, list_to_binary(timestamp_to_string(CreatedAt))},
   {<<"updated_at">>, list_to_binary(timestamp_to_string(UpdatedAt))}];

serialize({freyr_device_with_metadata, Device, Averages, Reading}) ->
  #{device=>serialize(Device),
    averages=>Averages,
    last_reading=>serialize(Reading),
    meta => device_meta(Device#freyr_device.uuid)};

serialize({freyr_plant, Uuid, DeviceId, Name, OptimalHumidity, OptimalTemperature,
           OptimalBrightness, CreatedAt, UpdatedAt}) ->
  [{<<"uuid">>, list_to_binary(Uuid)},
   {<<"device_id">>, list_to_binary(DeviceId)},
   {<<"name">>, list_to_binary(Name)},
   {<<"optimal_humidity">>, OptimalHumidity},
   {<<"optimal_temperature">>, OptimalTemperature},
   {<<"optimal_brightness">>, OptimalBrightness},
   {<<"created_at">>, list_to_binary(timestamp_to_string(CreatedAt))},
   {<<"updated_at">>, list_to_binary(timestamp_to_string(UpdatedAt))}];

serialize({freyr_plant_with_status, Plant, HealthStatus}) ->
  #{plant=>serialize(Plant),
    health_status=>HealthStatus}.

timestamp_to_string({{Year, Month, Day}, {Hour, Minutes, Seconds}}) ->
  Date = lists:map(fun(X) -> integer_to_list(X) end, [Year, Month, Day]),
  Time = lists:map(fun(X) -> integer_to_list(X) end, [Hour, Minutes, Seconds]),
  string:join(Date, ":") ++ "-" ++ string:join(Time, ":").

device_meta(DeviceId) ->
  ReadingsUrl = list_to_binary(freyr_url_helper:url_for([{devices, DeviceId}, readings])),
  #{links => #{readings => ReadingsUrl}}.
