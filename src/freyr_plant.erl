-module(freyr_plant).

-export([health_status/2, find/1, create/1, by_device/1]).
-export([create_table/0]).

-include("freyr_plant.hrl").
-include("freyr_reading.hrl").

-define(TOLERANCE, 20).
-define(TABLE_NAME, freyr_plant).

-record(freyr_plant_with_status, {
          plant,
          health_status
         }).

find(PlantId) when is_list(PlantId) ->
  case do_find(PlantId) of
    nil -> nil;
    Plant -> add_status_to_plant(Plant)
  end.

by_device(DeviceId) when is_list(DeviceId) ->
  Q = fun() ->
          freyr_queries:request({plant, by_device, DeviceId})
      end,
  {atomic, Plants} = mnesia:transaction(Q),
  lists:map(fun(Plant) ->
                add_status_to_plant(Plant)
            end, Plants).

add_status_to_plant(Plant) ->
  DeviceId = Plant#freyr_plant.device_id,
  LastReading = freyr_reading:last_by_device(DeviceId),
  HealthStatus = health_status(Plant, LastReading),
  #freyr_plant_with_status{plant=Plant,
                           health_status=HealthStatus}.

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

create(NewPlant) ->
  Insertion = fun() ->
                  mnesia:write(NewPlant)
              end,
  mnesia:transaction(Insertion).

create_table() ->
  Attributes = record_info(fields, ?TABLE_NAME),
  freyr_storage_utils:create_table(?TABLE_NAME, Attributes).

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

do_find(PlantId) ->
  Q = fun() ->
          mnesia:read(?TABLE_NAME, PlantId)
      end,
  case mnesia:transaction(Q) of
    {atomic, [Device]} -> Device;
    {atomic, []} -> nil
  end.
