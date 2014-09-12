-module(freyr_plant_test).

-c(freyr_plant).
-c(mocks).

-include_lib("eunit/include/eunit.hrl").

-include("../src/freyr_plant.hrl").
-include("../src/freyr_reading.hrl").

health_status_test() ->
  Plant = #freyr_plant{
             uuid=mocks:default_uuid(),
             device_id="abc4rq1q3rw",
             name="Habanero",
             optimal_humidity=400,
             optimal_temperature=21.0,
             optimal_brightness=3200,
             created_at=mocks:default_timestamp(),
             updated_at=mocks:default_timestamp()
            },
  LastReading = #freyr_reading{
                   uuid=mocks:default_uuid(),
                   device_id="abc4rq1q3rw",
                   temperature=26.0,
                   brightness=3000,
                   moisture=400,
                   timestamp=mocks:default_timestamp()},
  Expected = #{humidity => optimal,
               temperature => critical_high,
               brightness => low},
  ?assertEqual(Expected, freyr_plant:health_status(Plant, LastReading)).
