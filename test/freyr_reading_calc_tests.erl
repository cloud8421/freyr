-module(freyr_reading_calc_tests).

-c(freyr_reading_calc).
-c(mocks).

-include_lib("eunit/include/eunit.hrl").

-include("../src/freyr_reading.hrl").

readings() ->
  [#freyr_reading{
      uuid=mocks:default_uuid(),
      device_id="abc4rq1q3rw",
      temperature=25.0,
      brightness=3000,
      moisture=400,
      timestamp=mocks:default_timestamp()},
   #freyr_reading{
      uuid=mocks:default_uuid(),
      device_id="abc4rq1q3rw",
      temperature=23.0,
      brightness=5000,
      moisture=600,
      timestamp=mocks:default_timestamp()}].

averages_test() ->
  Expected = #{average_temperature=>24.0,
               average_brightness=>4000,
               average_moisture=>500},
  ?assertEqual(freyr_reading_calc:averages(readings()), Expected).
