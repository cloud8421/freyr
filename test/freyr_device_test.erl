-module(freyr_device_test).

-c(freyr_device).
-c(mocks).

-include_lib("eunit/include/eunit.hrl").

-include("../src/freyr_reading.hrl").
-include("../src/freyr_device.hrl").
-include("../src/freyr_device_with_metadata.hrl").

find_test() ->
  meck:new(freyr_reading_store),
  meck:expect(freyr_reading_store, by_device, fun("abc4rq1q3rw") -> readings() end),
  meck:new(freyr_device_store),
  meck:expect(freyr_device_store, by_id, fun("abc4rq1q3rw") -> device() end),
  ExpectedAverages = #{average_temperature=>24.0,
                       average_brightness=>4000,
                       average_moisture=>500},
  [Reading | _Rest] = readings(),
  Expected = #freyr_device_with_metadata{device=device(),
                                   averages=ExpectedAverages,
                                   last_reading=Reading},
  ?assertEqual(freyr_device:find("abc4rq1q3rw"), Expected).

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

device() ->
  #freyr_device{
     uuid="abc1234xyz",
     name="The first",
     location="Living Room",
     created_at=mocks:default_timestamp(),
     updated_at=mocks:default_timestamp()}.
