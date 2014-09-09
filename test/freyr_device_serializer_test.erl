-module(freyr_device_serializer_test).

-c(freyr_device_serializer).

-include_lib("eunit/include/eunit.hrl").

-include("../src/freyr_reading.hrl").

displays_metadata_test() ->
  meck:new(freyr_reading_store),
  meck:expect(freyr_reading_store, by_device, fun("abc4rq1q3rw") -> readings() end),
  Metadata = #{average_temperature=>24.0,
               average_brightness=>4000,
               average_moisture=>500},
  Expected = #{metadata => Metadata,
               last_reading => [{<<"uuid">>,<<"83b2bbda-354a-11e4-a328-b8e8563a72e8">>},
               {<<"device_id">>, <<"abc4rq1q3rw">>},
               {<<"temperature">>,25.0},
               {<<"brightness">>,3000},
               {<<"moisture">>,400},
               {<<"timestamp">>,<<"2014:9:5-22:23:18">>}]},
  ?assertEqual(freyr_device_serializer:metadata("abc4rq1q3rw"), Expected).

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
