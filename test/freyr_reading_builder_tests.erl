-module(freyr_reading_builder_tests).

-c(freyr_reading_builder).
-c(mocks).

-include_lib("eunit/include/eunit.hrl").

-include("../src/freyr_reading.hrl").

parses_payload_test() ->
  mocks:mock_uuid(),
  mocks:mock_calendar(),
  Expected = #freyr_reading{
                uuid=mocks:default_uuid(),
                device_id="abc4rq1q3rw",
                temperature=23.4,
                brightness=4000,
                moisture=300,
                timestamp=mocks:default_timestamp()},
  ?assertEqual(freyr_reading_builder:parse("device:abc4rq1q3rw|temperature:f:23.4|brightness:i:4000|moisture:i:300\r\n"), Expected),
  ?assertEqual(freyr_reading_builder:parse("device:abc4rq1q3rw|temperature:f:23.4|brightness:i:4000|moisture:i:300\n\r"), Expected),
  ?assertEqual(freyr_reading_builder:parse("device:abc4rq1q3rw|temperature:f:23.4|brightness:i:4000|moisture:i:300"), Expected).
