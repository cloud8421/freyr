-module(freyr_builder_test).

-c(freyr_builder).
-c(mocks).

-include_lib("eunit/include/eunit.hrl").

-include("../src/freyr_reading.hrl").

parses_payload_test() ->
  mocks:mock_uuid(),
  mocks:mock_calendar(),
  Expected = #freyr_reading{
                uuid=mocks:default_uuid(),
                temperature=23.4,
                brightness=4000,
                moisture=300,
                timestamp=mocks:default_timestamp()},
  ?assertEqual(freyr_builder:parse("temperature:f:23.4|brightness:i:4000|moisture:i:300\r\n"), Expected),
  ?assertEqual(freyr_builder:parse("temperature:f:23.4|brightness:i:4000|moisture:i:300\n\r"), Expected),
  ?assertEqual(freyr_builder:parse("temperature:f:23.4|brightness:i:4000|moisture:i:300"), Expected).
