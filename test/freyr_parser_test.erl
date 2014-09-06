-module(freyr_parser_test).

-c(freyr_parser).

-include_lib("eunit/include/eunit.hrl").

-include("../src/freyr_reading.hrl").

parses_payload_test() ->
  Expected = #freyr_reading{temperature=23.4, brightness=4000, moisture=300},
  ?assertEqual(freyr_parser:parse("temperature:f:23.4|brightness:i:4000|moisture:i:300\r\n"), Expected),
  ?assertEqual(freyr_parser:parse("temperature:f:23.4|brightness:i:4000|moisture:i:300\n\r"), Expected),
  ?assertEqual(freyr_parser:parse("temperature:f:23.4|brightness:i:4000|moisture:i:300"), Expected).
