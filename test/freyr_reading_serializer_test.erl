-module(freyr_reading_serializer_test).

-c(freyr_reading_serializer).

-include_lib("eunit/include/eunit.hrl").

-include("../src/freyr_reading.hrl").

serializes_to_list_test() ->
  Readings = [#freyr_reading{uuid="ee6ab164-366f-11e4-a7d9-b8e8563a72e8",
                             temperature=23.7,
                             brightness=390,
                             moisture=3700,
                             timestamp={{2014,9,7},{9,18,29}}},
              #freyr_reading{uuid="ee984930-366f-11e4-b874-b8e8563a72e8",
                             temperature=20.8,
                             brightness=750,
                             moisture=2800,
                             timestamp={{2014,9,7},{9,18,30}}}],
  Expected = [[{<<"uuid">>,<<"ee6ab164-366f-11e4-a7d9-b8e8563a72e8">>},
     {<<"temperature">>,23.7},
     {<<"brightness">>,390},
     {<<"moisture">>,3700},
     {<<"timestamp">>,<<"2014:9:7-9:18:29">>}],
    [{<<"uuid">>,<<"ee984930-366f-11e4-b874-b8e8563a72e8">>},
     {<<"temperature">>,20.8},
     {<<"brightness">>,750},
     {<<"moisture">>,2800},
     {<<"timestamp">>,<<"2014:9:7-9:18:30">>}]],
  Serialized = freyr_reading_serializer:serialize(Readings),
  ?assertEqual(Serialized, Expected).
