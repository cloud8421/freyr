-module(freyr_serializer_test).

-c(freyr_serializer).
-c(mocks).

-include_lib("eunit/include/eunit.hrl").

-include("../src/freyr_reading.hrl").
-include("../src/freyr_device.hrl").
-include("../src/freyr_device_with_metadata.hrl").

serializes_readings_to_list_test() ->
  Readings = [#freyr_reading{uuid="ee6ab164-366f-11e4-a7d9-b8e8563a72e8",
                             device_id="abc4rq1q3rw",
                             temperature=23.7,
                             brightness=390,
                             moisture=3700,
                             timestamp={{2014,9,7},{9,18,29}}},
              #freyr_reading{uuid="ee984930-366f-11e4-b874-b8e8563a72e8",
                             device_id="abc4rq1q3rw",
                             temperature=20.8,
                             brightness=750,
                             moisture=2800,
                             timestamp={{2014,9,7},{9,18,30}}}],
  Expected = [[{<<"uuid">>,<<"ee6ab164-366f-11e4-a7d9-b8e8563a72e8">>},
               {<<"device_id">>, <<"abc4rq1q3rw">>},
               {<<"temperature">>,23.7},
               {<<"brightness">>,390},
               {<<"moisture">>,3700},
               {<<"timestamp">>,<<"2014:9:7-9:18:29">>}],
              [{<<"uuid">>,<<"ee984930-366f-11e4-b874-b8e8563a72e8">>},
               {<<"device_id">>, <<"abc4rq1q3rw">>},
               {<<"temperature">>,20.8},
               {<<"brightness">>,750},
               {<<"moisture">>,2800},
               {<<"timestamp">>,<<"2014:9:7-9:18:30">>}]],
  Serialized = freyr_serializer:serialize(Readings),
  ?assertEqual(Serialized, Expected).

serializes_device_test() ->
  Device = #freyr_device_with_metadata{device={freyr_device,"abc4rq1q3rw","The first","Living Room",
                                               {{2014,9,10},{7,50,47}},
                                               {{2014,9,10},{7,50,47}}},
                                       averages=#{average_brightness => 692,
                                                  average_moisture => 3159,
                                                  average_temperature => 21.954999999999995},
                                       last_reading=#freyr_reading{uuid="0c45432e-3734-11e4-9dff-b8e8563a72e8",
                                                                   device_id="abc4rq1q3rw",
                                                                   temperature=24.2,
                                                                   brightness=750,
                                                                   moisture=4000,
                                                                   timestamp={{2014,9,8},{8,42,21}}}},
  Expected = #{device => [{<<"uuid">>, <<"abc4rq1q3rw">>},
                          {<<"name">>, <<"The first">>},
                          {<<"location">>, <<"Living Room">>},
                          {<<"created_at">>, <<"2014:9:10-7:50:47">>},
                          {<<"updated_at">>, <<"2014:9:10-7:50:47">>}],
               averages => #{average_brightness => 692,
                             average_moisture => 3159,
                             average_temperature => 21.954999999999995},
               last_reading => [{<<"uuid">>,<<"0c45432e-3734-11e4-9dff-b8e8563a72e8">>},
                                {<<"device_id">>, <<"abc4rq1q3rw">>},
                                {<<"temperature">>,24.2},
                                {<<"brightness">>,750},
                                {<<"moisture">>,4000},
                                {<<"timestamp">>,<<"2014:9:8-8:42:21">>}]},
  Serialized = freyr_serializer:serialize(Device),
  ?assertEqual(Serialized, Expected).
