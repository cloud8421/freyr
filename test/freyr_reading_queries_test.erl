-module(freyr_reading_queries_test).

-c(freyr_reading_queries).

-include_lib("eunit/include/eunit.hrl").

-include("../src/freyr_reading.hrl").

all_query_test() ->
  Sample = #freyr_reading{
                uuid="caa6b6ac-369b-11e4-bf59-b8e8563a72e8",
                device_id="abc4rq1q3rw",
                temperature=23.4,
                brightness=4000,
                moisture=300,
                timestamp={{2014,9,7},{14,32,27}}},
  MatchSpec = build_match_spec(freyr_reading_queries:all()),
  ?assertEqual(ets:test_ms(Sample, MatchSpec), {ok, Sample}).

by_hour_query_test() ->
  Match = #freyr_reading{
                uuid="caa6b6ac-369b-11e4-bf59-b8e8563a72e8",
                device_id="abc4rq1q3rw",
                temperature=23.4,
                brightness=4000,
                moisture=300,
                timestamp={{2014,9,7},{14,32,27}}},
  NonMatch = Match#freyr_reading{timestamp={{2014,9,7},{15,32,27}}},
  MatchSpec = build_match_spec(freyr_reading_queries:by_hour(14)),
  ?assertEqual(ets:test_ms(Match, MatchSpec), {ok, Match}),
  ?assertEqual(ets:test_ms(NonMatch, MatchSpec), {ok, false}).

by_device_test() ->
  Match = #freyr_reading{
                uuid="caa6b6ac-369b-11e4-bf59-b8e8563a72e8",
                device_id="abc4rq1q3rw",
                temperature=23.4,
                brightness=4000,
                moisture=300,
                timestamp={{2014,9,7},{14,32,27}}},
  NonMatch = Match#freyr_reading{device_id="123456abcd4"},
  MatchSpec = build_match_spec(freyr_reading_queries:by_device("abc4rq1q3rw")),
  ?assertEqual(ets:test_ms(Match, MatchSpec), {ok, Match}),
  ?assertEqual(ets:test_ms(NonMatch, MatchSpec), {ok, false}).

build_match_spec(Match) ->
  [{Match, [], ['$_']}].
