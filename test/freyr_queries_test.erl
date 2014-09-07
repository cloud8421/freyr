-module(freyr_queries_test).

-c(freyr_queries).

-include_lib("eunit/include/eunit.hrl").

-include("../src/freyr_reading.hrl").

all_query_test() ->
  Sample = #freyr_reading{
                uuid="caa6b6ac-369b-11e4-bf59-b8e8563a72e8",
                temperature=23.4,
                brightness=4000,
                moisture=300,
                timestamp={{2014,9,7},{14,32,27}}},
  MatchSpec = build_match_spec(freyr_queries:all()),
  ?assertEqual(ets:test_ms(Sample, MatchSpec), {ok, Sample}).

by_hour_query_test() ->
  Match = #freyr_reading{
                uuid="caa6b6ac-369b-11e4-bf59-b8e8563a72e8",
                temperature=23.4,
                brightness=4000,
                moisture=300,
                timestamp={{2014,9,7},{14,32,27}}},
  NonMatch = Match#freyr_reading{timestamp={{2014,9,7},{15,32,27}}},
  MatchSpec = build_match_spec(freyr_queries:by_hour(14)),
  ?assertEqual(ets:test_ms(Match, MatchSpec), {ok, Match}),
  ?assertEqual(ets:test_ms(NonMatch, MatchSpec), {ok, false}).

build_match_spec(Match) ->
  [{Match, [], ['$_']}].
