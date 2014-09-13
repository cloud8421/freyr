-module(freyr_url_helper_test).

-c(freyr_url_helper).

-include_lib("eunit/include/eunit.hrl").

generates_index_url_test() ->
  Expected = "http://localhost:9000/readings",
  ?assertEqual(Expected, freyr_url_helper:url_for(readings)).

generates_resource_url_test() ->
  Expected = "http://localhost:9000/readings/abc4rq1q3rw",
  ?assertEqual(Expected, freyr_url_helper:url_for(readings, "abc4rq1q3rw")).
