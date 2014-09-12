-module(freyr_reading_queries).

-export([all/0, by_hour/1, by_device/1, last_by_device/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("freyr_reading.hrl").

all() ->
  Query = qlc:q([Reading || Reading <- mnesia:table(freyr_reading)]),
  qlc:eval(qlc:sort(Query, {order, fun timestamp_descending/2})).

by_hour(Hour) ->
  Query = qlc:q([Reading || Reading <- mnesia:table(freyr_reading),
                            hours_match(Hour, Reading#freyr_reading.timestamp)]),
  qlc:eval(qlc:sort(Query, {order, fun timestamp_descending/2})).

by_device(DeviceId) ->
  qlc:eval(by_device_with_sort(DeviceId)).

last_by_device(DeviceId) ->
  Cursor = qlc:cursor(by_device_with_sort(DeviceId)),
  [Reading] = qlc:next_answers(Cursor, 1),
  ok = qlc:delete_cursor(Cursor),
  Reading.

by_device_with_sort(DeviceId) ->
  Query = qlc:q([Reading || Reading <- mnesia:table(freyr_reading),
                     Reading#freyr_reading.device_id == DeviceId]),
  qlc:sort(Query, {order, fun timestamp_descending/2}).

timestamp_descending(A, B) ->
  A#freyr_reading.timestamp > B#freyr_reading.timestamp.

hours_match(H, {_, {H, _, _}}) -> true;
hours_match(_, _) -> false.
