-module(freyr_queries).

-export([request/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("freyr_device.hrl").
-include("freyr_reading.hrl").
-include("freyr_plant.hrl").

%% device
request({device, all}) ->
  mnesia:match_object(#freyr_device{_='_'});
%% reading
request({reading, all}) ->
  Query = qlc:q([Reading || Reading <- mnesia:table(freyr_reading)]),
  qlc:eval(qlc:sort(Query, {order, fun reading_timestamp_descending/2}));
request({reading, by_hour, Hour}) when is_integer(Hour) ->
  Query = qlc:q([Reading || Reading <- mnesia:table(freyr_reading),
                            hours_match(Hour, Reading#freyr_reading.timestamp)]),
  qlc:eval(qlc:sort(Query, {order, fun reading_timestamp_descending/2}));
request({reading, by_device, DeviceId}) when is_list(DeviceId) ->
  qlc:eval(by_device_with_sort(DeviceId));
request({reading, last_by_device, DeviceId}) when is_list(DeviceId) ->
  Cursor = qlc:cursor(by_device_with_sort(DeviceId)),
  [Reading] = qlc:next_answers(Cursor, 1),
  ok = qlc:delete_cursor(Cursor),
  Reading;
%% plant
request({plant, by_device, DeviceId}) when is_list(DeviceId) ->
  Query = qlc:q([Plant || Plant <- mnesia:table(freyr_plant),
                     Plant#freyr_plant.device_id == DeviceId]),
  qlc:eval(qlc:sort(Query, {order, fun plant_name_ascending/2})).

%% Private

by_device_with_sort(DeviceId) ->
  Query = qlc:q([Reading || Reading <- mnesia:table(freyr_reading),
                     Reading#freyr_reading.device_id == DeviceId]),
  qlc:sort(Query, {order, fun reading_timestamp_descending/2}).

reading_timestamp_descending(A, B) ->
  A#freyr_reading.timestamp > B#freyr_reading.timestamp.

plant_name_ascending(A, B) ->
  A#freyr_plant.name < B#freyr_plant.name.

hours_match(H, {_, {H, _, _}}) -> true;
hours_match(_, _) -> false.
