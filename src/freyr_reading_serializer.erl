-module(freyr_reading_serializer).

-export([serialize/1]).

-include("freyr_reading.hrl").

serialize(Readings) ->
  lists:map(fun(Reading) ->
                do_serialize(Reading)
            end, Readings).

do_serialize({freyr_reading, Uuid, DeviceId, Temperature, Brigthness, Moisture, Timestamp}) ->
  [{<<"uuid">>, list_to_binary(Uuid)},
   {<<"device_id">>, list_to_binary(DeviceId)},
   {<<"temperature">>, Temperature},
   {<<"brightness">>, Brigthness},
   {<<"moisture">>, Moisture},
   {<<"timestamp">>, list_to_binary(timestamp_to_string(Timestamp))}].

timestamp_to_string({{Year, Month, Day}, {Hour, Minutes, Seconds}}) ->
  Date = lists:map(fun(X) -> integer_to_list(X) end, [Year, Month, Day]),
  Time = lists:map(fun(X) -> integer_to_list(X) end, [Hour, Minutes, Seconds]),
  string:join(Date, ":") ++ "-" ++ string:join(Time, ":").
