-module(freyr_builder).

-export([parse/1]).

-include("freyr_reading.hrl").

parse(Data) ->
  Normalized = normalize(Data),
  Pairs = string:tokens(Normalized, "|"),
  toPairs(Pairs, []).

toPairs([], Acc) ->
  {temperature, Temperature}= lists:keyfind(temperature, 1, Acc),
  {brightness, Brightness}= lists:keyfind(brightness, 1, Acc),
  {moisture, Moisture}= lists:keyfind(moisture, 1, Acc),
  #freyr_reading{uuid=uuid(),
                 temperature=Temperature,
                 brightness=Brightness,
                 moisture=Moisture,
                 timestamp=calendar:universal_time()};
toPairs([H|T], Acc) ->
  toPairs(T, [toPair(H) | Acc]).

toPair(Pair) ->
  Tokens = string:tokens(Pair, ":"),
  tokenize(Tokens).

tokenize(["temperature", "f", Value]) ->
  {Temp, []} = string:to_float(Value),
  {temperature, Temp};
tokenize(["brightness", "i", Value]) ->
  {Brightness, []} = string:to_integer(Value),
  {brightness, Brightness};
tokenize(["moisture", "i", Value]) ->
  {Moisture, []} = string:to_integer(Value),
  {moisture, Moisture}.

normalize(Data) ->
  case lists:reverse(Data) of
    "\n\r" ++ ReversedData -> lists:reverse(ReversedData);
    "\r\n" ++ ReversedData -> lists:reverse(ReversedData);
    _Else -> Data
  end.

uuid() ->
  uuid:to_string(uuid:uuid1()).
