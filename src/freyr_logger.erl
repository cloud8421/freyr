-module(freyr_logger).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).

-include("freyr_reading.hrl").

init([]) ->
  {ok, []}.

handle_event({insert, reading, Reading}, State) ->
  lager:info("~s\n", [reading_to_logline(Reading)]),
  {ok, State};

handle_event(_, State) ->
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

%% utility
reading_to_logline(Reading) ->
  DeviceId = Reading#freyr_reading.device_id,
  Timestamp = Reading#freyr_reading.timestamp,
  Temp = Reading#freyr_reading.temperature,
  Brightness = Reading#freyr_reading.brightness,
  Moisture = Reading#freyr_reading.moisture,

  TempString = float_to_list(Temp, [{decimals, 1}]),
  BrightnessString = integer_to_list(Brightness),
  MoistureString = integer_to_list(Moisture),
  TimestampString = timestamp_to_string(Timestamp),
  "[timestamp: " ++ TimestampString ++ ", "
  "device: " ++ DeviceId ++ ", "
  "temp: " ++ TempString ++ ", "
  "moisture: " ++ MoistureString ++ ", "
  "brightness: " ++ BrightnessString ++ "]".

timestamp_to_string({{Year, Month, Day}, {Hour, Minutes, Seconds}}) ->
  Date = lists:map(fun(X) -> integer_to_list(X) end, [Year, Month, Day]),
  Time = lists:map(fun(X) -> integer_to_list(X) end, [Hour, Minutes, Seconds]),
  string:join(Date, ":") ++ "-" ++ string:join(Time, ":").
