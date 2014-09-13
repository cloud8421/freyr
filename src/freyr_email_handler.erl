-module(freyr_email_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).

-include("freyr_reading.hrl").
-include("freyr_device.hrl").

init([]) ->
  {ok, []}.

handle_event({insert, reading, _Reading}, State) ->
  lager:info("Should decide about email"),
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

