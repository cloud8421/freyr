-module(freyr_listener).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PORT, 5678).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, LSock} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, true}]),
  {ok, LSock, 0}.

%% callbacks
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, LSock) ->
  {ok, _LSock} = gen_tcp:accept(LSock),
  {noreply, LSock};
handle_info({tcp, _Socket, Data}, LSock) ->
  freyr_store:insert_from_binary(reading, Data),
  {noreply, LSock};
handle_info({tcp_closed, _Socket}, LSock) ->
  {ok, _LSock} = gen_tcp:accept(LSock),
  {noreply, LSock}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
