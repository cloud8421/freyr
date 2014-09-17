-module(freyr_device_store).

-behaviour(gen_server).

-export([start_link/0, insert/1, all/0, by_id/1, create_table/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("freyr_device.hrl").

-define(TABLE_NAME, freyr_device).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(NewDevice) ->
  gen_server:cast(?MODULE, {insert, NewDevice}).

all() ->
  gen_server:call(?MODULE, all).

by_id(DeviceId) when is_list(DeviceId) ->
  gen_server:call(?MODULE, {by_id, DeviceId}).

%% callbacks
init([]) ->
  create_table(),
  {ok, EventDispatcher} = gen_event:start_link(),
  gen_event:add_handler(EventDispatcher, freyr_logger, []),
  {ok, EventDispatcher}.

handle_call(all, _From, EventDispatcher) ->
  Q = fun() ->
          freyr_queries:request({device, all})
      end,
  {atomic, Devices} = mnesia:transaction(Q),
  {reply, Devices, EventDispatcher};

handle_call({by_id, DeviceId}, _From, EventDispatcher) ->
  Q = fun() ->
          mnesia:read(?TABLE_NAME, DeviceId)
      end,
  Result = case mnesia:transaction(Q) of
             {atomic, [Device]} -> Device;
             {atomic, []} -> nil
           end,
  {reply, Result, EventDispatcher}.

handle_cast({insert, NewDevice}, EventDispatcher) ->
  freyr_device:create(NewDevice),
  gen_event:notify(EventDispatcher, {insert, device, NewDevice}),
  {noreply, EventDispatcher}.

handle_info(timeout, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private

create_table() ->
  Attributes = record_info(fields, ?TABLE_NAME),
  freyr_storage_utils:create_table(?TABLE_NAME, Attributes).
