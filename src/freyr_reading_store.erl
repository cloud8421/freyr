-module(freyr_reading_store).

-behaviour(gen_server).

-export([start_link/0, insert/1, insert_from_binary/1,
         all/0, create_table/0, by_hour/1, by_device/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("freyr_reading.hrl").

-define(TABLE_NAME, freyr_reading).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(NewReading) ->
  gen_server:cast(?MODULE, {insert, NewReading}).

insert_from_binary(BinaryData) ->
  gen_server:cast(?MODULE, {insert_from_binary, BinaryData}).

all() ->
  gen_server:call(?MODULE, all).

by_hour(Hour) when is_integer(Hour) ->
  gen_server:call(?MODULE, {by_hour, Hour}).

by_device(Device) when is_list(Device) ->
  gen_server:call(?MODULE, {by_device, Device}).

%% callbacks
init([]) ->
  create_table(),
  {ok, EventDispatcher} = gen_event:start_link(),
  gen_event:add_handler(EventDispatcher, freyr_logger, []),
  {ok, EventDispatcher}.

handle_call(all, _From, EventDispatcher) ->
  Q = fun() ->
          mnesia:match_object(freyr_reading_queries:all())
      end,
  {atomic, Readings} = mnesia:transaction(Q),
  {reply, Readings, EventDispatcher};

handle_call({by_hour, Hour}, _From, EventDispatcher) ->
  Q = fun() ->
          mnesia:match_object(freyr_reading_queries:by_hour(Hour))
      end,
  {atomic, Readings} = mnesia:transaction(Q),
  {reply, Readings, EventDispatcher};

handle_call({by_device, Device}, _From, EventDispatcher) ->
  Q = fun() ->
          mnesia:match_object(freyr_reading_queries:by_device(Device))
      end,
  {atomic, Readings} = mnesia:transaction(Q),
  {reply, Readings, EventDispatcher}.

handle_cast({insert, NewReading}, EventDispatcher) ->
  do_insert(NewReading, EventDispatcher),
  {noreply, EventDispatcher};

handle_cast({insert_from_binary, BinaryData}, EventDispatcher) ->
  Parsable = binary_to_list(BinaryData),
  NewReading = freyr_reading_builder:parse(Parsable),
  do_insert(NewReading, EventDispatcher),
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

do_insert(NewReading, EventDispatcher) ->
  Insertion = fun() ->
                  mnesia:write(NewReading)
              end,
  mnesia:transaction(Insertion),
  gen_event:notify(EventDispatcher, {insert, NewReading}).
