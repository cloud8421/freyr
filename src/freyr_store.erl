-module(freyr_store).

-behaviour(gen_server).

-export([start_link/0, insert/2, insert_from_binary/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("freyr_device.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(Type, NewItem) ->
  gen_server:cast(?MODULE, {insert, Type, NewItem}).

insert_from_binary(Type, BinaryData) ->
  gen_server:cast(?MODULE, {insert_from_binary, Type, BinaryData}).

%% callbacks
init([]) ->
  {ok, EventDispatcher} = gen_event:start_link(),
  gen_event:add_handler(EventDispatcher, freyr_logger, []),
  {ok, EventDispatcher}.

handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast({insert, Type, NewItem}, EventDispatcher) ->
  do_create(Type, NewItem),
  gen_event:notify(EventDispatcher, {insert, Type, NewItem}),
  {noreply, EventDispatcher};

handle_cast({insert_from_binary, Type, BinaryData}, EventDispatcher) ->
  NewItem = parse_binary(Type, BinaryData),
  do_create(Type, NewItem),
  gen_event:notify(EventDispatcher, {insert, Type, NewItem}),
  {noreply, EventDispatcher}.

handle_info(timeout, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private

do_create(device, NewItem) ->
  freyr_device:create(NewItem);

do_create(reading, NewItem) ->
  freyr_reading:create(NewItem);

do_create(plant, NewItem) ->
  freyr_plant:create(NewItem).

parse_binary(reading, BinaryData) ->
  Parsable = binary_to_list(BinaryData),
  freyr_reading_builder:parse(Parsable).
