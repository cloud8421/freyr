-module(freyr_storage).

-behaviour(gen_server).

-export([start_link/0, insert/1, all/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("freyr_reading.hrl").

-define(TABLE_NAME, freyr_reading).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(NewReading) ->
  gen_server:cast(?MODULE, {insert, NewReading}).

all() ->
  gen_server:call(?MODULE, all).

%% callbacks
init([]) ->
  create_table(),
  {ok, []}.

handle_call(all, _From, []) ->
  Selection = fun() ->
                  All = #freyr_reading{_ = '_'},
                  mnesia:match_object(All)
              end,
  {atomic, Readings} = mnesia:transaction(Selection),
  {reply, Readings, []}.

handle_cast({insert, NewReading}, []) ->
  Insertion = fun() ->
                  mnesia:write(NewReading)
              end,
  mnesia:transaction(Insertion),
  {noreply, []}.

handle_info(timeout, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private

create_table() ->
  case table_exists(?TABLE_NAME) of
    true -> ok;
    false -> mnesia:create_table(?TABLE_NAME,
                                [{type, ordered_set},
                                 {attributes, record_info(fields,
                                                          freyr_reading)}])
  end.

table_exists(TableName) ->
  Tables = mnesia:system_info(tables),
  lists:member(TableName, Tables).
