-module(freyr_storage).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("freyr_reading.hrl").

-ifdef(TEST).
  -define(TABLE_NAME, freyr_readings_test).
-else.
  -define(TABLE_NAME, freyr_readings).
-endif.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% callbacks
init([]) ->
  create_table(),
  {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

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
