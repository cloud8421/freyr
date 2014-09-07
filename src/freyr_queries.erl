-module(freyr_queries).

-export([all/0, by_hour/1]).

-include("freyr_reading.hrl").

all() ->
  #freyr_reading{_='_'}.

by_hour(Hour) ->
  #freyr_reading{timestamp={{'_','_','_'}, {Hour,'_','_'}}, _='_'}.
