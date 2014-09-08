-module(freyr_queries).

-export([all/0, by_hour/1, by_device/1]).

-include("freyr_reading.hrl").

all() ->
  #freyr_reading{_='_'}.

by_hour(Hour) ->
  #freyr_reading{timestamp={{'_','_','_'}, {Hour,'_','_'}}, _='_'}.

by_device(Device) ->
  #freyr_reading{device_id=Device, _='_'}.
