-module(freyr_url_helper).

-export([url_for/1, url_for/2]).

url_for(Resource) when is_atom(Resource) ->
  host() ++ "/" ++ atom_to_list(Resource);
url_for(NestedPathsList) when is_list(NestedPathsList) ->
  build_nested_url(NestedPathsList, host()).
url_for(Resource, Id) when is_atom(Resource) ->
  url_for(Resource) ++ "/" ++ Id;
url_for([], Acc) -> Acc.

build_nested_url([], Acc) -> Acc;

build_nested_url([{Resource, Id}|T], Acc) ->
  Path = "/" ++ atom_to_list(Resource) ++ "/" ++ Id,
  build_nested_url(T, Acc ++ Path);
build_nested_url([H|T], Acc) ->
  build_nested_url(T, Acc ++ "/" ++ atom_to_list(H)).

host() ->
  "http://" ++ freyr_settings:fqdn() ++ ":" ++ port().

port() ->
  integer_to_list(freyr_settings:http_port()).
