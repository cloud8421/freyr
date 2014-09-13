-module(freyr_url_helper).

-export([url_for/1, url_for/2]).

url_for(Resource) when is_atom(Resource) ->
  host() ++ atom_to_list(Resource).
url_for(Resource, Id) when is_atom(Resource) ->
  url_for(Resource) ++ "/" ++ Id.

host() ->
  "http://" ++ freyr_settings:fqdn() ++ ":" ++ freyr_settings:http_port() ++ "/".
