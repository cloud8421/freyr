-module(freyr_settings).

-export([http_port/0, http_host/0, fqdn/0]).

http_port() ->
  list_to_integer(os:getenv("HTTP_PORT")).

http_host() -> {127,0,0,1}.

fqdn() -> "localhost".
