%% See LICENSE for licensing information.
-module(cowboy_debug).

-export([onrequest_hook/1]).
-export([onresponse_hook/4]).

onrequest_hook(Req) ->
    Method = to_string(extract(cowboy_req:method(Req))),
    Path   = to_string(extract(cowboy_req:path(Req))),
    Params = params_to_string(extract(cowboy_req:qs_vals(Req))),
    Host   = to_string(extract(cowboy_req:host(Req))),
    Port   = port_to_string(extract(cowboy_req:port(Req))),
    lager:info("~n~nStarted " ++ Method ++ " " ++ Path ++ Params ++ " for " ++ Host ++ Port ++ "~n"
                "  qs_vals  : " ++ to_native_string(extract(cowboy_req:qs_vals(Req))) ++ "~n"
                "  raw_qs   : " ++ to_native_string(extract(cowboy_req:qs(Req))) ++ "~n"
                "  bindings : " ++ to_native_string(extract(cowboy_req:bindings(Req))) ++ "~n"
                "  cookies  : " ++ to_native_string(extract(cowboy_req:cookies(Req))) ++ "~n"
                "  headers  : " ++ to_native_string(extract(cowboy_req:headers(Req))) ++ "~n"),
    Req.

onresponse_hook(Code, Headers, Response, Req) ->
    Method = to_string(extract(cowboy_req:method(Req))),
    Path   = to_string(extract(cowboy_req:path(Req))),
    Params = params_to_string(extract(cowboy_req:qs_vals(Req))),
    Host   = to_string(extract(cowboy_req:host(Req))),
    Port   = port_to_string(extract(cowboy_req:port(Req))),
    lager:info(
      "~n~nCompleted " ++ to_string(Code) ++ " " ++ Method ++ " " ++ Path ++ Params ++ " for " ++ Host ++ Port ++ "~n"
      "  cookies  : " ++ to_native_string(extract(cowboy_req:cookies(Req))) ++ "~n"
      "  headers  : " ++ to_native_string(Headers) ++ "~n"
      "  response : " ++ to_native_string(Response)),
    Req.

extract({Value, _Req}) ->
    Value.

params_to_string(Params) ->
    case to_string(Params) of
        "" ->
            "";
        OtherParams -> "?" ++ OtherParams
    end.

port_to_string(Port) ->    case to_string(Port) of
                               "80" -> "";
                               OtherPort -> ":" ++ OtherPort
                           end.

%% print value as standard format
to_native_string(Value) ->
    io_lib:format("~p", [Value]).

%% convert everything to string
to_string(undefined) ->
    "";
to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_string(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
to_string([])
-> "";
to_string(List) when is_list(List) ->
    to_string(List, "").

%% convert lists to string
to_string(Binary, Separator) when is_binary(Binary) ->
    to_string(binary_to_list(Binary), Separator);
to_string(List, Separator) when is_list(List) ->
    string:join(list_to_string(List, []), Separator).

list_to_string([], Result) ->
    lists:reverse(Result);
list_to_string([Head| Rest], Result) ->
    list_to_string(Rest, [to_string(Head)|Result]).
