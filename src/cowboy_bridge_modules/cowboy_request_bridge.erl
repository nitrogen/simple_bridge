%% vim: ts=4 sw=4 et
% Simple Bridge Cowboy
% Copyright (c) 2012 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (cowboy_request_bridge).
-behaviour (simple_bridge_request).
-include_lib ("simple_bridge.hrl").

-export ([
    init/1,
    request_method/1, path/1, uri/1,
    peer_ip/1, peer_port/1,
    headers/1, cookies/1,
    query_params/1, post_params/1, request_body/1,
    socket/1, recv_from_socket/3
]).

-define(GET,_RequestCache=#request_cache{request=Req}=cowboy_request_server:get(ReqKey)).
-define(PUT,cowboy_request_server:set(ReqKey,NewRequestCache)).

new_key() ->
    {cowboy_bridge,now()}.

init({Req,DocRoot}) ->
    ReqKey = new_key(),
    NewRequestCache = #request_cache{
        body=not_loaded,
        request=Req,
        docroot=DocRoot
    },
    ?PUT,
    ReqKey.

request_method(ReqKey) ->
    ?GET,
    {Method, Req} = cowboy_http_req:method(Req),
    Method.

path(ReqKey) ->
    ?GET,
    {Path, Req} = cowboy_http_req:path(Req),
    case Path of
        [] -> "/";
        _ -> b2l(filename:join(Path))
    end.

uri(ReqKey) ->
    ?GET,
    {RawPath, Req} = cowboy_http_request:raw_path(Req),
    b2l(RawPath).

peer_ip(ReqKey) ->
    ?GET,
    {{IP, _Port, Req}} = cowboy_http_req:peer(Req),
    IP.

peer_port(ReqKey) ->
    ?GET,
    {{_IP, Port, Req}} = cowboy_http_req:peer(Req),
    Port.

headers(ReqKey) ->
    ?GET,
    {Headers,Req} = cowboy_http_req:headers(Req),
    [{simple_bridge_util:atomize_header(Header),b2l(Val)} || {Header,Val} <- Headers].


cookies(ReqKey) ->
    ?GET,
    {Cookies, NewReq} = cowboy_http_req:cookies(Req),
    NewRequestCache = _RequestCache#request_cache{request=NewReq},
    ?PUT,
    [{b2l(K),b2l(V)} || {K,V} <- Cookies].

query_params(ReqKey) ->
    ?GET,
    {QsVals, NewReq} = cowboy_http_req:qs_vals(Req),
    NewRequestCache = _RequestCache#request_cache{request=NewReq},
    ?PUT,
    [{b2l(K),b2l(V)} || {K,V} <- QsVals].



post_params(ReqKey) ->
    Body = request_body(ReqKey,binary),
    BodyQs = parse_qs(Body), 
    [{b2l(K),b2l(V)} || {K,V} <- BodyQs].

request_body(ReqKey) ->
    request_body(ReqKey,string).

request_body(ReqKey,binary) ->
    ?GET,
     %% We cache the body here because we can't request the body twice in cowboy or it'll crash
    {Body,NewReq} = case _RequestCache#request_cache.body of
        not_loaded -> 
            {ok, B, R} = cowboy_http_req:body(Req),
            {B,R};
        B -> {B,Req}
    end,
    NewRequestCache = _RequestCache#request_cache {
        body=Body,
        request=NewReq
    },
    ?PUT,
    Body;
request_body(ReqKey,string) ->
    b2l(request_body(ReqKey,binary)).


socket(ReqKey) ->
    ?GET,
    {ok, _Transport, Socket} = cowboy_http_req:transport(Req),
    Socket.

%% TODO: Cowboy's stream_body doesn't support customizable Length and Timeout
recv_from_socket(_Length, _Timeout, ReqKey) ->
    ?GET,
    %cowboy_http_req:init_stream(
    case cowboy_http_req:stream_body(Req) of
        {ok, Data, NewReq} ->
            NewRequestCache = _RequestCache#request_cache{request=NewReq},
            ?PUT,
            Data;
        {done, NewReq} ->
            NewRequestCache = _RequestCache#request_cache{request=NewReq},
            ?PUT,
            <<"">>;
        {error, Reason} ->
            exit({error, Reason}) %% exit(normal) instead?
    end.



%% parse_qs, borrowed from Cowboy by Loic Hugian :)
parse_qs(<<>>) ->
    [];
parse_qs(Qs) ->
    URLDecode = fun cowboy_http:urldecode/1,
    Tokens = binary:split(Qs, <<"&">>, [global, trim]),
    [case binary:split(Token, <<"=">>) of
        [Token] -> {URLDecode(Token), true};
        [Name, Value] -> {URLDecode(Name), URLDecode(Value)}
    end || Token <- Tokens].


b2l(B) when is_binary(B) ->
    binary_to_list(B);
b2l(B) ->
    B.

l2b(L) when is_list(L) ->
    list_to_binary(L);
l2b(L) ->
    L.

b2a(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B)).



