%% vim: ts=4 sw=4 et
% Simple Bridge Cowboy
% Copyright (c) 2012 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (cowboy_request_bridge).
-behaviour (simple_bridge_request).
-include_lib ("simple_bridge.hrl").

-export ([
      init/1,
      protocol/1,
      request_method/1,
      path/1,
      uri/1,
      peer_ip/1,
      peer_port/1,
      headers/1,
      cookies/1,
      query_params/1,
      post_params/1,
      request_body/1,
      recv_from_socket/3,
      protocol_version/1
     ]).

init({Req, DocRoot}) ->
    ReqKey = new_key(),
    put_key(ReqKey, #request_cache{body = not_loaded, docroot=DocRoot, request = Req}),
    ReqKey.

protocol(ReqKey) ->
    {_RequestCache, Req} = get_key(ReqKey),
    Transport = cowboy_req:get(transport, Req),
    case Transport:name() of
        tcp -> http;
        ssl -> https
    end.

request_method(ReqKey) ->
    {_RequestCache, Req} = get_key(ReqKey),
    {Method, Req} = cowboy_req:method(Req),
    list_to_atom(?B2L(Method)).

path(ReqKey) ->
    {_RequestCache, Req} = get_key(ReqKey),
    {Path, Req} = cowboy_req:path(Req),
    ?B2L(Path).

uri(ReqKey) ->
    {_RequestCache, Req} = get_key(ReqKey),
    {URL, Req} = cowboy_req:url(Req),
    ?B2L(URL).

peer_ip(ReqKey) ->
    {RequestCache, Req} = get_key(ReqKey),
    {{IP, _Port}, NewReq} = cowboy_req:peer(Req),
    put_key(ReqKey, RequestCache#request_cache{request = NewReq}),
    IP.

peer_port(ReqKey) ->
    {RequestCache, Req} = get_key(ReqKey),
    {{_IP, Port}, NewReq} = cowboy_req:peer(Req),
    put_key(ReqKey, RequestCache#request_cache{request = NewReq}),
    Port.

headers(ReqKey) ->
    {_RequestCache, Req} = get_key(ReqKey),
    {Headers, Req} = cowboy_req:headers(Req),
    [{simple_bridge_util:atomize_header(Header), ?B2L(Val)} || {Header, Val} <- Headers].

cookies(ReqKey) ->
    {RequestCache, Req} = get_key(ReqKey),
    {Cookies, NewReq} = cowboy_req:cookies(Req),
    put_key(ReqKey, RequestCache#request_cache{request = NewReq}),
    [{?B2L(K), ?B2L(V)} || {K, V} <- Cookies].

query_params(ReqKey) ->
    {RequestCache, Req} = get_key(ReqKey),
    {QsVals, NewReq} = cowboy_req:qs_vals(Req),
    put_key(ReqKey, RequestCache#request_cache{request = NewReq}),
    [{?B2L(K), ?B2L(V)} || {K, V} <- QsVals].

post_params(ReqKey) ->
    Body = request_body(ReqKey, binary),
    BodyQs = parse_qs(Body),
    [{?B2L(K), ?B2L(V)} || {K, V} <- BodyQs].

request_body(ReqKey) ->
    request_body(ReqKey, binary).

request_body(ReqKey, string) ->
    ?B2L(request_body(ReqKey, binary));
request_body(ReqKey, binary) ->
    {RequestCache, Req} = get_key(ReqKey),
     %% We cache the body here because we can't request the body twice in cowboy or it'll crash
    {Body, NewReq} =
    case RequestCache#request_cache.body of
        not_loaded ->
            {ok, B, R} = cowboy_req:body(infinity, Req),
            {B, R};
        B -> {B, Req}
    end,
    put_key(ReqKey, RequestCache#request_cache{body = Body, request = NewReq}),
    Body.

%% TODO: Cowboy's stream_body doesn't support customizable Length and Timeout
recv_from_socket(_Length, _Timeout, ReqKey) ->
    {RequestCache, Req} = get_key(ReqKey),
    case cowboy_req:stream_body(Req) of
        {ok, Data, NewReq} ->
        put_key(ReqKey, RequestCache#request_cache{request = NewReq}),
            Data;
        {done, NewReq} ->
        put_key(ReqKey, RequestCache#request_cache{request = NewReq}),
            <<"">>;
        {error, Reason} ->
            exit({error, Reason}) %% exit(normal) instead?
    end.

%% parse_qs, borrowed from Cowboy by Loic Hugian :)
parse_qs(<<>>) -> [];
parse_qs(Qs) ->
    URLDecode = fun cowboy_http:urldecode/1,
    Tokens = binary:split(Qs, <<"&">>, [global, trim]),
    [case binary:split(Token, <<"=">>) of
        [Token] -> {URLDecode(Token), true};
        [Name, Value] -> {URLDecode(Name), URLDecode(Value)}
    end || Token <- Tokens].

get_key(ReqKey) ->
    RequestCache = #request_cache{request = Req} = cowboy_request_server:get(ReqKey),
    {RequestCache, Req}.

put_key(ReqKey, NewRequestCache) ->
    cowboy_request_server:set(ReqKey, NewRequestCache).

new_key() -> {cowboy_bridge, erlang:make_ref()}.

protocol_version(ReqKey) ->
  {_RequestCache, Req} = get_key(ReqKey),
  {Version, Req} = cowboy_req:version(Req),
  case Version of
    'HTTP/1.1' -> {1, 1};
    'HTTP/1.0' -> {1, 0};
    {H, L} -> {H, L}
  end.
