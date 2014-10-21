%% vim: ts=4 sw=4 et
% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% Copyright (c) 2013-2014 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (webmachine_simple_bridge).
-behaviour (simple_bridge).
-include ("simple_bridge.hrl").

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
        socket/1,
        recv_from_socket/3,
        protocol_version/1
    ]).

-export([
        build_response/2
    ]).

init(Req) -> 
    Req.

protocol(Req) ->
    wrq:scheme(Req).

request_method(Req) -> 
    wrq:method(Req).

path(Req) -> 
    wrq:path(Req).

uri(Req) -> 
    wrq:raw_path(Req).

peer_ip(Req) ->
    {ok, Address} = inet_parse:address(wrq:peer(Req)),
    Address.

peer_port(Req) ->
    wrq:port(Req).

headers(Req) ->
    Mochiheaders = wrq:req_headers(Req),
    mochiweb_headers:to_list(Mochiheaders).

cookies(Req) ->
    wrq:req_cookie(Req).

query_params(Req) ->
    Value = wrq:req_qs(Req),
    Value.

post_params(Req) ->
    Body = wrq:req_body(Req),
    Value = mochiweb_util:parse_qs(Body),
    Value.

request_body(Req) ->
    wrq:req_body(Req).

socket(Req) ->
    %% If https://github.com/basho/webmachine/pull/175 is accepted, change to:
    %% wrq:socket(Req).

    %% 7th element of wm_reqdata record is wm_state, which contains another
    %% record, but which can be interacted with through
    %% webmachine_request:socket
    %% 
    %% If this suddenly starts breaking, then we need to verify the record
    %% structure of wm_reqdata.hrl
    %%
    %% https://github.com/basho/webmachine/blob/master/include/wm_reqdata.hrl
    ReqState = element(7, Req), 
    {Socket, _} = webmachine_request:socket(ReqState),
    Socket.

recv_from_socket(Length, Timeout, Req) ->
    Socket = socket(Req),
    case gen_tcp:recv(Socket, Length, Timeout) of
        {ok, Data} -> Data;
        _ -> exit(normal)
    end.

protocol_version(Req) ->
  wrq:version(Req).

build_response(Req, Res) -> 
    Code = Res#response.status_code,
    case Res#response.data of
        {data, Body} ->
            %% httpd_util:flatlength expects a list and body could be a binary,
            %% so wrap it in a list, and we have ourselves an iolist. MUCH
            %% EXCITE!
            Size = integer_to_list(httpd_util:flatlength([Body])),

            %% Assemble headers...
            Headers = lists:flatten([
                {content_length, Size},
                [{X#header.name, X#header.value} || X <- Res#response.headers],
                [create_cookie_header(X) || X <- Res#response.cookies]
            ]),     

            Req1 = wrq:set_response_code(Code, Req),
            Req2 = wrq:set_resp_headers(Headers, Req1),
            {ok, Body, Req2};
        {file, Path} ->
            %% We should really never get here, because Webmachine
            %% works via a dispatch table mapping routes to
            %% modules. For this case to happen, it means the dispatch
            %% table mapped a route to a static file.
            throw({unrouted_static_file, [
                {requested_file, Path},
                {description, "Simple Bridge for Webmachine is not set up to handle static files. Static files should be handled by Webmachine through the dispatch table."},
                {see_also, [
                    "https://github.com/nitrogen/simple_bridge/blob/master/src/webmachine/webmachine_simple_bridge_sup.erl",
                    "https://github.com/nitrogen/simple_bridge/blob/master/etc/webmachine.config"
                ]}
            ]})
            
    end.

create_cookie_header(Cookie) ->
    SecondsToLive = Cookie#cookie.minutes_to_live * 60,
    Expire = to_cookie_expire(SecondsToLive),
    Name = Cookie#cookie.name,
    Value = Cookie#cookie.value,
    Path = Cookie#cookie.path,
    {"Set-Cookie", io_lib:format("~s=~s; Path=~s; Expires=~s", [Name, Value, Path, Expire])}.

to_cookie_expire(SecondsToLive) ->
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
    httpd_util:rfc1123_date(DateTime).
