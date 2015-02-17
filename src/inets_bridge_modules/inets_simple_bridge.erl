% vim: ts=4 sw=4 et
% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (inets_simple_bridge).
-behaviour (simple_bridge).
-include("simple_bridge.hrl").
-include_lib("inets/include/httpd.hrl").
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
    case Req#mod.socket of
        S when is_tuple(S), element(1, S) =:= sslsocket -> https;
        _ -> http
    end.

request_method(Req) -> 
    list_to_atom(Req#mod.method).

path(Req) -> 
    {Path, _QueryString} = split_request_uri(Req#mod.request_uri, []),
    Path.

uri(Req) ->
    Req#mod.request_uri.

peer_ip(Req) -> 
    Socket = Req#mod.socket,
    {ok, {IP, _Port}} =
        case Socket of
            S when is_tuple(S), 
                   element(1, S) =:= sslsocket -> 
                ssl:peername(Socket);
            _ -> 
                inet:peername(Socket)
        end,
    IP.

peer_port(Req) -> 
    Socket = Req#mod.socket,
    {ok, {_IP, Port}} =
        case Socket of
            S when is_tuple(S), 
                   element(1, S) =:= sslsocket ->
                ssl:peername(Socket);
            _ -> 
                inet:peername(Socket)
        end,
    Port.

headers(Req) ->
    Req#mod.parsed_header.

cookies(Req) ->
    Headers = Req#mod.parsed_header,
    CookieData = proplists:get_value("cookie", Headers, ""),
    simple_bridge_util:parse_cookie_header(CookieData).

query_params(Req) ->
    {_Path, QueryString} = split_request_uri(Req#mod.request_uri, []),
    Query = httpd:parse_query(QueryString),
    [{Key, Value} || {Key, Value} <- Query, Key /= []].

post_params(Req) ->
    Body = request_body(Req),
    Query = httpd:parse_query(Body),
    [{Key, Value} || {Key, Value} <- Query, Key /= []].

request_body(Req) ->
    Req#mod.entity_body.

socket(Req) -> 
    Req#mod.socket.

recv_from_socket(Length, Timeout, Req) -> 
    Socket = socket(Req),
    case gen_tcp:recv(Socket, Length, Timeout) of
        {ok, Data} -> Data;
        _ -> exit(normal)
    end.

protocol_version(Req) ->
  case Req#mod.http_version of
    "HTTP/0.9" -> {0, 9};
    "HTTP/1.0" -> {1, 0};
    "HTTP/1.1" -> {1, 1}
  end.

%%% PRIVATE FUNCTIONS %%%
split_request_uri([], Path) -> {lists:reverse(Path), ""};
split_request_uri([$?|QueryString], Path) -> {lists:reverse(Path), QueryString};
split_request_uri([H|T], Path) -> split_request_uri(T,[H|Path]).

build_response(Req, Res) -> 
    ResponseCode = Res#response.status_code,
    case Res#response.data of
        {data, Data0} ->
            %% We wrap Data in a list, as httpd_util:flatlength expects a list
            %% and Data could conceivably be a binary, which would cause a
            %% crash
            Data = [Data0],
            Size = integer_to_list(httpd_util:flatlength(Data)),

            % Assemble headers...
            Headers = lists:flatten([
                {code, ResponseCode},
                {content_length, Size},
                [{massage(X#header.name), X#header.value} || X <- Res#response.headers],
                [create_cookie_header(X) || X <- Res#response.cookies]
            ]),     

            % Send the inets response...
            {break,[
                {response, {response, Headers, Data}}
            ]};

        {file, _Path} ->
            mod_get:do(Req)
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


% Inets wants some headers as lowercase atoms, and the rest as lists. So let's fix these up.
massage(Header) when is_binary(Header) ->
    massage(binary_to_list(Header));
massage(Header) ->
    X = simple_bridge_util:atomize_header(Header),
    case lists:member(X, special_headers()) of
        true  -> X;
        false -> Header
    end.

special_headers() ->
    [accept_ranges , allow , cache_control , content_MD5 , content_encoding , 
     content_language , content_length , content_location , content_range , 
     content_type , date , etag , expires , last_modified , location , 
     pragma , retry_after , server , trailer , transfer_encoding].
