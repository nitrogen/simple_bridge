% vim: ts=4 sw=4 et
% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(yaws_request_bridge).
-include_lib ("simple_bridge.hrl").
-export ([
    init/1,
    request_method/1, protocol/1, path/1, uri/1,
    peer_ip/1, peer_port/1,
    headers/1, cookie/2, cookies/1,
    query_params/1, post_params/1, request_body/1,
    socket/1, recv_from_socket/3, protocol_version/1
]).

init(Req) ->
    Req.

protocol(Arg) -> 
    case yaws_api:arg_clisock(Arg) of
        S when is_tuple(S), element(1, S) =:= sslsocket -> https;
        _ -> http
    end.

request_method(Arg) ->
    yaws_api:http_request_method(yaws_api:arg_req(Arg)).

path(Arg) ->
    yaws_api:arg_server_path(Arg).

uri(Arg) ->
    {abs_path, Path} = yaws_api:http_request_path(yaws_api:arg_req(Arg)),
    Path.

peer_ip(Arg) -> 
    Socket = socket(Arg),
    {ok, {IP, _Port}} =
        case Socket of
            {ssl, S} ->
                ssl:peername(S);
            _ ->
                inet:peername(Socket)
        end,
    IP.

peer_port(Arg) -> 
    Socket = socket(Arg),
    {ok, {_IP, Port}} = 
        case Socket of
            {ssl, S} ->
                ssl:peername(S);
            _ ->
                inet:peername(Socket)
        end,
    Port.

headers(Arg) ->
    Headers = yaws_api:arg_headers(Arg),
    
    %% Get the other headers and format them to fit the paradigm we're using above
    Others = yaws_api:headers_other(Headers),
    Others2 = [{simple_bridge_util:atomize_header(Header),Value} || {http_header,_Num,Header,_,Value} <- Others],

    [
        {connection, yaws_api:headers_connection(Headers)},
        {accept, yaws_api:headers_accept(Headers)},
        {host, yaws_api:headers_host(Headers)},
        {if_modified_since, yaws_api:headers_if_modified_since(Headers)},
        {if_match, yaws_api:headers_if_match(Headers)},
        {if_none_match, yaws_api:headers_if_none_match(Headers)},
        {if_range, yaws_api:headers_if_range(Headers)},
        {if_unmodified_since, yaws_api:headers_if_unmodified_since(Headers)},
        {range, yaws_api:headers_range(Headers)},
        {referer, yaws_api:headers_referer(Headers)},
        {user_agent, yaws_api:headers_user_agent(Headers)},
        {accept_ranges, yaws_api:headers_accept_ranges(Headers)},
        {cookie, yaws_api:headers_cookie(Headers)},
        {keep_alive, yaws_api:headers_keep_alive(Headers)},
        {location, yaws_api:headers_location(Headers)},
        {content_length, yaws_api:headers_content_length(Headers)},
        {content_type, yaws_api:headers_content_type(Headers)},
        {content_encoding, yaws_api:headers_content_encoding(Headers)},
        {authorization, yaws_api:headers_authorization(Headers)},
        {transfer_encoding, yaws_api:headers_transfer_encoding(Headers)},
        {x_forwarded_for, yaws_api:headers_x_forwarded_for(Headers)} 
        | Others2
    ].

cookie(Key, Req) ->
    Key1 = wf:to_list(Key),
    Headers = yaws_api:arg_headers(Req),
    yaws_api:find_cookie_val(Key1, yaws_api:headers_cookie(Headers)).

cookies(Req) ->
    Headers = yaws_api:arg_headers(Req),
    CookieList = yaws_api:headers_cookie(Headers),
    F = fun(Cookie) ->
        Key = hd(string:tokens(Cookie, "=")),
        Val = yaws_api:find_cookie_val(Key, [Cookie]),
        {Key, Val}
    end,
    [F(X) || X <- CookieList].

query_params(Arg) ->
    yaws_api:parse_query(Arg).

post_params(Arg) ->
    yaws_api:parse_post(Arg).

request_body(Arg) ->
    case yaws_api:arg_clidata(Arg) of
        {partial, Data} -> Data;
        Data -> Data
    end.  

socket(Arg) ->
    yaws_api:arg_clisock(Arg).

recv_from_socket(Length, Timeout, Arg) -> 
    Socket = socket(Arg),
    case gen_tcp:recv(Socket, Length, Timeout) of
        {ok, Data} -> Data;
        _ -> exit(normal)
    end.

protocol_version(Arg) ->
  yaws_api:http_request_version(yaws_api:arg_req(Arg)).
