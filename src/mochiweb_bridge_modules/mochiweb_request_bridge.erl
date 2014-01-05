% vim: sw=4 ts=4 et
% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (mochiweb_request_bridge).
-behaviour (simple_bridge_request).
-include_lib ("simple_bridge.hrl").
-export ([
    init/1,
    protocol/1, request_method/1, path/1, uri/1,
    peer_ip/1, peer_port/1,
    headers/1, header/2, cookies/1,
    query_params/1, post_params/1, request_body/1,
    socket/1, recv_from_socket/3,
    protocol_version/1
]).

%% Max Body of 10MB by default
%% NOTE: It seems to be common for folks to manually override this directly in
%% the code here.  That, however, is not necessary.  It's far simpler to set
%% the faux-mochiweb environment variable max_reqest_size. I say "faux-mochiweb"
%% because it isn't ACTUALLY a stock mochiweb variable, but just one that
%% simple_bridge uses to override this hard-coded default.
%%
%% Either put the value into a mochiweb.config and load that, or you can
%% just specify it in code or the console with:
%% 
%%      application:set_env(mochiweb, max_request_size, Size).
%%
%% where size is an integer containing the number of bytes
-define(MAX_RECV_BODY,(1024*1024*10)).

%% {Req, DocRoot} is deprecated
%% Maintained for backwards compatibility.
%% In Mochiweb 2.4.1, Req is a two-tuple, and this was parsing out just the
%% first element of the tuple.  This ensures that only if it's an old-style
%% {req, docroot} tuple will this clause match.
init({Req, _DocRoot}) when Req =/= mochiweb_request -> 
    init(Req);
init(Req) -> 
    Req.

protocol(Req) ->
    Req:get(scheme).

request_method(Req) -> 
    Req:get(method).

path(Req) -> 
    RawPath = Req:get(raw_path),
    {Path, _, _} = mochiweb_util:urlsplit_path(RawPath),
    Path.

uri(Req) ->
    Req:get(raw_path).

peer_ip(Req) -> 
    case Req:get(socket) of
        false -> {127, 0, 0, 1};
        Socket ->
            {ok, {IP, _Port}} = mochiweb_socket:peername(Socket),
            IP
    end.

peer_port(Req) -> 
    Socket = Req:get(socket),
    {ok, {_IP, Port}} = mochiweb_socket:peername(Socket),
    Port.
    
header(connection, Req) ->
    Req:get_header_value("connection");
header(accept, Req) ->
    Req:get_header_value("accept");
header(host, Req) ->
    Req:get_header_value("host");
header(if_modified_since, Req) ->
    Req:get_header_value("if-modified-since");
header(if_match, Req) ->
    Req:get_header_value("if-match");
header(if_none_match, Req) ->
    Req:get_header_value("if-none-match");
header(if_unmodified_since, Req) ->
    Req:get_header_value("if-unmodified-since");
header(if_range, Req) ->
    Req:get_header_value("if-range");
header(range, Req) ->
    Req:get_header_value("range");
header(user_agent, Req) ->
    Req:get_header_value("user-agent");
header(accept_language, Req) ->
    Req:get_header_value("accept-language");
header(accept_ranges, Req) ->
    Req:get_header_value("accept-ranges");
header(cookie, Req) ->
    Req:get_header_value("cookie");
header(keep_alive, Req) ->
    Req:get_header_value("keep-alive");
header(location, Req) ->
    Req:get_header_value("location");
header(content_length, Req) ->
    Req:get_header_value("content-length");
header(content_type, Req) ->
    Req:get_header_value("content-type");
header(content_encoding, Req) ->
    Req:get_header_value("content-encoding");
header(authorization, Req) ->
    Req:get_header_value("authorization");
header(x_forwarded_for, Req) ->
    Req:get_header_value("x-forwarded-for");
header(x_forwarded_proto, Req) ->
    Req:get_header_value("x-forwarded-proto");
header(transfer_encoding, Req) ->
    Req:get_header_value("transfer-encoding");
header(accept_encoding, Req) ->
    Req:get_header_value("accept-encoding");
header(Header, Req) ->
    Req:get_header_value(Header).

headers(Req) ->
    Headers = [connection, accept, host, if_modified_since,
        connection, accept, host, if_modified_since, if_match, 
        if_none_match, if_unmodified_since, if_range, range, 
        referer, user_agent, accept_language, accept_ranges, 
        cookie, keep_alive, location, content_length, content_type,
        content_encoding, authorization, x_forwarded_for, x_forwarded_proto, transfer_encoding,
        accept_encoding
    ],
    Headers1 = lists:map(fun(H) -> {H, header(H, Req)} end, Headers),
    [{K, V} || {K, V} <- Headers1, V /= undefined].

cookies(Req) ->
    Req:parse_cookie().

query_params(Req) ->
    Req:parse_qs().

post_params(Req) ->
    Req:parse_post().

request_body(Req) ->
    MaxBody = case application:get_env(mochiweb,max_request_size) of
        undefined -> 
            ?MAX_RECV_BODY;
        {ok, Max} when is_integer(Max) -> 
            Max;
        Other -> 
            error_logger:warning_msg("Mochiweb Simple Bridge Configuration Error!  Unknown value for 'mochiweb' application variable 'max_request_size': ~p. Expected: integer() or undefined. Using Default of ~p~n",[Other,?MAX_RECV_BODY]),
            ?MAX_RECV_BODY
    end,
    Req:recv_body(MaxBody).

socket(Req) ->  
    Req:get(socket).

recv_from_socket(Length, Timeout, Req) -> 
    Socket = socket(Req),
    case gen_tcp:recv(Socket, Length, Timeout) of
        {ok, Data} -> 
            put(mochiweb_request_recv, true),
            Data;
        _Other -> 
            exit(normal)
    end.

protocol_version(Req) ->
    case Req:get(version) of
        'HTTP/1.1' -> {1, 1};
        'HTTP/1.0' -> {1, 0};
        {H, L} -> {H, L}
    end.
