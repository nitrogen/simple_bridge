% vim: sw=4 ts=4 et
% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (mochiweb_simple_bridge).
-behaviour (simple_bridge).
-include("simple_bridge.hrl").
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

%% TODO: CONVERT ALL CALLS TO NON-PMOD CALLS

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

%% REQUEST FUNCTIONS

init(Req) -> 
    Req.

protocol(Req) ->
    mochiweb_request:get(scheme, Req).

request_method(Req) -> 
    mochiweb_request:get(method, Req).

path(Req) -> 
    RawPath = mochiweb_request:get(raw_path, Req),
    {Path, _, _} = mochiweb_util:urlsplit_path(RawPath),
    Path.

uri(Req) ->
    mochiweb_request:get(raw_path, Req).

peer_ip(Req) -> 
    case mochiweb_request:get(socket, Req) of
        false -> {127, 0, 0, 1};
        Socket ->
            {ok, {IP, _Port}} = mochiweb_socket:peername(Socket),
            IP
    end.

peer_port(Req) -> 
    Socket = mochiweb_request:get(socket, Req),
    {ok, {_IP, Port}} = mochiweb_socket:peername(Socket),
    Port.
    
headers(Req) ->
    Headers = mochiweb_request:get(headers, Req),
    mochiweb_headers:to_list(Headers).

cookies(Req) ->
    mochiweb_request:parse_cookie(Req).

query_params(Req) ->
    mochiweb_request:parse_qs(Req).

post_params(Req) ->
    mochiweb_request:parse_post(Req).

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
    mochiweb_request:recv_body(MaxBody, Req).

socket(Req) ->  
    mochiweb_request:get(socket, Req).

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
    case mochiweb_request:get(version, Req) of
        'HTTP/1.1' -> {1, 1};
        'HTTP/1.0' -> {1, 0};
        {H, L} -> {H, L}
    end.


%% RESPONSE BRIDGE

build_response(Req, Res) ->
    % Some values...
    Code = Res#response.status_code,

    %% Assemble headers...
    Headers = lists:flatten([
			     [{X#header.name, X#header.value} || X <- Res#response.headers],
			     [create_cookie_header(X) || X <- Res#response.cookies]
			  ]),

    case Res#response.data of
        {data, Body} ->
            % Send the mochiweb response...
            Headers2 = simple_bridge_util:ensure_header(Headers,{"Content-Type","text/html"}),
            mochiweb_request:respond({Code, Headers2, Body}, Req);
        {file, Path} ->
            %% Create the response telling Mochiweb to serve the file...
            Headers2 = simple_bridge_util:ensure_expires_header(Headers),
            DocRoot = simple_bridge_util:get_docroot(mochiweb),
            mochiweb_request:serve_file(tl(Path), DocRoot, Headers2, Req)
    end.



create_cookie_header(Cookie) ->
    SecondsToLive = Cookie#cookie.minutes_to_live * 60,
    Name = Cookie#cookie.name,
    Value = Cookie#cookie.value,
    Path = Cookie#cookie.path,
    mochiweb_cookies:cookie(Name, Value, [{path, Path}, {max_age, SecondsToLive}]).
