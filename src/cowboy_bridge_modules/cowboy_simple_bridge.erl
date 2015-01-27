%% vim: ts=4 sw=4 et
% Simple Bridge Cowboy
% Copyright (c) 2012 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (cowboy_simple_bridge).
-behaviour (simple_bridge).
-include("simple_bridge.hrl").

%% REQUEST EXPORTS

-export([
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

%% RESPONSE EXPORTS
-export([
        build_response/2
    ]).

new_key() ->
    {cowboy_bridge, erlang:make_ref()}.

get_key(ReqKey) ->
    try
        RequestCache = #request_cache{request = Req} = cowboy_request_server:get(ReqKey),
        {RequestCache, Req}
    catch E:T ->
        error_logger:info_msg("~p:~p~n~p", [E, T, erlang:get_stacktrace()])
    end.

put_key(ReqKey, NewRequestCache) ->
    cowboy_request_server:set(ReqKey, NewRequestCache).


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
    list_to_atom(simple_bridge_util:to_list(Method)).

path(ReqKey) ->
    {_RequestCache, Req} = get_key(ReqKey),
    {Path, Req} = cowboy_req:path(Req),
    simple_bridge_util:to_list(Path).

uri(ReqKey) ->
    {_RequestCache, Req} = get_key(ReqKey),
    {URL, Req} = cowboy_req:url(Req),
    case re:run(URL, "^https?://[^/]*(/.*)$", [{capture, all_but_first, list}]) of
        {match, [Uri]} -> Uri;
        _ -> ""
    end.

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
    Headers.

cookies(ReqKey) ->
    {RequestCache, Req} = get_key(ReqKey),
    {Cookies, NewReq} = cowboy_req:cookies(Req),
    put_key(ReqKey, RequestCache#request_cache{request = NewReq}),
    Cookies.

query_params(ReqKey) ->
    {RequestCache, Req} = get_key(ReqKey),
    {QsVals, NewReq} = cowboy_req:qs_vals(Req),
    put_key(ReqKey, RequestCache#request_cache{request = NewReq}),
    QsVals.

post_params(ReqKey) ->
    Body = request_body(ReqKey),
    BodyQs = cow_qs:parse_qs(Body),
    BodyQs.

request_body(ReqKey) ->
    {RequestCache, Req} = get_key(ReqKey),
    {Body, NewReq} = case RequestCache#request_cache.body of
        not_loaded ->
            %% We start with 2MB here, as headers and form fields will almost
            %% certainly be in the first 2mb of a request, and give the client
            %% 120 seconds to send the chunk.
            %% TODO, Make the read_timeout a configuration option for simple_bridge
            case cowboy_req:body(Req, [{length, 2000000}, {read_timeout, 120000}]) of
                {ok, B, R} -> {B, R};
                {more, B, R} -> {B, R}
            end;
        B ->
            {B, Req}
    end,
    put_key(ReqKey, RequestCache#request_cache{body = Body, request = NewReq}),
    Body.

socket(_ReqKey) ->
    undefined.

recv_from_socket(_Length, _Timeout, ReqKey) ->
    {RequestCache, Req} = get_key(ReqKey),
    case cowboy_req:body(Req, [{length, 8000000}]) of
        {ok, Data, NewReq} ->
            put_key(ReqKey, RequestCache#request_cache{request = NewReq}),
            Data;
        {more, Data, NewReq} ->
            put_key(ReqKey, RequestCache#request_cache{request = NewReq}),
            Data;
        {error, Reason} ->
            exit({error, Reason}) %% exit(normal) instead?
    end.


protocol_version(ReqKey) ->
    {_RequestCache, Req} = get_key(ReqKey),
    {Version, Req} = cowboy_req:version(Req),
    case Version of
        'HTTP/1.1' -> {1, 1};
        'HTTP/1.0' -> {1, 0};
        {H, L} -> {H, L}
    end.

%% RESPONSE

build_response(ReqKey, Res) ->
    RequestCache = #request_cache{request = Req, docroot=DocRoot} = cowboy_request_server:get(ReqKey),
    % Some values...
    Code = Res#response.status_code,

    %% assemble headers...
    Headers = lists:flatten([[{X#header.name, X#header.value} || X <- Res#response.headers]]),

    case Res#response.data of
        {data, Body} ->
            % Assemble headers...
            Headers2 = simple_bridge_util:ensure_header(Headers,"Content-Type","text/html"),

            % Send the cowboy cookies
            {ok, FinReq} = send(Code, Headers2, Res#response.cookies, Body, Req),
            cowboy_request_server:set(ReqKey, RequestCache#request_cache{request = FinReq}),
            {ok, FinReq};

        {file, P} ->
            %% Note: that this entire {file, Path} section should be avoided
            %% as much as possible. The cowboy static handler is designed for
            %% this task, but this is put here to help the programmer in the
            %% case of a misconfiguration.
            %% 
            %% You want to make sure that cowboy.config is properly set
            %% up with paths so that the requests for static files are
            %% properly handled by cowboy directly.
            %% 
            %% See https://github.com/nitrogen/nitrogen/blob/master/rel/overlay/cowboy/etc/cowboy.config
            %% and
            %% https://github.com/nitrogen/nitrogen/blob/master/rel/overlay/cowboy/site/src/nitrogen_sup.erl
 
            Path = strip_leading_slash(P),
            Mimetype = get_mimetype(Path),
            Headers2 = simple_bridge_util:ensure_header(Headers,{"Content-Type", Mimetype}),
            Headers3 = simple_bridge_util:ensure_expires_header(Headers2),
            FullPath = filename:join(DocRoot, Path),
            {ok, FinReq} = case filelib:is_regular(FullPath) of
                false ->
                    send(404, [], [], "Not Found", Req);
                true -> 
                    Size = filelib:file_size(FullPath),
                    StreamFun = fun(Socket, Transport) ->
                        case Transport:sendfile(Socket, FullPath) of
                            {ok, _} -> ok;
                            {error, closed} -> ok
                        end
                    end,
                    Body = {stream, Size, StreamFun},
                    send(200, Headers3, [], Body, Req)
            end,
            cowboy_request_server:set(ReqKey, RequestCache#request_cache{request = FinReq}),
            {ok, FinReq}
    end.

get_mimetype(Path) ->
    {Mime1, Mime2, _} = cow_mimetypes:all(list_to_binary(Path)),
    binary_to_list(Mime1) ++ "/" ++ binary_to_list(Mime2).

%% Just to strip leading slash, as cowboy tends to do this.
%% If no leading slash, just return the path.
strip_leading_slash([$/ | Path]) -> Path;
strip_leading_slash(Path) -> Path.

send(Code, Headers, Cookies, Body, Req) ->
    Req1 = prepare_cookies(Req, Cookies),
    Req2 = prepare_headers(Req1, Headers),
    Req3 = case Body of
        {stream, Size, Fun} -> 
            cowboy_req:set_resp_body_fun(Size, Fun, Req2);
        _ ->
            cowboy_req:set_resp_body(Body, Req2)
    end,
    {ok, _ReqFinal} = cowboy_req:reply(Code, Req3).

prepare_cookies(Req, Cookies) ->
    lists:foldl(fun(C, R) ->
        %% In case cookie name or value was set to an atom, we need to make
        %% sure it's something usable, so let's just use binary
        Name = simple_bridge_util:to_binary(C#cookie.name),
        Value = simple_bridge_util:to_binary(C#cookie.value),
        Path = C#cookie.path,
        SecsToLive = C#cookie.minutes_to_live * 60,
        Options = [{path, Path}, {max_age, SecsToLive}],
        cowboy_req:set_resp_cookie(Name, Value, Options, R)
    end, Req, Cookies).

prepare_headers(Req, Headers) ->
    lists:foldl(fun({Header, Value}, R) -> cowboy_req:set_resp_header(Header, Value, R) end, Req, Headers).
