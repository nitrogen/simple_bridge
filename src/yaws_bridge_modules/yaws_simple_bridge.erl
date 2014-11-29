% vim: ts=4 sw=4 et
% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(yaws_simple_bridge).
-behaviour(simple_bridge).

%% REQUEST EXPORTS
-include("simple_bridge.hrl").
-export ([
    init/1,
    request_method/1, protocol/1, path/1, uri/1,
    peer_ip/1, peer_port/1,
    headers/1, cookies/1,
    query_params/1, post_params/1, request_body/1,
    socket/1, recv_from_socket/3, protocol_version/1
]).

%% RESPONSE EXPORTS
-export([
        build_response/2
       ]).

%% REQUEST

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
    Others2 = [{Header,Value} || {http_header,_Num,Header,_,Value} <- Others],

    [
        {<<"Connection">>, yaws_api:headers_connection(Headers)},
        {<<"Accept">>, yaws_api:headers_accept(Headers)},
        {<<"Host">>, yaws_api:headers_host(Headers)},
        {<<"If-Modified-Since">>, yaws_api:headers_if_modified_since(Headers)},
        {<<"If-Match">>, yaws_api:headers_if_match(Headers)},
        {<<"If-None-Match">>, yaws_api:headers_if_none_match(Headers)},
        {<<"If-Range">>, yaws_api:headers_if_range(Headers)},
        {<<"If-Unmodified-Since">>, yaws_api:headers_if_unmodified_since(Headers)},
        {<<"Range">>, yaws_api:headers_range(Headers)},
        {<<"Referer">>, yaws_api:headers_referer(Headers)},
        {<<"User-Agent">>, yaws_api:headers_user_agent(Headers)},
        {<<"Accept-Ranges">>, yaws_api:headers_accept_ranges(Headers)},
        {<<"Cookie">>, yaws_api:headers_cookie(Headers)},
        {<<"Keep-Alive">>, yaws_api:headers_keep_alive(Headers)},
        {<<"Location">>, yaws_api:headers_location(Headers)},
        {<<"Content-Length">>, yaws_api:headers_content_length(Headers)},
        {<<"Content-Type">>, yaws_api:headers_content_type(Headers)},
        {<<"Content-Encoding">>, yaws_api:headers_content_encoding(Headers)},
        {<<"Authorization">>, yaws_api:headers_authorization(Headers)},
        {<<"Transfer-Encoding">>, yaws_api:headers_transfer_encoding(Headers)},
        {<<"X-Forwarded-For">>, yaws_api:headers_x_forwarded_for(Headers)} 
        | Others2
    ].

cookies(Req) ->
    Headers = yaws_api:arg_headers(Req),
    CookieList0 = yaws_api:headers_cookie(Headers),
    CookieList = string:join(CookieList0, ";"),
    simple_bridge_util:parse_cookie_header(CookieList).

query_params(Arg) ->
    yaws_api:parse_query(Arg).

post_params(Arg) ->
    case should_we_parse_post_params(request_method(Arg)) of
        true -> yaws_api:parse_post(Arg);
        _ -> []
    end.

should_we_parse_post_params('GET') -> false;
should_we_parse_post_params(_) -> true.

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

%% RESPONSE

build_response(_Arg, Res) ->
    % Get vars...
    Code = Res#response.status_code,

    %% Assemble headers...
    Headers = assemble_headers(Res),


    case Res#response.data of
        {data, Body} ->

            %% Get the content type...
            ContentType = get_content_type(Res),

            % Send the yaws response...
            lists:flatten([
                           {status, Code},
                           Headers,
                           {content, ContentType, Body}
                          ]);

        {file, Path} ->
            %% Note: This section should only be entered in the event that a static file is
            %% requested that isn't found in the 'appmod' section of the yaws.conf file.
            %% I've not found a way to "pass the buck" back to yaws and say "even though this
            %% directory isn't found in the appmod, I want you to serve it anyway".  This
            %% means that with the current implementation, you don't want to be serving files
            %% big files that aren't covered in the appmod section, primarily because this little
            %% snippet loads the entire file into memory then passes it off to yaws to be served,
            %% rather than streaming it.  I'll need to look further into to either 1) Pass the buck
            %% completely back to Yaws, or 2) how the streamcontent return types work as define in
            %% yaws_server:handle_out_reply

            %% Static Content should have an expires date.  If not, we're going to make one
            Headers2 = ensure_expires_header(Res, Headers),

            %% Docroot needed to find file in Path
            Docroot = yaws_api:arg_docroot(_Arg),
            FullPath = [Docroot,Path],

            %% Get the content type as defined by yaws
            ContentType = yaws_api:mime_type(Path),
           
            %% Get the file content
            FullResponse = case file:read_file(FullPath) of
                {error,enoent} -> 
                    yaws_outmod:out404(_Arg);
                {ok,Bin} -> 
                    [
                        {status, Code},
                        Headers2,
                        {content, ContentType, Bin}
                    ]
            end,
            lists:flatten(FullResponse)
    end.

assemble_headers(Res) ->
    lists:flatten([
                   [{header, {yaws_kosher_header(X#header.name), X#header.value}} || X <- Res#response.headers],
                   [create_cookie(X) || X <- Res#response.cookies]
                  ]).

yaws_kosher_header(A) when is_atom(A) -> A;
yaws_kosher_header(B) when is_binary(B) -> binary_to_list(B);
yaws_kosher_header(L) when is_list(L) -> L.

%% This is slightly different from the one in simple_bridge_util due to the
%% formatting of the yaws headers isn't just a simple proplist.
ensure_expires_header(Res,Headers) ->
    case simple_bridge_util:needs_expires_header(Res#response.headers) of
        true -> 
            Expires = simple_bridge_util:default_static_expires_header(),
            [{header, Expires} | Headers];
        false -> Headers
    end.
    

get_content_type(Res) ->
    coalesce([
              kvl3(content_type, Res#response.headers),
              kvl3("content-type", Res#response.headers),
              kvl3("Content-Type", Res#response.headers),
              kvl3("CONTENT-TYPE", Res#response.headers),
              "text/html"
             ]).

kvl3(Key,L) ->
    case lists:keysearch(Key,2,L) of
        {value, {_,_,Val}} -> Val;
        _                  -> undefined
    end.

coalesce([]) -> undefined;
coalesce([undefined|T]) -> coalesce(T);
coalesce([H|_T]) -> H.

create_cookie(Cookie) ->
    Name = Cookie#cookie.name,
    Value = Cookie#cookie.value,
    Path = Cookie#cookie.path,
    SecondsToLive = Cookie#cookie.minutes_to_live * 60,
    Expire = to_cookie_expire(SecondsToLive),
    yaws_api:setcookie(Name, Value, Path, Expire).

to_cookie_expire(SecondsToLive) ->
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
    httpd_util:rfc1123_date(DateTime).
