%% vim: ts=4 sw=4 et
% Simple Bridge
% Copyright (c) 2008-2012 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (cowboy_response_bridge).
-behaviour (simple_bridge_response).
-include_lib ("simple_bridge.hrl").
-export ([build_response/2,init/1]).

init(Request) when
      is_tuple(Request),
      element(1, Request) == simple_bridge_request_wrapper,
      element(2, Request) == cowboy_request_bridge ->
    element(3, Request); %% The third element of Request is the RequestKey from response_bridge
init(Req) ->
    %% Since cowboy request and response information are the same, this synchronizes it
    cowboy_request_bridge:init(Req).

build_response(ReqKey, Res) ->
    RequestCache = #request_cache{request = Req, docroot=DocRoot} = cowboy_request_server:get(ReqKey),
    % Some values...
    Code = Res#response.statuscode,

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
            %% as much as possible, since this reads the entire file into
            %% memory before sending.
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
            Headers2 = simple_bridge_util:ensure_header(Headers,{"Content-Type",Mimetype}),
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

%% generate_static_error(P) ->
%%     error_logger:warning_msg("~p",[
%%             {unrouted_static_file, [
%%             {requested_file, P},
%%             {description, "Simple Bridge through Cowboy is not set up to handle static files. Static Files should be handled by Cowboy through the routing table."},
%%             {see_also, [
%%                 "https://github.com/nitrogen/nitrogen/blob/master/rel/overlay/cowboy/site/src/nitrogen_sup.erl",
%%                 "https://github.com/nitrogen/nitrogen/blob/master/rel/overlay/cowboy/etc/cowboy.config"
%%             ]}
%%         ]}
%%     ]).

get_mimetype(Path) ->
    [$. | Ext] = filename:extension(Path),
    mimetypes:extension(Ext).

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
        Name = C#cookie.name,
        Value = ?B2L(C#cookie.value),
        Path = C#cookie.path,
        SecsToLive = C#cookie.minutes_to_live * 60,
        Options = [{path, Path}, {max_age, SecsToLive}],
        cowboy_req:set_resp_cookie(Name, Value, Options, R)
    end, Req, Cookies).

prepare_headers(Req, Headers) ->
    lists:foldl(fun({Header, Value}, R) -> cowboy_req:set_resp_header(Header, Value, R) end, Req, Headers).
