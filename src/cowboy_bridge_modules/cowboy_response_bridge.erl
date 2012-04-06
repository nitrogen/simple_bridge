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
        element(1,Request)==simple_bridge_request_wrapper,
        element(2,Request)==cowboy_request_bridge ->
    element(3,Request); %% The third element of Request is the RequestKey from response_bridge
init({Req,DocRoot}) ->
    %% Since cowboy request and response information are the same, this synchronizes it
    cowboy_request_bridge:init({Req,DocRoot}).

build_response(ReqKey, Res) ->
    RequestCache = #request_cache{request=Req,docroot=DocRoot} = cowboy_request_server:get(ReqKey),
    % Some values...
    Code = Res#response.statuscode,

    case Res#response.data of
        {data, Body} ->

            % Assemble headers...
            Headers = lists:flatten([
                [{X#header.name, X#header.value} || X <- Res#response.headers]
            ]),

            % Ensure content type...
            F = fun(Key) -> lists:keymember(Key, 1, Headers) end,
            HasContentType = lists:any(F, ["content-type", "Content-Type", "CONTENT-TYPE"]),
            Headers2 = case HasContentType of
                true -> Headers;
                false -> [{"Content-Type", "text/html"}|Headers]
            end,

            % Send the cowboy cookies
            {ok, FinReq} = send(Code,Headers2,Res#response.cookies,Body,Req),
        
            NewRequestCache = RequestCache#request_cache{
                request=FinReq
            },
            cowboy_request_server:set(ReqKey,NewRequestCache),
            {ok,FinReq};
            
        {file, Path} ->
            %% Calculate expire date far into future...
            Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
            TenYears = 10 * 365 * 24 * 60 * 60,
            Seconds1 = calendar:gregorian_seconds_to_datetime(Seconds + TenYears),
            ExpireDate = httpd_util:rfc1123_date(Seconds1),
            
            [$. | Ext] = filename:extension(Path),
            Mimetype =  mimetypes:extension(Ext),

            %% Create the response telling Mochiweb to serve the file...
            Headers = [
                {"Expires", ExpireDate},
                {"Content-Type",Mimetype}
            ],
            
            io:format("Serving static file ~p~n",[Path]),
        
            FullPath = filename:join(DocRoot,Path),
            {ok, FinReq} = case file:read_file(FullPath) of
                {error,enoent} -> 
                    {ok, _R} = send(404,[],[],"Not Found",Req);
                {ok,Bin} -> 
                    {ok, _R} = send(200,Headers,[],Bin,Req)
            end,
    
            NewRequestCache = RequestCache#request_cache{
                request=FinReq
            },
            cowboy_request_server:set(ReqKey,NewRequestCache),
            {ok, FinReq}
    end.

send(Code,Headers,Cookies,Body,Req) ->
    Req1 = prepare_cookies(Req,Cookies),
    Req2 = prepare_headers(Req1,Headers),
    {ok, Req3} = cowboy_http_req:set_resp_body(Body,Req2),
    {ok, _ReqFinal} = cowboy_http_req:reply(Code, Req3).

prepare_cookies(Req,Cookies) ->
    lists:foldl(fun(C,R) ->
        Name = iol2b(C#cookie.name),
        Value = iol2b(C#cookie.value),
        Path = iol2b(C#cookie.path),
        SecsToLive = C#cookie.minutes_to_live * 60,
        Options = [{path,Path},{max_age,SecsToLive}],
        {ok,NewReq} = cowboy_http_req:set_resp_cookie(Name,Value,Options,R),
        NewReq
    end,Req,Cookies).

prepare_headers(Req,Headers) ->
    lists:foldl(fun({Header,Value},R) ->
        {ok,NewReq} = cowboy_http_req:set_resp_header(iol2b(Header),iol2b(Value),R),
        NewReq
    end,Req,Headers).


iol2b(V) when is_binary(V) -> V;
iol2b(V) -> iolist_to_binary(V).
