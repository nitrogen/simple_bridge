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
    RequestCache = #request_cache{request = Req} = cowboy_request_server:get(ReqKey),
    % Some values...
    Code = Res#response.statuscode,

    case Res#response.data of
        {data, Body} ->
            % Assemble headers...
            Headers = lists:flatten([[{X#header.name, X#header.value} || X <- Res#response.headers]]),

            % Ensure content type...
            F = fun(Key) -> lists:keymember(Key, 1, Headers) end,
            HasContentType = lists:any(F, ["content-type", "Content-Type", "CONTENT-TYPE"]),
            Headers2 =
		case HasContentType of
		    true -> Headers;
		    false -> [{"Content-Type", "text/html"} | Headers]
		end,

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
            %% https://github.com/choptastic/nitrogen/blob/master/rel/overlay/cowboy/site/src/nitrogen_sup.erl

            %% % Cowboy path starts with / so we need to remove it
            %% Path = strip_leading_slash(P),
            %% ExpireDate = simple_bridge_util:expires(years, 10),
            %% [$. | Ext] = filename:extension(Path),
            %% Mimetype =  mimetypes:extension(Ext),
            %% Headers = [{"Expires", ExpireDate}, {"Content-Type",Mimetype}],

            %% io:format("Serving static file ~p from docroot of ~p ~n",[Path, DocRoot]),

            %% FullPath = filename:join(DocRoot, Path),
            %% {ok, FinReq} =
	    %% 	case file:read_file(FullPath) of
	    %% 	    {error,enoent} -> {ok, _R} = send(404, [], [], "Not Found", Req);
	    %% 	    {ok,Bin} -> {ok, _R} = send(200, Headers, [], Bin, Req)
	    %% 	end,
            %% cowboy_request_server:set(ReqKey, RequestCache#request_cache{request = FinReq}),
            %% {ok, FinReq}
	    throw({error, P, fix_cowboy_routing})
    end.

%% %% Just to strip leading slash, as cowboy tends to do this.
%% %% If no leading slash, just return the path.
%% strip_leading_slash([$/ | Path]) -> Path;
%% strip_leading_slash(Path) -> Path.

send(Code, Headers, Cookies, Body, Req) ->
    Req1 = prepare_cookies(Req, Cookies),
    Req2 = prepare_headers(Req1, Headers),
    Req3 = cowboy_req:set_resp_body(Body, Req2),
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
