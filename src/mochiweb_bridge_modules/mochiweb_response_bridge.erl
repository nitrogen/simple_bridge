% vim: sw=4 ts=4 et
% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (mochiweb_response_bridge).
-behaviour (simple_bridge_response).
-include_lib ("simple_bridge.hrl").
-export ([build_response/2,init/1]).

init({Req,DocRoot}) ->
    {Req,DocRoot}.

build_response({Req, DocRoot}, Res) ->
    % Some values...
    Code = Res#response.statuscode,

    %% Assemble headers...
    Headers = lists:flatten([
			     [{X#header.name, X#header.value} || X <- Res#response.headers],
			     [create_cookie_header(X) || X <- Res#response.cookies]
			    ]),

    %%  Ensure content type...
    F = fun(Key) ->
		lists:keymember(Key, 1, Headers)
	end,
    HasContentType = lists:any(F, ["content-type", "Content-Type", "CONTENT-TYPE"]),
    Headers2 = case HasContentType of
		   true -> Headers;
		   false -> [{"Content-Type", "text/html"}|Headers]
	       end,

    case Res#response.data of
        {data, Body} ->
            % Send the mochiweb response...
            Req:respond({Code, Headers2, Body});
        {file, Path} ->
            %% Create the response telling Mochiweb to serve the file...
            Req:serve_file(tl(Path), DocRoot, Headers2)
    end.

create_cookie_header(Cookie) ->
    SecondsToLive = Cookie#cookie.minutes_to_live * 60,
    Name = Cookie#cookie.name,
    Value = Cookie#cookie.value,
    Path = Cookie#cookie.path,
    mochiweb_cookies:cookie(Name, Value, [{path, Path}, {max_age, SecondsToLive}]).
