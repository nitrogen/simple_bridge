% Simple Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (response_bridge_wrapper, [Mod, Res]).
-include ("simplebridge.hrl").
-compile(export_all).

status_code(StatusCode) -> 
	Res1 = Res#response { statuscode=StatusCode },
	response_bridge_wrapper:new(Mod, Res1).

header(Name, Value) -> 
	Header = #header { name=Name, value=Value },
	Headers = Res#response.headers,
	Headers1 = [X || X <- Headers, X#header.name /= Name],
	Headers2 = [Header|Headers1],
	Res1 = Res#response { headers=Headers2 },
	response_bridge_wrapper:new(Mod, Res1).	
	
clear_headers() -> 
	Res1 = Res#response { headers=[] },
	response_bridge_wrapper:new(Mod, Res1).

cookie(Name, Value) ->
	cookie(Name, Value, "/", 20).

cookie(Name, Value, Path, MinutesToLive) -> 
	Cookie = #cookie { name=Name, value=Value, path=Path, minutes_to_live=MinutesToLive },
	Cookies = Res#response.cookies,
	Cookies1 = [X || X <- Cookies, X#cookie.name /= Name],
	Cookies2 = [Cookie|Cookies1],
	Res1 = Res#response { cookies=Cookies2 },
	response_bridge_wrapper:new(Mod, Res1).	

clear_cookies() -> 
	Res1 = Res#response { cookies=[] },
	response_bridge_wrapper:new(Mod, Res1).

data(Data) -> 
	Res1 = Res#response { data=Data },
	response_bridge_wrapper:new(Mod, Res1).

build_response() -> Mod:build_response(Res).