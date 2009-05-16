% Simple Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (inets_response_bridge).
-behaviour (response_bridge).
-include ("simplebridge.hrl").
-export ([build_response/1]).

build_response(Res) ->	
	ResponseCode = Res#response.statuscode,
	Data = Res#response.data,
	Size = integer_to_list(httpd_util:flatlength(Data)),
	
	% Assemble headers...
	Headers = lists:flatten([
		{code, ResponseCode},
		{content_length, Size},
		[{X#header.name, X#header.value} || X <- Res#response.headers],
		[create_cookie_header(X) || X <- Res#response.cookies]
	]),		

	% Send the inets response...
	{break,[
		{response, {response, Headers, Data}}
	]}.

create_cookie_header(Cookie) ->
	SecondsToLive = Cookie#cookie.minutes_to_live * 60,
	Expire = to_cookie_expire(SecondsToLive),
	Name = Cookie#cookie.name,
	Value = Cookie#cookie.value,
	Path = Cookie#cookie.path,
	{"Set-Cookie", io_lib:format("~s=~s; Path=~s; Expires=~s", [Name, Value, Path, Expire])}.

to_cookie_expire(SecondsToLive) ->
	Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
	httpd_util:rfc1123_date(DateTime).
