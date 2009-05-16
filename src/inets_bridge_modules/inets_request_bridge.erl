% Simple Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (inets_request_bridge).
-behaviour (request_bridge).
-include ("httpd_r12b5.hrl").
-include ("simplebridge.hrl").
-export ([
	init/1,
	request_method/1, path/1, query_string/1,
	peer_ip/1, peer_port/1,
	headers/1, cookies/1,
	query_params/1, post_params/1, request_body/1
]).

-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).

init(Req) -> 
	Req.

request_method(Req) -> 
	list_to_atom(Req#mod.method).

path(Req) -> 
	{Path, _QueryString} = split_request_uri(Req#mod.request_uri, []),
	Path.

query_string(Req) -> 
	{_Path, QueryString} = split_request_uri(Req#mod.request_uri, []),
	QueryString.

peer_ip(Req) -> 
	Socket = Req#mod.socket,
	{ok, {IP, _Port}} = inet:peername(Socket),
	IP.
	
peer_port(Req) -> 
	Socket = Req#mod.socket,
	{ok, {_IP, Port}} = inet:peername(Socket),
	Port.
	
headers(Req) ->
	Req#mod.parsed_header.
	
cookies(Req) ->
	Headers = headers(Req),
	CookieData = proplists:get_value("cookie", Headers, ""),
	F = fun(Cookie) ->
		case string:tokens(Cookie, "=") of
			[] -> [];
			L -> 
				X = string:strip(hd(L)),
				Y = string:join(tl(L), "="),
				{X, Y}
		end
	end,
	[F(X) || X <- string:tokens(CookieData, ";")].
	
query_params(Req) ->
	QueryString = query_string(Req),
	Query = httpd:parse_query(QueryString),
	[{Key, Value} || {Key, Value} <- Query, Key /= []].
	
post_params(Req) ->
	Body = request_body(Req),
	Query = httpd:parse_query(Body),
	[{Key, Value} || {Key, Value} <- Query, Key /= []].

request_body(Req) ->
	Req#mod.entity_body.

split_request_uri([], Path) -> {lists:reverse(Path), ""};
split_request_uri([$?|QueryString], Path) -> {lists:reverse(Path), QueryString};
split_request_uri([H|T], Path) -> split_request_uri(T,[H|Path]).
	