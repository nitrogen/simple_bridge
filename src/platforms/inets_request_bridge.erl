% Simple Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (inets_request_bridge).
-behaviour (request_bridge).
-include ("httpd_r12b5.hrl").
-export ([
	init/1,
	request_method/1, path/1, querystring/1,
	peer_ip/1, peer_port/1
]).


-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).

init(Req) -> 
	Req.

request_method(Req) -> 
	list_to_atom(Req#mod.method).

path(Req) -> 
	{Path, _QueryString} = split_request_uri(Req#mod.request_uri, []),
	Path.

querystring(Req) -> 
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
	


split_request_uri([], Path) -> {lists:reverse(Path), ""};
split_request_uri([$?|QueryString], Path) -> {lists:reverse(Path), QueryString};
split_request_uri([H|T], Path) -> split_request_uri(T,[H|Path]).
	