% Simple Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (mochiweb_request_bridge).
-behaviour (simple_bridge_request).
-include ("simplebridge.hrl").
-export ([
	init/1,
	request_method/1, path/1,
	peer_ip/1, peer_port/1,
	headers/1, cookies/1,
	query_params/1, post_params/1, request_body/1
]).

-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).

init({Req, DocRoot}) -> 
	{Req, DocRoot}.

request_method({Req, _DocRoot}) -> 
	Req:get(method).

path({Req, _DocRoot}) -> 
	RawPath = Req:get(raw_path),
	{Path, _, _} = mochiweb_util:urlsplit_path(RawPath),
	Path.

peer_ip({Req, _DocRoot}) -> 
	Socket = Req:get(socket),
	{ok, {IP, _Port}} = inet:peername(Socket),
	IP.
	
peer_port({Req, _DocRoot}) -> 
	Socket = Req:get(socket),
	{ok, {_IP, Port}} = inet:peername(Socket),
	Port.
	
headers({Req, _DocRoot}) ->
	Req:get(headers).
	
cookies({Req, _DocRoot}) ->
	Req:parse_cookie().
	
query_params({Req, _DocRoot}) ->
	Req:parse_qs().
	
post_params({Req, _DocRoot}) ->
	Req:parse_post().

request_body({_Req, _DocRoot}) -> 
	[].