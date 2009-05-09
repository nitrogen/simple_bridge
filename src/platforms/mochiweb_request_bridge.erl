% Simple Erlang Web Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (mochiweb_request_bridge).
-behaviour (request_bridge).
-export ([
	init/1,
	request_method/1, url/1, protocol/1, hostname/1, port/1, path/1, querystring/1,
	peer_ip/1, peer_port/1
]).


init(Req) -> 
	Req.

request_method(Req) -> 
	Req:get(method).

url(_Req) -> 
	ok.
	
protocol(_Req) -> 
	ok.
	
hostname(_Req) -> 
	ok.
	
port(_Req) -> 
	ok.
	
path(_Req) -> 
	ok.

querystring(Req) -> 
	Req = wf_platform:get_request(),
	RawPath = Req:get(raw_path),
	{_, QueryString, _} = mochiweb_util:urlsplit_path(RawPath),
	QueryString.

peer_ip(_Req) -> 
	ok.
	
peer_port(_Req) -> 
	ok.