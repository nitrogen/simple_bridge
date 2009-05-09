% Simple Erlang Web Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (request_bridge).
-export ([
	make/2, 
	behaviour_info/1
]).

make(Module, Req) -> 
	Req1 = Module:init(Req),
	request_bridge_wrapper:new(Module, Req1).

behaviour_info(callbacks) -> [
	% Init
	{init, 1},
	
	% GET, POST, etc.
	{request_method, 1},
	
	% URL PARTS
	% protocol://hostname:port/path/pathinfo?query
	{url, 1},
	{protocol, 1},
	{hostname, 1},
	{port, 1},
	{path, 1},
	{querystring, 1},
		
	% % Data Sent from Client
	% {headers, 1},
	% {cookies, 1},
	% {query_params, 1},
	% {post_params, 1},
	% {request_body, 2},

	% Client Information
	{peer_ip, 1},
	{peer_port, 1}
];

behaviour_info(_) -> undefined.