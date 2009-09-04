% Simple Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge_response).
-include ("simplebridge.hrl").
-export ([
	make/2,
	behaviour_info/1
]).

make(Mod, Req) ->
	simple_bridge_response_wrapper:new(Mod, Req, #response{}).

behaviour_info(callbacks) -> [
	{build_response, 2} 
];

behaviour_info(_) -> ok.