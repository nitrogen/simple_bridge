% Simple Erlang Web Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.


-module (response_bridge).
-include ("simplebridge.inc").
-export ([
	make/1,
	behaviour_info/1
]).

make(Mod) ->
	response_bridge_wrapper:new(Mod, #response{}).

behaviour_info(callbacks) -> [
	{build_response, 0} 
];

behaviour_info(_) -> ok.