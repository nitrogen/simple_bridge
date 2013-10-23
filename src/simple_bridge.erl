% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge).
-export ([
	start/1,
	make/3
]).

-include("simple_bridge.hrl").


-callback init(bridge()) 						-> bridge().
-callback protocol(bridge()) 					-> http | https | ws | wss | undefined.
-callback request_method(bridge()) 				-> 'GET' | 'POST' | 'DELETE' | atom().
-callback uri(bridge()) 						-> binary().
-callback path(bridge()) 						-> binary().
-callback headers(bridge()) 					-> [{key(), value()}].
-callback query_params(bridge()) 				-> [{key(), value()}].
-callback post_params(bridge())					-> [{key(), value()}].
-callback peer_ip(bridge())						-> ipv4() | ipv8().
-callback protocol_version(bridge())			-> {integer(), integer()}.
-callback build_response(any(), #response{}) 	-> iolist().

start(BridgeType) ->
	Sup = list_to_atom(atom_to_list(BridgeType) ++ "_simple_bridge_sup"),
	%% Let's just load it in case it hasn't been yet
	application:load(simple_bridge),
	Sup:start_link().

-spec make(bridge_type(), Req :: any(), DocRoot :: string()) -> bridge().
make(BridgeType, Req, _DocRoot) ->
	Module = make_module(BridgeType),
	make(Module, Req).
	
make_module(BridgeType) ->
	list_to_atom(atom_to_list(BridgeType) ++ "_simple_bridge").

make(Module, RequestData) ->
    try
        make_nocatch(Module, RequestData)
    catch Type : Error ->
        error_logger:error_msg("Error in simple_bridge:make/2 - ~p - ~p~n~p", [Type, Error, erlang:get_stacktrace()]),
        erlang:Type(Error)
    end.

make_nocatch(Module, RequestData) -> 
    RequestData1 = Module:init(RequestData),
 	   Bridge = simple_bridge_wrapper:new(Module, RequestData1, false, [], [], none),
    case simple_bridge_multipart:parse(Bridge) of
        {ok, Params, Files} -> 
            Bridge:set_multipart(Params, Files);
        {ok, not_multipart} -> 
            Bridge;
        {error, Error} -> 
            Bridge:set_error(Error);
        Other -> 
            throw({unexpected, Other})
    end.

