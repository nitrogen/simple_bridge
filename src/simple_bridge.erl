% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge).
-export ([
	start/0,
	start/1,
	start/2,
	make/2,
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
-callback build_response(any(), #response{}) 	-> any().

start() ->
	start(undefined).

start(BridgeType) ->
	start(BridgeType, undefined).

start(BridgeType, undefined) ->
	{ok, Callout} = application:get_env(callout),
	start(BridgeType, Callout);
start({supervisor, Supervisor}, Callout) ->
	application:load(simple_bridge),
	application:set_env(callout, Callout),
	Supervisor:start_link();
start({backend, Backend}, Callout) ->
	Supervisor = make_supervisor_module(Backend),
	start({supervisor, Supervisor}, Callout);
start(Backend, Callout) when is_atom(Backend) ->
	start({backend, Backend}, Callout).

make_supervisor_module(Backend) ->
	list_to_atom(atom_to_list(Backend) ++ "_simple_bridge_sup").


make(BridgeType, Req) ->
	make(BridgeType, Req, []).

make(BridgeType, Req, _DocRoot) ->
	Module = make_bridge_module(BridgeType),
	inner_make(Module, Req).
	
make_bridge_module(BridgeType) ->
	list_to_atom(atom_to_list(BridgeType) ++ "_simple_bridge").

inner_make(Module, RequestData) ->
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

