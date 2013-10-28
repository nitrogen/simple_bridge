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

start(Backend) ->
	start(Backend, undefined).


start(Backend, Callout) when is_atom(Backend) ->
	application:load(simple_bridge),
	Callout2 = case Callout of
		undefined ->
			simple_bridge_util:get_env(callout);
		_ ->
			application:set_env(simple_bridge, callout, Callout),
			Callout
	end,
	Backend2 = case Backend of
		undefined ->
			simple_bridge_util:get_env(backend);
		_ ->
			application:set_env(simple_bridge, backend, Backend),
			Backend
	end,

	case {Callout2, Backend2} of
		{undefined, _} -> throw("No backend defined for simple_bridge.");
		{_, undefined} -> io:format("*** Warning: No callout module defined for simple_bridge. If this intentional,~n*** if you are using a custom dispatch table, for example), then this message~n*** can be safely ignored.");
		{_,_} -> ok
	end,

	Supervisor = make_supervisor_module(Backend2),
	Supervisor:start_link().

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

