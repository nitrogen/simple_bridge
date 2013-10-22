% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge).
-export ([
	make/3,
	behaviour_info/1
]).

-include("simple_bridge.hrl").

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

%% TODO, CONVERT ALL THESE TO -callback
behaviour_info(callbacks) -> [
    {init, 1},           % Should accept the request value passed by the http server.

    {protocol, 1},       % http | https | undefined
    {request_method, 1}, % GET, POST, etc.
    {uri, 1},            % The uri (path and querystring)
    {path, 1},           % Just the path. (http://server.com/<PATH>?querystring)

    {headers, 1},        % Return a proplist of headers, key and value are strings.
    {cookies, 1},        % Return a proplist of cookies, key and value are strings.
    {query_params, 1},   % Return a proplist of query parameters, key and value are strings.
    {post_params, 1},    % Return a proplist of post parameters, key and value are strings.

    {peer_ip, 1},        % The remote IP address
    {protocol_version, 1}, % HTTP version, {High, Low}

	{build_response, 2}	%% Build the response
];

behaviour_info(_) -> undefined.
