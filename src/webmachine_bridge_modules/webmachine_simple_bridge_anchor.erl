-module(webmachine_simple_bridge_anchor).
-include("simple_bridge.hrl").
-export([
    init/1, 
    to_html/2, 
    allowed_methods/2,
    post_is_create/2,
    process_post/2,
	ping/2
]).


%% Resource Functions %%

ping(Req, State) ->
	{pong, Req, State}.

init(_Callout = Callout) -> 
    {ok, Callout}.

allowed_methods(Req, Callout) -> 
    {['HEAD', 'GET', 'POST'], Req, Callout}.

post_is_create(Req, Callout) -> 
    {false, Req, Callout}.

to_html(Req, Callout) ->
    {ok, Data, Req1} = do_bridge(Callout, Req),
    {Data, Req1, Callout}.

process_post(Req, Callout) ->
    {ok, Data, Req1} = do_bridge(Callout, Req),
    Req2 = wrq:set_resp_body(Data, Req1),
    {true, Req2, Callout}.

do_bridge(Callout, Req) ->
	Bridge = simple_bridge:make(webmachine, Req),
	case simple_bridge_websocket:attempt_hijacking(Bridge, Callout) of
		{hijacked, closed} -> gen_tcp:close(Bridge:socket());
		{hijacked, Bridge2} -> Bridge2:build_response();
		spared -> Callout:run(Bridge)
	end.
