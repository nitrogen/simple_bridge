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

init(_Handler = Handler) -> 
    {ok, Handler}.

allowed_methods(Req, Handler) -> 
    {['HEAD', 'GET', 'POST'], Req, Handler}.

post_is_create(Req, Handler) -> 
    {false, Req, Handler}.

to_html(Req, Handler) ->
    {ok, Data, Req1} = do_bridge(Handler, Req),
    {Data, Req1, Handler}.

process_post(Req, Handler) ->
    {ok, Data, Req1} = do_bridge(Handler, Req),
    Req2 = wrq:set_resp_body(Data, Req1),
    {true, Req2, Handler}.

do_bridge(Handler, Req) ->
	Bridge = simple_bridge:make(webmachine, Req),
	case simple_bridge_websocket:attempt_hijacking(Bridge, Handler) of
		{hijacked, closed} -> gen_tcp:close(Bridge:socket());
		{hijacked, Bridge2} -> Bridge2:build_response();
		spared -> Handler:run(Bridge)
	end.
