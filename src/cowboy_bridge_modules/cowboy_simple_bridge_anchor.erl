%% vim: ts=4 sw=4 et

-module(cowboy_simple_bridge_anchor).
-export([
        init/3,
        handle/2,
        terminate/3,
        websocket_init/3,
        websocket_handle/3,
        websocket_info/3,
        websocket_terminate/3
    ]).

-record(ws_state, {handler, keepalive_interval, bridge, state}).

init(_Transport, Req, _Opts) ->
    {Upgrade, _} = cowboy_req:header(<<"upgrade">>, Req),
    Upgrade2 = case Upgrade of
        undefined -> undefined;
        Other -> simple_bridge_util:binarize_header(Other)
    end,

    case Upgrade2 == <<"websocket">> of
        true ->
            {upgrade, protocol, cowboy_websocket};
        false ->
           {ok, Req, []}
   end.

handle(Req, State) ->
    DocRoot = simple_bridge_util:get_env(document_root),
    Handler = simple_bridge_util:get_env(handler),
    Bridge = simple_bridge:make(cowboy, {Req, DocRoot}),
    {ok, NewReq} = Handler:run(Bridge),
    {ok, NewReq, State}.

terminate(_Reason, _Req, _State) ->
    ok.

websocket_init(_Transport, Req, _Opts) ->
    %% Init stuff
    {ok, Handler} = application:get_env(simple_bridge, handler),
    Bridge = simple_bridge:make(cowboy, {Req, ""}),
    UserState = simple_bridge_websocket:call_init(Handler, Bridge),

    %% Keepalive stuff
    {KAInterval, KATimeout} = simple_bridge_util:get_websocket_keepalive_interval_timeout(cowboy),
    CowboyTimeout = simple_bridge_websocket:keepalive_timeout(KAInterval, KATimeout),
    simple_bridge_websocket:schedule_keepalive_msg(KAInterval),

    %% START YOUR ENGINES, FOLKS!
    WSState = #ws_state{bridge=Bridge,
                        keepalive_interval=KAInterval,
                        handler=Handler,
                        state=UserState},
    {ok, Req, WSState, CowboyTimeout}.

websocket_handle({ping, _Data}, Req, WSState) ->
    %% We don't need to pong, cowboy does that automatically. So just carry on!
    {ok, Req, WSState};
websocket_handle({pong, _PongMsg}, Req, WSState) ->
    {ok, Req, WSState};
websocket_handle(Data, Req, WSState) ->
    #ws_state{handler=Handler, bridge=Bridge, state=State} = WSState,
    Result = Handler:ws_message(Data, Bridge, State),
    massage_reply(Result, Req, WSState).

websocket_info(simple_bridge_send_ping, Req, WSState=#ws_state{keepalive_interval=KAInterval}) ->
    simple_bridge_websocket:schedule_keepalive_msg(KAInterval),
    {reply, {ping, <<"Simple Bridge Ping">>}, Req, WSState};
    
websocket_info(Data, Req, WSState) ->
    #ws_state{handler=Handler, bridge=Bridge, state=State} = WSState,
    Result = Handler:ws_info(Data, Bridge, State),
    massage_reply(Result, Req, WSState).

websocket_terminate(Reason, _Req, #ws_state{bridge=Bridge, handler=Handler, state=State}) ->
    ok = Handler:ws_terminate(Reason, Bridge, State).


%% MASSAGE_REPLY reformats a simple_bridge return value into something that can
%% be handled by cowboy.
massage_reply({reply, {Type, Data}, NewState}, Req, WSState)
        when Type==binary orelse Type==text ->
    {reply, {Type, iolist_to_binary(Data)}, Req, WSState#ws_state{state=NewState}};
massage_reply({reply, List, NewState}, Req, WSState) ->
    FixedList = [{Type, iolist_to_binary(Data)} || {Type, Data} <- List],
    {reply, FixedList, Req, WSState#ws_state{state=NewState}};
massage_reply({reply, Reply}, Req, WSState) ->
    massage_reply({reply, Reply, WSState#ws_state.state}, Req, WSState);
massage_reply({noreply, NewState}, Req, WSState) ->
    {ok, Req, WSState#ws_state{state=NewState}};
massage_reply(noreply, Req, WSState) ->
    {ok, Req, WSState};
massage_reply({close, _Reason}, Req, WSState) ->
    {shutdown, Req, WSState};
massage_reply(close, Req, WSState) ->
    {shutdown, Req, WSState}.
