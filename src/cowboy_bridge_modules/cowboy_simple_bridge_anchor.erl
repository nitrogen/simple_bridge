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
    {ok, Handler} = application:get_env(simple_bridge, handler),
    Bridge = simple_bridge:make(cowboy, {Req, ""}),
    ok = Handler:ws_init(Bridge),
    {ok, Req, {Bridge, Handler}}.

websocket_handle({ping, _Data}, Req, State) ->
    %% We don't need to pong, cowboy does that automatically. So just carry on!
    {ok, Req, State};
websocket_handle({pong, _}, Req, State) ->
    {ok, Req, State};
websocket_handle(Data, Req, State={Bridge, Handler}) ->
    Result = Handler:ws_message(Data, Bridge),
    Reply = massage_reply(Result, Req, State),
    Reply.

websocket_info(Data, Req, State={Bridge, Handler}) ->
    Result = Handler:ws_info(Data, Bridge),
    Reply = massage_reply(Result, Req, State),
    Reply.

websocket_terminate(Reason, _Req, {Bridge, Handler}) ->
    ok = Handler:ws_terminate(Reason, Bridge).

massage_reply({reply, {Type, Data}}, Req, State)
        when Type==binary orelse Type==text ->
    {reply, {Type, iolist_to_binary(Data)}, Req, State};
massage_reply({reply, List}, Req, State) ->
    FixedList = [{Type, iolist_to_binary(Data)} || {Type, Data} <- List],
    {reply, FixedList, Req, State};
massage_reply(noreply, Req, State) ->
    {ok, Req, State};
massage_reply(close, Req, State) ->
    {shutdown, Req, State}.
