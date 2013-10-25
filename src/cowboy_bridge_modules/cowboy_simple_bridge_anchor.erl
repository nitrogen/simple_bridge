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

    case Upgrade2 == <<"Websocket">> of
        true ->
            {upgrade, protocol, cowboy_websocket};
        false ->
           {ok, Req, []}
   end.

handle(Req, State) ->
    DocRoot = simple_bridge_util:get_env(document_root),
    Callout = simple_bridge_util:get_env(callout),
    Bridge = simple_bridge:make(cowboy, {Req, DocRoot}),
    {ok, NewReq} = Callout:run(Bridge),
    {ok, NewReq, State}.

terminate(_Reason, _Req, _State) ->
    ok.

websocket_init(_Transport, Req, _Opts) ->
    {ok, Callout} = application:get_env(simple_bridge, callout),
    Bridge = simple_bridge:make(cowboy, {Req, ""}),
    ok = Callout:ws_init(Bridge),
    {ok, Req, {Bridge, Callout}}.

websocket_handle(Data, Req, State={Bridge, Callout}) ->
    Result = Callout:ws_message(Data, Bridge),
    {reply, ReplyData} = simple_bridge_util:massage_websocket_reply(Result, State),
    {reply, ReplyData, Req, State}.

websocket_info(Data, Req, State={Bridge, Callout}) ->
    Result = Callout:ws_info(Data, Bridge),
    {reply, ReplyData} = simple_bridge_util:massage_websocket_reply(Result, State),
    {reply, ReplyData, Req, State}.

websocket_terminate(Reason, _Req, {Bridge, Callout}) ->
    ok = Callout:ws_terminate(Reason, Bridge).
