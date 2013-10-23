%% vim: ts=4 sw=4 et
-module(yaws_simple_bridge_anchor).
-export([
    out/1,
    init/1,
    handle_message/2,
    handle_info/2,
    terminate/2
]).

out(Arg) ->
    Bridge = simple_bridge:make(yaws, Arg),
    Callout = simple_bridge_util:get_env(callout),
    Upgrade = string:to_lower(simple_bridge_util:to_list(Bridge:header(<<"Upgrade">>))),
    io:format("Upgrade Header: ~p~n", [Upgrade]),
    case Upgrade of
        "websocket" ->
            %% Pass the Callout module and the Bridge as the initial State but
            %% use the current module as the actual Websocket handler
            Opts = [{callback, {basic, {Callout, Bridge}}}],
            {websocket, ?MODULE, Opts};
        _ ->
            Callout:run(Bridge)
    end.

init([_Arg, State={Callout, Bridge}]) ->
    Callout:ws_init(Bridge),
    {ok, State}.

handle_message(Data, State={Callout, Bridge}) ->
    io:format("Message: ~p, Callout: ~p~n",[Data, Callout]),
    Result = Callout:ws_message(Data, Bridge),
    simple_bridge_util:massage_websocket_reply(Result, State).

handle_info(Data, State={Callout, Bridge}) ->
    Result = Callout:ws_info(Data, Bridge),
    simple_bridge_util:massage_websocket_reply(Result, State).

terminate(Reason, {Callout, Bridge}) ->
    ok = Callout:ws_terminate(Reason, Bridge).
