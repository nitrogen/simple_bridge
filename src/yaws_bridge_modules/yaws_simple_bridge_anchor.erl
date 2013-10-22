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
    Upgrade = string:to_lower(Bridge:header(upgrade)),
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

handle_message(Data, {Callout, Bridge}) ->
    _Result = Callout:ws_message(Data, Bridge).

handle_info(Data, {Callout, Bridge}) ->
    _Result = Callout:ws_info(Data, Bridge).

terminate(Reason, {Callout, Bridge}) ->
    ok = Callout:ws_close(Reason, Bridge).
