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
    Handler = simple_bridge_util:get_env(handler),
    Upgrade = sbw:header_lower(upgrade, Bridge),
    case Upgrade of
        "websocket" ->
            %% Pass the Handler module and the Bridge as the initial State but
            %% use the current module as the actual Websocket handler
            Opts = [{callback, {basic, {Handler, Bridge}}}],
            {websocket, ?MODULE, Opts};
        _ ->
            Handler:run(Bridge)
    end.

init([_Arg, State={Handler, Bridge}]) ->
    Handler:ws_init(Bridge),
    {ok, State}.

handle_message({close, Status, Reason}, State) ->
    {close, {Status, Reason}, State};

handle_message(Data, State={Handler, Bridge}) ->
    Result = Handler:ws_message(Data, Bridge),
    Reply = massage_reply(Result, State),
    Reply.

handle_info(Data, State={Handler, Bridge}) ->
    Result = Handler:ws_info(Data, Bridge),
    Reply = massage_reply(Result, State),
    Reply.

terminate(Reason, {Handler, Bridge}) ->
    ok = Handler:ws_terminate(Reason, Bridge).

massage_reply(noreply, State) ->
    {noreply, State};
massage_reply({reply, {Type, Data}}, State)
        when Type==binary; Type==text ->
    {reply, {Type, iolist_to_binary(Data)}, State};
massage_reply({reply, List}, State) ->
    FixedList = [{Type, iolist_to_binary(Data)} || {Type, Data} <- List],
    {reply, FixedList, State};
massage_reply(close, State) ->
    {close, 1000, State}.
