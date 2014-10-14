%% vim: ts=4 sw=4 et
-module(yaws_simple_bridge_anchor).
-export([
    out/1,
    init/1,
    handle_message/2,
    handle_info/2,
    terminate/2
]).

-record(ws_state, {handler, bridge, state}).

out(Arg) ->
    Bridge = simple_bridge:make(yaws, Arg),
    Handler = simple_bridge_util:get_env(handler),
    Upgrade = sbw:header_lower(upgrade, Bridge),
    {KAInterval, KATimeout} = simple_bridge_util:get_websocket_keepalive_interval_timeout(yaws),
    case Upgrade of
        "websocket" ->
            %% Pass the Handler module and the Bridge as the initial State but
            %% use the current module as the actual Websocket handler
            Opts = setup_websocket_opts(Handler, Bridge, KAInterval, KATimeout),
            {websocket, ?MODULE, Opts};
        _ ->
            Handler:run(Bridge)
    end.

setup_websocket_opts(Handler, Bridge, KAInterval, KATimeout) ->
    KeepAlive = is_integer(KAInterval) andalso KAInterval > 0,
    [
        %% Standard Yaws websocket handler.
        {callback, {basic, #ws_state{handler=Handler, bridge=Bridge}}},

        %% Boolean turning on or off keepalive pings
        {keepalive, KeepAlive},

        %% The interval to send keepalive pings (even though it's
        %% labeled 'keepalive_timeout').
        {keepalive_timeout, KAInterval},

        %% The actual timeout. If we don't hear back about our ping in
        %% KATimeout milliseconds, yaws will kill the connection.
        {keepalive_grace_period, KATimeout}
    ].

init([_Arg, WSState=#ws_state{handler=Handler, bridge=Bridge}]) ->
    NewState = simple_bridge_websocket:call_init(Handler, Bridge),
    {ok, WSState#ws_state{state=NewState}}.

handle_message({close, Status, Reason}, State) ->
    {close, {Status, Reason}, State};

handle_message(Data, WSState=#ws_state{handler=Handler, bridge=Bridge, state=State}) ->
    Result = Handler:ws_message(Data, Bridge, State),
    massage_reply(Result, WSState).

handle_info(Data, WSState=#ws_state{handler=Handler, bridge=Bridge, state=State}) ->
    case erlang:function_exported(Handler, ws_info, 3) of
        true -> 
            Result = Handler:ws_info(Data, Bridge, State),
            massage_reply(Result, WSState);
        false ->
            {noreply, WSState}
    end.

terminate(Reason, #ws_state{handler=Handler, bridge=Bridge, state=State}) ->
    ok = Handler:ws_terminate(Reason, Bridge, State).

massage_reply(noreply, WSState) ->
    {noreply, WSState};
massage_reply({noreply, NewState}, WSState) ->
    {noreply, WSState#ws_state{state=NewState}};
massage_reply({reply, Reply}, WSState) ->
    massage_reply({reply, Reply, WSState#ws_state.state}, WSState);
massage_reply({reply, {Type, Data}, NewState}, WSState)
        when Type==binary; Type==text ->
    {reply, {Type, iolist_to_binary(Data)}, WSState#ws_state{state=NewState}};
massage_reply({reply, List, NewState}, WSState) ->
    FixedList = [{Type, iolist_to_binary(Data)} || {Type, Data} <- List],
    {reply, FixedList, WSState#ws_state{state=NewState}};
massage_reply({close, Reason}, WSState) when is_integer(Reason) ->
    {close, Reason, WSState};
massage_reply(close, WSState) ->
    {close, 1000, WSState}.
