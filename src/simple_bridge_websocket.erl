%% vim: ts=4 sw=4 et
-module(simple_bridge_websocket).
-export([
        attempt_hijacking/2,

        %% These three are used by cowboy or yaws and should basically never be
        %% called except from within the simple_bridge app
        call_init/2,
        keepalive_timeout/2,
        schedule_keepalive_msg/1,

        %% Exported for code-reloading
        websocket_loop/8
    ]).

%-compile(export_all).
-include("simple_bridge.hrl").
-include("crypto_compat.hrl").

-define(else, true).

-define(WS_MAGIC, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").
-define(WS_VERSION, "13").

-define(WS_MASKED, 1).
-define(WS_UNMASKED, 0).

-define(WS_EXTENDED_PAYLOAD_16BIT, 126).
-define(WS_EXTENDED_PAYLOAD_64BIT, 127).

-define(WS_CONTINUATION, 0).
-define(WS_TEXT, 1).
-define(WS_BINARY, 2).
-define(WS_CLOSE, 8).
-define(WS_PING, 9).
-define(WS_PONG, 10).

-define(IS_INVALID_OPCODE(Op),
    not(Op=:=?WS_CONTINUATION orelse
        Op=:=?WS_TEXT orelse
        Op=:=?WS_BINARY orelse
        Op=:=?WS_CLOSE orelse
        Op=:=?WS_PING orelse
        Op=:=?WS_PONG)).



-record(frame, {fin=1, rsv=0, opcode, masked=0, payload_len=0, mask_key, data = <<>>}).
-record(partial_data, {data = <<>>, message_frames=[]}).

-spec attempt_hijacking(bridge(), Handler :: atom()) -> spared | {hijacked, closed} | {hijacked, bridge()}.
attempt_hijacking(Bridge, Handler) ->
    ProtocolVersion = sbw:protocol_version(Bridge),
    UpgradeHeader = sbw:header_lower(upgrade, Bridge),
    WSVersionHead = sbw:header("Sec-WebSocket-Version", Bridge),
    ConnectionHeaderHasUpgrade = does_connection_header_have_upgrade(Bridge),
    if
        ProtocolVersion     >= {1,1},
        UpgradeHeader       =:= "websocket",
        ConnectionHeaderHasUpgrade,
        WSVersionHead       =/= undefined ->
            WSVersions = re:split(WSVersionHead, "[, ]+]", [{return, list}]),
            HijackedBridge = case lists:member(?WS_VERSION, WSVersions) of
                true ->
                    hijack(Bridge, Handler);
                false ->
                    hijack_request_fail(Bridge)
            end,
            {hijacked, HijackedBridge};
        ?else ->
            spared      %% Spared from being hijacked
    end.

does_connection_header_have_upgrade(Bridge) ->
    case sbw:header_lower(connection, Bridge) of
        undefined ->
            false;
        ConnectionHeader ->
            case re:run(ConnectionHeader, "upgrade") of
                nomatch -> false;
                {match, _} -> true
            end
    end.


call_init(Handler, Bridge) ->
    case erlang:function_exported(Handler, ws_init, 1) of
        true ->
            case Handler:ws_init(Bridge) of
                ok -> undefined;
                {ok, State} -> State
            end;
        false -> undefined
    end.

keepalive_timeout(infinity, _) -> infinity;
keepalive_timeout(KAInterval, KATimeout) when is_integer(KAInterval), is_integer(KATimeout)->
    %% For a timeout at least a message every X milliseconds, so we add
    %% Interval+Timeout, then trigger a ping every Interval seconds, that way,
    %% we get timeout milliseconds to either get a new message (pong or
    %% otherwise), and if we don't hear back from the ping within Timeout
    %% milliseconds, it will be the whole timeframe and the server will kill
    %% the connection.
    KAInterval + KATimeout.

schedule_keepalive_msg(infinity) ->
    ok;
schedule_keepalive_msg(KAInterval) ->
    timer:send_after(KAInterval, simple_bridge_send_ping).

cancel_pong_timer(undefined) ->
    ok;
cancel_pong_timer(TRef) ->
    {ok, cancel} = timer:cancel(TRef).

hijack_request_fail(Bridge) ->
    Bridge2 = sbw:set_status_code(400, Bridge),
    Bridge3 = sbw:set_header("Sec-Websocket-Version", ?WS_VERSION, Bridge2),
    Bridge4 = sbw:set_response_data(["Invalid Websocket Upgrade Request. Please use Websocket version ",?WS_VERSION], Bridge3),
    Bridge4.

prepare_response_key(WSKey) ->
    FullString = WSKey ++ ?WS_MAGIC,
    Sha = ?HASH(FullString),
    base64:encode(Sha).

hijack(Bridge, Handler) ->
    WSKey = sbw:header("Sec-Websocket-Key", Bridge),
    ResponseKey = prepare_response_key(WSKey),
    Socket = sbw:socket(Bridge),
    inet:setopts(Socket, [{buffer,65535}]),
    send_handshake_response(Socket, ResponseKey),
    inet:setopts(Socket, [{active, once}]),
    State = call_init(Handler, Bridge),
    Backend = simple_bridge_util:get_env(backend),
    {KAInterval, KATimeout} = simple_bridge_util:get_websocket_keepalive_interval_timeout(Backend),
    schedule_keepalive_msg(KAInterval),
    websocket_loop_init(Socket, Bridge, Handler, KAInterval, KATimeout, undefined, State, #partial_data{}).

send_handshake_response(Socket, ResponseKey) ->
    Handshake = [
                 <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                 <<"Upgrade: websocket\r\n">>,
                 <<"Connection: Upgrade\r\n">>,
                 <<"Sec-WebSocket-Accept: ">>,ResponseKey,<<"\r\n">>,
                 <<"\r\n">>
                ],
    gen_tcp:send(Socket, Handshake).

websocket_loop_init(Socket, Bridge, Handler, KAInterval, KATimeout, PongTimer, State, PartialData) ->
    try websocket_loop(Socket, Bridge, Handler, KAInterval, KATimeout, PongTimer, State, PartialData)
    catch
        exit:{websocket, ReasonCode, _Reason} ->
            send(Socket, {close, ReasonCode}),
            gen_tcp:close(Socket),
            %% cascade the error up
            exit(normal)
    end.

websocket_loop(Socket, Bridge, Handler, KAInterval, KATimeout, PongTimer, State, PartialData) ->
    receive 
        {tcp, Socket, Data} ->
            cancel_pong_timer(PongTimer),
            AttemptPacket = <<(PartialData#partial_data.data)/binary, Data/binary>>,
            Frames = parse_frame(AttemptPacket),
            
            PendingFrames = PartialData#partial_data.message_frames,
            case process_frames(Frames, Socket, Bridge, Handler, State, PendingFrames) of
                {PendingFrames2, RemainderData, NewState} ->
                    inet:setopts(Socket, [{active, once}]),
                    ?MODULE:websocket_loop(Socket, Bridge, Handler, KAInterval, KATimeout, undefined, NewState, #partial_data{data=RemainderData, message_frames=PendingFrames2});
                closed -> closed
            end;
        {tcp_closed, _Socket} ->
            closed;
        simple_bridge_pong_timeout ->
            %% If this message is received, it means no TCP message was
            %% received in the expected timeframe, so we kill the
            %% connection. Any TCP message received would have cancelled
            %% the timer.
            send(Socket, {close, 1006}),
            gen_tcp:close(Socket),
            closed;
        simple_bridge_send_ping ->
            Reply = {ping, <<"simple bridge websocket">>},
            schedule_keepalive_msg(KAInterval),
            send(Socket, Reply),
            {ok, NewPongTimer} = timer:send_after(KATimeout, simple_bridge_pong_timeout),
            ?MODULE:websocket_loop(Socket, Bridge, Handler, KAInterval, KATimeout, NewPongTimer, State, PartialData);
        Msg ->
            {Reply, NewState} = call_info(Handler, Bridge, Msg, State),
            send(Socket, Reply),
            ?MODULE:websocket_loop(Socket, Bridge, Handler, KAInterval, KATimeout, PongTimer, NewState, PartialData)
    end.

call_info(Handler, Bridge, Msg, State) ->
    case erlang:function_exported(Handler, ws_info, 3) of
        true ->
            HandlerReturn = Handler:ws_info(Msg, Bridge, State),
            {_Reply, _NewState} = extract_reply_state(State, HandlerReturn);
        false ->
            {noreply, State}
    end.

extract_reply_state(State, InfoMsgReturn) ->
    case InfoMsgReturn of
        noreply -> {noreply, State};
        {noreply, NewState} -> {noreply, NewState};
        {reply, Reply} -> {{reply, Reply}, State};
        {reply, Reply, NewState} -> {{reply, Reply}, NewState};
        close -> {close, 1000};
        {close, CloseReason} -> {close, CloseReason}
    end.

close_with_purpose(ReasonCode, Reason) ->
    exit({websocket, ReasonCode, Reason}).

send(_, noreply) ->
    do_nothing;
send(Socket, {ping, Data}) ->
    send_frame(Socket, #frame{opcode=?WS_PING, data=Data});
send(Socket, {pong, Data}) ->
    send_frame(Socket, #frame{opcode=?WS_PONG, data=Data});
send(Socket, close) ->
    send(Socket, {close, 1000});
send(Socket, {close, ReasonCode}) ->
    ReasonBody = <<ReasonCode:16>>,
    send_frame(Socket, #frame{opcode=?WS_CLOSE, data=ReasonBody});
send(Socket, {reply, {text, Data}}) ->
    send_frame(Socket, #frame{opcode=?WS_TEXT, data=Data});
send(Socket, {reply, {binary, Data}}) ->
    send_frame(Socket, #frame{opcode=?WS_BINARY, data=Data});
send(Socket, {reply, Fragments}) when is_list(Fragments) ->
    send_fragments(Socket, Fragments).
    
%%send(Socket, {stream, {binary, Fun}}) ->
%%    send_frame(#frame{opcode=?WS_BINARY, fin=0, data=Fu

send_fragments(_Socket, []) ->
    do_nothing;
%% If it's a list of one fragment, just send a self-contained frame
send_fragments(Socket, [H]) ->
    send(Socket, H);
send_fragments(Socket, [{binary,Data}|T]) ->
    send_frame(Socket, #frame{fin=0, opcode=?WS_BINARY, data=Data}),
    send_fragments_rest(Socket, T);
send_fragments(Socket, [{text,Data}|T]) ->
    send_frame(Socket, #frame{fin=0, opcode=?WS_TEXT, data=Data}),
    send_fragments_rest(Socket, T).

send_fragments_rest(Socket, [{_,Data}]) ->
    send_frame(Socket, #frame{fin=1, opcode=?WS_CONTINUATION, data=Data});
send_fragments_rest(Socket, [{_,Data}|T]) ->
    send_frame(Socket, #frame{fin=0, opcode=?WS_CONTINUATION, data=Data}),
    send_fragments_rest(Socket, T).
    

send_frame(Socket, F) ->
    BinFrame = encode_frame(F),
    gen_tcp:send(Socket, BinFrame).
    

encode_frame(#frame{
        fin=Fin, rsv=RSV, opcode=Opcode,
        %% commented because server should not mask. We can always implement if
        %% masking becomes necessary
        %% masked=Masked, mask_key=Mask,
        data=Data}) ->
    BinData = iolist_to_binary(Data),
    {PayloadLen, ExtLen, ExtBitSize} = case byte_size(BinData) of
        L when L < 126   -> {L, 0, 0};
        L when L < 65536 -> {126, L, 16};
        L                -> {127, L, 64}
    end,
    Masked = 0,
    <<Fin:1, RSV:3, Opcode:4, Masked:1, PayloadLen:7, ExtLen:ExtBitSize, BinData/binary>>.


%% This goes through each Frame in "Frames", and processes it, responding to
%% control processes, dispatching handlers if necessary, and if we're in the
%% middle of a series of fragments, store up those fragments and append them to
%% PendingFrames, or if a series of fragments is completed, then dispatch those
%% frames to the handler module and discard them.

%% Done processing frames
process_frames([], _Socket, _Bridge, _Handler, State, PendingFrames) ->
    {PendingFrames, <<>>, State};

%% Done processing frames, and we have some left-over binary data
process_frames([Bin], _Socket, _Bridge, _Handler, State, PendingFrames) when is_binary(Bin) ->
    {PendingFrames, Bin, State};

%% Handling erroneous Frams:
%% Control frames with payload > 126
process_frames([#frame{opcode=Ctl, payload_len=PayloadLen} | _], _Socket, _Bridge, _Handler, _State, _Pending) 
        when (Ctl=:=?WS_PING orelse Ctl=:=?WS_PONG orelse Ctl=:=?WS_CLOSE) andalso PayloadLen >= ?WS_EXTENDED_PAYLOAD_16BIT ->
    close_with_purpose(1002, {control_frame_payload_to_large, PayloadLen});
%% RSV bits set to anything but zero
process_frames([#frame{rsv=RSV} | _], _Socket, _Bridge, _Handler, _State, _Pending)
        when RSV =/= 0 ->
    close_with_purpose(1002, {invalid_rsv, RSV});
%% Invalid Opcode
process_frames([#frame{opcode=Op} | _], _Socket, _Bridge, _Handler, _State, _Pending)
        when ?IS_INVALID_OPCODE(Op) ->
    close_with_purpose(1002, {invalid_opcode, Op});


%% Single Text or Binary Frame (PendingFrames must be empty)
process_frames([_F = #frame{opcode=Opcode, fin=1, data=Data} |Rest], Socket, Bridge, Handler, State, []) 
        when Opcode=:=?WS_BINARY; Opcode=:=?WS_TEXT ->
    Type = type(Opcode),
    %% Side-effects, look out!
    close_on_invalid_utf8_text(Type, Data),
    HandlerReturn = Handler:ws_message({Type, Data}, Bridge, State),
    {Reply, NewState} = extract_reply_state(State, HandlerReturn),
    send(Socket, Reply),
    process_frames(Rest, Socket, Bridge, Handler, NewState, []);

%% First Text or Binary Fragment (PendingFrames must be empty)
process_frames([F = #frame{opcode=Opcode, fin=0}|Rest], Socket, Bridge, Handler, State, [])
        when Opcode=:=?WS_BINARY; Opcode=:=?WS_TEXT ->
    process_frames(Rest, Socket, Bridge, Handler, State, [F]);

%% Continuation Frame
process_frames([F = #frame{opcode=?WS_CONTINUATION, fin=0}|Rest], Socket, Bridge, Handler, State, PendingFrames=[_|_]) ->
    process_frames(Rest, Socket, Bridge, Handler, State, PendingFrames ++ [F]);

%% Last fragment of a fragmented message
process_frames([F = #frame{opcode=?WS_CONTINUATION, fin=1}|Rest], Socket, Bridge, Handler, State, PendingFrames=[_|_]) ->
    ReorderedFrames = PendingFrames ++ [F],
    Type = type((hd(ReorderedFrames))#frame.opcode),
    Msg = defragment_data(ReorderedFrames),
    %% Side-effects, look out!
    close_on_invalid_utf8_text(Type, Msg),
    HandlerReturn = Handler:ws_message({Type, Msg}, Bridge, State),
    {Reply, NewState} = extract_reply_state(State, HandlerReturn),
    send(Socket, Reply),
    process_frames(Rest, Socket, Bridge, Handler, NewState, []);

process_frames([_F = #frame{opcode=?WS_PONG}|Rest], Socket, Bridge, Handler, State, PendingFrames) ->
    %% do nothing?
    process_frames(Rest, Socket, Bridge, Handler, State, PendingFrames);

process_frames([#frame{opcode=?WS_PING, data=Data, fin=1}|Rest], Socket, Bridge, Handler, State, PendingFrames) ->
    send(Socket, {pong, Data}),
    process_frames(Rest, Socket, Bridge, Handler, State, PendingFrames);

process_frames([_F = #frame{opcode=?WS_CLOSE, data=Data}|_Rest], Socket, Bridge, Handler, State, _PendingFrames) ->
    StatusCode = case Data of
        <<_:8>> ->
            1002;
        <<ReasonCode:16,Text/binary>> ->
            case is_valid_close_code(ReasonCode) of
                true ->
                    case is_utf8(Text) of
                        true -> ReasonCode;
                        false -> 1007
                    end;
                false -> 1002
            end;
        _ -> 1000
    end,
            
    send(Socket, {close, StatusCode}),
    Handler:ws_terminate(closed, Bridge, State),
    inet:close(Socket),
    closed;

%% None of the above caught it. something must be wrong, so let's just die
process_frames([F|_], _Socket, _Bridge, _Handler, _State, _PendingFrames) ->
    close_with_purpose(1002, {unknown_error_processing_frame, F}).
    
defragment_data(Frames) ->
    iolist_to_binary([F#frame.data || F <- Frames]).

is_valid_close_code(Code) ->
    (Code >= 3000 andalso Code < 5000)
        orelse lists:member(Code, [1000, 1001, 1002, 1003, 1007, 1008, 1009, 1010, 1011]).

type(?WS_BINARY) -> binary;
type(?WS_TEXT) -> text.

-define(DO_FRAMES(Mask),
    if 
        byte_size(Data) >= PayloadLen ->
            do_frames(Fin,RSV,Op,PayloadLen,Mask,Data);
        ?else ->
            [Raw]
    end).

%% @doc Takes some binary data, assumed to be the beginning of a frame, and
%% tries to parse it into one or more frames. It will return either a list of
%% frames, with the last element being either any remaining binary data for
%% incomplete frames or an incomplete frame itself. Any completed frames will
%% be processed and discarded, with the remainder being passed back into
%% websocket_loop's partial data.
%%
%% This will be left to crash if an unmasked frame is sent from the client.  My
%% apologies for making these lines extremely long. I wanted to model a single
%% frame packet in pattern matching horizontally so it's obvious what's going
%% on.
-spec parse_frame(binary()) -> [#frame{} | binary()].
parse_frame(<<>>) -> [<<>>];
    %FRAME:         FIN    RSV    OPCODE  MASKED          PAYLOAD_LEN                   EXT_PAYLOAD_LEN  MASK _KEY  PAYLOAD
parse_frame(Raw = <<Fin:1, RSV:3, Op:4,   ?WS_MASKED:1,   ?WS_EXTENDED_PAYLOAD_16BIT:7, PayloadLen:16,   Mask:32,   Data/binary>>) ->
    ?DO_FRAMES(Mask);
parse_frame(Raw = <<Fin:1, RSV:3, Op:4,   ?WS_MASKED:1,   ?WS_EXTENDED_PAYLOAD_64BIT:7, PayloadLen:64,   Mask:32,   Data/binary>>) ->
    ?DO_FRAMES(Mask);
parse_frame(Raw = <<Fin:1, RSV:3, Op:4,   ?WS_MASKED:1,   PayloadLen:7,                                  Mask:32,   Data/binary>>) ->
    ?DO_FRAMES(Mask);
parse_frame(<<_:8,                        ?WS_UNMASKED:1, _/binary>>) ->
    close_with_purpose(1002, unmasked_packet_received_from_client);
parse_frame(Data) ->
    [Data]. %% not enough data to parse the frame, so just return the data, and we'll try after we get more data

do_frames(Fin, RSV, Op, PayloadLen, Mask, Data) ->
    F = #frame{fin=Fin, rsv=RSV, opcode=Op, masked=1, payload_len=PayloadLen, mask_key=Mask, data = <<>>},
    append_frame_data_and_parse_remainder(F, Data).

append_frame_data_and_parse_remainder(F = #frame{payload_len=PayloadLen, mask_key=Mask, data=CurrentPayload}, Data) ->
    FullData = <<CurrentPayload/binary,Data/binary>>,
    Length = byte_size(FullData),
    if
        Length =:= PayloadLen ->
            Unmasked = apply_mask(Mask, FullData),
            [F#frame{data=Unmasked}, <<>>];

        Length > PayloadLen  ->
            %% Since the length of the received data is longer than the
            %% required payload length, break off the part we want for our
            %% frame
            FrameData = binary:part(FullData, 0, PayloadLen),
            Unmasked = apply_mask(Mask, FrameData),

            %% Then let's break off the remainder of the binary, we're going to
            %% try parsing this, too
            RemainingData = binary:part(Data, PayloadLen, Length-PayloadLen),
            [F#frame{data=Unmasked} | parse_frame(RemainingData)]
    end.

%%apply_mask(<<Mask:32>>, Data) ->
%%    apply_mask(Mask, Data);
apply_mask(Mask, Data) ->
    {_Time, Unmasked} = timer:tc(fun apply_mask/3, [Mask, Data,<<>>]),
    Unmasked.

apply_mask(_, <<>>, Acc) ->
    Acc;
apply_mask(M, <<D:32,Rest/binary>>, Acc) ->
    Masked = D bxor M,
    apply_mask(M, Rest, <<Acc/binary,Masked:32>>);
apply_mask(FullM, <<D:8>>, Acc ) ->
    <<M:8,_:24>> = <<FullM:32>>,
    Masked = D bxor M,
    <<Acc/binary,Masked:8>>;
apply_mask(FullM, <<D:16>>, Acc) ->
    <<M:16,_:16>> = <<FullM:32>>,
    Masked = D bxor M,
    <<Acc/binary,Masked:16>>;
apply_mask(FullM, <<D:24>>, Acc) -> 
    <<M:24,_:8>> = <<FullM:32>>,
    Masked = D bxor M,
    <<Acc/binary,Masked:24>>.

close_on_invalid_utf8_text(binary, _)  -> do_nothing;
close_on_invalid_utf8_text(text, Data) ->
    case is_utf8(Data) of
        true -> do_nothing;
        false -> close_with_purpose(1007, {invalid_utf8, Data})
    end.

%% UTF8-Validation Below:
%% Copyright 2011-2013 Loïc Hoguin <essen@ninenines.eu>
%% Borrowed from Cowboy:
%% https://github.com/extend/cowboy/blob/0d5a12c3ecd3bd093c33e9a8126f1d129719b9ea/src/cowboy_websocket.erl#L491
-spec is_utf8(binary()) -> boolean().
is_utf8(<<>>) ->
        true;
is_utf8(<< _/utf8, Rest/binary >>) ->
        is_utf8(Rest);
%% 2 bytes. Codepages C0 and C1 are invalid; fail early.
is_utf8(<< 2#1100000:7, _/bits >>) ->
        false;
%%is_utf8(<< 2#110:3, _:5 >>) ->
%%        true;
%%%% 3 bytes.
%%is_utf8(<< 2#1110:4, _:4 >>) ->
%%        true;
%%is_utf8(<< 2#1110:4, _:4, 2#10:2, _:6 >>) ->
%%        true;
%%%% 4 bytes. Codepage F4 may have invalid values greater than 0x10FFFF.
is_utf8(<< 2#11110100:8, 2#10:2, High:6, _/bits >>) when High >= 2#10000 ->
        false;
%%is_utf8(<< 2#11110:5, _:3 >>) ->
%%        true;
%%is_utf8(<< 2#11110:5, _:3, 2#10:2, _:6 >>) ->
%%        true;
%%is_utf8(<< 2#11110:5, _:3, 2#10:2, _:6, 2#10:2, _:6 >>) ->
%%        true;
%% Invalid.
is_utf8(_) ->
        false.
