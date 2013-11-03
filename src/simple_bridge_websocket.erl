%% vim: ts=4 sw=4 et
-module(simple_bridge_websocket).
-export([
        attempt_hijacking/2
    ]).

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

-record(frame, {fin=1, rsv1=0, rsv2=0, rsv3=0, opcode, masked=0, payload_len=0, mask_key, data = <<>>}).
-record(partial_data, {data = <<>>, message_frames=[]}).

attempt_hijacking(Bridge, Callout) ->
    ProtocolVersion = sbw:protocol_version(Bridge),
    UpgradeHeader = sbw:header_lower(upgrade, Bridge),
    ConnectionHeader = sbw:header_lower(connection, Bridge),
    WSVersionHead = sbw:header("Sec-WebSocket-Version", Bridge),

    if
        ProtocolVersion     =:= {1,1},
        UpgradeHeader       =:= "websocket",
        ConnectionHeader    =:= "upgrade",
        WSVersionHead       =/= undefined ->
            WSVersions = re:split(WSVersionHead, "[, ]+]", [{return, list}]),
            HijackedBridge = case lists:member(?WS_VERSION, WSVersions) of
                true ->
                    hijack(Bridge, Callout);
                false ->
                    hijack_request_fail(Bridge)
            end,
            {hijacked, HijackedBridge};
        ?else ->
            spared      %% Spared from being hijacked
    end.

hijack_request_fail(Bridge) ->
    Bridge2 = sbw:set_status_code(400, Bridge),
    Bridge3 = sbw:set_header(Bridge2, "Sec-Websocket-Version", ?WS_VERSION),
    Bridge4 = sbw:set_response_data(["Invalid Websocket Upgrade Request. Please use Websocket version ",?WS_VERSION], Bridge3),
    Bridge4.


prepare_response_key(WSKey) ->
    FullString = WSKey ++ ?WS_MAGIC,
    Sha = crypto:sha(FullString),
    base64:encode(Sha).

hijack(Bridge, Callout) ->
    WSKey = sbw:header("Sec-Websocket-Key", Bridge),
    ResponseKey = prepare_response_key(WSKey),
    Socket = sbw:socket(Bridge),
    send_handshake_response(Socket, ResponseKey),
    case erlang:function_exported(Callout, ws_init, 1) of
        true -> ok = Callout:ws_init(Bridge);
        false -> do_nothing
    end,
    inet:setopts(Socket, [{active, once}]),
    websocket_loop(Socket, Bridge, Callout, #partial_data{}).
    
send_handshake_response(Socket, ResponseKey) ->
    Handshake = [
                 <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                 <<"Upgrade: websocket\r\n">>,
                 <<"Connection: Upgrade\r\n">>,
                 <<"Sec-WebSocket-Accept: ">>,ResponseKey,<<"\r\n">>,
                 <<"\r\n">>
                ],
    error_logger:info_msg("Handshake: ~s~n", [Handshake]),
    gen_tcp:send(Socket, Handshake).


websocket_loop(Socket, Bridge, Callout, PartialData) ->
    receive 
        {tcp, Socket, Data} ->
            Frames = parse_packet_into_frames(Data, PartialData#partial_data.data),
            PendingFrames = PartialData#partial_data.message_frames,
            {PendingFrames2, RemainderData} = process_frames(Frames, Socket, Bridge, Callout, PendingFrames),
            inet:setopts(Socket, [{active, once}]),
            websocket_loop(Socket, Bridge, Callout, #partial_data{data=RemainderData, message_frames=PendingFrames2});
        {tcp_closed, Socket} ->
            closed;
        Msg ->
            ok
            %do_something()
    end.

send(Socket, {ping, Data}) ->
    send_frame(Socket, #frame{opcode=?WS_PING, data=Data});
send(Socket, {pong, Data}) ->
    send_frame(Socket, #frame{opcode=?WS_PONG, data=Data});
send(Socket, close) ->
    send_frame(Socket, #frame{opcode=?WS_CLOSE});
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
    error_logger:info_msg("Sending to ~p: ~p~n", [Socket, BinFrame]),
    gen_tcp:send(Socket, BinFrame).

encode_frame(#frame{
        fin=Fin, rsv1=R1, rsv2=R2, rsv3=R3, opcode=Opcode,
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
    <<Fin:1, R1:1, R2:1, R3:1, Opcode:4, Masked:1, PayloadLen:7, ExtLen:ExtBitSize, BinData/binary>>.


%% This goes through each Frame in "Frames", and processes it, responding to
%% control processes, dispatching callouts if necessary, and if we're in the
%% middle of a series of fragments, store up those fragments and append them to
%% PendingFrames, or if a series of fragments is completed, then dispatch those
%% frames to the callout module and discard them.
process_frames([], _Socket, _Bridge, _Callout, PendingFrames) ->
    {PendingFrames, <<>>};
process_frames([<<>>], _Socket, _Bridge, _Callout, PendingFrames) ->
    {PendingFrames, <<>>};

%% Single Text or Binary Frame (PendingFrames must be empty)
process_frames([_F = #frame{opcode=Opcode, fin=1, data=Data} |Rest], Socket, Bridge, Callout, []) 
        when Opcode=:=?WS_BINARY; Opcode=:=?WS_TEXT ->
    Reply = Callout:ws_message({type(Opcode), Data}, Bridge),
    send(Socket, Reply),
    process_frames(Rest, Socket, Bridge, Callout, []);

%% First Text Fragment (PendingFrames must be empty)
process_frames([F = #frame{opcode=Opcode, fin=0}|Rest], Socket, Bridge, Callout, [])
        when Opcode=:=?WS_BINARY; Opcode=:=?WS_TEXT ->
    process_frames(Rest, Socket, Bridge, Callout, [F]);

%% Continuation Frame
process_frames([F = #frame{opcode=?WS_CONTINUATION, fin=0}|Rest], Socket, Bridge, Callout, PendingFrames=[_|_]) ->
    process_frames(Rest, Socket, Bridge, Callout, [F | PendingFrames]);

%% Last fragment of a fragmented message
process_frames([F = #frame{opcode=?WS_CONTINUATION, fin=1}|Rest], Socket, Bridge, Callout, PendingFrames=[_|_]) ->
    ReorderedFrames = lists:reverse([F|PendingFrames]),
    Type = type((hd(ReorderedFrames))#frame.opcode),
    Msg = defragment_data(ReorderedFrames),
    Reply = Callout:ws_message({Type, Msg}, Bridge),
    send(Socket, Reply),
    process_frames(Rest, Socket, Bridge, Callout, []);

process_frames([_F = #frame{opcode=?WS_PONG}|Rest], Socket, Bridge, Callout, PendingFrames) ->
    %% do nothing?
    process_frames(Rest, Socket, Bridge, Callout, PendingFrames);

process_frames([#frame{opcode=?WS_PING, data=Data}|Rest], Socket, Bridge, Callout, PendingFrames) ->
    send(Socket, {pong, Data}),
    process_frames(Rest, Socket, Bridge, Callout, PendingFrames);

process_frames([_F = #frame{opcode=?WS_CLOSE}|_Rest], Socket, Bridge, Callout, _PendingFrames) ->
    send(Socket, close),
    Callout:ws_terminate(Bridge),
    inet:close(Socket).
    
defragment_data(Frames) ->
    iolist_to_binary([F#frame.data || F <- Frames]).


type(?WS_BINARY) -> binary;
type(?WS_TEXT) -> text.

%% New Frame, Woohoo!
parse_packet_into_frames(Data, Raw) when is_binary(Raw) ->
    FullData = <<Raw/binary,Data/binary>>,
    parse_frame(FullData);
%% Partial frame that only received part of the payload
parse_packet_into_frames(Data, F=#frame{}) ->
    append_frame_data_and_parse_remainder(F, Data).

-define(DO_FRAMES(Mask), do_frames(Fin,R1,R2,R3,Op,PayloadLen,Mask,Data)).

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
    %FRAME:   FIN    RSV1  RSV2  RSV3  OPCODE  MASKED          PAYLOAD_LEN                   EXT_PAYLOAD_LEN  MASK _KEY  PAYLOAD
parse_frame(<<Fin:1, R1:1, R2:1, R3:1, Op:4,   ?WS_MASKED:1,   PayloadLen:7,                                  Mask:32,   Data/binary>>) when PayloadLen < ?WS_EXTENDED_PAYLOAD_16BIT ->
    ?DO_FRAMES(Mask);
parse_frame(<<Fin:1, R1:1, R2:1, R3:1, Op:4,   ?WS_MASKED:1,   ?WS_EXTENDED_PAYLOAD_16BIT:7, PayloadLen:16,   Mask:32,   Data/binary>>) ->
    ?DO_FRAMES(Mask);
parse_frame(<<Fin:1, R1:1, R2:1, R3:1, Op:4,   ?WS_MASKED:1,   ?WS_EXTENDED_PAYLOAD_64BIT:7, PayloadLen:64,   Mask:32,   Data/binary>>) ->
    ?DO_FRAMES(Mask);
parse_frame(<<_:8,                             ?WS_UNMASKED:1, _/binary>>) ->
    throw({unmasked_packet_received_from_client});
parse_frame(Data) ->
    [Data]. %% not enough data to parse the frame, so just return the data, and we'll try after we get more data

do_frames(Fin, R1, R2, R3, Op, PayloadLen, Mask, Data) ->
    F = #frame{fin=Fin, rsv1=R1, rsv2=R2, rsv3=R3, opcode=Op, masked=1, payload_len=PayloadLen, mask_key=Mask, data = <<>>},
    error_logger:info_msg("applying mask: ~p to ~p~n",[Mask, Data]),
    Unmasked = apply_mask(Mask, Data),
    append_frame_data_and_parse_remainder(F, Unmasked).

append_frame_data_and_parse_remainder(F = #frame{payload_len=PayloadLen, data=CurrentPayload}, Data) ->
    FullData = <<CurrentPayload/binary,Data/binary>>,
    Length = byte_size(FullData),
    if
        Length =:= PayloadLen ->
            [F#frame{data=FullData}, <<>>];
        Length < PayloadLen ->
            [F#frame{data=FullData}];
        Length > PayloadLen ->
            %% Since the length of the received data is longer than the required payload length, break off the part we want for our frame
            FrameData = binary:part(FullData, 0, PayloadLen),

            %% Then let's break off the remainder of the binary, we're going to try parsing this, too
            RemainingData = binary:part(Data, PayloadLen, Length-PayloadLen),
            [F#frame{data=FrameData} | parse_frame(RemainingData)]
    end.

apply_mask(_, <<>>)                      -> <<>>;
apply_mask(M, <<D:32,Rest/binary>>) 
        when is_integer(M)               -> <<(D bxor M):32, (apply_mask(M, Rest))/binary>>;
apply_mask(M, Rest)
        when byte_size(Rest) < 4
        andalso is_integer(M)            -> apply_mask(<<M:32>>, Rest);
apply_mask(<<M:8,_/binary>>,   <<D:8>> ) -> <<(D bxor M)>>;
apply_mask(<<M:16,_/binary>>,  <<D:16>>) -> <<(D bxor M)>>;
apply_mask(<<M:24,_/binary>>,  <<D:24>>) -> <<(D bxor M)>>.
