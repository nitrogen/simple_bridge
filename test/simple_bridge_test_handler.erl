-module(simple_bridge_test_handler).
-behaviour(simple_bridge_handler).
-export([run/1,
        ws_init/1,
        ws_message/3,
        ws_info/3,
        ws_terminate/3]).

run(Bridge) ->
	Path = sbw:path(Bridge),
    Bridge2 = run(Path, Bridge),
	sbw:build_response(Bridge2).

run("/peer_ip", Bridge) -> simple_call(peer_ip, Bridge);
run("/protocol", Bridge) -> simple_call(protocol, Bridge);
run("/path", Bridge) -> simple_call(path, Bridge);
run("/uri", Bridge) -> simple_call(uri, Bridge);
run("/request_method_get", Bridge) -> simple_call(request_method, Bridge);
run("/request_method_post", Bridge) -> simple_call(request_method, Bridge);
run("/request_body", Bridge) -> simple_call(request_body, Bridge);
run("/uploaded_files", Bridge) -> get_uploaded_files(Bridge);
run("/query_params", Bridge) -> simple_call(query_params, Bridge);
run("/post_params", Bridge) -> simple_call(post_params, Bridge);
run("/cookie", Bridge) -> do_cookies(Bridge);
run(Path, Bridge) ->
    sbw:set_response_data(io_lib:format("Unhandled Path: ~p", [Path]), Bridge).

simple_call(Call, Bridge) ->
	Val = sbw:Call(Bridge),
	Body = io_lib:format("~p", [Val]),
    sbw:set_response_data(Body, Bridge).

get_uploaded_files(Bridge) ->
    Files = sbw:post_files(Bridge),
    Errors = sbw:error(Bridge),
    RawResponse = term_to_binary({lists:map(fun(File) ->
        {sb_uploaded_file:original_name(File), File}
    end, Files), Errors}),
    EncodedResponse = RawResponse,

    %base64:encode(RawResponse),
    sbw:set_response_data(EncodedResponse, Bridge).

do_cookies(Bridge) ->
    Type = sbw:query_param(type, Bridge),
    Cookies = sbw:cookies(Bridge),
    Bridge2 = lists:foldl(fun({K,V}, Br) ->
        set_cookie(Type, K, V, Br)
    end, Bridge, Cookies),
    sbw:set_response_data("ok", Bridge2).

set_cookie("list", K, V, Bridge) ->
    K2 = simple_bridge_util:to_list(K),
    V2 = simple_bridge_util:to_list(V),
    sbw:set_cookie(K2, V2, Bridge);
set_cookie("binary", K, V, Bridge) ->
    K2 = simple_bridge_util:to_binary(K),
    V2 = simple_bridge_util:to_binary(V),
    sbw:set_cookie(K2, V2, Bridge).

%% WebSockets
%% stubs, no real tests
ws_init(_Bridge) ->
    ok.

ws_message({text, <<"frag">>}, _State, _Bridge) ->
    Reply = [{text, [Msg," "]} || Msg <- ["A","spoon","full","of","sugar"]],
    {reply, Reply};
ws_message({text, Data}, _Bridge, _State) ->
    {reply, {text, Data}};
ws_message({binary, Data}, _Bridge, _State) ->
    {reply, {binary, Data}}.

ws_info(Data, _Bridge, _State) ->
    Reply = {text, io_lib:format("~s", [Data])},
    {reply, Reply}.

ws_terminate(_Reason, _Bridge, _State) ->
    ok.
