%% vim: ts=4 sw=4 et
-module(simple_bridge_handler_sample).
-behaviour(simple_bridge_handler).
-export([run/1,
        ws_init/1,
        ws_message/3,
        ws_info/3,
        ws_terminate/3]).


run(Bridge) ->
    try
        Bridge2 = Bridge:set_response_data(body(Bridge)),
        Bridge2:build_response()
    catch E:C ->
        error_logger:error_msg("~p:~p: ~p",[E, C, erlang:get_stacktrace()]),
        exit("Error building response")
    end.

ws_init(_Bridge) ->
    %erlang:send_after(1000, self(), "START"),
    ok.

ws_message({text, <<"frag">>}, _State, _Bridge) ->
    Reply = [{text, [Msg," "]} || Msg <- ["A","spoon","full","of","sugar"]],
    {reply, Reply};
ws_message({text, Data}, _State, _Bridge) ->
    %Reply = io_lib:format("~s", [Data]),
    {reply, {text, Data}};
ws_message({binary, Data}, _State, _Bridge) ->
    {reply, {binary, Data}}.

ws_info(Data, _Bridge, _State) ->
    Reply = {text, io_lib:format("~s", [Data])},
    {reply, Reply}.

ws_terminate(_Reason, _Bridge, _State) ->
    ok.


%%% HTML BODY AND DRAWING TESTING THE BRIDGE CAPABILITIES

body(Bridge) ->
    [
    <<"<html>
    <head>
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
        <title>Hello from Simple Bridge!</title>
        <script src='http://code.jquery.com/jquery-1.10.2.js'></script>
        <script src='/js/websocket.js'></script>
    </head>
    <body>
        <h1>Hello from Simple Bridge!</h1>">>,
        connection_info(Bridge),
        <<"<div id='header'>
          <div id='status'></div>
        </div>


        <div id='navigation'>
          <p id='connecting'>
            <input type='text' id='server' value='ws://localhost:8000/'></input>
            <button type='button' onclick='toggle_connection()'>(re)connect websocket</button>
          </p>
          <div id='connected'>                                
            <p>
              <input type='text' id='send_txt' value=''></input>
              <button type='button' onclick='sendTxt();'>send</button>
            </p>
          </div>

          <div id='content'>                                                
            <button id='clear' onclick='clearScreen()' >Clear text</button>
            <div id='output'></div>
          </div>

        </div>
      </body>
    </html>">>
    ].

connection_info(Bridge) ->
    Fields = [socket, protocol, request_method, uri, path, headers, cookies, query_params, post_params, deep_query_params, peer_ip, protocol_version],
    [
        <<"<table>">>,
        [draw_connection_info(Field, Bridge) || Field <- Fields],
        <<"</table>">>
    ].

draw_connection_info(Field, Bridge) ->
    io_lib:format("<tr><td>~p</td><td><pre>~p</pre></td></tr>",[Field, sbw:Field(Bridge)]).

