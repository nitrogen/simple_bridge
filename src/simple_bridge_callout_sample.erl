-module(simple_bridge_callout_sample).
-behaviour(simple_bridge_callout).
-export([run/1,
		ws_init/1,
		ws_message/2,
		ws_info/2,
		ws_terminate/2]).


run(Bridge) ->
	Bridge2 = Bridge:set_response_data(body()),
	Bridge2:build_response().

ws_init(_Bridge) ->
	io:format("Connection Established"),
	ok.

ws_message({text, Data}, _Bridge) ->
	io:format("Text Received: ~p~n", [Data]),
	Reply = io_lib:format("Text Received: ~p~n", [Data]),
	{reply, {text, Reply}};
ws_message({binary, Data}, _Bridge) ->
	io:format("Binary Received: ~p~n", [Data]),
	Reply = io_lib:format("Binary Received echo: ~p~n", [Data]),
	{reply, {text, Reply}}.

ws_info(Data, _Bridge) ->
	io:format("Info Received: ~p~n", [Data]),
	Reply = io_lib:format("Info Message: ~p~n", [Data]),
	{reply, Reply}.

ws_terminate(_Reason, _Bridge) ->
	ok.


body() ->
	<<"<html>
	<head>
		<title>Hello from Simple Bridge!</title>
		<script src='//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js'></script>
		<script src='/js/websocket.js'></script>
	</head>
	<body>
		<h1>Hello from Simple Bridge!</h1>
		<div id='header'>
		  <h1>Websocket client</h1>
		  <div id='status'></div>
		</div>


		<div id='navigation'>

		  <p id='connecting'>
			<input type='text' id='server' value='ws://localhost:8000/'></input>
			<button type='button' onclick='toggle_connection()'>connection</button>
		  </p>
		  <div id='connected'>                                
			<p>
			  <input type='text' id='send_txt' value=></input>
			  <button type='button' onclick='sendTxt();'>send</button>
			</p>
		  </div>

		  <div id='content'>                                                
			<button id='clear' onclick='clearScreen()' >Clear text</button>
			<div id='output'></div>
		  </div>

		</div>
	  </body>
</html> 	
">>.

