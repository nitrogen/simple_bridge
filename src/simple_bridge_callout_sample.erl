-module(simple_bridge_callout_sample).
-behaviour(simple_bridge_callout).
-export([run/1,
		ws_init/1,
		ws_message/2,
		ws_info/2,
		ws_terminate/2]).

run(_Bridge) ->
	{ok, "Hello World"}.

ws_init(_Bridge) ->
	io:format("Connection Established"),
	ok.

ws_message({text, Data}, _Bridge) ->
	io:format("Text Received: ~p~n", [Data]),
	Reply = io_lib:format("Text Received: ~p~n", [Data]),
	{reply, Reply};
ws_message({binary, Data}, _Bridge) ->
	io:format("Binary Received: ~p~n", [Data]),
	Reply = io_lib:format("Binary Received echo: ~p~n", [Data]),
	{reply, Reply}.

ws_info(Data, _Bridge) ->
	io:format("Info Received: ~p~n", [Data]),
	Reply = io_lib:format("Info Message: ~p~n", [Data]),
	{reply, Reply}.

ws_terminate(_Reaso, _Bridge) ->
	ok.
