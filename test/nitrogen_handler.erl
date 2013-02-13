%% Feel free to use, reuse and abuse the code in this file.

-module(nitrogen_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-include_lib("common_test/include/ct.hrl").
-record(state, {headers, body}).

init({_Transport, http}, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    RequestBridge = simple_bridge:make_request(cowboy_request_bridge, Req),
    ResponseBridge = simple_bridge:make_response(cowboy_response_bridge, RequestBridge),
    %% Establishes the context with the Request and Response Bridges
    nitrogen:init_request(RequestBridge, ResponseBridge),
    {ok, Req2} = nitrogen:run(),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

