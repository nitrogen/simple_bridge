-module(cowboy_simple_bridge_anchor).
-include("simple_bridge.hrl").

%%-export([
%%]).
%%
%%upgrade_or_not(Headers) ->
%%	UpgradeCheck = proplists:get_value(<<"Upgrade">>, Headers) =:= <<"websocket">> 
%%				   andalso proplists:get_value(<<"Connection">>, Headers) =:= <<"Upgrade">>,
%%	case UpgradeCheck of
%%		true ->
%%			{ok, {upgrade, protocol, cowboy_websocket, Req, Opts}};
%%		false -> 
%%			no_upgrade
%%	end.
%%
%%
%%websocket_init(_TransportName, Req, _Opts) ->
%%	{ok, Req, []}.
%%
%%websocket_handle({text, Msg}, Req, _Opts) ->
%%	{
