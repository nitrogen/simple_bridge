%% vim: ts=4 sw=4 et
%% -*- mode: nitrogen -*-
-module(mochiweb_simple_bridge_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% Start Mochiweb...
    application:load(mochiweb),

    {Address, Port} = simple_bridge_util:get_address_and_port(mochiweb),

    ServerName = simple_bridge_util:get_server_name(mochiweb),

    {DocRoot, StaticPaths} = simple_bridge_util:get_docroot_and_static_paths(mochiweb),

    Anchor = simple_bridge_util:get_anchor_module(mochiweb),

    io:format("Starting Mochiweb Server on ~s:~p~n", [Address, Port]),
    io:format("Static Paths: ~p~nDocument Root for Static: ~s~n", [StaticPaths, DocRoot]),

    % Start Mochiweb...
    Options = [
        {name, ServerName},
        {ip, Address}, 
        {port, Port},
        {loop, fun Anchor:loop/1}
    ],
    mochiweb_http:start(Options),

    {ok, { {one_for_one, 5, 10}, []} }.

