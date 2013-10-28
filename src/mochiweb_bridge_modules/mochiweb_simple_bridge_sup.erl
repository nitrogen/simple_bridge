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


    ServerName = simple_bridge_util:get_env([
                                                {simple_bridge, server_name},
                                                {mochiweb, server_name}
                                            ], simple_bridge_mochiweb),

    DocRoot = simple_bridge_util:get_docroot(mochiweb),

    io:format("Starting Mochiweb Server (~s) on ~s:~p, root: '~s'~n", [ServerName, Address, Port, DocRoot]),

    % Start Mochiweb...
    Options = [
        {name, ServerName},
        {ip, Address}, 
        {port, Port},
        {loop, fun mochiweb_simple_bridge_anchor:loop/1}
    ],
    mochiweb_http:start(Options),

    {ok, { {one_for_one, 5, 10}, []} }.

