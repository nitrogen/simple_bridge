%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(yaws_simple_bridge_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1,
    start_embedded_yaws/0
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
    application:load(yaws),
    case application:get_env(yaws, conf) of
        {ok, Conf} -> 
            io:format("Starting Yaws based on the configuration file: ~s~n", [Conf]),
            application:start(yaws);
        undefined ->
            start_embedded_yaws()
    end,
    {ok, { {one_for_one, 5, 10}, []}}.

start_embedded_yaws() ->
    {DocRoot, StaticPaths} = simple_bridge_util:get_docroot_and_static_paths(yaws),
    {Address, Port} = simple_bridge_util:get_address_and_port(yaws),
    RealAddress = simple_bridge_util:parse_ip(Address),
    Servername = simple_bridge_util:to_list(simple_bridge_util:get_server_name(yaws)),
    Anchor = simple_bridge_util:get_anchor_module(yaws),
    ExcludePaths = [filename:split(P) || P <- StaticPaths],
    Appmods = [{"/", Anchor, ExcludePaths}],

    SConf = [
        {servername, Servername ++ ":" ++ integer_to_list(Port)},
        {docroot, DocRoot},
        {listen, RealAddress},
        {port, Port},
        {allowed_scripts, []},
        {index_files, ["index.html"]},
        {appmods, Appmods}
    ],

    GConf = [{id, Servername}],
    io:format("Starting Yaws Server at ~s:~p~n", [Address, Port]),
    io:format("Static Paths: ~p~nDocument Root for Static: ~s~n", [StaticPaths, DocRoot]),
    yaws:start_embedded(DocRoot, SConf, GConf, Servername).
