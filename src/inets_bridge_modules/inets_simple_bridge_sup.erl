%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(inets_simple_bridge_sup).
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
    application:start(mimetypes),
    application:load(inets),
    case application:get_env(inets, services) of
        undefined -> build_config();
        {ok, Services} ->
            case proplists:get_value(Services, httpd) of
                undefined ->
                    build_config();
                _ -> ok
            end
    end,

    case application:start(inets) of
        ok -> ok;
        {error, {already_started, inets}} ->
            {ok, SrvConfig} = application:get_env(inets, services),
            Httpd = proplists:get_value(httpd, SrvConfig),
            {ok, _Pid} = inets:start(httpd, Httpd)
    end,
    {ok, { {one_for_one, 5, 10}, []} }.

build_config() ->
    io:format("No configuration for inets httpd, so constructing from simple_bridge.config...~n"),
    {Address, Port} = simple_bridge_util:get_address_and_port(inets),
    {DocRoot, StaticPaths} = simple_bridge_util:get_docroot_and_static_paths(inets),
    io:format("Starting Inets Server at ~p:~p~n", [Address, Port]),
    io:format("Static Paths: ~p~nDocument Root for Static: ~s~n", [StaticPaths, DocRoot]),

    Httpd = {httpd, [
        {bind_address, simple_bridge_util:parse_ip(Address)},
        {port, Port},
        {server_name, "simple_bridge_inets"},
        {server_root, "."},
        {document_root, DocRoot},
        {error_log, "./log/inets.log"},
        {modules, [simple_bridge_util:get_anchor_module(inets)]},
        {mime_types, build_mimetypes()}
    ]},

    case application:get_env(inets, services) of
        undefined ->
            application:set_env(inets, services, [Httpd]);
        Services ->
            NewServices = [Httpd | Services],
            application:set_env(inets, services, NewServices)
    end.

build_mimetypes() ->
    [{binary_to_list(Ext), binary_to_list(hd(mimetypes:ext_to_mimes(Ext)))} || Ext <- mimetypes:extensions()].
