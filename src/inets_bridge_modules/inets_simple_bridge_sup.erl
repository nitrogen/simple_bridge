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
    HasMimetypes = start_mimetypes(),
    application:load(inets),
    case application:get_env(inets, services) of
        undefined -> build_config(HasMimetypes);
        {ok, Services} ->
            case proplists:get_value(httpd, Services) of
                undefined ->
                    build_config(HasMimetypes);
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

start_mimetypes() ->
    case application:start(mimetypes) of
        ok -> true;
        {error, {already_started, mimetypes}} -> true;
        {error, _} -> false
    end.

build_config(HasMimetypes) ->
    io:format("No configuration for inets httpd, so constructing from simple_bridge.config...~n"),
    {Address, Port} = simple_bridge_util:get_address_and_port(inets),
    {DocRoot, StaticPaths} = simple_bridge_util:get_docroot_and_static_paths(inets),
    io:format("Starting Inets Server at ~p:~p~n", [Address, Port]),
    io:format("Static Paths: ~p~nDocument Root for Static: ~s~n", [StaticPaths, DocRoot]),
    LogPath = "./log/inets.log",
    ok = filelib:ensure_dir(LogPath),

    Httpd = {httpd, [
        {bind_address, simple_bridge_util:parse_ip(Address)},
        {port, Port},
        {server_name, "simple_bridge_inets"},
        {server_root, "."},
        {document_root, DocRoot},
        {error_log, LogPath},
        {modules, [mod_log, mod_disk_log, simple_bridge_util:get_anchor_module(inets)]},
        {mime_types, build_mimetypes(HasMimetypes)}
    ]},

    case application:get_env(inets, services) of
        undefined ->
            application:set_env(inets, services, [Httpd]);
        {ok, Services} ->
            NewServices = [Httpd | Services],
            application:set_env(inets, services, NewServices)
    end.

build_mimetypes(true = _HasMimetypes) ->
    [{binary_to_list(Ext), binary_to_list(hd(mimetypes:ext_to_mimes(Ext)))} 
        || Ext <- mimetypes:extensions()];
build_mimetypes(false = _HasMimetypes) ->
    Mimetypes = [
        {"html", "text/html"},
        {"js", "text/javascript"},
        {"jpg", "image/jpeg"},
        {"jpeg", "image/jpeg"},
        {"png", "image/png"},
        {"gif", "image/gif"}
    ],
    io:format("mimetypes application not found.~n Specifying bare-bones mimetypes:~n~p~n", [Mimetypes]),
    Mimetypes.
