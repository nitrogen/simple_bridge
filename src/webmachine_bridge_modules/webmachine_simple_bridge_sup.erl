%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(webmachine_simple_bridge_sup).
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
    application:start(inets),
    application:start(crypto),
    application:load(webmachine),

    {Address, Port} = simple_bridge_util:get_address_and_port(webmachine),
    Dispatch = generate_dispatch(),

    io:format("Starting Webmachine Server on ~s:~p~n", [Address, Port]),

    Options = [
        {ip, Address}, 
        {port, Port},
        {dispatch, Dispatch}
    ],
    Web = {webmachine_mochiweb,
            {webmachine_mochiweb, start, [Options]},
            permanent, 5000, worker, [mochiweb_socket_server]},
    Processes = [Web],
    application:start(mochiweb),
    application:start(webmachine),
    {ok, { {one_for_one, 5, 10}, Processes} }.

generate_dispatch() ->
    case application:get_env(simple_bridge, webmachine_dispatch) of
        {ok, Dispatch} -> Dispatch;
        undefined ->
            case application:get_env(simple_bridge, webmachine_dispatch_fun) of
                {ok, {M,F}} ->
                    M:F();
                undefined ->
                    build_dispatch()
            end
    end.

build_dispatch() ->
    {DocRoot, StaticPaths} = simple_bridge_util:get_docroot_and_static_paths(webmachine),
    io:format("Static Paths: ~p~nDocument Root for Static: ~s~n",
              [StaticPaths, DocRoot]),
    build_dispatch(DocRoot, StaticPaths).
    

build_dispatch(DocRoot, StaticPaths) -> 
    Handler = simple_bridge_util:get_env(handler),
    StaticDispatches = [make_static_dispatch(DocRoot, StaticPath) || StaticPath <- StaticPaths],
    StaticDispatches ++ [
        %% Static content handlers can be defined manually like so:
        %% {["js", '*'],       webmachine_simple_bridge_static, [{root, "./site/static/js"}]},
        %% {["css", '*'],      webmachine_simple_bridge_static, [{root, "./site/static/css"}]},
        %% {["images", '*'],   webmachine_simple_bridge_static, [{root, "./site/static/images"}]},
        %%
        %% But instead of doing it manually, we'll load it from the configuration

        %% Add routes to your modules here. The last entry makes the
        %% system use simple_bridge's handler, which is a generic handler for
        %% all non-static requests, and uses the `handler` configuration
        %% variable to call out to your application.
        %% 
        %% p.s. - Remember that you will need to RESTART THE VM for
        %%        dispatch changes to take effect!!!
        %% 
        %% {["path","to","module1",'*'], HandlerModule1, InitialState1}
        %% {["path","to","module2",'*'], HandlerModule2, InitialState2}
        %% {["path","to","module3",'*'], HandlerModule3, InitialState3}
        {['*'], simple_bridge_util:get_anchor_module(webmachine), Handler}
    ].

join_path(Root,Path) when is_binary(Root) orelse is_binary(Path) ->
    join_path(wf:to_list(Root),wf:to_list(Path));
join_path(Root,Path) ->
    RootEndsWithSlash = lists:last(Root)==$/,
    PathStartsWithSlash = hd(Path)==$/,
    if
        RootEndsWithSlash andalso PathStartsWithSlash -> 
            Root ++ tl(Path);
        not(RootEndsWithSlash) andalso not(PathStartsWithSlash) ->
            Root ++ "/" ++ Path;
        true ->
            %% Either the root ends with a slash, or the path starts with a
            %% slash, but not both, so we can just concantenate them together
            %% safely
            Root ++ Path
    end.

make_static_dispatch(DocRoot, StaticPath) ->
    TokenStatic = string:tokens(StaticPath,"/"),
    FormattedPath = TokenStatic ++ ['*'],
    {FormattedPath, webmachine_simple_bridge_static, [{root, join_path(DocRoot,StaticPath)}]}.
