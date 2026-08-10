%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(cowboy_simple_bridge_sup).
-behaviour(supervisor).
-include("simple_bridge.hrl").
-export([
    start_link/0,
    init/1,
    get_dispatch_info/2
    ]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    application:ensure_all_started(cowboy),
    {Address, Port} = simple_bridge_util:get_address_and_port(cowboy),
    IP = simple_bridge_util:parse_ip(Address),
    
    io:format("Starting Cowboy Server on ~p:~p~n",
        [IP, Port]),
    
    Dispatch = generate_dispatch(),
    io:format("Using Cowboy Dispatch Table:~n  ~p~n",[Dispatch]),
    
    Opts = #{env => #{dispatch => Dispatch},
            max_keepalive => 100},
    
    %% TODO: This should be cowboy:start_tls ir using TLS.
    cowboy:start_clear(http, [{ip, IP}, {port, Port}], Opts),
    
    {ok, { {one_for_one, 5, 10}, []}}.


%% @doc Generates the dispatch based on the desired environment
%% 1) First get dispatch strategy, that can be override or merge
%% 2) Then it checks if there's a cowboy_dispatch config, if there is, it uses
%%    that.
%% 3) Then it checks for cowboy_dispatch_fun, which is a tuple
%%    {Module,Function} and uses the result of calling Module:Function()
%% 4) Finally, if all else fails, it uses the provided document_root and
%%    static_paths config values standard to simple_bridge to generate a
%%    cowboy-specific dispatch table.
generate_dispatch() ->    
    DispatchStrategy = case application:get_env(simple_bridge, dispatch_strategy) of
        {ok, override} -> override;
        {ok, merge} -> merge;
        _ -> override
    end,    
    Dispatches = case application:get_env(simple_bridge, cowboy_dispatch) of
        {ok, CustomDispatches} -> 
            if 
                DispatchStrategy =:= override -> CustomDispatches;
                true -> make_default_dispatch(CustomDispatches)
            end;
        undefined ->
            case application:get_env(simple_bridge, cowboy_dispatch_fun) of
                {ok, {M,F}} ->
                    CustomDispatches = M:F(),
                    if 
                        DispatchStrategy =:= override -> CustomDispatches;
                        true -> make_default_dispatch(CustomDispatches) 
                    end;
                undefined ->
                    make_default_dispatch([])
            end
    end,
    build_dispatch(Dispatches).


%% @doc Gets the environment variables document_root and static_paths, and
%% generates dispatches from them
make_default_dispatch(CustomRoutes) ->
    {DocRoot, StaticPaths} = simple_bridge_util:get_docroot_and_static_paths(cowboy),
    io:format("Static Paths: ~p~nDocument Root for Static: ~s~nCustom Routes: ~p~n",
        [StaticPaths, DocRoot, CustomRoutes]),
    {StaticDispatches,HandlerModule,HandlerOpts} = get_dispatch_info(DocRoot,StaticPaths),
    StaticDispatches ++ CustomRoutes ++ [{'_', HandlerModule , HandlerOpts}].

%% @doc Generate the dispatch items
build_dispatch(Dispatches) ->    
    %% Start Cowboy...
    %% NOTE: According to Loic, there's no way to pass the buck back to cowboy 
    %% to handle static dispatch files so we want to make sure that any large 
    %% files get caught in general by cowboy and are never passed to the nitrogen
    %% handler at all. In general, your best bet is to include the directory in
    %% the static_paths section of cowboy.config
    %%
    %% Simple Bridge will do its best to efficiently handle static files, if
    %% necessary but it's recommended to just make sure you properly use the
    %% static_paths, or rewrite cowboy's dispatch table    
    BaseDispatch=[
            %% Nitrogen will handle everything that's not handled in the StaticDispatches
            {'_', Dispatches}
            ],
    cowboy_router:compile(BaseDispatch).

%% @doc Return base, Nitrogen-specific dispatch information (potentially 
%% reusable by third-parties
get_dispatch_info(DocRoot,StaticPaths) ->
    StaticDispatches = lists:map(fun(Dir) ->
                    Path = reformat_path(Dir),
                    {StaticType, StaticFileDir} = localized_dir_file(DocRoot, Dir),
                    Opts = [{mimetypes, cow_mimetypes, all}],
                    %% This will end up being something like:
                    %% { [<<"js">>,'...'], cowboy_static, {dir, "./priv/static/js", Opts}}
                    {Path, cowboy_static, {StaticType, StaticFileDir, Opts}}
            end,StaticPaths),
    
    %% HandlerModule will end up calling HandlerModule:handle(Req,HandlerOpts)
    HandlerModule = simple_bridge_util:get_anchor_module(cowboy),
    HandlerOpts = [],    
    {StaticDispatches,HandlerModule,HandlerOpts}.

localized_dir_file(DocRoot,Path) ->
    NewPath = case hd(Path) of
        $/ -> DocRoot ++ Path;
        _ -> DocRoot ++ "/" ++ Path
    end,
    _NewPath2 = case lists:last(Path) of
        $/ -> {dir, NewPath};
        _ ->  {file, NewPath}
    end.

%% Ensure the paths start with /, and if a path ends with /, then add "[...]" to it
reformat_path(Path) ->
    Path2 = case hd(Path) of
        $/ -> Path;
        $\ -> Path;
        _ -> [$/|Path]
    end,
    Path3 = case lists:last(Path) of 
        $/ -> Path2 ++ "[...]";
        $\ -> Path2 ++ "[...]";
        _ -> Path2
    end,
    Path3.
