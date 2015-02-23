% vim: ts=4 sw=4 et
% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge).
-export ([
    start/0,
    start/1,
    start/2,
    make/2,
    make/3,

    %% deprecated
    make_request/2,
    make_response/2
]).

-include("simple_bridge.hrl").

-callback init(req())                        -> req().
-callback protocol(req())                    -> http | https | ws | wss | undefined.
-callback request_method(req())              -> 'GET' | 'POST' | 'DELETE' | atom().
-callback uri(req())                         -> string().
-callback path(req())                        -> string().
-callback headers(req())                     -> [{key(), value()}].
-callback query_params(req())                -> [{key(), value()}].
-callback post_params(req())                 -> [{key(), value()}].
-callback peer_ip(req())                     -> ipv4() | ipv8().
-callback build_response(req(), #response{}) -> any().

start() ->
    start(undefined).

start(Backend) ->
    start(Backend, undefined).


start(Backend, Handler) when is_atom(Backend) ->
    application:load(simple_bridge),
    Handler2 = get_handler_set_env(Handler),
    Backend2 = get_backend_set_env(Backend),

    handler_check(Handler2),
    backend_check(Backend2),

    Supervisor = make_supervisor_module(Backend2),
    Supervisor:start_link().

handler_check(undefined) ->
    error_logger:warning_msg("*** Warning: No handler module defined for simple_bridge. If this intentional,~n*** (like if you are using a custom dispatch table, for example), then this message~n*** can be safely ignored.");
handler_check(Handler) ->
    case code:ensure_loaded(Handler) of
        {module, Handler} ->
            ok;
        {error, Error} ->
            throw({'unable to load handler module', [{module, Handler}, {error, Error}]})
    end.

backend_check(undefined) ->
    throw('no backend defined');
backend_check(_) ->
    ok.

make_supervisor_module(Backend) ->
    list_to_atom(atom_to_list(Backend) ++ "_simple_bridge_sup").

get_backend_set_env(Backend) ->
    simple_bridge_util:get_maybe_set_env(backend, Backend).

get_handler_set_env(Handler) ->
    simple_bridge_util:get_maybe_set_env(handler, Handler).

make(BridgeType, Req) ->
    make(BridgeType, Req, []).

make(BridgeType, Req, _DocRoot) ->
    Module = make_bridge_module(BridgeType),
    inner_make(Module, Req).
    
make_bridge_module(BridgeType) ->
    list_to_atom(atom_to_list(BridgeType) ++ "_simple_bridge").

inner_make(Module, RequestData) ->
    try
        make_nocatch(Module, RequestData)
    catch Type : Error ->
        error_logger:error_msg("Error in simple_bridge:make/2 - ~p - ~p~n~p", [Type, Error, erlang:get_stacktrace()]),
        erlang:Type(Error)
    end.

make_nocatch(Module, RequestData) -> 
    RequestData1 = Module:init(RequestData),
    Bridge = sbw:new(Module, RequestData1),
    case simple_bridge_multipart:parse(Bridge) of
        {ok, PostParams, Files} -> 
            %% Post Params are read from the multipart parsing
            sbw:set_multipart(PostParams, Files, Bridge);
        {ok, not_multipart} -> 
            %% But if it's not a multipart post, then we need to manually tell
            %% simple bridge to cache the post params in the wrapper for quick
            %% lookup
            sbw:cache_post_params(Bridge);
        {error, Error} -> 
            Bridge:set_error(Error)
    end.


%% DEPRECATED STUFF BELOW
%%
%% please use application:start(simple_bridge),
%%            simple_bridge:start/0-2, or
%%            simple_bridge:make/2-3
make_request(Module, {Req = {mochiweb_request, _}, Docroot}) ->
    application:set_env(simple_bridge, document_root, Docroot),
    make_request(Module, Req);
make_request(Module, Req) ->
    FixedModule = fix_old_modules(Module),
    inner_make(FixedModule, Req).

make_response(cowboy_response_bridge, Bridge = #sbw{}) ->
    Bridge;
make_response(Module, {Req = {mochiweb_request, _}, Docroot}) ->
    application:set_env(simple_bridge, document_root, Docroot),
    make_response(Module, Req);
make_response(Module, Req) ->
    FixedModule = fix_old_modules(Module),
    inner_make(FixedModule, Req).


%%   wow
%%         much grow
%%
%%    such aesthetic
%%
%%        bridge modules so old
%%
%%      many deprecated
fix_old_modules(yaws_request_bridge) -> yaws_simple_bridge;
fix_old_modules(yaws_response_bridge) -> yaws_simple_bridge;
fix_old_modules(inets_request_bridge) -> inets_simple_bridge;
fix_old_modules(inets_response_bridge) -> inets_simple_bridge;
fix_old_modules(cowboy_request_bridge) -> cowboy_simple_bridge;
fix_old_modules(cowboy_response_bridge) -> cowboy_simple_bridge;
fix_old_modules(mochiweb_request_bridge) -> mochiweb_simple_bridge;
fix_old_modules(mochiweb_response_bridge) -> mochiweb_simple_bridge;
fix_old_modules(webmachine_request_bridge) -> webmachine_simple_bridge;
fix_old_modules(webmachine_response_bridge) -> webmachine_simple_bridge;
%% wow
%% 
%%     so long error msg
%%
%%  such descriptive
%%
%%      wow
%%
fix_old_modules(Other) -> throw({unknown_or_non_deprecated_bridge_module_specified_in_deprecated_call, {simple_bridge, make_request, [Other]}}).

