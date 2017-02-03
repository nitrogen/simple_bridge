% vim: ts=4 sw=4 et
-module(simple_bridge_util).
-include("simple_bridge.hrl").
-export([
    get_env/1,
    get_env/2,
    get_maybe_set_env/2,
    get_anchor_module/1,
    get_server_name/1,
    get_address_and_port/1,
    get_docroot/1,
    get_static_paths/1,
    get_docroot_and_static_paths/1,
    get_max_post_size/1,
    get_max_file_size/1,
    get_max_file_in_memory_size/1,
    get_websocket_keepalive_interval_timeout/1,
    get_scratch_dir/1,
    atomize_header/1,
    binarize_header/1,
    is_static_path/2,
    expires/2,
    to_list/1,
    to_binary/1,
    has_header/2,
    has_any_header/2,
    ensure_header/3,
    ensure_header/2,
    ensure_headers/2,
    default_static_expires_header/0,
    ensure_expires_header/1,
    needs_expires_header/1,
    parse_ip/1,
    parse_cookie_header/1
]).

-type header_key() :: string() | binary() | atom().
-type header() :: {header_key(), string()}.
-type header_list() :: [header()].

get_env(Key) ->
    get_env(Key, undefined).

get_env([], Default) ->
    Default;
get_env([{'INIT-ARG', Key}|AppKeys], Default) ->
    case init:get_argument(Key) of
        {ok, [[Value]]} -> Value;
        _ -> get_env(AppKeys, Default)
    end;
get_env([{App,Key}|AppKeys], Default) ->
    case application:get_env(App,Key) of
        {ok, V} -> V;
        undefined -> get_env(AppKeys, Default)
    end;
get_env(Key, Default) when is_atom(Key) ->
    get_env([{simple_bridge, Key}], Default).

-spec get_maybe_set_env(Var :: atom(), Value :: any()) -> any().
get_maybe_set_env(Var, undefined) ->
    get_env(Var);
get_maybe_set_env(Var, Value) ->
    application:set_env(simple_bridge, Var, Value),
    Value.

get_anchor_module(BackendApp) ->
    get_env([{simple_bridge, anchor},
             {BackendApp, anchor}],
            list_to_atom(atom_to_list(BackendApp) ++ "_simple_bridge_anchor")).

get_server_name(BackendApp) ->
    get_env([{simple_bridge, server_name},
             {BackendApp, server_name}],
             "simple_bridge").

get_address_and_port(BackendApp) ->
    Address = get_env([{simple_bridge,address},
                       {simple_bridge,bind_address},
                       {BackendApp, address},
                       {BackendApp,bind_address}],
                       ?DEFAULT_IP),

    Port =    get_env([{simple_bridge,port},
                       {simple_bridge,bind_port},
                       {BackendApp, port},
                       {BackendApp, bind_port}],
                       ?DEFAULT_PORT),
    {Address, Port}.

get_docroot(BackendApp) ->
    get_env([{simple_bridge,document_root},
             {BackendApp, document_root}],
             ?DEFAULT_DOCROOT).

get_static_paths(BackendApp) ->
    get_env([{simple_bridge,static_paths},
             {BackendApp, static_paths}],
             ?DEFAULT_STATIC_PATHS).

get_docroot_and_static_paths(BackendApp) ->
    DocRoot = get_docroot(BackendApp),
    StaticPaths = get_static_paths(BackendApp),
    {DocRoot, StaticPaths}.

get_env_or_init(ConfigVar, InitVar, Default) ->
    case get_env([{simple_bridge, ConfigVar},
                  {'INIT-ARG', InitVar}]) of
        undefined -> Default;
        Other -> Other
    end.

get_websocket_keepalive_interval_timeout(BackendApp) ->
    Interval = get_websocket_keepalive_interval(BackendApp),
    Timeout = get_websocket_keepalive_timeout(BackendApp),
    {Interval, Timeout}.

get_websocket_keepalive_interval(BackendApp) ->
    get_env([{simple_bridge, websocket_interval},
             {BackendApp, websocket_interval}], 10000).

get_websocket_keepalive_timeout(BackendApp) ->
    get_env([{simple_bridge, websocket_timeout},
             {BackendApp, websocket_timeout}], 5000).

get_scratch_dir(Default) ->
    get_env_or_init(scratch_dir, simple_bridge_scratch_dir, Default).

get_max_size(ConfigVar, InitVar, Default) ->
    Size = case get_env_or_init(ConfigVar, InitVar, Default) of
        L when is_list(L) -> list_to_integer(L);
        I when is_integer(I); is_float(I) -> I
    end,
    Size * 1024 * 1024.

get_max_post_size(Default) ->
    get_max_size(max_post_size, simple_bridge_max_post_size, Default).

get_max_file_size(Default) ->
    get_max_size(max_file_size, simple_bridge_max_file_size, Default).

get_max_file_in_memory_size(Default) ->
    get_max_size(max_file_in_memory_size, simple_bridge_max_file_in_memory_size, Default).

is_static_path(Backend, URI) ->
    StaticPaths = get_static_paths(Backend),
    lists:any(fun(StaticPath) ->
        StaticPathLength = length(StaticPath),
        case lists:sublist(URI, StaticPathLength) of
            StaticPath -> true;
            _ ->
                case lists:sublist(URI, StaticPathLength+1) of
                    "/" ++ StaticPath -> true;
                    _ -> false
                end
         end
    end, StaticPaths).


atomize_header(Header) when is_binary(Header) ->
    atomize_header(binary_to_list(Header));
atomize_header(Header) when is_atom(Header) ->
    atomize_header(atom_to_list(Header));
atomize_header(Header) when is_list(Header) ->
    LowerUnderscore = fun(H) ->
        if
            H >= 65 andalso H =< 90 ->
                H + 32; % Convert "A" to "a" by adding 32 to its ASCII val
            H == 45 ->
                95; %% convert "-" to "_"
            true -> H
        end
    end,
    list_to_atom(lists:map(LowerUnderscore,Header)).

binarize_header(Header) when is_binary(Header) ->
    binarize_header(binary_to_list(Header));
binarize_header(Header) when is_atom(Header) ->
    Header1 = string:to_lower(atom_to_list(Header)),
    Header2 = lists:map(fun($_) -> $-; (C) -> C end, Header1),
    list_to_binary(Header2);
binarize_header(Header) when is_list(Header) ->
    list_to_binary(string:to_lower(Header)).

%% TODO: All this below "ensure_header" stuff needs to be reworked.  Since
%% headers are now normalized before being inserted into the response header
%% list, it will be easier to check for existence, and shouldn't require all
%% this conversion that's being done right here.
%%
%% Checks if `Header` exists as a key in `HeaderList`
%% if it doesn't, inserts it with the value `Value`
-spec ensure_header(header_list(), {header_key(), term()}) -> header_list().
ensure_header(HeaderList,{Header,Value}) ->
    ensure_header(HeaderList,Header,Value).

-spec ensure_header(header_list(), header_key(), term()) -> header_list().
ensure_header(HeaderList,Header,Value) ->
    case has_header(HeaderList,Header) of
        true -> HeaderList;
        false -> [{Header,Value} | HeaderList]
    end.

-spec ensure_headers(header_list(), header_list()) -> header_list().
ensure_headers(HeaderList,HeadersToEnsure) ->
    LowerList = lower_keys(HeaderList),
    lists:foldl(fun({Header,Value},NewHeaderList) ->
        case has_lower_header(LowerList, Header) of
            true -> NewHeaderList;
            false -> [{Header,Value} | NewHeaderList]
        end
    end,HeaderList,HeadersToEnsure). 

-spec ensure_expires_header(header_list()) -> header_list().
ensure_expires_header(HeaderList) ->
    case needs_expires_header(HeaderList) of
        true ->
            Expires = default_static_expires_header(),
            [Expires | HeaderList];
        false ->
            HeaderList
    end.
    
-spec needs_expires_header(header_list()) -> boolean().
needs_expires_header(HeaderList) ->
    not(has_any_header(HeaderList,["Expires","Cache-Control"])).

-spec has_header(header_list(), header_key()) -> boolean().
has_header(HeaderList,Header) ->
    LowerKeys = lower_keys(HeaderList),
    has_lower_header(LowerKeys, Header).

-spec has_any_header(header_list(), [header_key()]) -> boolean().
has_any_header(HeaderList,HeadersToCheck) ->
    LowerKeys = lower_keys(HeaderList),
    lists:any(fun(Key) -> has_lower_header(LowerKeys,Key) end,HeadersToCheck).

-spec has_lower_header([string()], header_key()) -> boolean().
has_lower_header(HeaderLowerKeyList, Header) ->
    HeaderLower = to_lower(Header),
    lists:member(HeaderLower, HeaderLowerKeyList).

-spec lower_keys(header_list()) -> [string()].
lower_keys(HeaderList) ->
    [to_lower(Key) || {Key,_} <- HeaderList].

-spec to_lower(header_key()) -> string().
to_lower(Header) when is_atom(Header) ->
    to_lower(atom_to_list(Header));
to_lower(Header) when is_binary(Header) ->
    to_lower(binary_to_list(Header));
to_lower(Header) when is_list(Header) ->
    string:to_lower(Header).

-spec default_static_expires_header() -> header().
default_static_expires_header() ->
    case application:get_env(simple_bridge,default_expires) of
        {ok, immediate} ->
            {"Cache-control","no-cache"};

        {ok, Seconds} when is_integer(Seconds) ->
            Expires = expires(seconds,Seconds),
            {"Expires", Expires};

        {ok, {Unit, Value}} when Unit==years orelse 
                                 Unit==months orelse
                                 Unit==weeks orelse
                                 Unit==days orelse
                                 Unit==hours orelse
                                 Unit==minutes orelse
                                 Unit==seconds ->
            Expires = expires(Unit,Value),
            {"Expires", Expires};
        _ -> 
            Expires = expires(years,10),
            {"Expires", Expires}
    end.

-type unit_of_time() :: years|months|weeks|days|hours|minuites|seconds.
-spec expires(unit_of_time(), integer()) -> string().
expires(years,X) when is_integer(X) ->
    make_expires_from_seconds(X*31536000);
expires(months,X) when is_integer(X) ->
    make_expires_from_seconds(X*2592000);
expires(weeks,X) when is_integer(X) ->
    make_expires_from_seconds(X*604800);
expires(days,X) when is_integer(X) ->
    make_expires_from_seconds(X*86400);
expires(hours,X) when is_integer(X) ->
    make_expires_from_seconds(X*3600);
expires(minutes,X) when is_integer(X) ->
    make_expires_from_seconds(X*60);
expires(seconds,X) when is_integer(X) ->
    make_expires_from_seconds(X).

-spec make_expires_from_seconds(integer()) -> string().
make_expires_from_seconds(Seconds) ->
    {NowMegaSec,NowSec,_} = os:timestamp(),
    ExpiresDate = calendar:now_to_local_time({NowMegaSec,NowSec+Seconds,0}),
    httpd_util:rfc1123_date(ExpiresDate).


-spec to_list(any()) -> string().
to_list(A) when is_atom(A) ->
    atom_to_list(A);
to_list(B) when is_binary(B) ->
    binary_to_list(B);
to_list(L) when is_list(L) ->
    L.

-spec to_binary(iolist() | atom() | binary()) -> binary().
to_binary(B) when is_binary(B) ->
    B;
to_binary(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
to_binary(L) ->
    iolist_to_binary(L).



%% This is borrowed from Nitrogen
parse_ip(IP = {_,_,_,_}) ->
    IP;
parse_ip(IP = {_,_,_,_,_,_,_,_}) ->
    IP;
parse_ip(Binary) when is_binary(Binary) ->
    parse_ip(binary_to_list(Binary));
parse_ip(String) ->
    case parse_address(String) of
        {ok, IP} -> IP;
        {error, _} -> undefined
    end.

%% This should just be inet:parse_address, but because it's so new, older
%% versions of erlang fail on it
parse_address(String) ->
    inet_parse:address(String).

parse_cookie_header(CookieData) ->
    F = fun(Cookie) ->
        case string:tokens(Cookie, "=") of
            [] -> [];
            L -> 
                X = string:strip(hd(L)),
                Y = string:join(tl(L), "="),
                {X, Y}
        end
    end,
    [F(X) || X <- string:tokens(CookieData, ";")].
