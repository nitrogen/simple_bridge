% vim: ts=4 sw=4 et
-module(simple_bridge_util).
-include("simple_bridge.hrl").
-export([
    get_env/1,
    get_env/2,
    get_address_and_port/1,
    get_docroot/1,
    get_static_paths/1,
    get_docroot_and_static_paths/1,
    atomize_header/1,
    deatomize_header/1,
    binarize_header/1,
    expires/2,
    b2l/1,
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
    massage_websocket_reply/2
]).

-type header_key() :: string() | binary() | atom().
-type header() :: {header_key(), string()}.
-type header_list() :: [header()].

%% converts a Header to a lower-case, underscored version
%% ie. "X-Forwarded-For" -> x_forwarded_for

get_env(Key) ->
    get_env(Key, undefined).

get_env([], Default) ->
    Default;
get_env([{App,Key}|AppKeys], Default) ->
    case application:get_env(App,Key) of
        {ok, V} -> V;
        undefined -> get_env(AppKeys, Default)
    end;
get_env(Key, Default) when is_atom(Key) ->
    get_env([{simple_bridge, Key}], Default).

get_address_and_port(BackendApp) ->
    Address =   simple_bridge_util:get_env([{simple_bridge,address},
                                            {simple_bridge,bind_address},
                                            {BackendApp, address},
                                            {BackendApp,bind_address}],
                                            ?DEFAULT_IP),

    Port =      simple_bridge_util:get_env([{simple_bridge,port},
                                            {simple_bridge,bind_port},
                                            {BackendApp, port},
                                            {BackendApp, bind_port}],
                                            ?DEFAULT_PORT),
    {Address, Port}.

get_docroot(BackendApp) ->
    simple_bridge_util:get_env([{simple_bridge,document_root},
                                {BackendApp, document_root}],
                                ?DEFAULT_DOCROOT).

get_static_paths(BackendApp) ->
    simple_bridge_util:get_env([{simple_bridge,static_paths},
                                {BackendApp, static_paths}],
                                ?DEFAULT_STATIC_PATHS).

get_docroot_and_static_paths(BackendApp) ->
    DocRoot = get_docroot(BackendApp),
    StaticPaths = get_static_paths(BackendApp),
    {DocRoot, StaticPaths}.

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

deatomize_header(B) when is_binary(B) ->
    B;
deatomize_header(Header) when is_atom(Header) ->
    deatomize_header(atom_to_list(Header));
deatomize_header([$_|T]) ->
    [$-|deatomize_header(T)];
deatomize_header([H|T]) ->
    [H|deatomize_header(T)];
deatomize_header([]) ->
    [].

binarize_header(Header) when is_binary(Header) ->
    binarize_header(binary_to_list(Header));
binarize_header(Header) when is_atom(Header) ->
    binarize_header(atom_to_list(Header));
binarize_header(Header) when is_list(Header) ->
    list_to_binary(fix_header_caps(Header, capitalize)).

%% @doc changes a header to always capitalize the first letter, and also any
%% characters after dashes Example: <<"x-forwarded-for">> becomes
%% <<"X-Forwarded-For">>.
fix_header_caps([C|Rest], capitalize) when C>=$a,C=<$z ->
    %% we are ordered to capitalize the next character, and the next character
    %% happens to be lower case, so let's capitalize it
    [C-32 | fix_header_caps(Rest, normal)];
fix_header_caps([$-|Rest], _) ->
    %% The next character is a dash, so we need to tell it to capitalize the
    %% next character
    [$-,fix_header_caps(Rest, capitalize)];
fix_header_caps([C|Rest], _) ->
    %% Either the character is non-lower-case already or we don't have to deal
    %% with it anyway, so just move on.
    [C|fix_header_caps(Rest, normal)];
fix_header_caps([], _) -> [].


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
    to_lower(b2l(Header));
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
    {NowMegaSec,NowSec,_} = now(),
    ExpiresDate = calendar:now_to_local_time({NowMegaSec,NowSec+Seconds,0}),
    httpd_util:rfc1123_date(ExpiresDate).

-spec b2l(binary() | string()) -> string().
b2l(B) when is_binary(B) -> binary_to_list(B);
b2l(B) -> B.

-spec to_list(any()) -> string().
to_list(A) when is_atom(A) ->
    atom_to_list(A);
to_list(B) when is_binary(B) ->
    b2l(B);
to_list(L) when is_list(L) ->
    L.

-spec to_binary(iolist() | atom()) -> binary().
to_binary(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
to_binary(L) ->
    iolist_to_binary(L).

massage_websocket_reply({reply, Text}, _State) when is_list(Text); is_binary(Text) ->
    {reply, {binary, iolist_to_binary(Text)}};
massage_websocket_reply({reply, {binary, Text}}, _State) ->
    {reply, {binary, iolist_to_binary(Text)}};
massage_websocket_reply({reply, {text, Text}}, _State) ->
    {reply, {text, iolist_to_binary(Text)}}.
%%massage_websocket_reply({close, StatusCode}) when is_integer(StatusCode) ->
%%    {close, StatusCode};
%%massage_websocket_reply({close, {StatusCode, Reason}}) ->
%%    {close, {StatusCode, iolist_to_binary(Reason)}}.
%%massage_websocket_reply({close, StatusCode, Reply}) when is_integer(StatusCode) ->
%%    {close, StatusCode};
%%massage_websocket_reply({close, {StatusCode, Reason}}) ->
%%    {close, {StatusCode, iolist_to_binary(Reason)}};
