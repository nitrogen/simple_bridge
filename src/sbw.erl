% vim: ts=4 sw=4 et
% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% Copyright (c) 2013 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (sbw).
-include("simple_bridge.hrl").

%% REQUEST BRIDGE EXPORTS
-export([
    new/2,
    set_multipart/3,
    set_error/2,
    cache_post_params/1,
    protocol/1,
    path/1,
    uri/1,
    peer_ip/1,
    peer_port/1,
    get_peername/1,
    error/1,
    socket/1,
    protocol_version/1,
    request_body/1,
    request_method/1,

    headers/1,
    header/2,
    header_lower/2,

    cookies/1,
    cookie/2,

    query_params/1,
    query_param/2,
    query_param/3,

    post_params/1,
    post_param/2,
    post_param/3,
    post_param_group/2,
    post_param_group/3,

    param_group/2,
    param_group/3,
    query_param_group/2,
    query_param_group/3,

    params/1,
    param/2,
    param/3,

    deep_params/1,
    deep_param/2,

    deep_query_params/1,
    deep_query_param/2,

    deep_post_params/1,
    deep_post_param/2,

    post_files/1,
    recv_from_socket/3,
    insert_into/3
]).

%% RESPONSE BRIDGE EXPORTS
-export([
    set_status_code/2,
    set_header/3,
    get_response_header/2,
    clear_headers/1,
    set_cookie/3,
    set_cookie/5,
    clear_cookies/1,
    set_response_data/2,
    set_response_file/2,
    build_response/1,

    %% retained for backwards compatibility
    status_code/2,
    header/3,
    cookie/3,
    cookie/5,
    data/2,
    file/2
]).

%% TODO: Add Typespecs to all these

%% REQUEST WRAPPERS

new(Mod, Req) ->
    Bridge = #sbw{
        mod=Mod,
        req=Req
    },
    %% We don't cache the Post params here because the multipart parser might
    %% do it for us. See simple_bridge:make_nocatch/2
    Bridge2 = cache_headers(Bridge),
    Bridge3 = cache_query_params(Bridge2),
    _Bridge4 = cache_cookies(Bridge3).


set_multipart(PostParams, PostFiles, Wrapper) ->
    Wrapper#sbw{
        is_multipart=true,
        post_params=PostParams,
        post_files=PostFiles
    }.

%% PRECACHING HEADERS, POST PARAMS AND QUERY PARAMS
%% So we don't have to convert to and from binary/lists/atom for different
%% backends. We do it once per request.

cache_headers(Wrapper) ->
    Mod = Wrapper#sbw.mod,
    Req = Wrapper#sbw.req,
    FormattedHeaders = [normalize_header({K,V}) || {K,V} <- Mod:headers(Req), V=/=undefined],
    Wrapper#sbw{headers=FormattedHeaders}.

cache_cookies(Wrapper) ->
    Mod = Wrapper#sbw.mod,
    Req = Wrapper#sbw.req,
    Wrapper#sbw{
        cookies=[normalize_header({K,V}) || {K,V} <- Mod:cookies(Req), V=/=undefined]
    }.

normalize_header({Key0, Val0}) ->
    Key = simple_bridge_util:binarize_header(Key0),
    Val = simple_bridge_util:to_binary(Val0),
    {Key, Val}.

cache_post_params(Wrapper) ->
    Mod = Wrapper#sbw.mod,
    Req = Wrapper#sbw.req,
    PostParams = Mod:post_params(Req),
    Wrapper#sbw{
        post_params=[normalize_param(Param) || Param <- PostParams]
    }.

cache_query_params(Wrapper) ->
    Mod = Wrapper#sbw.mod,
    Req = Wrapper#sbw.req,
    Wrapper#sbw{
        query_params=[normalize_param(Param) || Param <- Mod:query_params(Req)]
    }.


normalize_param({K, V}) ->  
    {simple_bridge_util:to_binary(K), simple_bridge_util:to_binary(V)}.

set_error(Error, Wrapper) ->
    Wrapper#sbw{
        error=Error
    }.

get_peername(Wrapper) ->
    inet:peername(socket(Wrapper)).

error(Wrapper) ->
    Wrapper#sbw.error.

-define(PASSTHROUGH(FunctionName),
    FunctionName(Wrapper) -> 
        (Wrapper#sbw.mod):FunctionName(Wrapper#sbw.req)).

?PASSTHROUGH(protocol).
?PASSTHROUGH(path).
?PASSTHROUGH(uri).
?PASSTHROUGH(peer_ip).
?PASSTHROUGH(peer_port).
?PASSTHROUGH(protocol_version).
?PASSTHROUGH(socket).

request_body(Wrapper) ->
    Mod = Wrapper#sbw.mod,
    Req = Wrapper#sbw.req,
    Body = Mod:request_body(Req),
    simple_bridge_util:to_binary(Body).

request_method(Wrapper) ->
    Mod = Wrapper#sbw.mod,
    Req = Wrapper#sbw.req,
    case Mod:request_method(Req) of
        Method when is_binary(Method) ->
            list_to_atom(binary_to_list(Method));
        Method when is_list(Method) ->
            list_to_atom(Method);
        Method when is_atom(Method) ->
            Method
     end.


recv_from_socket(Length, Timeout, Wrapper) ->
    Mod = Wrapper#sbw.mod,
    Req = Wrapper#sbw.req,
    case erlang:function_exported(Mod, recv_from_socket, 3) of
        true ->  Mod:recv_from_socket(Length, Timeout, Req);
        false -> throw({not_supported, Mod, recv_from_socket})
    end.

%% FILES

post_files(Wrapper) ->
    Wrapper#sbw.post_files.

%% REQUEST HEADERS

headers(Wrapper) ->
    Wrapper#sbw.headers.

header(Header, Wrapper) ->
    BinHeader = simple_bridge_util:binarize_header(Header),
    case lists:keyfind(BinHeader, 1, Wrapper#sbw.headers) of
        false -> undefined;
        {_, Val} ->
            if  is_list(Header);
                is_atom(Header)   -> binary_to_list(Val);
                is_binary(Header) -> Val
            end
    end.

header_lower(Header, Wrapper) ->
    case header(Header, Wrapper) of
        undefined ->
            undefined;
        Other when is_binary(Header) ->
            list_to_binary(string:to_lower(Other));
        Other when is_atom(Header); is_list(Header) ->
            string:to_lower(Other)
    end.

%% REQUEST COOKIES

cookies(Wrapper) ->
    Wrapper#sbw.cookies.

cookie(Cookie, Wrapper) ->
    BinCookie = list_to_binary(string:to_lower(simple_bridge_util:to_list(Cookie))),
    case lists:keyfind(BinCookie, 1, Wrapper#sbw.cookies) of
        false -> undefined;
        {_, Val} ->
            if  is_list(Cookie);
                is_atom(Cookie)   -> binary_to_list(Val);
                is_binary(Cookie) -> Val
            end
    end.

%% PARAM GROUPS

param_group(Param, Wrapper) ->
     param_group(Param, [], Wrapper).

param_group(Param, DefaultValue, Wrapper) ->
    query_param_group(Param, DefaultValue, Wrapper) ++ post_param_group(Param, DefaultValue, Wrapper).

query_param_group(Param, DefaultValue, Wrapper) ->
    find_param_group(Param, DefaultValue, query_params(Wrapper)).

query_param_group(Param, Wrapper) ->
    query_param_group(Param, [], Wrapper).

post_param_group(Param, Wrapper) ->
    post_param_group(Param, [], Wrapper).

post_param_group(Param, DefaultValue, Wrapper) ->
    find_param_group(Param, DefaultValue, post_params(Wrapper)).

%% QUERY PARAMS

query_params(Wrapper) ->
    Wrapper#sbw.query_params.

query_param(Param, Wrapper) ->
    query_param(Param, undefined, Wrapper).

query_param(Param, DefaultValue, Wrapper) ->
    find_param(Param, DefaultValue, query_params(Wrapper)).

%% POST PARAMS

post_params(Wrapper) ->
    Wrapper#sbw.post_params.

post_param(Param, Wrapper) ->
    post_param(Param, undefined, Wrapper).

post_param(Param, DefaultValue, Wrapper) ->
    find_param(Param, DefaultValue, post_params(Wrapper)).

%% AGNOSTIC PARAMS

params(Wrapper) ->
    post_params(Wrapper) ++ query_params(Wrapper).

param(Param, Wrapper) ->
    param(Param, undefined, Wrapper).

param(Param, DefaultValue, Wrapper) ->
    case post_param(Param, Wrapper) of
        undefined -> query_param(Param, DefaultValue, Wrapper);
        V -> V
    end.

%% FIND PARAM

find_param(Param, Default, ParamList) when is_binary(Param) ->
    case lists:keyfind(Param, 1, ParamList) of
        {_, Val} -> Val;
        false -> Default
    end;
find_param(Param, Default, ParamList) when is_atom(Param); is_list(Param) ->
    Param1 = simple_bridge_util:to_binary(Param),
    simple_bridge_util:to_list(find_param(Param1, Default, ParamList)).


find_param_group(Param, Default, ParamList) when is_binary(Param) ->
    case [V || {K,V} <- ParamList, K=:=Param] of
        [] -> Default;
        L -> L
    end;
find_param_group(Param, Default, ParamList) when is_list(Param); is_atom(Param) ->
    Param1 = simple_bridge_util:to_binary(Param),
    ResultList = find_param_group(Param1, Default, ParamList),
    [simple_bridge_util:to_list(V) || V <- ResultList].

%% DEEP PARAM STUFF

deep_params(Wrapper) ->
    Params = params(Wrapper),
    parse_deep_post_params(Params, []).

deep_param(Path, Wrapper) ->
    find_deep_post_param(Path, deep_params(Wrapper)).

deep_query_params(Wrapper) ->
    Params = query_params(Wrapper),
    parse_deep_post_params(Params, []).

deep_query_param(Path, Wrapper) ->
    find_deep_post_param(Path, deep_query_params(Wrapper)).

deep_post_params(Wrapper) ->
    Params = post_params(Wrapper),
    parse_deep_post_params(Params, []).

deep_post_param(Path, Wrapper) ->
    find_deep_post_param(Path, deep_post_params(Wrapper)).

find_deep_post_param([], Params) ->
    Params;
find_deep_post_param([Index|Rest], Params) when is_integer(Index) ->
    find_deep_post_param(Rest, lists:nth(Index, Params));
find_deep_post_param([Index|Rest], Params) when is_list(Index) ->
    find_deep_post_param(Rest, proplists:get_value(Index, Params)).

parse_deep_post_params([], Acc) ->
    Acc;
parse_deep_post_params([{Key, Value}|Rest], Acc) ->
    case re:run(Key, "^(\\w+)(?:\\[([\\w-\\[\\]]*)\\])?$", [{capture, all_but_first, list}]) of
        {match, [Key]} ->
            parse_deep_post_params(Rest, [{Key, Value}|Acc]);
        {match, [KeyName, Path]} ->
            PathList = re:split(Path, "\\]\\[", [{return, list}]),
            parse_deep_post_params(Rest, insert_into(Acc, [KeyName|PathList], Value));
        Other ->
            error_logger:warning_msg("Unable to parse key: ~p. Returned: ~p", [Key, Other]),
            parse_deep_post_params(Rest, Acc)
    end.

insert_into(_List, [], Value) ->
    Value;
insert_into(undefined, PathList, Value) ->
    insert_into([], PathList, Value);
insert_into(N, PathList, Value) when is_integer(N) ->
    insert_into([], PathList, Value);
insert_into(List, [ThisKey|Rest], Value) ->
    case catch list_to_integer(ThisKey) of
        {'EXIT', _} ->
            ExistingVal = proplists:get_value(ThisKey, List),
            [{ThisKey, insert_into(ExistingVal, Rest, Value)}|
                proplists:delete(ThisKey, List)];
        N when N < erlang:length(List) ->
            ExistingVal = lists:nth(N+1, List),
            lists:sublist(List, N) ++ [insert_into(ExistingVal, Rest, Value)|
                lists:nthtail(N+1, List)];
        N when N >= erlang:length(List) ->
            List ++ lists:reverse([insert_into(undefined, Rest, Value)|
                    lists:seq(0, N - erlang:length(List) - 1)])
    end.

%% RESPONSE WRAPPERS

set_status_code(StatusCode, Wrapper) ->
    update_response(fun(Res) ->
        Res#response{status_code=StatusCode}
    end, Wrapper).

set_header(Name0, Value, Wrapper) ->
    Name = simple_bridge_util:binarize_header(Name0),
    update_response(fun(Res) ->
        Header = #header { name=Name, value=Value },
        Headers = Res#response.headers,
        Headers1 = [X || X <- Headers, X#header.name /= Name orelse X#header.name =:= <<"set-cookie">>],
        Headers2 = [Header|Headers1],
        Res#response{headers=Headers2}
    end, Wrapper).

get_response_header(Name0, Wrapper) ->
    Name = simple_bridge_util:binarize_header(Name0),
    Res = Wrapper#sbw.response,
    Headers = [H#header.value || H <- Res#response.headers, H#header.name == Name],
    case Headers of
        [] -> undefined;
        [V] -> V
    end.

clear_headers(Wrapper) ->
    update_response(fun(Res) ->
        Res#response{headers=[]}
    end, Wrapper).

set_cookie(Name, Value, Wrapper) ->
    set_cookie(Name, Value, "/", 20, Wrapper).

set_cookie(Name, Value, Path, MinutesToLive, Wrapper) ->
    update_response(fun(Res) ->
        Cookie = #cookie { name=Name, value=Value, path=Path, minutes_to_live=MinutesToLive },
        Cookies = Res#response.cookies,
        Cookies1 = [X || X <- Cookies, X#cookie.name /= Name],
        Cookies2 = [Cookie|Cookies1],
        Res#response{cookies=Cookies2}
    end, Wrapper).

clear_cookies(Wrapper) ->
    update_response(fun(Res) ->
        Res#response{cookies=[]}
    end, Wrapper).

set_response_data(Data, Wrapper) ->
    update_response(fun(Res) ->
        Res#response{data={data, Data}}
    end, Wrapper).

set_response_file(Path, Wrapper) ->
    update_response(fun(Res) ->
        Res#response{data={file,Path}}
    end, Wrapper).

build_response(Wrapper) ->
    Mod = Wrapper#sbw.mod,
    Req = Wrapper#sbw.req,
    Res = Wrapper#sbw.response,
    Mod:build_response(Req,Res).

update_response(Fun, Wrapper) ->
    NewRes = Fun(Wrapper#sbw.response),
    Wrapper#sbw{response=NewRes}.


%% DEPRECATED RESPONSE WRAPPERS

status_code(StatusCode, Wrapper) ->
    set_status_code(StatusCode, Wrapper).

header(Name, Value, Wrapper) ->
    set_header(Name, Value, Wrapper).

cookie(Name, Value, Wrapper) ->
    set_cookie(Name, Value, Wrapper).

cookie(Name, Value, Path, MinutesToLive, Wrapper) ->
    set_cookie(Name, Value, Path, MinutesToLive, Wrapper).

data(Data, Wrapper) ->
    set_response_data(Data, Wrapper).

file(File, Wrapper) ->
    set_response_file(File, Wrapper).
