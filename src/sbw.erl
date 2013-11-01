% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% Copyright (c) 2013 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (sbw).
-include("simple_bridge.hrl").
-define(PASSTHROUGH(FunctionName),
	FunctionName(Wrapper) -> 
		(Wrapper#sbw.mod):FunctionName(Wrapper#sbw.req)).

%% REQUEST BRIDGE EXPORTS
-export([
	new/6,
	set_multipart/3,
	set_error/2,
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

	param/2,
	param/3,

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

%% REQUEST WRAPPERS

new(Mod, Req, IsMultiPart, PostParams, PostFiles, Error) ->
	Bridge = #sbw{
		mod=Mod,
		req=Req,
		is_multipart=IsMultiPart,
		post_params=PostParams,
		post_files=PostFiles,
		error=Error
	},
	Headers = pre_process_headers(Bridge:headers()),
	Bridge#sbw{headers=Headers}.



set_multipart(PostParams, PostFiles, Wrapper) ->
	Wrapper#sbw{
		is_multipart=true,
		post_params=PostParams,
		post_files=PostFiles
	}.

set_error(Error, Wrapper) ->
	Wrapper#sbw{
		error=Error
	}.

get_peername(Wrapper) ->
	inet:peername(socket(Wrapper)).

error(Wrapper) ->
	Wrapper#sbw.error.

?PASSTHROUGH(protocol).
?PASSTHROUGH(path).
?PASSTHROUGH(uri).
?PASSTHROUGH(peer_ip).
?PASSTHROUGH(peer_port).
?PASSTHROUGH(protocol_version).
?PASSTHROUGH(request_body).
?PASSTHROUGH(socket).

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

pre_process_headers(Headers) ->
	[pre_process_header(Header) || Header <- Headers].

pre_process_header({Key, Val}) ->
	Key2 = simple_bridge_util:deatomize_header(Key),
	Key3 = simple_bridge_util:binarize_header(Key2),
	Val2 = simple_bridge_util:to_binary(Val),
	{Key3, Val2}.

headers(Wrapper) when Wrapper#sbw.headers =:= [] ->
	Mod = Wrapper#sbw.mod,
	Req = Wrapper#sbw.req,
	[{K,V} || {K,V} <- Mod:headers(Req), V=/=undefined];
headers(Wrapper) ->
	Wrapper#sbw.headers.

header(Header, Wrapper) ->
	BinHeader = simple_bridge_util:binarize_header(Header),
	case lists:keyfind(BinHeader, 1, Wrapper#sbw.headers) of
		false -> undefined;
		{_, Val} ->
			if	is_list(Header);
				is_atom(Header)   -> binary_to_list(Val);
				is_binary(Header) -> Val
			end
	end.

header_lower(Header, Wrapper) when is_atom(Header); is_list(Header) ->
	string:to_lower(header(Header, Wrapper));
header_lower(Header, Wrapper) when is_binary(Header) ->
	list_to_binary(string:to_lower(binary_to_list(header(Header, Wrapper)))).

?PASSTHROUGH(cookies).
cookie(Cookie, Wrapper) ->
	Mod = Wrapper#sbw.mod,
	Req = Wrapper#sbw.req,
    case erlang:function_exported(Mod, cookie, 2) of
        true ->
            Mod:cookie(Cookie, Req);
        false ->
            Cookies = Mod:cookies(Req),
            proplists:get_value(Cookie, Cookies)
    end.

param_group(Param, Wrapper) ->
    param_group(Param, [], Wrapper).

param_group(Param, DefaultValue, Wrapper) ->
	case 	[V || {K, V} <- query_params(Wrapper), K == Param] 
		 ++ [V || {K, V} <- post_params(Wrapper), K == Param] of
		[] -> DefaultValue;
		L -> L
	end.

query_param_group(Param, DefaultValue, Wrapper) ->
	case [V || {K, V} <- query_params(Wrapper), K == Param] of
		[] -> DefaultValue;
		L -> L
	end.

query_param_group(Param, Wrapper) ->
    query_param_group(Param, [], Wrapper).

post_param_group(Param, Wrapper) ->
    post_param_group(Param, [], Wrapper).

post_param_group(Param, DefaultValue, Wrapper) ->
	case [V || {K, V} <- post_params(Wrapper), K == Param] of
		[] -> DefaultValue;
		L -> L
	end.    

?PASSTHROUGH(query_params).

query_param(Param, Wrapper) ->
    query_param(Param, undefined, Wrapper).

query_param(Param, DefaultValue, Wrapper) ->
    proplists:get_value(Param, query_params(Wrapper), DefaultValue).

post_params(Wrapper) ->
	Mod = Wrapper#sbw.mod,
	Req = Wrapper#sbw.req,
	IsMultipart = Wrapper#sbw.is_multipart,
	case {request_method(Wrapper), IsMultipart} of
		{'POST', true}  -> Wrapper#sbw.post_params;
		{'POST', false} -> Mod:post_params(Req);
		{'PUT', false} -> Mod:post_params(Req);
		_ -> []
	end.

post_param(Param, Wrapper) ->
    post_param(Param, undefined, Wrapper).

post_param(Param, DefaultValue, Wrapper) ->
    proplists:get_value(Param, post_params(Wrapper), DefaultValue).

param(Param, Wrapper) ->
    param(Param, undefined, Wrapper).

param(Param, DefaultValue, Wrapper) ->
    post_param(Param, query_param(Param, DefaultValue, Wrapper)).

post_files(Wrapper) ->
	Wrapper#sbw.post_files.

recv_from_socket(Length, Timeout, Wrapper) ->
	Mod = Wrapper#sbw.mod,
	Req = Wrapper#sbw.req,
    case erlang:function_exported(Mod, recv_from_socket, 3) of
        true ->  Mod:recv_from_socket(Length, Timeout, Req);
        false -> throw({not_supported, Mod, recv_from_socket})
    end.

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
    case re:run(Key, "^(\\w+)(?:\\[([\\w-\\[\\]]+)\\])?$", [{capture, all_but_first, list}]) of
        {match, [Key]} ->
            parse_deep_post_params(Rest, [{Key, Value}|Acc]);
        {match, [KeyName, Path]} ->
            PathList = re:split(Path, "\\]\\[", [{return, list}]),
            parse_deep_post_params(Rest, insert_into(Acc, [KeyName|PathList], Value))
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

set_header(Name, Value, Wrapper) ->
	update_response(fun(Res) ->
		Header = #header { name=Name, value=Value },
		Headers = Res#response.headers,
		Headers1 = [X || X <- Headers, X#header.name /= Name orelse X#header.name =:= "Set-Cookie"],
		Headers2 = [Header|Headers1],
		Res#response{headers=Headers2}
	end, Wrapper).

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


%% Backwards compatible calls below
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
