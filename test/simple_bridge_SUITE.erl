-module(simple_bridge_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
	all/0,
	groups/0,
	init_per_group/2,
	end_per_group/2
%	init_per_testcase/2,
%	end_per_testcase/2
]).

-export([
	peer_ip/1,
	protocol/1,
	path/1,
	uri/1,
	request_method_get/1,
	request_method_post/1,
	query_params/1,
	post_params/1
]).

all() -> [{group, main}].

groups() ->
	[{main, 
		[],
		%[parallel, {repeat, 10}],
		[peer_ip, request_method_get, request_method_post, protocol, path, query_params, post_params]
	}].

init_per_group(main, Config) ->
	io:format("CWD: ~p~n", [file:get_cwd()]),
	inets:start(),
	%% using 'undefined' will load it from the app.config
	simple_bridge:start(undefined, simple_bridge_test_handler),
	timer:sleep(10000),
	Config.

end_per_group(main, Config) ->
	inets:stop(),
	application:stop(simple_bridge),
	Config.


peer_ip(_) ->
	"{127,0,0,1}" = request("peer_ip").

protocol(_) ->
	"http" = request("protocol").

path(_) ->
	"\"/path\"" = request("path").

uri(_) ->
	"http://127.0.0.1:8000/uri" = request("uri").

request_method_get(_) ->
	"'GET'" = request("request_method_get").

request_method_post(_) ->
	"'POST'" = post("request_method_post", "").

query_params(_) ->
	"[{<<\"a\">>,<<\"1\">>},{<<\"b\">>,<<\"2\">>}]" = request("query_params?a=1&b=2").

post_params(_) ->
	"[{<<\"a\">>,<<\"1\">>},{<<\"b\">>,<<\"2\">>}]" = post("post_params", "a=1&b=2").

request(Path) ->
	URL = "http://127.0.0.1:8000/" ++ Path,
	{ok, {_, _, Val}} = httpc:request(URL),
	Val.

post(Path, Body) ->
	URL = "http://127.0.0.1:8000/" ++ Path,
	{ok, {_, _, Val}} = httpc:request(post, {URL, [], "", Body}, [], []),
	Val.
