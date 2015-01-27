-module(simple_bridge_test_handler).
-behaviour(simple_bridge_handler).
-export([run/1]).

run(Bridge) ->
	Path = sbw:path(Bridge),
	Body = run(Path, Bridge),
	Bridge2 = sbw:set_response_data(Body, Bridge),
	sbw:build_response(Bridge2).

run("/peer_ip", Bridge) -> simple_call(peer_ip, Bridge);
run("/protocol", Bridge) -> simple_call(protocol, Bridge);
run("/path", Bridge) -> simple_call(path, Bridge);
run("/uri", Bridge) -> simple_call(uri, Bridge);
run("/request_method_get", Bridge) -> simple_call(request_method, Bridge);
run("/request_method_post", Bridge) -> simple_call(request_method, Bridge);
run("/request_body", Bridge) -> simple_call(request_body, Bridge);
run("/query_params", Bridge) -> simple_call(query_params, Bridge);
run("/post_params", Bridge) -> simple_call(post_params, Bridge);
run(Path, _Bridge) -> io_lib:format("Unhandled Path: ~p", [Path]).

simple_call(Call, Bridge) ->
	Val = sbw:Call(Bridge),
	io_lib:format("~p", [Val]).
