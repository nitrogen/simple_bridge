-module(inets_simple_bridge_anchor).
-export ([do/1]).
	
do(Req) ->
	Bridge = simple_bridge:make(inets, Req),
	ReqPath = Bridge:path(),
	case simple_bridge_util:is_static_path(inets, ReqPath) of
		true ->
			Bridge2 = Bridge:set_response_file(ReqPath),
			Bridge2:build_response();
		false ->
			Callout = simple_bridge_util:get_env(callout),
			Callout:run(Bridge)
	end.
