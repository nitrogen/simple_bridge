-module(inets_simple_bridge_anchor).
-export ([do/1]).
	
do(Req) ->
	Bridge = simple_bridge:make(inets, Req),
	URI = Bridge:uri(),
	case simple_bridge_util:is_static_path(inets, URI) of
		true ->
			Bridge2 = Bridge:set_response_file(URI),
			Bridge2:build_response();
		false ->
			Callout = simple_bridge_util:get_env(callout),
			Callout:run(Bridge)
	end.
