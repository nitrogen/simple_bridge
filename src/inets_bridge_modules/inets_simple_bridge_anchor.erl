-module(inets_simple_bridge_anchor).
-export ([do/1]).
	
do(Req) ->
	Bridge = simple_bridge:make(inets, Req),
	ReqPath = sbw:path(Bridge),
	case simple_bridge_util:is_static_path(inets, ReqPath) of
		true ->
			Bridge2 = sbw:set_response_file(ReqPath, Bridge),
			sbw:build_response(Bridge2);
		false ->
			Handler = simple_bridge_util:get_env(handler),
			case simple_bridge_websocket:attempt_hijacking(Bridge, Handler) of
				{hijacked, closed} ->
					done;
				{hijacked, Bridge2} ->
					sbw:build_response(Bridge2);
				spared ->
					Handler:run(Bridge)
			end
	end.
