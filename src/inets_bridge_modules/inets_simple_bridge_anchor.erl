-module(inets_simple_bridge_anchor).
-export ([do/1]).
	
do(Req) ->
	try
		Bridge = simple_bridge:make(inets, Req),
		ReqPath = Bridge:path(),
		case simple_bridge_util:is_static_path(inets, ReqPath) of
			true ->
				Bridge2 = Bridge:set_response_file(ReqPath),
				Bridge2:build_response();
			false ->
				Callout = simple_bridge_util:get_env(callout),
				case simple_bridge_websocket:attempt_hijacking(Bridge, Callout) of
					{hijacked, Bridge2} ->
						Bridge2:build_response();
					spared ->
						Callout:run(Bridge)
				end
		end
	catch
		T:E -> error_logger:error_msg("~p:~p~nStacktrace: ~p~n",[T,E,erlang:get_stacktrace()])
	end.
