%% vim: ts=4 sw=4 et
-module(mochiweb_simple_bridge_anchor).
-export ([
        loop/1
    ]).

%% We catch this because if it fails otherwise, Mochiweb just silently handles
%% it, apparently, and we'd rather see the error. So print it to the console.
loop(Req) ->
    try
        Bridge = simple_bridge:make(mochiweb, Req),
        ReqPath = Bridge:path(),
        IsStatic = simple_bridge_util:is_static_path(mochiweb, ReqPath),
        case IsStatic of
            true ->
                Bridge2 = Bridge:set_response_file(ReqPath),
                Bridge2:build_response();
            false ->
                Callout = simple_bridge_util:get_env(callout),
                case simple_bridge_websocket:attempt_hijacking(Bridge, Callout) of
                    {hijacked, closed} ->
                        gen_tcp:close(Bridge:socket());
                    {hijacked, Bridge2} ->
                        Bridge2:build_response();
                    spared ->
                        Callout:run(Bridge)
                end
        end
    catch
        exit:normal -> exit(normal);
        T:E -> error_logger:error_msg("Error: ~p:~p~nStacktrace: ~p~n",[T, E, erlang:get_stacktrace()])
    end.
