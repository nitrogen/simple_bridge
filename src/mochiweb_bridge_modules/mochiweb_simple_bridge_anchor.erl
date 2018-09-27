%% vim: ts=4 sw=4 et
-module(mochiweb_simple_bridge_anchor).
-include("simple_bridge.hrl").
-export ([
        loop/1
    ]).

%% We catch this because if it fails otherwise, Mochiweb just silently handles
%% it, and we'd rather see the error. So print it to the console.
loop(Req) ->
    try
        Bridge = simple_bridge:make(mochiweb, Req),
        ReqPath = sbw:path(Bridge),
        IsStatic = simple_bridge_util:is_static_path(mochiweb, ReqPath),
        case IsStatic of
            true ->
                Bridge2 = sbw:set_response_file(ReqPath, Bridge),
                sbw:build_response(Bridge2);
            false ->
                Handler = simple_bridge_util:get_env(handler),
                case simple_bridge_websocket:attempt_hijacking(Bridge, Handler) of
                    {hijacked, closed} ->
                        gen_tcp:close(sbw:socket(Bridge)),
                        exit(normal);
                    {hijacked, Bridge2} ->
                        sbw:build_response(Bridge2);
                    spared ->
                        Handler:run(Bridge)
                end
        end
    catch
        exit:normal -> exit(normal);
        T:E -> error_logger:error_msg("Error: ~p:~p~nStacktrace: ~p~n",[T, E, erlang:get_stacktrace()])
    end.
