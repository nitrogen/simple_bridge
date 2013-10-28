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
        URI = Bridge:uri(),
        IsStatic = simple_bridge_util:is_static_path(mochiweb, URI),
        case IsStatic of
            true ->
                Bridge2 = Bridge:set_response_file(URI),
                Bridge2:build_response();
            false ->
                Callout = simple_bridge_util:get_env(callout),
                Callout:run(Bridge)
        end
    catch
        T:E -> error_logger:error_msg("Error: ~p:~p~nStacktrace: ~p~n",[T, E, erlang:get_stacktrace()])
    end.
