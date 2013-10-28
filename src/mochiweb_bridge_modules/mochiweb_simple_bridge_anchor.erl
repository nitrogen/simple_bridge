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
        StaticPaths = simple_bridge_util:get_static_paths(mochiweb),
        URI = Bridge:uri(),
        IsStatic = lists:any(fun(StaticPath) ->
                                is_static_path(StaticPath, URI)
                             end, StaticPaths),
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

%% If URI Starts with the contents of StaticPath, then it is static
is_static_path(StaticPath, URI) ->
    StaticPathLength = length(StaticPath),
    case lists:sublist(URI, StaticPathLength) of
        StaticPath -> true;
        _ ->
            case lists:sublist(URI, StaticPathLength+1) of
                "/" ++ StaticPath -> true;
                _ -> false
            end
    end.
