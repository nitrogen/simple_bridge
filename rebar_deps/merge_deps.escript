#!/usr/bin/env escript
%% vim: ts=4 sw=4 et ft=erlang

main([SrcFile, DepsFile, DestFile]) ->
    io:format("Generating ~p~n", [DestFile]),



    io:format("Reading Base Config from ~p~n",[SrcFile]),
    {ok, Src} = file:consult(SrcFile),

    io:format("Reading Deps from ~p~n",[DepsFile]),
    {ok, [DepsToAdd]} = file:consult(DepsFile),

    BaseDeps = proplists:get_value(deps, Src, []),
    NewDeps = BaseDeps ++ DepsToAdd,

    io:format("Went from ~p deps to ~p deps~n",[length(BaseDeps), length(NewDeps)]),

    RemovedDeps = proplists:delete(deps, Src),
    Merged = RemovedDeps ++ [{deps, NewDeps}],
    Formatted = lists:map(fun(Term) ->
        io_lib:format("~tp.~n", [Term])
    end, Merged),

    file:write_file(DestFile, Formatted),
    io:format("Wrote new file at: ~p~n",[DestFile]).
