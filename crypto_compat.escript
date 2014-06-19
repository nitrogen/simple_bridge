#!/usr/bin/env escript
%% vim: ts=4 sw=4 et ft=erlang

main([]) ->
    crypto:start(),

	Filename = "include/crypto_compat.hrl",
	io:format("Generating crypto compatibility for simple_bridge...\n"),
	Hash = hash(),

	io:format("...Using: ~p~n",[Hash]),

	Contents = [
		"-define(HASH(Data), ",Hash,").\n"
	],

    ContentsBin = iolist_to_binary(Contents),
    case file:read_file(Filename) of
        {ok, ContentsBin} -> 
            io:format("...no changes needed to ~p. Skipping writing new file\n",[Filename]);
        _ -> 
            io:format("...writing ~p\n",[Filename]),
            file:write_file(Filename, Contents)
    end.

hash() ->
	case erlang:function_exported(crypto, hash, 2) of
		true ->
			"crypto:hash(sha, Data)";
		false ->
			"crypto:sha(Data)"
	end.
