% Simple Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge_multipart).
-export ([update_for_multipart_request/1]).

% Alas, so many Erlang HTTP Servers, and so little parsing of Multipart forms.
% This file contains multipart form parsing logic that is shared by all 
% request bridges.
% Large portions of this file are from mochiweb_multipart.erl
% Copyright 2007 Mochi Media, Inc., written by Bob Ippolito <bob@mochimedia.com>.

-define(CHUNKSIZE, 4096).
-define(IDLE_TIMEOUT, 30000).
-define (SCRATCH_DIR, "./scratch").
-record(mp, {state, boundary, length, buffer, callback}).
-record(state, {queryargs = [], filename=undefined, localfiledata=undefined}).

update_for_multipart_request(Req) ->
	try
		case is_multipart_request(Req) of
			true -> 
				io:format("Got a multipart request!~n"),
				io:format("Results: ~p~n", [parse_upload(Req)]);
			false -> 
				io:format("Normal request~n")
		end
		
	catch Type : Reason ->
		io:format("~p~n~p~n~p~n", [Type, Reason, erlang:get_stacktrace()])
	end,
	Req.

is_multipart_request(Req) ->
	Headers = Req:headers(),
	ContentType = proplists:get_value(content_type, Headers),
	
	case ContentType of
		"multipart/form-data" ++ _ -> true;
		_ -> false
	end.
	
parse_upload(Req) -> 
	ScratchDir = ?SCRATCH_DIR,
	file:make_dir(ScratchDir),
	TempFileName = get_tempfilename(),
	LocalFileData = filename:join(ScratchDir, TempFileName),
 	State = #state { localfiledata=LocalFileData },
	Callback = fun(X) -> callback(X, undefined, State, Req) end,
	parse(Callback, Req).

parse(Callback, Req) ->
	% Figure out the boundary and length...
	Length = list_to_integer(Req:header(content_length)),
	Boundary = iolist_to_binary(get_boundary(Req:header(content_type))),
	Prefix = <<"\r\n--", Boundary/binary>>,
	
	% Get whatever yaws/mochiweb/inets has already read...
	Chunk = Req:request_body(),
	HasChunk = 
		Chunk /= undefined andalso 
		((is_binary(Chunk) andalso size(Chunk) > 0) orelse (is_list(Chunk) andalso length(Chunk) > 0)),
	Chunk1 = case HasChunk of
		true  ->  to_binary(Chunk);
		false ->  Req:read_chunk(Length, Req)
	end,
	
	% Parse the first chunk...
	Length1 = Length - size(Chunk1),
	BS = size(Boundary),
	<<"--", Boundary:BS/binary, "\r\n", Rest/binary>> = Chunk1,
	
	% Parse the headers...
	MP = #mp{boundary=Prefix, length=Length1, buffer=Rest, callback=Callback},
	feed_mp(headers, MP, Req).

feed_mp(headers, State=#mp{buffer=Buffer, callback=Callback}, Req) ->
	% Find the end of the headers...
	FindResult = find_in_binary(<<"\r\n\r\n">>, Buffer),
	{State1, P} = case FindResult of
		{exact, N} -> 
			{State, N};
		_ ->
		  S1 = read_more(State, Req),
		  %% Assume headers must be less than ?CHUNKSIZE
		  {exact, N} = find_in_binary(<<"\r\n\r\n">>, S1#mp.buffer),
		  {S1, N}
	end,
	
	% Split out the headers...
	<<Headers:P/binary, "\r\n\r\n", Rest/binary>> = State1#mp.buffer,
	
	% Call back with headers...
	NextCallback = Callback({headers, parse_headers(Headers)}),
	
	% Parse the body...
	feed_mp(body, State1#mp{buffer=Rest, callback=NextCallback}, Req);
	
feed_mp(body, State=#mp{boundary=Prefix, buffer=Buffer, callback=Callback}, Req) ->
	FindResult = find_boundary(Prefix, Buffer),
	case FindResult of
		{end_boundary, Size, Skip} ->
			% Feed data into the callback function...
			<<Data:Size/binary, _:Skip/binary, _Rest/binary>> = Buffer,
			C1 = Callback({body, Data}),
			
			% Finish...
			C2 = C1(body_end),
			C2(eof);
		
		{next_boundary, Size, Skip} ->
			% Feed data into the callback function...
			<<Data:Size/binary, _:Skip/binary, Rest/binary>> = Buffer,
			C1 = Callback({body, Data}),
			
			% Parse the next set of headers...
			State1 = State#mp{callback=C1(body_end), buffer=Rest},
			feed_mp(headers, State1, Req);
			
		{maybe, Size} ->
			% Feed the data we have into the callback function...
			<<Data:Size/binary, Rest/binary>> = Buffer,
			C1 = Callback({body, Data}),
			
			% Continue parsing the body...
			State1 = State#mp{callback=C1, buffer=Rest},
			feed_mp(body, read_more(State1, Req), Req);
			
		not_found ->
			% No boundary found, so this is pure data. Feed
			% it into the callback function...
			Data = Buffer,
			C1 = Callback({body, Data}),
			
			% Continue parsing the body...
			State1 = State#mp{callback=C1, buffer = <<>>},
			feed_mp(body, read_more(State1, Req), Req)
	end.

callback({headers, Headers}, _, State, Req) ->
	case get_name_and_filename(Headers) of
		{FormKey, FileName} when FileName == undefined; FileName == "" -> 
			fun(X) -> callback(X, FormKey, State, Req) end;
		{_FormKey, FileName} ->			
			State1 = State#state { filename=FileName },
			fun(X) -> callback(X, file, State1, Req) end
	end;

callback({body, _}, undefined, _, _) ->	
	throw(unexpected_data_received);

callback({body, Data}, file, State = #state { localfiledata=LocalFileData }, Req) ->	
	{ok, File} = file:open(LocalFileData, [raw, append]),
	file:write(File, Data),
	file:close(File),
	fun(X) -> callback(X, file, State, Req) end;

callback({body, Data}, FormKey, State, Req) ->	
	QueryArgs1 = [{FormKey, Data}|State#state.queryargs],
	State1 = State#state { queryargs=QueryArgs1 },
	fun(X) -> callback(X, undefined, State1, Req) end;

callback(body_end, _, State, Req) ->	
	fun(X) -> callback(X, undefined, State, Req) end;

callback(eof, _, State, _Req) ->
	{ok, State#state.queryargs, State#state.filename, State#state.localfiledata};
	
callback(Directive, _, _, _) ->
	throw({invalid_callback, Directive}).
	
get_name_and_filename(Headers) ->
	F = fun(Props) -> [{string:to_lower(Key), Value} || {Key, Value} <- Props] end,
	{_, FormData} = proplists:get_value("content-disposition", F(Headers)),
	Name = proplists:get_value("name", F(FormData)),
	FileName = proplists:get_value("filename", F(FormData)),
	{Name, FileName}.

%% parse_headers/0 - 
%% Given a binary, return a proplist of headers.
parse_headers(<<>>) -> 
	[];
parse_headers(Binary) ->
	parse_headers(Binary, []).

parse_headers(Binary, Acc) ->
	case find_in_binary(<<"\r\n">>, Binary) of
		{exact, N} ->
			<<Line:N/binary, "\r\n", Rest/binary>> = Binary,
			parse_headers(Rest, [split_header(Line) | Acc]);
		not_found ->
			lists:reverse([split_header(Binary) | Acc])
	end.

split_header(Line) ->
	{Name, [$: | Value]} = lists:splitwith(fun (C) -> C =/= $: end, binary_to_list(Line)),
	{string:to_lower(string:strip(Name)),
	parse_header(Value)}.
	
%% read_chunk/2 -
%% Read a new chunk of data from the supplied socket.

read_chunk(Length, Req) when Length > 0 ->
	case Length < ?CHUNKSIZE of
		true  -> Req:recv_from_socket(Length, ?IDLE_TIMEOUT);
		false -> Req:recv_from_socket(?CHUNKSIZE, ?IDLE_TIMEOUT)
	end.


%% read_more/1 -
%% Read more data from the socket, return a new multipart state.
read_more(State=#mp{length=Length, buffer=Buffer}, Req) ->
	Data = read_chunk(Length, Req),
	Buffer1 = <<Buffer/binary, Data/binary>>,
	State#mp{length=Length - size(Data), buffer=Buffer1}.



%% find_in_binary/2 -
%% Find the location of a binary value
%% within a longer binary value.
find_in_binary(B, Data) when size(B) > 0 ->
	case size(Data) - size(B) of
		Last when Last < 0 ->
			partial_find(B, Data, 0, size(Data));
		Last ->
			find_in_binary(B, size(B), Data, 0, Last)
	end.

find_in_binary(B, BS, D, N, Last) when N =< Last->
	case D of
		<<_:N/binary, B:BS/binary, _/binary>> ->
			{exact, N};
		_ ->
			find_in_binary(B, BS, D, 1 + N, Last)
	end;
	
find_in_binary(B, BS, D, N, Last) when N =:= 1 + Last ->
	partial_find(B, D, N, BS - 1).

partial_find(_B, _D, _N, 0) ->
	not_found;
partial_find(B, D, N, K) ->
	<<B1:K/binary, _/binary>> = B,
	case D of
		<<_Skip:N/binary, B1:K/binary>> ->
			{partial, N, K};
		_ ->
			partial_find(B, D, 1 + N, K - 1)
	end.


%% find_boundary/2 - 
%% Given a binary boundary prefix, and binary data,
%% return:
%%
%% - {next_boundary, LengthOfData, OffsetToNextData} if the boundary was found.
%% - {end_boundary, LengthOfData, OffsetToNextData>} if the ending of a multipart section was found.
%% - {maybe, LengthOfData} if there is not enough data present.
%% - not_found if the boundary was not found.
%%
%% Where:
%%
%% - LengthOfData is the longth of the current block of data.
%%
%% - OffsetToNextData is the additional bytes, after the 
%%   current block of data, until the next block starts.

find_boundary(Prefix, Data) ->
	case find_in_binary(Prefix, Data) of
		{exact, Skip} ->
			PrefixSkip = Skip + size(Prefix),
			case Data of
				<<_:PrefixSkip/binary, "\r\n", _/binary>> ->
					{next_boundary, Skip, size(Prefix) + 2};
				<<_:PrefixSkip/binary, "--\r\n", _/binary>> ->
					{end_boundary, Skip, size(Prefix) + 4};
				_ when size(Data) < PrefixSkip + 4 ->
					%% Underflow
					{maybe, Skip};
				_ ->
					%% False positive
					not_found
			end;
		{partial, Skip, Length} when (Skip + Length) =:= size(Data) ->
			%% Underflow
			{maybe, Skip};
		_ ->
			not_found
	end.



%%% BOUNDARY %%%

get_boundary(ContentType) ->
	{"multipart/form-data", Opts} = parse_header(ContentType),
	case proplists:get_value("boundary", Opts) of
		S when is_list(S) -> S
	end.
	
%% @spec parse_header(string()) -> {Type, [{K, V}]}
%% @doc  Parse a Content-Type like header, return the main Content-Type
%%	   and a property list of options.
parse_header(String) ->
	%% TODO: This is exactly as broken as Python's cgi module.
	%%	   Should parse properly like mochiweb_cookies.
	[Type | Parts] = [string:strip(S) || S <- string:tokens(String, ";")],
	F = fun (S, Acc) ->
				case lists:splitwith(fun (C) -> C =/= $= end, S) of
					{"", _} ->
						%% Skip anything with no name
						Acc;
					{_, ""} ->
						%% Skip anything with no value
						Acc;
					{Name, [$\= | Value]} ->
						[{string:to_lower(string:strip(Name)),
						  unquote_header(string:strip(Value))} | Acc]
				end
		end,
	{string:to_lower(Type),
	 lists:foldr(F, [], Parts)}.

unquote_header("\"" ++ Rest) ->
	unquote_header(Rest, []);
unquote_header(S) ->
	S.
unquote_header("", Acc) ->
	lists:reverse(Acc);
unquote_header("\"", Acc) ->
	lists:reverse(Acc);
unquote_header([$\\, C | Rest], Acc) ->
	unquote_header(Rest, [C | Acc]);
unquote_header([C | Rest], Acc) ->
	unquote_header(Rest, [C | Acc]).


get_tempfilename() ->
	Parts = [integer_to_list(X) || X <- binary_to_list(erlang:md5(term_to_binary(erlang:now())))],
	string:join(Parts, "-").

to_binary(A) when is_atom(A) -> to_binary(atom_to_list(A));
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(L) when is_list(L) -> list_to_binary(L).
