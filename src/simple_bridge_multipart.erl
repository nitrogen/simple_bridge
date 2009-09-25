% Simple Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge_multipart).
-export ([parse/1]).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).

% Alas, so many Erlang HTTP Servers, and so little parsing of Multipart forms.
% This file contains multipart form parsing logic that is shared by all 
% request bridges.
% Large portions of this file are from mochiweb_multipart.erl
% Copyright 2007 Mochi Media, Inc., written by Bob Ippolito <bob@mochimedia.com>.

% Replace with http://bitbucket.org/justin/webmachine/src/tip/src/webmachine_multipart.erl

-define(CHUNKSIZE, 16).
-define(IDLE_TIMEOUT, 30000).
-define (SCRATCH_DIR, "./scratch").
% -record(mp, {state, boundary, length, buffer, callback}).
% -record(state, {queryargs = [], filename=undefined, localfiledata=undefined}).

-record (state, {
	req,          % The simplebridge request object.
	boundary,     % The multipart boundary
	length,       % The length of the post.
	bytes_read=0, % How many bytes we have read so far.
	parts         % The finished parts we have accumulated.
}).

-record (part, {
	name,         % The name of the form element that created this part
	value=[],     % The value of the part, as a string, or {'file', Tempfile} if it's a file
	filename,     % The name of the posted file
	mime_type,    % The mime type of the file
	size=0,       % The size of the part's value
	needs_rn      % True if we should add a \r\n before the next write 
}).

-define (NEWLINE, "\r\n").

parse(Req) ->
	case is_multipart_request(Req) of
		true -> parse_multipart(Req);
		false -> not_multipart
	end.

is_multipart_request(Req) ->
	case Req:header(content_type) of
		"multipart/form-data" ++ _ -> true;
		_ -> false
	end.

parse_multipart(Req) -> 
  % Get the boundary...
	{_K, _V, Props} = parse_header(Req:header(content_type)),
	Boundary = to_binary(proplists:get_value("boundary", Props)),
	
	% Get whatever the underlying server has already read...
	Data = to_binary(Req:request_body()),
	
	% Create the state...
	State = #state { req = Req, boundary = Boundary, bytes_read = size(Data), parts = [] },
	
	try 
		State1 = read_boundary(Data, State),
	catch Type : Message ->
		?PRINT({Type, Message}),
		?PRINT(erlang:get_stacktrace())
	end.
	
% Not yet in a part. Read the POST headers to get content boundary and length.
read_boundary(Data, State = #state { boundary=Boundary }) ->
	{Line, Data1, State} = get_next_line(Data, State),
	case interpret_line(Line, Boundary) of
		start_next_part -> read_part_header(Data1, #part {}, State);
		Other -> throw({unexpected, Other, Line})
	end.
	
%%% PART HEADERS %%%
	
% We are in a part. Read headers.
read_part_header(Data, Part, State = #state { boundary=Boundary }) ->
	{Line, Data1, State} = get_next_line(Data, State),
	case interpret_line(Line, Boundary) of
		start_next_part -> throw({value_expected, Line});
		start_value -> read_part_value(Data1, Part, State);
		continue ->
			% Parse the header, add it to Part, then loop.
			Part1 = update_part_with_header(parse_header(Line), Part),
			read_part_header(Data1, Part1, State);
		eof -> State
	end.
	
update_part_with_header({"content-disposition", "form-data", Params}, Part) ->
	Part1 = case proplists:get_value("name", Params) of
		undefined -> Part;
		Name -> Part#part { name=Name }
	end,
	Part2 = case proplists:get_value("filename", Params) of
		undefined -> Part1;
		Filename -> 
			Part1#part { 
				filename=Filename,
				value = {file, get_tempfilename()}
			}
	end,
	Part2;
update_part_with_header(_, Part) -> Part.
	
%%% PART VALUES %%%
	
% We are in a part's value. Read the value until we see a boundary.
read_part_value(Data, Part, State = #state { boundary=Boundary }) ->
	{Line, Data1, State} = get_next_line(Data, State),
	case interpret_line(Line, Boundary) of
		start_next_part -> 
			% Finalize the write, then start the next part.
			State1 = update_state_with_part(Part, State),
			read_part_header(Data1, #part {}, State1);
		A when A == start_value orelse A == continue ->
			% Write the line, then continue...	
			Part1 = update_part_with_value(Line, true, Part),
			read_part_value(Data1, Part1, State);
		eof -> 
			State1 = update_state_with_part(Part, State),
			State1
	end.
	
update_part_with_value(Data, IsLine, Part = #part { value={file, TempFile}, size=Size, needs_rn=NeedsRN }) ->
	{Prefix, NewSize} = get_prefix_and_newsize(NeedsRN, Size, Data),
	ok = filelib:ensure_dir(TempFile),
	% TODO - Stop if the uploaded file is getting too big.
	% Write to the file...
	{ok, FD} = file:open(TempFile, [append, raw]),
	ok = file:write(FD, Prefix),
	ok = file:write(FD, Data),
	ok = file:close(FD),
	Part#part { size=NewSize, needs_rn=IsLine };	
	
update_part_with_value(Data, IsLine, Part = #part { value=Value, size=Size, needs_rn=NeedsRN }) ->
	{Prefix, NewSize} = get_prefix_and_newsize(NeedsRN, Size, Data),
	NewValue = Value ++ Prefix ++ binary_to_list(Data),
	Part#part { value=NewValue, size=NewSize, needs_rn=IsLine }.
	
update_state_with_part(Part, State = #state { parts=Parts }) ->
	State#state { parts=[Part|Parts] }.
	
% Returns {Prefix, NewSize}.
get_prefix_and_newsize(NeedsRN, Size, Data) ->
	case NeedsRN of
		true -> {?NEWLINE, Size + length(?NEWLINE) + size(Data)};
		_    -> {"", Size + size(Data)}
	end.
	

% Return the next line of input from the post, reading
% more data if necessary.
% get_next_line(Data, State) -> {Line, RemainingData, NewState}.
get_next_line(Data, State)	-> get_next_line(Data, <<>>, State).
get_next_line(<<?NEWLINE, Data/binary>>, Acc, State) -> {<<Acc/binary>>, Data, State};
get_next_line(<<C, Data/binary>>, Acc, State) -> get_next_line(Data, <<Acc/binary, C>>, State);
get_next_line(Data, Acc, State) when Data == undefined orelse Data == <<>> ->
	{Data1, State1} = read_chunk(State),
	% TODO, if we have more than ?CHUNKSIZE data already read, then flush
	% it into a part.
	get_next_line(Data1, Acc, State1).
	
read_chunk(State = #state { req=Req, length=Length, bytes_read=BytesRead }) ->
	BytesToRead = lists:min([Length - BytesRead, ?CHUNKSIZE]),
	Data = Req:recv_from_socket(BytesToRead, ?IDLE_TIMEOUT),
	{Data, State#state { bytes_read=BytesRead + size(Data) }}.

interpret_line(Line, Boundary) -> 
	if 
		Line == <<"--", Boundary/binary, "--">> -> eof;
		Line == <<"--", Boundary/binary>> -> start_next_part;
		Line == <<>>   -> start_value;
		true             -> continue
	end. 
	

parse_header(B) when is_binary(B) -> parse_header(binary_to_list(B));
parse_header(String) ->
	[First|Rest] = [string:strip(S) || S <- string:tokens(String, ";")],
	{Name, Value} = parse_keyvalue($:, First),
	Params = [parse_keyvalue($=,X) || X <- Rest],
	Params1 = [{K,V} || {K,V} <- Params, K /= "", V /= ""],
	{Name, Value, Params1}.
	
parse_keyvalue(Char, S) ->
	% If Char not found, then use an empty value...
	{Key, Value} = case string:chr(S, Char) of
		0   -> {S, ""};
		Pos -> {string:substr(S, 1, Pos - 1), string:substr(S, Pos + 1)}
	end,
	{string:to_lower(string:strip(Key)), 
	 unquote_header(string:strip(Value))}.

get_tempfilename() ->
	Parts = [integer_to_list(X) || X <- binary_to_list(erlang:md5(term_to_binary(erlang:now())))],
	filename:join(["temp", string:join(Parts, "-")]).


% unquote_header borrowed from Mochiweb.
unquote_header("\"" ++ Rest) -> unquote_header(Rest, []);
unquote_header(S) -> S.
unquote_header("", Acc) -> lists:reverse(Acc);
unquote_header("\"", Acc) -> lists:reverse(Acc);
unquote_header([$\\, C | Rest], Acc) -> unquote_header(Rest, [C | Acc]);
unquote_header([C | Rest], Acc) -> unquote_header(Rest, [C | Acc]).

to_binary(undefined) -> <<"">>;
to_binary(A) when is_atom(A) -> to_binary(atom_to_list(A));
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(L) when is_list(L) -> list_to_binary(L).
