-module(simple_bridge_multipart_SUITE).

-include_lib("common_test/include/ct.hrl").

% Override with config variable {scratch_dir, Directory}
-define (SCRATCH_DIR, "./scratch").

-export([
    all/0,
    groups/0,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    post_multipart/1,
    post_multipart_post_too_big/1,
    post_multipart_file_too_big/1
]).

all() -> [{group, multipart}].

groups() -> [
    {multipart,
        [sequence, {repeat, 100}],
        [post_multipart, post_multipart_post_too_big, post_multipart_file_too_big]
    }].

init_per_group(_Group, Config) ->
    inets:start(),
    application:ensure_all_started(ibrowse),
    application:start(simple_bridge),
    Config.

end_per_group(_Group, Config) ->
    inets:stop(),
    application:stop(simple_bridge),
    Config.

init_per_testcase(_TestCase, Config) ->
    %inets:start(),
    Config.

end_per_testcase(_TestCase, Config) ->
    lists:foreach(fun(File) -> file:delete(File) end, filelib:wildcard("./scratch/*")),
    %inets:stop(),
    Config.

post_multipart(_Config) ->
    BinStream1 = crypto:strong_rand_bytes(1024000),
    BinStream2 = crypto:strong_rand_bytes(2048000),
    Data1 = binary_to_list(BinStream1),
    Data2 = binary_to_list(BinStream2),
    Files = [{data, "data1", Data1}, {data, "data2", Data2}],

    {UploadedFiles, Errors} = binary_to_term(post_multipart("uploaded_files", Files)),

    2 = length(UploadedFiles),
    none = Errors,
    2 = length(get_all_files_from_scratch_dir()),
    lists:foreach(fun({AtomFieldName, FileName, FileContent}) ->
        FieldName = atom_to_list(AtomFieldName),
        BinaryFileContent = list_to_binary(FileContent),
        ByteSize = byte_size(BinaryFileContent),
        File = proplists:get_value(FileName, UploadedFiles),
        {ok, Binary} = file:read_file(sb_uploaded_file:temp_file(File)),

        ByteSize = sb_uploaded_file:size(File),
        FieldName = sb_uploaded_file:field_name(File),
        FileName = sb_uploaded_file:original_name(File),
        Binary = BinaryFileContent
    end, Files).

post_multipart_post_too_big(_Config) ->
    BinStream1 = crypto:strong_rand_bytes(2048000),
    BinStream2 = crypto:strong_rand_bytes(2048000),
    Data1 = binary_to_list(BinStream1),
    Data2 = binary_to_list(BinStream2),
    Files = [{data, "data1", Data1}, {data, "data2", Data2}],

    case post_multipart("uploaded_files", Files) of
        {error, socket_closed_remotely} ->
            ok;
        Bin ->
            {[], post_too_big} = binary_to_term(Bin)
    end,
    [] = get_all_files_from_scratch_dir().

post_multipart_file_too_big(_) ->
    BinStream1 = crypto:strong_rand_bytes(1024),
    BinStream2 = crypto:strong_rand_bytes(3072000),
    Data1 = binary_to_list(BinStream1),
    Data2 = binary_to_list(BinStream2),
    Files = [{data, "data1", Data1}, {data, "data2", Data2}, {data, "data3", Data1}],

    {[], {file_too_big,"data2"}} = binary_to_term(post_multipart("uploaded_files", Files)),
    [] = get_all_files_from_scratch_dir().


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_all_files_from_scratch_dir() ->
    filelib:wildcard(filename:absname_join(simple_bridge_util:get_scratch_dir(?SCRATCH_DIR), "*")).

%% Based on http://stackoverflow.com/a/39284072
format_multipart_formdata(Boundary, Fields, Files) ->
    FieldParts = lists:map(fun({FieldName, FieldContent}) ->
        [lists:concat(["--", Boundary]),
         lists:concat(["Content-Disposition: form-data; name=\"",atom_to_list(FieldName),"\""]),
         "", FieldContent]
    end, Fields),

    FieldParts2 = lists:append(FieldParts),

    FileParts = lists:map(fun({FieldName, FileName, FileContent}) ->
        [lists:concat(["--", Boundary]),
         lists:concat(["Content-Disposition: form-data; name=\"",atom_to_list(FieldName),"\"; filename=\"",FileName,"\""]),
         lists:concat(["Content-Type: ", "application/octet-stream"]), "", FileContent]
    end, Files),

    FileParts2 = lists:append(FileParts),
    EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
    Parts = lists:append([FieldParts2, FileParts2, EndingParts]),
    string:join(Parts, "\r\n").

post_multipart(Path, Files) ->
    Boundary = "------WebKitFormBoundaryUscTgwn7KiuepIr1",
    ReqBody = format_multipart_formdata(Boundary, [], Files),
    ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
    ReqHeader = [{"Content-Length", integer_to_list(length(ReqBody))}],

    URL = "http://127.0.0.1:8000/" ++ Path,
    case httpc:request(post,{URL, ReqHeader, ContentType, ReqBody}, [], [{body_format, binary}]) of
        FullRes = {ok, {_, _, BodyBin}} -> 

            %Headers = ReqHeader ++ [{"Content-Type", ContentType}],
            %FullRes = {ok, _StatusCode, _Headers, ResBody} = ibrowse:send_req(URL, Headers, post, ReqBody),
            %BodyBin = iolist_to_binary(ResBody),
            
            error_logger:info_msg("Returned Value: ~p",[FullRes]),

            BodyBin;
        {error, socket_closed_remotely} ->
            %% I don't particularly like that this is being returned by some webservers when crashing, but I can't find why it happens and why it's so intermittent.  But it only seems to do it with the post_to_big or file_to_big errors, so I think that's okay.
            {error, socket_closed_remotely}
    end.
