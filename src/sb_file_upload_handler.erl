%%%-------------------------------------------------------------------
%%% @author Mehmet Emin Tok
%%% @copyright (c) 2013, Synlay Technologies
%%% See MIT-LICENSE for licensing information.
%%%
%%% @doc Handler for file uploads
%%%
%%% @end
%%% Created : 19.09.13
%%%-------------------------------------------------------------------
-module(sb_file_upload_handler).

%% API
-export([
    new_file/1
    ,new_file/2
    ,get_tempfile/1
    ,get_data/1
    ,complete_file/1
    ,receive_data/2
    ,memory_file_handler/0
    ,temporary_file_handler/0
]).


% Override with config variable {scratch_dir, Directory}
-define (SCRATCH_DIR, "./scratch").

% Override with config var {max_file_in_memory_size, SizeInMB}
% for backwards compatibility default is 0
-define (MAX_MEMORY_SIZE, 0).

% Override with config var {max_file_size, SizeInMB}
-define (MAX_FILE_SIZE, 100).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starting a new file
%%
%% Starts a new file with an optional File upload Handler
%% and a FileName.
%% When no File upload handler is given the memory file
%% handler is set as default.
%%
%% @end
%%--------------------------------------------------------------------
new_file(FileName) ->
    Result = new_file(memory_file_handler(), FileName),
    Result.
new_file(FileUploadHandler={_Id, Funs}, FileName) ->
    NewFileFun = proplists:get_value(new_file, Funs),
    Result = NewFileFun(FileUploadHandler, FileName),
    Result.

%%--------------------------------------------------------------------
%% @doc Completes the File
%%
%% Completes the File and returns the new File upload handler state.
%%
%% @end
%%--------------------------------------------------------------------
complete_file(FileUploadHandlerState={_Id, _State, Funs}) ->
    CompleteFileFun = proplists:get_value(complete_file, Funs),
    CompleteFileFun(FileUploadHandlerState).

%%--------------------------------------------------------------------
%% @doc Receiving new appending data
%%
%% Receives a "chunk" of data from the file upload and returns
%% the new File upload handler state.
%%
%% @end
%%--------------------------------------------------------------------
receive_data(FileUploadHandlerState={_Id, _State, Funs}, Data) ->
    ReceiveDataFun = proplists:get_value(receive_data, Funs),
    ReceiveDataFun(FileUploadHandlerState, Data).

%%--------------------------------------------------------------------
%% @doc Getting the temporary file name
%%
%% This is empty for "memory" handler and set by the
%% "temporary_file" handler.
%%
%% @end
%%--------------------------------------------------------------------
get_tempfile(FileUploadHandlerState={_Id, _State, Funs}) ->
    TempFileFun = proplists:get_value(get_tempfile, Funs),
    TempFileFun(FileUploadHandlerState).

%%--------------------------------------------------------------------
%% @doc Getting the File data
%%
%% This is empty for "temporary_file" handler and set by the
%% "memory" handler.
%%
%% @end
%%--------------------------------------------------------------------
get_data(FileUploadHandlerState={_Id, _State, Funs}) ->
    DataFun = proplists:get_value(get_data, Funs),
    DataFun(FileUploadHandlerState).


%%--------------------------------------------------------------------
%% @doc File Upload Handler "temporary_file"
%%
%% the temporary_file file upload handler is writing the uploaded
%% files to a temporary file on the hard disk.
%%
%% @end
%%--------------------------------------------------------------------
temporary_file_handler() ->
    {temporary_file, [
        {new_file,      fun handle_new_file/2}
        ,{receive_data,  fun handle_receive_data/2}
        ,{complete_file, fun handle_complete_file/1}
        ,{get_tempfile,  fun handle_get_tempfile/1}
        ,{get_data,      fun handle_get_data/1}
    ]}.



%%--------------------------------------------------------------------
%% @doc File Upload Handler "memory"
%%
%% the memory file upload handler is keeping the file in the
%% memory while the file is smaller than `get_max_memory_size()'.
%% When file is bigger this handler transforms to "temporary_file"
%% handler.
%%
%% @end
%%--------------------------------------------------------------------
memory_file_handler() ->
    {memory, [
        {new_file,      fun handle_new_file/2}
        ,{receive_data,  fun handle_receive_data/2}
        ,{complete_file, fun handle_complete_file/1}
        ,{get_tempfile,  fun handle_get_tempfile/1}
        ,{get_data,      fun handle_get_data/1}
    ]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_new_file({temporary_file, Funs}, FileName) ->
    {temporary_file, {FileName, get_tempfilename(), 0}, Funs};
handle_new_file({memory, Funs}, FileName) ->
    {memory, {FileName, <<>>, 0}, Funs}.

handle_receive_data({memory, {FileName, DataState, CurrentSize}, Funs}, Data) ->
    NewSize = CurrentSize + size(Data),
    case NewSize > get_max_memory_size() of
        true ->
            %% Transform current MemoryUploadHandler to FileUploadHandler
            %% when Data is too big
            NewFileHandler = new_file(temporary_file_handler(), FileName),
            receive_data(NewFileHandler, <<DataState/binary, Data/binary>>);
        false ->
            {memory, {FileName, <<DataState/binary, Data/binary>>, NewSize}, Funs}
    end;
handle_receive_data({temporary_file, {FileName, TempFile, CurrentSize}, Funs}, Data) ->
    NewSize = CurrentSize + size(Data),

    % Throw exception if the uploaded file is getting too big.
    case NewSize > get_max_file_size() of
        true ->
            file:delete(TempFile),
            throw({file_too_big, FileName});
        false ->
            continue
    end,

    ok = filelib:ensure_dir(TempFile),
    {ok, FD} = file:open(TempFile, [append, raw]),
    ok = file:write(FD, Data),
    ok = file:close(FD),
    {temporary_file, {FileName, TempFile, NewSize}, Funs}.

handle_get_tempfile({temporary_file, {_FileName, TempFile, _CurrentSize}, _}) -> TempFile;
handle_get_tempfile({memory, _, _}) -> undefined.

handle_get_data({temporary_file, _, _}) -> undefined;
handle_get_data({memory, {_FileName, DataState, _CurrentSize}, _}) -> DataState.

handle_complete_file(State) -> State.

get_tempfilename() ->
	Dir = simple_bridge_util:get_scratch_dir(?SCRATCH_DIR),
    Parts = [integer_to_list(X) || X <- binary_to_list(erlang:md5(term_to_binary(os:timestamp())))],
    filename:join([Dir, string:join(Parts, "-")]).


get_max_file_size() ->
	simple_bridge_util:get_max_file_size(?MAX_FILE_SIZE).

get_max_memory_size() ->
	simple_bridge_util:get_max_file_in_memory_size(?MAX_MEMORY_SIZE).
