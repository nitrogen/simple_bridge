%% vim: ts=4 sw=4 et
%% Cowboy Request Server. Becaue cowboy returns Request object for each
%% request, this helps to manage that object so it can be pushed back onto
%% the pile after it's done being used.
%%
%% Right now, it's nasty and uses the process dict.  Not exactly the smoothest approach.
%%
%% I keep it at set/2 and get/1 as I'd like to eventually update it to be a server that uses a dict
%% but the concerns are for memory usage for expired requests.  We can at least 
%% Clean up our requests when complete, with a delete(Key), but I'd like to also 
%% make sure that the handler cleans itself up if the request crashes, say if
%% nitrogen crashes while doing whatever.
%%
%% Also, while using the process dict, the key will always be 'proc_dict_cowboy_request'

-module(cowboy_request_server).
-include_lib("simple_bridge.hrl").

-export([set/2,get/1]).

%-behaviour(gen_server).
-compile({no_auto_import,[get/1]}).

set(Key,RequestCache) ->
    %error_logger:info_msg("Saving ~p~n",[Req]),
    erlang:put(Key,RequestCache).

get(Key) ->
    %error_logger:info_msg("Getting ~p~n",[Key]),
    _RequestCache = erlang:get(Key).


%% -define(M,?MODULE).
%% 
%% -export([start/0,start_link/0,set/2,get/1,delete/1]).
%% -export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
%% 
%% start_link() ->
%%     gen_server:start_link({local, ?M}, ?M, [], []).  
%% 
%% start() ->
%%     gen_server:start({local, ?M}, ?M, [], []).
%% 
%% %% Public Functions 
%% init(_) ->
%%     {ok, dict:new()}.
%% 
%% set(Key, Req) ->
%%     ok = gen_server:call(?M,{set,Key,Req}).
%% 
%% get(Key) ->
%%     {ok, Req} = gen_server:call(?M,{get,Key}),
%%     Req.
%% 
%% delete(Key) ->
%%     ok = gen_server:call(?M,{delete,Key}).
%% 
%% %% Private functions
%% handle_call({set,Key,Req},_From,Dict) ->
%%     NewDict = dict:store(Key,Req,Dict),
%%     {reply,ok,NewDict};
%% handle_call({get,Key},_From,Dict) ->
%%     Reply = dict:find(Key,Dict),
%%     {reply,Reply,Dict};
%% handle_call({delete,Key},_From,Dict) ->
%%     NewDict = dict:erase(Key,Dict),
%%     {reply,ok,NewDict}.
%% 
%% handle_info(_Info,State) ->
%%     {noreply,State}.
%% 
%% handle_cast(_,State) ->
%%     {noreply,State}.
%% 
%% terminate(_Reason,_State) ->
%%     ok.
%% 
%% code_change(_OldVsn,State,_Extra) ->
%%     {ok,State}.
