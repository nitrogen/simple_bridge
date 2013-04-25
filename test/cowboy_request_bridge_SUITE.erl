-module(cowboy_request_bridge_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{seconds,30}}].
all() -> [{group, onrequest}].

groups() -> [{onrequest, [], [request]}].

%% for cowboy 0.8
init_per_suite(Config) ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    Config.

end_per_suite(_Config) ->
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto),
    ok.

init_per_group(onrequest, Config) ->
    Port = 33084,
    Transport = ranch_tcp,
    {ok, _} = cowboy:start_http(onrequest, 100, [{port, Port}], [
								 {env, [{dispatch, init_dispatch()}]},
								 {max_keepalive, 50},
								 {timeout, 500}
								]),
    {ok, Client} = cowboy_client:init([]),
    [{scheme, <<"http">>}, {port, Port}, {opts, []},
     {transport, Transport}, {client, Client} | Config].

end_per_group(Name, _) ->
    cowboy:stop_listener(Name),
    ok.

init_dispatch() ->
    cowboy_router:compile([{"localhost", [{'_', cowboy_request_bridge_SUITE, []}]}]).

build_url(Path, Config) ->
    {scheme, Scheme} = lists:keyfind(scheme, 1, Config),
    {port, Port} = lists:keyfind(port, 1, Config),
    PortBin = list_to_binary(integer_to_list(Port)),
    PathBin = list_to_binary(Path),
    << Scheme/binary, "://localhost:", PortBin/binary, PathBin/binary >>.


-define(PATH, "/page").
-define(QUERY_STRING, "query_string=type").
-define(EXPECTED_RESPONSE, <<"Simple Bridge Response">>).

request(Config) ->
    Client = ?config(client, Config),
    URL = build_url(?PATH ++ "?" ++ ?QUERY_STRING, Config),
    ct:log("-> url ~p", [URL]),
    {ok, Client2} = cowboy_client:request(<<"GET">>, URL, Client),
    {ok, 200, Headers, Client3} = cowboy_client:response(Client2),
    {ok, Body, _} = cowboy_client:response_body(Client3),
    ct:log("-> response Body: ~p", [Body]),
    ?EXPECTED_RESPONSE = Body,
    ok.

%% RequestBridge interface
%% {init, 1},           % Should accept the request value passed by the http server.
%% {protocol, 1},       % http | https | undefined
%% {request_method, 1}, % GET, POST, etc.
%% {uri, 1},            % The uri (path and querystring)
%% {path, 1},           % Just the path. (http://server.com/<PATH>?querystring)
%% {headers, 1},        % Return a proplist of headers, key and value are strings.
%% {cookies, 1},        % Return a proplist of cookies, key and value are strings.
%% {query_params, 1},   % Return a proplist of query parameters, key and value are strings.
%% {post_params, 1},    % Return a proplist of post parameters, key and value are strings.
%% {peer_ip, 1}         % The remote IP address

%% handle to process http request
-record(state, {headers, body}).
init({_Transport, http}, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    ct:log("-> init RequestBridge and ResponseBridge", []),
    %% init RequestBridge and ResponseBridge
	DocRoot = undefined,
    RequestBridge = simple_bridge:make_request(cowboy_request_bridge, {Req, DocRoot}),
    ResponseBridge = simple_bridge:make_response(cowboy_response_bridge, RequestBridge),

    %% test API functions for RequestBridge interface
    Protocol = RequestBridge:protocol(),
    ct:log("-> Protocol ~p", [Protocol]),

    ct:log("-> RequestMethod ~p", [RequestMethod = RequestBridge:request_method()]),
    'GET' = RequestMethod,

    ct:log("-> URI ~p", [URI = RequestBridge:uri()]),
    "http://localhost:33084/page?query_string=type" = URI,

    ct:log("-> Path ~p", [Path = RequestBridge:path()]),
    ?PATH = Path,

    ct:log("-> Headers ~p", [Headers = RequestBridge:headers()]),
    [{host, _},{user_agent,"Cow"}] = Headers,

    ct:log("-> Cookies ~p", [Cookies = RequestBridge:cookies()]),
    [] = Cookies,

    ct:log("-> Query_params ~p", [Query_params = RequestBridge:query_params()]),
    [{"query_string","type"}] = Query_params,

    ct:log("-> Post_params ~p", [Post_params = RequestBridge:post_params()]),
    [] = Post_params,

    ct:log("-> Peer_IP ~p", [Peer_IP = RequestBridge:peer_ip()]),
    {127,0,0,1} = Peer_IP,

    %% test ResponseBridge
    %% create response
    ResponseBridge1 = ResponseBridge:data(binary_to_list(?EXPECTED_RESPONSE)),
    {ok, Req2} = ResponseBridge1:build_response(),
    ct:log("-> Response ~p", [Req2]),

    ct:log("-> send response", []),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
