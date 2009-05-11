-module (request_bridge_tests).
-export ([
	url_test/0
]).

-define(GOOGLE_IP, {64,233,169,147}).
-define(GOOGLE_PORT, 80).
-define(PEER_IP, ?GOOGLE_IP).
-define(PEER_PORT, ?GOOGLE_PORT).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(MSG(S), io:format(S)).

url_test() ->
	InetsBridge = utils:make_inets_bridge(),
	'GET' = InetsBridge:request_method(),
	"/web/req" = InetsBridge:path(),
	"querystring" = InetsBridge:querystring(),
	?PEER_IP = InetsBridge:peer_ip(),
	?PEER_PORT = InetsBridge:peer_port().