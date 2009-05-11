-module (eunit_helper).
-export ([start/0]).

start() ->
	eunit:test(request_bridge).