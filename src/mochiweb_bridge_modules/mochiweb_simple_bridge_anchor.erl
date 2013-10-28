-module(mochiweb_simple_bridge_anchor).
-export ([
		loop/1
	]).

loop(Req) ->
	Bridge = simple_bridge:make(mochiweb, Req),
	Callout = simple_bridge_util:get_env(callout),
	Callout:run(Bridge).
